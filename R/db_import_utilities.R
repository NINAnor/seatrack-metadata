#' Check existing rows in database given certain columns from a metadata dataframe
#'
#' This function checks for existing rows in a database table based on specified columns from a metadata dataframe.
#' It constructs a SQL query to compare the values in the dataframe with those in the database table. IF a row from the dataframe
#' does not exist in the database table based on the specified columns, it is marked as missing.
#'
#' @param metadata_df A dataframe containing metadata to be checked against the database.
#' @param table The name of the database table to check against.
#' @param col_names A vector of column names from the metadata dataframe to be used for comparison. If NULL, all columns are used.
#' @param db_col_names A vector of column names in the database table corresponding to `col_names`. If NULL, `col_names` are used.
#' @return A logical vector indicating which rows in the metadata dataframe do not exist in the database table.
#' @export
#' @concept db_import_utility
check_db_metadata_import <- function(metadata_df, table, col_names = NULL, db_col_names = NULL) {
    if (nrow(metadata_df) == 0) {
        return(logical(0))
    }
    existing_rows <- get_db_metadata_import(metadata_df, table, col_names, db_col_names)
    log_trace("Check existing rows in database using columns: ", paste(col_names, collapse = ", "), "... ", nrow(existing_rows), " existing rows found")

    existing_rows_combined <- sapply(seq_len(nrow(existing_rows)), function(row_idx) {
        paste(existing_rows[row_idx, ], collapse = " ")
    })
    metadata_df_combined <- sapply(seq_len(nrow(metadata_df)), function(row_idx) {
        paste(metadata_df[row_idx, ], collapse = " ")
    })
    missing_row_bool <- !metadata_df_combined %in% existing_rows_combined
    log_trace(sum(missing_row_bool), " rows are not in the database")

    return(missing_row_bool)
}

get_db_metadata_import <- function(metadata_df, table, col_names = NULL, db_col_names = NULL, additional_db_col_names = c()) {
    if (nrow(metadata_df) == 0) {
        stop("No data to check!")
    }
    metadata_df <- tibble(metadata_df)
    if (is.null(col_names)) {
        col_names <- names(metadata_df)
    }
    if (is.null(db_col_names)) {
        db_col_names <- col_names
    }
    if (length(db_col_names) != length(col_names)) {
        stop("`db_col_names`` must be equal in length to `col_names`")
    }
    # given a table
    res <- DBI::dbSendQuery(con, glue::glue("SELECT {paste(db_col_names, collapse = ',')} FROM {table} LIMIT 1"))
    column_info <- DBI::dbColumnInfo(res)
    DBI::dbClearResult(res)

    # check certain columns
    db_cols <- metadata_df[, col_names]

    temp_values <- seatrackR::R_df_to_db_values(db_cols, column_info$type)

    t_db_names <- paste0("t.", db_col_names)
    if (length(additional_db_col_names) > 0) {
        t_additional_db_names <- paste0("t.", additional_db_col_names)
    } else {
        t_additional_db_names <- c()
    }

    t_db_names_combined <- paste(c(t_db_names, t_additional_db_names), collapse = ",\n")
    mv_db_names <- paste0("mv.", db_col_names)
    mv_db_names_combined <- paste(mv_db_names, collapse = ",\n")

    match_names <- lapply(seq_along(t_db_names), function(name_idx) {
        glue::glue("({t_db_names[name_idx]} IS NOT DISTINCT FROM {mv_db_names[name_idx]})")
    })
    combined_match_names <- paste(match_names, collapse = " AND \n")

    combined_db_names <- paste(db_col_names, collapse = ", ")


    query <- glue::glue("
    WITH match_values ({combined_db_names}) AS (
        VALUES
            { temp_values }
    )
    SELECT
        {t_db_names_combined}
    FROM
        {table} t
        JOIN match_values mv ON
        {combined_match_names}
        ;
    ")

    existing_rows <- DBI::dbGetQuery(con, query)
    return(existing_rows)
}

#' Check existing active sessions in database
#'
#' Function to check which startups from the master import sheet correspond to active sessions in the database.
#' It constructs a dataframe of session IDs and uses the `check_db_metadata_import` function
#' to determine which startups are associated with active sessions.
#' @param startup_shutdown A tibble containing session information from master import startup_shutdown.
#' @param filter_startups A logical indicating whether to filter out startups associated with active sessions. Default is TRUE.
#' @return If filter_startups is TRUE, a tibble of startups that are associated with active sessions in the database. Otherwise
#' @concept db_import_utility
#' @export
check_startups_active_db <- function(startup_shutdown, filter_startups = FALSE) {
    if (nrow(startup_shutdown) == 0) {
        return(startup_shutdown)
    }

    # Get database logger IDs
    all_ids <- startup_shutdown$logger_serial_no

    db_logger_ids <- get_db_metadata_import(data.frame(
        logger_serial_no = all_ids
    ), table = "loggers.logger_info", additional_db_col_names = "logger_id")

    all_ids_db <- db_logger_ids$logger_id[match(all_ids, db_logger_ids$logger_serial_no)]

    # Check for an active session
    db_startup_df_active <- tibble(logger_id = all_ids_db[!is.na(all_ids_db)], active = TRUE)
    active_session_bool <- check_db_metadata_import(db_startup_df_active, "loggers.logging_session")

    # Log the removal of active sessions

    active_session_bool_all_ids <- active_session_bool[match(all_ids_db, all_ids_db[!is.na(all_ids_db)])]
    active_session_bool_all_ids[is.na(active_session_bool_all_ids)] <- TRUE

    active_summary <- startup_shutdown[!active_session_bool_all_ids, c("logger_serial_no", "starttime_gmt")]
    if (nrow(active_summary) > 0) {
        log_warn(nrow(active_summary), " startups for loggers already in active sessions in database.")
        if (filter_startups) {
            log_warn("Sessions being removed:")
        }
        log_warn("Sessions :\n", paste(capture.output(print(active_summary, n = nrow(active_summary)))[c(-1, -3)], collapse = "\n"))
    }
    if (filter_startups) {
        startup_shutdown <- startup_shutdown[active_session_bool_all_ids, ]
    }

    return(startup_shutdown)
}

#' Check existing startups in database
#'
#' Function to check which startups from the master import sheet are not already present in the database.
#' It constructs a dataframe of session IDs and start times, and uses the `check_db_metadata_import` function
#' to determine which startups are new and need to be added to the database.
#' @param startup_shutdown A tibble containing session information from master import startup_shutdown.
#' @return A tibble of startups that are not already present in the database.
#' @concept db_import_utility
#' @export
check_startups_db <- function(startup_shutdown) {
    if (nrow(startup_shutdown) == 0) {
        return(startup_shutdown)
    }

    db_startup_df <- data.frame(
        session_id = paste(startup_shutdown$logger_serial_no, as.Date(startup_shutdown$starttime_gmt), sep = "_"),
        starttime_gmt = startup_shutdown$starttime_gmt
    )

    sessions_to_startup_bool <- check_db_metadata_import(db_startup_df, "loggers.startup")
    startup_new <- startup_shutdown[sessions_to_startup_bool, ]
    return(startup_new)
}

#' Check existing shutdowns in database
#'
#' Function to check which shutdowns from the master import sheet are not already present in the database.
#' It constructs a dataframe of session IDs and download dates, and uses the `check_db_metadata_import` function
#' to determine which shutdowns are new and need to be added to the database.
#' @param startup_shutdown A tibble containing session information from master import startup_shutdown.
#' @param check_active A logical indicating whether to first check for active sessions and filter out associated startups. Default is FALSE.
#' @return A tibble of shutdowns that are not already present in the database.
#' @concept db_import_utility
#' @export
check_shutdown_db <- function(startup_shutdown, check_active = FALSE) {
    if (nrow(startup_shutdown) == 0) {
        return(startup_shutdown)
    }
    shutdown <- startup_shutdown[!is.na(startup_shutdown$download_date) | !is.na(startup_shutdown$shutdown_date), ]
    if (nrow(shutdown) == 0) {
        return(shutdown)
    }


    db_shutdown_df <-
        data.frame(
            session_id = paste(shutdown$logger_serial_no, as.Date(shutdown$starttime_gmt), sep = "_"),
            download_date = shutdown$download_date
        )

    db_shutdown_df$download_date[is.na(db_shutdown_df$download_date)] <- shutdown$shutdown_date[is.na(db_shutdown_df$download_date)]

    sessions_to_shutdown_bool <- check_db_metadata_import(db_shutdown_df, "loggers.shutdown")
    shutdown_new <- shutdown[sessions_to_shutdown_bool, ]

    if (nrow(shutdown_new) == 0) {
        return(shutdown_new)
    }

    if (check_active) {
        # First check for an active session
        db_shutdown_df <-
            data.frame(
                session_id = paste(shutdown_new$logger_serial_no, as.Date(shutdown_new$starttime_gmt), sep = "_"), active = TRUE
            )

        closed_session_bool <- check_db_metadata_import(db_shutdown_df, "loggers.logging_session")


        removed_shutdowns <- shutdown_new[closed_session_bool, ]
        shutdown_new <- shutdown_new[!closed_session_bool, ]


        if (nrow(removed_shutdowns) > 0) {
            log_warn(glue::glue("The following {nrow(removed_shutdowns)} shutdowns have been removed as they have no active sessions in the database."))
            db_shutdown_df <-
                data.frame(
                    session_id = paste(removed_shutdowns$logger_serial_no, as.Date(removed_shutdowns$starttime_gmt), sep = "_")
                )
            closed_sessions <- get_db_metadata_import(db_shutdown_df, "loggers.shutdown", additional_db_col_names = c("download_date"))

            for (i in seq_len(nrow(removed_shutdowns))) {
                session_summary <- removed_shutdowns[i, c("logger_serial_no", "starttime_gmt", "download_type", "download_date", "shutdown_date")]
                log_warn(
                    paste0(session_summary$logger_serial_no, "_", as.Date(session_summary$starttime_gmt)),
                    ":\n", paste(capture.output(print(session_summary, n = nrow(session_summary)))[c(-1, -3)], collapse = "\n")
                )
                if (paste(session_summary$logger_serial_no, as.Date(session_summary$starttime_gmt), sep = "_") %in% closed_sessions$session_id) {
                    current_closed_session <- closed_sessions[closed_sessions$session_id == paste(session_summary$logger_serial_no, as.Date(session_summary$starttime_gmt), sep = "_"), ]
                    log_warn(paste("This session was closed with a download date of ", as.Date(current_closed_session$download_date)))
                } else {
                    log_warn("This session does not exist.")
                }
            }
        }
    }
    # Check also the shutdown date, as the download date can differ from the database entry
    db_shutdown_df <-
        data.frame(
            session_id = paste(shutdown_new$logger_serial_no, as.Date(shutdown_new$starttime_gmt), sep = "_"),
            shutdown_date = shutdown_new$shutdown_date
        )
    db_shutdown_df <- db_shutdown_df[!is.na(db_shutdown_df$shutdown_date), ]
    sessions_to_shutdown_bool <- rep(TRUE, nrow(shutdown_new))

    check_result <- check_db_metadata_import(db_shutdown_df, "loggers.shutdown")
    sessions_to_shutdown_bool[!is.na(shutdown_new$shutdown_date)] <- check_result
    shutdown_new <- shutdown_new[sessions_to_shutdown_bool, ]

    return(shutdown_new)
}

#' Check existing retrievals in database
#'
#' Function to check which retrievals from the master import sheet are not already present in the database.
#' It constructs a dataframe of retrieval dates and individ IDs, and uses the `check_db_metadata_import` function
#' to determine which retrievals are new and need to be added to the database.
#' @param retrievals A tibble containing retrieval information from master import metadata.
#' @return A tibble of retrievals that are not already present in the database.
#' @concept db_import_utility
#' @export
check_retrieval_db <- function(retrievals) {
    if (nrow(retrievals) == 0) {
        return(retrievals)
    }
    db_retrievals <- dplyr::tbl(con, dbplyr::in_schema("loggers", "retrieval"))
    db_logger_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
    db_retrievals <- dplyr::left_join(db_retrievals, db_logger_info, by = "logger_id", suffix = c("", ".y"))
    db_individ_info <- dplyr::tbl(con, dbplyr::in_schema("individuals", "individ_info"))
    db_retrievals <- dplyr::left_join(db_retrievals, db_individ_info, by = "individ_id", suffix = c("", ".y"))

    # retrieval_individ_id <- paste(retrievals$euring_code, retrievals$ring_number, sep = "_")
    all_db_retrievals <- dplyr::filter(
        db_retrievals,
        retrieval_date %in% retrievals$date &
            logger_serial_no %in% retrievals$logger_id_retrieved &
            euring_code %in% retrievals$euring_code &
            ring_number %in% retrievals$ring_number
    ) %>%
        dplyr::select(retrieval_date, euring_code, ring_number, logger_serial_no, retrieval_id) %>%
        dplyr::collect()

    db_retrieval_df <- tibble(
        retrieval_date = retrievals$date,
        euring_code = retrievals$euring_code,
        ring_number = retrievals$ring_number,
        logger_serial_no = retrievals$logger_id_retrieved
    )
    db_retrieval_df <- left_join(db_retrieval_df, all_db_retrievals, by = join_by(retrieval_date, euring_code, ring_number, logger_serial_no))

    new_retrievals <- retrievals[is.na(db_retrieval_df$retrieval_id), ]



    # db_retrievals_df <- tibble(
    #     retrieval_date = retrievals$date,
    #     individ_id = paste(retrievals$euring_code, retrievals$ring_number, sep = "_")
    # )
    # new_rows_bool <- check_db_metadata_import(db_retrievals_df, "loggers.retrieval")
    # new_retrievals <- retrievals[new_rows_bool, ]
    return(new_retrievals)
}

#' Check existing deployments in database
#'
#' Function to check which deployments from the master import sheet are not already present in the database.
#' It constructs a dataframe of deployment dates and individ IDs, and uses the `check_db_metadata_import` function
#' to determine which deployments are new and need to be added to the database.
#' @param deployments A tibble containing deployment information from master import metadata.
#' @return A tibble of deployments that are not already present in the database.
#' @concept db_import_utility
#' @export
check_deployment_db <- function(deployments) {
    if (nrow(deployments) == 0) {
        return(deployments)
    }
    db_deployments <- dplyr::tbl(con, dbplyr::in_schema("loggers", "deployment"))
    db_logger_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
    db_deployments <- dplyr::left_join(db_deployments, db_logger_info, by = "logger_id", suffix = c("", ".y"))
    db_individ_info <- dplyr::tbl(con, dbplyr::in_schema("individuals", "individ_info"))
    db_deployments <- dplyr::left_join(db_deployments, db_individ_info, by = "individ_id", suffix = c("", ".y"))


    # deployments_individ_id <- paste(deployments$euring_code, deployments$ring_number, sep = "_")

    all_db_deployments <- dplyr::filter(
        db_deployments,
        deployment_date %in% deployments$date &
            logger_serial_no %in% deployments$logger_id_deployed &
            euring_code %in% deployments$euring_code &
            ring_number %in% deployments$ring_number
    ) %>%
        dplyr::select(deployment_date, euring_code, ring_number, logger_serial_no, deployment_id) %>%
        dplyr::collect()

    db_deployments_df <- tibble(
        deployment_date = deployments$date,
        euring_code = deployments$euring_code,
        ring_number = deployments$ring_number,
        logger_serial_no = deployments$logger_id_deployed
    )
    db_deployments_df <- left_join(db_deployments_df, all_db_deployments, by = join_by(deployment_date, euring_code, ring_number, logger_serial_no))
    # Need to join
    # new_rows_bool <- check_db_metadata_import(db_deployments_df, "loggers.deployment")
    new_deployments <- deployments[is.na(db_deployments_df$deployment_id), ]
    return(new_deployments)
}
