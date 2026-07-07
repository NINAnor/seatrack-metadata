push_startup_file_to_db <- function(file_path) {
    if (!file.exists(file_path)) {
        stop("The specified file does not exist.")
    }

    wb <- openxlsx2::wb_load(file_path)
    startup <- tibble::tibble(wb$to_df())

    # Can split off some of the tests/cleaning in the main prep function for here.

    push_startup(startup)
    push_shutdowns(startup[!is.na(startup$download_date), ])
}

#' Push master import file to database
#'
#' Function to push a master import file to the database. The function loads the master import file, prepares the data for database import, and then pushes the data to the database using the `push_db_import_collections` function.
#' @param colony Optional colony name to filter the master import file.
#' @param file_path Optional file path to a master import Excel file.
#' @param master_sheets Optional list of master import sheets. If not provided, the function will load the master import file using the provided colony and file_path.
#' @param dry_run Logical indicating whether to perform a dry run (default is FALSE). If TRUE, no data will be written to the database.
#' @concept db_import
#' @export
push_master_import_file_to_db <- function(colony = NULL, file_path = NULL, master_sheets = NULL, dry_run = FALSE) {
    log_info_all(paste("Start push attempt for", master_sheets$path))
    if (is.null(master_sheets)) {
        if (is.null(colony) || is.null(file_path)) {
            stop("Either master_sheets or both colony and file_path must be provided.")
        }
        master_sheets <- load_master_import(colony, file_path)
    }

    db_import_collections <- prepare_master_sheet_for_db(master_sheets)
    push_db_import_collections(db_import_collections, dry_run = dry_run)
    log_info_all(paste("Finished database push attempt for", master_sheets$path))
}



#' Push multiple database import collections to database
#'
#' Function to push multiple DBImportCollection objects to the database. Each collection is processed in turn using the `push_db_import_collection` function.
#' @param db_import_collections A list of DBImportCollection objects.
#' @param dry_run Logical indicating whether to perform a dry run (default is FALSE). If TRUE, no data will be written to the database.
#' @concept db_import
push_db_import_collections <- function(db_import_collections, dry_run = FALSE) {
    log_info_all(paste("Upload to database."))
    for (collection_idx in seq_along(db_import_collections)) {
        log_info_all(glue::glue("Pushing session batch {collection_idx}/{length(db_import_collections)}"))
        push_db_import_collection(db_import_collections[[collection_idx]], dry_run = dry_run)
    }
}

#' Push database import collection to database
#'
#' Function to push a DBImportCollection object to the database. The function handles the import of startups, deployments, retrievals, and shutdowns associated with the sessions in the collection.
#' It checks the database for existing entries and only imports new entries to avoid duplicates.
#' It performs checks to make sure sessions are started before a logger is deployed, deployed before it is retrieved and (if it has a deployment) is retrieved before being shutdown.
#' A database error leads to the whole function stopping and no partial imports of the table the database fails on.
#' @param db_import_collection A DBImportCollection object containing session, deployment, and retrieval information.
#' @param dry_run Logical indicating whether to perform a dry run (default is FALSE). If TRUE, no data will be written to the database.
#' @concept db_import
#' @export
push_db_import_collection <- function(db_import_collection, dry_run = FALSE) {
    push_startup(db_import_collection$sessions$sessions, dry_run = dry_run)

    push_deployments(db_import_collection$deployments, dry_run = dry_run)

    push_retrievals(db_import_collection$retrievals, dry_run = dry_run)

    push_shutdowns(db_import_collection$sessions$sessions, dry_run = dry_run)
}

#' Push startups to database
#'
#' Function to push startups from a master import sheet to the database. It checks the database for existing startups and only imports new startups to avoid duplicates.
#' @param startup A tibble containing session information from a DBImportCollection object.
#' @param dry_run Logical indicating whether to perform a dry run (default is FALSE). If TRUE, no data will be written to the database.
#' @concept db_import
#' @export
push_startup <- function(startup, dry_run = FALSE) {
    # startup - check if startup already in database
    startup <- check_startups_db(startup)
    startup <- check_startups_active_db(startup)

    # Check that startups are in the same year as deployment - the db does this too.
    db_sessions <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logging_session"))
    db_logger_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
    db_startup <- dplyr::tbl(con, dbplyr::in_schema("loggers", "startup"))
    db_sessions <- dplyr::left_join(db_sessions, db_logger_info, by = "logger_id")
    db_sessions <- dplyr::left_join(db_sessions, db_startup, by = "session_id")
    db_sessions <- dplyr::filter(db_sessions, active == TRUE & !is.null(starttime_gmt))
    db_sessions <- dplyr::filter(db_sessions, logger_serial_no %in% startup$logger_serial_no)
    db_sessions <- dplyr::select(db_sessions, logger_serial_no, starttime_gmt)
    db_sessions <- tibble::as_tibble(db_sessions)

    if (nrow(db_sessions) > 0) {
        startup_year <- format(startup$starttime_gmt, "%Y")

        db_sessions$startup_year <- startup_year[match(db_sessions$logger_serial_no, startup$logger_serial_no)]

        db_sessions$db_start_year <- format(db_sessions$starttime_gmt, "%Y")
        invalid_loggers <- db_sessions$logger_serial_no[db_sessions$startup_year == db_sessions$db_start_year]
        startup_same_year <- startup[startup$logger_serial_no %in% invalid_loggers, ]
        n_wrong_years <- nrow(startup_same_year)
        if (n_wrong_years > 0) {
            log_warn(glue::glue("The following {n_wrong_years} loggers cannot be registered as being started as there is an existing open session present in the database starting in the same year"))
            log_warn("Attempting to push them to the database would result in an error. These loggers will not be registered as started")

            for (i in seq_len(nrow(startup_same_year))) {
                startup_summary <- startup_same_year[i, c("starttime_gmt", "logger_serial_no")]
                # get db startup
                existing_db_session <- db_sessions[db_sessions$logger_serial_no == startup_summary$logger_serial_no, ]
                startup_summary <- tibble(startup_summary, db_serial_no = existing_db_session$logger_serial_no)
                startup_summary <- tibble(startup_summary, db_starttime_gmt = existing_db_session$starttime_gmt)
                log_warn(
                    startup_summary$logger_serial_no[1],
                    ":\n", paste(capture.output(print(startup_summary, n = nrow(startup_summary)))[c(-1, -3)], collapse = "\n")
                )
            }
            log_warn("- This session needs to be closed before new sessions can be started in the same year")
        }
        startup <- startup[!startup$logger_serial_no %in% invalid_loggers, ]
    }

    if (nrow(startup) > 0) {
        log_info(glue::glue("Pushing {nrow(startup)} startups to database"))
        startup$shutdown_session <- FALSE
        if (!dry_run) {
            tryCatch(
                seatrackR::writeLoggerImport(startup[, 1:15]),
                error = function(e) {
                    log_error("Database error occurred while pushing startups:\n", e$message)
                    stop("Aborting startup push due to database error.")
                }
            )
            log_success(glue::glue("Pushed {nrow(startup)} startups to database"))
        } else {
            log_info_all("Dry run - no data written to database")
        }
    } else {
        log_info_all("No new startups to push to database.")
    }
}

#' Push deployments to database
#'
#' Function to push deployments from a master import sheet to the database. It checks the database for existing deployments and only imports new deployments to avoid duplicates.
#' It also checks that the loggers being deployed have an associated session in the database.
#' @param deployments A tibble containing deployment information from a DBImportCollection object.
#' @param dry_run Logical indicating whether to perform a dry run (default is FALSE). If TRUE, no data will be written to the database.
#' @concept db_import
#' @export
push_deployments <- function(deployments, dry_run = FALSE) {
    deployments <- check_deployment_db(deployments)
    if (nrow(deployments) == 0) {
        log_info_all("No new deployments to push to database.")
        return()
    }

    valid_cols <- DBI::dbListFields(con, DBI::Id(schema = "imports", table = "metadata_import"))
    # check if logger is registered as starting and it makes sense

    db_startups <- dplyr::tbl(con, dbplyr::in_schema("loggers", "startup"))
    db_sessions <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logging_session"))
    db_sessions <- dplyr::left_join(db_sessions, db_startups, by = "session_id", suffix = c("", ".y"))
    db_logger_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
    db_sessions <- dplyr::left_join(db_sessions, db_logger_info, by = "logger_id", suffix = c("", ".y"))

    existing_sessions <- dplyr::filter(
        db_sessions,
        logger_serial_no %in% deployments$logger_id_deployed &
            # is.na(deployment_id) &
            # is.na(retrieval_id) &
            active == TRUE
    ) %>%
        dplyr::select(logger_serial_no, starttime_gmt, programmed_gmt_time, session_id, deployment_id) %>%
        dplyr::collect()

    existing_sessions <- dplyr::mutate(existing_sessions,
        start_time = as.POSIXct(ifelse(!is.na(programmed_gmt_time), programmed_gmt_time, starttime_gmt), tz = "UTC"),
        start_date = as.Date(start_time)
    )
    deploy_startup_df <- data.frame(
        logger_serial_no = deployments$logger_id_deployed,
        deployment_date = deployments$date
    )
    deploy_startup_df <- dplyr::left_join(deploy_startup_df, existing_sessions, by = join_by("logger_serial_no", dplyr::closest(deployment_date >= start_date)))

    deploy_startup_df <- deploy_startup_df[!is.na(deploy_startup_df$start_time) & deploy_startup_df$deployment_date >= deploy_startup_df$start_date, ]

    missing_startups <- !paste(deployments$logger_id_deployed, deployments$date) %in% paste(deploy_startup_df$logger_serial_no, deploy_startup_df$deployment_date)
    deployment_missing_startups <- deployments[missing_startups, ]

    n_missing_startups <- nrow(deployment_missing_startups)

    if (n_missing_startups > 0) {
        log_warn(glue::glue("The following {n_missing_startups} loggers cannot be registered as being deployed as they have no active session, starting before deployment date without an existing deployment in the database."))
        log_warn("Attempting to push them to the database would result in an error. These loggers will not be registered as deployed")

        for (i in seq_len(nrow(deployment_missing_startups))) {
            logger_id_deployed <- deployment_missing_startups$logger_id_deployed[i]
            deployment_summary <- deployment_missing_startups[i, c("date", "ring_number", "euring_code")]

            # get active from db
            db_sessions <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logging_session"))

            db_logger_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
            db_deployment_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "deployment"))
            db_individ_info <- dplyr::tbl(con, dbplyr::in_schema("individuals", "individ_info"))
            db_sessions <- dplyr::left_join(db_sessions, db_startups, by = "session_id", suffix = c("", ".y"))
            db_sessions <- dplyr::left_join(db_sessions, db_individ_info, by = "individ_id", suffix = c("", ".x"))
            db_sessions <- dplyr::left_join(db_sessions, db_logger_info, by = "logger_id", suffix = c("", ".x"))
            db_sessions <- dplyr::filter(db_sessions, logger_serial_no == logger_id_deployed)
            db_sessions <- dplyr::left_join(db_sessions, db_deployment_info, by = "session_id", suffix = c("", ".x"))
            db_summary <- dplyr::select(db_sessions, session_id, db_deployment_date = deployment_date, starttime_gmt, programmed_gmt_time, ring_number, euring_code, active) %>% dplyr::collect()
            if (nrow(db_summary) > 0) {
                log_warn(
                    logger_id_deployed,
                    ":\n", paste(capture.output(print(deployment_summary, n = nrow(deployment_summary)))[c(-1, -3)], collapse = "\n"),
                    logger_id_deployed, " \n existing database sessions", ":\n", paste(capture.output(print(db_summary, n = nrow(db_summary), width = Inf))[c(-1, -3)], collapse = "\n")
                )
            }
        }

        log_warn("- Check which session these deployments should occur in. Is there an error in the metadata sheet?")
    }

    deployments <- deployments[!missing_startups, valid_cols[valid_cols %in% names(deployments)]]
    if (nrow(deployments) == 0) {
        log_info_all("No deployments to push to database.")
        return()
    }

    deploy_existing_df <- deploy_startup_df[is.na(deploy_startup_df$deployment_id), ]
    existing_deployments <- !paste(deployments$logger_id_deployed, deployments$date) %in% paste(deploy_existing_df$logger_serial_no, deploy_existing_df$deployment_date)
    clashing_deployments <- deployments[existing_deployments, ]
    deployment_ids <- deploy_startup_df$deployment_id[match(paste(clashing_deployments$logger_id_deployed, clashing_deployments$date), paste(deploy_startup_df$logger_serial_no, deploy_startup_df$deployment_date))]
    n_clashing_deployments <- nrow(clashing_deployments)

    if (n_clashing_deployments > 0) {
        log_warn(glue::glue("The following {n_clashing_deployments} loggers cannot be registered as being deployed as the logging session already has a deployment."))
        log_warn("Attempting to push them to the database would result in an error. These loggers will not be registered as deployed")

        for (i in seq_len(nrow(clashing_deployments))) {
            logger_id_deployed <- clashing_deployments$logger_id_deployed[i]
            deployment_summary <- clashing_deployments[i, c("date", "ring_number", "euring_code")]
            db_deployment_id <- deployment_ids[i]
            # get active from db
            db_sessions <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logging_session"))

            db_logger_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
            db_deployment_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "deployment"))
            db_individ_info <- dplyr::tbl(con, dbplyr::in_schema("individuals", "individ_info"))
            db_sessions <- dplyr::left_join(db_sessions, db_startups, by = "session_id", suffix = c("", ".y"))
            db_sessions <- dplyr::left_join(db_sessions, db_individ_info, by = "individ_id", suffix = c("", ".x"))
            db_sessions <- dplyr::left_join(db_sessions, db_logger_info, by = "logger_id", suffix = c("", ".x"))
            db_sessions <- dplyr::filter(db_sessions, logger_serial_no == logger_id_deployed, deployment_id == db_deployment_id)
            db_sessions <- dplyr::left_join(db_sessions, db_deployment_info, by = "session_id", suffix = c("", ".x"))
            db_summary <- dplyr::select(db_sessions, session_id, db_deployment_date = deployment_date, starttime_gmt, programmed_gmt_time, ring_number, euring_code, active) %>% dplyr::collect()
            if (nrow(db_summary) > 0) {
                log_warn(
                    logger_id_deployed,
                    ":\n", paste(capture.output(print(deployment_summary, n = nrow(deployment_summary)))[c(-1, -3)], collapse = "\n"),
                    logger_id_deployed, " \n existing database sessions", ":\n", paste(capture.output(print(db_summary, n = nrow(db_summary), width = Inf))[c(-1, -3)], collapse = "\n")
                )
            }
        }

        log_warn("- Check which session these deployments should occur in. Is there an error in the metadata sheet?")
    }

    deployments <- deployments[!existing_deployments, ]
    if (nrow(deployments) == 0) {
        log_info_all("No deployments to push to database.")
        return()
    }

    deployment_years <- as.POSIXct(as.Date(paste0(format(deployments$date, "%Y"), "-01-01")))

    # Check that startups are in the same year as deployment - the db does this too.
    db_sessions <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logging_session"))
    db_logger_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
    db_startup <- dplyr::tbl(con, dbplyr::in_schema("loggers", "startup"))
    db_sessions <- dplyr::left_join(db_sessions, db_logger_info, by = "logger_id")
    db_sessions <- dplyr::left_join(db_sessions, db_startup, by = "session_id")
    db_sessions <- dplyr::filter(db_sessions, logger_serial_no %in% deployments$logger_id_deployed)
    db_sessions <- dplyr::select(db_sessions, logger_serial_no, starttime_gmt)
    db_sessions <- tibble::as_tibble(db_sessions)

    if (nrow(db_sessions) > 0) {
        db_sessions$logger_id_deployed <- deployments$logger_id_deployed[match(db_sessions$logger_serial_no, deployments$logger_id_deployed)]
        db_sessions$deployment_years <- deployment_years[match(db_sessions$logger_serial_no, deployments$logger_id_deployed)]


        valid_loggers <- db_sessions$logger_serial_no[db_sessions$starttime_gmt >= db_sessions$deployment_years]
        deployment_wrong_years <- deployments[!deployments$logger_id_deployed %in% valid_loggers & !grepl("ignore_year", deployments$comment), ]
        n_wrong_years <- nrow(deployment_wrong_years)
        if (n_wrong_years > 0) {
            log_warn(glue::glue("The following {n_wrong_years} loggers cannot be registered as being deployed as the open session present in the database does not start in the year the logger was deployed"))
            log_warn("Attempting to push them to the database would result in an error. These loggers will not be registered as deployed")
            for (i in seq_len(nrow(deployment_wrong_years))) {
                deployment_summary <- deployment_wrong_years[i, c("date", "logger_id_deployed", "comment")]
                log_warn(
                    deployment_summary$logger_id_deployed,
                    ":\n", paste(capture.output(print(deployment_summary, n = nrow(deployment_summary)))[c(-1, -3)], collapse = "\n")
                )
            }
            log_warn("- These loggers may not have the correct session started in the master metadata")
            log_warn("- The startup of this session may not have been registered in the database")
        }

        deployments <- deployments[deployments$logger_id_deployed %in% valid_loggers | grepl("ignore_year", deployments$comment), ]
    }


    if (nrow(deployments) == 0) {
        log_info_all("No new deployments to push to database.")
        return()
    }
    log_info(glue::glue("Pushing {nrow(deployments)} deployments to database"))
    # deployment - check if in db
    if (!dry_run) {
        tryCatch(
            seatrackR::writeMetadata(deployments),
            error = function(e) {
                log_error("Database error occurred while pushing deployments:\n", e$message)
                stop("Aborting deployment push due to database error.")
            }
        )
        log_success(glue::glue("Pushed {nrow(deployments)} deployments to database"))
    } else {
        log_info_all("Dry run - no data written to database")
    }
}

#' Push retrievals to database
#'
#' Function to push retrievals from a master import sheet to the database. It checks the database for existing retrievals and only imports new retrievals to avoid duplicates.
#' It also checks that the loggers being retrieved have an associated deployment in the database.
#' @param retrievals A tibble containing retrieval information from a DBImportCollection object.
#' @param dry_run Logical indicating whether to perform a dry run (default is FALSE). If TRUE, no data will be written to the database.
#' @concept db_import
#' @export
push_retrievals <- function(retrievals, dry_run = FALSE) {
    retrievals <- check_retrieval_db(retrievals)
    if (nrow(retrievals) > 0) {
        valid_cols <- DBI::dbListFields(con, DBI::Id(schema = "imports", table = "metadata_import"))

        # get db tables
        db_sessions <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logging_session"))
        db_deployments <- dplyr::tbl(con, dbplyr::in_schema("loggers", "deployment"))
        db_individ_info <- dplyr::tbl(con, dbplyr::in_schema("individuals", "individ_info"))
        db_sessions <- dplyr::left_join(db_sessions, db_individ_info, by = "individ_id", suffix = c("", ".y"))
        db_sessions <- dplyr::left_join(db_sessions, db_deployments, by = "deployment_id", suffix = c("", ".y"))
        db_logger_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
        db_sessions <- dplyr::left_join(db_sessions, db_logger_info, by = "logger_id", suffix = c("", ".y"))

        retrieval_deploy_df <- data.frame(
            euring_code = retrievals$euring_code,
            ring_number = retrievals$ring_number,
            logger_serial_no = retrievals$logger_id_retrieved
        )

        all_db_deployments <- dplyr::filter(
            db_sessions,
            logger_serial_no %in% retrievals$logger_id_retrieved &
                euring_code %in% retrievals$euring_code &
                ring_number %in% retrievals$ring_number &
                is.na(retrieval_id) &
                active == TRUE
        ) %>%
            dplyr::select(euring_code, ring_number, logger_serial_no, deployment_id) %>%
            dplyr::collect()

        # missing_deployments <- check_db_metadata_import(retrieval_deploy_df, "loggers.logging_session")
        retrieval_deploy_df <- left_join(retrieval_deploy_df, all_db_deployments, by = join_by(euring_code, ring_number, logger_serial_no))
        missing_deployments <- is.na(retrieval_deploy_df$deployment_id)

        retrievals_missing_deployments <- retrievals[missing_deployments, ]

        n_missing_deployments <- nrow(retrievals_missing_deployments)
        if (n_missing_deployments > 0) {
            log_warn(glue::glue("The following {n_missing_deployments} logger/ring combinations cannot be registered as being retrieved as they have no matching deployment events in the database."))
            log_warn("Attempting to push them to the database would result in an error. These loggers will not be registered as retrieved")

            # get logger_deployment_exists
            db_sessions <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logging_session"))
            db_logger_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
            db_sessions <- dplyr::left_join(db_sessions, db_logger_info, by = "logger_id")
            db_logger_info <- dplyr::tbl(con, dbplyr::in_schema("individuals", "individ_info"))
            db_deployment <- dplyr::tbl(con, dbplyr::in_schema("loggers", "deployment"))
            db_sessions <- dplyr::left_join(db_sessions, db_logger_info, by = "individ_id", suffix = c("", ".y"))
            db_sessions <- dplyr::left_join(db_sessions, db_deployment, by = "session_id", suffix = c("", ".y"))
            db_sessions <- dplyr::filter(db_sessions, logger_serial_no %in% retrievals_missing_deployments$logger_id_retrieved & active & !is.na(deployment_id))
            logger_deployment_exists <- dplyr::select(db_sessions, logger_serial_no, db_date = deployment_date, db_ring_number = ring_number) %>% dplyr::collect()

            for (i in seq_len(nrow(retrievals_missing_deployments))) {
                retrieval_summary <- retrievals_missing_deployments[i, c("date", "ring_number", "logger_id_retrieved", "logger_model_retrieved", "comment")]
                missing_logger_id <- retrieval_summary$logger_id_retrieved
                log_warn(
                    missing_logger_id,
                    ":\n", paste(capture.output(print(retrieval_summary, n = nrow(retrieval_summary)))[c(-1, -3)], collapse = "\n")
                )
                logger_db_deployment <- logger_deployment_exists[logger_deployment_exists$logger_serial_no == missing_logger_id & logger_deployment_exists$db_date < retrieval_summary$date, ]
                if (nrow(logger_db_deployment > 0)) {
                    log_warn(
                        "There is a deployment in the database on an open session of this logger, but it does not match the ring number and euring code of the retrieval event. ",
                        ":\n", paste(capture.output(print(logger_db_deployment, n = nrow(logger_db_deployment)))[c(-1, -3)], collapse = "\n")
                    )
                }
            }

            # Check if a retrieval already exists for this logger/ring combo - would indicate a date mismatch.

            log_warn("- Check METADATA sheet for an appropriate deployment event.")
            log_warn("- Check starttime_gmt of a session would contain the deployment event.")
            log_warn("- Check the ring number (and euring_code) of this event. It needs to match the deployment event.")
        }

        retrievals <- retrievals[!missing_deployments, ]
        if (nrow(retrievals) == 0) {
            log_info_all("No new retrievals to push to database.")
            return()
        }
        log_info(glue::glue("Pushing {nrow(retrievals)} retrievals to database"))
        if (!dry_run) {
            tryCatch(
                seatrackR::writeMetadata(retrievals[, valid_cols[valid_cols %in% names(retrievals)]]),
                error = function(e) {
                    log_error("Database error occurred while pushing retrievals:\n", e$message)
                    stop("Aborting retrieval push due to database error.")
                }
            )

            log_success(glue::glue("Pushed {nrow(retrievals)} retrievals to database"))
        } else {
            log_info_all("Dry run - no data written to database")
        }
    } else {
        log_info_all("No new retrievals to push to database.")
        return()
    }
}

#' Push shutdowns to database
#'
#' Function to push shutdowns from a master import sheet to the database. It checks the database for existing shutdowns and only imports new shutdowns to avoid duplicates.
#' It also checks that the loggers being shutdown have an associated retrieval in the database if they have a deployment.
#' It also checks that loggers without deployments are not being shutdown if they are marked as 'Successfully downloaded' or 'Nonresponsive'.
#' @param shutdown A tibble containing session information from a DBImportCollection object.
#' @param dry_run Logical indicating whether to perform a dry run (default is FALSE). If TRUE, no data will be written to the database.
#' @concept db_import
#' @export
push_shutdowns <- function(shutdown, dry_run = FALSE) {
    # shutdown - check if shutdown already in database
    initial_shutdown <- shutdown
    shutdown <- check_shutdown_db(shutdown, check_active = TRUE)

    shutdown$temp_idx <- seq_len(nrow(shutdown))
    # if a session is to be shut down and has a deployment, check for a retrieval
    shutdown_deployment_df <- data.frame(session_id = paste0(shutdown$logger_serial_no, "_", as.Date(shutdown$starttime_gmt)))
    existing_deployments <- !check_db_metadata_import(shutdown_deployment_df, "loggers.deployment")
    shutdowns_with_deployments <- shutdown[existing_deployments & !shutdown$download_type %in% c("Lost", "Failed", "Not used"), ]
    shutdown_retrieval_df <- data.frame(session_id = paste0(shutdowns_with_deployments$logger_serial_no, "_", as.Date(shutdowns_with_deployments$starttime_gmt)))
    missing_retrievals <- check_db_metadata_import(shutdown_retrieval_df, "loggers.retrieval")
    shutdowns_missing_retrievals <- shutdowns_with_deployments[missing_retrievals, ]
    missing_retrievals <- nrow(shutdowns_missing_retrievals)

    if (missing_retrievals > 0) {
        log_warn(glue::glue("The following {missing_retrievals} loggers cannot be registered as shutdown as they have deployment events but no retrieval event in the database."))
        log_warn("Attempting to push them to the database would result in an error. These sessions will not be shutdown.")

        for (i in seq_len(nrow(shutdowns_missing_retrievals))) {
            session_summary <- shutdowns_missing_retrievals[i, c("logger_serial_no", "starttime_gmt", "download_type", "download_date", "shutdown_date")]
            log_warn(
                paste0(session_summary$logger_serial_no, "_", as.Date(session_summary$starttime_gmt)),
                ":\n", paste(capture.output(print(session_summary, n = nrow(session_summary)))[c(-1, -3)], collapse = "\n")
            )
        }

        log_warn("- Check METADATA sheet for an appropriate retrieval event that falls inside the session.")
        log_warn("- Check download date of session would contain the retrieval event.")
        log_warn("- Check if the session has a later shutdown date as well as a download date. Check this date is formatted to be imported safely.")
    }


    shutdown <- shutdown[!shutdown$temp_idx %in% shutdowns_missing_retrievals$temp_idx, ]

    shutdown_deployment_df <- data.frame(session_id = paste0(shutdown$logger_serial_no, "_", as.Date(shutdown$starttime_gmt)))

    no_deployments <- check_db_metadata_import(shutdown_deployment_df, "loggers.deployment")
    shutdown_without_deployments <- shutdown[no_deployments, ]
    downloaded_missing_deployments <- shutdown_without_deployments[shutdown_without_deployments$download_type %in% c("Successfully downloaded", "Reconstructed", "Unsuccessful reconstruction", "Nonresponsive"), ]
    missing_deployments <- nrow(downloaded_missing_deployments)
    if (missing_deployments > 0) {
        log_warn(glue::glue("The following {missing_deployments} loggers cannot be registered as shutdown as they have no deployment events but are flagged as 'Succesfully downloaded', 'Nonresponsive', 'Reconstructed' or 'Unsuccessful reconstruction' ."))
        log_warn("Attempting to push them to the database would result in an error. These sessions will not be shutdown.")

        for (i in seq_len(nrow(downloaded_missing_deployments))) {
            session_summary <- downloaded_missing_deployments[i, c("logger_serial_no", "starttime_gmt", "download_type", "download_date", "shutdown_date")]
            log_warn(
                paste0(session_summary$logger_serial_no, "_", as.Date(session_summary$starttime_gmt)),
                ":\n", paste(capture.output(print(session_summary, n = nrow(session_summary)))[c(-1, -3)], collapse = "\n")
            )
        }
    }
    shutdown <- shutdown[!shutdown$temp_idx %in% downloaded_missing_deployments$temp_idx, ]

    if (nrow(shutdown) > 0) {
        log_info(glue::glue("Pushing {nrow(shutdown)} shutdowns to database"))
        # Tell the DB the session is being shutdown
        shutdown$intended_species <- NA
        shutdown$shutdown_session <- TRUE
        if (!dry_run) {
            tryCatch(
                seatrackR::writeLoggerImport(shutdown[, 1:22]),
                error = function(e) {
                    log_error("Database error occurred while pushing shutdowns:\n", e$message)
                    stop("Aborting shutdown push due to database error.")
                }
            )
            log_success(glue::glue("Pushed {nrow(shutdown)} shutdowns to database"))
        } else {
            log_info_all("Dry run - no data written to database")
        }
    } else {
        log_info_all("No new shutdowns to push to database.")
    }
}
