check_startup_rows <- function(startup_shutdown) {
    if (nrow(startup_shutdown) == 0) {
        stop("No valid sessions! Cannot proceed with database import.")
    }
}

#' Prepare master sheet for database import
#'
#' This function prepares the master import sheet for database import by processing the metadata and startup/shutdown information.
#' It checks the database for existing sessions and categorizes them into sessions to be closed, opened, or both opened and closed.
#' It then prepares session batches for database import. Metadata is linked to sessions within these batches.
#' @param master_sheets A LoadedWBCollection object containing the master import sheets.
#' @return A vector of DBImportCollection objects representing the prepared session batches for database import.
#' @export
#' @concept db_import_prep
prepare_master_sheet_for_db <- function(master_sheets) {
    # check morph
    fix_num_col <- function(num_col) {
        if (!is.numeric(num_col)) {
            num_col <- gsub(",", ".", num_col, fixed = TRUE)
            num_col <- gsub("[^0-9.]", "", num_col)
            num_col <- as.numeric(num_col)
        }
        return(num_col)
    }
    log_info_all(paste("Prepare", master_sheets$path, "for database upload"))
    metadata <- master_sheets$data$METADATA
    metadata <- metadata[order(metadata$date), ]

    # Temporarily bring back scull until we fix the name
    if("skull" %in% names(metadata)){
        metadata <- dplyr::rename(metadata, scull = skull)
    }

    metadata$ring_number <- as.character(metadata$ring_number)

    startup_shutdown <- master_sheets$data$STARTUP_SHUTDOWN
    # remove cases with no startup date. For some loggers, this should be inferred from the deployment date. This can happen here.

    original_count <- nrow(startup_shutdown)



    # Check shutdowns
    problem_shutdown <- startup_shutdown[is.na(startup_shutdown$starttime_gmt), ]
    if (nrow(problem_shutdown) > 0) {
        startup_shutdown <- startup_shutdown[!is.na(startup_shutdown$starttime_gmt), ]
        log_warn(paste("Removed ", original_count - nrow(startup_shutdown), " rows with no startup date."))

        row_summary <- problem_shutdown[, c("logger_serial_no", "starttime_gmt", "download_date", "shutdown_date")]
        log_warn("The following rows have no startup date and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
    }
    check_startup_rows(startup_shutdown)
    startup_shutdown <- startup_shutdown[order(startup_shutdown$starttime_gmt), ]

    valid_sex <- c("male", "female", "unknown", NA)
    sex_alias <- data.frame(db_name = c("male", "female", "unknown"), alias = c("m", "f", "u"))

    # check euring code
    problem_ring_bool <- is.na(metadata$ring_number) | is.na(metadata$euring_code)
    if (sum(problem_ring_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_ring_bool), " rows with missing ring number or euring code."))
        row_summary <- metadata[problem_ring_bool, c("date", "ring_number", "euring_code")]
        log_warn("The following rows have missing ring number or euring code and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!problem_ring_bool, ]
    }
    # Check people

    db_names <- dplyr::pull(seatrackR::getNames(), "name")
    startup_shutdown$started_by <- gsub("/", "_", startup_shutdown$started_by, fixed = TRUE)
    startup_shutdown$intended_deployer <- gsub("/", "_", startup_shutdown$intended_deployer, fixed = TRUE)
    metadata$data_responsible <- gsub("/", "_", metadata$data_responsible, fixed = TRUE)

    startup_shutdown$started_by <- gsub(" & ", "_", startup_shutdown$started_by, fixed = TRUE)
    startup_shutdown$intended_deployer <- gsub(" & ", "_", startup_shutdown$intended_deployer, fixed = TRUE)
    metadata$data_responsible <- gsub(" & ", "_", metadata$data_responsible, fixed = TRUE)

    startup_shutdown$started_by <- gsub(" _ ", "_", startup_shutdown$started_by, fixed = TRUE)
    startup_shutdown$intended_deployer <- gsub(" _ ", "_", startup_shutdown$intended_deployer, fixed = TRUE)
    metadata$data_responsible <- gsub(" _ ", "_", metadata$data_responsible, fixed = TRUE)

    # Startup not so important, can try and simplify it
    startup_shutdown$started_by <- sapply(startup_shutdown$started_by, function(x) {
        names <- trimws(strsplit(x, "_")[[1]])
        preferred <- names[names %in% c(db_names)]
        if (length(preferred) > 0) preferred[1] else x
    })
    problem_names_bool <- !startup_shutdown$started_by %in% c(NA, db_names)
    if (sum(problem_names_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_names_bool), " rows with invalid started_by names."))
        row_summary <- startup_shutdown[problem_names_bool, c("logger_serial_no", "starttime_gmt", "started_by")]
        log_warn("The following rows have invalid started_by value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        startup_shutdown <- startup_shutdown[!problem_names_bool, ]
    }
    problem_names_bool <- !startup_shutdown$intended_deployer %in% c(NA, db_names)
    if (sum(problem_names_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_names_bool), " rows with invalid intended_deployer names."))
        row_summary <- startup_shutdown[problem_names_bool, c("logger_serial_no", "starttime_gmt", "intended_deployer")]
        log_warn("The following rows have invalid intended_deployer value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        startup_shutdown <- startup_shutdown[!problem_names_bool, ]
    }

    check_startup_rows(startup_shutdown)

    problem_names_bool <- !metadata$data_responsible %in% db_names
    if (sum(problem_names_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_names_bool), " rows with invalid data_responsible names."))
        row_summary <- metadata[problem_names_bool, c("date", "ring_number", "data_responsible", "logger_id_deployed", "logger_id_retrieved")]
        log_warn("The following rows have invalid data_responsible value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!problem_names_bool, ]
    }



    # Check sex
    metadata$sex <- tolower(metadata$sex)
    invalid_sex_bool <- !metadata$sex %in% valid_sex
    if (sum(invalid_sex_bool) > 0) {
        metadata$sex[invalid_sex_bool] <- sex_alias$db_name[match(metadata$sex[invalid_sex_bool], sex_alias$alias)]
        invalid_sex_bool <- !metadata$sex %in% valid_sex
        problem_sex <- metadata[invalid_sex_bool, ]
        if (nrow(problem_sex) > 0) {
            metadata <- metadata[!invalid_sex_bool, ]
            log_warn(paste("Removed ", nrow(problem_sex), " rows with invalid sex."))
            row_summary <- problem_sex[, c("date", "ring_number", "sex", "logger_id_deployed", "logger_id_retrieved")]
            log_warn("The following rows have invalid sex value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        }
        metadata <- metadata[!invalid_sex_bool, ]
    }

    # Check sexing method

    # check hatching success/breeding success
    metadata$hatching_success <- as.logical(metadata$hatching_success)
    metadata$breeding_success <- as.logical(metadata$breeding_success)

    metadata$chicks <- fix_num_col(metadata$chicks)
    metadata$eggs <- fix_num_col(metadata$eggs)

    lat_lon_cols <- c("colony_latitude", "colony_longitude", "nest_latitude", "nest_longitude")
    for (col in lat_lon_cols) {
        # If column exists
        if (col %in% names(metadata)) {
            # Remove non-numeric characters and convert to numeric
            metadata[[col]] <- as.numeric(gsub("[^0-9.-]", "", metadata[[col]]))
        }
    }

    # check breeding stage
    db_breeding_table <- dplyr::tbl(con, dbplyr::in_schema("metadata", "breeding_stages"))
    valid_breeding_stages <- dplyr::pull(db_breeding_table, "breeding_stage")
    invalid_breeding_bool <- !is.na(metadata$breeding_stage) & !metadata$breeding_stage %in% valid_breeding_stages
    if (sum(invalid_breeding_bool) > 0) {
        problem_breed <- metadata[invalid_breeding_bool, ]
        log_warn(paste("Removed ", nrow(problem_breed), " rows with invalid breeding stages."))
        row_summary <- problem_breed[, c("date", "ring_number", "breeding_stage", "logger_id_deployed", "logger_id_retrieved")]
        log_warn("The following rows have invalid breeding_stage value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!invalid_breeding_bool, ]
    }


    metadata$tarsus <- fix_num_col(metadata$tarsus)
    metadata$scull <- fix_num_col(metadata$scull)
    metadata$weight <- fix_num_col(metadata$weight)
    metadata$wing <- fix_num_col(metadata$weight)

    # check back_on_nest
    back_on_nest <- metadata$back_on_nest
    back_on_nest[back_on_nest == "yes"] <- TRUE
    back_on_nest[back_on_nest == "no"] <- FALSE
    metadata$back_on_nest <- as.logical(back_on_nest)

    # check eggs
    metadata$eggs <- fix_num_col(metadata$eggs)

    # Check colony
    db_colonies <- seatrackR::getColonies()$colony_int_name
    problem_colony_bool <- !metadata$colony %in% db_colonies
    if (sum(problem_colony_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_colony_bool), " rows with invalid colony names."))
        row_summary <- metadata[problem_colony_bool, c("date", "ring_number", "colony", "logger_id_deployed", "logger_id_retrieved")]
        log_warn("The following rows have invalid colony value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!problem_colony_bool, ]
    }

    db_locs <- seatrackR::getColonies(allLocations = TRUE)$location_name
    problem_colony_bool <- !metadata$colony %in% db_locs
    if (sum(problem_colony_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_colony_bool), " rows with invalid location names."))
        row_summary <- metadata[problem_colony_bool, c("date", "ring_number", "colony", "logger_id_deployed", "logger_id_retrieved")]
        log_warn("The following rows have invalid location value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!problem_colony_bool, ]
    }

    # Check mounting types
    metadata$logger_mount_method[!is.na(metadata$logger_mount_method)] <- tolower(metadata$logger_mount_method[!is.na(metadata$logger_mount_method)])

    db_mounting_table <- dplyr::tbl(con, dbplyr::in_schema("metadata", "mounting_types"))
    valid_mountings <- dplyr::pull(db_mounting_table, "logger_mount_method")
    invalid_mounting_bool <- !is.na(metadata$logger_mount_method) & !metadata$logger_mount_method %in% valid_mountings
    if (sum(invalid_mounting_bool) > 0) {
        problem_mount <- metadata[invalid_mounting_bool, ]
        log_warn(paste("Removed ", nrow(problem_mount), " rows with invalid mounting types."))
        row_summary <- problem_mount[, c("date", "ring_number", "logger_mount_method", "logger_id_deployed", "logger_id_retrieved")]
        log_warn("The following rows have invalid mounting value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!invalid_mounting_bool, ]
    }

    # check species
    db_species <- seatrackR::getSpecies()$species_name_eng
    startup_shutdown$intended_species <- gsub(" adults", "", startup_shutdown$intended_species)
    startup_shutdown$intended_species <- gsub(" chicks", "", startup_shutdown$intended_species)

    problem_species_bool <- !startup_shutdown$intended_species %in% c(db_species, NA)
    if (sum(problem_species_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_species_bool), " rows with invalid species."))
        row_summary <- startup_shutdown[problem_species_bool, c("starttime_gmt", "logger_serial_no", "intended_species")]
        log_warn("The following rows have invalid species value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        startup_shutdown <- startup_shutdown[!problem_species_bool, ]
    }
    check_startup_rows(startup_shutdown)

    problem_species_bool <- !metadata$species %in% db_species
    if (sum(problem_species_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_species_bool), " rows with invalid species."))
        row_summary <- metadata[problem_species_bool, c("date", "ring_number", "species", "logger_id_deployed", "logger_id_retrieved")]
        log_warn("The following rows have invalid species value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!problem_species_bool, ]
    }

    # Check species mismatch
    species_ring <- dplyr::distinct(metadata, species, ring_number)
    problem_rings <- species_ring$ring_number[duplicated(species_ring$ring_number)]
    if (length(problem_rings) > 0) {
        problem_rings_bool <- metadata$ring_number %in% problem_rings
        log_warn(paste("Removed ", sum(problem_rings_bool), " rows with species/ring number mismatch."))
        row_summary <- metadata[problem_rings_bool, c("date", "ring_number", "species")]
        log_warn("The following rows have species/ring number mismatch and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        metadata <- metadata[!problem_rings_bool, ]
    }

    # Check logger model ID
    db_logger_model <-
        data.frame(
            logger_serial_no = c(metadata$logger_id_deployed, metadata$logger_id_retrieved, startup_shutdown$logger_serial_no),
            logger_model = c(metadata$logger_model_deployed, metadata$logger_model_retrieved, startup_shutdown$logger_model)
        )
    db_logger_model <- db_logger_model[!is.na(db_logger_model$logger_serial_no), ]
    db_logger_model <- dplyr::distinct(db_logger_model)

    logger_model_bool <- check_db_metadata_import(db_logger_model, "loggers.logger_info")
    missing_logger_models <- db_logger_model[logger_model_bool, ]

    # Ignore cases where the logger model is not in the database at all
    db_logger <- data.frame(logger_serial_no = missing_logger_models$logger_serial_no)
    logger_exists_bool <- check_db_metadata_import(db_logger, "loggers.logger_info")
    mismatch_logger_models <- missing_logger_models[!logger_exists_bool, ]
    if (nrow(mismatch_logger_models) > 0) {
        # Correct these
        db_true_logger_model <- get_db_metadata_import(data.frame(logger_serial_no = unique(mismatch_logger_models$logger_serial_no)), "loggers.logger_info", additional_db_col_names = c("logger_model", "production_year"))
        n_mismatch <- nrow(db_true_logger_model)
        if (n_mismatch > 0) {
            model_summary <- tibble::as_tibble(db_true_logger_model)
            ambiguity_loggers <- unique(model_summary$logger_serial_no[duplicated(model_summary$logger_serial_no)])
            n_ambiguity <- length(ambiguity_loggers)
            if (n_ambiguity > 0) {
                ambiguity_loggers_summary <- model_summary[model_summary$logger_serial_no %in% ambiguity_loggers, ]
                log_warn(
                    glue::glue("{n_ambiguity} logger serial numbers have a mismatch between the master sheet. \n However, there are multiple different logger models for these serial numbers in the database."), "\n",
                    paste(capture.output(print(ambiguity_loggers_summary, n = nrow(ambiguity_loggers_summary)))[c(-1, -3)], collapse = "\n")
                )
                log_warn("These cannot be corrected from the database. Master metadata must be updated to resolve this ambiguity.")
            }
            model_summary <- model_summary[!model_summary$logger_serial_no %in% ambiguity_loggers, ]

            model_summary$md_logger_model <- missing_logger_models$logger_model[match(model_summary$logger_serial_no, missing_logger_models$logger_serial_no)]
            model_summary <- model_summary[model_summary$md_logger_model != model_summary$logger_model | is.na(model_summary$md_logger_model), ]
            log_warn(glue::glue("{n_mismatch} logger deployments/retrievals had a mismatch between the master sheet and the database."), "\n", paste(capture.output(print(model_summary, n = nrow(model_summary)))[c(-1, -3)], collapse = "\n"))
            log_warn("Database value will be used. Consider updating master metadata")

            metadata$logger_model_retrieved[metadata$logger_id_retrieved %in% db_true_logger_model$logger_serial_no] <- db_true_logger_model$logger_model[match(metadata$logger_id_retrieved[metadata$logger_id_retrieved %in% db_true_logger_model$logger_serial_no], db_true_logger_model$logger_serial_no)]
            metadata$logger_model_deployed[metadata$logger_id_deployed %in% db_true_logger_model$logger_serial_no] <- db_true_logger_model$logger_model[match(metadata$logger_id_deployed[metadata$logger_id_deployed %in% db_true_logger_model$logger_serial_no], db_true_logger_model$logger_serial_no)]
            startup_shutdown$logger_model[startup_shutdown$logger_serial_no %in% db_true_logger_model$logger_serial_no] <- db_true_logger_model$logger_model[match(startup_shutdown$logger_serial_no[startup_shutdown$logger_serial_no %in% db_true_logger_model$logger_serial_no], db_true_logger_model$logger_serial_no)]
            startup_shutdown$production_year[startup_shutdown$logger_serial_no %in% db_true_logger_model$logger_serial_no] <- db_true_logger_model$production_year[match(startup_shutdown$logger_serial_no[startup_shutdown$logger_serial_no %in% db_true_logger_model$logger_serial_no], db_true_logger_model$logger_serial_no)]
        }
    }

    # Check for invalid logger models and producers
    db_model_producer <- seatrackR::getLoggerModels()

    # Check for logger models that don't exist in the database
    problem_logger_models <- unique(startup_shutdown$logger_model[!startup_shutdown$logger_model %in% db_model_producer$model])

    n_problem_logger_models <- length(problem_logger_models)
    if (n_problem_logger_models > 0) {
        # Try a case insensitive match of these models
        correct_case <- db_model_producer$model[match(tolower(problem_logger_models), tolower(db_model_producer$model))]
        fixable <- data.frame(md_model = problem_logger_models[!is.na(correct_case)], db_model = correct_case[!is.na(correct_case)])

        metadata$logger_model_retrieved[metadata$logger_model_retrieved %in% fixable$md_model] <- fixable$db_model[match(metadata$logger_model_retrieved[metadata$logger_model_retrieved %in% fixable$md_model], fixable$md_model)]
        metadata$logger_model_deployed[metadata$logger_model_deployed %in% fixable$md_model] <- fixable$db_model[match(metadata$logger_model_deployed[metadata$logger_model_deployed %in% fixable$md_model], fixable$md_model)]
        startup_shutdown$logger_model[startup_shutdown$logger_model %in% fixable$md_model] <- fixable$db_model[match(startup_shutdown$logger_model[startup_shutdown$logger_model %in% fixable$md_model], fixable$md_model)]

        problem_logger_models <- problem_logger_models[!problem_logger_models %in% fixable$md_model]
    }

    n_problem_logger_models <- length(problem_logger_models)
    if (n_problem_logger_models > 0) {
        id_problem_startups <- startup_shutdown$logger_model %in% problem_logger_models
        missing_model_summary <- startup_shutdown[id_problem_startups, c("logger_serial_no", "logger_model", "starttime_gmt")]
        log_warn(glue::glue("{nrow(missing_model_summary)} logging sessions have logger models not present in database."), "\n", paste(capture.output(print(missing_model_summary, n = nrow(missing_model_summary)))[c(-1, -3)], collapse = "\n"))
        startup_shutdown <- startup_shutdown[!id_problem_startups, ]
    }

    startup_shutdown$producer <- db_model_producer$producer[match(startup_shutdown$logger_model, db_model_producer$model)]

    # Check for remaining model mismatch
    db_logger_model <-
        data.frame(
            logger_serial_no = c(metadata$logger_id_deployed, metadata$logger_id_retrieved, startup_shutdown$logger_serial_no),
            logger_model = c(metadata$logger_model_deployed, metadata$logger_model_retrieved, startup_shutdown$logger_model)
        )
    db_logger_model <- db_logger_model[!is.na(db_logger_model$logger_serial_no), ]



    problem_logger_models_bool <- sapply(unique(db_logger_model$logger_serial_no), function(logger_serial_no) {
        length(unique(db_logger_model$logger_model[db_logger_model$logger_serial_no == logger_serial_no])) > 1
    })

    problem_logger_ids <- unique(db_logger_model$logger_serial_no)[problem_logger_models_bool]
    problem_logger_model_string <- sapply(problem_logger_ids, function(logger_serial_no) {
        logger_model_string <- unique(db_logger_model$logger_model[db_logger_model$logger_serial_no == logger_serial_no])
        logger_model_strings <- sapply(logger_model_string, function(x) {
            glue::glue("'{x}'")
        })
        paste0(logger_serial_no, ": ", paste(logger_model_strings, collapse = ", "))
    })
    if (length(problem_logger_ids) > 0) {
        log_warn(paste0("The following logger serial numbers have multiple different logger models in the master sheet. This could cause a database error.:\n", paste(problem_logger_model_string, collapse = "\n")))
    }

    # Check production_year
    problem_production_year_bool <- is.na(startup_shutdown$production_year) | startup_shutdown$production_year < 1900 | startup_shutdown$production_year > as.numeric(format(Sys.Date(), "%Y"))
    if (sum(problem_production_year_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_production_year_bool), " rows with invalid production year."))
        row_summary <- startup_shutdown[problem_production_year_bool, c("logger_serial_no", "logger_model", "production_year", "starttime_gmt")]
        log_warn("The following rows have invalid production year and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        startup_shutdown <- startup_shutdown[!problem_production_year_bool, ]
    }

    # check download status

    startup_shutdown$download_type <- translate_logger_status(startup_shutdown$download_type)
    # Theoretically no other download statuses should be allowed, but perhaps check?
    db_download_status <- translate_logger_status(list_valid_only = TRUE)
    problem_download_bool <- !startup_shutdown$download_type %in% c(db_download_status) & (!is.na(startup_shutdown$download_date) | !is.na(startup_shutdown$shutdown_date))
    if (sum(problem_download_bool) > 0) {
        log_warn(paste("Removed ", sum(problem_download_bool), " rows with invalid download status."))
        row_summary <- startup_shutdown[problem_download_bool, c("starttime_gmt", "logger_serial_no", "download_type")]
        log_warn("The following rows have invalid download_type value and will not be handled", ":\n", paste(capture.output(print(row_summary, n = nrow(row_summary)))[c(-1, -3)], collapse = "\n"))
        startup_shutdown <- startup_shutdown[!problem_download_bool, ]
    }
    check_startup_rows(startup_shutdown)

    # check retrieval type

    startup_shutdown <- startup_shutdown[order(startup_shutdown$starttime_gmt), ]
    # combine download/shutdown
    startup_shutdown$download_date[is.na(startup_shutdown$download_date)] <- startup_shutdown$shutdown_date[is.na(startup_shutdown$download_date)]

    startup_shutdown_id <- paste(startup_shutdown$logger_serial_no, startup_shutdown$starttime_gmt)

    # check db for which sessions are already in the correct state
    shutdown_new <- check_shutdown_db(startup_shutdown)

    startup_new <- check_startups_db(startup_shutdown)

    # As the above only checks startups and shutdowns, we need to check for sessions that might have missing deployments or retrievals
    log_info("Check deployments")
    deployments_new <- prepare_session_deployments(startup_shutdown, metadata, TRUE, TRUE)
    missing_deployment_ids <- paste(deployments_new$sessions$logger_serial_no[!is.na(deployments_new$sessions$logger_serial_no)], deployments_new$sessions$starttime_gmt[!is.na(deployments_new$sessions$logger_serial_no)])

    log_info("Check retrievals")
    retrievals_new <- prepare_session_retrievals(startup_shutdown, metadata, TRUE, TRUE)
    missing_retrieval_ids <- paste(retrievals_new$sessions$logger_serial_no[!is.na(retrievals_new$sessions$logger_serial_no)], retrievals_new$sessions$starttime_gmt[!is.na(retrievals_new$sessions$logger_serial_no)])

    # which of our closed sessions are open sessions in the database vs. not existing at all in the database?
    db_shutdown_starts_df <-
        data.frame(
            session_id = paste(shutdown_new$logger_serial_no, as.Date(shutdown_new$starttime_gmt), sep = "_"),
            starttime_gmt = shutdown_new$starttime_gmt
        )
    closed_sessions_bool <- check_db_metadata_import(db_shutdown_starts_df, "loggers.startup")

    # rows of sessions that are currently open in the database and will only be closed
    db_to_close_only <- shutdown_new[!closed_sessions_bool, ]
    log_info(nrow(db_to_close_only), " sessions to close")

    # rows of sessions that currently don't exist in the database and therefore need to be started first before being closed.
    db_to_open_and_close <- shutdown_new[closed_sessions_bool, ]
    log_info(nrow(db_to_open_and_close), " sessions to open and close")

    # row of sessions that will be opened in the database and not closed
    db_to_open_only <- startup_new[is.na(startup_new$download_date), ]
    log_info(nrow(db_to_open_only), " sessions to open")

    # neither db_to_close or db_to_open_only should have any duplicates. Remove these and warn the user
    # # THIS SHOULDN'T REALLY HAPPEN - but it does!
    # duplicate_to_close_loggers <- unique(paste(db_to_close_only$logger_serial_no, db_to_close_only$logger_model)[duplicated(paste(db_to_close_only$logger_serial_no, db_to_close_only$logger_model))])
    # if (length(duplicate_to_close_loggers) > 0) {
    #     log_warn(
    #         "The following logger serial numbers have multiple close entries in the import sheet that cannot be added to the database: "
    #     )

    #     for (duplicate_logger in duplicate_to_close_loggers) {
    #         duplicate_summary <- db_to_close_only[paste(db_to_close_only$logger_serial_no, db_to_close_only$logger_model) == duplicate_logger, c("logger_serial_no", "starttime_gmt", "download_date")]
    #         log_warn(duplicate_logger, ":\n", paste(capture.output(print(duplicate_summary, n = nrow(duplicate_summary)))[c(-1, -3)], collapse = "\n"))
    #     }

    #     db_to_close_only <- db_to_close_only[!db_to_close_only$logger_serial_no %in% duplicate_to_close_loggers, ]
    #     log_warn("These sessions will not be added to the database. Check if these sessions can be closed in the master import sheet.")
    # }
    # # WOULD SUGGEST MULTIPLE OPEN SESSIONS IN THE DB - this can happen! Or overlapping sessions within the master sheet

    # duplicate_to_open_loggers <- unique(db_to_open_only$logger_serial_no[duplicated(paste(db_to_open_only$logger_serial_no, db_to_open_only$logger_model))])
    # if (length(duplicate_to_open_loggers) > 0) {
    #     log_warn(
    #         "The following logger serial numbers have multiple open startup entries in the import sheet that cannot be added to the database: ",
    #         paste(duplicate_to_open_loggers, collapse = ", ")
    #     )

    #     for (duplicate_logger in duplicate_to_open_loggers) {
    #         duplicate_summary <- db_to_open_only[db_to_open_only$logger_serial_no == duplicate_logger, c("logger_serial_no", "starttime_gmt", "download_date")]
    #         log_warn(duplicate_logger, ":\n", paste(capture.output(print(duplicate_summary, n = nrow(duplicate_summary)))[c(-1, -3)], collapse = "\n"))
    #     }

    #     db_to_open_only <- db_to_open_only[!db_to_open_only$logger_serial_no %in% duplicate_to_open_loggers, ]

    #     log_warn("These sessions will not be added to the database. Check if these sessions can be closed in the master import sheet.")
    # }

    # Combine all db IDs from these three dataframes
    all_db_ids <- paste(
        c(
            db_to_close_only$logger_serial_no,
            db_to_open_only$logger_serial_no,
            db_to_open_and_close$logger_serial_no
        ),
        c(
            db_to_close_only$starttime_gmt,
            db_to_open_only$starttime_gmt,
            db_to_open_and_close$starttime_gmt
        )
    )

    # Check which of the missing deployment/retrieval only sessions are still missing after accounting for sessions being opened/closed here
    still_missing_retrieval_ids <- missing_retrieval_ids[!missing_retrieval_ids %in% all_db_ids]
    still_missing_deployment_ids <- missing_deployment_ids[!missing_deployment_ids %in% all_db_ids]

    # Sort these by type
    retrieval_only_ids <- startup_shutdown_id[startup_shutdown_id %in% still_missing_retrieval_ids & !startup_shutdown_id %in% still_missing_deployment_ids]
    deployment_only_ids <- startup_shutdown_id[!startup_shutdown_id %in% still_missing_retrieval_ids & startup_shutdown_id %in% still_missing_deployment_ids]
    deployment_retrieval_ids <- startup_shutdown_id[startup_shutdown_id %in% still_missing_retrieval_ids & startup_shutdown_id %in% still_missing_deployment_ids]

    # reinsert these
    if (length(retrieval_only_ids) > 0) {
        log_info(glue::glue("Reinserting {length(retrieval_only_ids)} sessions due to missing retrievals"))
        db_to_close_only <- rbind(db_to_close_only, startup_shutdown[startup_shutdown_id %in% retrieval_only_ids, ])
    }
    if (length(deployment_only_ids) > 0) {
        log_info(glue::glue("Reinserting {length(deployment_only_ids)} sessions due to missing deployments"))
        db_to_open_only <- rbind(db_to_open_only, startup_shutdown[startup_shutdown_id %in% deployment_only_ids, ])
    }
    if (length(deployment_retrieval_ids) > 0) {
        log_info(glue::glue("Reinserting {length(deployment_retrieval_ids)} sessions due to missing deployments and retrievals"))
        db_to_open_and_close <- rbind(db_to_open_and_close, startup_shutdown[startup_shutdown_id %in% deployment_retrieval_ids, ])
    }



    # batch open closed sessions
    db_to_open_and_close_list <- list()
    db_to_open_and_close_list <- c(db_to_open_and_close_list, list(db_to_open_and_close))

    while (any(duplicated(paste(db_to_open_and_close_list[[length(db_to_open_and_close_list)]]$logger_serial_no, db_to_open_and_close_list[[length(db_to_open_and_close_list)]]$logger_model)))) {
        current_index <- length(db_to_open_and_close_list)
        current_df <- db_to_open_and_close_list[[length(db_to_open_and_close_list)]]
        first_df <- current_df[!duplicated(paste(current_df$logger_serial_no, current_df$logger_model)), ]
        new_df <- current_df[duplicated(paste(current_df$logger_serial_no, current_df$logger_model)), ]
        db_to_open_and_close_list[[current_index]] <- first_df
        db_to_open_and_close_list <- c(db_to_open_and_close_list, list(new_df))
    }

    db_to_open_and_close_sessions <- sapply(db_to_open_and_close_list, SessionBatch$new, type = "open_and_close")
    log_info("Split sessions to open and close into ", length(db_to_open_and_close_sessions), " sessions.")

    session_batches <- c(SessionBatch$new(db_to_close_only, "close_only"), db_to_open_and_close_sessions, SessionBatch$new(db_to_open_only, "open_only"))
    session_retrieval_deployments <- prepare_session_batches(session_batches, metadata)
    return(session_retrieval_deployments)
}

#' Prepare session batches for database import
#'
#' This function prepares a list of session batches for database import by processing each batch and extracting relevant metadata.
#' It filters out any batches that do not contain sessions and returns a list of prepared session batches.
#' It adds deployments and retrievals associated with the sessions in each batch.
#' In cases where sessions have multiple deployments or retrievals, those sessions are removed from the batch and a warning is logged.
#' @param session_batches A vector of SessionBatch objects to be prepared for database import.
#' @param metadata A dataframe containing metadata information for the sessions.
#' @return A vector of DBImportCollection objects representing the prepared session batches for database import.
#' @export
#' @concept db_import_prep
prepare_session_batches <- function(session_batches, metadata) {
    log_info("Preparing ", length(session_batches), " session batches for database import.")
    session_metadata_batches <- sapply(session_batches, prepare_session_batch, metadata = metadata)
    session_metadata_batches <- session_metadata_batches[which(!sapply(session_metadata_batches, is.null))]
    return(session_metadata_batches)
}

#' Prepare session batch for database import
#'
#' This function prepares a session batch for database import by processing each batch and extracting relevant metadata.
#' It adds deployments and retrievals associated with the sessions in each batch.
#' In cases where sessions have multiple deployments or retrievals, those sessions are removed from the batch and a warning is logged.
#' @param session_batch A SessionBatch object to be prepared for database import.
#' @param metadata A dataframe containing metadata information for the sessions.
#' @return DBImportCollection object representing the prepared session batches for database import.
#' @export
#' @concept db_import_prep
prepare_session_batch <- function(session_batch, metadata) {
    if (nrow(session_batch$sessions) == 0) {
        log_info(glue::glue("No sessions available. Sessions would be {gsub('_',' ', session_batch$type)}"))
        return(NULL)
    }
    original_count <- nrow(session_batch$sessions)
    log_info(glue::glue("Prepare {original_count}. Sessions will be {gsub('_',' ', session_batch$type)}"))

    deployment_results <- prepare_session_deployments(session_batch$sessions, metadata, duplicate_warnings = FALSE)
    deployments <- deployment_results$deployments
    session_batch$sessions <- deployment_results$sessions

    retrieval_results <- prepare_session_retrievals(session_batch$sessions, metadata, duplicate_warnings = FALSE)
    retrievals <- retrieval_results$retrievals
    session_batch$sessions <- retrieval_results$sessions

    # as we may have lost sessions, double check.
    deployment_results <- prepare_session_deployments(session_batch$sessions, metadata, duplicate_warnings = FALSE)
    deployments <- deployment_results$deployments
    session_batch$sessions <- deployment_results$sessions


    new_count <- nrow(session_batch$sessions)
    log_success(glue::glue("Removed {original_count - new_count} sessions. {new_count} sessions prepared.."))


    return(DBImportCollection$new(
        session_batch,
        retrievals,
        deployments
    ))
}

#' Get open session dates
#' Convenience function to get the open date for a set of logger sessions.
#' If the shutdown date is available, use that. If not, use the download date. If both exist, use the earlier date.
#' If there is another session from the same logger, use the start of that.
#' If neither are available, use a date far in the future to indicate the session is still open.
#' @param sessions A tibble containing session information from master import startup_shutdown.
#' @return A vector of POSIXct dates representing the open dates for the sessions.
#' @concept utility
get_open_session_dates <- function(sessions) {
    session_batch_open_date <- sessions$shutdown_date
    session_batch_open_date[is.na(session_batch_open_date)] <- sessions$download_date[is.na(session_batch_open_date)]
    earlier_download_bool <- (!is.na(sessions$shutdown_date)) & (!is.na(sessions$download_date)) & (sessions$shutdown_date > sessions$download_date)
    session_batch_open_date[earlier_download_bool] <- sessions$download_date[earlier_download_bool]

    inferred_close <- sapply(which(is.na(session_batch_open_date)), function(missing_idx) {
        logger_id <- sessions$logger_serial_no[missing_idx]
        start_time <- sessions$starttime_gmt[missing_idx]
        logger_model <- sessions$logger_model[missing_idx]
        # Find other instances of this logger ID that are not this start time
        other_sessions <- sessions[sessions$logger_serial_no %in% logger_id & sessions$logger_model == logger_model & sessions$starttime_gmt > start_time, ]
        if (nrow(other_sessions) == 0) {
            return(NA)
        }
        # find which other session start time is closes to start time
        time_diff <- other_sessions$starttime_gmt - start_time
        return(as.Date(other_sessions$starttime_gmt[time_diff == min(time_diff)][1]) - 1)
    })
    session_batch_open_date[is.na(session_batch_open_date)] <- inferred_close

    session_batch_open_date[is.na(session_batch_open_date)] <- sessions$starttime_gmt[is.na(session_batch_open_date)] + (100 * 60 * 60 * 24 * 365)
    return(session_batch_open_date)
}

#' Get session deployments
#'
#' Function to get deployments associated with a set of logger sessions. The function will look for deployments that occurred between the session start time and the session end time (download/shutdown).
#' If the session is open (no download/shutdown date), the function will use a date far in the future to indicate the session is still open.
#' If there are ambiguities (multiple deployments for a single session), those sessions will be removed and a warning will be logged.
#' @param sessions A tibble containing session information from master import startup_shutdown.
#' @param metadata A dataframe containing metadata information for the sessions.
#' @param filter_sessions Logical indicating whether to filter out sessions with no deployments.
#' @param report_missing Logical indicating whether to report deployments that are not associated with any sessions.
#' @param duplicate_warnings Logical indicating whether to report duplicate deployments
#' @return A list containing a dataframe of deployments and a tibble of sessions.
#' @concept db_import_prep
#' @export
prepare_session_deployments <- function(sessions, metadata, filter_sessions = FALSE, report_missing = FALSE, duplicate_warnings = TRUE) {
    session_batch_open_date <- get_open_session_dates(sessions)
    session_batch_start_date <- as.Date(sessions$programmed_gmt_time)
    session_batch_start_date[is.na(session_batch_start_date)] <- as.Date(sessions$starttime_gmt[is.na(session_batch_start_date)])
    # foreach session batch
    # find the deployments and retrievals for those loggers that are associated with these sessions
    # deployments

    original_idx <- seq_len(nrow(sessions))

    deployed_match_idx <- lapply(original_idx, function(session_row_idx) {
        session_row <- sessions[session_row_idx, ]
        session_open_date <- session_batch_open_date[session_row_idx]
        session_start_date <- session_batch_start_date[session_row_idx]
        match_idx <- which(metadata$date >= session_start_date &
            metadata$date < session_open_date &
            metadata$logger_id_deployed == session_row$logger_serial_no & metadata$logger_model_deployed == session_row$logger_model & !is.na(metadata$logger_id_deployed))
        return(match_idx)
    })

    n_matches <- sapply(deployed_match_idx, length)
    session_has_matches <- n_matches > 0
    duplicate_deployments <- n_matches > 1
    all_deployed_match_idx <- deployed_match_idx

    if (any(duplicate_deployments)) {
        duplicate_idx <- which(duplicate_deployments)
        if (duplicate_warnings) {
            log_warn(sum(duplicate_deployments), " sessions have multiple deployments within them")
            log_warn("These sessions will not be handled.")
            for (i in duplicate_idx) {
                session_summary <- sessions[i, c("logger_serial_no", "starttime_gmt", "download_date")]
                duplicate_logger <- session_summary$logger_serial_no
                log_warn(duplicate_logger, ":\n", paste(capture.output(print(session_summary, n = 1))[c(-1, -3)], collapse = "\n"))

                deployment_summary <- metadata[deployed_match_idx[[i]], c("date", "ring_number", "logger_id_retrieved", "logger_id_deployed", "comment")]
                log_warn("Deployments:\n", paste(capture.output(print(deployment_summary, n = nrow(deployment_summary)))[c(-1, -3)], collapse = "\n"))
            }
        }

        deployed_match_idx <- deployed_match_idx[!original_idx %in% duplicate_idx]
        sessions <- sessions[!original_idx %in% duplicate_idx, ]
        session_has_matches <- session_has_matches[!original_idx %in% duplicate_idx]
        original_idx <- original_idx[!original_idx %in% duplicate_idx]
    }

    if (report_missing) {
        flat_all_deployed_match_idx <- unlist(all_deployed_match_idx)
        orphaned_deployments <- metadata[(!seq_len(nrow(metadata)) %in% flat_all_deployed_match_idx) & (!is.na(metadata$logger_id_deployed)), ]
        if (nrow(orphaned_deployments) > 0) {
            orphan_deployment_summary <- orphaned_deployments[, c("date", "ring_number", "logger_id_deployed", "comment")]
            log_warn(glue::glue("Found {nrow(orphaned_deployments)}/{nrow(metadata[!is.na(metadata$logger_id_deployed),])} deployments that are not associated with any sessions."))
            log_warn("Orphaned deployments:\n", paste(capture.output(print(orphan_deployment_summary, n = nrow(orphan_deployment_summary)))[c(-1, -3)], collapse = "\n"))
            log_warn("This could be due to incorrect dates on the encounter or sessions, incorrect logger IDs or incorrect models.")
        }
    }

    flat_deployed_match_idx <- unlist(deployed_match_idx)
    deployments <- metadata[flat_deployed_match_idx, ]
    log_info(glue::glue("Found {nrow(deployments)} deployments associated with these sessions"))

    new_deployments <- check_deployment_db(deployments)
    # As we are handling retrievals and deployments seperately, remove deployment information
    new_deployments$logger_model_retrieved <- NA
    new_deployments$logger_id_retrieved <- NA
    log_info_all(glue::glue("Found {nrow(new_deployments)} deployments associated with these sessions and not present in database."))

    if (filter_sessions) {
        # Get original deployment IDs
        deployment_id <- paste(deployments$logger_id_deployed, deployments$date)
        # Get filtered retrieval IDs
        new_deployments_id <- paste(new_deployments$logger_id_deployed, new_deployments$date)

        sessions <- sessions[session_has_matches, ]
        sessions <- sessions[deployment_id %in% new_deployments_id, ]
    }

    return(list(deployments = new_deployments, sessions = sessions))
}


#' Get session retrievals
#'
#' Function to get retrievals associated with a set of logger sessions. The function will look for retrievals that occurred between the session start time and the session end time (download/shutdown).
#' If the session is open (no download/shutdown date), the function will use a date far in the future to indicate the session is still open.
#' If there are ambiguities (multiple retrievals for a single session), those sessions will be removed and a warning will be logged.
#' @param sessions A tibble containing session information from master import startup_shutdown.
#' @param metadata A dataframe containing metadata information for the sessions.
#' @param filter_sessions Logical indicating whether to filter out sessions with no retrievals.
#' @param report_missing Logical indicating whether to report retrievals that are not associated with any sessions.
#' @param duplicate_warnings Logical indicating whether to report duplicate retrievals
#' @return A list containing a dataframe of retrievals and a tibble of sessions.
#' @concept db_import_prep
#' @export
prepare_session_retrievals <- function(sessions, metadata, filter_sessions = FALSE, report_missing = FALSE, duplicate_warnings = TRUE) {
    session_batch_open_date <- get_open_session_dates(sessions)

    original_idx <- seq_len(nrow(sessions))
    retrieved_match_idx <- lapply(original_idx, function(session_row_idx) {
        session_row <- sessions[session_row_idx, ]
        session_open_date <- session_batch_open_date[session_row_idx]
        match_idx <- which(metadata$date > as.Date(session_row$starttime_gmt) &
            metadata$date <= session_open_date &
            metadata$logger_id_retrieved == session_row$logger_serial_no & metadata$logger_model_retrieved == session_row$logger_model & !is.na(metadata$logger_id_retrieved))
        return(match_idx)
    })

    n_matches <- sapply(retrieved_match_idx, length)
    session_has_matches <- n_matches > 0
    duplicate_retrievals <- n_matches > 1

    all_retrieved_match_idx <- retrieved_match_idx

    if (any(duplicate_retrievals)) {
        duplicate_idx <- which(duplicate_retrievals)
        if (duplicate_warnings) {
            log_warn(sum(duplicate_retrievals), " sessions have multiple retrievals within them")
            for (i in duplicate_idx) {
                session_summary <- sessions[i, c("logger_serial_no", "starttime_gmt", "download_date")]
                duplicate_logger <- session_summary$logger_serial_no
                log_warn(duplicate_logger, ":\n", paste(capture.output(print(session_summary, n = 1))[c(-1, -3)], collapse = "\n"))

                retrieval_summary <- metadata[retrieved_match_idx[[i]], c("date", "ring_number", "logger_id_retrieved", "logger_id_deployed", "comment")]
                log_warn("Retrievals:\n", paste(capture.output(print(retrieval_summary, n = nrow(retrieval_summary)))[c(-1, -3)], collapse = "\n"))
                log_warn("This session will not be handled.")
            }
        }
        retrieved_match_idx <- retrieved_match_idx[!original_idx %in% duplicate_idx]
        sessions <- sessions[!original_idx %in% duplicate_idx, ]
        session_has_matches <- session_has_matches[!original_idx %in% duplicate_idx]
        original_idx <- original_idx[!original_idx %in% duplicate_idx]
    }

    flat_retrieved_match_idx <- unlist(retrieved_match_idx)
    retrievals <- metadata[flat_retrieved_match_idx, ]

    if (report_missing) {
        flat_all_retrieved_match_idx <- unlist(all_retrieved_match_idx)
        orphaned_retrievals <- metadata[(!seq_len(nrow(metadata)) %in% flat_all_retrieved_match_idx) & (!is.na(metadata$logger_id_retrieved)), ]
        if (nrow(orphaned_retrievals) > 0) {
            orphan_retrieval_summary <- orphaned_retrievals[, c("date", "ring_number", "logger_id_retrieved", "comment")]
            log_warn(glue::glue("Found {nrow(orphaned_retrievals)}/{nrow(metadata[!is.na(metadata$logger_id_retrieved),])} retrievals that are not associated with any sessions."))
            log_warn(paste("Orphaned retrievals:\n", paste(capture.output(print(orphan_retrieval_summary, n = nrow(orphan_retrieval_summary)))[c(-1, -3)], collapse = "\n")))
            log_warn("This could be due to incorrect dates on the encounter or sessions, incorrect logger IDs or incorrect models.")
        }
    }

    log_info(glue::glue("Found {nrow(retrievals)} retrievals associated with these sessions"))

    new_retrievals <- check_retrieval_db(retrievals)

    # As we are handling retrievals and deployments seperately, remove deployment information
    new_retrievals$logger_model_deployed <- NA
    new_retrievals$logger_id_deployed <- NA
    log_info_all(glue::glue("Found {nrow(new_retrievals)} retrievals associated with these sessions and not present in database."))

    if (filter_sessions) {
        # Get original retrieval IDs
        retrieval_id <- paste(retrievals$logger_id_retrieved, retrievals$date)
        # Get filtered retrieval IDs
        new_retrievals_id <- paste(new_retrievals$logger_id_retrieved, new_retrievals$date)

        sessions <- sessions[session_has_matches, ]
        sessions <- sessions[retrieval_id %in% new_retrievals_id, ]
    }

    return(list(retrievals = new_retrievals, sessions = sessions))
}
