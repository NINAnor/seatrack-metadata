

#' Handle existing GLS calibration data
#'
#' Function to reshape existing seatrack GLS calibration settings for immediate use in seatrackRgls
#'
#' @param metadata_path Path to metadata Excel file
#' @param split_years Character string indicating the month and day to split years for calibration (e.g., "06-01" for June 1st). Default is "06-01".
#' @return A list with two dataframes: calibration_data and extra_metadata
#' @export
#' @concept gls_helper
gls_seatrack_calibration <- function(metadata_path, split_years = "06-01") {
    metadata <- openxlsx2::read_xlsx(metadata_path)
    metadata <- metadata[!is.na(metadata$logger_id), ]

    cols_to_get <- c("logger_id", "logger_model", "species", "date_deployed", "date_retrieved", "colony", "sun_angle_start", "sun_angle_end", "light_threshold", "analyzer", "year_tracked")
    if (!any(cols_to_get %in% names(metadata))) {
        log_error("Calibration data missing required columns.", namespace = "error")
        return(NULL)
    }

    calibration_data <- metadata[, cols_to_get]

    extra_metadata <- metadata[!is.na(metadata$logger_id) & !duplicated(metadata$logger_id), c("logger_id", "date_retrieved", "date_deployed", "logger_producer", "ring_number", "country_code", "data_responsible", "age_deployed")]

    calibration_data$logger_id <- as.character(calibration_data$logger_id)
    calibration_data$logger_model <- as.character(calibration_data$logger_model)
    calibration_data$sun_angle_start <- as.numeric(calibration_data$sun_angle_start)
    calibration_data$sun_angle_end <- as.numeric(calibration_data$sun_angle_end)
    calibration_data$light_threshold <- as.numeric(calibration_data$light_threshold)
    calibration_data$date_deployed <- as.Date(calibration_data$date_deployed)
    calibration_data$date_retrieved <- as.Date(calibration_data$date_retrieved)

    extra_metadata$logger_id <- as.character(extra_metadata$logger_id)
    extra_metadata$age_deployed <- as.character(extra_metadata$age_deployed)
    extra_metadata$date_deployed <- as.Date(extra_metadata$date_deployed)
    extra_metadata$date_retrieved <- as.Date(extra_metadata$date_retrieved)
    extra_metadata$ring_number <- as.character(extra_metadata$ring_number)

    deployment_year <- as.numeric(format(calibration_data$date_deployed, "%Y"))
    retrieval_year <- as.numeric(format(calibration_data$date_retrieved, "%Y"))
    current_year_tracked <- metadata$year_tracked


    calibration_data$total_years_tracked <- paste(deployment_year, retrieval_year, sep = "_")

    # split by deployment/retrieval dates
    id_year <- paste(calibration_data$logger_id, calibration_data$date_deployed, calibration_data$date_retrieved)
    all_time_windows <- lapply(unique(id_year), function(idy) {
        logger_calibration <- calibration_data[id_year == idy, ]
        logger_deployment_year <- as.numeric(format(logger_calibration$date_deployed, "%Y"))[1]
        logger_retrieval_year <- as.numeric(format(logger_calibration$date_retrieved, "%Y"))[1]

        current_years <- as.numeric(sapply(logger_calibration$year_tracked, function(x) {
            strsplit(as.character(x), "_")[[1]][1]
        }))
        all_years <- seq(logger_deployment_year, logger_retrieval_year)
        if (length(all_years) > 1) {
            all_years <- all_years[1:(length(all_years) - 1)]
        }
        missing_years <- all_years[!all_years %in% current_years]
        if (length(missing_years) > 0) {
            template_row <- logger_calibration[1, , drop = FALSE]
            new_rows <- do.call(rbind, lapply(missing_years, function(y) {
                r <- template_row
                if ("analyzer" %in% names(r)) r$analyzer <- NA
                if ("sun_angle_start" %in% names(r)) r$sun_angle_start <- NA
                if ("sun_angle_end" %in% names(r)) r$sun_angle_end <- NA
                if ("light_threshold" %in% names(r)) r$light_threshold <- NA
                if ("year_tracked" %in% names(r)) r$year_tracked <- as.character(y)
                if ("total_years_tracked" %in% names(r)) r$total_years_tracked <- paste(logger_deployment_year, logger_retrieval_year, sep = "_")
                if ("year_tracked" %in% names(r)) r$year_tracked <- paste(y, (y + 1) - 200, sep = "_")
                return(r)
            }))
            logger_calibration <- rbind(logger_calibration, new_rows)
            current_years <- as.numeric(sapply(logger_calibration$year_tracked, function(x) {
                strsplit(as.character(x), "_")[[1]][1]
            }))
            logger_calibration <- logger_calibration[order(current_years), ]
        }

        time_windows <- seatrackRgls:::get_calibration_splits(logger_calibration, split_years)
        if (nrow(time_windows) < nrow(logger_calibration)) {
            logger_calibration <- logger_calibration[1:nrow(time_windows), ]
        }

        new_logger_calibration <- data.frame(
            logger_calibration[, c("logger_id", "logger_model")],
            time_windows,
            logger_calibration[, !names(logger_calibration) %in% c("logger_id", "logger_model", "date_deployed", "date_retrieved")]
        )

        return(new_logger_calibration)
    })
    calibration_data <- do.call(rbind, all_time_windows)
    calibration_data$year_tracked <- paste(format(calibration_data$start_datetime, "%Y"), format(calibration_data$end_datetime, "%Y"), sep = "_")

    return(list(calibration_data = calibration_data, extra_metadata = extra_metadata))
}

gls_get_existing_calibration <- function(existing_calibration_dir = file.path(the$sea_track_folder, "Locations"), split_years = "06-01") {
    if (!dir.exists(existing_calibration_dir)) {
        stop("Could not find existing calibration directory: ", existing_calibration_dir)
    }

    metadata_files <- list.files(existing_calibration_dir, pattern = "^metadata_new\\.xlsx$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

    if (length(metadata_files) == 0) {
        return(list(calibration_data = data.frame(), extra_metadata = data.frame()))
    }

    results <- lapply(metadata_files, function(f) {
        tryCatch(
            gls_seatrack_calibration(f, split_years),
            error = function(e) {
                log_warn(sprintf("Failed to process '%s': %s", f, e$message))
                NULL
            }
        )
    })
    results <- Filter(Negate(is.null), results)

    if (length(results) == 0) {
        return(list(calibration_data = data.frame(), extra_metadata = data.frame()))
    }


    calibration_data_all <- do.call(dplyr::bind_rows, lapply(results, function(x) x$calibration_data))
    extra_metadata_all <- do.call(dplyr::bind_rows, lapply(results, function(x) x$extra_metadata))

    return(list(calibration_data = calibration_data_all, extra_metadata = extra_metadata_all))
}

#' Get metadata from database based on GLS files in import directory
#'
#' Function to scan a directory for GLS files and retrieve corresponding metadata from the Sea Track database.
#' @param import_directory Path to the directory containing GLS files.
#' @param colony Colony name to filter metadata. Default is NULL (no filtering).
#' @param species Species name to filter metadata. Default is NULL (no filtering).
#' @param id_year_model Character string in the format "loggerID_year_model" to filter for a specific logger, year and model. Default is NULL (no filtering).
#' @param time_windows Logical indicating whether to split metadata into time windows based on deployment/retrieval dates. Default is TRUE.
#' @param split_years Character string indicating the month and day to split years for calibration (e.g., "06-01" for June 1st). Default is "06-01".
#' @param no_pos_only Logical indicating whether to include only loggers without position data in the database. Default is TRUE.
#' @param download_types Character vector of logger download types to include (e.g., c("Successfully downloaded", "Reconstructed")). Default is c("Successfully downloaded", "Reconstructed").
#' @return A dataframe containing metadata for the GLS loggers found in the import directory.
#' @export
#' @concept gls_helper
gls_metadata <- function(import_directory, colony = NULL, species = NULL, id_year_model = NULL, time_windows = TRUE, split_years = "06-01", no_pos_only = TRUE, download_types = c("Successfully downloaded", "Reconstructed")) {
    log_info("Scan import directory for files...")
    all_files <- list.files(import_directory, pattern = "*.lux|*.lig", recursive = TRUE, full.names = TRUE)
    all_files_split <- strsplit(tools::file_path_sans_ext(basename(all_files)), "_")
    file_info_list <- lapply(all_files_split, function(x) {
        data.frame(logger_id = x[1], year_downloaded = x[2], id_year = paste(x[1], x[2], sep = "_"), id_year_model = paste(c(x[1], x[2], x[3:length(x)]), collapse = "_"))
    })
    file_info <- do.call(rbind, file_info_list)
    file_info <- data.frame(filename = all_files, file_info)
    if (!is.null(id_year_model)) {
        # Get only file info for the desired file
        file_info <- file_info[file_info$id_year_model == id_year_model, ]
        if (nrow(file_info) == 0) {
            log_error(glue::glue("File {id_year_model} not found in import directory"))
            return(data.frame())
        }
    }

    if (!no_pos_only) {
        has_pos_data <- NULL
    } else {
        has_pos_data <- FALSE
    }
    db_info <- seatrackR::getSessionInfo(
        posdata_filename = file_info$id_year_model,
        has_pos_data = has_pos_data,
        logger_download_type = download_types,
        colony_names = colony, 
        species_names = species
    )
    if (nrow(db_info) == 0) {
        return(data.frame())
    }
    # logger_id, logger_model, species, date_deployed, date_retrieved, colony
    metadata <- dplyr::select(
        db_info,
        session_id,
        logger_id = logger_serial_no,
        logger_model,
        species,
        starttime_gmt = starttime_gmt,
        date_deployed = deployment_date,
        date_retrieved = retrieval_date,
        download_date,
        download_type,
        colony
    )
# # report missing files
# db_info$id_year_model <- paste(db_info$logger_serial_no, format(db_info$retrieval_date, "%Y"), db_info$logger_model, sep = "_")
# missing_files <- file_info[!file_info$id_year_model %in% db_info$id_year_model, ]

# if (nrow(missing_files) > 0) {
#     log_warn(glue::glue("The following files were found in the import directory but have no corresponding metadata in the database:\n{paste(missing_files$filename, collapse = '\n')}"))
# }
    if (time_windows) {
        # split by deployment/retrieval dates
        id_year <- paste(metadata$logger_id, metadata$date_deployed, metadata$date_retrieved)
        all_time_windows <- lapply(unique(id_year), function(idy) {
            logger_metadata <- metadata[id_year == idy, ]
            logger_deployment_year <- as.numeric(format(logger_metadata$date_deployed, "%Y"))[1]
            logger_retrieval_year <- as.numeric(format(logger_metadata$date_retrieved, "%Y"))[1]


            time_windows <- seatrackRgls:::get_calibration_splits(logger_metadata, split_years)

            new_metadata <- data.frame(
                logger_metadata[, c("logger_id", "logger_model")],
                time_windows,
                logger_metadata[, !names(logger_metadata) %in% c("logger_id", "logger_model", "session_id", "date_deployed", "date_retrieved", "starttime_gmt", "download_date", "download_type")]
            )
            new_metadata$total_years_tracked <- paste(logger_deployment_year, logger_retrieval_year, sep = "_")
            new_metadata$year_tracked <- paste(format(new_metadata$start_datetime, "%Y"), format(new_metadata$end_datetime, "%Y"), sep = "_")
            return(new_metadata)
        })
        metadata <- do.call(rbind, all_time_windows)
    }

    return(metadata)
}


#' Get `all_colony_info` for GLS processing
#'
#' Simple function to get seatrack colonies from database and shape the dataframe for immediate use in seatrackRgls
#'
#' @export
#' @concept gls_helper
gls_seatrack_colony_info <- function() {
    all_colony_info <- seatrackR::getColonies(allLocations = TRUE)
    all_colony_info <- data.frame(colony = all_colony_info$location_name, col_lat = all_colony_info$lat, col_lon = all_colony_info$lon)
    return(all_colony_info)
}
