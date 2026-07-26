#' Process GLS position data using seatrackRgls
#'
#' Function to process calibrated GLS position data using seatrackRgls.
#' @param no_pos_only Logical indicating whether to include only loggers without position data in the database. Default is TRUE.
#' @param filter_plots Logical indicating whether to export filter plots. Default is FALSE.
#' @param skip_existing_files Logical indicating whether to skip processing for files that already have processed position data in the output directory. Default is TRUE.
#' @param n_workers Integer specifying the number of worker processes to use for parallel processing.
#' @return None. The function saves the processed position data to `Database\\Imports_Logger data\\Output_GLSpositions`
#' @export
#' @concept gls_helper
gls_process_positions <- function(no_pos_only = TRUE, filter_plots = TRUE, skip_existing_files = TRUE, n_workers = 4) {
    # progressr::handlers(global = FALSE)
    # progressr::handlers("progress")
    future::plan(future::multisession, workers = n_workers)
    seatrackR::connectSeatrack()
    all_colony_info <- gls_seatrack_colony_info()

    import_directory <- file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL")
    output_directory <- file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Output_GLSpositions")
    log_info("Get all species/colony combinations from ", import_directory)
    all_metadata <- gls_metadata(import_directory, no_pos_only = no_pos_only)
    if (nrow(all_metadata) == 0) {
        stop("No metadata available for these files")
    }
    species_colony <- all_metadata[, c("species", "colony")]
    species_colony <- species_colony[order(all_metadata$species, all_metadata$colony), ]
    species_colony <- species_colony[stats::complete.cases(species_colony), ]
    species_colony <- dplyr::distinct(species_colony)

    # p <- progressr::progressor(along = seq_len(nrow(species_colony)))
    result <- foreach::foreach(i = seq_len(nrow(species_colony)), .errorhandling = "pass") %dofuture% {
        species <- species_colony$species[i]
        colony <- species_colony$colony[i]

        gls_process_species_colony(
            species = species,
            colony = colony,
            import_directory = import_directory,
            output_directory = output_directory,
            all_colony_info = all_colony_info,
            no_pos_only = no_pos_only,
            filter_plots = filter_plots,
            skip_existing_files = skip_existing_files
        )
    }

    for (res in result) {
        if (inherits(res, "error")) {
            log_error("Error in calibration preparation: ", res$message)
        }
    }
}

gls_process_species <- function(
    species,
    import_directory = file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL"),
    output_directory = file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Output_GLSpositions"),
    all_colony_info = gls_seatrack_colony_info(),
    no_pos_only = TRUE,
    filter_plots = TRUE,
    skip_existing_files = TRUE) {
    species_folder <- file.path(the$sea_track_folder, "Database", "Imports_Logger data", "Callibration_GLSpositions", species)
    all_colony <- list.dirs(species_folder, recursive = FALSE, full.names = FALSE)
    for (colony in all_colony) {
        gls_process_species_colony(
            species = species,
            colony = colony,
            import_directory = import_directory,
            output_directory = output_directory,
            all_colony_info = all_colony_info,
            no_pos_only = no_pos_only,
            filter_plots = filter_plots,
            skip_existing_files = skip_existing_files
        )
    }
}

#' Process GLS position data for a specific species/colony combination
#'
#' Function to process calibrated GLS position data for a specific species/colony combination using seatrackRgls.
#' @param species Species name to filter metadata.
#' @param colony Colony name to filter metadata.
#' @param import_directory Path to the directory containing GLS files. Default is `Database\\Imports_Logger data\\Raw logger data\\ALL`.
#' @param output_directory Path to the directory where the processed position data will be saved. Default is `Database\\Imports_Logger data\\Output_GLSpositions`.
#' @param all_colony_info Dataframe containing colony information for all species/colony combinations. Default is obtained from `gls_seatrack_colony_info()`.
#' @param no_pos_only Logical indicating whether to include only loggers without position data in the database. Default is TRUE.
#' @param filter_plots Logical indicating whether to export filter plots. Default is TRUE.
#' @param skip_existing_files Logical indicating whether to skip processing for files that already have processed position data in the output directory. Default is TRUE.
#' @param stop_on_error Logical indicating whether to stop processing if an error occurs. Default is FALSE, which means that the function will log the error and continue processing other files.
#' @return None. The function saves the processed position data to the specified output directory.
#' @export
#' @concept gls_helper
gls_process_species_colony <- function(
    species, colony,
    import_directory = file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL"),
    output_directory = file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Output_GLSpositions"),
    all_colony_info = gls_seatrack_colony_info(),
    no_pos_only = TRUE,
    filter_plots = TRUE,
    skip_existing_files = TRUE,
    stop_on_error = FALSE) {
    calibration_dir <- file.path(the$sea_track_folder, "Database", "Imports_Logger data", "Callibration_GLSpositions", species, colony)
    calibration_filename <- paste(species, colony, "calibration.xlsx", sep = "_")
    calibration_data <- file.path(calibration_dir, calibration_filename)
    filter_path <- file.path(calibration_dir, "filter_settings.xlsx")
    if (!file.exists(calibration_data)) {
        log_warn("No calibration data found for species '", species, "' and colony '", colony, "'. Skipping position processing.")
        return(invisible())
    }

    log_info("Processing positions for species '", species, "' and colony '", colony, "'.")
    calibration_data <- openxlsx2::read_xlsx(calibration_data)
    logger_years <- paste(calibration_data$logger_id, sapply(strsplit(calibration_data$total_years_tracked, "_"), function(x) {
        x[2]
    }), calibration_data$logger_model, sep = "_")
    problem_dir <- file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\problems")
    dir.create(problem_dir, recursive = TRUE, showWarnings = FALSE)

    all_files <- list.files(import_directory, pattern = "*.lux|*.lig", recursive = TRUE, full.names = TRUE)
    all_files_split <- strsplit(tools::file_path_sans_ext(basename(all_files)), "_")
    file_info_list <- lapply(all_files_split, function(x) {
        data.frame(logger_id = x[1], year_downloaded = x[2], id_year = paste(x[1], x[2], sep = "_"), id_year_model = paste(c(x[1], x[2], x[3:length(x)]), collapse = "_"))
    })
    file_info <- do.call(rbind, file_info_list)
    file_info <- data.frame(filename = all_files, file_info)
    # Find problem files and move them
    for (x in unique(logger_years)) {
        if (all(!is.na(calibration_data$problem[logger_years == x]) & calibration_data$problem[logger_years == x])) {
            log_info("Moving problem file for logger/year/model: ", x, " to problems folder.")
            # Find matching file in import director
            current_file <- file_info[file_info$id_year_model == x, ]
            # Move it to a new folder
            new_path <- file.path(problem_dir, basename(current_file$filename))
            file.rename(current_file$filename, new_path)
        }
    }

    calibration_data <- calibration_data[!is.na(calibration_data$sun_angle_start), ]

    filter_list <- seatrackRgls::read_filter_file(filter_path)

    pos_output_dir <- file.path(output_directory, species, colony)
    if (!file.exists(output_directory)) {
        dir.create(output_directory, recursive = TRUE)
    }

    if (skip_existing_files) {
        process_pos_dir <- file.path(pos_output_dir, "processed_positions")
        end_years <- strsplit(calibration_data$total_years_tracked, "_") %>% sapply(function(x) x[length(x)])
        logger_end_years <- paste(calibration_data$logger_id, end_years, sep = "_")
        existing_files <- list.files(process_pos_dir, pattern = "*.csv")
        existing_files_end_years <- strsplit(tools::file_path_sans_ext(existing_files), "_-_") %>% sapply(function(x) x[length(x)])

        calibration_data <- calibration_data[!logger_end_years %in% existing_files_end_years, ]
        if (nrow(calibration_data) == 0) {
            log_info("All positions already processed for species '", species, "' and colony '", colony, "'. Skipping position processing.")
            return(invisible())
        }
    }

    if (any(calibration_data$sun_angle_start >= 0, na.rm = TRUE) || any(calibration_data$sun_angle_end >= 0, na.rm = TRUE)) {
        log_error("Positive sun angles are not allowed!")
        return(invisible())
    }

    # Other quality control

    seatrackRgls::process_positions(
        import_directory = import_directory,
        calibration_data = calibration_data,
        all_colony_info = all_colony_info,
        output_directory = pos_output_dir,
        stop_on_error = stop_on_error,
        show_filter_plots = filter_plots,
        filter_setting_list = filter_list
    )
}

#' Import GLS position data to the SEATRACK database
#'
#' Function to import prepared GLS position data from a specified directory to the SEATRACK database.
#' @param species A character vector of species names to filter the import. If NULL, all species directories in the specified GLS directory will be processed. Default is NULL.
#' @param colony A character vector of colony names to filter the import. If NULL, all colony directories within the specified species directories will be processed. Default is NULL.
#' @param chunk_size Integer specifying the number of sessions to upload in each chunk. Default is 10.
#' @param gls_directory_path Path to the directory containing the prepared GLS position data files. Default is `Database\\Imports_Logger data\\Output_GLSpositions` within the SEATRACK folder.
#' @return None. The function uploads the position data to the SEATRACK database.
#' @export
#' @concept gls_helper
gls_import <- function(species = NULL, colony = NULL, chunk_size = 10, gls_directory_path = file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Output_GLSpositions")) {
    if (is.null(species)) {
        species <- list.dirs(gls_directory_path, recursive = FALSE, full.names = FALSE)
    }
    for (current_species in species) {
        species_dir <- file.path(gls_directory_path, current_species)
        if (!dir.exists(species_dir)) {
            log_warn("Directory for species '", current_species, "' does not exist. Skipping this species.")
            next
        }
        if (is.null(colony)) {
            colony_dir <- list.dirs(species_dir, recursive = FALSE, full.names = FALSE)
        }else{
            colony_dir <- colony
        }
        for (current_colony in colony_dir) {
            colony_dir_path <- file.path(species_dir, current_colony)
            if (!dir.exists(colony_dir_path)) {
                log_warn("Directory for colony '", current_colony, "' does not exist. Skipping this colony.")
                next
            }
            log_info("Preparing upload for species '", current_species, "' and colony '", current_colony, "'.")
            pos_list <- gls_prepare_folder_upload(colony_dir_path)
            log_info("Prepared position data for ", length(pos_list), " sessions for upload.")
            gls_upload_pos_data(pos_list, chunk_size = chunk_size)
        }
    }
}

#' Upload GLS position data to the SEATRACK database
#'
#' Function to upload prepared GLS position data to the Sea Track database.
#' @param pos_list A list of dataframes containing the prepared GLS position data, where each dataframe corresponds to a session and is named with the session ID.
#' @param chunk_size Integer specifying the number of sessions to upload in each chunk. Default is 10.
#' @return None. The function uploads the position data to the SEATRACK database.
#' @export
#' @concept gls_helper
gls_upload_pos_data <- function(pos_list, chunk_size = 10) {
    all_session_ids <- names(pos_list)
    log_info("Checking session information for ", length(all_session_ids), " sessions in the database.")
    db_info <- seatrackR::getSessionInfo(session_ids = all_session_ids, has_pos_data = FALSE)
    filtered_pos_data <- pos_list[names(pos_list) %in% db_info$session_id]
    removed_pos_data <- pos_list[!names(pos_list) %in% db_info$session_id]
    if (length(removed_pos_data) > 0) {
        log_warn("The following session IDs from the position data do not have corresponding session information without pos data in the database and will be skipped for upload: ", paste(unique(removed_pos_data$session_id), collapse = "\n "))
    }
    if (length(filtered_pos_data) == 0) {
        log_warn("No session information without position data found for the provided session IDs. Skipping upload.")
        return(invisible())
    }

    chunks <- split(filtered_pos_data, ceiling(seq_along(filtered_pos_data) / chunk_size))
    for (i in seq_along(chunks)) {
        chunk <- chunks[[i]]
        log_info("Uploading chunk ", i, " of ", length(chunks))
        log_trace("Uploading chunk with session IDs: ", paste(names(chunk), collapse = ", "))
        seatrackR::writePositions(datatype = "GLS", positionData = chunk, refreshView = i == length(chunks))
    }
}

#' Prepare GLS position data for database upload
#'
#' Function to prepare GLS position data from a specified directory for upload to the Sea Track database.
#' @param gls_directory_path Path to the directory containing GLS position data files.
#' @return A dataframe containing the prepared GLS position data ready for database upload.
#' @export
#' @concept gls_helper
gls_prepare_folder_upload <- function(gls_directory_path = file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Output_GLSpositions")) {
    all_files <- list.files(gls_directory_path, pattern = "^positions_-_.*\\.csv$", recursive = TRUE)
    all_file_paths <- file.path(gls_directory_path, all_files)
    pos_list <- lapply(all_file_paths, gls_prepare_file_upload)
    # Remove NULL entries from the list (files that were skipped due to missing or multiple session information)
    pos_list <- pos_list[!sapply(pos_list, is.null)]
    names(pos_list) <- sapply(pos_list, function(df) {
        return(unique(df$session_id))
    })
    return(pos_list)
}

#' Prepare a single GLS position data file for database upload
#'
#' Function to prepare a single GLS position data file for upload to the Sea Track database.
#' @param gls_file_path Path to the GLS position data file.
#' @return A dataframe containing the prepared GLS position data ready for database upload.
#' @export
#' @concept gls_helper
gls_prepare_file_upload <- function(gls_file_path) {
    gls_pos_data <- read.csv(gls_file_path)
    db_info <- seatrackR::getSessionInfo(posdata_filename = tools::file_path_sans_ext(gls_pos_data$raw_data_file[1]), has_pos_data = FALSE)
    if (nrow(db_info) == 0) {
        log_warn("No session information without pos data found for file: ", basename(gls_file_path), ". Skipping this file.")
        return(NULL)
    } else if (nrow(db_info) > 1) {
        log_error("Multiple sessions found for file: ", basename(gls_file_path), ". Skipping this file")
        return(NULL)
    }
    gls_pos_data$session_id <- db_info$session_id[1]
    gls_pos_data$data_version <- 3
    db_template <- dplyr::select(
        gls_pos_data,
        date_time,
        year_tracked,
        session_id,
        lon_raw,
        lat_raw,
        lon,
        lat,
        eqfilter,
        tfirst = tFirst,
        tsecond = tSecond,
        twl_type = type,
        sun = sun_angle,
        light_threshold,
        analyzer,
        data_version
    ) %>% mutate(light_threshold = round(light_threshold))
    return(db_template)
}
