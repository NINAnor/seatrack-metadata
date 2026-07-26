#' Calibrate all species/colony combinations found in seatrack logger data import folder
#'
#' Uses hard coded file paths to call gls_calibrate_species_colony for all species/colony combinations found in seatrack database for loggers in import folder.
#' @param no_pos_only Logical indicating whether to include only loggers without position data in the database. Default is TRUE.
#' @param rerun_existing Logical indicating whether to rerun calibration for loggers that already have calibration data. Default is FALSE.
#' @param include_existing Logical indicating whether to include existing calibration data in the final output. Default is TRUE.
#' @param filter_plots Logical indicating whether to export filter plots. Default is FALSE.
#' @param rerun_existing_plots Logical indicating whether to rerun calibration for loggers that already have calibration plots. Default is FALSE.
#' @param n_workers Integer specifying the number of worker processes to use for parallel processing. Default is 4.
#' @param new_filter_settings Logical indicating whether to force creation of a new filter settings files for each species/colony combination, using the seatrackRgls defaults. Default is FALSE. If TRUE, existing filter settings files will be overwritten.
#' @return None. The function saves the prepared calibration data to the specified output directory.
#' @concept gls_helper
#' @export
gls_calibrate_all <- function(no_pos_only = TRUE, rerun_existing = FALSE, include_existing = TRUE, filter_plots = FALSE, rerun_existing_plots = FALSE, n_workers = 4, new_filter_settings = FALSE) {
    # progressr::handlers(global = TRUE)
    # progressr::handlers("progress")
    future::plan(future::multisession, workers = n_workers)
    seatrackR::connectSeatrack()

    import_directory <- file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL")
    log_info("Get all species/colony combinations from ", import_directory)
    all_metadata <- gls_metadata(import_directory, no_pos_only = no_pos_only)
    if (nrow(all_metadata) == 0) {
        stop("No metadata avaiable for files.")
    }
    species_colony <- all_metadata[, c("species", "colony")]
    species_colony <- species_colony[order(all_metadata$species, all_metadata$colony), ]
    species_colony <- species_colony[stats::complete.cases(species_colony), ]
    species_colony <- dplyr::distinct(species_colony)
    # p <- progressr::progressor(along = seq_len(nrow(species_colony)))
    result <- foreach::foreach(i = seq_len(nrow(species_colony)), .errorhandling = "pass") %dofuture% {
        if (n_workers > 1) {
            seatrackR::connectSeatrack()
        }

        species <- species_colony$species[i]
        colony <- species_colony$colony[i]
        gls_calibrate_species_colony(
            import_directory = import_directory,
            species = species,
            colony = colony,
            no_pos_only = no_pos_only,
            rerun_existing = rerun_existing,
            include_existing = include_existing,
            filter_plots = filter_plots,
            rerun_existing_plots = rerun_existing_plots,
            new_filter_settings = FALSE
        )

        if (n_workers > 1) {
            seatrackR::disconnectSeatrack()
        }

        # p(sprintf("i=%g", i))
    }
    for (res in result) {
        if (inherits(res, "error")) {
            log_error("Error in calibration preparation: ", res$message)
        }
    }
    export_check_calibration()
}

#' Prepare a seatrack species/colony combination for calibration
#'
#' Uses hard coded file paths to call gls_prepare_calibration. Filters metadata passed to gls_prepare_calibration by species/colony.
#' @param species Species name to filter metadata.
#' @param colony Colony name to filter metadata.
#' @param import_directory Path to the directory containing GLS files.
#' @param no_pos_only Logical indicating whether to include only loggers without position data in the database. Default is TRUE.
#' @param rerun_existing Logical indicating whether to rerun calibration for loggers that already have calibration data. Default is TRUE.
#' @param include_existing Logical indicating whether to include existing calibration data in the final output. Default is TRUE.
#' @param filter_plots Logical indicating whether to export filter plots. Default is FALSE.
#' @param rerun_existing_plots Logical indicating whether to rerun calibration for loggers that already have calibration plots. Default is TRUE.
#' @param new_filter_settings Logical indicating whether to force creation of a new filter settings file for the species/colony combination, using the seatrackRgls defaults. Default is FALSE. If TRUE, existing filter settings file will be overwritten.
#' @return None. The function saves the prepared calibration data to the specified output directory.
#' @export
#' @concept gls_helper
gls_calibrate_species_colony <- function(
    species = NULL, colony = NULL, import_directory = file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL"), no_pos_only = TRUE, rerun_existing = TRUE, include_existing = TRUE, filter_plots = FALSE, rerun_existing_plots = TRUE, new_filter_settings = FALSE) {
    log_info("Preparing calibration for species '", species, "' and colony '", colony, "'")

    existing_calibration_dir <- file.path(the$sea_track_folder, "Locations")

    gls_prepare_calibration(
        import_directory = import_directory,
        output_directory = NULL,
        species = species,
        colony = colony,
        no_pos_only = no_pos_only,
        existing_calibration_dir = existing_calibration_dir,
        rerun_existing = rerun_existing,
        include_existing = include_existing,
        filter_plots = filter_plots,
        rerun_existing_plots = rerun_existing_plots,
        new_filter_settings = new_filter_settings
    )
}

#' Prepare GLS files for calibration
#'
#' Uses hard coded file paths to call gls_prepare_calibration. Filters metadata passed to gls_prepare_calibration by file names.
#' @param id_year_model Character string in the format "loggerID_year_loggermodel" to filter for a specific logger, year and model.
#' @param no_pos_only Logical indicating whether to include only loggers without position data in the database. Default is TRUE.
#' @param rerun_existing Logical indicating whether to rerun calibration for loggers that already have calibration data. Default is TRUE.
#' @param include_existing Logical indicating whether to include existing calibration data in the final output. Default is TRUE.
#' @param filter_plots Logical indicating whether to export filter plots. Default is FALSE.
#' @param rerun_existing_plots Logical indicating whether to rerun calibration for loggers that already have calibration plots. Default is TRUE.
#' @param new_filter_settings Logical indicating whether to force creation of a new filter settings file for the species/colony combination, using the seatrackRgls defaults. Default is FALSE. If TRUE, existing filter settings file will be overwritten.
#' @return None. The function saves the prepared calibration data to the specified output directory.
#' @export
#' @concept gls_helper
gls_calibrate_file <- function(id_year_model, no_pos_only = TRUE, rerun_existing = TRUE, include_existing = TRUE, filter_plots = TRUE, rerun_existing_plots = TRUE, new_filter_settings = FALSE) {
    import_directory <- file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL")
    existing_calibration_dir <- file.path(the$sea_track_folder, "Locations")

    gls_prepare_calibration(
        import_directory = import_directory,
        output_directory = NULL,
        id_year_model = id_year_model,
        no_pos_only = no_pos_only,
        existing_calibration_dir = existing_calibration_dir,
        rerun_existing = rerun_existing,
        include_existing = include_existing,
        filter_plots = filter_plots,
        rerun_existing_plots = rerun_existing_plots,
        new_filter_settings = new_filter_settings
    )
}


#' Prepare GLS calibration data using seatrack database
#'
#' Function to prepare GLS calibration data for use with seatrackRgls, based on GLS files in the import directory and metadata from the Sea Track database.
#' @param import_directory Path to the directory containing GLS files.
#' @param output_directory Path to the directory where the prepared calibration data will be saved.
#' @param species Species name to filter metadata. Default is NULL (no filtering).
#' @param colony Colony name to filter metadata. Default is NULL (no filtering).
#' @param id_year_model Character string in the format "loggerID_year_loggermodel" to filter for a specific logger, year and model. Default is NULL (no filtering).
#' @param no_pos_only Logical indicating whether to include only loggers without position data in the database. Default is TRUE.
#' @param existing_calibration_dir Directory containing existing calibration data. Default is NULL. If provided, calibration data from this directory will be merged.
#' @param rerun_existing Logical indicating whether to rerun calibration for loggers that already have calibration data. Default is TRUE.
#' @param include_existing Logical indicating whether to include existing calibration data in the final output. Default is TRUE.
#' @param filter_plots Logical indicating whether to export filter plots. Default is FALSE.
#' @param rerun_existing_plots Logical indicating whether to rerun calibration for loggers that already have calibration plots. Default is FALSE.
#' @param new_filter_settings Logical indicating whether to force creation of a new filter settings file for the species/colony combination, using the seatrackRgls defaults. Default is FALSE. If TRUE, existing filter settings file will be overwritten.
#' @return None. The function saves the prepared calibration data to the specified output directory.
#' @export
#' @concept gls_helper
gls_prepare_calibration <- function(
    import_directory, output_directory = NULL,
    species = NULL, colony = NULL, id_year_model = NULL,
    no_pos_only = TRUE, existing_calibration_dir = NULL, rerun_existing = TRUE,
    include_existing = TRUE, filter_plots = FALSE, rerun_existing_plots = FALSE,
    new_filter_settings = FALSE) {
    # Get metadata
    metadata <- gls_metadata(import_directory = import_directory, colony = colony, species = species, id_year_model = id_year_model, time_windows = TRUE, no_pos_only = no_pos_only)
    if (nrow(metadata) == 0) {
        # Get string of non null parameters for log message
        if (!is.null(species) && !is.null(colony) && !is.null(id_year_model)) {
            filter_string <- paste("species =", species, ", colony =", colony, ", id_year_model =", id_year_model)
        } else if (!is.null(species) && !is.null(colony)) {
            filter_string <- paste("species =", species, ", colony =", colony)
        } else if (!is.null(species) && !is.null(id_year_model)) {
            filter_string <- paste("species =", species, ", id_year =", id_year_model)
        } else if (!is.null(colony) && !is.null(id_year_model)) {
            filter_string <- paste("colony =", colony, ", id_year =", id_year_model)
        } else if (!is.null(species)) {
            filter_string <- paste("species =", species)
        } else if (!is.null(colony)) {
            filter_string <- paste("colony =", colony)
        } else if (!is.null(id_year_model)) {
            filter_string <- paste("id_year_model =", id_year_model)
        } else {
            filter_string <- "no filters"
        }
        log_warn(glue::glue("No metadata returned for {filter_string}. No position only = {no_pos_only}."))
    }
    if (is.null(species)) {
        species <- unique(metadata$species)
    }
    if (is.null(colony)) {
        colony <- unique(metadata$colony)
    }
    if (is.null(species) || is.null(colony)) {
        return()
    }

    for (current_species in species) {
        for (current_colony in colony) {
            current_metadata <- metadata[metadata$species == current_species & metadata$colony == current_colony, ]
            if (length(species) > 0 || length(colony) > 0 || is.null(output_directory)) {
                output_directory <- file.path(the$sea_track_folder, "Database", "Imports_Logger data", "Callibration_GLSpositions", current_species, current_colony)
            }

            # Set up the output file names
            calibration_output_dir <- file.path(output_directory)
            calibration_filename <- paste(current_species, current_colony, "calibration.xlsx", sep = "_")
            if (nrow(current_metadata) == 0 && file.exists(file.path(calibration_output_dir, calibration_filename))) {
                # Remove output directory
                log_info("Metadata for species '", current_species, "' and colony '", current_colony, "' is empty but existing calibration file found. Removing existing calibration file.")
                unlink(file.path(calibration_output_dir, calibration_filename))
                next
            }
            # Create the dir
            dir.create(calibration_output_dir, showWarnings = FALSE, recursive = TRUE)
            # Check if settings already exist
            settings_path <- file.path(calibration_output_dir, "filter_settings.xlsx")
            if (!file.exists(settings_path) || new_filter_settings) {
                if (file.exists(settings_path)) {
                    unlink(settings_path)
                }
                seatrackRgls::create_filter_settings_file(settings_path, current_species)
            }

            # Load to get year splits
            filter_setings_list <- seatrackRgls::read_filter_file(settings_path)
            species_filter_settings <- filter_setings_list$get_settings_from_list(species = current_species)


            # Load older calibration data
            existing_calibration_data <- data.frame(logger_id = character(), total_years_tracked = character())
            if (!is.null(existing_calibration_dir)) {
                # Load existing calibration data if available and merge
                existing_calibration_file <- gls_get_existing_calibration(existing_calibration_dir, species_filter_settings$split_years)
                existing_calibration_data <- existing_calibration_file$calibration_data
                existing_calibration_data <- existing_calibration_data[existing_calibration_data$species %in% current_species & existing_calibration_data$colony %in% current_colony, ]
                if (!is.null(id_year_model)) {
                    existing_calibration_id_year <- sapply(strsplit(tools::file_path_sans_ext(existing_calibration_data$file_name), "_"), function(x) paste(c(x[1], x[2], x[3:length(x)]), collapse = "_"))

                    existing_calibration_data <- existing_calibration_data[existing_calibration_id_year == id_year_model, ]
                }
            }

            # check for current calibration data in export dir and merge it with the older calibration data
            if (file.exists(file.path(calibration_output_dir, calibration_filename))) {
                existing_export_data <- openxlsx2::read_xlsx(file.path(calibration_output_dir, calibration_filename))

                # if any columns are incompatible, coerce to character
                common_cols <- intersect(names(existing_calibration_data), names(existing_export_data))
                for (col in common_cols) {
                    if (any(class(existing_calibration_data[[col]]) != class(existing_export_data[[col]]))) {
                        existing_calibration_data[[col]] <- as.character(existing_calibration_data[[col]])
                        existing_export_data[[col]] <- as.character(existing_export_data[[col]])
                    }
                }

                non_na_export <- existing_export_data[!is.na(existing_export_data$sun_angle_start), ]

                # Keep latest
                existing_calibration_data <- existing_calibration_data[!paste(existing_calibration_data$logger_id, existing_calibration_data$total_years_tracked) %in% paste(non_na_export$logger_id, non_na_export$total_years_tracked), ]
                existing_calibration_data$problem <- rep(FALSE, nrow(existing_calibration_data))

                # Remove cases where there is uncalibrated export_data but existing calibration_data
                existing_id_year <- paste(existing_calibration_data$logger_id, existing_calibration_data$total_years_tracked)[!is.na(existing_calibration_data$sun_angle_start)]
                existing_export_data <- existing_export_data[!paste(existing_export_data$logger_id, existing_export_data$total_years_tracked) %in% existing_id_year, ]

                # Combined pre-calibrated data
                existing_calibration_data <- dplyr::bind_rows(existing_calibration_data, existing_export_data)
            }

            metadata_id_year <- paste(current_metadata$logger_id, current_metadata$total_years_tracked)
            existing_id_year <- paste(existing_calibration_data$logger_id, existing_calibration_data$total_years_tracked)

            # # Remove cases from existing calibration data where files do not exist
            existing_calibration_data <- existing_calibration_data[existing_id_year %in% metadata_id_year, ]

            existing_plot_data <- data.frame(logger_id = character(), year_tracked = character())
            # check if these already have plots
            if (!rerun_existing_plots && file.exists(file.path(calibration_output_dir, "sun_calib"))) {
                # Get plot files
                existing_plots_files <- list.files(file.path(calibration_output_dir, "sun_calib"), pattern = "*.tiff")
                if (length(existing_plots_files) > 0) {
                    existing_plots_ids <- data.frame(t(sapply(strsplit(existing_plots_files, "_"), function(x) x[1:3])))
                    names(existing_plots_ids) <- c("logger_id", "year1", "year2")
                    existing_plots_ids <- unique(paste(existing_plots_ids[, "logger_id"],
                        paste(existing_plots_ids[, "year1"],
                            existing_plots_ids[, "year2"],
                            sep = "_"
                        ),
                        sep = "_"
                    ))

                    existing_id_year <- paste(existing_calibration_data$logger_id, existing_calibration_data$year_tracked, sep = "_")
                    existing_plot_data <- existing_calibration_data[existing_id_year %in% existing_plots_ids, ]
                    # Remove existing plot rows from existing calibration data
                    existing_calibration_data <- existing_calibration_data[!existing_id_year %in% paste(existing_plot_data$logger_id, existing_plot_data$year_tracked, sep = "_"), ]

                    # Remove existing plot data from metadata
                    current_metadata <- current_metadata[!paste(current_metadata$logger_id, current_metadata$year_tracked, sep = "_") %in% paste(existing_plot_data$logger_id, existing_plot_data$year_tracked, sep = "_"), ]
                }
            }

            if (nrow(existing_calibration_data) > 0) {
                metadata_id_year <- paste(current_metadata$logger_id, current_metadata$total_years_tracked)

                # Get only unique id_year combos
                existing_calibration_data <- existing_calibration_data[!is.na(existing_calibration_data$sun_angle_start), ]
                existing_id_year <- paste(existing_calibration_data$logger_id, existing_calibration_data$total_years_tracked)

                # Remove existing calibration rows from metadata
                current_metadata <- current_metadata[!metadata_id_year %in% existing_id_year, ]
            }

            if (!rerun_existing && nrow(current_metadata) == 0) {
                log_info("All files already have calibration data.")
            } else {
                # Metadata should now only have rows for which we want to generate plots/rows.

                if (rerun_existing && nrow(existing_calibration_data) > 0) {
                    # Force existing calibration data back into metadata
                    common_cols <- intersect(names(existing_calibration_data), names(current_metadata))
                    for (col in common_cols) {
                        if (any(class(existing_calibration_data[[col]]) != class(metadata[[col]]))) {
                            existing_calibration_data[[col]] <- as.character(existing_calibration_data[[col]])
                            current_metadata[[col]] <- as.character(current_metadata[[col]])
                        }
                    }
                    current_metadata <- dplyr::bind_rows(existing_calibration_data, current_metadata)
                }


                if (nrow(current_metadata) == 0 && nrow(existing_calibration_data) == 0 && nrow(existing_plot_data) == 0) {
                    log_warn("No metadata or previous calibration data found for", current_species, current_colony)
                    next()
                }

                current_metadata$start_datetime <- as.Date(current_metadata$start_datetime)
                current_metadata$end_datetime <- as.Date(current_metadata$end_datetime)
            }

            if (nrow(current_metadata) > 0) {
                all_colony_info <- gls_seatrack_colony_info()
                calibration_template <- seatrackRgls::prepare_calibration(
                    import_directory = import_directory,
                    metadata = data.frame(current_metadata),
                    all_colony_info = all_colony_info,
                    output_directory = output_directory,
                    export_calibration_template = FALSE,
                    show_filter_plots = filter_plots,
                    filter_setting_list = settings_path
                )
            } else {
                log_info("No new metadata to process, only existing calibration data.")
                calibration_template <- data.frame(logger_id = character(), year_tracked = character())
            }

            calibration_template_id_year <- paste(calibration_template$logger_id, calibration_template$year_tracked)
            existing_id_year <- paste(existing_calibration_data$logger_id, existing_calibration_data$year_tracked)
            existing_plot_id_year <- paste(existing_plot_data$logger_id, existing_plot_data$year_tracked)

            if (include_existing) {
                common_cols <- intersect(names(existing_calibration_data), names(calibration_template))
                for (col in common_cols) {
                    if (any(class(existing_calibration_data[[col]]) != class(calibration_template[[col]]))) {
                        existing_calibration_data[[col]] <- as.character(existing_calibration_data[[col]])
                        calibration_template[[col]] <- as.character(calibration_template[[col]])
                    }
                }

                match_idx <- match(existing_id_year, calibration_template_id_year)
                calibration_template[match_idx[!is.na(match_idx)], c("sun_angle_start", "sun_angle_end", "light_threshold")] <- existing_calibration_data[!is.na(match_idx), c("sun_angle_start", "sun_angle_end", "light_threshold")]
                calibration_template <- dplyr::bind_rows(existing_calibration_data[is.na(match_idx), ], calibration_template)
            } else {
                calibration_template <- calibration_template[!calibration_template_id_year %in% existing_id_year, ]
            }

            # reattach rows skipped due to existing plots
            if (include_existing && !rerun_existing_plots) {
                common_cols <- intersect(names(existing_plot_data), names(calibration_template))
                for (col in common_cols) {
                    if (any(class(existing_plot_data[[col]]) != class(calibration_template[[col]]))) {
                        existing_plot_data[[col]] <- as.character(existing_plot_data[[col]])
                        calibration_template[[col]] <- as.character(calibration_template[[col]])
                    }
                }
                calibration_template <- dplyr::bind_rows(existing_plot_data, calibration_template)
            }

            calibration_template$start_datetime <- as.Date(calibration_template$start_datetime)
            calibration_template$end_datetime <- as.Date(calibration_template$end_datetime)
            calibration_template <- calibration_template[order(calibration_template$logger_id, calibration_template$total_years_tracked), ]

            seatrackRgls::calibration_to_wb(
                calibration_template,
                calibration_output_dir,
                calibration_filename = calibration_filename
            )
        }
    }
}

#' Check calibration status for all species/colony combinations
#'
#' Function to check the calibration status for all species/colony combinations in the Sea Track database. It looks for existing calibration files and summarizes the number of calibrated, uncalibrated, and valid entries for each combination.
#' @return A dataframe summarizing the calibration status for each species/colony combination, including the number of calibrated, uncalibrated, and valid entries, as well as the total number of entries and the path to the calibration file.
#' @export
#' @concept gls_helper
gls_check_calibration <- function() {
    output_directory <- file.path(the$sea_track_folder, "Database", "Imports_Logger data", "Callibration_GLSpositions")
    all_species <- list.dirs(output_directory, recursive = FALSE, full.names = FALSE)
    all_result <- data.frame()
    for (current_species in all_species) {
        species_dir <- file.path(output_directory, current_species)
        species_colonies <- list.dirs(species_dir, recursive = FALSE, full.names = FALSE)
        for (current_colony in species_colonies) {
            calibration_filename <- paste(current_species, current_colony, "calibration.xlsx", sep = "_")
            calibration_path <- file.path(output_directory, current_species, current_colony, calibration_filename)
            if (!file.exists(calibration_path)) {
                next
            }
            existing_export_data <- openxlsx2::read_xlsx(calibration_path)
            existing_export_data <- existing_export_data[!is.na(existing_export_data$logger_id), ]
            if (nrow(existing_export_data) == 0) {
                next
            }
            n_valid <- sum(!existing_export_data$problem | is.na(existing_export_data$problem), na.rm = TRUE)
            total <- nrow(existing_export_data)
            n_calibrated <- sum(!is.na(existing_export_data$sun_angle_start) & (!existing_export_data$problem | is.na(existing_export_data$problem)), na.rm = TRUE)
            n_uncalibrated <- sum(is.na(existing_export_data$sun_angle_start) & !existing_export_data$problem, na.rm = TRUE)
            calibration_summary <- data.frame(
                species = current_species,
                colony = current_colony,
                n_calibrated = n_calibrated,
                n_uncalibrated = n_uncalibrated,
                prop_calibrated = n_calibrated / n_valid,
                n_valid = n_valid,
                n_invalid = total - n_valid,
                total = total,
                path = calibration_path
            )
            all_result <- rbind(all_result, calibration_summary)
        }
    }
    return(all_result)
}

#' Export calibration check results to Excel
#'
#' Function to export the results of the calibration check to an Excel file. It uses the `gls_check_calibration` function to get the calibration status for all species/colony combinations and then formats the results into a flextable with colored backgrounds based on the proportion of calibrated entries. The resulting table is saved as an Excel file in the specified output directory.
#' @return None. The function saves the calibration check results to an Excel file in the specified output directory.
#' @export
#' @concept gls_helper
export_check_calibration <- function() {
    output_directory <- file.path(the$sea_track_folder, "Database", "Imports_Logger data", "Callibration_GLSpositions")
    calibration_result <- gls_check_calibration()

    calibration_result <- rbind(calibration_result, data.frame(
        species = "Total", colony = "Total", n_calibrated = sum(calibration_result$n_calibrated, na.rm = TRUE), n_uncalibrated = sum(calibration_result$n_uncalibrated, na.rm = TRUE),
        prop_calibrated = sum(calibration_result$n_calibrated, na.rm = TRUE) / sum(calibration_result$n_valid, na.rm = TRUE),
        n_valid = sum(calibration_result$n_valid, na.rm = TRUE),
        n_invalid = sum(calibration_result$n_invalid, na.rm = TRUE),
        total = sum(calibration_result$total, na.rm = TRUE), path = ""
    ))

    row_cols <- sapply(calibration_result$prop_calibrated, success_col_func, background = "white", breaks = c(0, 0.25, 0.5, 0.75, Inf))
    calibration_result_ft <- flextable(calibration_result)
    calibration_result_ft <- bg(calibration_result_ft, j = c("species", "colony", "n_calibrated", "n_uncalibrated", "prop_calibrated"), bg = row_cols)
    sheet_name <- "calibration status"
    wb <- openxlsx2::wb_workbook()$add_worksheet(sheet_name)
    wb <- flexlsx::wb_add_flextable(wb, sheet_name, calibration_result_ft)
    wb$freeze_pane(
        sheet = sheet_name,
        first_active_col = 3,
        first_active_row <- 2
    )
    wb$set_col_widths(cols = 1:ncol(calibration_result), widths = "auto")
    openxlsx2::wb_add_filter(wb, row = 1, cols = 1:ncol(calibration_result))
    wb$save(file.path(output_directory, "calibration_status.xlsx"))
}
