#' Get the path of the master import file
#'
#' This function constructs a path to the master import file for a given colony.
#' On finding the path to a colony's master import sheets, it will be stored in the internal environment for later use.
#'
#' @param colony A character string specifying the name of the colony.
#' @param use_stored If TRUE, use a pre-existing path, rather than searching for a new one. Defaults to TRUE.
#' @return A character string representing the path to the master import file.
#' @examples
#' \dontrun{
#' get_master_import_folder("ColonyA")
#' }
#' @export
#' @concept metadata
get_master_import_path <- function(colony, use_stored = TRUE) {
    if (is.null(the$sea_track_folder)) {
        stop("Sea track folder is not set. Please use set_sea_track_folder() to set it.")
    }
    log_info("Get Master import file for colony '", colony, "', use existing paths: ", use_stored)
    if (use_stored && colony %in% names(the$master_sheet_paths)) {
        full_colony_file_path <- the$master_sheet_paths[[colony]]

        if (file.exists(full_colony_file_path)) {
            log_info("Master import file for colony '", colony, "' loaded from: ", full_colony_file_path)
            return(full_colony_file_path)
        }
    }

    # Get the path to the master import folder
    master_import_folder <- file.path(the$sea_track_folder, "Database", "Imports_Metadata")

    # List all files in the master import folder
    files <- list.files(master_import_folder, pattern = "^[^~].*\\.xlsx$")

    # Split the filenames to get colony names
    colony_names <- sapply(strsplit(files, "_"), `[`, 2)

    # Try a straight match
    colony_bool <- tolower(colony) == tolower(colony_names)

    if (!any(colony_bool)) {
        # Try a within string match
        colony_bool <- grepl(colony, colony_names, ignore.case = TRUE)
    }

    if (!any(colony_bool)) {
        # stop(paste("Colony", colony, "not found in the master import folder. Available colonies are:", paste(colony_names, collapse = ", ")))
        # Try the country of the colony
        all_country_colonies <- get_all_locations()
        colony_bool <- sapply(all_country_colonies, function(country_colonies) {
            return(colony %in% country_colonies)
        })
        country <- names(colony_bool)[colony_bool]
        country_file_bool <- colony_names == country
        if (any(country_file_bool)) {
            colony_file_name <- files[country_file_bool]
        } else {
            # Final fallback, open the file and check
            log_info("Master import file for colony '", colony, "' not found by location or country. Checking intended_location")
            colony_file_name <- character()
            for (import_file in files) {
                log_trace("Check ", import_file)
                import_file_path <- file.path(master_import_folder, import_file)
                master_import_list <- load_master_import(file_path = import_file_path)
                master_import_startup <- master_import_list$data$STARTUP_SHUTDOWN
                location_bool <- grepl(colony, master_import_startup$intended_location)
                log_trace("Checked ", import_file)
                if (any(location_bool)) {
                    colony_file_name <- import_file
                    break
                }
            }
        }
    } else {
        colony_file_name <- files[colony_bool]
    }

    if (length(colony_file_name) == 0) {
        log_warn("Master import file for colony '", colony, "' not found")
        the$master_sheet_paths[[colony]] <- NULL
        return(NULL)
    }

    full_colony_file_path <- file.path(master_import_folder, colony_file_name)
    log_info("Master import file for colony '", colony, "' found at: ", full_colony_file_path)
    the$master_sheet_paths[[colony]] <- full_colony_file_path
    return(full_colony_file_path)
}

#' Set master import paths
#'
#' This function allows setting the master import paths directly, for example if they have been discovered through other means. This will overwrite any previously stored paths.
#' @param location_paths A named list of paths, where the names are colony names and the values are file paths to the master import sheets for those colonies.
#' @export
#' @concept metadata
set_master_import_paths <- function(location_paths = NULL) {
    if (!is.null(location_paths)) {
        log_info("Setting master sheet paths")
        the$master_sheet_paths <- location_paths
    }
}

#' Get master import paths
#'
#' This function retrieves the currently stored master import paths.
#' @return A named list of paths, where the names are colony names and the values are file paths to the master import sheets for those colonies.
#' @export
#' @concept metadata
get_master_import_paths <- function() {
    return(the$master_sheet_paths)
}

#' Load master import file for a given colony
#'
#' This function loads the master import file for a specified colony or directly from a file path.
#' Either a colony name of a file path must be provided.
#' It iterates through the appropriate sheets and combines the data into a list of data frames.
#' @param colony A character string specifying the name of the colony.
#' @param file_path File path of master import file
#' @param use_stored If TRUE, use a pre-existing path, rather than searching for a new one. Defaults to TRUE.
#' @return A LoadedWB object.
#' @examples
#' \dontrun{
#' load_master_import("ColonyA")
#' }
#' @export
#' @concept metadata
load_master_import <- function(colony = NULL, file_path = NULL, use_stored = TRUE) {
    if (!is.null(colony) && is.null(file_path)) {
        file_path <- get_master_import_path(colony, use_stored)
    }

    if (is.null(file_path)) {
        stop("Unable to find master import sheet for this colony.")
    }

    if (!file.exists(file_path)) {
        stop("The specified master import file does not exist.")
    }

    # Desired sheets
    sheets <- c("METADATA", "STARTUP_SHUTDOWN")
    startup_col_types <- c(
        logger_serial_no = 0,
        logger_model = 0,
        producer = 0,
        production_year = 1,
        project = 0,
        starttime_gmt = 3,
        logging_mode = 0,
        started_by = 0,
        started_where = 0,
        days_delayed = 1,
        programmed_gmt_time = 3,
        intended_species = 0,
        intended_location = 0,
        intended_deployer = 0,
        shutdown_session = 4,
        field_status = 0,
        downloaded_by = 0,
        download_type = 0,
        download_date = 2,
        decomissioned = 2,
        shutdown_date = 2,
        comment = 0
    )

    import_list <- load_sheets_as_list(file_path, sheets, col_types = list(NULL, startup_col_types))

    if (class(import_list$data$METADATA$date) != "Date") {
        log_warn(glue::glue("{file_path}: Trying to force metadata date. Check results before saving, or correct excel sheet and reimport."))
        import_list$data$METADATA$date <- as.Date(import_list$data$METADATA$date)
    }

    return(import_list)
}



#' Load master import file for all colonies
#'
#' This function attempts to load all master import sheets available in the seatrack folder.
#' @param combine Boolean determining whether or not to combine the sheets into a single dataframe.
#' @param skip character vector of location names to not load.
#' @param distinct Boolean determining whether to only keep unique sheets (non duplicated paths)
#' @param use_stored Boolean determining whether previously discovered paths can be reused. Default is TRUE
#' @return If combine is TRUE: A tibble consisting of combined metadata and startup_shutdown sheets, with an extra column for path appended to each.
#'  Otherwise a list where every element is a LoadedWB object.
#' @export
#' @concept metadata
load_all_master_import <- function(combine = TRUE, skip = c(), distinct = TRUE, use_stored = TRUE) {
    if (is.null(the$sea_track_folder)) {
        stop("Sea track folder is not set. Please use set_sea_track_folder() to set it.")
    }
    log_info(paste("Loading master imports from", the$sea_track_folder))
    all_colony <- unlist(get_all_locations())
    all_colony <- all_colony[!all_colony %in% skip]

    all_paths <- lapply(all_colony, get_master_import_path, use_stored = use_stored)

    no_path <- all_colony[which(sapply(all_paths, is.null))]
    if (length(no_path) > 0) {
        missing_import_files <- data.frame(country = names(no_path), colony = no_path)
        missing_import_files$country <- gsub("\\d+$", "", missing_import_files$country)
        row.names(missing_import_files) <- NULL
        log_warn("Import files for the following locations could not be found: \n", paste(capture.output(print(missing_import_files)), collapse = "\n"))
    }

    null_path_idx <- which(!sapply(all_paths, is.null))

    all_paths <- all_paths[null_path_idx]
    all_colony <- all_colony[null_path_idx]

    distinct_idx <- which(!duplicated(all_paths))
    all_colony_distinct <- all_colony[distinct_idx]
    all_paths_distinct <- all_paths[distinct_idx]

    all_sheets <- lapply(all_colony_distinct, load_master_import)


    if (!distinct) {
        all_sheets <- lapply(all_paths, function(current_path) {
            return(all_sheets[[which(all_paths_distinct == current_path)]])
        })
        names(all_sheets) <- all_colony
    } else {
        names(all_sheets) <- all_colony_distinct
    }

    if (!combine) {
        # If not combining, return the lists
        return(all_sheets)
    }

    return(combine_all_metadata(all_sheets, all_paths))
}

combine_all_metadata <- function(all_sheets, all_paths) {
    all_data <- lapply(all_sheets, function(x) x$data)
    # combine metadata
    all_metadata <- lapply(all_data, function(x) x$METADATA)

    # If the master_metadata sheet is is lacking the nest_latitude and nest_longitude columns, add them
    all_metadata <- lapply(all_metadata, function(x) {
        if (!"nest_latitude" %in% colnames(x)) {
            x$nest_latitude <- NA
        }
        if (!"nest_longitude" %in% colnames(x)) {
            x$nest_longitude <- NA
        }
        return(x)
    })

    all_metadata_names <- unique(unlist(sapply(all_metadata, names)))

    # which columns are in all
    names_present_bools <- lapply(all_metadata, function(x) all_metadata_names %in% names(x))
    names_present_bools <- do.call(rbind, names_present_bools)
    in_all_bool <- colSums(names_present_bools) == nrow(names_present_bools)
    in_all_names <- all_metadata_names[in_all_bool]
    not_in_all_names <- all_metadata_names[!in_all_bool]

    for (i in seq_along(all_metadata)) {
        metadata <- all_metadata[[i]]
        bad_names_bool <- not_in_all_names %in% names(metadata)
        if (any(bad_names_bool)) {
            bad_names <- not_in_all_names[bad_names_bool]
            path <- all_paths[[i]]
            log_warn(path, " had the following non-standard columns: ", paste0(bad_names, collapse = ", "))
        }
    }

    all_metadata_clean <- lapply(all_metadata, function(x) x[, in_all_names])
    all_metadata_clean <- lapply(seq_along(all_metadata_clean), function(i) {
        metadata <- all_metadata_clean[[i]]
        metadata$path <- all_paths[[i]]
        return(metadata)
    })

    all_metadata_clean <- do.call(rbind, all_metadata_clean)
    # combine startups
    all_startup <- lapply(all_data, function(x) x$`STARTUP_SHUTDOWN`)

    all_startup <- lapply(seq_along(all_startup), function(i) {
        startup <- all_startup[[i]]
        startup$path <- all_paths[[i]]
        return(startup)
    })


    all_startup <- do.call(rbind, all_startup)

    return(list(METADATA = all_metadata_clean, STARTUP_SHUTDOWN = all_startup))
}

#' Get unimported metadata
#'
#' Checks a location to see if there is unimported metadata in the "not_processed" folder.
#' If found, returns a list of file paths to these unimported metadata files.
#'
#' @param location Character string specifying the location (colony) to check for unprocessed metadata.
#'
#' @concept metadata
#' @export
get_location_unprocessed <- function(location) {
    if (is.null(the$sea_track_folder)) {
        stop("Sea track folder is not set. Please use set_sea_track_folder() to set it.")
    }
    locations_path <- file.path(the$sea_track_folder, "Locations")
    if (!dir.exists(locations_path)) {
        stop("Locations folder not found in the sea track folder.")
    }
    all_location_dirs <- list.dirs(locations_path, full.names = FALSE, recursive = TRUE)
    location_unprocessed_dir <- all_location_dirs[grepl("not_processed", tolower(all_location_dirs), fixed = TRUE) &
        grepl(location, all_location_dirs, fixed = TRUE)]
    if (length(location_unprocessed_dir) == 0) {
        log_info(paste("No 'not_processed' folder found for location:", location))
        return(NULL)
    }
    unprocessed_files_list <- lapply(location_unprocessed_dir, function(unprocessed_dir) {
        list.files(file.path(locations_path, unprocessed_dir), pattern = "^[^~].*\\.xls*", full.names = TRUE)
    })
    unprocessed_files <- unlist(unprocessed_files_list)
    if (length(unprocessed_files) == 0) {
        log_info(paste("No unprocessed files found for location:", location))
        return(NULL)
    }
    return(unprocessed_files)
}

#' Add partner provided metadata to the master import file
#'
#' This function adds metadata provided by partners to a master import file of the appropriate colony.
#' It firstly adds missing sessions by checking start up files.
#' It then appends the reported encounter data, avoiding duplicate rows.
#' Finally it updates sessions based on reported logger returns. This includes generating new sessions for loggers restarted in the field.
#'
#' @param colony A character string specifying the name of the colony.
#' @param new_metadata List of tibbles, each corresponding to a sheet in the partner provided information file.
#' @param master_import List of tibbles, each corresponding to a sheet in the master import file.
#' @param nonresponsive_list A named list of tibbles, each containing nonresponsive logger data for different manufacturers.
#'
#' @return An updated version of the master import file, as a list where each element is a sheet from the excel file.
#' @export
#' @concept metadata
handle_partner_metadata <- function(colony, new_metadata, master_import, nonresponsive_list = LoadedWBCollection$new()) {
    log_info_all(paste("Handle partner metadata", new_metadata$path, "\n for", colony, master_import$path))
    if (new_metadata$version == "2025") {
        required_sheets <- c("ENCOUNTER DATA", "LOGGER RETURNS", "RESTART TIMES")
    } else if (new_metadata$version == "2026") {
        required_sheets <- c("ENCOUNTER DATA", "LOGGER RETURNS", "LOGGER STARTUPS")
    }
    if (!all(required_sheets %in% names(new_metadata$data))) {
        stop(glue::glue("new_metadata must contain the sheets: {paste(required_sheets, collapse = ", ")}"))
    }

    if (!all(c("METADATA", "STARTUP_SHUTDOWN") %in% names(master_import$data))) {
        stop("master_import must contain the sheets: METADATA, STARTUP_SHUTDOWN")
    }

    if (new_metadata$version == "2026") {
        # Handle adding extra logger startups
    }

    log_info("Add missing sessions from start up files")
    updated_loggers <- add_loggers_from_startup(master_import, new_metadata)

    master_import$data$`STARTUP_SHUTDOWN` <- updated_loggers

    log_info("Check for duplicate sessions")
    logger_id_date <- paste(master_import$data$STARTUP_SHUTDOWN$logger_serial_no, as.character(master_import$data$STARTUP_SHUTDOWN$starttime_gmt))
    master_import$data$`STARTUP_SHUTDOWN` <- master_import$data$`STARTUP_SHUTDOWN`[!duplicated(logger_id_date), ]

    log_info("Append encounter data")
    updated_metadata <- append_encounter_data(master_import$data$METADATA, new_metadata$data$`ENCOUNTER DATA`, new_metadata$version)

    master_import$data$METADATA <- updated_metadata

    log_info("Update sessions from logger returns")
    if (new_metadata$version == "2025") {
        restart_sheet <- new_metadata$data$`RESTART TIMES`
    } else if (new_metadata$version == "2026") {
        restart_sheet <- new_metadata$data$`LOGGER STARTUPS`
    }
    updated_sessions <- handle_returned_loggers(
        colony,
        master_import$data$`STARTUP_SHUTDOWN`,
        new_metadata$data$`LOGGER RETURNS`,
        restart_sheet,
        nonresponsive_list,
        new_metadata$version
    )

    master_import$data$`STARTUP_SHUTDOWN` <- updated_sessions$master_startup
    master_import$modified <- TRUE
    nonresponsive_list <- updated_sessions$nonresponsive_list
    log_info_all(paste("Finished handling partner metadata", new_metadata$path, "\n for", colony, master_import$path))
    return(list(master_import = master_import, nonresponsive_list = nonresponsive_list))
}

#' Given a new master import, modify all master imports in a list sharing the same path
#'
#' When loading all master import files with distinct = FALSE, we will end up with duplicate master import files.
#' This is because multiple colonies can share the same master import file.
#' If we modify one of these (for example, updating from partner metadata for that colony), we need to make sure the files stay in sync.
#'
#' @param all_master_import A list of master imports, as produced by load_all_master_import(combine = FALSE, distinct = FALSE)
#' @param new_master_import A modified master import sheet to be distributed throughout the list
#' @return An updated list of master imports, with the new master import replacing any with the same path.
#' @export
#' @concept metadata
modify_master_import_in_list <- function(all_master_import, new_master_import) {
    new_master_import_path <- new_master_import$path

    new_all_master_import <- lapply(all_master_import, function(x) {
        if (x$path == new_master_import_path) {
            return(new_master_import)
        } else {
            return(x)
        }
    })

    return(new_all_master_import)
}


#' Load partner provided metadata from an Excel file
#'
#' This function reads metadata provided by partners from an Excel file.
#' It iterates through the appropriate sheets and combines the data into a list of data frames.
#' @param file_path A character string specifying the absolute path to the Excel file.
#' @return A list of data frames, each corresponding to a sheet in the Excel file.
#' @examples
#' \dontrun{
#' load_partner_metadata("path/to/partner_metadata.xlsx")
#' }
#' @export
#' @concept metadata
load_partner_metadata <- function(file_path) {
    if (!file.exists(file_path)) {
        stop("The specified file does not exist.")
    }
    # Peak at file to ascertain version

    wb <- openxlsx2::wb_load(file_path)
    if ("sheet_metadata" %in% wb$get_sheet_names()) {
        sheet_metadata <- openxlsx2::wb_to_df(wb, "sheet_metadata")
        version <- sheet_metadata$version
    } else {
        version <- "2025"
    }

    if (version == "2025") {
        # Desired sheets (pre 2026)
        sheets <- c("ENCOUNTER DATA", "LOGGER RETURNS", "RESTART TIMES")
        restart_sheet_types <- c(
            logger_id = 0,
            logger_model = 0,
            startdate_GMT = 2,
            starttime_GMT = 3,
            `Logging mode` = 0,
            intended_species = 0,
            comment = 0
        )
    } else if (version == "2026") {
        # Desired sheets (2026)
        sheets <- c("ENCOUNTER DATA", "LOGGER RETURNS", "LOGGER STARTUPS")
        restart_sheet_types <- c(
            logger_serial_no = 0,
            logger_model = 0,
            producer = 0,
            production_year = 1,
            project = 0,
            starttime_gmt = 3,
            logging_mode = 0,
            started_by = 0,
            started_where = 0,
            days_delayed = 1,
            programmed_gmt_time = 3,
            intended_species = 0,
            intended_location = 0,
            intended_deployer = 0,
            comment = 0
        )
    }



    # Skip the first row as it contains extra headers.
    metadata_list <- load_sheets_as_list(file_path, sheets, 1,
        col_types = list(
            NULL,
            c(
                logger_id = 0,
                `logger model` = 0,
                `downloaded by` = 0,
                `download / stop_date` = 2,
                status = 0,
                `stored or sent to?` = 0,
                comment = 0
            ),
            restart_sheet_types
        ),
        col_upper = list(
            c("logger_id_retrieved", "logger_id_deployed"),
            c("logger_id"),
            c("logger_id", "logger_serial_no")
        )
    )
    if (any(is.na(as.Date(metadata_list$data$`ENCOUNTER DATA`$date)))) {
        stop("Error reading ENCOUNTER DATA date column. Check date formatting.")
    }
    return(metadata_list)
}

#' Save all modified master sheets
#'
#' This function will save any modified master sheets from a LoadedWBCollection object.
#' @param all_master_sheets LoadedWBCollection object.
#' @export
#' @concept metadata
save_all_modified <- function(all_master_sheets) {
    all_modified <- all_master_sheets$modified
    for (new_master_sheets in all_modified) {
        save_master_sheet(new_master_sheets, modified_only = TRUE)
    }
}

#' Save a master sheet to an Excel file.
#'
#' This function writes the provided data frame (`new_master_sheets`) to an Excel file
#' specified by `filename`.
#'
#' @param new_master_sheets A LoadedWB object.
#' @param filepath A string specifying the path and name of the Excel file to be created. If NULL will be path of the loaded sheet.
#' @param modified_only Only save the file if the sheet has been flagged as modified.
#'
#' @return The function returns the updated LoadedWB object after saving.
#'
#' @examples
#' \dontrun{
#' save_master_sheet(new_master_sheets, "output.xlsx")
#' }
#'
#' @export
#' @concept metadata
save_master_sheet <- function(new_master_sheets, filepath = NULL, modified_only = FALSE) {
    if (is.null(new_master_sheets)) {
        stop("new_master_sheets is null!")
    }
    if (modified_only && !new_master_sheets$modified) {
        log_trace("Sheet has not been modified.")
        return()
    }
    if (is.null(filepath)) {
        filepath <- new_master_sheets$wb$path
    }

    for (sheet in names(new_master_sheets$data)) {
        sheet_index <- which(names(new_master_sheets$data) == sheet)
        hidden_rows <- new_master_sheets$wb$worksheets[[sheet_index]]$sheet_data$row_attr$hidden
        new_master_sheets$wb$worksheets[[sheet_index]]$sheet_data$row_attr$hidden <- rep("", length(hidden_rows))

        sheet_index <- which(names(new_master_sheets$data) == sheet)
        hidden_rows <- new_master_sheets$wb$worksheets[[sheet_index]]$sheet_data$row_attr$hidden
        new_master_sheets$wb$worksheets[[sheet_index]]$sheet_data$row_attr$hidden <- rep("", length(hidden_rows))

        new_master_sheets$wb$add_data(
            sheet = sheet,
            x = new_master_sheets$data[[sheet]],
            with_filter = TRUE,
            remove_cell_style = TRUE,
            na.strings = ""
        )
        new_master_sheets$wb$freeze_pane(
            sheet = sheet,
            first_active_col = 5,
            first_active_row <- 2
        )
        new_master_sheets$wb$set_col_widths(sheet = sheet, cols = seq_len(ncol(new_master_sheets$data[[sheet]])), widths = "auto")
        new_master_sheets$wb$set_row_heights(sheet = sheet, rows = seq_len(nrow(new_master_sheets$data[[sheet]]) + 1), hidden = FALSE)
        wb_set_row_heights(new_master_sheets$wb, sheet = sheet, rows = seq_len(nrow(new_master_sheets$data[[sheet]]) + 1), hidden = FALSE)
        new_master_sheets$wb$set_col_widths(sheet = sheet, cols = seq_len(ncol(new_master_sheets$data[[sheet]])), widths = "auto")
        new_master_sheets$wb$set_row_heights(sheet = sheet, rows = seq_len(nrow(new_master_sheets$data[[sheet]]) + 1), hidden = FALSE)
        wb_set_row_heights(new_master_sheets$wb, sheet = sheet, rows = seq_len(nrow(new_master_sheets$data[[sheet]]) + 1), hidden = FALSE)
        new_master_sheets$wb$remove_conditional_formatting(sheet = sheet)
        col_dims <- seq_len(ncol(new_master_sheets$data[[sheet]]))
        dims_mat_header <- openxlsx2::wb_dims(rows = 1, cols = col_dims)
        new_master_sheets$wb$add_fill(sheet, dims_mat_header, openxlsx2::wb_color(hex = "#ACB9CA"))

        logger_dims <- openxlsx2::wb_dims(x = new_master_sheets$data[[sheet]], cols = which(colnames(new_master_sheets$data[[sheet]]) %in% c("ring_number", "logger_id_deployed", "logger_id_retrieved", "logger_serial_no")), select = "data")
        new_master_sheets$wb$add_font(sheet, logger_dims, name = "Consolas")

        if (sheet == "STARTUP_SHUTDOWN") {
            col_dims <- openxlsx2::wb_dims(
                x = new_master_sheets$data[[sheet]],
                cols = c("download_date", "shutdown_date"), select = "col_names"
            )


            col_letters <- sapply(strsplit(col_dims, ","), function(x) gsub("[[:digit:]]+", "", x))

            cf_formula <- sprintf('=OR($%s2<>"", $%s2<>"")', col_letters[1], col_letters[2])

            tryCatch(
                {
                    suppressWarnings(
                        new_master_sheets$wb$add_dxfs_style(
                            "seatrack_pos",
                            fontColour = openxlsx2::wb_color(hex = "#006100"),
                            bgFill = openxlsx2::wb_color("#C6EFCE")
                        )
                    )
                },
                error = function(e) {
                    invisible(NULL)
                }
            )

            new_master_sheets$wb$add_conditional_formatting(
                sheet,
                dims = openxlsx2::wb_dims(x = new_master_sheets$data[[sheet]], select = "data"),
                rule = cf_formula,
                style = "seatrack_pos"
            )
        }
        # full_dims <- openxlsx2::wb_dims(x = new_master_sheets$data[[sheet]])
        # all_col_filters <- lapply(1:ncol(new_master_sheets$data[[sheet]]), function(i) {
        #     glue::glue('<filterColumn colId="{i}">
        #     </filterColumn>')
        # })
        # all_col_filters <- paste(all_col_filters, collapse = "\n")
        # autoFilter_value <- glue::glue('
        # <autoFilter ref="{full_dims}">
        # {all_col_filters}
        # </autoFilter>
        # ')
        # new_master_sheets$wb$worksheets[[which(names(new_master_sheets$data) == sheet)]]$autoFilter <- autoFilter_value
    }


    new_master_sheets$wb$save(filepath)
    new_master_sheets$modified <- FALSE
    log_success(paste("Saved master import sheet to", filepath))
    return(new_master_sheets)
}
