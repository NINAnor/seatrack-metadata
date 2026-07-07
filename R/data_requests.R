get_col_from_list <- function(data_list, col_names) {
    for (curr_data in data_list) {
        for (col_name in col_names){
            if (col_name %in% names(curr_data)) {
                return(curr_data[[col_name]])
            }
        }
    }
    return(NULL)
}

popmap_latin_to_english <- function(latin) {
    # For now. Better to have this in the database.
    species_names <- data.frame(
        latin = c(
            "Alle alle",
            "Fratercula arctica",
            "Fulmarus glacialis",
            "Rissa tridactyla",
            "Uria aalge",
            "Uria lomvia"
        ),
        common = c(
            "Little auk",
            "Atlantic puffin",
            "Northern fulmar",
            "Black-legged kittiwake",
            "Common guillemot",
            "Br\u00FCnnich's guillemot"
        )
    )
    return(species_names$common[species_names$latin == latin])
}

popmap_colonies_to_db <- function(popmap_colony, colony_list = getColonies()) {
    if (popmap_colony %in% colony_list$colony_int_name) {
        return(popmap_colony)
    }
    # Fuzzy match popmap colony names to SEATRACK colony names
    matched_colonies <- colony_list$colony_int_name[agrep(popmap_colony, colony_list$colony_int_name, max.distance = 4)]
    if (length(matched_colonies) == 0) {
        warning(paste0("No matching colony found for popmap colony: ", popmap_colony))
        return(popmap_colony)
    } else if (length(matched_colonies) > 1) {
        warning(paste0("Multiple matching colonies found for popmap colony: ", popmap_colony, ". Using first match: ", matched_colonies[1]))
    }
    return(matched_colonies[1])
}

#' Export a data request package
#'
#' This function will create a zip file of seatrack data. This includes:
#' - Writing each data type as a compressed parquet file
#' - Creating a README file with metadata about the data request
#' @param data_request_result List containing the result of a data request, which will contain all the arguments needed for the export. If this is provided, other arguments are ignored.
#' @param all_data A named list of lists containing `data`: a data.frame to be exported and `description`: A string description. Each name corresponds to a data type.
#' @param request_name A string representing the name of the data request.
#' @param output_dir An optional string specifying the directory where the zip file will be saved. Defaults to `requested_data_packages/<current_year>`.
#' @param additional_notes An optional string containing additional notes to be included in the README file.
#' @param species A character vector of species included in the data request. If NULL, it will be inferred from the data.
#' @param times A vector of two dates representing the start and end of the data request. If NULL, it will be inferred from the data.
#' @param colony A character vector of colonies included in the data request. If NULL, it will be inferred from the data.
#' @param additional_data_files An optional list of additional data files to include in the data directory of the exported zip file. Each element of the list should contain the file path to the file to be included and a description.
#' @param additional_files An optional list of additional files to include in the base directory of the exported zip file. Each element of the list should contain the file path to the file to be included and a description.
#' If NULL, it will be saved in a default location based on the current year.
#' @return None. The function creates a zip file in the specified output directory.
#' @concept data_requests
#' @export
export_data_package <- function(data_request_result = NULL, all_data = NULL, request_name = NULL, output_dir = NULL, species = NULL, colony = NULL, times = NULL, additional_notes = "", additional_data_files = list(), additional_files = list()) {
    creation_date <- Sys.Date()

    if (!is.null(data_request_result)) {
        all_data <- data_request_result$all_data
        request_name <- data_request_result$request_name
        output_dir <- data_request_result$output_dir
        species <- data_request_result$species
        colony <- data_request_result$colony
        times <- data_request_result$times
        additional_notes <- data_request_result$additional_notes
        additional_data_files <- data_request_result$additional_data_files
        additional_files <- data_request_result$additional_files
    }

    if (is.null(all_data)) {
        stop("Either all_data or data_request_result must be provided.")
    }

    if (is.null(request_name)) {
        stop("request_name must be provided.")
    }


    tmp_dir <- tempfile(pattern = paste0(request_name, "_"))
    dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(tmp_dir, "data"), recursive = TRUE)
    log_info(paste0("Creating data package in temporary directory: ", tmp_dir))
    file_list <- list()

    for (type in names(all_data)) {
        if (type %in% c("raw_data")) {
            next()
        }
        log_info(paste0("Writing data type: ", type))
        file_name <- paste0(request_name, "_", type, "_", creation_date, ".gz.parquet")
        file_list[[type]] <- list(path = file_name, description = all_data[[type]]$description)
        arrow::write_parquet(all_data[[type]]$data, file.path(tmp_dir, "data", file_name), compression = "gzip")
    }
    all_data_data_only <- lapply(all_data, function(x) {
        x$data
    })

    if (is.null(species)) {
        species <- get_col_from_list(all_data_data_only, c("species", "deployment_species"))
        if (!is.null(species)) {
            species <- unique(species)
        }
    }

    if (is.null(times)) {
        times <- get_col_from_list(all_data_data_only, "date_time")
        if (!is.null(times)) {
            times <- c(min(as.Date(times)), max(as.Date(times)))
        }
    }

    if (is.null(colony)) {
        colony <- get_col_from_list(all_data_data_only, "colony")
        if (!is.null(colony)) {
            colony <- unique(colony)
        }
    }

    sessions <- unique(get_col_from_list(all_data_data_only, "session_id"))
    if (!is.null(sessions)) {
        data_responsible <- seatrackR::get_responsible(session = sessions)
    } else if (!is.null(species) && !is.null(colony)) {
        data_responsible <- seatrackR::get_responsible(species = species, colony = colony)
    } else {
        data_responsible <- NULL
    }

    if ("raw_data" %in% names(all_data)) {
        raw_data_files <- all_data$raw_data$data
        file_list$raw_data <- list(description = "raw_data", files = list())
        dir.create(file.path(tmp_dir, "data", "raw_data"), recursive = TRUE)
        for (i in seq_len(nrow(raw_data_files$local_files))) {
            current_file <- raw_data_files$local_files[i, ]
            if (!file.exists(current_file$file_path)) {
                log_warn(paste0("Raw data file not found, skipping: ", current_file$file_path))
                next
            }
            log_info(paste0("Adding raw data file to data package: ", current_file$file_path))
            new_name <- paste0(current_file$session_id, current_file$file_basename)
            new_path <- file.path(tmp_dir, "data", "raw_data", new_name)
            file.copy(current_file$file_path, new_path)
            file_list$raw_data$files[[paste("raw_data", current_file$session_id, current_file$filename, sep = "_")]] <- list(
                path = file.path("raw_data", new_name),
                description = paste0("Raw ", current_file$file_basename, " for session ", current_file$session_id)
            )
        }
        # Do the same for FTP files.
    }

    if (length(additional_data_files) > 0) {
        db_colonies <- seatrackR::getColonies()
        for (i in seq_along(additional_data_files)) {
            additional_file <- additional_data_files[[i]]
            if (!file.exists(additional_file$path)) {
                log_warn(paste0("Additional file not found, skipping: ", additional_file$path))
                additional_data_files[[i]] <- NULL
                next
            }
            log_info(paste0("Adding additional data file to data package: ", additional_file$path))
            current_path <- additional_file$path
            current_description <- additional_file$description
            new_path <- file.path(tmp_dir, "data", basename(current_path))
            file.copy(current_path, new_path)

            # Try to extract metadata from additional_data_file
            # Check if this is a pop map
            if (grepl("Abundance_Model", additional_file$path)) {
                if (is.null(colony)) {
                    colony <- c()
                }
                if (is.null(species)) {
                    species <- c()
                }
                if (is.null(data_responsible)) {
                    data_responsible <- data.frame()
                }

                file_name <- basename(current_path)
                split_file_name <- strsplit(file_name, "_")[[1]]
                latin <- paste(split_file_name[c(4, 5)], collapse = " ")
                version <- paste(split_file_name[c(7, 8)], collapse = ".")
                common <- popmap_latin_to_english(latin)

                species <- c(species, common)
                loaded_netcdf <- ncdf4::nc_open(new_path)
                colony_names <- ncdf4::ncvar_get(loaded_netcdf, "SmcolName")
                ncdf4::nc_close(loaded_netcdf)
                colony_names <- gsub(" ", "_", colony_names)
                colony_names <- sapply(colony_names, popmap_colonies_to_db, colony_list = db_colonies)

                colony <- c(colony, colony_names)

                current_description <- glue::glue("SEATRACK population map {version} - {common}")

                popmap_responsible <- seatrackR::get_responsible(colony = colony_names, species = common)
                data_responsible <- rbind(data_responsible, popmap_responsible)
                file_list[[paste("Abundance_model", version, common, sep = "_")]] <- list(path = basename(current_path), description = current_description)
            }
        }
    }
    additional_data_files <- additional_data_files[!sapply(additional_data_files, is.null)]

    if (length(additional_files) > 0) {
        for (i in seq_along(additional_files)) {
            additional_file <- additional_files[[i]]
            if (!file.exists(additional_file$path)) {
                log_warn(paste0("Additional file not found, skipping: ", additional_file$path))
                additional_files[[i]] <- NULL
                next
            }
            log_info(paste0("Adding additional file to data package: ", additional_file$path))
            current_path <- additional_file$path
            new_path <- file.path(tmp_dir, basename(current_path))
            file.copy(current_path, new_path)
        }
    }
    additional_files <- additional_files[!sapply(additional_files, is.null)]


    # render markdown template to the temp dir
    create_readme(
        request_name = request_name,
        file_list = file_list,
        species = species,
        colonies = colony,
        times = times,
        data_responsible = data_responsible,
        data_dir = tmp_dir,
        additional_notes = additional_notes,
        additional_files = additional_files,
        output_file = file.path(tmp_dir, paste0("README_", request_name, "_", creation_date, ".html"))
    )

    if (is.null(output_dir)) {
        output_dir <- file.path("requested_data_packages", format(creation_date, "%Y"))
    }
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    log_info(paste0("Creating zip file in output directory: ", output_dir))
    zipfile <- file.path(output_dir, paste0(request_name, "_", creation_date, ".zip"))
    zip::zipr(
        zipfile = zipfile,
        files = tmp_dir,
        include_directories = TRUE,
        recurse = TRUE
    )
    log_info(paste0("Data package created: ", zipfile))
    unlink(tmp_dir)
    log_info("Temporary files cleaned up.")
}

#' Create SEATRACK documentation README
#'
#' This function generates a README file for a SEATRACK data request package using R Markdown.
#' The README includes metadata about the data request, such as species, colonies, time span, and additional notes.
#' @param request_name A string representing the name of the data request.
#' @param file_list A character vector of file names included in the data request.
#' @param species A character vector of species included in the data request.
#' @param colonies A character vector of colonies included in the data request.
#' @param times A vector of two dates representing the start and end of the data request
#' @param data_responsible Table of people responsible for the data
#' @param data_dir A string representing the directory where the data files are located.
#' @param additional_notes An optional string containing additional notes to be included in the README file
#' @param additional_files An optional list of additional files to include in the README file. Each element of the list should contain the file path to the file to be included and a description.
#' @param output_file A string representing the name and path under which the README file will be saved.
#' @return None. The function creates a README file in the specified data directory.
#' @concept data_requests
#' @export
create_readme <- function(request_name, file_list, species, colonies, times, data_responsible, data_dir, additional_notes = "", additional_files = list(), output_file = "README.html") {
    print("Creating README file...")
    print(file_list)
    rmarkdown::render(
        system.file("rmd", "README_template.Rmd", package = "seatrackRtools"),
        params = list(
            request_name = request_name,
            file_list = file_list,
            species = species,
            colonies = colonies,
            times = times,
            data_responsible = data_responsible,
            data_dir = data_dir,
            notes = additional_notes,
            additional_files = additional_files
        ),
        output_file = output_file,
        envir = new.env(),
        encoding = "UTF-8"
    )
}


#' Get SEATRACK data request
#'
#' This function retrieves SEATRACK data for a specified request, including position data, morphological and breeding data, and recordings of light, temperature, and activity.
#' If `export` is TRUE the `export_data_package` function is used to create a zip file containing the data. This includes writing each data type as a compressed parquet file and creating a README file with metadata about the data request.
#' If `export` is FALSE the function returns a named list of data.frames containing the requested data. This can be edited and passed to `export_data_package` later if desired.
#' Requires an active connection to the SEATRACK database.
#' Currently netCDF files are not included in the data request package, so these have to be injected using `additional_data_files` if needed.
#' @param request_name A string representing the name of the data request.
#' @param data_types A character vector specifying the types of data to include in the request. Possible values are "GLS_positional_data", "IRMA_positional_data", "individual_data", "light", "temperature", "activity", "population_maps" and "logger_info". Defaults to all types.
#' @param start_year An integer representing the start year for the data request.
#' @param end_year An integer representing the end year for the data request. Defaults to the current year.
#' @param species An optional string specifying the species to filter the data. If NULL, data for all species will be retrieved.
#' @param colony An optional string specifying the colony to filter the data. If NULL, data for all colonies will be retrieved.
#' @param age_deployment An optional string specifying the age class to filter the data. Possible values are "A" for adults and "C" for juveniles. Defaults to "A".
#' @param project An optional string specifying the project to filter the data. Defaults to "SEATRACK".
#' @param exclude_embargoed A boolean indicating whether to exclude data that is currently under embargo. Defaults to TRUE.
#' @param session_ID Optional list of strings specifying exact session IDs.
#' @param export A boolean indicating whether to export the data package as a zip file. If FALSE, the function will return the data as a list instead.
#' @param output_dir An optional string specifying the directory where the exported zip file will be saved.
#' @param additional_notes An optional string containing additional notes to be included in the README file in the export.
#' @param additional_data_files An optional list of additional data files to include in the data directory of the exported zip file. Each element of the list should contain the file path to the file to be included and a description.
#' @param additional_data An optional named list of lists containing `data`: a data.frame to be included in the export and `description`: A string description. Each name corresponds to a data type. This can be used to include data that is not directly available through the SEATRACK database, but is relevant for the data request.
#' @param additional_files An optional list of additional files to include in the exported zip file. Each element of the list should contain the file path to the file to be included and a description.
#' If NULL, it will be saved in a default location based on the current year.
#' @return If `export` is `TRUE`: None. The function creates a zip file in the specified output directory.
#' If `export` is `FALSE`: A named list of data.frames containing the requested data.
#' @examples
#' \dontrun{
#' data_request("Mosbech_120925", 2023, 2025, "Common eider", "Christiansø")
#' }
#' @export
#' @concept data_requests
data_request <- function(
    request_name,
    data_types = c("raw_data", "GLS_positional_data", "IRMA_positional_data", "individual_data", "light", "temperature", "activity", "population_maps", "logger_info", "immersion"),
    start_year = "2000", end_year = format(Sys.Date(), "%Y"), species = NULL, colony = NULL,
    age_deployment = "A",
    project = NULL,
    exclude_embargoed = TRUE,
    session_ID = NULL,
    export = TRUE, output_dir = NULL,
    additional_notes = "", additional_data_files = list(), additional_files = list(), additional_data = list()) {
    start_date <- as.Date(paste0(start_year, "-01-01"))
    end_date <- as.Date(paste0(end_year, "-12-31")) + 1
    all_data <- list()
    if (!is.vector(data_types)) {
        data_types <- c(data_types)
    }
    data_types <- match.arg(data_types, several.ok = TRUE)
    data_types[data_types == "immersion"] <- "activity"

    # check colonies
    if (!is.null(colony)) {
        available_colonies <- seatrackR::getColonies(allLocations = TRUE)$location_name
        missing_colony_bool <- !colony %in% available_colonies
        if (any(missing_colony_bool)) {
            missing_colonies <- paste(colony[missing_colony_bool], collapse = ", ")
            stop(paste(c("Colonies", missing_colonies, "not in database."), collapse = " "))
        }
    }

    log_info(paste("Starting data request, datatypes are", paste(data_types, collapse = " ")))

    if ("GLS_positional_data" %in% data_types) {
        log_info("Fetching GLS position data...")
        all_pos <- seatrackR::getPositions(species = species, colony = colony, age_deployment_class = age_deployment, sessionId = session_ID, project = project, exclude_embargoed = exclude_embargoed)
        all_data$GLS_positional_data <- list(
            data = all_pos[all_pos$date_time >= start_date & all_pos$date_time < end_date, ],
            description = "GLS Positional data"
        )
    }

    if ("IRMA_positional_data" %in% data_types) {
        log_info("Fetching IRMA position data...")
        irma_pos <- seatrackR::getPositions(datatype = "IRMA", species = species, colony = colony, age_deployment_class = age_deployment, sessionId = session_ID, project = project, exclude_embargoed = exclude_embargoed)
        all_data$IRMA_positional_data <- list(
            data = irma_pos[irma_pos$date_time >= start_date & irma_pos$date_time < end_date, ],
            description = "IRMA Positional data"
        )
    }

    all_data <- c(all_data, additional_data)

    if (any(c("GLS_positional_data", "IRMA_positional_data", "individual_data", "light", "temperature", "activity") %in% c(data_types, names(all_data)))) {
        log_info("Fetching individual data...")
        individuals <- seatrackR::getIndividInfo(colony = colony, year = NULL, age_at_deployment = age_deployment, species = species, session_id = session_ID, project = project, exclude_embargoed = exclude_embargoed)

        if ("individual_data" %in% data_types) {
            # Filter the individual information based on the data being returned
            session_ids <- c()
            if ("GLS_positional_data" %in% names(all_data)) {
                session_ids <- c(session_ids, unique(all_data$GLS_positional_data$data$session_id))
            }
            if ("IRMA_positional_data" %in% names(all_data)) {
                session_ids <- c(session_ids, unique(all_data$IRMA_positional_data$data$session_id))
            }
            if (length(session_ids) > 0) {
                individuals <- individuals[individuals$session_id %in% session_ids, ]
            } else {
                overlap_individuals <- lapply(unique(individuals$session_id), function(x) {
                    current_individ <- individuals[individuals$session_id == x, ]
                    deployment <- current_individ[current_individ$eventType == "Deployment", ]
                    retrieval <- current_individ[current_individ$eventType == "Retrieval", ]
                    deployment_date <- min(deployment$status_date)
                    if (nrow(retrieval) > 0) {
                        retrieval_date <- max(retrieval$status_date)
                    } else {
                        retrieval_date <- as.Date(Inf)
                    }

                    if ((deployment_date <= end_date) && (retrieval_date >= start_date)) {
                        return(current_individ)
                    } else {
                        return(NULL)
                    }
                })
                individuals <- do.call(rbind, overlap_individuals)
            }
            all_data$individual_data <- list(data = individuals, description = "Individual information data")
        }
    }
    if ("raw_data" %in% data_types) {
        all_data$raw_data <- list(
            data = find_raw_data(session_ids = unique(all_data$individual_data$data$session_id)),
            description = "Raw data files"
        )
    }

    if ("logger_info" %in% data_types) {
        log_info("Fetching logger data...")


        session_ids <- c()
        if ("GLS_positional_data" %in% names(all_data)) {
            session_ids <- c(session_ids, unique(all_data$GLS_positional_data$data$session_id))
        }
        if ("IRMA_positional_data" %in% names(all_data)) {
            session_ids <- c(session_ids, unique(all_data$IRMA_positional_data$data$session_id))
        }
        if ("raw_data" %in% names(all_data)) {
            session_ids <- c(session_ids, unique(c(all_data$raw_data$data$local_files$session_id, all_data$raw_data$data$ftp_files$session_id)))
        }
        if ("individual_data" %in% names(all_data)) {
            session_ids <- c(session_ids, unique(all_data$individual_data$data$session_id))
        }

        if (length(session_ids) > 0) {
            logger_info <- seatrackR::getLoggerInfo(session = session_ids, exclude_embargoed = exclude_embargoed)
        } else {
            logger_info <- seatrackR::getLoggerInfo(colony = colony, species = species, project = project, exclude_embargoed = exclude_embargoed)

            logger_info <- logger_info[logger_info$deployment_date >= start_date & (!is.na(logger_info$retrieval_date) & logger_info$retrieval_date < end_date), ]
            session_ids <- unique(logger_info$session_id)
        }
        all_data$logger_info <- list(data = logger_info, description = "Logger info")
    }

    descriptions <- c(
        light = "Light data", temperature = "Temperature data", activity = "Standardized immersion data"
    )
    types <- data_types[data_types %in% c("light", "temperature", "activity")]
    if (length(types) > 0) {
        activity_light_temp <- lapply(types, function(type) {
            log_info(paste0("Fetching ", type, " data..."))
            recordings <- seatrackR::getRecordings(type = type, sessionId = session_ids, exclude_embargoed = exclude_embargoed)
            recordings <- recordings[recordings$date_time >= start_date & recordings$date_time < end_date, ]
            return(list(data = recordings, description = descriptions[type]))
        })
        names(activity_light_temp) <- types
        names(activity_light_temp)[names(activity_light_temp) == "activity"] <- "immersion"
        names(activity_light_temp) <- paste0(names(activity_light_temp), "_data")
        all_data <- c(all_data, activity_light_temp)
    }

    data_exists_bool <- sapply(all_data, function(x) {
        if (!is.null(x$data)) {
            if ("data.frame" %in% class(x$data)) {
                if (nrow(x$data) == 0) {
                    return(FALSE)
                }
            }
            return(TRUE)
        }
        return(FALSE)
    })
    # should log the removal of the data
    all_data <- all_data[data_exists_bool]

    if (any(c("GLS_positional_data", "IRMA_positional_data", "light_data", "temperature_data", "immersion_data") %in% names(all_data))) {
        times <- NULL
    } else {
        times <- c(start_date, end_date)
    }

    if (any(c("GLS_positional_data", "IRMA_positional_data", "individual_data") %in% names(all_data))) {
        species <- NULL
        colony <- NULL
    }

    data_request_result <- list(
        all_data = all_data,
        request_name = request_name,
        output_dir = output_dir,
        species = species,
        colony = colony,
        times = times,
        additional_notes = additional_notes,
        additional_data_files = additional_data_files,
        additional_files = additional_files
    )

    if (export) {
        log_info("Exporting data package...")
        export_data_package(data_request_result = data_request_result)
    } else {
        return(
            data_request_result
        )
    }
}

#' Get IRMA data from OneDrive
#'
#' This function retrieves IRMA positional data from OneDrive for a specified species, colony, time range. and age class.
#' The function reads the IRMA data from RDS files stored in the SEATRACK OneDrive folder, filters the data based on the provided parameters, and returns a data frame containing the relevant IRMA positional data.
#' @param start_year An integer representing the start year for the data retrieval.
#' @param end_year An integer representing the end year for the data retrieval. Defaults to the current year.
#' @param species An optional string specifying the species to filter the data. If NULL, data for all species will be retrieved.
#' @param colony An optional string specifying the colony to filter the data. If NULL, data for all colonies will be retrieved.
#' @param session_ids Optional list of strings specifying exact session IDs to filter the data. If NULL, data for all sessions will be retrieved.
#' @param age_deployment An optional string specifying the age class to filter the data. Possible values are "A" for adults and "C" for juveniles. Defaults to "A".
#' @param release An optional string specifying the release of the IRMA data to retrieve. Defaults to "20241120".
#' @param version An optional string specifying the version of the IRMA data to retrieve. Defaults to "v3.1".
#' @return A named list to be appended inside the data_request function.
#' @export
#' @concept data_requests
get_irma_from_onedrive <- function(
    start_year = "2000", end_year = format(Sys.Date(), "%Y"), species = NULL, colony = NULL, session_ids = NULL,
    age_deployment = "A", release = "20241120", version = "v3.1") {
    start_date <- as.Date(paste0(start_year, "-01-01"))
    end_date <- as.Date(paste0(end_year, "-12-31")) + 1

    all_species <- cbind.data.frame(
        full_name = c(
            "Little auk", "Atlantic puffin", "Northern fulmar",
            "Black-legged kittiwake", "Common guillemot", "Brünnich's guillemot", "Leach's storm petrel"
        ),
        latin_name = c("Alle_alle", "Fratercula_arctica", "Fulmarus_glacialis", "Rissa_tridactyla", "Uria_aalge", "Uria_lomvia", "Hydrobates_leucorhous"), acronym = c("ALALL", "FRARC", "FUGLA", "RITRI", "URAAL", "URLOM", "HYLEU")
    )



    irma_dir <- file.path(the$sea_track_folder, "Data", "Data products", "IRMA_data")
    all_irma_files <- list.files(path = irma_dir, pattern = paste0(release, "_", gsub(".", "\\.", version, fixed = TRUE), ".*", "\\.rds$"), recursive = TRUE)
    if (length(all_irma_files) == 0) {
        stop(paste0("No IRMA files found for release ", release, " and version ", version, " in OneDrive."))
    }

    db_sessions <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logging_session"))
    db_colony <- dplyr::tbl(con, dbplyr::in_schema("metadata", "colony"))
    db_sessions <- dplyr::left_join(db_sessions, db_colony, by = dplyr::join_by(colony == colony_int_name), suffix = c("", ".colony"))

    db_deployments <- dplyr::tbl(con, dbplyr::in_schema("loggers", "deployment"))

    db_retrievals <- dplyr::tbl(con, dbplyr::in_schema("loggers", "retrieval"))

    db_info <- dplyr::tbl(con, dbplyr::in_schema("individuals", "individ_status"))
    db_deployments <- dplyr::left_join(db_deployments, db_info, dplyr::join_by("session_id", deployment_date == status_date), suffix = c("", ".status"))
    db_deployments <- dplyr::mutate(db_deployments,
        age_deployment = age,
        age2 = if_else(is.na(age), "NA", age),
        age_class_irma = ifelse(tolower(age2) %in% c("pullus", "chick", "pull"), "C", "A")
    )

    db_logger_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
    db_sessions <- dplyr::left_join(db_sessions, db_logger_info, by = "logger_id", suffix = c("", ".logger"))

    db_sessions <- dplyr::left_join(db_sessions, db_deployments, by = "deployment_id", suffix = c("", ".deployment"))
    db_sessions <- dplyr::left_join(db_sessions, db_retrievals, by = "retrieval_id", suffix = c("", ".retrieval"))

    db_sessions <- dplyr::select(db_sessions,
        logger_id = logger_serial_no,
        logger_model,
        year_tracked,
        session_id,
        individ_id,
        deployment_date,
        retrieval_date,
        ring_number,
        country_code = euring_code,
        species,
        colony,
        col_lon = lon,
        col_lat = lat,
        sex,
        age_deployment,
        age_class_irma,
        data_responsible
    )

    if (!is.null(session_ids)) {
        db_sessions <- dplyr::filter(db_sessions, session_id %in% {{ session_ids }})
    }

    if (!is.null(colony)) {
        db_sessions <- dplyr::filter(db_sessions, colony %in% {{ colony }})
    }

    if (!is.null(age_deployment)) {
        db_sessions <- dplyr::filter(db_sessions, age_class_irma %in% {{ age_deployment }})
    }

    if (!is.null(species)) {
        db_sessions <- dplyr::filter(db_sessions, species %in% {{ species }})
    }

    target_species <- all_species[tolower(all_species$full_name) %in% tolower(dplyr::pull(db_sessions, species)), ]

    all_irma_data <- list()
    for (i in seq_len(nrow(target_species))) {
        current_species <- target_species[i, ]
        irma_file_path <- all_irma_files[grep(current_species$acronym, all_irma_files)]
        irma_file <- readRDS(file.path(irma_dir, irma_file_path))
        irma_file <- dplyr::mutate(irma_file, individ_year_tracked = case_when(
            lubridate::month(timestamp) %in% c(6:12) ~ paste(individ_id, lubridate::year(timestamp), formatC(as.numeric(substr(lubridate::year(timestamp), 3, 4)) + 1, flag = "0", width = 2), sep = "_"),
            lubridate::month(timestamp) %in% c(1:5) ~ paste(individ_id, lubridate::year(timestamp) - 1, formatC(as.numeric(substr(lubridate::year(timestamp), 3, 4)), flag = "0", width = 2), sep = "_")
        ))
        irma_file <- dplyr::filter(irma_file, as.Date(timestamp) >= start_date & as.Date(timestamp) <= end_date)

        irma_db_info <- dplyr::filter(irma_file, session_id %in% dplyr::pull(db_sessions, session_id)) %>% dplyr::select(
            session_id,
            date_time = timestamp,
            loc_type,
            lon,
            lat
        )

        irma_db_info <- dplyr::inner_join(irma_db_info, db_sessions, by = "session_id", copy = TRUE)
        irma_db_info$id <- seq_len(nrow(irma_db_info))
        irma_db_info <- dplyr::select(
            irma_db_info,
            id,
            date_time,
            logger_id,
            logger_model,
            year_tracked,
            session_id,
            individ_id,
            deployment_date,
            retrieval_date,
            ring_number,
            country_code,
            species,
            colony,
            col_lon,
            col_lat,
            loc_type,
            lon,
            lat,
            sex,
            age_deployment,
            age_class_irma,
            data_responsible
        )
        all_irma_data <- rbind(all_irma_data, irma_db_info)
    }

    return(list(IRMA_positional_data = list(data = all_irma_data, description = glue::glue("IRMA Positional data version: {version}, release: {release}"))))
}

#' Find raw data files for a given session ID and data type
#'
#' This function searches for raw data files associated with specific session IDs and a given data type. It first checks a local directory for the files and, if not found, it can be extended to check an FTP server or other storage locations.
#' @param session_ids A vector of session IDs for which to find raw data files.
#' @return A list containing two elements: `local_files`, a data frame of files found in the local directory with their paths, and `ftp_files`, a data frame of files found on the FTP server (currently empty, to be implemented).
#' @export
#' @concept data_requests
find_raw_data <- function(session_ids) {
    # Get file name
    db_file_archive <- dplyr::tbl(con, dbplyr::in_schema("loggers", "file_archive"))
    session_files <- dplyr::filter(db_file_archive, session_id %in% session_ids) %>% dplyr::collect()

    # Check ALL folder for associated files
    import_directory <- file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL")
    import_dir_files <- list.files(import_directory)
    files_local_bool <- tolower(session_files$filename) %in% tolower(import_dir_files)

    local_files <- session_files[files_local_bool, ]
    if (length(local_files) > 0) {
        local_files$file_path <- file.path(import_directory, local_files$filename)
    }

    missing_files <- session_files[!files_local_bool, ]
    if (nrow(missing_files) > 0) {
        # If nothing found, check FTP server
        archive_files <- seatrackR::listFileArchive()

        missing_files$ftp_available <- tolower(missing_files$filename) %in% tolower(archive_files$filesInArchive$filename)
        ftp_files <- missing_files[missing_files$ftp_available, ]
        unavailable_files <- missing_files[!missing_files$ftp_available, ]
        if (nrow(unavailable_files) > 0) {
            log_warn(paste0("The following raw data files were not found locally or on the FTP server and will be excluded from the data package: ", paste(unavailable_files$filename, collapse = ", ")))
        }
    } else {
        ftp_files <- missing_files
    }
    return(list(local_files = local_files, ftp_files = ftp_files))
}
