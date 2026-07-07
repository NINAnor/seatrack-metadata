#' Run the Shiny app for visualising SEATRACK metadata.
#'
#' This function launches the SEATRACK web metadata app locally.
#' @return None. This function launches the Shiny app and does not return a value
#' @export
#' @concept web_metadata_app
run_web_metadata_app <- function() {
    app_dir <- system.file("shiny/web_metadata_app", package = "seatrackRtools")
    if (app_dir == "") stop("Could not find Shiny app directory.", call. = FALSE)
    shiny::runApp(app_dir)
}

#' Get and prepare data for the SEATRACK metadata Shiny app.
#'
#' This function retrieves and processes data from the SEATRACK database to prepare it for use in the Shiny app that visualizes metadata. It gathers information on individuals, logger deployments, and positional data, and then saves this processed data as CSV files within the package for use in the app.
#' @return None. This function processes data and saves it as CSV files for use in the Shiny app, but does not return a value.
#' @export
#' @concept web_metadata_app
get_web_metadata_data <- function() {
    SEATRACK_species <- c(
        "Herring gull", "Lesser black-backed gull", "Glaucous gull",
        "Northern fulmar", "Northern gannet", "Black-legged kittiwake", "Great skua", "Arctic tern", "Leach's storm petrel",
        "Razorbill", "Atlantic puffin", "Common guillemot", "Brünnich's guillemot", "Little auk",
        "European shag", "Common eider"
    )
    latin_species <- c(
        "Larus argentatus", "Larus fuscus", "Larus hyperboreus",
        "Fulmarus glacialis", "Morus bassanus", "Rissa_tridactyla", "Stercorarius_skua", "Sterna paradisaea", "Hydrobates_leucorhous",
        "Alca_torda", "Fratercula_arctica", "Uria aalge", "Uria lomvia", "Alle alle",
        "Gulosus aristotelis", "Somateria mollissima"
    )
    ind <- seatrackR::getIndividInfo(species = SEATRACK_species, age_at_deployment = NULL)

    log <- seatrackR::getLoggerInfo(species = SEATRACK_species)
    GPS_sessions <- log$session_id[grepl("NanoFix|picofx|gps|ornitrack", log$logger_model, ignore.case = TRUE)]

    # available individual data -----------------------------------------------
    # all sessions where birds were tagged as pullus
    pul <- ind[ind$status_age %in% c("pullus", "chick", "juvenile", "1") & ind$eventType %in% "Deployment", ]

    ind_sub <- ind[!is.na(ind$eventType), ]
    ind_sub$status_year <- as.numeric(strftime(ind_sub$status_date, "%Y"))

    ind_sub$known_age <- "yes"
    # ind_sub$known_age[ind_sub$status_age %in% c(1:100, "chick","pullus","5y","7y")] <- "yes"
    ind_sub$known_age[ind_sub$status_age %in% c("adult_unknown", "adut_unknown", "ADULT_UNKNOWN", "adult unknown", "adult")] <- "no"

    ind_sub$status_age <- "adult"
    ind_sub$status_age[ind_sub$session_id %in% pul$session_id] <- "juvenile"

    ind_sub$data <- "GLS"
    ind_sub$data[ind_sub$session_id %in% GPS_sessions] <- "GPS"

    readr::write_excel_csv(
        ind_sub[, c("species", "colony", "ring_number", "eventType", "status_year", "status_age", "known_age", "status_sex", "year_tracked", "data", "session_id")],
        file.path(system.file("shiny/web_metadata_app/data", package = "seatrackRtools"), "SEATRACK database individual event data.csv")
    )

    # available sexing data ---------------------------------------------------
    sex <- ind_sub
    sex <- sex[sex$eventType == "Retrieval", ]
    sex$status_sexing_method[sex$status_sex %in% "unknown"] <- sex$latest_sexing_method[sex$status_sex %in% "unknown"]
    sex$status_sex[sex$status_sex %in% "unknown"] <- sex$latest_sex[sex$status_sex %in% "unknown"]
    sex <- sex[!duplicated(sex$ring_number), ]

    readr::write_excel_csv(
        sex[, c(
            "species", "colony", "status_sex", "status_sexing_method",
            "status_age", "known_age", "ring_number", "latest_info_date", "session_id"
        )],
        file.path(system.file("shiny/web_metadata_app/data", package = "seatrackRtools"), "SEATRACK database individual sexing data.csv")
    )

    # colonies ----------------------------------------------------------------
    colonies <- seatrackR::getColonies(allLocations = TRUE, loadGeometries = TRUE)
    colonies <- colonies[!sf::st_is_empty(colonies), , drop = FALSE]

    colonies <- dplyr::inner_join(colonies, distinct(dplyr::select(ind, colony, species)), by = dplyr::join_by("location_name" == "colony"), relationship = "many-to-many")

    # Get country - should save to db
    proj.aezd <- paste0(
        "+proj=aeqd  +lat_0=", mean(range(colonies$lat)), "  +lon_0=",
        mean(range(colonies$lon)), " +units=km"
    )

    sf::st_crs(colonies) <- 4326
    colonies <- sf::st_transform(colonies, proj.aezd)

    land <- rnaturalearth::ne_countries(scale = 50)
    land <- sf::st_transform(land, proj.aezd)

    unique_colonies <- colonies[!duplicated(colonies$location_name), ]
    closest <- list()
    for (i in seq_len(nrow(unique_colonies))) {
        closest[[i]] <- land$sovereignt[which.min(sf::st_distance(land, unique_colonies[i, ]))]
    }
    unique_colonies$country <- unlist(closest)
    unique_colonies <- unique_colonies[order(unique_colonies$lon, decreasing = T), ]
    unique_colonies$country <- factor(unique_colonies$country, levels = unique(unique_colonies$country))

    cc <- data.frame(table(unique_colonies$country))
    cc$col_range <- c("Greys", "Purples", "PuBu", "Greys", "Blues", "BuGn", "PuRd", "Reds", "OrRd", "Oranges", "YlOrBr", "Greens")[1:nrow(cc)]
    for (i in 1:nrow(cc)) {
        cc2 <- colorRampPalette(c(RColorBrewer::brewer.pal(9, cc$col_range[i])))(cc$Freq[i])
        if (i == 1) colony_cols <- cc2 else colony_cols <- c(colony_cols, cc2)
    }

    colony_cols_names <- unique_colonies[order(unique_colonies$lat), ]
    colony_cols_names <- colony_cols_names[order(colony_cols_names$country), ]
    colony_cols_names <- unique(colony_cols_names$location_name)
    names(colony_cols) <- colony_cols_names


    unique_colonies$colony_no <- seq_along(unique_colonies$location_name)
    unique_colonies$col <- colony_cols[match(unique_colonies$location_name, names(colony_cols))]
    unique_colonies <- dplyr::select(as.data.frame(unique_colonies), -geometry)
    readr::write_excel_csv(unique_colonies, file.path(system.file("shiny/web_metadata_app/data", package = "seatrackRtools"), "SEATRACK database colonies.csv"))

    for (sp in SEATRACK_species) {
        log_info(sp)
        pos <- seatrackR::getPositions(species = sp, datatype = "GLS")
        pos$year <- as.numeric(substr(pos$date_time, 1, 4))
        pos$month <- as.numeric(substr(pos$date_time, 6, 7))
        pos <- pos[pos$month %in% c(1:5, 9:12), ]
        pos$winter <- paste(substr(pos$year, 3, 4),
            substr(pos$year + 1, 3, 4),
            sep = "/"
        )
        pos$winter[pos$month < 9] <- paste(substr(pos$year[pos$month < 9] - 1, 3, 4),
            substr(pos$year[pos$month < 9], 3, 4),
            sep = "/"
        )

        sub_pos <- pos[!duplicated(paste(pos$session_id, pos$year, pos$month)), ]

        sub_pos$data <- "GLS"
        sub_pos$data[sub_pos$session_id %in% GPS_sessions] <- "GPS"

        # sub_pos$id <- paste(sub_pos$individ_id, sub_pos$logger_id)
        sub_pos <- merge(sub_pos, ind_sub[ind_sub$eventType == "Deployment", c("session_id", "status_age")], by = "session_id", all.x = TRUE)
        sub_pos <- sub_pos[!duplicated(paste(sub_pos$session_id, sub_pos$winter)), c("colony", "winter", "species", "status_age", "data", "session_id")]

        if (sp == SEATRACK_species[1]) pos_sum <- sub_pos else pos_sum <- rbind(pos_sum, sub_pos)
    }

    # IRMA summary
    irma_species <- data.frame(
        full_name = c(
            "Little auk", "Atlantic puffin", "Northern fulmar",
            "Black-legged kittiwake", "Common guillemot", "Brünnich's guillemot", "Leach's storm petrel"
        ),
        latin_name = c("Alle_alle", "Fratercula_arctica", "Fulmarus_glacialis", "Rissa_tridactyla", "Uria_aalge", "Uria_lomvia", "Hydrobates_leucorhous"),
        acronym = c("ALALL", "FRARC", "FUGLA", "RITRI", "URAAL", "URLOM", "HYLEU")
    )
    irma_release <- "20241120"
    irma_version <- "v3.1"

    irma_dir <- file.path(the$sea_track_folder, "Data", "Data products", "IRMA_data")
    all_irma_files <- list.files(path = irma_dir, pattern = paste0(irma_release, "_", gsub(".", "\\.", irma_version, fixed = TRUE), ".*", "\\.rds$"), recursive = TRUE)

    for (i in seq_len(nrow(irma_species))) {
        current_species <- irma_species[i, ]
        log_info("IRMA ", current_species$acronym)
        irma_file_path <- all_irma_files[grep(current_species$acronym, all_irma_files)]
        irma_file <- readRDS(file.path(irma_dir, irma_file_path))
        irma_file <- dplyr::mutate(irma_file, individ_year_tracked = case_when(
            lubridate::month(timestamp) %in% c(6:12) ~ paste(individ_id, lubridate::year(timestamp), formatC(as.numeric(substr(lubridate::year(timestamp), 3, 4)) + 1, flag = "0", width = 2), sep = "_"),
            lubridate::month(timestamp) %in% c(1:5) ~ paste(individ_id, lubridate::year(timestamp) - 1, formatC(as.numeric(substr(lubridate::year(timestamp), 3, 4)), flag = "0", width = 2), sep = "_")
        ))

        pos <- seatrackR::getPositions(sessionId = unique(irma_file$session_id), datatype = "GLS")
        pos$year <- as.numeric(substr(pos$date_time, 1, 4))
        pos$month <- as.numeric(substr(pos$date_time, 6, 7))
        pos <- pos[pos$month %in% c(1:5, 9:12), ]
        pos$winter <- paste(substr(pos$year, 3, 4),
            substr(pos$year + 1, 3, 4),
            sep = "/"
        )
        pos$winter[pos$month < 9] <- paste(substr(pos$year[pos$month < 9] - 1, 3, 4),
            substr(pos$year[pos$month < 9], 3, 4),
            sep = "/"
        )

        sub_pos <- pos[!duplicated(paste(pos$session_id, pos$year, pos$month)), ]

        sub_pos$data <- "IRMA"

        # sub_pos$id <- paste(sub_pos$individ_id, sub_pos$logger_id)
        sub_pos <- merge(sub_pos, ind_sub[ind_sub$eventType == "Deployment", c("session_id", "status_age")], by = "session_id", all.x = TRUE)
        sub_pos <- sub_pos[!duplicated(paste(sub_pos$session_id, sub_pos$winter)), c("colony", "winter", "species", "status_age", "data", "session_id")]
        pos_sum <- rbind(pos_sum, sub_pos)
    }

    readr::write_excel_csv(pos_sum, file.path(system.file("shiny/web_metadata_app/data", package = "seatrackRtools"),
        "SEATRACK database positional data available.csv",
        sep = ""
    ))

    # Copy data request sheet from one drive

    request_folder <- file.path(the$sea_track_folder, "Admin\\08_Data requests and AoU")
    request_summary <- "SEATRACK data_request_summary.csv"

    file.copy(
        from = file.path(request_folder, request_summary),
        to = file.path(system.file("shiny/web_metadata_app/data", package = "seatrackRtools"), request_summary),
        overwrite = TRUE,
        copy.date = TRUE
    )
}

set_shiny_credentials_Renv <- function(account = NULL, token = NULL, secret = NULL) {
    if (is.null(account)) {
        account <- readline("Enter shinyapps.io account name:")
    }

    if (is.null(token)) {
        token <- getPass::getPass(msg = "Enter shinyapps.io token (from dashboard):")
    }

    if (is.null(secret)) {
        secret <- getPass::getPass(msg = "Enter shinyapps.io secret (from dashboard):")
    }

    if (file.exists(".Renviron")) {
        environ_lines <- readLines(".Renviron")
    } else {
        environ_lines <- c()
    }

    if (Sys.getenv("SHINYAPPS_ACCOUNT", "") != account ||
        Sys.getenv("SHINYAPPS_TOKEN", "") != token || Sys.getenv("SHINYAPPS_SECRET", "") != secret) {
        environ_lines <- environ_lines[!grepl("SHINYAPPS_ACCOUNT", environ_lines, fixed = TRUE)]
        environ_lines <- environ_lines[!grepl("SHINYAPPS_TOKEN", environ_lines, fixed = TRUE)]
        environ_lines <- environ_lines[!grepl("SHINYAPPS_SECRET", environ_lines, fixed = TRUE)]
        environ_lines <- c(environ_lines, paste0("SHINYAPPS_ACCOUNT = '", account, "'"), paste0("SHINYAPPS_TOKEN = '", token, "'"), paste0("SHINYAPPS_SECRET = '", secret, "'"))
        writeLines(unique(environ_lines), ".Renviron")
        print("Wrote credentials to .Renviron")

        # immediately set the env var so R does not have to be restarted
        Sys.setenv(SHINYAPPS_ACCOUNT = account)
        Sys.setenv(SHINYAPPS_TOKEN = token)
        Sys.setenv(SHINYAPPS_SECRET = secret)
    }

    if (file.exists(".gitignore")) {
        git_ignore_lines <- readLines(".gitignore")
    } else {
        if (git2r::in_repository(".")) {
            # add git ignore file
            git_ignore_lines <- c()
        } else {
            # Exit
            return()
        }
    }
    if (!".Renviron" %in% git_ignore_lines) {
        log_info("Adding .Renviron to .gitignore")
        git_ignore_lines <- c(git_ignore_lines, ".Renviron")
        writeLines(unique(git_ignore_lines), ".gitignore")
    }
}

#' Get and prepare data for the SEATRACK metadata Shiny app.
#'
#' Convenience function calling `get_web_metadata_data` to get the latest data from the database and then pushing it to shinyapps.io.
#' Note that the default is to upload to the test app. Set test = FALSE to update the production app.
#' @param test A boolean indicating whether to deploy the app in test mode. Defaults to TRUE, which deploys the app under the name "seatrack_shinyapp_TEST". If FALSE, the app will be deployed under the name "seatrack_shinyapp".
#' @param account A string specifying the shinyapps.io account name. If NULL, it will attempt to retrieve the account name from the SHINYAPPS_ACCOUNT environment variable or prompt the user for input.
#' @param token A string specifying the shinyapps.io token. If NULL, it will attempt to retrieve the token from the SHINYAPPS_TOKEN environment variable or prompt the user for input.
#' @param secret A string specifying the shinyapps.io secret. If NULL, it will attempt to retrieve the secret from the SHINYAPPS_SECRET environment variable or prompt the user for input.
#' @return None. This function processes data, saves it as CSV files for use in the Shiny app, and deploys the app to shinyapps.io, but does not return a value.
#' @export
#' @concept web_metadata_app
build_and_deploy_metada_app <- function(test = TRUE, account = NULL, token = NULL, secret = NULL) {
    get_web_metadata_data()
    deploy_web_metadata_app(test = test, account = account, token = token, secret = secret)
}

#' Deploy the SEATRACK metadata Shiny app to shinyapps.io.
#'
#' This function deploys the SEATRACK metadata Shiny app to the shinyapps.io platform. It sets up the necessary credentials for deployment, checks for the presence of required data, and then uses the rsconnect package to deploy the app under a specified account and app name.
#' @param test A boolean indicating whether to deploy the app in test mode. Defaults to TRUE, which deploys the app under the name "seatrack_shinyapp_TEST". If FALSE, the app will be deployed under the name "seatrack_shinyapp".
#' @param account A string specifying the shinyapps.io account name. If NULL, it will attempt to retrieve the account name from the SHINYAPPS_ACCOUNT environment variable or prompt the user for input.
#' @param token A string specifying the shinyapps.io token. If NULL, it will attempt to retrieve the token from the SHINYAPPS_TOKEN environment variable or prompt the user for input.
#' @param secret A string specifying the shinyapps.io secret. If NULL, it will attempt to retrieve the secret from the SHINYAPPS_SECRET environment variable or prompt the user for input.
#' @return None. This function deploys the Shiny app to shinyapps.io and does not return a value.
#' @export
#' @concept web_metadata_app
deploy_web_metadata_app <- function(test = TRUE, account = NULL, token = NULL, secret = NULL) {
    if (is.null(account)) {
        account <- Sys.getenv("SHINYAPPS_ACCOUNT", NA)
        if (is.na(account)) {
            account <- NULL
        }
    }
    if (is.null(token)) {
        token <- Sys.getenv("SHINYAPPS_TOKEN", NA)
        if (is.na(token)) {
            token <- NULL
        }
    }
    if (is.null(secret)) {
        secret <- Sys.getenv("SHINYAPPS_SECRET", NA)
        if (is.na(secret)) {
            secret <- NULL
        }
    }

    set_shiny_credentials_Renv(account, token, secret)

    if (length(dir(file.path(system.file("shiny/web_metadata_app/data", package = "seatrackRtools")))) == 0) {
        stop("No data for shiny app. Run get_web_metadata_data() first.")
    }

    rsconnect::setAccountInfo(
        name   = account,
        token  = token,
        secret = secret
    )

    app_name <- ifelse(test, "seatrack_shinyapp_TEST", "seatrack_shinyapp")

    rsconnect::deployApp(
        appDir      = system.file("shiny/web_metadata_app", package = "seatrackRtools"),
        appName     = app_name,
        account     = account,
        server      = "shinyapps.io",
        forceUpdate = TRUE # Forces overwrite without prompting "Y/N"
    )
}
