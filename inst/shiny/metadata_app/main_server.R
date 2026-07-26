main_server <- function(id) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        default_log_line_n <- 5
        default_log_interval_millis <- 1000

        busy <- reactiveVal(FALSE)
        unsaved <- reactiveVal(FALSE)
        # Can load from settings
        app_settings_list <- app_load_settings()

        app_settings <- reactiveVal(app_settings_list)

        loaded_user_name <- ifelse(!is.null(app_settings_list$user_full_name), app_settings_list$user_full_name, "")
        user_full_name <- reactiveVal(loaded_user_name)
        updateTextInput(session, "user_full_name", value = loaded_user_name)


        observeEvent(input$user_full_name, {
            user_full_name(input$user_full_name)
            app_settings_list <- app_settings()
            app_settings_list$user_full_name <- user_full_name()
            app_settings(app_settings_list)
        })

        all_locations <- reactiveVal(list())
        log_line_n <- reactiveVal(default_log_line_n)
        log_interval_millis <- reactiveVal(default_log_interval_millis)
        # Logger module
        logger_server <- logger_server("logger", log_line_n, log_interval_millis)
        logger_is_fullscreen <- reactive({
            input$logger_card_full_screen
        })

        observeEvent(logger_is_fullscreen(), {
            if (logger_is_fullscreen()) {
                log_line_n(1000)
                log_interval_millis(10000)
            } else {
                log_line_n(default_log_line_n)
                log_interval_millis(default_log_interval_millis)
            }
        })

        settings_init <- reactiveVal(TRUE)
        observeEvent(app_settings(), {
            if (!settings_init()) {
                app_save_settings(app_settings())
            } else {
                settings_init(FALSE)
            }
        })

        manager_loggers <- manage_logger_server("manage_loggers", busy, all_locations, unsaved, user_full_name)
        manage_metadata <- manage_metadata_server("manage_metadata", busy, all_locations, unsaved)
        manage_db_upload <- manage_db_upload_server("manage_db_upload", busy, all_locations, unsaved)
        connect_db <- connect_db_server("connect_db", busy, getShinyOption("test", FALSE),
            on_busy = function(is_busy) {
                if (is_busy) {
                    shinyjs::disable("login")
                } else {
                    shinyjs::enable("login")
                }
            },
            on_success = function() {
                log_success("Succesfully connected")
            }, on_fail = function(e) {
                log_error(paste("ERROR", e), namespace = "error")
            }, app_settings
        )
        # sea_track_path <- reactiveVal()
        # Folder selector
        folder_select <- folder_selector_server("folder", busy, all_locations, app_settings)


        observeEvent(busy(), {
            if (busy()) {
                show_spinner(spin_id = ns("main_spinner"), session = session)
                shinyjs::disable("user_full_name")
            } else {
                shinyjs::enable("user_full_name")
                hide_spinner(spin_id = ns("main_spinner"), session = session)
            }
        })

        observeEvent(all_locations(), {
            n_loc <- length(all_locations())
            output$location_label <- renderUI(p(
                paste(n_loc, "locations loaded")
            ))
        })



        observeEvent(unsaved(), {
            if (unsaved()) {
                output$data_saved_label <- renderUI(p(
                    "Unsaved changes!"
                ))
                shinyjs::enable("save_all_btn")
            } else {
                output$data_saved_label <- renderUI(p(
                    "No unsaved changes"
                ))
                shinyjs::disable("save_all_btn")
            }
        })

        observeEvent(input$save_all_btn, {
            busy(TRUE)
            locations <- all_locations()
            unique_sheets_only <- list()
            seen_paths <- c()
            for (i in seq_along(locations)) {
                x <- locations[[i]]
                if (x$modified && !x$path %in% seen_paths) {
                    unique_sheets_only <- c(unique_sheets_only, x)
                    seen_paths <- c(seen_paths, x$path)
                }
            }
            unsaved_locations <- FALSE
            for (i in seq_along(unique_sheets_only)) {
                x <- unique_sheets_only[[i]]
                new_x <- tryCatch(
                    {
                        save_master_sheet(x, modified_only = TRUE)
                    },
                    ERROR = function(e) {
                        log_error(paste0("Error saving master import sheet: ", basename(x$path), " - ", e$message))
                        return(NULL)
                    }
                )
                if (!is.null(new_x)) {
                    unique_sheets_only[[i]] <- new_x
                    locations <- modify_master_import_in_list(locations, new_x)
                } else {
                    unsaved_locations <- TRUE
                }
            }
            all_locations(locations)
            unsaved(unsaved_locations)
            busy(FALSE)
        })
    })
}

app_load_settings <- function() {
    settings_path <- getShinyOption("settings_path")
    full_settings_path <- file.path(settings_path, "settings")
    log_info("Load settings from: ", full_settings_path)
    if (!file.exists(full_settings_path)) {
        log_info("No settings file found, using defaults")
        return(list())
    }
    temp_env <- new.env()
    load(full_settings_path, envir = temp_env)
    if (!exists("app_settings", envir = temp_env) || class(temp_env$app_settings) != "list") {
        log_info("No app_settings object found in settings file, using defaults")
        return(list())
    }

    return(temp_env$app_settings)
}

app_save_settings <- function(app_settings) {
    settings_path <- getShinyOption("settings_path")
    full_settings_path <- file.path(settings_path, "settings")
    log_info("Saving settings to: ", full_settings_path)
    save(app_settings, file = full_settings_path)
}
