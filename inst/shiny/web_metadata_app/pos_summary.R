pos_summary_ui <- function(id) {
    ns <- NS(id)
    fluidRow(
        shinydashboard::box(
            uiOutput(ns("title")),
            width = 12
        ),
        shinydashboard::box(
            uiOutput(ns("last_updated")),
            width = 12
        ),
        shinydashboard::box(
            selectInput(inputId = ns("age_overall"), label = "Choose an age group:", choices = c("all", "adults", "juveniles"), selected = "all"),
            plotOutput(ns("plot_overall"), height = 200),
            width = 12
        ),
        shinydashboard::box(
            p(" "),
            uiOutput(ns("instruction")),
            width = 12
        ),
        shinydashboard::box(
            selectInput(inputId = ns("species_logger"), label = "Choose a species:", choices = c("all species"), selected = "all species"),
            selectInput(inputId = ns("age_logger"), label = "Choose an age group:", choices = c("all", "adults", "juveniles"), selected = "all"),
            plotOutput(ns("plot_by_species"), height = 520),
            width = 12
        )
    )
}

pos_summary_server <- function(id, pos, pos_data_last_modification, log, species_df, logger_type) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        # Set inputs
        updateSelectInput(inputId = "species_logger", choices = c("all species", species_df$eng))

        output$last_updated <- renderUI(p(HTML(paste("Last updated:", as.Date(pos_data_last_modification)))))

        title_string <- ""
        instruction_string <- ""
        if (logger_type == "GLS") {
            title_string <- "Amount of light-level geolocators deployed and retrieved each year on all SEATRACK species (including historical data)."
            instruction_string <- "Below, you can select a species to investigate how much geolocator tracking data (as # of tracks in database/year) is available at each location and year."
        } else if (logger_type == "GPS") {
            title_string <- "Amount of GPS logger deployed and retrieved each year on all SEATRACK species"
            instruction_string <- "Below, you can select a species to investigate how much GPS logger tracking data (as # of tracks in database/year) is available at each location and year."
        }else if (logger_type == "IRMA") {
            title_string <- "Amount of light-level geolocator tracks augmented with IRMA (Informed Random Movement Algorithm)"
            instruction_string <- "Below, you can select a species to investigate how much geolocator tracking data is augmented with IRMA (as # of tracks in database/year) is available at each location and year."
        }

        output$title <- renderUI(p(title_string))

        output$instruction <- renderUI(p(instruction_string))

        output$plot_overall <- renderPlot({
            if(logger_type == "IRMA"){
                x <- log[log$session_id %in% pos$session_id[pos$data == logger_type], ]
            }else{
                x <- log[log$data == logger_type, ]
            }
            
            if (input$age_overall == "adults") x <- x[x$status_age == "adult", ]
            if (input$age_overall == "juveniles") x <- x[x$status_age == "juvenile", ]


            dat <- rbind(
                data.frame(table(x$year[x$eventType == "Deployment"]), type = "deployed"),
                data.frame(table(x$year[x$eventType == "Retrieval"]), type = "retrieved")
            )
            dat$year <- as.numeric(dat$Var1) + 2005
            dat$cumsum <- c(cumsum(dat$Freq[dat$type == "deployed"]), cumsum(dat$Freq[dat$type == "retrieved"]))


            ggplot(data = dat, aes(x = year, y = Freq, fill = type)) +
                geom_bar(stat = "identity", position = "dodge") +
                theme_minimal() +
                ylab("# devices") +
                xlab(" ") +
                theme(
                    legend.position = "bottom",
                    legend.title = element_blank(), text = element_text(size = 20)
                ) # +
        })

        output$plot_by_species <- renderPlot({
            tryCatch(
                {
                    if (input$species_logger == "all species") {
                        x <- pos[pos$data == logger_type, ]
                    } else {
                        x <- pos[pos$data == logger_type & pos$species == input$species_logger, ]
                    }

                    if (input$age_logger == "adults") x <- x[x$status_age == "adult", ]
                    if (input$age_logger == "juveniles") x <- x[x$status_age == "juvenile", ]

                    # Create the data frame based on selected filtering
                    dat <- data.frame(table(
                        x[[if (input$species_logger == "all species") "species" else "colony"]],
                        x$winter
                    ))
                    colnames(dat) <- c(if (input$species_logger == "all species") "Species" else "Location", "Winter", "Amount")
                    dat$Amount[dat$Amount == 0] <- NA

                    # If there's no valid data, skip plot rendering
                    if (all(is.na(dat$Amount))) {
                        stop("No data available")
                    }

                    # Plot the data using ggplot2
                    ggplot(dat, aes(x = Winter, y = dat[[1]], size = Amount, colour = Amount)) +
                        geom_point() +
                        scale_colour_viridis_c() +
                        scale_size(guide = FALSE) +
                        ylab(NULL) +
                        theme(
                            axis.line = element_blank(),
                            axis.text.x = element_text(size = 11, margin = margin(b = 10), colour = "black", angle = 90, vjust = 0.5),
                            axis.text.y = element_text(size = 13, margin = margin(l = 10), colour = "black"),
                            axis.ticks = element_blank(),
                            axis.title = element_text(size = 18),
                            panel.grid.major = element_line(colour = grey(0.95)),
                            panel.background = element_blank(),
                            legend.text = element_text(size = 10),
                            legend.title = element_text(size = 12)
                        )
                },
                error = function(e) {
                    # Handle the error and show "No data available"
                    plot.new() # Create an empty plot
                    text(0.5, 0.5, "No data available", cex = 1.5) # Add the custom text
                }
            )
        })
    })
}
