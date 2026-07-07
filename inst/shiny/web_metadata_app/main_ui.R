main_ui <- function(id) {
    ns <- NS(id)
    shiny::navbarPage(
        title = "Metadata App",
        header = (tags$head(
            tags$style(HTML("
        table.dataTable thead th {
            background-color: darkgrey; /* Header background color */
            border: none;               /* No border for header */
        }
        table.dataTable {
            border: none;               /* No border for the table */
        }
        table.dataTable th,
        table.dataTable td {
            border: none;               /* No borders for all cells */
        }
        "))
        )
        ),
        tabPanel(
            "Data Request archive",
            dat_req_ui(ns("data_request"))
        ),
        tabPanel(
            "GLS summary",
            pos_summary_ui(ns("gls_summary"))
        ),
        tabPanel(
            "GPS summary",
            pos_summary_ui(ns("gps_summary"))
        ),
        tabPanel(
            "IRMA summary",
            pos_summary_ui(ns("irma_summary"))
        ),
        tabPanel(
            "Sex",
            sex_summary_ui(ns("sex_summary"))
        ),
        tabPanel(
            "Age",
            age_summary_ui(ns("age_summary"))
        )
    )
}
