main_server <- function(id) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        species_df <- data.frame(
            lat = c(
                "Alle alle", "Fratercula arctica", "Fulmarus glacialis",
                "Larus argentatus", "Larus fuscus", "Larus hyperboreus",
                "Phalacrocorax aristotelis", "Rissa tridactyla", "Somateria mollissima",
                "Uria aalge", "Uria lomvia",
                "Alca torda", "Stercorarius skua", "Morus bassanus", "Hydrobates leucorhous", "Sterna paradisaea"
            ),
            eng = c(
                "Little auk", "Atlantic puffin", "Northern fulmar",
                "Herring gull", "Lesser black-backed gull", "Glaucous gull",
                "European shag", "Black-legged kittiwake", "Common eider",
                "Common guillemot", "Brünnich's guillemot",
                "Razorbill", "Great skua", "Northern gannet", "Leach's storm petrel", "Arctic tern"
            ),
            abrv = c(
                "LIAU", "ATPU", "NOFU",
                "HEGU", "LBBG", "GLGU",
                "EUSH", "BLKW", "COEI",
                "COGU", "BRGU",
                "RABI", "GRSK", "NOGA", "LSP", "ARTE"
            )
        )

        species_df <- species_df[order(species_df$eng), ]

        colonies <- readr::read_csv("data/SEATRACK database colonies.csv", show_col_types = FALSE)
        colony_cols <- colonies$col
        names(colony_cols) <- colonies$location_name

        ind <- readr::read_csv("data/SEATRACK database individual sexing data.csv", show_col_types = FALSE)

        log <- readr::read_csv("data/SEATRACK database individual event data.csv", show_col_types = FALSE)
        log$year <- factor(log$status_year, 2006:max(log$status_year))

        pos <- readr::read_csv("data/SEATRACK database positional data available.csv", show_col_types = FALSE)
        pos_data_last_modification <- file.info("data/SEATRACK database positional data available.csv")$mtime


        data_req_summary <- dat_req_server("data_request", species_df)
        gls_summary <- pos_summary_server("gls_summary", pos, pos_data_last_modification, log, species_df, "GLS")
        gps_summary <- pos_summary_server("gps_summary", pos, pos_data_last_modification, log, species_df, "GPS")
        irma_summary <- pos_summary_server("irma_summary", pos, pos_data_last_modification, log, species_df, "IRMA")
        age_summary <- age_summary_server("age_summary", ind, pos_data_last_modification, log, species_df, colony_cols)
        sex_summary <- sex_summary_server("sex_summary", ind, pos_data_last_modification, log, species_df, colony_cols)
    })
}
