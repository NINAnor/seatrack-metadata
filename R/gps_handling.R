push_gps_data <- function() {
    all_gps_data <- seatrackRgps::open_gps_data_files(file.path(the$sea_track_folder, "Database", "Import_Positions_GPS", "raw_data", "ALL"))

    # Some of this functionality could be moved to the library
    multi_tags <- unique(names(all_gps_data)[duplicated(names(all_gps_data))])
    multi_tag_idx <- which(names(all_gps_data) %in% multi_tags)
    n_files <- sapply(multi_tag_idx, function(i) {
        sum(!sapply(all_gps_data[[i]], is.null))
    })
    max_per_tag <- aggregate(n_files ~ names(all_gps_data)[multi_tag_idx], FUN = max)
    names(max_per_tag) <- c("tag_id", "n_files")

    all_gps_data_nd <- all_gps_data

    for (i in seq_along(multi_tag_idx)) {
        tag_idx <- multi_tag_idx[i]
        tag_id <- names(all_gps_data)[tag_idx]
        tag_idx_n_files <- n_files[i]
        tag_id_max <- max_per_tag$n_files[max_per_tag$tag_id == tag_id]
        if (tag_idx_n_files != tag_id_max) {
            all_gps_data_nd[tag_idx] <- NULL
        }
    }
    all_gps_data_nd <- all_gps_data_nd[!sapply(all_gps_data_nd, is.null)]

    # remove remaining duplicates
    all_gps_data_nd <- all_gps_data_nd[which(!duplicated(names(all_gps_data_nd)))]

    all_pos <- do.call(rbind, lapply(all_gps_data_nd, function(x) {
        x$pos_data
    }))

    all_immersion <- do.call(rbind, lapply(all_gps_data_nd, function(x) {
        x$acc_immersion_data
    }))

    all_acc <- do.call(rbind, lapply(all_gps_data_nd, function(x) {
        x$acc_data
    }))


    names(all_pos) <- tolower(names(all_pos))
    write_result <- dbWriteTable(con,
        DBI::Id(schema = "imports", table = "gps_import"),
        all_pos,
        row.names = FALSE,
        append = TRUE
    )

    names(all_immersion) <- tolower(names(all_immersion))
    all_immersion <- dplyr::select(all_immersion, -dplyr::contains("acceleration"))
    write_result <- dbWriteTable(con,
        DBI::Id(schema = "imports", table = "gps_immersion_import"),
        all_immersion,
        row.names = FALSE,
        append = TRUE
    )

    names(all_acc) <- tolower(names(all_acc))
    write_result <- dbWriteTable(con,
        DBI::Id(schema = "imports", table = "gps_acc_import"),
        all_acc,
        row.names = FALSE,
        append = TRUE
    )

    gps_db <- dplyr::tbl(con, dbplyr::in_schema("positions", "gps"))
    # Check which deployment rows did and didn't make it into the database
    temp_pos_table <- dplyr::copy_to(
        con,
        all_pos,
        name = "tmp_pos",
        temporary = TRUE,
        overwrite = TRUE
    )

    gps_db <- dplyr::mutate(gps_db, logger_id_str = as.character(logger_id))

    missing_rows <- dplyr::anti_join(temp_pos_table, gps_db, by = dplyr::join_by(tag_id == logger_id_str, "date_time")) %>%
        dplyr::group_by(tag_id) %>%
        dplyr::summarise(n_mising = n()) %>%
        dplyr::collect()
    if (nrow(missing_rows) > 0) {
        log_warn("Data import for the following logger IDs was incomplete", ":\n", paste(capture.output(print(missing_rows, n = nrow(missing_rows)))[c(-1, -3)], collapse = "\n"))
    }
}


# all_gps_data <- seatrackRgps::open_gps_data_files(the$sea_track_folder)
# all_paths <- sapply(all_gps_data_nd, function(x) {
#     x$base_path
# })

# new_root <- file.path(the$sea_track_folder, "Database\\Import_Positions_GPS\\raw_data\\ALL")

# for (i in seq_along(all_paths)) {
#     tag <- names(all_paths)[i]
#     path <- all_paths[i]
#     if (grepl("*.zip|*.7z", path)) {
#         files_to_move <- path
#     } else {
#         all_files <- list.files(path, recursive = FALSE, full.names = TRUE)

#         files_to_move <- all_files[grepl(paste0("Tag", tag, ".*(AccWetDry.txt|Accel.txt|.pos)$"), all_files)]
#     }

#     new_names <- file.path(new_root, basename(files_to_move))

#     file.copy(files_to_move, new_names, overwrite = FALSE)
# }
