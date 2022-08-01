write_fcs <- function(cell_matrix, filepath, max_val = 1e7, min_val = -1e3) {
    # n cells = rows
    # p markers = columns

    cell_matrix <- rbind(cell_matrix, max_val, min_val)
    ff <- flowCore::flowFrame(cell_matrix)
    ff <- ff[-nrow(ff), ]
    ff <- ff[-nrow(ff), ]
    flowCore::write.FCS(
        ff,
        filepath
    )
}