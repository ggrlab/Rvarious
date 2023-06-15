#' Export a matrix to an .fcs file
#'
#' It adds (and removes) two cells with constant maximum and minimum values to have nice visual
#' visual ranges for Kaluza
#'
#' @param cell_matrix
#' Matrix with cols=Samples and rows=features
#' @param filepath
#' Where the .fcs gets saved
#' @param max_val
#' The fake cell with maximum values which gets added and after flowFrame creation removed
#' @param min_val
#' The fake cell with miminum values which gets added and after flowFrame creation removed
#' @return NULL
#' @export
#'
write_fcs <- function(cell_matrix, filepath, max_val = 1e7, min_val = -1e3) {
    stop("Deprecated")
    cell_matrix <- rbind(cell_matrix, max_val)
    cell_matrix <- rbind(cell_matrix, min_val)
    ff <- flowCore::flowFrame(cell_matrix)
    ff <- ff[-nrow(ff), ]
    ff <- ff[-nrow(ff), ]
    flowCore::write.FCS(
        ff,
        filepath
    )
}