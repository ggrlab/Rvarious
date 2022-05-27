#' Convert a matrix to a vector
#'
#' Convert a named matrix with one column to a named vector where the rownames are the names.
#'
#' @param named_matrix
#' Matrix with one column which gets transformed into a named vector
#'
#' @return
#' named vector
#' @export
#'
#' @examples
#' mymat <- matrix(1:6, nrow = 6)
#' vectorize_named(mymat)
#'
#' rownames(mymat) <- paste0("f_", 1:6)
#' vectorize_named(mymat)
#'
#' mymat <- matrix(1:6, nrow = 3)
#' # This here should fail because there is more than 1 column in the matrix mymat!
#' vectorize_named(mymat)
#'
vectorize_named <- function(named_matrix) {
    if (ncol(named_matrix) > 1) {
        stop("Only a matrix with a single column is allowed")
    }
    new_vec <- named_matrix[, 1]
    names(new_vec) <- rownames(named_matrix)
    return(new_vec)
}