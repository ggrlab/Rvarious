#' print_as_python
#'
#' Print a 2d matrix python-like
#'
#' @param x
#' The matrix to print
#'
#' @return
#' prints only
#' @export
#'
#' @examples
#' print_as_python(
#'     matrix(c(
#'         -1.2067, 0.6976,
#'         0.1576, 0.6925,
#'         1.0208, -0.6271
#'     ), ncol = 2, byrow = TRUE)
#' )
print_as_python <- function(x) {
    if (length(dim(x)) != 2) {
        stop("Must be 2d matrix")
    }
    cat("[")
    for (i in 1:(nrow(x) - 1)) {
        cat("[", paste0(x[i, ], collapse = ", "), "],\n", sep = "")
    }
    cat("[", paste0(x[nrow(x), ], collapse = ", "), "]]\n", sep = "")
}