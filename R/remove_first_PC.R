#' Remove first PC
#'
#' Remove first principal component from a dataset X.
#' This means:
#' 1. Do a PCA
#' 2. Apply the PCA on the data --> some value for the first PC for each sample
#' 3. Do this function, receive X'.
#' 4. Apply PCA from 1., then all values of the first principal component must be approx. zero.
#'
#' @param X
#' Rows are features
#' Columns are samples
#'
#' @return
#' A matrix with same dimension as X but where each sample got the first principal component of a pca on all data removed.
#' @export
#'
#' @examples
#' data("mtcars")
#' tmp <- t(mtcars[, c(1:7, 10, 11)])
#' remove_first_PC(tmp)
remove_first_PC <- function(X) {
    mt.pca <- prcomp(t(X), scale = TRUE, center = TRUE)
    R <- mt.pca$rotation
    A <- t(mt.pca$rotation) %*% X # =applied

    # X' = X - R_{.1} * A_{1.}
    X_without_PC1 <- X - R[, 1, drop = FALSE] %*% A[1, , drop = FALSE]
    # check:
    if (mean(t(R[, 1, drop = FALSE]) %*% X_without_PC1) > 1e-5) { # should be approx zero
        stop("Something went wrong, the first principal component applied should be approx. zero")
    }
    return(X_without_PC1)
}
