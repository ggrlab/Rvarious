#' coef_no_zero result to matrix
#'
#' @param coefs
#' some coefficients
#'
#' @return
#' A matrix with non-zero coefficients
coef_no_zero_to_matrix <- function(coefs,
                                   do.sort = FALSE,
                                   do.sort.sortfun = function(coefvec) {
                                       order(abs(coefvec), decreasing = TRUE)
                                   },
                                   sort.rownames.exceptions = c("(Intercept)", "intercept")) {
    tmp.coef <- as.matrix(coefs)
    tmp.coef <- tmp.coef[apply(tmp.coef, 1, function(x) any(x != 0)), , drop = FALSE]
    if (do.sort) {
        tmp.coef_noIntercept <- tmp.coef[!rownames(tmp.coef) %in% sort.rownames.exceptions, , drop = FALSE]
        tmp.coef_noIntercept <- tmp.coef_noIntercept[do.sort.sortfun(tmp.coef_noIntercept), , drop = FALSE]
        tmp.coef <- rbind(
            tmp.coef[rownames(tmp.coef) %in% sort.rownames.exceptions, , drop = FALSE],
            tmp.coef_noIntercept
        )
    }
    tmp.coef
}