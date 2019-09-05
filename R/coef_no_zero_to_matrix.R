#' coef_no_zero result to matrix
#'
#' @param coefs
#' some coefficients
#'
#' @return
#' A matrix with non-zero coefficients
coef_no_zero_to_matrix <- function(coefs){
	tmp.coef <- as.matrix(coefs)
	tmp.coef <- tmp.coef[apply(tmp.coef, 1, function(x)any(x != 0)), , drop=FALSE]
	tmp.coef
}
