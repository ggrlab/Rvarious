#' lambda finding
#'
#' If "s" is not the usual lambda-name, I have to search for it in another way.
#' Here's how.
#'
#' @param fit
#' @param s
#' @param fit.lambda.name
#' @param fit.lambda.error.name
#' @param fit.lambda.errorSD.name
#' @param ...
#'
#' @return
#'
#' @examples
unimplemented_s_finding <- function(fit, s="lambda.1se"
									, fit.lambda.name = "lambda"
									, fit.lambda.error.name = "cve"
									, fit.lambda.errorSD.name = "cvse"
									,...){
	cv.lambdaIndex(fit=fit
				   ,s=s
				   ,fit.lambda.name=fit.lambda.name
				   ,fit.lambda.error.name=fit.lambda.error.name
				   ,fit.lambda.errorSD.name=fit.lambda.errorSD.name)
}
