#' Get lambdaindex
#'
#' Get the index of the lambda(=s) with a specific name (lambda.1se or lambda.min) or a specific value.
#'
#' @param fit
#' A cv-model fit
#' @param s
#' lambda-value, usually "lambda.1se" or "lambda.min"
#' @param fit.lambda.name
#' The name of the model's lambda-sequence
#' @param fit.lambda.error.name
#' The name of the model's mean error related to the lambda-sequence
#' @param fit.lambda.errorSD.name
#' The name of the model's mean error standard deviation related to the lambda-sequence
#'
#' @return
#' single numeric - index of
#' @export
#'
#' @examples
#' library(glmnet)
#' set.seed(1010)
#' n=1000;p=100
#' nzc=trunc(p/10)
#' x=matrix(rnorm(n*p),n,p)
#' beta=rnorm(nzc)
#' fx= x[,seq(nzc)] %*% beta
#' eps=rnorm(n)*5
#' y=drop(fx+eps)
#' px=exp(fx)
#' px=px/(1+px)
#' ly=rbinom(n=length(px),prob=px,size=1)
#' set.seed(1011)
#' cvob1=cv.glmnet(x,y)
#' # glmnet
# cv.lambdaIndex(fit = cvob1
# 			   , s="lambda.min"
# 			   ,fit.lambda.name = "lambda"
# 			   ,fit.lambda.error.name = "cvm"
# 			   ,fit.lambda.errorSD.name = "cvsd")
#'
#' # grpregoverlap:
#' if(require(grpregOverlap)){
#' ## linear regression, a simulation demo.
#' set.seed(123)
#' group <- list(gr1 = c(1, 2, 3),
#' 			  gr2 = c(1, 4),
#' 			  gr3 = c(2, 4, 5),
#' 			  gr4 = c(3, 5),
#' 			  gr5 = c(6))
#' beta.latent.T <- c(5, 5, 5, 0, 0, 0, 0, 0, 5, 5, 0) # true latent coefficients.
#' # beta.T <- c(5, 5, 10, 0, 5, 0), true variables: 1, 2, 3, 5; true groups: 1, 4.
#' X <- matrix(rnorm(n = 6*100), ncol = 6)
#' X.latent <- expandX(X, group)
#' y <- X.latent %*% beta.latent.T + rnorm(100)
#'
#' cvfit <- cv.grpregOverlap(X, y, group, penalty = 'grMCP')
#' cv.lambdaIndex(fit = cvfit
#' 			   , s="lambda.min"
#' 			   ,fit.lambda.name = "lambda"
#' 			   ,fit.lambda.error.name = "cve"
#' 			   ,fit.lambda.errorSD.name = "cvse")
#' }



cv.lambdaIndex <- function(fit, s
						   ,fit.lambda.name="lambdas"
						   ,fit.lambda.error.name="lldiff"
						   ,fit.lambda.errorSD.name="llSD"
						   ,...){
	get.lambda.1se <- function(loss.mean, loss.sd, lambdas){
		minimum.index <- which.min(loss.mean)
		min.plus.sd <- loss.mean[minimum.index] + loss.sd[minimum.index]
		# min.lambda <- lambdas[minimum.index]
		# i wnat the maximal lambda which is still just below min.plus.sd
		return(max(lambdas[loss.mean < min.plus.sd]))
	}
	if(s=="lambda.min"){
		lambda.index <- which.min(fit[[fit.lambda.error.name]])
	}else if(s == "lambda.1se"){
		lambda.index <- which(fit[[fit.lambda.name]] == get.lambda.1se(loss.mean = fit[[fit.lambda.error.name]]
																	   ,loss.sd = fit[[fit.lambda.errorSD.name]]
																	   ,lambdas = fit[[fit.lambda.name]]))
	}else{
		lambda.index <- which(fit[[fit.lambda.name]] == s)
		if(length(lambda.index) == 0)
			stop("your lambda is not inside the lambdasequence exactly")
	}
	return(lambda.index)
}

