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
coef_no_zero_to_matrix <- function(coefs){
	tmp.coef <- as.matrix(coefs)
	tmp.coef <- tmp.coef[apply(tmp.coef, 1, function(x)any(x != 0)), , drop=FALSE]
	tmp.coef
}

#' Get coefficients without zeros
#'
#' @param x
#' A model from
#'     - cv.glmnet
#'
#' @return
#' @export
#'
#' @examples
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
#'
#' if(require(glmnet)){
#' 		set.seed(1011)
#' 		cvob1=glmnet::cv.glmnet(x,y)
#'
#' 		print(coef_no_zero(cvob1))
#' 		print(coef_no_zero(cvob1, s="lambda.min"))
#' 	}
#' if(require(zeroSum)){
#'		cvob2=zeroSum::zeroSum(x,y)
#'
#' 		print(coef_no_zero(cvob2))
#' 		print(coef_no_zero(cvob2, s="lambda.min"))
#' 	}
#' #'
#' if(require(grpreg)){
#' 	cvob3 = grpreg::cv.grpreg(x,y)
#' 	cvob4 = grpreg::grpreg(x,y)
#'
#' 	print(coef_no_zero(cvob3))
#' 	print(coef_no_zero(cvob3, s="lambda.min"))
#' 	print(coef_no_zero(cvob4))
#'  print(coef_no_zero(cvob4, s="lambda.1se"))
#' }
coef_no_zero <- function(x, ...){
	UseMethod("coef_no_zero", x)
}

#' @export
coef_no_zero.cv.glmnet <- function(x, ...){
	tmp.coef <- glmnet::coef.cv.glmnet(x, ...)
	coef_no_zero_to_matrix(tmp.coef)
}
#' @export
coef_no_zero.zeroSum <- function(x, ...){
	tmp.coef <- zeroSum:::coef.zeroSum(x, ...)
	coef_no_zero_to_matrix(tmp.coef)
}
#' @export
coef_no_zero.grpreg <- function(x, ...){
	if("s"%in% names(list(...))){
		warning("You supplied \"s\" for a non-CV grpreg model. I cannot determine which s you wanted, therefore I give you all.")
	}
	tmp.coef <- grpreg:::coef.grpreg(x, ...)
	coef_no_zero_to_matrix(tmp.coef)
}
#' @export
coef_no_zero.cv.grpreg <- function(x, ...){
	lambda.index <- unimplemented_s_finding(
		fit = x
		,fit.lambda.name = "lambda"
		,fit.lambda.error.name = "cve"
		,fit.lambda.errorSD.name = "cvse"
		,...)
	tmp.coef <- grpreg:::coef.cv.grpreg(x, lambda=x$lambda[[lambda.index]], ...)
	coef_no_zero_to_matrix(tmp.coef)
}
