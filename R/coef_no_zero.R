#' Get coefficients without zeros
#'
#' @param x
#' A model from
#'     - cv.glmnet
#'
#' @return
#' A matrix with non-zero coefficients
#' @export
#'
#' @examples
#' # library(Rvarious)
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
#'
#' if(require(grpreg)){
#' 		cvob3 = grpreg::cv.grpreg(x,y)
#' 		cvob4 = grpreg::grpreg(x,y)
#' 		Lung <- grpreg::Lung
#' 		X <- Lung$X
#' 		y <- Lung$y
#' 		group <- Lung$group
#'
#' 		cvob5 <- grpreg::cv.grpsurv(X, y, group)
#'
#' 		print(coef_no_zero(cvob3))
#' 		print(coef_no_zero(cvob3, s="lambda.min"))
#' 		print(coef_no_zero(cvob4))
#' 		print(coef_no_zero(cvob4, s="lambda.1se"))
#' 		print(coef_no_zero(cvob5))
#' 		print(coef_no_zero(cvob5, s="lambda.1se"))
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
	lambda.index <- cv.lambdaIndex(
		fit = x
		,fit.lambda.name = "lambda"
		,fit.lambda.error.name = "cve"
		,fit.lambda.errorSD.name = "cvse"
		,...)
	tmp.coef <- grpreg:::coef.cv.grpreg(x, lambda=x$lambda[[lambda.index]], ...)
	coef_no_zero_to_matrix(tmp.coef)
}
