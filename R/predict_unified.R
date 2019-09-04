
#' Get a unified prediction function
#'
#' @param x
#' A model from
#'     - cv.glmnet
#'     - zeroSum
#'     - grpreg
#'
#' @return
#' @export
#'
#' @examples
#' # library(Rvarious)
#' set.seed(1010)
#' n=1000;p=100
#' nzc=trunc(p/10)
#' x=matrix(rnorm(n*p),n,p)
#' rownames(x) <- paste0("f", 1:nrow(x))
#' colnames(x) <- paste0("S", 1:ncol(x))
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
#' 		print(head(predict_unified(cvob1,newx = x,  s="lambda.min")))
#' 		print(head(predict_unified(cvob1,newx = x)))
#' 	}
#' if(require(zeroSum)){
#'		cvob2=zeroSum::zeroSum(x,y)
#'
#' 		print(head(predict_unified(cvob2,newx = x,  s="lambda.min")))
#' 	}
#'
#' if(require(grpreg)){
#' 		cvob3 = grpreg::cv.grpreg(x,y)
#' 		cvob4 = grpreg::grpreg(x,y)
#'
#' 		print(head(predict_unified(cvob3,newx = x,  s="lambda.min")))
#' 		print(head(predict_unified(cvob4,newx = x,  s="lambda.min")))
#'
#' 		Lung <- grpreg::Lung
#' 		x_surv <- Lung$X
#' 		y_surv <- Lung$y
#' 		group <- Lung$group
#'
#' 		cvob5 <- grpreg::cv.grpsurv(x_surv, y_surv, group)
#' 		cvob6 <- grpreg::grpsurv(x_surv, y_surv, group)
#' 		print(head(predict_unified(cvob5,newx = x_surv,  s="lambda.min")))
#' 		print(head(predict_unified(cvob6,newx = x_surv,  s=4)))
#' }
#'
#'


predict_unified <- function(fit, newx, s, ...){
	UseMethod("predict_unified", fit)
}

#' @export
predict_unified.cv.glmnet <- function(fit, newx, s="lambda.1se", ...){
	glmnet::predict.cv.glmnet(
		object = fit
		,newx=newx
		,s=s
		,...)
}
#' @export
predict_unified.zeroSum <- function(fit, newx, s="lambda.1se", ...){
	zeroSum:::predict.zeroSum(
		object = fit
		,newx=newx
		,s=s
		,...)
}
#' @export
predict_unified.grpreg <- function(fit, newx, s="lambda.1se", ...){
	tryCatch({
		s <- as.numeric(s)
	},warning=function(w){
		warning("s should be a lambda-index but was no numeric. Has been set to 1.")
		s <<- 1
	})
	if(floor(s) != s){
		warning("You supplied \"s\" for a non-CV grpreg model. I cannot determine which s you wanted, therefore I interpret s as lambda-index. s was a double, I floor() it. ")
	}
	s <- floor(s)

	s <- fit$lambda[[s]]
	tmp.result <- grpreg:::predict.grpreg(
		object = fit
		, X=newx, lambda = s, ...)
	as.matrix(tmp.result)
}
#' @export
predict_unified.grpsurv <- function(fit, newx, s="lambda.1se", ...){
	tryCatch({
		s <- as.numeric(s)
	},warning=function(w){
		warning("s should be a lambda-index but was no numeric. Has been set to 1.")
		s <<- 1
	})
	if(floor(s) != s){
		warning("You supplied \"s\" for a non-CV grpreg model. I cannot determine which s you wanted, therefore I interpret s as lambda-index. s was a double, I floor() it. ")
	}
	s <- floor(s)

	s <- fit$lambda[[s]]
	tmp.result <- grpreg:::predict.grpsurv(
		object = fit
		, X=newx, lambda = s, ...)
	as.matrix(tmp.result)
}
#' @export
predict_unified.cv.grpreg <- function(fit, newx, s="lambda.1se", ...){
	lambda.index <- unimplemented_s_finding(
		fit = fit, s=s
		,fit.lambda.name = "lambda"
		,fit.lambda.error.name = "cve"
		,fit.lambda.errorSD.name = "cvse"
		,...)
	s <- fit$lambda[[lambda.index]]
	tmp.result <- grpreg:::predict.cv.grpreg(
		object = fit
		, X=newx, lambda = s
		, ...)
	as.matrix(tmp.result)
}
#' @export
predict_unified.cv.grpsurv <- function(fit, newx, s="lambda.1se", ...){
	lambda.index <- unimplemented_s_finding(
		fit = fit, s=s
		,fit.lambda.name = "lambda"
		,fit.lambda.error.name = "cve"
		,fit.lambda.errorSD.name = "cvse"
		,...)
	s <- fit$lambda[[lambda.index]]
	warning(paste0("This is a self-built prediction because grpreg has an error because"
			,"they want the intercept (alpha) in the cox-prediction where no intercept is. "))
	tmp.coef <- grpreg:::predict.cv.grpreg(
		object = fit
		, X=newx, lambda = s, type="coef")
	newx %*% tmp.coef
}
