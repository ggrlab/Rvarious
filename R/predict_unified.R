
#' Get a unified prediction function
#'
#' @param x
#' Tested: a model from
#'     - cv.glmnet
#'     - zeroSum
#'     - grpreg
#'
#' @return
#' matrix with rows as samples, predictions from the model
#' @export
#'
#' @examples
#' # library(Rvarious)
#' set.seed(1010)
#' n <- 1000
#' p <- 100
#' nzc <- trunc(p / 10)
#' x <- matrix(rnorm(n * p), n, p)
#' rownames(x) <- paste0("S", 1:nrow(x))
#' colnames(x) <- paste0("f", 1:ncol(x))
#' beta <- rnorm(nzc)
#' fx <- x[, seq(nzc)] %*% beta
#' eps <- rnorm(n) * 5
#' y <- drop(fx + eps)
#' px <- exp(fx)
#' px <- px / (1 + px)
#' ly <- rbinom(n = length(px), prob = px, size = 1)
#'
#' if (require(glmnet)) {
#'     set.seed(1011)
#'     cvob1 <- glmnet::cv.glmnet(x, y)
#'
#'     print(head(predict_unified(cvob1, newx = x, s = "lambda.min")))
#'     print(head(predict_unified(cvob1, newx = x)))
#' }
#' if (require(zeroSum)) {
#'     cvob2 <- zeroSum::zeroSum(x, y)
#'
#'     print(head(predict_unified(cvob2, newx = x, s = "lambda.min")))
#' }
#'
#' if (require(grpreg)) {
#'     cvob3 <- grpreg::cv.grpreg(x, y)
#'     cvob4 <- grpreg::grpreg(x, y)
#'
#'     print(head(predict_unified(cvob3, newx = x, s = "lambda.min")))
#'     print(head(predict_unified(cvob4, newx = x, lambda.index = 2)))
#'
#'     Lung <- grpreg::Lung
#'     x_surv <- Lung$X
#'     y_surv <- Lung$y
#'     group <- Lung$group
#'
#'     cvob5 <- grpreg::cv.grpsurv(x_surv, y_surv, group)
#'     cvob6 <- grpreg::grpsurv(x_surv, y_surv, group)
#'     print(head(predict_unified(cvob5, newx = x_surv, s = "lambda.min")))
#'     print(head(predict_unified(cvob6, newx = x_surv, lambda.index = 4)))
#' }
#'
predict_unified <- function(fit, newx, s = "lambda.min", lambda.index, ...) {
    if (any(grepl("grpreg", class(fit)) | grepl("grpsurv", class(fit)))) {
        if (missing(lambda.index)) {
            lambda.index <- cv.lambdaIndex(
                fit = fit, s = s,
                fit.lambda.name = "lambda",
                fit.lambda.error.name = "cve",
                fit.lambda.errorSD.name = "cvse",
                ...
            )
        }
        s <- fit$lambda[[lambda.index]]
        if (any(grepl("grpsurv", class(fit)))) {
            tmp.coef <- predict(fit, lambda = s, type = "coef")
            if (nrow(tmp.coef) == 0) {
                retval <- newx[, 1]
                retval[TRUE] <- 0
            } else {
                retval <- newx %*% tmp.coef
            }
        } else {
            retval <- as.matrix(predict(fit, X = newx, lambda = s))
        }
    } else {
        retval <- as.matrix(predict(fit, newx = newx, s = s, ...))
    }
    # UseMethod("predict_unified", fit)
    return(retval)
}