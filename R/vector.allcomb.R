#' All combinations of vector
#'
#' Generate all possible combinations of a named vector with a specific combination function.
#'
#' @param named.num.vector
#' A (named) numeric vector
#' @param combfun
#' How it should be combined, is overgiven to \code{outer(FUN=combfun)}
#' @param combfun.name
#' If the combfun is an (anonymous) function, you have to specify its name in combfun.name
#' @param ...
#' optional further arguments passed to FUN in the outer call: \code{outer(X, Y, FUN=combfun, ...)}
#'
#' @return
#' Numeric which has as names what is combined with what by which combfun:
#'
#' valuename1_<combfun>_valuename2
#' @export
#'
#' @examples
#' vector.allcomb(1:10, combfun = function(x, y) x**2, combfun.name = "^2")
#' vector.allcomb(1:10, combfun = "*")
#' myvec <- 1:10
#' names(myvec) <- paste0("gene", myvec)
#' vector.allcomb(1:10, combfun = "-")
#' vector.allcomb(1:10, combfun = "/")
#' complex_function <- function(x, y) {
#'     x**y
#' }
#' vector.allcomb(1:10, combfun = complex_function, combfun.name = "complex")
vector.allcomb <- function(named.num.vector, combfun = "/", combfun.name, ...) {
    if (length(named.num.vector) == 1) {
        return(named.num.vector)
    }
    # matrix with all ratios
    a <- outer(named.num.vector, named.num.vector, FUN = combfun, ...)
    # i only need a part of it (upper or lower triangular matrix)
    a[upper.tri(a, diag = TRUE)] <- NA
    b <- na.omit(reshape2::melt(a))
    named.values <- b$value

    # to check if the naming is correct:
    # exprs(miR_Ros.set)[1:3, 1]
    # a <- outer(exprs(miR_Ros.set)[1:3, 1], exprs(miR_Ros.set)[1:3, 1], "/")
    # a
    # ## so I see, it is x-axis-gene / y-axis-gene
    # a[upper.tri(a, diag=TRUE)] <- NA
    # b <- na.omit(reshape2::melt(a))
    # b
    # ## so it is Var1/Var2
    #
    if (is.function(combfun)) {
        if (missing(combfun.name)) {
            stop("If the combfun is an (anonymous) function, you have to specify its name in combfun.name!")
        }
    } else {
        combfun.name <- combfun
    }
    names(named.values) <- paste0(b$Var1, "_", combfun.name, "_", b$Var2)
    return(named.values)
}
