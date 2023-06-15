#' Cut at cutvalues
#'
#'
#'
#' @param values.to.cut
#' What should be cutted
#' @param cutvalues
#' where should be cutted
#' @param labels
#' optional labels
#'
#' @return
#' A factor
#' @export
#'
#' @examples
#' cut_at(1:15, 5)
#' cut_at(1:15, c(5, 7))
#' cut_at(1:15, c(5, 7, 9))
cut_at <- function(values.to.cut, cutvalues, labels) {
    cutvalues <- sort(unique(cutvalues))
    tmp <- cut(values.to.cut, breaks = c(-Inf, cutvalues, Inf))
    if (length(cutvalues) == 1) {
        levels(tmp) <- c("lo", "hi")
    }
    if (length(cutvalues) == 2) {
        levels(tmp) <- c("lo", "mid", "hi")
    }
    if (!missing(labels)) {
        levels(tmp) <- labels
    }
    return(tmp)
}
