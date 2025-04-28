
#' Get binary performance
#'
#' Get performance measures for binary classification problems
#'
#' @param prob_positive
#'   Probabilities to be in the positive class. Do not essentially have to be 0...1 values but
#'   must be the values predicting the positive class
#' @param class_positive
#'   The true class values
#' @param measures
#'   Which measures you want to have, usually measures which are connected to a cutoff.
#'   The measures are returned from ROCit::measureit, see further information there.
#'   'Cutoff' is the actual cutoff value, 'Depth' what proportion of observations fall on or above
#'   the cutoff.
#' @param cutoff
#'   The cutoff where you want the "measures" from.
#'
#' @export
#'
#' @examples
#'
#' get_binary_performance(
#'     prob_positive = 1:100,
#'     class = c(rep(0, 50), rep(1, 50))
#' )
#' get_binary_performance(
#'     prob_positive = 1:100,
#'     class = c(rep(0, 50), rep(1, 50)),
#'     cutoff = 50
#' )
#' get_binary_performance(
#'     prob_positive = (1:100) / 100,
#'     class = c(rep(0, 50), rep(1, 50)),
#'     cutoff = .50,
#'     measures = "AUC"
#' )
get_binary_performance <- function(prob_positive,
                                   class,
                                   measures = c("AUC", "ACC", "SPEC", "SENS"),
                                   cutoff = 0.5) {
    rocit_perf <- ROCit::rocit(
        score = prob_positive,
        class = class
    )
    # Measureit returns all possible cutoffs (or atleast many)
    rocit_singleperf <- ROCit::measureit(
        rocit_perf,
        measure = measures
    )
    rocit_singleperf <- data.frame(do.call(cbind, rocit_singleperf))
    # Get the first cutoff which is smaller than the prospected cutoffs
    # as the cutoffs are sorted decreasingly
    cutoff_index <- which(rocit_singleperf$Cutoff <= cutoff)[1]
    rocit_singleperf <- rocit_singleperf[cutoff_index, ]

    rocit_measures <- unlist(rocit_perf[names(rocit_perf) %in% measures])
    for (measureX in names(rocit_measures)) {
        rocit_singleperf <- cbind(rocit_singleperf, rocit_measures[measureX])
        colnames(rocit_singleperf)[length(rocit_singleperf)] <- measureX
    }
    return(list("perf" = rocit_perf, "cutoff_measures" = rocit_singleperf))
}
