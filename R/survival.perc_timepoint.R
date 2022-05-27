#' Survival percentage at timepoint
#'
#' Get the percentage of survivors for all strata at a specific timepoint
#'
#' @param survfit.obj
#' An object returned by survival::survfit()
#' @param timepoint
#' Which timepoint do you want to look at?
#' @param conf.int
#' What should the confidence interval be (in real numbers)
#' @param ret.pvals
#' Binary, return the p-values?
#'
#' @return
#' tibble
#' @export
#'
#' @examples
#'
#' library(survival)
#' library(Rvarious)
#' leukemia.surv <- survfit(Surv(time, status) ~ x, data = aml)
#' tmp <- survival.perc_timepoint(leukemia.surv, 10)
#' print(tmp)
#' plot(leukemia.surv)
#' abline(h = tmp$survestimates[1, "surv"], lty = 4, col = "red")
#' abline(v = tmp$survestimates[1, "time"], lty = 4, col = "green")
#' abline(v = 10, lty = 2, col = "blue")
survival.perc_timepoint <- function(survfit.obj, timepoint = 10, conf.int = 0.95,
                                    ret.pvals = TRUE) {
    calc.pval <- function(two_survrows) {
        # calculation of p-values taken from SurvivalAnalysis_KleinMoeschberger,
        # 7.8 "Test Based on Differences in Outcome at a Fixed Point in Time"
        if (nrow(two_survrows) != 2) {
            stop("Only works with two rows!")
        }
        z_score <- (two_survrows$surv[1] - two_survrows$surv[2]) /
            (sqrt(two_survrows$std.err[1]^2 + two_survrows$std.err[2]^2))
        pz <- pnorm(abs(z_score))
        # get p value - two sided
        return(2 * (1 - pz))
    }
    if (conf.int != 0.95) {
        stop("To change the confidence interval you have to change it in survfit.obj. See ?survival::survfitKM")
    }
    a <- summary(survfit.obj, time = timepoint)
    tmp.df <- data.frame(
        "time" = a$time, "strata" = a$strata, "surv" = a$surv,
        "std.err" = a$std.err, "lowerCI" = a$lower, "upperCI" = a$upper
    )
    tmp.df$CI <- conf.int
    tmp.df$conf.type <- a$conf.type
    retval <- tmp.df
    if (ret.pvals) {
        combinations <- combn(nrow(tmp.df), 2)
        pval.mat <- apply(combinations, 2, function(rowsN) {
            calc.pval(tmp.df[rowsN, ])
        })
        names(pval.mat) <- apply(combinations, 2, function(x) {
            paste0(as.character(tmp.df$strata)[x], collapse = "_vs_")
        })
        retval <- list(
            "survestimates" = tmp.df,
            "pvalues_KleinMoesch" = pval.mat
        )
    }

    return(retval)
}