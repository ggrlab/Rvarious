#' Survival percentage at timepoint
#'
#' Get the percentage of survivors for all strata at a specific timepoint
#'
#' @param survfit.obj
#' An object returned by survival::survfit()
#' @param timepoint
#' Which timepoint do you want to look at?
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
#' abline(h = tmp$surv.closest.lower.time, lty=2, col="red")
#' abline(v = tmp$closest.lower.time, lty=2, col="green")
#' abline(v = 10, lty=2, col="blue")

survival.perc_timepoint <- function(survfit.obj, timepoint=10, conf.int=0.95){
	if(conf.int != 0.95)
		stop("To change the confidence interval you have to change it in survfit.obj. See ?survival::survfitKM")
	a <- summary(survfit.obj, time=timepoint)
	tmp.df <- data.frame("time"=a$time, "strata"=a$strata, "surv"=a$surv
						 , "std.err"=a$std.err, "lowerCI"=a$lower, "upperCI"=a$upper)
	tmp.df$CI <- conf.int
	tmp.df$conf.type <- a$conf.type
	return(tmp.df)
}
