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
#'
#'
survival.perc_timepoint <- function(survfit.obj, timepoint=10){
	a <- summary(survfit.obj)
	tmp.df <- data.frame(time=a$time, strata=a$strata, surv=a$surv)
	# add a "0" timepoint (before everything what happened anytime, so -Inf)
	for(strataX in unique(tmp.df$strata)){
		tmp.df <- dplyr::add_row(tmp.df, time=-Inf, strata=strataX, surv=1)
	}
	tmp.df <- tmp.df %>% dplyr::arrange(strata, time)
	closest <- tmp.df %>%
		dplyr::filter(time <= timepoint) %>%
		dplyr::group_by(strata) %>%
		dplyr::summarise(surv.closest.lower.time = surv[which.max(time)]
				  ,closest.lower.time = max(time))
	return(closest)
}
