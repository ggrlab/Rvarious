#' Title
#'
#' @param predictions
#' The prediction values
#' @param trues
#' The respective true values
#' @param names
#' Names of the samples
#' @param truenames
#' E.g. c("1"="infected", "0"="non.infected")
#' Those names replace the values on the y-axis
#' @param maintitle
#' Title
#' @param cutoff
#' Cutoff, necessary for horizontal line and performance calculations
#' @param probabilities
#' If true, a second y-axis on the right side is added claiming
#' c("certainly Infected", "certainly not infected")
#'
#' @return
#' A dataframe with AUC, accuracy, sensitivity, specificity and the used cutoff.
#' @export
#'
#' @examples
#'
plot_predictions <- function(
	predictions, trues, names, truenames=c("1"="infected", "0"="non.infected")
	,maintitle="", cutoff=.5, probabilities=TRUE){

	tmp.res <- data.frame("sample"=names
					  ,"prob.Infected"=predictions
					  ,"trueValues"=trues
					  ,"trueClass"=truenames[as.character(trues)])
	tmp.res$prediction_cutoff <- ifelse(tmp.res$prob.Infected > cutoff, 1, 0)
	tmp.res <- arrange(tmp.res, prob.Infected)
	tmp.res$sample <- factor(tmp.res$sample, levels=tmp.res$sample)
	plot00 <- ggplot(tmp.res, aes(x=sample, y=prob.Infected, col=trueClass)) +
		geom_point(na.rm = TRUE) +
		theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5, size=7)
			  ,legend.position = "top")+
		geom_hline(yintercept = cutoff)

	if(probabilities){
		plot00 <- plot00 +
			scale_y_continuous(sec.axis =
							   	sec_axis(~ .
							   			 , breaks = c(1, 0), labels=c("certainly Infected", "certainly not infected"))
							   , limits = c(0,1))
	}
	pred <- ROCR::prediction(predictions = tmp.res$prob.Infected
							 ,labels = tmp.res$trueValues)
	perf <- c()
	perf <- c(perf, "AUC"=ROCR::performance(pred, "auc")@y.values[[1]])
	perf2 <- ROCR::performance(pred, "tpr", "fpr")
	pred.discrete <- ROCR::prediction(predictions = tmp.res$prediction_cutoff
									  ,labels = tmp.res$trueValues)
	# data(ROCR.simple);tmproc <- data.frame(preds=pred@predictions[[1]],labs=pred@labels[[1]])

	# check if 2 or 3 datapoints:
	if(length(ROCR::performance(pred.discrete, measure = "acc")@y.values[[1]]) == 2){
		select_index <- unique(tmp.res$prediction_cutoff)+1
	}else{
		select_index <- 2
	}
	# see  ?performance
	perf <- c(perf, "Accuracy"=ROCR::performance(pred.discrete, measure = "acc")@y.values[[1]][select_index])
	# Specificity = TNR = noninfected.precision = P(Yhat = - | Y = -) = TN / N
	perf <- c(perf, "specificity"=ROCR::performance(pred.discrete, measure = "spec")@y.values[[1]][select_index])
	# sensitivity
	perf <- c(perf, "sensitivity"=ROCR::performance(pred.discrete, measure = "sens")@y.values[[1]][select_index])
	names(perf)[2:4] <- paste0(names(perf)[2:4], "_", round(cutoff, 2))
	perf_subtitle <- paste0(names(perf), "=", round(perf, 3), collapse = "  ")
	plot00 <- plot00 + ggtitle(label = maintitle
							   ,subtitle = perf_subtitle)
	print(plot00)
	ROCR::plot(perf2, main=maintitle, sub=perf_subtitle)
	perf <- c(perf, "cutoff"=cutoff)
	return(perf)
}
