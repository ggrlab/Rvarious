
#' Prediction matrix of a multiclass problem
#'
#' @param pred_matrix
#' The predicted probabilities for each class (=columns) of each sample (=row)
#'
#' @param true_values
#' Either the labels from the columns of pred_matrix, or the indices for the respective columns
#'
#'
#' @param sort_by_true
#' If TRUE, then the rows are sorted by the true values.
#'
#' @return
#' @export
#'
#' @examples
#' n_samples <- 200
#' classes <- c('A', 'B', 'C', 'D')
#' ex_trues <- sample(classes, n_samples, replace=TRUE)
#'
#' example_predictions <- lapply(
#' 	ex_trues,
#' 	function(x){multiclass_probability_simulation(length(classes))}
#' )
#' example_predictions <- do.call(rbind, example_predictions)
#' colnames(example_predictions) <- classes
#'
#'
#' pdf('removeme1.pdf')
#' print(predplot_multiclass(pred_matrix = example_predictions
#' 					,true_values = ex_trues))
#' for(complete_letter in classes){
#' 	print(predplot_multiclass(pred_matrix = example_predictions
#' 							  ,true_values = rep(complete_letter, nrow(example_predictions))))
#' }
#' dev.off()

predplot_multiclass <- function(pred_matrix, true_values, sort_by_true=TRUE){

	# The idea is based on the code from plot_prob_multiclass in the CCC package
	possible_colors <- Rvarious::gg_color_hue(length(colnames(pred_matrix)))
	names(possible_colors) <- sort(colnames(pred_matrix))

	pred_matrix_color <- matrix(possible_colors, nrow=nrow(pred_matrix), ncol=ncol(pred_matrix), byrow=TRUE)

	pred_matrix_color_alphaed <- scales::alpha(pred_matrix_color, pred_matrix)
	dim(pred_matrix_color_alphaed) <- dim(pred_matrix)
	colnames(pred_matrix_color_alphaed) <- names(possible_colors)


	max_pred_col <- scales::alpha(
		possible_colors[apply(pred_matrix, 1, which.max)],
		apply(pred_matrix, 1, max))
	true_col <- possible_colors[vapply(true_values, function(x)which(colnames(pred_matrix) == x), numeric(1))]

	true_pred_matrix_color_alphaed <- cbind(true_col, max_pred_col, pred_matrix_color_alphaed)

	if(sort_by_true){
		true_pred_matrix_color_alphaed <- true_pred_matrix_color_alphaed[order(true_values, decreasing = TRUE), ]
	}

	rownames(true_pred_matrix_color_alphaed) <- paste0('S_', 1:nrow(true_pred_matrix_color_alphaed))
	melted.df <- reshape2::melt(true_pred_matrix_color_alphaed)
	return(ggplot2::ggplot(melted.df, ggplot2::aes(Var2, Var1)) +
		   	ggplot2::geom_raster(ggplot2::aes(fill = value)) +
		   	ggplot2::scale_fill_identity() +
		   	ggplot2::theme(
		   		legend.position = 'none',
		   		axis.title.y=ggplot2::element_blank(),
		   		axis.text.y=ggplot2::element_blank(),
		   		axis.ticks.y=ggplot2::element_blank()) + xlab(''))
}

multiclass_probability_simulation <- function(n_classes){

	probabilities <- numeric(n_classes)
	shuffled_indices <- sample(length(probabilities))
	for(i in shuffled_indices[-1]){
		probabilities[i] <- runif(1, min=0, max=1-sum(probabilities))
	}
	probabilities[shuffled_indices[1]] <- 1-sum(probabilities)
	return(probabilities)
}

