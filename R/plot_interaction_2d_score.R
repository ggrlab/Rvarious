#' Plotting an interaction plot
#'
#' Plot the x-range score, the y-range score and the interaction score.
#'
#' The interaction score is $x*y * coef_{interaction}$
#'
#' And the total score
#'
#' @param range.x
#' Range of x axis
#' @param range.y
#' Range of y axis
#' @param n.points.x
#' How many points should be generated in the x-axis range.
#' The final grid will have n.points.x * n.points.y points!
#' @param n.points.y
#' How many points should be generated in the y-axis range.
#' The final grid will have n.points.x * n.points.y points!
#' @param coefs
#' The coefficients, length == 3
#' @param additional.points
#' [Optional] additional points which will be plotted on the score-grid
#' @param cutoff.x
#' [Optional] Cutoff of the x-axis, check the plots to see the effect.
#' @param cutoff.y
#' [Optional] Cutoff of the y-axis, check the plots to see the effect.
#'
#' @return
#' Plots directly
#' @export
#'
#' @examples
#' library(ggplot2)
#' testing_points <- expand.grid(x=(-1:5), y=(-1:9))
#' plot_interaction_2d_score(range.x = c(-5, 5)
#' 					   ,n.points.x = 100
#' 					   ,coefs = c(1, 1, -2))
#' plot_interaction_2d_score(range.x = range(testing_points$x)
#' 					   ,range.y = range(testing_points$y)
#' 					   ,n.points.x = 100
#' 					   ,coefs = c(1, 1, -.5)
#' 					   ,additional.points = testing_points)
#' plot_interaction_2d_score(range.x = range(testing_points$x)
#' 					   ,range.y = range(testing_points$y)
#' 					   ,n.points.x = 20
#' 					   ,coefs = c(1, 1, -.5)
#' 					   ,additional.points = testing_points
#' 					   ,cutoff.x = 2, cutoff.y = 1
#' )

plot_interaction_2d_score <- function(range.x, range.y
								   ,n.points.x, n.points.y
								   ,coefs=c("x"=1, "y"=1, "interaction"=.5)
								   ,additional.points
								   ,cutoff.x, cutoff.y){
	if(length(coefs)!=3)
		stop("length(coefs) must be 3")
	if(is.null(names(coefs))){
		names(coefs) <- c("x", "y", "interaction")
	}
	if(missing(n.points.x) && !missing(n.points.y))
		n.points.x <- n.points.y
	if(!missing(n.points.x) && missing(n.points.y))
		n.points.y <- n.points.x
	if(missing(range.x) && !missing(range.y))
		range.x <- range.y
	if(!missing(range.x) && missing(range.y))
		range.y <- range.x
	if(!missing(additional.points)){
		if(ncol(additional.points) != 2)
			stop("I need exactly two columns, first column goes x-axis, second column y-axis.")
		colnames(additional.points) <- c("value_1", "value_2")
	}

	values <- expand.grid("value_1"=seq(min(range.x), max(range.x), length.out = n.points.x)
						  ,"value_2"=seq(min(range.y), max(range.y), length.out = n.points.y))
	values$interaction <- values[, 1] * values[, 2]
	values_with_score <- tibble::as_tibble(values)
	values_with_score$score_total <- (as.matrix(values) %*% coefs)[, 1]
	values_with_score$score_x <- values$value_1 * coefs["x"]
	values_with_score$score_y <- values$value_2 * coefs["y"]
	values_with_score$score_interaction <- values$interaction * coefs["interaction"]

	score.limits <- range(values_with_score[, c("score_total", "score_x", "score_y", "score_interaction")])
	# library(ggplot2)
	plotlist <- list(
		"score_x" = ggplot(values_with_score, aes(x=value_1, y=value_2, fill=score_x))+
			ggtitle("Score x")
		,"score_y" = ggplot(values_with_score, aes(x=value_1, y=value_2, fill=score_y))+
			ggtitle("Score y")
		,"score_interaction" = ggplot(values_with_score, aes(x=value_1, y=value_2, fill=score_interaction))+
			ggtitle("Score interaction")
		,"score_total" = ggplot(values_with_score, aes(x=value_1, y=value_2, fill=score_total))+
			ggtitle("Score total"))

	plotlist <- lapply(plotlist, function(x){
		x +
			# geom_tile()+
			geom_raster(interpolate = TRUE)+ # high performance of geom_tile if all tiles are equal size
			scale_fill_gradient2(limits=score.limits, low = scales::muted("blue"), high = scales::muted("red"))+
			labs(fill="Score")
	})

	if(!missing(additional.points)){

		plotlist <- lapply(plotlist, function(x){
			x + geom_point(data=additional.points, aes(fill=NULL))
		})
	}
	cutoff.x <- ifelse(missing(cutoff.x), NA, cutoff.x)
	cutoff.y <- ifelse(missing(cutoff.y), NA, cutoff.y)
	plotlist <- lapply(plotlist, function(x){
		if(is.na(cutoff.y) && is.na(cutoff.x)){
			return(x)
		}else{
			alphavalue <- 0.002
			if(is.na(cutoff.x)){
				x <- x + geom_rect(xmin=-Inf, xmax=cutoff.x, ymin=-Inf, ymax=Inf, fill=alpha("green", alphavalue))
				x <- x + geom_rect(xmin=cutoff.x, xmax=Inf, ymin=-Inf, ymax=Inf, fill=alpha("red", alphavalue))
			}
			if(is.na(cutoff.y)){
				x <- x + geom_rect(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=cutoff.y, fill=alpha("green", alphavalue))
				x <- x + geom_rect(xmin=-Inf, xmax=Inf, ymin=cutoff.y, ymax=Inf, fill=alpha("red", alphavalue))
			}
			if(!is.na(cutoff.x) && !is.na(cutoff.y)){
				x <- x + geom_rect(xmin=-Inf, xmax=cutoff.x, ymin=-Inf, ymax=cutoff.y, fill=alpha("green", alphavalue))
				x <- x + geom_rect(xmin=cutoff.x, xmax=Inf, ymin=cutoff.y, ymax=Inf, fill=alpha("darkred", alphavalue))
				x <- x + geom_rect(xmin=-Inf, xmax=cutoff.x, ymin=cutoff.y, ymax=Inf, fill=alpha("orange", alphavalue))
				x <- x + geom_rect(xmin=cutoff.x, xmax=Inf, ymin=-Inf, ymax=cutoff.y, fill=alpha("orangered3", alphavalue))
			}
		}
		return(x)
	})


	p0_legend <- ggpubr::get_legend(plotlist[[1]])
	plotlist <- lapply(plotlist, function(x){x +
			theme(legend.position = "none")})
	gridExtra::grid.arrange(p0_legend, plotlist[[1]], plotlist[[2]]
							, plotlist[[3]], plotlist[[4]]
							, layout_matrix=matrix(c(0, 3, NA, 1, 4,2)
												   , nrow=2, byrow = TRUE))
	# a <- gridExtra::arrangeGrob(p0_legend, p0, p1, p2, p3, layout_matrix=matrix(c(0, 3, NA, 1, 4,2), nrow=2, byrow = TRUE))
	# return(ggpubr::as_ggplot(a))
}
