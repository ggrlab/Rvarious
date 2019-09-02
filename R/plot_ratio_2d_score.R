#' Plotting an "ratio"-plot
#'
#' Plot the x-range score, the y-range score when calculating
#' "minus" = y - x
#' "plus" =  y + x
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
#' @param titles
#' Named character vector c("minus"="", "plus"="", "total"="")
#' With the titles of the respective plot.
#' @param coefs
#' The coefficients, length == 3
#' @param interpolate
#' parameter for geom_raster()
#' @param additional.points
#' [Optional] additional points which will be plotted on the score-grid
#' @param cutoff.x
#' [Optional] Cutoff of the x-axis, check the plots to see the effect.
#' @param cutoff.y
#' [Optional] Cutoff of the y-axis, check the plots to see the effect.
#'
#' @return
#' Plots directly
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' testing_points <- expand.grid(x=(-1:5), y=(-1:9))
#' plot_ratio_2d_score(range.x = c(-5, 5)
#' 					,n.points.x = 100
#' 					,coefs = c(1, -1))
#' plot_ratio_2d_score(range.x = range(testing_points$x)
#' 					   ,range.y = range(testing_points$y)
#' 					   ,n.points.x = 100
#' 					   ,coefs = c(1, 1)
#' 					   ,additional.points = testing_points)
#' plot_ratio_2d_score(range.x = range(testing_points$x)
#' 					   ,range.y = range(testing_points$y)
#' 					   ,n.points.x = 20
#' 					   ,coefs = c(1, 1)
#' 					   ,additional.points = testing_points
#' 					   ,cutoff.x = 2, cutoff.y = 1
#' )

plot_ratio_2d_score <- function(range.x, range.y
								,n.points.x, n.points.y
								,coefs=c("minus"=1, "plus"=1)
								,titles=c("minus"="", "plus"="", "total"="")
								,additional.points
								,cutoff.x, cutoff.y
								,interpolate=TRUE
								,standardize.plot.color=FALSE
								,additional.points_color.values=NA){
	if(missing(n.points.x) && !missing(n.points.y))
		n.points.x <- n.points.y
	if(!missing(n.points.x) && missing(n.points.y))
		n.points.y <- n.points.x
	if(missing(range.x) && !missing(range.y))
		range.x <- range.y
	if(!missing(range.x) && missing(range.y))
		range.y <- range.x
	if(!missing(additional.points)){
		if(ncol(additional.points) < 2){
			stop("I need atleast two columns, first column goes x-axis, second column y-axis.")
		}else if(ncol(additional.points) == 2){
			# Then all fine
		}else{
			warning("I hardcode set the first two columns as value_1 and value_2 respectively, the other columns are not touched.")
		}
		colnames(additional.points)[1:2] <- c("value_1", "value_2")
	}



	if(length(coefs) != 2){
		stop("I need exactly two coefficients")
	}
	if(is.null(names(coefs))){
		names(coefs) <- c("minus", "plus")
	}
	values <- expand.grid("value_1"=seq(min(range.x), max(range.x), length.out = n.points.x)
						  ,"value_2"=seq(min(range.y), max(range.y), length.out = n.points.y))




	values$minus <- values[, 1] - values[, 2]
	values$plus <- values[, 1] + values[, 2]
	values_with_score <- tibble::as_tibble(values)
	values_with_score$score_minus <- values$minus * coefs["minus"]
	values_with_score$score_plus <- values$plus * coefs["plus"]
	values_with_score$score_total <- with(values_with_score, {score_minus + score_plus})

	if(standardize.plot.color){
		values_with_score[, 3:ncol(values_with_score)] <- tibble::as_tibble(scale(values_with_score[, 3:ncol(values_with_score)]))
	}

	score.limits <- range(values_with_score[, c("score_total", "score_minus", "score_plus")])
	plotlist <- list(
		"score_minus" = ggplot2::ggplot(values_with_score, ggplot2::aes(x=value_1, y=value_2, fill=score_minus))+
			ggtitle(titles["minus"], subtitle="Score minus")
		,"score_plus" = ggplot2::ggplot(values_with_score, ggplot2::aes(x=value_1, y=value_2, fill=score_plus))+
			ggtitle(titles["plus"], subtitle="Score plus")
		,"score_total" = ggplot2::ggplot(values_with_score, ggplot2::aes(x=value_1, y=value_2, fill=score_total))+
			ggtitle(titles["total"], subtitle="Score total"))

	plotlist <- lapply(plotlist, function(x){
		tmpplot <- x +
			# geom_tile()+
			geom_raster(interpolate = interpolate)+ # high performance of geom_tile if all tiles are equal size
			scale_fill_gradient2(limits=score.limits, low = scales::muted("blue"), high = scales::muted("red"))
		if(standardize.plot.color){
			tmpplot <- tmpplot +
				labs(fill="Score standardized per plot")
		}else{
			tmpplot <- tmpplot +
				labs(fill="Score")
		}
		tmpplot
	})


	if(!missing(additional.points)){
		if(standardize.plot.color)
			plotlist <- lapply(plotlist, function(x){
				if("col" %in% colnames(additional.points)){
					retval <- x + geom_point(data=additional.points, aes(fill=NULL, col=col))
					if(!all(is.na(additional.points_color.values))){
						retval <- retval + ggplot2::scale_color_manual(values=additional.points_color.values)
					}
				}else{
					retval <- x + geom_point(data=additional.points, aes(fill=NULL))
				}
				retval
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
				x <- x + ggplot2::geom_rect(xmin=-Inf, xmax=cutoff.x, ymin=-Inf, ymax=Inf, fill=ggplot2::alpha("green", alphavalue))
				x <- x + ggplot2::geom_rect(xmin=cutoff.x, xmax=Inf, ymin=-Inf, ymax=Inf, fill=ggplot2::alpha("red", alphavalue))
			}
			if(is.na(cutoff.y)){
				x <- x + ggplot2::geom_rect(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=cutoff.y, fill=ggplot2::alpha("green", alphavalue))
				x <- x + ggplot2::geom_rect(xmin=-Inf, xmax=Inf, ymin=cutoff.y, ymax=Inf, fill=ggplot2::alpha("red", alphavalue))
			}
			if(!is.na(cutoff.x) && !is.na(cutoff.y)){
				x <- x + ggplot2::geom_rect(xmin=-Inf, xmax=cutoff.x, ymin=-Inf, ymax=cutoff.y, fill=ggplot2::alpha("green", alphavalue))
				x <- x + ggplot2::geom_rect(xmin=cutoff.x, xmax=Inf, ymin=cutoff.y, ymax=Inf, fill=ggplot2::alpha("darkred", alphavalue))
				x <- x + ggplot2::geom_rect(xmin=-Inf, xmax=cutoff.x, ymin=cutoff.y, ymax=Inf, fill=ggplot2::alpha("orange", alphavalue))
				x <- x + ggplot2::geom_rect(xmin=cutoff.x, xmax=Inf, ymin=-Inf, ymax=cutoff.y, fill=ggplot2::alpha("orangered3", alphavalue))
			}
		}
		return(x)
	})

	p0_legend <- ggpubr::get_legend(plotlist[[1]])
	plotlist <- lapply(plotlist, function(x){x +
			ggplot2::theme(legend.position = "none")})
	gridExtra::grid.arrange(p0_legend, plotlist[[1]], plotlist[[3]], plotlist[[2]]
							,layout_matrix=matrix(c(0, 1, 1, 1, 2, 2, 2, 3, 3, 3)
												  , nrow=1))
}
