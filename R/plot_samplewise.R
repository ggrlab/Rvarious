#' Plot multiple (1+) values for the same names
#'
#' @param values.df Data frame with numerical values (1+ columns, colname is taken as "dataname")
#' @param samplenames Names of the samples (N-th samplename --> N-th value in values.df)
#' @param valuename How are the values called you plot?
#' @param xname x-axis Label name
#' @param hlineintercept (named) intercept for the hline
#' @param yrange y-range
#' @param connectinglines Should per Sample the values be connected?
#' @param maintitle00
#'    Title of the plot
#' @param sorting.dfindex Standard: NA, unsorted. Else: Give index after which values.df-column the x-axis should be sorted
#' @param returnADS
#'    Return the absolute distance sum between all points for one column, summed up over all columns.
#'
#' @return
#' @export
#'
#' @examples
#' values.df <- data.frame("a"=1:20, "b"=1:20+0.5, 'c'=(20:1)*.5)
#' names <- paste0('f_', 101:120)
#' print(plot_samplewise(values.df, names))
#' print(plot_samplewise(values.df, names, sorting.dfindex=3))
#' print(plot_samplewise(values.df, names, hlineintercept=5))
#' print(plot_samplewise(values.df, names, hlineintercept=c("cutoff"=5)))
#' print(plot_samplewise(values.df, names, hlineintercept=c("cutoff"=5)))
#' print(plot_samplewise(values.df, names, connectinglines=TRUE))
#' print(plot_samplewise(values.df, names, returnADS=TRUE))
plot_samplewise <- function(values.df
							,samplenames
							,valuename="Coefficient"
							,xname="Genename"
							,hlineintercept=NA
							,yrange=NA
							,connectinglines=FALSE
							,maintitle00=NA
							,sorting.dfindex=FALSE
							,returnADS=FALSE){

	if(dim(values.df)[1] != length(samplenames)){
		stop("Samplenames and amount of coefficients is different!")
	}

	values.df$names <- samplenames
	if(sorting.dfindex){
		values.df$names <- factor(values.df$names, levels = values.df$names[order(values.df[, sorting.dfindex])])
	}
	values.df.melted <- reshape2::melt(values.df, id.vars = "names")
	colnames(values.df.melted) <- c(xname, "Dataname", valuename)
	plot00 <- ggplot2::ggplot(
		values.df.melted,
		ggplot2::aes_string(x=xname, y=valuename, col="Dataname")) +
		ggplot2::geom_point()
	if(!is.na(hlineintercept)){
		plot00 <- plot00 + ggplot2::geom_hline(yintercept = hlineintercept)
		if(!is.null(names(hlineintercept))){
			plot00 <- plot00 +
				ggplot2::geom_text(ggplot2::aes( -Inf, hlineintercept
							   , label = names(hlineintercept)
							   , vjust=0, hjust=0),  color="darkgreen", size=3)+
				ggplot2::geom_text(ggplot2::aes( Inf, hlineintercept
							   , label = names(hlineintercept)
							   , vjust=0, hjust=1),  color="darkgreen", size=3)
		}
	}

	distval.df <- values.df[, ! (colnames(values.df) %in% "names")]
	distsum <- 0
	for (colN in 1:(dim(distval.df)[2]-1)){
		for(colM in (colN+1):dim(distval.df)[2]){
			distsum <- distsum + sum(abs(distval.df[, colN] - distval.df[, colM]))
		}
	}
	if(connectinglines){
		correlation <- cor(values.df[, 1:2])
		correlation <- correlation[1,2]
		label0 <- sprintf("AbsoluteDistSum=%6.3f\nr2=%6.4f", distsum, correlation**2)

		plot00 <- plot00 +
			ggplot2::geom_line(ggplot2::aes_string(group=xname))+
			ggplot2::annotate(
				"text", x = dim(distval.df)[1], y = Inf,
				label = label0, hjust=1)
	}
	plot00 <- plot00  +
		ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)
			  ,legend.justification=c(0.05,0.85),
			  legend.position=c(0, 1),
			  legend.background = ggplot2::element_rect(fill="transparent"),
			  legend.title = ggplot2::element_text(face="bold"),
			  legend.key.size = ggplot2::unit(0.4, "cm"),
			  legend.box.just = "left"
			  ,plot.title = ggplot2::element_text(size=11))
	if(! anyNA(yrange)){
		plot00 <- plot00 + ggplot2::ylim(yrange)
	}

	plot00 <- plot00 + ggplot2::ggtitle(maintitle00)

	if(returnADS){
		return(list("plot"=plot00, "ADS"=distsum))
	}else{
		return(plot00)
	}
}
