#' Color a facet title background
#'
#' Fill the background of a facet title with color!
#'
#' @param ggplotobj
#' A ggplot object with facet
#' @param bg.fills.vector
#' Colors, either length 1 or with length = # plots for this facet
#'
#' @return
#' A ggplot object
#' @export
#'
#' @examples
#'
#' #'
#' library(ggplot2)
#' p <- ggplot(mpg, aes(displ, cty)) +
#'     geom_point()
#' p_colfacetted <- p + facet_grid(cols = vars(cyl))
#' facet_color_bg_manual(p_colfacetted, "yellow")
#' facet_color_bg_manual(p_colfacetted, "green")
#' facet_color_bg_manual(p_colfacetted, c("red", "green", "orange", "gray"))
facet_color_bg_manual <- function(ggplotobj, bg.fills.vector = "red", remove.axis.labels = FALSE) {
    # From https://stackoverflow.com/questions/53455092/r-ggplot2-change-colour-of-font-and-background-in-facet-strip
    if (remove.axis.labels) {
        ggplotobj <- ggplotobj + ggplot2::theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
        )
    }
    # Generate the ggplot2 plot grob
    g <- grid::grid.force(ggplot2::ggplotGrob(ggplotobj))
    # Get the names of grobs and their gPaths into a data.frame structure
    grobs_df <- do.call(cbind.data.frame, grid::grid.ls(g, print = FALSE))
    # Build optimal gPaths that will be later used to identify grobs and edit them
    grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::")
    grobs_df$gPath_full <- gsub(
        pattern = "layout::",
        replacement = "",
        x = grobs_df$gPath_full,
        fixed = TRUE
    )
    # Get the gPaths of the strip background grobs
    strip_bg_gpath <- grobs_df$gPath_full[grepl(
        pattern = ".*strip\\.background.*",
        x = grobs_df$gPath_full
    )]
    # strip_bg_gpath[1] # example of a gPath for strip background
    # ## [1] "strip-t-1.7-5-7-5::strip.1-1-1-1::strip.background.x..rect.5374"

    if (length(bg.fills.vector) == 1) {
        fills <- rep(bg.fills.vector, length(strip_bg_gpath))
    } else if (length(bg.fills.vector) != length(strip_bg_gpath)) {
        stop("Need either one color for all or one color for each resulting facet ")
    } else {
        fills <- bg.fills.vector
    }
    # Edit the grobs
    for (i in 1:length(strip_bg_gpath)) {
        g <- grid::editGrob(grob = g, gPath = strip_bg_gpath[i], gp = grid::gpar(fill = fills[i]))
    }
    ggpubr::as_ggplot(g)
}




# facet_color_bg <- function(ggplotobj
# 						   , bg.values=c("Hepatitis", "nonHepatitis")){
# 	if(is.numeric(bg.values)){
# 		stop("Not implemented yet, but gradual coloring")
# 	}else{
# 		unique.values <- unique(bg.values)
# 		col.colors <- Rvarious::gg_color_hue(length(unique.values))
# 		names(col.colors) <- unique.values
# 	}
# 	facet_color_bg_manual(ggplotobj, col.colors[unique.values])
# }
# facet_color_bg(p_colfacetted)