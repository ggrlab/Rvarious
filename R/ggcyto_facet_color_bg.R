#' ggcyto facet coloring
#'
#' color facets of ggcyto output
#' @param ggcyto.plotlist
#' A plotlist received when *samplewise* applying ggcyto
#' @param bg.values
#' The values to the respective sample
#'
#' @return
#' A ggplot-plotlist with colored facets
#' @export
#'
#' @examples
#' library(ggcyto)
#' data(GvHD)
#' fs <- GvHD[1:3]
#' col1 <- "`FSC-H`"
#' col2 <- "`SSC-H`"
#' p.list <- flowCore::fsApply(fs, function(x) {
#'     ggcyto(x, aes_string(col1, col2)) +
#'         geom_hex(bins = 100)
#' })
#' ggpubr::ggarrange(plotlist = ggcyto_facet_color_bg(p.list, c("a", "b", "a")))
#' ggpubr::ggarrange(plotlist = ggcyto_facet_color_bg(p.list, c("a", "b", NA)))
#' ggpubr::ggarrange(plotlist = ggcyto_facet_color_bg(p.list, c(1, 6, 1)))
ggcyto_facet_color_bg <- function(ggcyto.plotlist,
                                  bg.values,
                                  numeric.color.palette = colorRampPalette(c("blue", "red")),
                                  numeric.max.distinct = 50,
                                  bg.legend.title = "NoTitle",
                                  remove.axis.labels = FALSE) {
    ggplot.list <- lapply(ggcyto.plotlist, ggcyto::as.ggplot)
    unique.values <- unique(bg.values)
    if (is.numeric(bg.values)) {
        stop("NOT properly implemented yet!")
        # Ansatz ggplot fuer farben:
        x <- seq(0, 1, length.out = 25)
        scales::seq_gradient_pal()(x)
        scales::seq_gradient_pal("white", "black")(x)

        # ansatz ohne ggplot, das macht den "legende-plot" dann aber schwieriger?
        if (length(unique.values) < numeric.max.distinct) {
            numeric.max.distinct <- length(unique.values)
        }
        color.possibilities <- numeric.color.palette(numeric.max.distinct)
        color.values <- as.numeric(cut(bg.values, breaks = numeric.max.distinct))
        ggplot.facet.colors <- color.possibilities[color.values]
    } else {
        col.colors <- Rvarious::gg_color_hue(length(unique.values))
        names(col.colors) <- unique.values
        ggplot.facet.colors <- col.colors[bg.values]

        color.df.legend <- tibble::tibble(
            "value" = names(col.colors),
            "color" = col.colors
        )
        color.df.legend[is.na(color.df.legend)] <- "<NA>"

        plot.legend <- ggplot2::ggplot(
            color.df.legend,
            ggplot2::aes_string(
                x = 1, y = "value", fill = "value", label = "value"
            )
        ) +
            ggplot2::geom_tile() +
            ggplot2::geom_label() +
            ggplot2::scale_fill_manual(values = col.colors) +
            ggplot2::theme_void() +
            ggplot2::ggtitle(bg.legend.title) +
            ggplot2::theme(
                legend.position = "none",
                plot.title = ggplot2::element_text(hjust = 0.5, size = 14)
            )
    }
    ggplot.list.facet.colored <- lapply(1:length(ggplot.list), function(facetN) {
        facet_color_bg_manual(ggplot.list[[facetN]], ggplot.facet.colors[facetN],
            remove.axis.labels = remove.axis.labels
        )
    })
    return(c(ggplot.list.facet.colored, list(plot.legend)))
}