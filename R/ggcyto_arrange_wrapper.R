#' Plot multiple colored ggcyto objects
#'
#' Plot multiple ggcyto objects, colored by facet.coloring.vec
#'
#' @param ggcyto.plotlist
#' A list of ggcyto objects, received from ggcyto::ggcyto (tested: one sample one element)
#' @param facet.coloring.vec
#' A vector with length(ggcyto.plotlist) responsible for coloring the facet background
#' @param do.sort
#' Should sorting for facet.coloring.vec be done?
#' @param max.columns
#' Maximum amount of columns in a plot
#' @param facet.coloring.title
#' Title for the facet color legend
#' @param global.axis.labels
#' If TRUE, then all single-plot-axis are removed and one large global axis is added.
#'
#' @return
#' A ggplot object
#' @export
#'
#' @examples
#' # ggcyto_arrange_wrapper(ggcyto.plotlist = combinationXX_plots, facet.coloring.vec = as.character(Biobase::pData(tmp.panel)$Hepatitis))
#'
ggcyto_arrange_wrapper <- function(ggcyto.plotlist, facet.coloring.vec, do.sort = TRUE,
                                   max.columns = 10, facet.coloring.title = "NoTitle",
                                   global.axis.labels = TRUE) {
    if (length(facet.coloring.vec) != length(ggcyto.plotlist)) {
        stop("Unequal length")
    }
    colored.combinationXX_plots <- Rvarious::ggcyto_facet_color_bg(
        ggcyto.plotlist = ggcyto.plotlist,
        bg.values = facet.coloring.vec,
        bg.legend.title = facet.coloring.title,
        remove.axis.labels = global.axis.labels
    )
    if (global.axis.labels) {
        arrow.df <- data.frame(x = c(0, 1), y = 0)
        axis.names <- as.character(ggcyto.plotlist[[1]]$mapping)
        axis.names <- sub("~", "", axis.names)
        names(axis.names) <- names(ggcyto.plotlist[[1]]$mapping)
        x.axis <- ggplot(arrow.df, aes(x, y)) +
            geom_point(size = .01) +
            theme_void() +
            annotate("text",
                x = .5,
                y = 0, label = axis.names["x"], angle = 0,
                colour = "black", vjust = .5
            ) +
            geom_segment(
                x = 0, y = 0, xend = 1, yend = 0,
                size = 1,
                arrow = arrow(length = unit(0.5, "cm"))
            )
        y.axis <-
            ggplot(arrow.df, aes(y, x)) +
            geom_point(size = .01) +
            theme_void() +
            annotate("text",
                x = 0,
                y = .5, label = axis.names["y"], angle = 90,
                colour = "black", vjust = .5
            ) +
            geom_segment(
                x = 0, y = 0, xend = 0, yend = 1, size = 1,
                arrow = arrow(length = unit(0.5, "cm"))
            )
    }
    if (do.sort) {
        colored.combinationXX_plots.ordered <- colored.combinationXX_plots[order(facet.coloring.vec)]
        values.table <- table(facet.coloring.vec, useNA = "always")
        list.layoutmats <- lapply(values.table, function(valX) {
            layoutmat <- do.call(cbind, rep(list(rep(1, max.columns)), valX / max.columns))
            if ((valX %% max.columns) != 0) {
                layoutmat <- cbind(layoutmat, c(rep(1, valX %% max.columns), rep(NA, max.columns - (valX %% max.columns))))
            }
            return(layoutmat)
        })
        layoutmat <- do.call(cbind, list.layoutmats)
        layoutmat[!is.na(layoutmat)] <- 1:length(colored.combinationXX_plots.ordered)
        layoutmat <- t(layoutmat)
        plotting.list <- c(
            colored.combinationXX_plots.ordered,
            colored.combinationXX_plots[length(colored.combinationXX_plots)]
        )
    } else {
        fillvec <- c(1:(length(colored.combinationXX_plots) - 1), rep(NA, max.columns - ((length(colored.combinationXX_plots) - 1) %% max.columns)))
        layoutmat <- matrix(fillvec, ncol = max.columns, byrow = TRUE)
        plotting.list <- colored.combinationXX_plots
    }
    # now add the legend as the last NA plot or as own row if there are no NA plots
    if (any(is.na(layoutmat))) {
        layoutmat[is.na(layoutmat)][sum(is.na(layoutmat))] <- length(colored.combinationXX_plots)
    } else {
        layoutmat <- rbind(layoutmat, length(colored.combinationXX_plots))
    }
    # add the axis
    if (global.axis.labels) {
        # This is that the arrows get way smaller page-size than the plots itself
        # valid, tested values instead of 4 are: 2
        for (i in 1:4) {
            layoutmat <- apply(layoutmat, 2, function(x) rbind(x, x))
            layoutmat <- apply(layoutmat, 1, function(x) rbind(x, x))
        }
        layoutmat <- rbind(layoutmat, length(colored.combinationXX_plots) + 1)
        layoutmat <- cbind(length(colored.combinationXX_plots) + 2, layoutmat)
        layoutmat[nrow(layoutmat), 1] <- NA
        plotting.list <- c(plotting.list, list(x.axis, y.axis))
    }
    ggpubr::as_ggplot(gridExtra::arrangeGrob(
        grobs = plotting.list,
        layout_matrix = layoutmat
    ))
}