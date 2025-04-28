
#' Title
#'
#' @export
#'
#' @examples
#'
#' set.seed(17)
#' sampletype <- sample(c("train", "test"), 100, replace = TRUE)
#' plot_probability_pred_true(
#'     predictions = 1:100,
#'     trues = c(rep(0, 50), rep(1, 50)),
#'     names = paste0("S", 1:100),
#'     truenames = c("1" = "infected", "0" = "non.infected"),
#'     maintitle = "CRP",
#'     cutoff = 60,
#'     probabilities = FALSE
#'     # ,sampletype=sampletype
#'     , blackwhite = TRUE
#' )
#'
plot_probability_pred_true <- function(predictions, trues, names, truenames = c("1" = "infected", "0" = "non.infected"),
                                       maintitle = "", cutoff = .5, probabilities = TRUE,
                                       sampletype, blackwhite = FALSE,
                                       measures = c("AUC", "ACC", "SPEC", "SENS")) {
    tmp.res <- tibble::tibble(
        "sample" = names,
        "prob.positiveclass" = predictions,
        "trueValues" = trues,
        "trueClass" = truenames[as.character(trues)]
    )
    if (!missing(sampletype) && length(sampletype) == nrow(tmp.res)) {
        tmp.res$sampletype <- sampletype
    } else {
        tmp.res$sampletype <- "same"
    }

    tmp.res <- dplyr::arrange(tmp.res, prob.positiveclass)
    tmp.res$sample <- factor(tmp.res$sample, levels = tmp.res$sample)

    if (blackwhite) {
        if (all(tmp.res$sampletype == "same")) {
            tmp.res$sampletype <- paste0(tmp.res$trueClass)
        } else {
            tmp.res$sampletype <- paste0(tmp.res$trueClass, "_", tmp.res$sampletype)
        }
        plot00 <- ggplot2::ggplot(tmp.res, ggplot2::aes(x = sample, y = prob.positiveclass, shape = sampletype)) +
            ggplot2::scale_shape_manual(values = c(17, 2, 16, 1))
    } else {
        if (all(tmp.res$sampletype == "same")) {
            plot00 <- ggplot2::ggplot(tmp.res, ggplot2::aes(x = sample, y = prob.positiveclass, col = trueClass)) +
                ggplot2::scale_shape_manual(values = c(17, 2))
        } else {
            plot00 <- ggplot2::ggplot(tmp.res, ggplot2::aes(x = sample, y = prob.positiveclass, shape = sampletype, col = trueClass)) +
                ggplot2::scale_shape_manual(values = c(17, 2))
        }
    }
    plot00 <- plot00 +
        ggplot2::geom_point(na.rm = TRUE) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = .5, size = 7),
            legend.position = "top"
        ) +
        ggplot2::geom_hline(yintercept = cutoff)


    if (probabilities) {
        plot00 <- plot00 +
            ggplot2::scale_y_continuous(
                sec.axis =
                    ggplot2::sec_axis(~.,
                        breaks = c(1, 0),
                        labels = c(
                            paste0(
                                "certainly ",
                                truenames["1"]
                            ),
                            paste0(
                                "certainly ",
                                truenames["0"]
                            )
                        )
                    ),
                limits = c(0, 1)
            )
    }
    rocit_perf <- get_binary_performance(
        prob_positive = tmp.res$prob.positiveclass,
        class = tmp.res$trueValues,
        cutoff = cutoff,
        measures = measures
    )

    perf_subtitle_all <- paste0(names(rocit_perf$cutoff_measures), "=", round(rocit_perf$cutoff_measures, 3), collapse = "  ")
    perf_subtitle <- perf_subtitle_all
    perf_list <- list("allsamples" = rocit_perf)
    if (!missing(sampletype) && length(sampletype) == nrow(tmp.res)) {
        res.train <- tmp.res[tmp.res$sampletype == "train", ]
        res.test <- tmp.res[tmp.res$sampletype == "test", ]
        perf_train <- get_binary_performance(
            prob_positive = res.train$prob.positiveclass,
            class = res.train$trueValues,
            cutoff = cutoff,
            measures = measures
        )
        perf_test <- get_binary_performance(
            prob_positive = res.test$prob.positiveclass,
            class = res.test$trueValues,
            cutoff = cutoff,
            measures = measures
        )
        perf_subtitle <- paste0("only test:   ", paste0(names(perf_test$cutoff_measures), "=", round(perf_test$cutoff_measures, 3), collapse = "  "))
        perf_list <- c(perf_list, list("train" = perf_train, "test" = perf_test))
    }

    plot00 <- plot00 + ggplot2::ggtitle(
        label = maintitle,
        subtitle = perf_subtitle
    )


    perf_list <- c(perf_list, "cutoff" = cutoff)
    return(
        list(
            "perf" = perf_list,
            "predplot" = plot00,
            "ROCcurve" = ROCit:::plot.rocit(perf_list$allsamples$perf)
        )
    )
}
