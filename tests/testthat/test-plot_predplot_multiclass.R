test_that("plot_predplot_multiclass", {
    n_samples <- 200
    classes <- c("A", "B", "C", "D")
    ex_trues <- sample(classes, n_samples, replace = TRUE)

    example_predictions <- lapply(
        ex_trues,
        function(x) {
            multiclass_probability_simulation(length(classes))
        }
    )
    example_predictions <- do.call(rbind, example_predictions)

    colnames(example_predictions) <- classes
    predplot_multiclass(
        pred_matrix = example_predictions,
        true_values = ex_trues
    )
})
test_that("plot_predplot_multiclass factors", {
    n_samples <- 25
    classes <- c("A", "B", "C", "D")
    ex_trues <- sample(classes, n_samples, replace = TRUE)

    example_predictions <- lapply(
        ex_trues,
        function(x) {
            multiclass_probability_simulation(length(classes))
        }
    )
    example_predictions <- do.call(rbind, example_predictions)

    colnames(example_predictions) <- classes
    predplot_multiclass(
        pred_matrix = example_predictions,
        true_values = factor(ex_trues, levels = rev(classes))
    )
    predplot_multiclass(
        pred_matrix = example_predictions,
        true_values = factor(ex_trues, levels = c("B", "C", "A", "D"))
    )
})
