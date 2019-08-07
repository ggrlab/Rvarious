#' ggplot color hue
#'
#' This generates the default ggplot colors
#'
#' @param n
#' How many colors you want
#'
#' @return
#' Character vector with the hex-colors of ggplot.
#' @export
#'
#' @examples
#' gg_color_hue(10)
#' gg_color_hue(2)
gg_color_hue <- function(n) {
	hues = seq(15, 375, length = n + 1)
	hcl(h = hues, l = 65, c = 100)[1:n]
}
