#' create a directory
#'
#' Create a directory without the (possible) warning that the directory already
#' exists. All other warnings are reported
#'
#' @param dirpath
#' The directory to create
#' @param ...
#' Further given to dir.create()
#'
#' @return
#' Nothing, directory is created
#'
#' @export
#'
#' @examples
#' dir.create_noExistWarn("removeme")
#' dir.create_noExistWarn("removeme1/sfdg/a")
dir.create_noExistWarn <- function(dirpath, ...) {
    withCallingHandlers(
        expr = {
            dir.create(dirpath, ...)
        },
        warning = function(w) {
            if (endsWith(conditionMessage(w), "already exists")) {
                invokeRestart("muffleWarning")
            }
        }
    )
}