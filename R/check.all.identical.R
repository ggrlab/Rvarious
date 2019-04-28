#' Check if all elements are identical
#'
#' \enumerate{
#'   \item Apply func on all elements of "..."
#'   \item Check if all returned values of func are identical
#'   \item Return either TRUE or FALSE
#' }
#'
#' @param func
#' The function applied to all elements of "..."
#' @param ...
#'
#'
#' @return
#' TRUE if all elements in "..." are after application of func identical.
#'
#' @export
#'
#' @examples
#' check.all.identical(1, 1, "a", 1)
#' check.all.identical(1, 1, 1, 1)
#' check.all.identical("a", "a", "a", "a")
check.all.identical <- function(..., func=function(x){x}){
	tmplist <- list(...)
	# are.all.identical <- TRUE
	if(length(tmplist) < 2){
		warning("Less than 2 elements in ..., they are of course all the same")
	}else{
		applied.func <- lapply(tmplist, function(y){func(y)})
		first.val <- applied.func[[1]]
		for(indexN in 2:length(applied.func)){
			if(!identical(first.val, applied.func[[indexN]])){
				return(FALSE)
				# are.all.identical <- FALSE # this has the performance issue that even if it is already FALSE, it will still go through all elements in ...
			}
		}
	}
	return(TRUE)
	# return(are.all.identical)
}
