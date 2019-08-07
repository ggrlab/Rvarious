#' Title
#'
#'
#' Print which elements are in the first but not in the second vector. And the
#' other way aroung
#'
#' @param charvec.1
#' First character vector
#' @param charvec.2
#' Second character vector
#' @param name.1
#' The (optional) more descriptive name for character vector 1
#' @param name.2
#' The (optional) more descriptive name for character vector 2
#'
#' @return
#' NULL, but prints information
#' @export
#'
#' @examples
#' check_missing_names(charvec.1 = letters[1:5]
#' 					,charvec.2 = letters[4:6])
#' check_missing_names(charvec.1 = letters[1:5]
#' 					,charvec.2 = letters[4:6]
#' 					,name.1 = "firstLetters"
#' 					,name.2 = "secondLetters")


check_missing_names <- function(charvec.1, charvec.2, name.1, name.2){
	if(missing(name.1)){
		name.1 <- "charvec.1"
	}
	if(missing(name.2)){
		name.2 <- "charvec.2"
	}

	# include only those where phenodata from leipzig are available
	cat("In ", name.1, " but not in ", name.2, "\n")
	print(charvec.1[!charvec.1 %in% charvec.2])
	cat("In ", name.2, " but not in ", name.1, "\n")
	print(charvec.2[!charvec.2 %in% charvec.1])
	return(NULL)
}

