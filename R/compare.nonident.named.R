

#' cbind nonidentical named vectors
#'
#' cbind into a data frame vectors with nonidentical names.
#'
#' @param ...
#' A (named) concatenation of named vectors or matrices which will then be ordered in a data frame
#' and the values for the matching names are next to each other in the result
#'
#' @return
#' data.frame where each column is a single given (named) element of ..., each row
#' a (potentially missing in some elements) name of a coefficient.
#'
#' @export
#'
#' @examples
#'
#' vals1 <- 1:10
#' names(vals1) <- LETTERS[vals1]
#' vals2 <- 5:12
#' names(vals2) <- LETTERS[vals2]
#' compare.nonident.named(tmp=vals1, vals2)
#' # the following could not work as the rownames are missing!
#' # compare.nonident.named(tmp=matrix(1:10, ncol=2))
#' tmpmat <- matrix(1:10, ncol=2)
#' rownames(tmpmat) <- LETTERS[1:nrow(tmpmat)]
#' compare.nonident.named(tmp=tmpmat)
#' compare.nonident.named(tmpmat)
#' compare.nonident.named(tmpmat, vals2)

compare.nonident.named <- function(...){
	valuelist <- list(...)
	clean.valuelist <- list()
	cleaned.values.counter <- 1
	for(named.N in 1:length(valuelist)){
		tmpname <- names(valuelist)[named.N]
		if(is.null(tmpname) || tmpname == "")
			tmpname <- paste0("NoNameGiven.", named.N)
		if(is.matrix(valuelist[[named.N]])){
			for(colN in 1:ncol(valuelist[[named.N]])){
				tmp <- valuelist[[named.N]][, colN]
				names(tmp) <- rownames(valuelist[[named.N]])
				clean.valuelist <- c(clean.valuelist, list(tmp))
				if(all(is.null(colnames(valuelist[[named.N]])))){
					colnames(valuelist[[named.N]]) <- paste0("col.", 1:ncol(valuelist[[named.N]]))
				}

				names(clean.valuelist)[length(clean.valuelist)] <- paste0(tmpname, "_", colnames(valuelist[[named.N]])[colN])
			}
			# different approach: if it is a matrix, use all of the columns as comparisons - maybe even named.
		}else{
			tmp <- valuelist[[named.N]]
			clean.valuelist <- c(clean.valuelist, list(tmp))
			names(clean.valuelist)[length(clean.valuelist)] <- tmpname
		}

	}

	for(clean.N in 1:length(clean.valuelist)){
		if(is.null(names(clean.valuelist[[clean.N]])))
			stop(paste0("clean.valuelist element ", clean.N, " has no names!"))
	}

	uniquenames <- unique(unlist(lapply(clean.valuelist, names)))
	uniquenames <- sort(uniquenames)
	coef.df <- data.frame(lapply(clean.valuelist, function(x)x[uniquenames]))
	rownames(coef.df) <- uniquenames
	return(coef.df)
}
