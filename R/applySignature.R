
#' Apply a feature signature on a matrix
#'
#' @param count.mat Matrix with cols=Samples and rows=features
#' @param genesig00 named numeric vector, Coefficients for a subset of features from count.mat
#' @param interceptindex If present, the index of the intercept in genesig00
#'
#' @return response named numeric vector, Holds the response for each sample
#' @export
#'
#' @examples
applySignature <- function(count.mat
						   ,genesig00
						   ,interceptindex=NA){
	if(length(genesig00) == 0){
		warning("You supplied an empty signature! return NAs for all samples")
		response <- 1:ncol(count.mat) * NA
		names(response) <- colnames(count.mat)
		return(response)
	}
	if(!is.na(interceptindex)){		# extract intercept if index is given
		intercept <- genesig00[interceptindex]
		genesig00 <- genesig00[-interceptindex]
	}
	if(is.null(names(genesig00))){
		stop("names(genesig00) are Null? They should not be!")
	}
	
	# check if all genes of genesig00 are present
	if(! all(names(genesig00) %in% rownames(count.mat))){
		stop("Not all genes of the genesig are in the given countmatrix!")
	}
	
	# restrict on genesig00-genes
	count.mat <- count.mat[names(genesig00), ,drop=FALSE]
	
	# apply genesignature (afterwards: for each sample 1 value)
	response <- 1:ncol(count.mat) * 0
	for(sampleN in 1:ncol(count.mat)){
		for(geneX in names(genesig00)){
			
			#eventually take care of NaN of count.mat
			if(! is.nan(count.mat[geneX, sampleN])){
				response[sampleN] <- response[sampleN] + count.mat[geneX, sampleN] * genesig00[geneX]
			}
		}
		if(!is.na(interceptindex)){
			response[sampleN] <- response[sampleN] + intercept
		}
	}
	names(response) <- colnames(count.mat)
	return(response)
	
}