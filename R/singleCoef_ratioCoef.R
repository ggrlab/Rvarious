#' Title
#'
#' @param named.ratio_coefmat
#' @param splitchar
#'
#' @return
#' @export
#'
#' @examples
#' ratio.signature <- c("longname.b_/_longname.a"=1, "longname.c_/_longname.b"=-1)
#' single.coef_signature <- singleCoef_ratioCoef(ratio.signature)
#' print(single.coef_signature)
singleCoef_ratioCoef <- function(named.ratio_coefmat, splitchar="_/_"){
	if(!is.matrix(named.ratio_coefmat)){
		named.ratio_coefmat <- as.matrix(named.ratio_coefmat)
	}
	# save rownames
	rn.ratios <- rownames(named.ratio_coefmat)

	# create matrix for (unique) single coefficients
	rn.single <- strsplit(rn.ratios, splitchar)
	names(rn.single) <- rn.ratios
	single.coefnames <- unique(unlist(rn.single))
	single.coefmat <- matrix(0, ncol=1, nrow=length(single.coefnames))
	rownames(single.coefmat) <- single.coefnames

	# go through all rows of the named.ratio_coefmat
	newmat <- cbind(named.ratio_coefmat, named.ratio_coefmat)
	for(rowN in 1:nrow(newmat)){
		tmp_rn <- rownames(newmat)[rowN]
		split_features <- strsplit(tmp_rn, splitchar)[[1]] # split the ratios into single features
		for(featureN in 1:length(split_features)){
			# newmat[rowN, featureN] # depending on which feature it is, either add or subtract the coefficient
			if(featureN == 1){
				single.coefmat[split_features[featureN], 1] <-
					single.coefmat[split_features[featureN], 1] + newmat[rowN, featureN]
			}else if(featureN == 2){
				single.coefmat[split_features[featureN], 1] <-
					single.coefmat[split_features[featureN], 1] - newmat[rowN, featureN]
			}else{
				stop("What happened? There should be at most two features (We have ratios you know!)")
			}
		}

	}
	return(single.coefmat)
}
