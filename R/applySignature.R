#' Apply a feature signature on a matrix
#'
#' @param count.mat
#' Matrix with cols=Samples and rows=features
#' @param genesig
#' named numeric vector, Coefficients for a subset of features from count.mat
#' If a matrix is given,
#' @param interceptindex
#' If present, the index of the intercept in genesig
#' @param genesig.matrix.colN
#' If the given genesig is a matrix, which column should be taken
#' @param genesig.data.frame.cols_names
#' If the given genesig is a data frame, which column is the signature-feature-names
#' @param genesig.data.frame.cols_coefs
#' If the given genesig is a data frame, which column holds the signature-coefficients
#'
#' @return response named numeric vector, Holds the response for each sample
#' @export
#'
#' @examples
#' datamat <- matrix(1:20, nrow = 5)
#' colnames(datamat) <- paste0("S", 1:ncol(datamat))
#' rownames(datamat) <- paste0("F", 1:nrow(datamat))
#'
#' sig1 <- rep(2, nrow(datamat))
#' names(sig1) <- rownames(datamat)
#' applySignature(
#'     count.mat = datamat,
#'     genesig = sig1
#' )
#'
#'
#' sig2 <- as.matrix(sig1)
#' applySignature(
#'     count.mat = datamat,
#'     genesig = sig2
#' )
#'
#' sig3 <- cbind(sig1, -1 * sig1, 2 * sig1)
#' applySignature(
#'     count.mat = datamat,
#'     genesig = sig3,
#'     genesig.matrix.colN = 1
#' )
#' applySignature(
#'     count.mat = datamat,
#'     genesig = sig3,
#'     genesig.matrix.colN = 2
#' )
#'
#' sig4 <- as.data.frame(sig3)
#' applySignature(
#'     count.mat = datamat,
#'     genesig = sig4,
#'     genesig.data.frame.cols_names = 0,
#'     genesig.data.frame.cols_coefs = 1
#' )
#' applySignature(
#'     count.mat = datamat,
#'     genesig = sig4,
#'     genesig.data.frame.cols_names = 0,
#'     genesig.data.frame.cols_coefs = 2
#' )
#' sig5 <- sig4
#' sig5$names <- rownames(sig5)
#' rownames(sig5) <- NULL
#' applySignature(
#'     count.mat = datamat,
#'     genesig = sig5,
#'     genesig.data.frame.cols_names = 4,
#'     genesig.data.frame.cols_coefs = 2
#' )
#' applySignature(
#'     count.mat = datamat,
#'     genesig = sig5,
#'     genesig.data.frame.cols_names = "names",
#'     genesig.data.frame.cols_coefs = 2
#' )
#' applySignature(
#'     count.mat = datamat,
#'     genesig = sig5,
#'     genesig.data.frame.cols_names = 4,
#'     genesig.data.frame.cols_coefs = "V2"
#' )
applySignature <- function(count.mat,
                           genesig,
                           interceptindex = NA,
                           genesig.matrix.colN = 1,
                           genesig.data.frame.cols_names = 1,
                           genesig.data.frame.cols_coefs = 2) {
    if (length(genesig) == 0) {
        warning("You supplied an empty signature! return NAs for all samples")
        response <- 1:ncol(count.mat) * NA
        names(response) <- colnames(count.mat)
        return(response)
    }
    saved_genesig <- genesig
    if (is.matrix(saved_genesig)) {
        genesig <- saved_genesig[, genesig.matrix.colN]
        names(genesig) <- rownames(saved_genesig)
    }
    if (is.data.frame(saved_genesig)) {
        genesig <- saved_genesig[, genesig.data.frame.cols_coefs]
        if (genesig.data.frame.cols_names == 0) {
            names(genesig) <- rownames(saved_genesig)
        } else {
            names(genesig) <- saved_genesig[, genesig.data.frame.cols_names]
        }
    }

    if (!is.na(interceptindex)) { # extract intercept if index is given
        intercept <- genesig[interceptindex]
        genesig <- genesig[-interceptindex]
    }
    if (is.null(names(genesig))) {
        stop("names(genesig) are Null? They should not be!")
    }

    # check if all genes of genesig are present
    if (!all(names(genesig) %in% rownames(count.mat))) {
        stop("Not all genes of the genesig are in the given countmatrix!")
    }

    # restrict on genesig-genes
    count.mat <- count.mat[names(genesig), , drop = FALSE]

    # apply genesignature (afterwards: for each sample 1 value)
    response <- 1:ncol(count.mat) * 0
    for (sampleN in 1:ncol(count.mat)) {
        for (geneX in names(genesig)) {

            # eventually take care of NaN of count.mat
            if (!is.nan(count.mat[geneX, sampleN])) {
                response[sampleN] <- response[sampleN] + count.mat[geneX, sampleN] * genesig[geneX]
            }
        }
        if (!is.na(interceptindex)) {
            response[sampleN] <- response[sampleN] + intercept
        }
    }
    names(response) <- colnames(count.mat)
    return(response)
}
