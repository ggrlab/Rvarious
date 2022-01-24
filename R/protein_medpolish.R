#' Median polish to summarize proteins
#'
#' Median polish to summarize proteins from multiple peptides
#'
#' @param peptidematrix
#' A matrix with rows=peptides and cols=samples
#' @param peptide_protein_affiliation
#' A vector of length nrow(peptidematrix) which holds the protein-names of the
#' corresponding peptide in the peptidematrix
#' @param previous_MP_params
#' a named list with "featurefactors" and "overalleffects" given.
#' Can also be directly a previous returnvalue of this function
#'
#' @param overall_Included
#' should the overall effect be included in the final protein values?
#' @param na.rm
#' Should NAs be removed?
#' @param return.completeMP
#' Should the median polish result per protein be returned
#' @return
#' A named list
#' \describe{
#' \item{protein.values}{proteinmat}
#' \item{featurefactors}{featurefactors}
#' \item{overalleffects}{overalleffects)}
#' }
#' @export
#'
#' @examples
#' ncol <- 10
#' nrow <- 15
#' examplemat <- matrix(rnorm(ncol * nrow), ncol = ncol, nrow = nrow)
#' rownames(examplemat) <- paste0("peptide", 1:nrow)
#' colnames(examplemat) <- paste0("sample", 1:ncol)
#' protein_affiliations <- c(
#'     rep("Protein1", 5),
#'     rep("Protein2", 3),
#'     rep("Protein3", 2),
#'     rep("Protein4", 5)
#' )
#' tmp <- protein_medpolish(
#'     peptidematrix = examplemat,
#'     peptide_protein_affiliation = protein_affiliations,
#'     previous_MP_params = NULL,
#'     overall_Included = TRUE,
#'     na.rm = TRUE
#' )
#' tmp$protein.values
#' tmp2 <- protein_medpolish(
#'     peptidematrix = examplemat,
#'     peptide_protein_affiliation = protein_affiliations,
#'     previous_MP_params = tmp,
#'     overall_Included = TRUE,
#'     na.rm = TRUE
#' )
#' tmp2$protein.values
#' summary(c(tmp$protein.values - tmp2$protein.values)) # almost zero, done.
#'
#'
#'
#' tmp <- protein_medpolish(
#'     peptidematrix = examplemat,
#'     peptide_protein_affiliation = protein_affiliations,
#'     previous_MP_params = NULL,
#'     overall_Included = TRUE,
#'     na.rm = TRUE,
#'     return.completeMP = TRUE
#' )
protein_medpolish <- function(peptidematrix,
                              peptide_protein_affiliation,
                              previous_MP_params = NULL,
                              overall_Included = TRUE,
                              na.rm = TRUE,
                              return.completeMP = FALSE) {
    if (is.null(previous_MP_params)) { # then I have to do the medianpolish from scratch
        unique.proteins <- unique(peptide_protein_affiliation)
        names(unique.proteins) <- unique.proteins
        proteinwise_mp <- lapply(unique.proteins, function(protX) {
            protpart_mat <- peptidematrix[peptide_protein_affiliation == protX, , drop = FALSE]
            medpol <- medpolish(protpart_mat, na.rm = na.rm, trace.iter = FALSE)
            if (overall_Included) {
                ret_protein_i <- medpol$col + medpol$overall
            } else {
                ret_protein_i <- medpol$col
            }
            return(list(
                "protein.values" = ret_protein_i,
                "medpol" = medpol
            ))
        })
        if (return.completeMP) {
            medpol.list <- lapply(proteinwise_mp, function(x) x$medpol)
        }
        # for each protein, there should be the number of samples return values.
        proteinmat <- vapply(proteinwise_mp, function(x) x[["protein.values"]],
            FUN.VALUE = numeric(ncol(peptidematrix))
        )

        overalleffects <- unlist(lapply(proteinwise_mp, function(x) x[["medpol"]][["overall"]]))
        # the following removing of the names of "proteinwise_mp"
        # is used to remove the protein names of the featurefactors before they
        # are even created by lapply.
        # Else the names of featurefactors would be something like
        # 	protein_name.peptide_name
        # but I want only
        # 	peptide_name
        # because no peptide is in two proteins. So from each protein there comes
        # one row-factor for all of its own peptides
        names(proteinwise_mp) <- NULL
        featurefactors <- unlist(lapply(proteinwise_mp, function(x) x[["medpol"]][["row"]]))
    } else {
        if (!all(c("featurefactors", "overalleffects") %in% names(previous_MP_params))) {
            stop("previous_MP_params must be a named list with \"featurefactors\" and \"overalleffects\" given!")
        }
        featurefactors <- previous_MP_params[["featurefactors"]]
        overalleffects <- previous_MP_params[["overalleffects"]]
        if (!identical(names(featurefactors), rownames(peptidematrix))) {
            stop("Rownames of \"peptidematrix\" are different to the names of \"featurefactors\"")
        }
        if (!all(unique(peptide_protein_affiliation) %in% names(overalleffects))) {
            stop("Not all unique names of \"peptide_protein_affiliation\" are inside \"overalleffects\"")
        }
        peptidematrix_without_featurefactors <- sweep(peptidematrix, 1, featurefactors, "-")
        unique.proteins <- unique(peptide_protein_affiliation)
        names(unique.proteins) <- unique.proteins
        proteinmat <- vapply(unique.proteins, function(protX) {
            protpart_mat <- peptidematrix_without_featurefactors[peptide_protein_affiliation == protX, , drop = FALSE]
            if (overall_Included) {
                ret_protein_i <- apply(protpart_mat, 2, median, na.rm = na.rm)
            } else {
                ret_protein_i <- apply(protpart_mat, 2, median, na.rm = na.rm) - overalleffects[protX]
            }
        }, FUN.VALUE = numeric(ncol(peptidematrix)))
    }
    # just to check: If previous_MP_params=NULL and the medianpolished is done
    # then the second if-part is done (per hand)
    # 	summary(c(proteinmat2 - proteinmat))
    # results in 0 +- 1e-14
    # therefore it works.

    retval <- list(
        "protein.values" = t(proteinmat) # transpose to have samples as columns (as the given peptidematrix)
        , "featurefactors" = featurefactors,
        "overalleffects" = overalleffects
    )
    if (return.completeMP) {
        retval <- c(retval, MP.per.protein = list(medpol.list))
    }
    return(retval)
}