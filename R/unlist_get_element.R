#' Recursive unlisting for a specific element
#'
#' Retrieve a value (vector and matrix tested) from a list of list of list of ...
#' and return a dataframe.
#'
#' @param mylist
#' Your listlistlist... you want to delist
#' @param depth
#' How many list-hierarchies do you want to unlist?
#'
#' \itemize{
#' \item Example 1 - depth=1 :
#'
#' 	You have a list of CrossValidation-steps and want to have the coefficients
#' 	in each step
#'
#' \item Example 2 - depth=2 :
#'
#' 	You have a list of different Models with CrossValidation-steps and want to have the coefficients
#' 	from each model in each CV-step
#' }
#'
#'
#'
#'
#' @param which.element
#' How is the element you want called? "coefNoZero", "cutoff", etc.
#' @param save_depth
#' Do not touch this. This is just that you get a bit nicer output
#' (Not a list with the current depth but the data.frame you are interested in)
#'
#' @return
#' A data.frame
#' @export
#'
#' @examples
#' ##load("exampleData_unlist.rda")
#' a <- yaml::yaml.load_file("R/a.yaml")
#' b <- a
#' b$trainScaled <- b$train.res...p
#' b_2 <- b
#' names(b_2) <- NULL
#' unlist_get_element(a, 2, "cutoff")
#' unlist_get_element(b, 2, "cutoff")
#' unlist_get_element(b_2, 2, "cutoff")
#' ## unlist_get_element(different.norm.models, 1, "coefNOzero")
#'
#' # If there are arguments which are different then the rest:
#' b_modified <- b
#' b_modified$train.res...p$CVstep1$cutoff <- list("a", "b", "c")
#' unlist_get_element(b_modified, 2, "cutoff")
#'
#'
#' unlist_get_element(b, 3, "cutoff") # ideally i want to remove the depth

unlist_get_element <- function(mylist, which.element){
	matrix.result <- uge_first_occurence_matrix(mylist = mylist
												,which.element = which.element)
	return(interpret_unlist_get_element(matrix.result))
}

interpret_unlist_get_element <- function(uge.result){
	interpreted.df <- data.frame(stringsAsFactors = FALSE
							  ,apply(uge.result[, -ncol(uge.result)], 2, unlist))
	rownames(interpreted.df) <- NULL
	interpreted.df$values.list.format <- uge.result[, ncol(uge.result)]
	colnames(interpreted.df) <- colnames(uge.result)
	return(interpreted.df)
}


uge_first_occurence_matrix <- function(mylist, which.element, depth=0){
	# print(depth)
	if(which.element %in% names(mylist)){
		return(list("element"=mylist[[which.element]]
					, "found_which.element_obscure_name_that_it_is_not_wrongly_chosen"=TRUE))
	}else if(("list" %in% class(mylist)) && length(mylist) > 0) {
		if(is.null(names(mylist))){
			iteration_vec <- 1:length(mylist)
		}else{
			iteration_vec <- names(mylist)
		}
		# go through the current list elements
		for(nameX in iteration_vec){
			# one step deeper
			tmp.return <- uge_first_occurence_matrix(
				mylist[[nameX]]
				, which.element
				,depth=depth+1)
			# if the following is false, which.element was not found at all or,
			# 	because isTRUE => FALSE for isTRUE(NULL)
			# it could also be a return value from lower levels
			if(!isTRUE(tmp.return$found_which.element_obscure_name_that_it_is_not_wrongly_chosen)){
				new.element <- tmp.return
			}else{ # otherwise it was found and we are exactly one above the bottom
				if(length(tmp.return[[1]]) > 1){
					new.element <- list(tmp.return[[1]])
				}else{
					new.element <- tmp.return[[1]]
				}
			}
			# now, new.element is either a list with length 1 OR a single value
			new.withDepth <- cbind("new"=nameX, new.element)
			colnames(new.withDepth)[1] <- paste0("L", depth)
			if(!exists("tmp.df")){
				tmp.df <- new.withDepth
			}else{
				tmp.df <- rbind(tmp.df, new.withDepth)
			}
		}
		colnames(tmp.df)[ncol(tmp.df)] <- which.element
		rownames(tmp.df) <- NULL
		return(tmp.df)
	}else{ # Then it was not found
		return(list("found_which.element_obscure_name_that_it_is_not_wrongly_chosen"=FALSE))
	}
}

a <- yaml::yaml.load_file("R/a.yaml")
b <- a
b$trainScaled <- b$train.res...p
a_small <- a
a_small$train.res...p <- a_small$train.res...p[1:3]
print(unlist_get_element(a_small, "cutoff"))
unlist_get_element(a, "cutoff")
unlist_get_element(b, "cutoff")

b_modified <- b
b_modified$train.res...p$CVstep1$cutoff <- list("a", "b", "c")
unlist_get_element(b_modified, "cutoff")

# one list here is deeper than
a_different_depths <- a_small
a_different_depths$train.res...p$CVstep1$asdflkjh$cutoff <- "sads"
a_different_depths$train.res...p$CVstep1$cutoff <- NULL
print(unlist_get_element(a_different_depths, "cutoff"))

