#' Recursive unlisting for a specific element
#'
#' Retrieve a value (vector and matrix tested) from a list of list of list of ...
#' and return a dataframe.
#'
#' @param mylist
#' Your listlistlist... you want to delist
#' @param which.element
#' How is the element you want called? "coefNoZero", "cutoff", etc.
#' @param simplify
#' [default: TRUE] Try to make usefull stuff out of the last column of the interpreted dataframe
#' @param get.names
#' [default: FALSE] Should names be extracted?
#'
#' @return
#' A data.frame, if not simplified with a list as last-column elements per row
#' @export
#'
#' @examples
#'
#' a <- yaml::yaml.load_file("R/a.yaml")
#' b <- a
#' b$trainScaled <- b$train.res...p
#' a_small <- a
#' a_small$train.res...p <- a_small$train.res...p[19:20]
#' tmp <- unlist_get_element(a_small, "cutoff")
#' tmp <- unlist_get_element(a, "cutoff")
#' tmp <- unlist_get_element(b, "cutoff")
#'
#'
#' b_modified <- b
#' b_modified$train.res...p$CVstep1$cutoff <- list("a", "b", "c")
#' unlist_get_element(b_modified, "cutoff", simplify = TRUE)
#'
#' # first list here is deeper than
#' a_different_depths <- a_small
#' a_different_depths$train.res...p$CVstep1$asdflkjh$cutoff <- "sads"
#' a_different_depths$train.res...p$CVstep1$cutoff <- NULL
#' print(unlist_get_element(a_different_depths, "cutoff"))
#' # second list here is deeper than
#' a_different_depths <- a_small
#' a_different_depths$train.res...p$CVstep2$asdflkjh$cutoff <- "sads"
#' a_different_depths$train.res...p$CVstep2$cutoff <- NULL
#' print(unlist_get_element(a_different_depths, "cutoff"))
#' print(unlist_get_element(a_different_depths, "cutoff", simplify = FALSE))
#'
#'
#' # one element is a 1-col matrix
#' a_different_depths <- a_small
#' a_different_depths$train.res...p$CVstep2$asdflkjh$cutoff <- matrix(1:4, ncol=1)
#' a_different_depths$train.res...p$CVstep2$cutoff <- NULL
#' print(unlist_get_element(a_different_depths, "cutoff"))
#'
#' # one element is a 1-row matrix # error
#' a_different_depths <- a_small
#' a_different_depths$train.res...p$CVstep2$asdflkjh$cutoff <- matrix(1:4, nrow=1)
#' a_different_depths$train.res...p$CVstep2$cutoff <- NULL
#' print(unlist_get_element(a_different_depths, "cutoff"))
#'
#'
#' # one element is a 1-row matrix # error
#' a_different_depths <- a_small
#' a_different_depths$train.res...p$CVstep2$asdflkjh$cutoff <- matrix(1:4, ncol=1)
#' rownames(a_different_depths$train.res...p$CVstep2$asdflkjh$cutoff) <- letters[1:4]
#' a_different_depths$train.res...p$CVstep2$cutoff <- NULL
#' print(unlist_get_element(a_different_depths, "cutoff"))



unlist_get_element <- function(mylist, which.element, simplify=TRUE, get.names=FALSE
							   ,max.depth=Inf){
	matrix.result <- uge_first_occurence_matrix(mylist = mylist
												,which.element = which.element
												,max.depth=max.depth)
	# print(matrix.result)
	return(interpret_unlist_get_element(matrix.result[[1]], simplify = simplify
										,get.names=get.names))
}

interpret_unlist_get_element <- function(uge.result, simplify=TRUE, get.names=FALSE){
	interpreted.df <- data.frame(stringsAsFactors = FALSE
								 ,apply(uge.result[, -ncol(uge.result)], 2, unlist))
	rownames(interpreted.df) <- NULL
	interpreted.df$values.list.format <- uge.result[, ncol(uge.result)]
	colnames(interpreted.df) <- colnames(uge.result)

	if(simplify){
		interpreted.df <- simplify_results(interpreted.df, get.names)
	}
	return(interpreted.df)
}

#' simplify matrix of lists
#'
#' Matrix from interpret_unlist_get_element. The last column holds a list of the searched values.
#' Often, those values can be simplified!
#'
#' Each row is gone through and duplicated the number of elements in the last column.
#' (Remark 2019-12-16: could be problematic with arrays)
#'
#' @param interpreted.df
#'
#' Matrix from interpret_unlist_get_element. The last column holds a list of the searched values.
#'
#' @return
#' A simplified version of interpreted.df
simplify_results <- function(interpreted.df, get.names=FALSE){
	duplicated.rows.list <- apply(interpreted.df, 1, function(x){
		len.list <- length(x[[length(x)]])
		duplicated.rows <- matrix(unlist(x[-length(x)])
								  ,nrow=len.list, ncol=length(x)-1
								  ,byrow = TRUE)
		colnames(duplicated.rows) <- names(x[-length(x)])

		duplicated.rows <- data.frame(duplicated.rows, stringsAsFactors = FALSE)
		duplicated.rows$vals <- x[[length(x)]]
		colnames(duplicated.rows)[length(duplicated.rows)] <- names(x)[length(x)]
		if(get.names){
			if(!is.null(names(x[[length(x)]]))){
				duplicated.rows$name <- names(x[[length(x)]])
			}else if(!is.null(rownames(x[[length(x)]]))){
				duplicated.rows$name <- rownames(x[[length(x)]])
			}else{
				duplicated.rows$name <- paste0("placeholder_", 1:nrow(duplicated.rows))
			}
		}
		duplicated.rows
	})
	return(do.call(rbind, duplicated.rows.list))
}




# for "unlist_get_element_first_occurence_matrix"
uge_first_occurence_matrix <- function(mylist, which.element
									   , depth=0, max.depth=Inf){
	not.found.element <- list("element"=list(NA)
							  , "found_which.element"=FALSE)
	if(which.element %in% names(mylist)){
		retval <- mylist[[which.element]]
		if(all(is.na(retval))){
			retval <- as.numeric(mylist[[which.element]])
		}
		return(list("element"= retval
					, "found_which.element"=TRUE))
	}else if(max.depth < depth){
		return(not.found.element)
	}else if(("list" %in% class(mylist)) && length(mylist) > 0) {
		if(is.null(names(mylist))){
			iteration_vec <- 1:length(mylist)
		}else{
			iteration_vec <- names(mylist)
		}
		tmp.df_was_created <- FALSE
		# go through the current list elements
		for(nameX in iteration_vec){
			# one step deeper
			tmp.return <- uge_first_occurence_matrix(
				mylist[[nameX]]
				, which.element
				,depth=depth+1
				,max.depth=max.depth)
			return.found <- tmp.return[[2]]
			return.values <- tmp.return[[1]]
			# if the following is false, which.element was not found at all or,
			# 	because isTRUE => FALSE for isTRUE(NULL)
			# it could also be a return value from lower levels
			if(!return.found){
				new.element <- return.values
			}else{ # otherwise it was found and we are exactly one above the bottom
				# if(length(return.values) > 1){
				# 	new.element <- list(return.values)
				# }else{
				# 	new.element <- return.values
				# }
				new.element <- list(return.values)
			}
			# now, new.element is either a list with length 1 OR a single value
			new.withDepth <- cbind("new"=nameX, new.element)
			colnames(new.withDepth)[1] <- paste0("L", depth)
			if(!tmp.df_was_created){
				tmp.df <- new.withDepth
				tmp.df_was_created <- TRUE
			}else{
				if(ncol(tmp.df) == ncol(new.withDepth)){
					tmp.df <- rbind(tmp.df, new.withDepth)
				}else{
					if(ncol(tmp.df) > ncol(new.withDepth)){
						big.df <- tmp.df
						small.df <- new.withDepth
						big.or.small.first <- "big"
					}else{
						big.df <- new.withDepth
						small.df <- tmp.df
						big.or.small.first <- "small"
					}
					removed.element <- small.df[, -ncol(small.df), drop=FALSE]
					small.filled <- cbind(
						removed.element
						,matrix(NA, nrow=nrow(small.df)
								, ncol = ncol(big.df) - ncol(small.df))
						,small.df[, ncol(small.df), drop=FALSE])
					if(big.or.small.first == "big"){
						tmp.df <- rbind(big.df, small.filled)
					}else{
						tmp.df <- rbind(small.filled, big.df)
					}
					colnames(tmp.df) <- colnames(big.df)
				}
			}
		}
		colnames(tmp.df)[ncol(tmp.df)] <- which.element
		rownames(tmp.df) <- NULL
		return(list("element"=tmp.df
					, "found_which.element"=FALSE))
	}else{ # Then it was not found
		return(not.found.element)
	}
}

