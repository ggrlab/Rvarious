

#' Generate a network from a rpart-tree
#'
#' As a starting point for visualization, this function generates
#' "from" and "to" indices.
#'
#' @param rpart.obj
#' A rpart object from rpart::rpart()
#'
#' @return
#' A data frame with
#' #'
#'
#' 	- 'from'                # index
#' 	- 'to'                  # index
#' 	- 'node_numbers_from'   # node number in concordance with print.rpart
#' 	- 'node_numbers_to'     # node number in concordance with print.rpart

#'
#' @export
#'
#' @examples
#' fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#' graph_from_rpart(fit)
#'
#' # # example for igraph
#' # routes_igraph <- igraph::graph_from_data_frame(
#' # 	d = na.omit(generated_edges),
#' # 	vertices = data.frame('id'=1:length(depth_vec)),
#' # 	directed = TRUE)

graph_from_rpart <- function(rpart.obj){
	node <- as.numeric(row.names(rpart.obj$frame))
	depth_vec <- rpart:::tree.depth(node)

	edges_from <- c()
	edges_to <- c()
	for(nodeN in 1:length(depth_vec)){
		next_same_depth_index <- which(depth_vec[(nodeN+1):length(depth_vec)] == depth_vec[nodeN])
		if(length(next_same_depth_index) > 0){
			next_same_depth_index <- next_same_depth_index[1] + nodeN
		}else{
			next_same_depth_index <- length(depth_vec)+1
		}

		possible_indices <- (nodeN+1):(next_same_depth_index - 1)
		new_edges_to <- possible_indices[(depth_vec[nodeN]+1) == depth_vec[possible_indices]]
		new_edges_from <- rep(nodeN, length(new_edges_to))

		edges_to <- c(edges_to, new_edges_to)
		edges_from <- c(edges_from, new_edges_from)
	}

	generated_edges <- data.frame(
		'from'=edges_from,
		'to'=edges_to,
		'node_numbers_from'=node[edges_from],
		'node_numbers_to'=node[edges_to])


	return(generated_edges)
}
