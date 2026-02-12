#' Build a bipartite graph with an incidence matrix
#' @param matrix An incidence matrix
#' @param size A vector of length 2, size of nodes in rows and in columns
#' @param color A vector of length 2, color of nodes in rows and in columns
#' @param label.cex A vector of length 2, font size of labels of nodes
#'   in rows and in columns
#' @param label.color A vector of length 2, color of labels of nodes
#'   in rows and in columns
#' @param V A named list of other node styles, each item of length 1 or 2.  In the latter case, the first value is used for nodes in rows and the second for nodes in columns
#' @param E A named list of edge styles. Each item must be length of 1.
#' @return An instance of \code{igraph} graph
#' @importFrom igraph graph_from_incidence_matrix V E `V<-` `E<-`
#' @examples
#' myIncMat <- matrix(c(0, 0, 1, 
#'   0, 1, 0,
#'   1, 0, 0,
#'   0, 1, 1,
#'   1, 1, 1),
#'   ncol=3, byrow=TRUE, dimnames=list(LETTERS[1:5], letters[1:3]))
#' myGraph <- incidence2bipartite(myIncMat,
#'   size=c(18,12),
#'   V=list(shape=c("rectangle", "circle"),
#'          frame.color="lightgray"))
#' if(requireNamespace("igraph")) {
#'   igraph::plot.igraph(myGraph)
#' }
#' @export
incidence2bipartite <- function(matrix,
                                size=c(12, 9),
                                color=c("orange", "lightblue"),
                                label.cex=c(1.1, 0.95),
                                label.color=c("black", "navyblue"),
                                V=list(),
                                E=list(color="black")) {
  graph <- igraph::graph_from_incidence_matrix(matrix)
  isRow <- V(graph)$name %in% rownames(matrix)
  V(graph)$size <- ifelse(isRow, size[1], size[2])
  V(graph)$color <- ifelse(isRow, color[1], color[2])
  V(graph)$label.cex <- ifelse(isRow, label.cex[1], label.cex[2])
  V(graph)$label.color <- ifelse(isRow, label.color[1], label.color[2])
  for(vattr in names(V)) {
    vval <- V[[vattr]]
    if(length(vval)==1) {
      V(graph) <- do.call(`$<-`, list(V(graph), vattr, vval))
    } else if (length(vval)==2) {
      V(graph) <- do.call(`$<-`, list(V(graph), vattr, ifelse(isRow, vval[1], vval[2])))
    } else {
      stop("Elements in 'V' must be of length 1 or 2.")
    }
  }
  for(eattr in names(E)) {
    E(graph) <- do.call(`$<-`, list(E(graph), eattr, E[[eattr]]))
  }
  return(graph)
}
