#' Create a bipartite graph from data frame
#' @param x A \code{data.frame} with at least two columns.
#' 
#' @return A \code{igraph} object that represents a bipartite graph. The \code{type} attribute of vertices is a logical vector that represents the two classes of nodes: nodes in the first column in the input \code{data.frame} are of the type \code{TRUE}, and those in the second column are of the type \code{FALSE}.
#' 
#' Extra columns besides the first two are used as edge attribtues. See example below.
#'
#' @importFrom igraph graph.empty add.vertices add.edges is.bipartite E
#' @export
#' @examples 
#' myDataFrame <- data.frame(Alpha=c("A", "E", "O", "U", "B", "D"), 
#'   Type=c("Vowel", "Vowel", "Vowel", "Vowel", "Consonance", "Consonance"))
#' myBpGraph <- bipartite_graph_from_data_frame(myDataFrame)
#' 
#' myDataFrame2 <- data.frame(Alpha=c("A", "E", "O", "U", "B", "D"), 
#'   Type=c("Vowel", "Vowel", "Vowel", "Vowel", "Consonance", "Consonance"),
#'   Example=c("BAT", "BED", "BOT", "BUT", "DUB", "DUB"))
#' myBpGraph2 <- bipartite_graph_from_data_frame(myDataFrame2)
#' igraph::E(myBpGraph2)$Example

bipartite_graph_from_data_frame <- function(x) {
  node1 <- unique(x[, 1L])
  node2 <- unique(x[, 2L])
  
  edgeListVec <- as.vector(t(as.matrix(data.frame(S1=as.character(x[, 1L]),
                                                  S2=as.character(x[, 2L])))))
  
  g <- igraph::graph.empty()
  g <- igraph::add.vertices(g,nv=length(node1),attr=list(name=as.character(node1),
                                                         type=rep(TRUE,length(node1))))
  g <- igraph::add.vertices(g,nv=length(node2),attr=list(name=as.character(node2),
                                                         type=rep(FALSE,length(node2))))
  
  attrs <- list()
  if (ncol(x) > 2) {
    for (i in 3:ncol(x)) {
      newval <- x[, i]
      if (inherits(newval, "factor")) {
        newval <- as.character(newval)
      }
      attrs[[names(x)[i]]] <- newval
    }
  }
  
  # we need to turn edgeList into a vector (and using names instead of indexes)
  g <- igraph::add.edges(g, edgeListVec, attr=attrs)
  
  # check if is recognized as bipartite
  stopifnot(igraph::is.bipartite(g))
  return(g)
}

#' Layout a bipartite graph from left to right
#' @param g A \code{igraph} object
#' @return A two-column matrix, the layout of the graph
#' 
#' The function simply calls \code{\link[igraph]{layout_as_bipartite}} and reverses the \code{X} and \code{Y} coordinates.
#' @importFrom igraph layout_as_bipartite
#' @export
#' @examples 
#' myDataFrame <- data.frame(Alpha=c("A", "E", "O", "U", "B", "D"), 
#'   Type=c("Vowel", "Vowel", "Vowel", "Vowel", "Consonance", "Consonance"))
#' myBpGraph <- bipartite_graph_from_data_frame(myDataFrame)
#' myLayout <- layout_as_bipartiteLR(myBpGraph)
layout_as_bipartiteLR <- function(g) {
  return(igraph::layout_as_bipartite(g)[,c(2,1)])
}

#' Plot a bipartite graph using plot_ly
#' @param g A \code{igraph} object of a bipartite graph
#' @param layout The layout, the LR layout is used by default
#' @param edge.line List, specifying edge lines
#' @param axis List, specifying axes
#' @param title Character string, plot title
#' 
#' @return A \code{plotly} and \code{htmlwidget} object
#' 
#' If the layout is left-right, the function takes care of the alignment of labels
#' 
#' @importFrom magrittr %>%
#' @importFrom igraph V get.edgelist
#' @importFrom plotly plot_ly add_annotations layout
#' @export
#' 
#' @examples 
#' myDataFrame <- data.frame(word=c("ja", "nein", "yes", "no", "stark", "stark"),
#'   language=c("German", "German", "English", "English", "English", "German"))
#' myBpGraph <- bipartite_graph_from_data_frame(myDataFrame)
#' plotlyBipartiteGraph(myBpGraph)
plotlyBipartiteGraph <- function(g, layout=layout_as_bipartiteLR(g),
                                 edge.line=list(color="#030303", width=0.3),
                                 axis=list(title = "", showgrid = FALSE, 
                                           showticklabels = FALSE, zeroline = FALSE),
                                 title="") {
  vs <- igraph::V(g)
  vnames <- vs$name
  es <- as.data.frame(igraph::get.edgelist(g))
  
  Nv <- length(vs)
  Ne <- length(es[1]$V1)
  
  ## create nodes
  Xn <- layout[,1]
  Yn <- layout[,2]
  
  vsTrueLabel <- vs$name; vsTrueLabel[!vs$type] <- ""
  vsFalseLabel <- vs$name; vsFalseLabel[vs$type] <- ""
  network <- plotly::plot_ly(x = ~Xn, y = ~Yn, mode = "markers", 
                             text = vs$name, hoverinfo = "text", size=3,
                             type="scatter") %>%
    plotly::add_annotations(text = vsTrueLabel, inherit=FALSE, size=0.5, xshift=-5,
                            showarrow=FALSE, xanchor="right") %>%
    plotly::add_annotations(text = vsFalseLabel, inherit=FALSE, size=0.5,xshift=5,
                            showarrow=FALSE, xanchor="left")
  
  edge_shapes <- lapply(1:Ne, function(i) {
    v0 <- match(es$V1[i], vnames)
    v1 <- match(es$V2[i], vnames)
    
    res <- list(type = "line",
                line = edge.line,
                x0 = Xn[v0],
                y0 = Yn[v0],
                x1 = Xn[v1],
                y1 = Yn[v1])
    return(res)
  })
  
  p <- plotly::layout(network,
                      title = title,
                      shapes = edge_shapes,
                      xaxis = axis,
                      yaxis = axis)
  
  return(p)
}
