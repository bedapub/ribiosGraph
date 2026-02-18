#' Convert a list of character strings into an incidence matrix
#' 
#' @param list A list of character strings, can be unique or redundant
#' @param type How the values of the incidence matrix will be filled, see details.
#' @return An incidence matrix, containing either binary (\code{TRUE/FALSE}) or integer values.
#'
#' Type 'binary' will produce a logical matrix, whereas 'count' will produce a matrix
#' where the frequency of the character strings in the list.
#'
#' @examples
#' wordList <- list("2006"=c("HSV", "BVB", "FCB"), 
#'   "2007"=c("BVB", "VFB", "STP"), 
#'   "2008"=c("VFL", "BVB", "HSV"))
#' list2incidenceMatrix(wordList, type="binary")
#' 
#' letterList <- list("First"=c("A", "a", "A", "a"), "Second"=c("B", "b", "A"))
#' list2incidenceMatrix(letterList, type="count")
#' list2incidenceMatrix(letterList, type="binary")
#'
#' @author Jitao David Zhang, \email{jitao_david.zhang@roche.com}
#' @importFrom ribiosUtils munion mmatch
#' @export
list2incidenceMatrix <- function(list, type=c("binary", "count")) {
  type <- match.arg(type)
  uitems <- munion(list)
  if(type=="binary") {
    res <- sapply(list, function(x) uitems %in% x)
  } else if (type=="count") {
    res <- sapply(list, function(x) sapply(mmatch(uitems, x), function(x) length(x[!is.na(x)])))
  }
  rownames(res) <- uitems
  return(res)
}
