 #' Extract value from an html element
#' 
#' These functions have to be used in \code{\link{cssApply}} and 
#' \code{\link{html2df}}. They aim to facilitate extraction of
#'  different kind of value.
#'
#' \code{cssNumeric} extracts the numeric value of an element, 
#' \code{cssCharacter} extracts text. \code{cssLink} extracts the url of a link. 
#' \code{cssSrc}, \code{cssId}, \code{cssClass}, \code{cssName} and 
#' \code{cssValue} are less usefull and extract respectively the source of an
#' element, its id, its css class, its name and its value (may be usefull for
#' input elements).
#' 
#' @name cssExtract
#' @rdname cssExtract
#' @aliases cssLink
#' 
#' @param node 
#' An xml node
#' @param ...
#' Parameters passed to \code{\link{xmlValue}} 
#' 
#' @return All these function return a chracter string except \code{cssNumeric}
#' which returns a Numeric value
#' 
#' @examples
#' doc <- "<html>
#' <head></head>
#' <body>
#'   <div id='character1' class='character'>
#'     <span class='name'>Mike</span>
#'     <span class='level digit'>10</span>
#'     <a href='http://someurl.com'>Complete profile</a>
#'   </div>
#'   <div id='character2' class='character'>
#'     <span class='name'>Stan</span>
#'     <a href='http://someurl2.com'>Complete profile</a>
#'   </div>
#' </body>
#' </html>"
#'
#' doc <- htmlParse(doc)
#'
#' # Names of the characters
#' cssApply(doc, ".character>.name", cssCharacter)
#' 
#' # Name of character1
#' cssApply(doc, "#character1>.name", cssCharacter)
#' 
#' # Urls of the profiles
#' cssApply(doc, ".character>a", cssLink)
#' 
#' # Level of characters
#' cssApply(doc, ".character>.level", cssNumeric)
#' 
#' # character 2 does not have level, we would want to have a NA value instead of nothing
#' cssApplyInNodeSet(doc, ".character", ".level", cssNumeric)
cssLink <- function(node) {
  # Pour extraire des liens
  xmlGetAttr(node, name="href")
}

#' @rdname cssExtract
cssClass <- function(node) {
  xmlGetAttr(node, name="class")
}

#' @rdname cssExtract
cssId <- function(node) {
  xmlGetAttr(node, name="id")
}

#' @rdname cssExtract
cssSrc <- function(node) {
  xmlGetAttr(node, name="src")
}

#' @rdname cssExtract
cssValue <- function(node) {
  xmlGetAttr(node, name="value")
}

#' @rdname cssExtract
cssName <- function(node) {
  xmlGetAttr(node, name="name")
}

#' @rdname cssExtract
cssNumeric <- function(node, ...) {
  x <- xmlValue(node, ...)
  x <- gsub("[^\\d\\.]", "", x, perl=TRUE)
  x <- gsub("(^\\.+)|(\\.+$)", "", x)
  x <- as.numeric(x)
}

#' @rdname cssExtract
cssCharacter <- function(node, ...) {
  xmlValue(node, ...)
}