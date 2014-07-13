#' Apply a function to elements identified by a CSS path.
#' 
#' This function is a wrapper function for \code{\link{xpathSApply}}. It 
#' selects nodes inside an html document corresponding to a CSS path and then
#' applies an arbitrary function to theses nodes. It is useful to extract data
#' frome an html page.
#' 
#' @param doc
#' An html document parsed with \code{\link{htmlParse}}.
#' @param path
#' CSS path
#' @param fun
#' Function to extract data from the selected nodes. See \code{\link{cssExtract}}
#' @param ...
#' Parameters passed to "\code{fun}"
#' 
#' @return The result may differ based on the extarction function used. It will
#'  generally be a character or numeric vector with length equal to the number
#'   of nodes selected.
#' 
#' If no element in the html document corresponds to the path provided, the
#' function will return an empty list.
#'  
#' @examples
#' doc <- "<html>
#' <head></head>
#'   <body>
#'   <div id='character1' class='character'>
#'   <span class='name'>Mike</span>
#'   <span class='level digit'>10</span>
#'   </div>
#'   <div id='character2' class='character'>
#'   <span class='name'>Stan</span>
#'   </div>
#'   </body>
#'   </html>"
#' 
#' doc <- htmlParse(doc)
#' 
#' # Names of the characters
#' cssApply(doc, ".character>.name", cssCharacter)
#' 
#' # Name of character1
#' cssApply(doc, "#character1>.name", cssCharacter)
#' 
cssApply <- function(doc, path, fun, ...) {
  path <- cssToXpath(path)
  xpathSApply(doc, path, fun, ...)
}

#' Find a set of elements and inside each of them apply a function to some
#' element.
#' 
#' Consider the following case : on an html page you have information about 
#' several people. Several informations are included in a div of class "people", 
#' but for some people some information is missing. 
#'
#' With this function, you can first select all divs of class "people" and then 
#' search inside them if the information is available. If not, the function will 
#' return a NA value for the person.
#' 
#' @param doc
#' An html document parsed with \code{\link{htmlParse}}.
#' @param path
#' Character. It CSS path used to identify elements where to search the 
#' information
#' @param relPath
#' Character. CSS path used to select elements in the elements selected with 
#' "path"
#' @param fun
#' Function to apply to the selected nodes.
#' @param prefix
#' Should be "./" if the first element in relPath has to be the child of the 
#' elements selected by "path", or ".//" if it may be any descendent (and not 
#' necessarily a direct child). 
#' @param ...
#' Parameters passed to "\code{fun}"
#' 
#' @return
#' If no element in the html document corresponds to "path", the function will 
#' return an empty list. Else it will return a list of length equal to the 
#' number of elements selected.
#' 
#' @examples
#' doc <- "<html>
#' <head></head>
#'   <body>
#'   <div id='character1' class='character'>
#'   <span class='name'>Mike</span>
#'   <span class='level digit'>10</span>
#'   </div>
#'   <div id='character2' class='character'>
#'   <span class='name'>Stan</span>
#'   </div>
#'   </body>
#'   </html>"
#' 
#' doc <- htmlParse(doc)
#' 
#' # Level of characters
#' cssApply(doc, ".character>.level", cssNumeric)
#' 
#' # character 2 does not have level, we would want to have a NA value instead of nothing
#' cssApplyInNodeSet(doc, ".character", ".level", cssNumeric)
#'
cssApplyInNodeSet <- function(doc, path, relPath, fun, prefix = "./", ...) {
  path <- cssToXpath(path)
  nodes <- getNodeSet(doc, path)
  if (length(nodes) == 0) {
    warning("Node set was length 0. Nothing has been returned")
    return(list())
  }
  res <- list()
  for (i in 1:length(nodes)) {
    path <- cssToXpath(relPath, prefix)
    res[[i]] <- xpathSApply(nodes[[i]], path, fun, ...)
    if(length(res[[i]]) == 0) res[[i]] <- NA
  }
  res
}