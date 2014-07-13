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

#' Translate a cssPath to an xpath.
#' 
#' this function translates a CSS path in an xpath query. It is used by
#' \code{\link{cssApply}} and you generally won't have to use it directly.
#' Nevertheless, it may be useful for debug if \code{\link{cssApply}} does
#' unexpected things.
#' 
#' @param cssPath
#' Character. A CSS path
#' @param prefix
#' Character string appended at the beggining of the xpath query. Valid options
#' are "/", "//", "./" and ".//"
#' 
#' @return
#' A character string representing an xpath query.
#' 
#' @examples
#' cssToXpath(".character>.name")
#' cssToXpath("#character1 .name")
#' 
cssToXpath <- function(cssPath, prefix="//") {
  cssPath <- gsub(" ?> ?", " >", cssPath)
  cssPath <- gsub(" *\\[ *", "\\[", cssPath)
  cssPath <- gsub(" *\\] *", "\\] ", cssPath)
  
  el <- str_extract_all(cssPath, ">?[^ ]+(\\[ ?(\\w[^]]+)+ ?\\])?( |$)")[[1]]
  
  path <- sapply(el, function(x) {   
    elAttrs <- NULL
    
    # Is the element a direct child or simply a descendent ?
    if (str_detect(x, "^>")) {
      child <- TRUE
      x <- str_replace(x, "^>", "")
    } else {
      child <- FALSE
    }
    
    # Name
    if (str_detect(x, "^(\\.|#)")) {
      elName <- "*"
    } else {
      # tolower ensures case insensitivity
      elName <- tolower(str_extract(x, "^((\\w+)|\\*)")) 
      x <- str_replace(x, "^((\\w+)|\\*)", "")
    }
    
    # attributes (except ID and CLASS)
    if (str_detect(x, "\\[.+\\]")) {
      elAttrs <- str_match(x, "\\[(.+)\\]")[2]
      elAttrs <- str_replace_all(elAttrs, "^([a-zA-Z0-9-]+)([ =]|$)", "@\\1\\2")
      elAttrs <- str_replace_all(elAttrs, " ([a-zA-Z0-9-]+)([ =]|$)", " @\\1\\2")
      elAttrs <- str_replace_all(elAttrs, " @", " and @")
      
      # Ensure case insensitivity
      elAttrs <- gsub("@(\\w+)", "@\\L\\1", elAttrs, perl = T)
      
      x <- str_replace(x, "\\[.+\\]\\s*", "")
    }
    
    # ID
    if (str_detect(x, "#")) {
      id <- str_match(x, "#((\\w|-)+)")[2]
      elAttrs <- c(elAttrs, sprintf("@id='%s'", id))
    }
    
    # classes
    if (str_detect(x, "\\.")) {
      class <- str_match_all(x, "\\.((\\w|-)+)")[[1]][,2]
      elAttrs <- c(elAttrs, sprintf("contains(concat(' ',normalize-space(translate(@class, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')), ' '),' %s ')", tolower(class)))
    }
    
    if(!is.null(elAttrs)) {
      elAttrs <- paste(elAttrs, collapse=" and ")
      elAttrs <- sprintf("[%s]", elAttrs)
    } else {
      elAttrs <- ""
    }
    
    sprintf("%s%s%s",
            ifelse(child, "/", "//"),
            elName,
            elAttrs)
  })
  
  path[1] <- str_replace(path[1], "^/+", prefix)
  
  paste(path, collapse = "")
}


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