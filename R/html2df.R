#' Convert a html document in data.frame
#'
#' This a high level function that facilitate the creation of a data.frame from
#' an html document
#' 
#' @param doc 
#' A parsed html document created with \code{\link{htmlParse}}
#' @param obsPath
#' CSS path to the html elements containing the observations (see example)
#' @param varPath
#' Named list containing the paths to the elements containing the values to
#' extract inside an observation (see example)
#' @param extractFun
#' Function or vector of functions of same length than \code{varPath}. This 
#' parameter indicates what kind of value to extract for each variable 
#' (cssCharacter for character value, cssNumeric for numeric value, etc.)
#' 
#' @return a data.frame with number of rows corresponding to the number of 
#' elements selected by "obsPath" and number of columns equal to the length of
#' \code{varPath}

html2df <- function(doc, obsPath, varPath, extractFun=cssCharacter) {
  
  if (is.function(extractFun)) {
    tmp <- list()
    for (i in 1:length(varPath)) {
      tmp[[i]] <- extractFun
    }
    extractFun <- tmp
  }
  
  if (length(varPath) != length(extractFun)) {
    stop("extractFun should be a function or a list of functions with same length than \"varPath\"")
  }
  
  obsPath <- cssToXpath(obsPath)
  nodes <- getNodeSet(doc, obsPath)
  
  if (length(nodes) == 0) {
    warning("No observation found. Nothing has been returned")
    return(list())
  }
  
  idx <- 1:length(varPath)
  names(idx) <- names(varPath)
  
  res <- lapply(idx, function(x) {
    r <- list()
    for (i in 1:length(nodes)) {
      path <- cssToXpath(varPath[[x]], ".//")
      tmp <- xpathSApply(nodes[[i]], path, extractFun[[x]])
      if(length(tmp) == 0) tmp <- NA
      r[[i]] <- tmp[1]
    }
    unlist(r)
  })
  
  do.call(data.frame, res)
}