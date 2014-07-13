#' Translate a css path to an xpath.
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
