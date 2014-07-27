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
  # Removing extra-spaces and adding necessary spaces
  cssPath <- gsub(" ?> ?", " >", cssPath)
  cssPath <- gsub(" *\\[ *", "\\[", cssPath)
  cssPath <- gsub(" *\\] *", "\\] ", cssPath)
  cssPath <- gsub(" *\\] *\\[", "\\]\\[", cssPath)
  cssPath <- gsub(" +:", ":", cssPath)
  
  # Parsing
  el <- str_extract_all(cssPath, ">?[^ ]+(\\[ ?(\\w[^]]+)+ ?\\])?(:[()a-zA-Z0-9\\-]+)?( |$)")[[1]]
  el <- str_trim(el)
  
  # Are the elements a direct child or simply a descendent ?
  child <- str_detect(el, "^>")
  el <- str_replace(el, "^>", "")
  
  # name of the elements
  elName <- ifelse(str_detect(el, "^(\\.|#|\\*)"), "*", 
                   tolower(str_extract(el, "^\\w+")))
  
  # id of the elements
  elId <- str_match(el, "#((\\w|-)+)")[,2]
  
  # classes of the elements
  elClasses <- str_match(el, "((\\.(\\w|-)+)+)")[,2]
  
  # pseudo classes
  nthChild <- str_match(el, ":nth-child\\( ?(-? ?\\d+) ?\\)")[,2]
  nthChild <- ifelse(str_detect(el, ":first-child"), "1", nthChild)
  nthChild <- ifelse(str_detect(el, ":last-child"), "-1", nthChild)
  nthLastChild <- str_match(el, ":nth-last-child\\( ?(\\d+) ?\\)")[,2]
  nthChild <- ifelse(is.na(nthLastChild), nthChild, paste0("-", nthLastChild))
  
  nthOfType <- str_match(el, ":nth-of-type\\((-?\\d+)\\)")[,2]
  nthOfType <- ifelse(str_detect(el, ":first-of-type"), "1", nthOfType)
  nthOfType <- ifelse(str_detect(el, ":last-of-type"), "-1", nthOfType)
  nthLastOfType <- str_match(el, ":nth-last-of-type\\( ?(\\d+) ?\\)")[,2]
  nthOfType <- ifelse(is.na(nthLastOfType), nthOfType, paste0("-", nthLastOfType))
  
  el <- str_replace_all(el, ":[^\\[]+($| |\\[)", "\\1")
  
  # Other attributes
  elAttrs <- str_match(el, "\\[(.+)\\] *$")[,2]
  
  data.frame(Child=child, Name=elName, Id=elId, Classes=elClasses, 
             NthChild=nthChild, NthType=nthOfType, Attrs=elAttrs)
  # Conversion to xpath
  path <- sapply(1:length(el), function(i) {
    # basic path
    res <- paste0(ifelse(child[i], "/", "//"), elName[i])
    if (!is.na(nthOfType[i])) {
      res <- sprintf("%s[%s]", res, 
                     str_replace(nthOfType[i], "- ?(\\d+)", "last()-\\1+1"))
    }
    if (!is.na(nthChild[i])) {
      res <- sprintf("%s/../*[%s]",res, 
                     str_replace(nthChild[i], "- ?(\\d+)", "last()-\\1+1"))
      if (elName[i] != "*") res <- sprintf("%s[name()='%s']", res, elName[i])
    }
    
    # Filters
    cond <- c()
    # Id
    if (!is.na(elId[i])) cond <- sprintf("translate(@id, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')='%s'", tolower(elId[i]))
    
    # Classes
    if (!is.na(elClasses[i])) {
      cl <- str_split(elClasses[i], "\\.")[[1]][-1]
      cond <- c(cond, sprintf("contains(concat(' ',normalize-space(translate(@class, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')), ' '),' %s ')",tolower(cl)))
    }
    
    # Attributes
    if (!is.na(attrs <- elAttrs[i])) {
      attrs <- str_replace_all(attrs, "^([a-zA-Z0-9-]+)([ =]|$)", "@\\1\\2")
      attrs <- str_replace_all(attrs, " ([a-zA-Z0-9-]+)([ =]|$)", " @\\1\\2")
      attrs <- str_replace_all(attrs, " @", " and @")
      
      # Ensure case insensitivity
      attrs <- gsub("@(\\w+)", "@\\L\\1", attrs, perl = T)
      cond <- c(cond, attrs)
    }
    
    if (length(cond) > 0) res <- sprintf("%s[%s]", res, 
                                         paste(cond, collapse=" and "))
    
    res
  })
  
  path <- paste(path, collapse="")
  str_replace(path, "^/+", prefix)
}
