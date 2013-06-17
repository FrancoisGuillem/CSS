cssLink <- function(node) {
  # Pour extraire des liens
  xmlGetAttr(node, name="href")
}

cssClass <- function(node) {
  xmlGetAttr(node, name="class")
}

cssId <- function(node) {
  xmlGetAttr(node, name="id")
}

cssSrc <- function(node) {
  xmlGetAttr(node, name="src")
}

cssValue <- function(node) {
  xmlGetAttr(node, name="value")
}

cssName <- function(node) {
  xmlGetAttr(node, name="name")
}

cssNumeric <- function(node, ...) {
  x <- xmlValue(node, ...)
  x <- gsub("[^\\d\\.]", "", x, perl=TRUE)
  x <- gsub("(^\\.+)|(\\.+$)", "", x)
  x <- as.numeric(x)
}

cssCharacter <- xmlValue

cssCharacter <- function(node, ...) {
  xmlValue(node, ...)
}

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
      elAttrs <- str_replace_all(elAttrs, "^(\\w+)([ =]|$)", "@\\1\\2")
      elAttrs <- str_replace_all(elAttrs, " (\\w+)([ =]|$)", " @\\1\\2")
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

cssApply <- function(doc, path, fun, ...) {
  path <- cssToXpath(path)
  xpathSApply(doc, path, fun, ...)
}

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