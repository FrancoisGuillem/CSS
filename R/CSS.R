cssLink <- function(node) {
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

cssNumeric <- function(node, ...) {
  x <- xmlValue(node, ...)
  x <- gsub("[^\\d\\.]", "", x, perl=TRUE)
  x <- gsub("(^\\.+)|(\\.+$)", "", x)
  x <- as.numeric(x)
}

cssCharacter <- function(node, ...) {
  xmlValue(node, ...)
}

cssToXpath <- function(cssPath, prefix = "//") {
  cssPath <- gsub(">", " >", cssPath)
  path <- strsplit(cssPath, " ")[[1]]
  path <- gsub("#","#[@id='", path)
  path <- gsub("\\.","#\\[contains(@class,'", path)
  path <- strsplit(path, "#")
  for(i in 1:length(path)) {
  	if(path[[i]][1] == "") {path[[i]][1]  <-  "*"} 
  	if(path[[i]][1] == ">") {path[[i]][1]  <-  ">*"} 
  	if(length(path[[i]]) > 1) {
  	  for (j in 2:length(path[[i]])) {
  	    path[[i]][j] <- gsub("id='(.+)$", "id='\\1']",path[[i]][j])
  	    path[[i]][j] <- gsub("class,'(.+)$", "class,'\\1')]",path[[i]][j])
  	  }
  	}
  }
  path <- lapply(path, paste, collapse="")
  path <- paste(unlist(path), collapse="//")
  
  path <- gsub("/>", "", path)
  paste(prefix, path, sep ="")
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