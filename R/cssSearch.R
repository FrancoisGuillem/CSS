xmlSearch <- function(doc, values, n) {
  candidates <- xpathSApply(doc,"//*", function(x) {
    if(xmlSize(x) != 1) {
      return(NULL)
    } else {
      v <- xmlValue(x)
      if(v == "") {
        return(NULL)
      } else if (grepl(values, v)){
        return(cssPath(x))
      } else {
        return(NULL)
      }
    }
  })
  i <- which(!sapply(candidates, is.null))
  candidates <- candidates[i]
  finalCand <- character(0)
  while(length(finalCand) < 1) {
    for(c in candidates) {
      c <- .cssCandidates(c)
      tmp <- sapply(c, function(x) length(getNodeSet(doc,cssToXpath(x))))
      finalCand <- c(finalCand, c[which(tmp == n)])
    }
    finalCand <- finalCand[which.min(nchar(finalCand))]
    for(i in 1:length(candidates)) {
      candidates[[i]] <- candidates[[i]][-nrow(candidates[[i]]),]
    }
  }
  .cssSimplify(doc,finalCand)
}

.cssCandidates <- function(cssPath, comb = NULL) {
  child <- tail(cssPath, 1)
  newElem <- with(child, c(sprintf("%s", name)))
  
  if(!is.na(child$id)) newElem <- c(newElem,with(child, c(sprintf("%s#%s", name, id))))
  if(!is.na(child$class)) newElem <- c(newElem,with(child, c(sprintf("%s.%s", name, class))))
  
  newComb <- NULL
  for (n in newElem) {
    newComb <- c(newComb, paste(n,comb, sep =" "))
  }
  if (nrow(cssPath)==1) {
    return(newComb)
  } else {
    .cssCandidates(cssPath[-nrow(cssPath),], newComb)
  }
}	

.cssSimplify <- function(doc,path) {
  n <- length(getNodeSet(doc, cssToXpath(path)))
  parsedPath <- strsplit(path, " ")[[1]]
  res <- path
  for(i in 1:(length(parsedPath)-1)) {
    simplerPath <- paste(parsedPath[-(1:i)], collapse = " ")
    if(length(getNodeSet(doc, cssToXpath(simplerPath))) == n) {
      res <- simplerPath
    }
  }
  res
}

cssPath <- function(x) {
  f <- function(x, name) {
    res <- xmlGetAttr(x, name)
    if(is.null(res)) {
      res <- NA
    }
    res
  }
  ancestors <- xmlAncestors(x)
  names <- sapply(ancestors[-1], xmlName)
  class <- unlist(sapply(ancestors[-1], f, name = "class"))
  id <- unlist(sapply(ancestors[-1], f, name = "id"))
  data.frame(name = names, class = class, id = id, stringsAsFactors = F)
}
