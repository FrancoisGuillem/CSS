\name{CSS-package}
\alias{CSS-package}
\alias{CSS}
\docType{package}
\title{
Extract information from an html document with CSS selectors
}
\description{
The package provides functions that facilitate extraction of information from an html document by using css selectors instead of xpath queries.
}
\details{
\tabular{ll}{
Package: \tab CSS\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-01-31\cr
License: \tab GPL (>= 2)\cr
}
The functions of this packages are actually wrapper functions for the package XML. For instance \code{\link{cssApply}} converts a css path in an xpath query and then invokes \code{\link[XML]{xpathSApply}}.
}
\author{
Francois Guillem <guillem.francois@gmail.com>

}

\keyword{ package }

\examples{
doc <- "<html>
<head></head>
<body>
  <div id='character1' class='character'>
    <span class='name'>Mike</span>
    <span class='level digit'>10</span>
  </div>
  <div id='character2' class='character'>
    <span class='name'>Stan</span>
  </div>
</body>
</html>"

doc <- htmlParse(doc)

# Names of the characters
cssApply(doc, ".character>.name", cssCharacter)

# Name of character1
cssApply(doc, "#character1>.name", cssCharacter)

# Level of characters
cssApply(doc, ".character>.level", cssNumeric)

# character 2 does not have level, we would want to have a NA value instead of nothing
cssApplyInNodeSet(doc, ".character", ".level", cssNumeric)
}
