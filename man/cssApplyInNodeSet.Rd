\name{cssApplyInNodeSet}
\alias{cssApplyInNodeSet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find a set of elements and inside each of them apply a function to some element.
}
\description{
Consider the following case : on an html page you have information about several people. Several informations are included in a div of class "people", but for some people some information is missing. 

With this function, you can first select all divs of class "people" and then search inside them if the information is available. If not, the function will return a NA value for the person.
}
\usage{
cssApplyInNodeSet(doc, path, relPath, fun, prefix = "./", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{
An html document parsed with \code{\link{htmlParse}}.
}
  \item{path}{
Character. It CSS path used to identify elements where to search the information
}
  \item{relPath}{
Character. CSS path used to select elements in the elements selected with "path"
}
  \item{fun}{
Function to apply to the selected nodes.
}
  \item{prefix}{
Should be "./" if the first element in relPath has to be the child of the elements selected by "path", or ".//" if it may be any descendent (and not necessarily a direct child). 
}
  \item{\dots}{
Parameters passed to "\code{fun}"
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
If no element in the html document corresponds to "path", the function will return an empty list. Else it will return a list of length equal to the number of elements selected.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Francois Guillem <guillem.francois@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cssApply}}, \code{\link{cssNumeric}}
}
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

# Level of characters
cssApply(doc, ".character>.level", cssNumeric)

# character 2 does not have level, we would want to have a NA value instead of nothing
cssApplyInNodeSet(doc, ".character", ".level", cssNumeric)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
