\name{cssApply}
\alias{cssApply}

\title{
Apply a function to elements identified by a CSS path.
}
\description{
This function is a wrapper function for \code{\link{xpathSApply}}. It selects nodes inside an html document corresponding to a CSS path and then applies an arbitrary function to theses nodes. It is useful to extract data frome an html page.
}
\usage{
cssApply(doc, path, fun, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{
An html document parsed with \code{\link{htmlParse}}.
}
  \item{path}{
CSS path
}
  \item{fun}{
Function to apply to the selected nodes.
}
  \item{\dots}{
Parameters passed to "\code{fun}"
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The result may differ based on the function used. It will generally be a character or numeric vectorwith length equal to the number of nodes selected.

If no element in the html document corresponds to the path provided, the function will return an empty list.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Francois Guillem <guillem_francois@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cssApplyInNodeSet}}, \code{\link{cssNumeric}}
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

# Names of the characters
cssApply(doc, ".character>.name", cssCharacter)

# Name of character1
cssApply(doc, "#character1>.name", cssCharacter)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
