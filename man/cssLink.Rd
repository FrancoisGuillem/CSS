\name{cssLink}
\alias{cssLink}
\alias{cssNumeric}
\alias{cssCharacter}
\alias{cssSrc}
\alias{cssId}
\alias{cssClass}
\title{
Extract information from an html element
}
\description{
These function has to be used in \code{\link{cssApply}} and \code{\link{cssApplyInNodeSet}}. They aim to facilitate extraction of different kind of information.

\code{cssNumeric} extracts the numeric value of an element, \code{cssCharacter} extracts text. \code{cssLink} extracts the url of a link. 

\code{cssSrc}, \code{cssId}, \code{cssClass} are less usefull and extract respectively the source of an element, its id and its css class.
}
\usage{
cssLink(node)
cssNumeric(node, ...)
cssCharacter(node, ...)
cssSrc(node)
cssId(node)
cssClass(node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{
An html element
}
  \item{\dots}{
  Additional arguments passed to \code{\link{xmlValue}}
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
All these function return a chracter string except \code{cssNumeric} which returns a Numeric value
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
\code{\link{cssApply}}, \code{\link{cssApplyInNodeSet}}
}
\examples{
doc <- "<html>
<head></head>
<body>
  <div id='character1' class='character'>
    <span class='name'>Mike</span>
    <span class='level digit'>10</span>
    <a href='http://someurl.com'>Complete profile</a>
  </div>
  <div id='character2' class='character'>
    <span class='name'>Stan</span>
    <a href='http://someurl2.com'>Complete profile</a>
  </div>
</body>
</html>"

doc <- htmlParse(doc)

# Names of the characters
cssApply(doc, ".character>.name", cssCharacter)

# Name of character1
cssApply(doc, "#character1>.name", cssCharacter)

# Urls of the profiles
cssApply(doc, ".character>a", cssLink)

# Level of characters
cssApply(doc, ".character>.level", cssNumeric)

# character 2 does not have level, we would want to have a NA value instead of nothing
cssApplyInNodeSet(doc, ".character", ".level", cssNumeric)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
