\name{cssToXpath}
\alias{cssToXpath}
\title{
Translate a cssPath to an xpath.
}
\description{
this function translate a CSS path in an xpath query. It is used by \code{\link{cssApply}} and you generally won't have to use it directly. Nevertheless, it may be useful for debug if \code{\link{cssApply}} does unexpected things.
}
\usage{
cssToXpath(cssPath, prefix = "//")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cssPath}{
Character. A CSS path
}
  \item{prefix}{
Character string appended at the beggining of the xpath query. Valid options are "/", "//", "./" and ".//"
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A character string representing an xpath query.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

}
\examples{
cssToXpath(".character>.name")
cssToXpath("#character1 .name")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
