\name{SanitizeURL}
\alias{SanitizeURL}
\title{
Remove illegal URL characters
}
\description{
An internal routine for replacing illegal URL character in level and variable names
}
\usage{
SanitizeURL(bad.strs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bad.str}{
    A vector of strings destined for concatenation into a URL.
    They may have characters that are not allowed in URLs.
}
}
\details{
This is an internal function that helps make sure variable and level names create valid URLS for downloading model data.
}
\value{
\item{good.str}{A vector of strings with illegal URL characters replaced by their percent encoding}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}

\examples{

bad.strs <- c("I am full of ^ill=/\\\\egal characters")
good.str <- SanitizeURL(bad.strs)
print(good.str[1])
}
\keyword{ internal }
