\name{LinkExtractor}
\alias{LinkExtractor}
\title{
Extracts links from web pages
}
\description{
Parse a web page, capturing and returning any links found.
}
\usage{
LinkExtractor(url)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{
   A URL to scan for links.
}
}
\details{
This is an internal routine used by several functions in the package.
}
\value{
\item{links}{A vector of link URLs}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}
\note{
While it might be fun to try \code{LinkExtractor} on a large website such as Google, the results will be unpredictable and perhaps disastrous if \code{depth} is not set.
This is because there is no protection against infinite recursion. 
}

\seealso{
\code{\link{WebCrawler}}
}
\examples{

#Find model runs for the 
#GFS 0.5x0.5 model

\dontrun{
urls.out <- LinkExtractor(
"http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p50.pl")
}

}
\keyword{ connection }
