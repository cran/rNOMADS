\name{WebCrawler}
\alias{WebCrawler}
\title{
Get web pages 
}
\description{
Discover all links on a given web page, follow each one, and recursively scan every link found.
Return a list of web addresses whose pages contain no links.
}
\usage{
WebCrawler(url, depth = NULL, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{
   A URL to scan for links.
}
  \item{depth}{
  How many links to return.
  This avoids having to recursively scan hundreds of links.
  Defaults to \code{NULL}, which returns everything.
}
  \item{verbose}{
  Print out each link as it is discovered.
  Defaults to \code{TRUE}.
  }  
}
\details{
\code{\link{CrawlModels}} uses this function to get all links present on a model page.
}
\value{
\item{urls.out}{A list of web page addresses, each of which corresponds to a model instance.}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}
\note{
While it might be fun to try \code{WebCrawler} on a large website such as Google, the results will be unpredictable and perhaps disastrous if \code{depth} is not set.
This is because there is no protection against infinite recursion. 
}

\seealso{
\code{\link{CrawlModels}}, \code{\link{ParseModelPage}}
}
\examples{

#Find the first 10 model runs for the 
#GFS 0.5x0.5 model

\dontrun{urls.out <- WebCrawler(
"http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p50.pl", depth = 10)}

}
\keyword{ connection }
