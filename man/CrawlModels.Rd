\name{CrawlModels}
\alias{CrawlModels}
\title{
Get Available Model Runs
}
\description{
This function determine which instances of a given model are available for download.
}
\usage{
CrawlModels(abbrev = NULL, model.url = NULL, depth = NULL, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{abbrev}{
   The model abbreviation, see \code{\link{NOMADSRealTimeList}}.
   Defaults to \code{NULL}.
}
  \item{model.url}{
   A URL to use instead of using the abbreviations in \code{\link{NOMADSRealTimeList}}.
   Defaults to \code{NULL}.
}
  \item{depth}{
  How many model instances to return.
  This avoids having to download the entire model list (sometimes several hundred) if only the first few instances are required.
  Defaults to \code{NULL}, which returns everything.
}
  \item{verbose}{
  Print out each link as it is discovered.
  Defaults to \code{TRUE}.
  }  
}
\details{
This function calls \code{\link{WebCrawler}}, a recursive algorithm that discovers each link available in the URL provided.
It then searches each link in turn, and follows those links until it reaches a dead end.
At that point, it returns the URL.
For the model pages on the NOMADS web site, each dead end is a model instance that can be examined using \code{\link{ParseModelPage}} or have data retrieved from it using \code{\link{GribGrab}}.
}
\value{
\item{urls.out}{A list of web page addresses, each of which corresponds to a model instance.}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}
\note{
It is a good idea to set \code{depth} to a small number rather than leave it at the default value.
Some models (such as the Global Forecast System) have a large number of instances, and crawling each one can take a lot of time.
I recommend \code{depth = 2}, since the first URL may not have an active model on it yet if the model is still being uploaded to the server.
In that case,the first URL will contain no data, and the second URL can be used instead.
}

\seealso{
\code{\link{WebCrawler}}, \code{\link{ParseModelPage}}, \code{\link{NOMADSRealTimeList}}, \code{\link{GribGrab}}
}
\examples{

#Get the latest 5 instances
#for the Global Forecast System 0.5 degree model

\dontrun{urls.out <- CrawlModels(abbrev = "gfs_0p50", depth = 5)}

}
\keyword{ connection }
