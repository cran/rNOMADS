\name{ParseModelPage}
\alias{ParseModelPage}
\title{
Extract predictions, levels, and variables
}
\description{
This function parses the model download pages on NOMADS, and extracts information on predictions, levels, and variables available for each.
}
\usage{
ParseModelPage(model.url)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model.url}{
   The URL of the model to extract information from, probably returned by \code{\link{NOMADSRealTimeList}}.
}
}
\details{
This function scrapes the web page for a given model and determines which predictions, levels, and variables are present for each.
Predictions are instances returned by each model (for example, the GFS model produces 3 hour predictions up to 192 hours from the model run).
Levels are regions of the atmosphere, surface of the Earth, or subsurface that the model produces output for (for example the GFS model has a ``2 m above ground'' level that has data for temperature, etc, at that height across the Earth).
Variables are types of data (temperature, for example).
}
\value{
\item{pred}{Model predictions}
\item{levels}{Locations of data points}
\item{variables}{Data types}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}
\note{
Many of the names for predictions, levels, and variables are somewhat cryptic.
}

\seealso{
\code{\link{WebCrawler}}, \code{\link{ParseModelPage}}, \code{\link{GribGrab}}
}
\examples{

#An example for the Global Forecast System 0.5 degree model

#Get the latest model url
\dontrun{
urls.out <- CrawlModels(abbrev = "gfs_0p50", depth = 1)

#Get a list of forecasts, variables and levels
model.parameters <- ParseModelPage(urls.out[1])

}


}
\keyword{ utilities }
