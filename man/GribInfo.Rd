\name{GribInfo}
\alias{GribInfo}
\title{
Get grib file inventory.
}
\description{
Find out what model, date, levels, and variables are contained in a grib file.
}
\usage{
GribInfo(grib.file, file.type = "grib2")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grib.file}{
  Full path to a grib file.
}
  \item{file.type}{
  Whether the file is in grib2 format (\code{"grib2"}) or grib format (\code{"grib"}).  
}
}
\details{
This function allows you to find out what is inside an unknown grib file.
It does this by performing a system call to \code{wgrib2} or \code{wgrib}.
}
\value{
\item{grib.info}{Inventory of the grib file.
If the input is in grib2 format, you also get the grid definition.
}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}
\note{
In order to use this function, you need to have installed \code{wgrib2} (for grib2 files) or \code{wgrib} (for grib files).
You can find these here: \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/} and \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html}.
}
\seealso{
\code{\link{ReadGrib}}, \code{\link{GetDODSModelRunInfo}}, \code{\link{GribGrab}}
}
\examples{

\dontrun{
#An example for the Global Forecast System 0.5 degree model

#Get the second latest model url, for stability
urls.out <- CrawlModels(abbrev = "gfs_0p50", depth = 2)

#Get a list of forecasts, variables and levels
model.parameters <- ParseModelPage(urls.out[2])

#Figure out which one is the 6 hour forecast
#provided by the latest model run
#(will be the forecast from 6-12 hours from the current date) 

my.pred <- model.parameters$pred[grep("06$", model.parameters$pred)]

#What region of the atmosphere to get data for
levels <- c("2 m above ground", "800 mb")

#What data to return
variables <- c("TMP", "RH") #Temperature and relative humidity

#Get the data
grib.info <- GribGrab(urls.out[2], my.pred, levels, variables)

#Print out the inventory - it should match the requested data
grib.inv <- GribInfo(grib.info[[1]]$file.name, "grib2")
}
}
\keyword{ utilities }
