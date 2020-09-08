\name{GribGrab}
\alias{GribGrab}
\title{
Download grib file from the NOMADS server.
}
\description{
This function interfaces with the programming API at \url{http://nomads.ncep.noaa.gov/} to download NOMADS model data.
The available models can be viewed by calling \code{\link{NOMADSRealTimeList}}.
The data arrives in grib (gridded binary) format that can be read with \code{\link{ReadGrib}}.
}
\usage{
GribGrab(model.url, preds, levels, variables, 
    local.dir = NULL, file.names = NULL,
    model.domain = NULL, tidy = FALSE, verbose = TRUE,
    check.url = TRUE, download.method = NULL)
}
\arguments{
  \item{model.url}{
      The address of a model download page, probably from \code{\link{CrawlModels}}.
}
  \item{preds}{
      A vector of predictions (or model times) determined by the specific model from \code{model.url}
}
  \item{levels}{
  A list of model levels to download.
}
  \item{variables}{
  A list of model variables to download.
}
  \item{local.dir}{
  Where to save the grib file, defaults to the current directory.
}
  \item{file.names}{
  What to name the grib file, defaults to \code{"fcst.grb"}.
}
  \item{model.domain}{
  A vector of latitudes and longitudes that specify the area to return a forecast for.
  This is a rectangle with elements: west longitude, east longitude, north latitude, south latitude.
}
  \item{tidy}{
  If \code{TRUE}, remove all files with the suffix ".grb" from \code{local.dir} prior to downloading a new grib file.
}
  \item{verbose}{
  If \code{TRUE}, give information on connection status.  Default \code{TRUE}
}
 \item{check.url}{
  If \code{TRUE}, verify that the model URL is real and contains data.  Default \code{TRUE}
}
 \item{download.method}{
  Allows the user to set the download method used by \code{download.file}: \code{"internal"}, \code{"wget"} \code{"curl"}, \code{"lynx"}.
  If \code{NULL} (the default), let R decide.
}
}
\value{
\item{grib.info$file.name}{The path and file name of the grib file that was downloaded.}
\item{grib.info$url}{The URL that the grib file was downloaded from}
}
\references{
\url{http://nomads.ncep.noaa.gov/}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}

\note{
This requires the external programs \code{wgrib2} and\\or \code{wgrib} to be installed (depending on whether the files are in GRIB2 or GRIB format).}

\seealso{
\code{\link{CrawlModels}}, \code{\link{ParseModelPage}}, \code{\link{ReadGrib}}
}
\examples{

#An example for the Global Forecast System 0.5 degree model

#Get the latest model url
\dontrun{
urls.out <- CrawlModels(abbrev = "gfs_0p50", depth = 1)

#Get a list of forecasts, variables and levels
model.parameters <- ParseModelPage(urls.out[1])

#Figure out which one is the 6 hour forecast
#provided by the latest model run
#(will be the forecast from 6-12 hours from the current date) 

my.pred <- model.parameters$pred[grep("06$", model.parameters$pred)]

#What region of the atmosphere to get data for
levels <- c("2 m above ground", "800 mb") 

#What data to return
variables <- c("TMP", "RH") #Temperature and relative humidity

#Get the data
grib.info <- GribGrab(urls.out[1], my.pred, levels, variables)

#Extract the data
model.data <- ReadGrib(grib.info[[1]]$file.name, levels, variables)

#Reformat it
model.grid <- ModelGrid(model.data, c(0.5, 0.5))

#Show an image of world temperature at ground level
image(model.grid$z[2, 1,,])
}
}
\keyword{ connection }
