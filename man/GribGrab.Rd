\name{GribGrab}
\alias{GribGrab}
\title{
Download model data from the NOMADS server.
}
\description{
This function interfaces with the programming API at \url{http://nomads.ncep.noaa.gov/} to download NOMADS model data.
The available models (54 in total) can be viewed by calling \code{\link{NOMADSList}} without arguments.
The data arrives in grib (gridded binary) format that can be read with \code{\link{ReadGrib}}.
}
\usage{
GribGrab(model.url, pred, levels, variables, 
    local.dir = ".", file.name = "fcst.grb",
    model.domain = NULL, tidy = FALSE, verbose = TRUE)
}
\arguments{
  \item{model.url}{
      The address of a model download page, probably from \code{\link{CrawlModels}}.
}
  \item{pred}{
      The list of predictions (or model times) determined by the specific model from \code{model.url}
  }
  \item{variables}{
  A list of model variables to download.
}
  \item{levels}{
  A list of model levels to download.
}
  \item{local.dir}{
  Where to save the grib file, defaults to the current directory.
}
  \item{file.name}{
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
}
\value{
\item{file.name}{the name of the grib file that was downloaded.}

}
\references{
\url{http://nomads.ncep.noaa.gov/}
}
\author{
Daniel Bowman \email{daniel.bowman@unc.edu}
}

\seealso{
\code{\link{CrawlModels}}, \code{\link{ParseModelPage}}, \code{\link{ReadGrib}}
}
\examples{

#An example for the Global Forecast System 0.5 degree model

#Get the latest model url
\dontrun{urls.out <- CrawlModels(abbrev = "gfs0.5", depth = 1)}

#Get a list of forecasts, variables and levels
\dontrun{model.parameters <- ParseModelPage(urls.out[1])}

#Figure out which one is the 6 hour forecast
#provided by the latest model run
#(will be the forecast from 6-12 hours from the current date) 

\dontrun{my.pred <- model.parameters$pred[grep("06$", model.parameters$pred)]}

#What region of the atmosphere to get data for
levels <- c("2 m above ground", "800 mb") 

#What data to return
variables <- c("TMP", "RH") #Temperature and relative humidity

#Get the data
\dontrun{file.name <- GribGrab(urls.out[1], my.pred, levels, variables)}

#Extract the data
\dontrun{model.data <- ReadGrib(file.name, levels, variables)}

#Reformat it
\dontrun{model.grid <- ModelGrid(model.data)}

#Show an image of world temperature at ground level
\dontrun{image(model.grid$z[2, 1,,])}

}
\keyword{ connection }
