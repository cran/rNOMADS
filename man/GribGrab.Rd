\name{GribGrab}
\alias{GribGrab}
\title{
Download GFS model data from the NOMADS server.
}
\description{
This function interfaces with the programming API at \url{http://nomads.ncep.noaa.gov/} to download GFS model data.
The data arrives in grib (gridded binary) format that can be read with \code{\link{ReadGrib}}.
Forecasts are available to 192 hours from the latest model run.
}
\usage{
GribGrab(levels, variables, which.fcst = "back", local.dir = ".", 
    file.name = "fcst.grb", model.date = Sys.time(), 
    fcst.date = Sys.time(), model.domain = NULL, 
    tidy = FALSE, verbose = TRUE)
}
\arguments{
  \item{levels}{
  A list of model levels to download.
}
  \item{variables}{
  A list of model variables to download.
}
  \item{which.fcst}{
  Whether to download the nearest forecast before the requested time (\code{"back"}) or the nearest forecast after the requested time (\code{"foreward"}).  Defaults to \code{"back"}.
}
  \item{local.dir}{
  Where to save the grib file, defaults to the current directory.
}
  \item{file.name}{
  What to name the grib file, defaults to \code{"fcst.grb"}.
}
  \item{model.date}{
  The date and time of the requested model run, in GMT and POSIXlt format, defaults to current system time 
}
  \item{fcst.date}{
  The requested forecast date, defaults to current system time
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
\item{model.date}{Date at which the model was run}
\item{fcst.date}{Date which forecast is for}

}
\details{
This function requires an Internet connection to work.
Also, the NOMADS web site can go down occasionally, causing \code{GribGrab} to fail.
In this case, wait an hour or two and try again.
}
\references{
\url{http://nomads.ncep.noaa.gov/}
}
\author{
Daniel Bowman \email{daniel.bowman@unc.edu}
}

\seealso{
\code{\link{GetModelRunHour}}
}
\examples{

#Get the latest model date
model.date <- Sys.time()

#Get the forecast 6 hours from now - addition is defined in seconds
fcst.date <- model.date + 6 * 3600

#What region of the atmosphere to get data for
levels <- c("2 m above ground", "800 mb") 

#What data to return
variables <- c("TMP", "RH") #Temperature and relative humidity

#What region of the world to get data for - omit this and you get the whole planet!
model.domain <- c(-84, -74, 37, 32) #Get the area around North Carolina, USA

#Get the data
\dontrun{file.name <- GribGrab(levels, variables, model.date = model.date,
    fcst.date = fcst.date, model.domain = model.domain)}

}
\keyword{ connection }
