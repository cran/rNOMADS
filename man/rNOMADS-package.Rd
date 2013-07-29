\name{rNOMADS-package}
\alias{rNOMADS-package}
\alias{rNOMADS}
\docType{package}
\title{
An interface to the NOAA Operational Model Archive and Distribution System
}
\description{
Automatically download forecast data from the National Oceanic and Atmospheric Administration's Operational Model Archive and Distribution System (NOMADS) and read it into R.
Currently, the Global Forecast System 0.5 x 0.5 degree model is the only one supported, but future versions of \code{rNOMADS} will include an interface to all models on NOMADS.
\code{rNOMADS} uses an external series of routines called \code{wgrib2} to read the model data; get \code{wgrib2} at \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/}.
}
\details{
\tabular{ll}{
Package: \tab rNOMADS\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-06-13\cr
License: \tab GPL v3\cr
}}
\author{
Daniel Bowman \email{daniel.bowman@unc.edu}
}
\references{
NOMADS website: \cr
\url{http://nomads.ncep.noaa.gov/} \cr
wgrib2 download page:\cr
\url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/} \cr
wgrib2 reference: \cr
Ebisuzaki, W, Bokhorst, R., Hyvatti, J., Jovic, D., Nilssen, K,
    Pfeiffer, K.,  Romero, P.,  Schwarb, M., da Silva, A., Sondell, N., and Varlamov, S. (2011).
    wgrib2: read and write GRIB2 files.  \emph{National Weather Service Climate Prediction Center}, \cr
    \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/}

}
\keyword{ package }
\examples{
#Getting temperature and relative humidity data for North Carolina, USA, 
#approximately 6 hours from now.
#Values at the ground surface and at the 800 mb level
#Then make a contour plot of the surface temperature.

#Get the latest model date
model.date <- Sys.time()

#Get the forecast 6 hours from now - addition is defined in seconds
fcst.date <- model.date + 6 * 3600

#What region of the atmosphere to get data for
#2 meters above the ground and at 800 mb

levels <- c("2 m above ground", "800 mb")

#What data to return
variables <- c("TMP", "RH") #Temperature and relative humidity

#What region of the world to get data for - omit this and you get the whole planet!
model.domain <- c(-84, -74, 37, 32) #Get the area around North Carolina, USA

file.name <- "fcst.grb" #Name of file to save downloaded data to

#Get the data
GribGrab(levels, variables, model.date = model.date,
    fcst.date = fcst.date, model.domain = model.domain, file.name = file.name)

#Read the data, only capturing temperature at the 2 m above ground level
\dontrun{model.data <- ReadGrib(file.name, c("TMP"), c("2 m above ground"))}

#Make it into an array
\dontrun{model.array <- ModelGrid(model.data, 0.5, 0.5)}

#Make a contour plot of the temperature around North Carolina, USA:
\dontrun{contour(x = model.array$x, y = model.array$y, 
    model.array$z[1,1,,] - 273.15, xlab = "Latitude", ylab = "Longitude",
   main = paste("North Carolina Temperatures for", 
   model.array$fcst.date, "GMT in Celsius"))}



}
