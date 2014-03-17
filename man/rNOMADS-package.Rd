\name{rNOMADS-package}
\alias{rNOMADS-package}
\alias{rNOMADS}
\docType{package}
\title{
An interface to the NOAA Operational Model Archive and Distribution System
}
\description{
Automatically download forecast data from the National Oceanic and Atmospheric Administration's Operational Model Archive and Distribution System (NOMADS) and read it into R.
\code{rNOMADS} uses an external series of routines called \code{wgrib2} to read operational model data; get \code{wgrib2} at \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/}.
The package will also attempt to call another external routine called \code{wgrib} if the user wishes to read GRIB1 files; get \code{wgrib} at \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html}.
}
\details{
\tabular{ll}{
Package: \tab rNOMADS\cr
Type: \tab Package\cr
Version: \tab 1.2.0\cr
Date: \tab 2014-03-14\cr
License: \tab GPL v3\cr
}}
\author{
Daniel C. Bowman \email{daniel.bowman@unc.edu}
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
    \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/}\cr
wgrib download page:\cr
\url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html}\cr
}
\keyword{ package }
\examples{
#Getting temperature and relative humidity data for North Carolina, USA, 
#6-12 hours from now depending on when the GFS model was last run.
#Get values at the ground surface and at the 800 mb level
#Then make a contour plot of the surface temperature.

#Using the Global Forecast System 0.5x0.5 model
#Get the first two models available
\dontrun{urls.out <- CrawlModels(abbrev = "gfs0.5", depth = 2)}

#Get predictions, variables, and levels
#If the first URL fails, try the second
#(sometimes there's a lag time between the URL appearing
#and the data becoming available)

\dontrun{model.parameters <- ParseModelPage(urls.out[1])}

#Get the 6 hour prediction
\dontrun{pred.6 <- model.parameters$pred[grep("06$", model.parameters$pred)]}

#What region of the atmosphere to get data for
#2 meters above the ground and at 800 mb

levels <- c("2 m above ground", "800 mb")

#What data to return
variables <- c("TMP", "RH") #Temperature and relative humidity

#What region of the world to get data for - omit this and you get the whole planet!
model.domain <- c(-84, -74, 37, 32) #Get the area around North Carolina, USA

file.name <- "fcst.grb" #Name of file to save downloaded data to

#Get the data
\dontrun{GribGrab(urls.out[1], pred.6, levels, variables,
    model.domain = model.domain, file.name = file.name)}

#Read the data, only capturing temperature at the 2 m above ground level
\dontrun{model.data <- ReadGrib(file.name, c("2 m above ground"), c("TMP"))}

#Make it into an array
\dontrun{model.array <- ModelGrid(model.data, c(0.5, 0.5))}

#Make a contour plot of the temperature around North Carolina, USA:
\dontrun{contour(x = model.array$x, y = model.array$y, 
    model.array$z[1,1,,] - 273.15, xlab = "Longitude", ylab = "Latitude",
   main = paste("North Carolina Temperatures for", 
   model.array$fcst.date, "GMT in Celsius"))}
}
