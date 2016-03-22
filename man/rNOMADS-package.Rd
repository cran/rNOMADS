\name{rNOMADS-package}
\alias{rNOMADS-package}
\alias{rNOMADS}
\docType{package}
\title{
An interface to the NOAA Operational Model Archive and Distribution System
}
\description{
Automatically download forecast data from the National Oceanic and Atmospheric Administration's Operational Model Archive and Distribution System (NOMADS) and read it into R.
This can be done in two ways: reading ascii data directly from the server using the DODS-GrADS system or downloading binary files in GRIB1 or GRIB2 format.
The grib capability of \code{rNOMADS} uses an external series of routines called \code{wgrib2} to read operational model data; get \code{wgrib2} at \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/}.
The package will also attempt to call another external routine called \code{wgrib} if the user wishes to read GRIB1 files; get \code{wgrib} at \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html}.
}
\details{
\tabular{ll}{
Package: \tab rNOMADS\cr
Type: \tab Package\cr
Version: \tab 2.2.0\cr
Date: \tab 2016-03-21\cr
License: \tab GPL v3\cr
}}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}
\references{
Bowman, D. C. and Lees, J. M. (2015).
Near real time weather and ocean model data access with rNOMADS.
\emph{Computers \& Geosciences} 78, pp. 88-95.\cr 
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
#Getting temperature for North Carolina, USA, 
#6-12 hours ago depending on when the latest model run was.
#Get values at the ground surface and at the 800 mb level
#Then make a contour plot of the surface temperature.
#We use GrADS-DODS here for compatibility.

#Using the Global Forecast System 0.5x0.5 model
\dontrun{

urls.out <- GetDODSDates(abbrev = "gfs_0p50")
model.url <- tail(urls.out$url, 1) #Get most recent model date

#Get most recent model run

model.runs <- GetDODSModelRuns(model.url)
model.run <- tail(model.runs$model.run, 1)
 

#Get ground temperature for the 6 hour prediction
variable <- "tmp2m" #temp at 2 m
time <- c(2,2) #6 hour prediction
lon.dom <- seq(0, 360, by = 0.5) #domain of longitudes in model
lat.dom <- seq(-90, 90, by = 0.5) #domain of latitudes in model
lon <- which((lon.dom >= 360 - 84) & (lon.dom <= 360 - 74)) - 1 #NOMADS indexes start at 0
lat <- which((lat.dom <= 37) & (lat.dom >= 32)) - 1 #NOMADS indexes start at 0 
model.data.surface <- DODSGrab(model.url, model.run, variable, time, c(min(lon), max(lon)), 
   c(min(lat), max(lat)))

lev <- c(8, 8) #800 mb level
variable <- "tmpprs"
model.data.800mb <- DODSGrab(model.url, model.run, variable, time, c(min(lon), max(lon)),
   c(min(lat), max(lat)), level = lev)

#Make results into arrays
model.array.surface <- ModelGrid(model.data.surface, c(0.5, 0.5))
model.array.800mb <- ModelGrid(model.data.800mb, c(0.5, 0.5))

#Make a contour plot of the temperature around North Carolina, USA:
contour(x = model.array.surface$x - 360, y = model.array.surface$y, 
    model.array.surface$z[1,1,,] - 273.15, xlab = "Longitude", ylab = "Latitude",
   main = paste("North Carolina Surface Temperatures for", 
   model.array.surface$fcst.date, "UTC in Celsius"))

dev.new()
contour(x = model.array.800mb$x - 360, y = model.array.800mb$y,
    model.array.800mb$z[1,1,,] - 273.15, xlab = "Longitude", ylab = "Latitude",
   main = paste("North Carolina Temperatures at 800 mb for",
   model.array.surface$fcst.date, "UTC in Celsius"))
}
}
