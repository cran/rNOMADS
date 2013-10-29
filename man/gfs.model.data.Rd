\name{gfs.model.data}
\alias{gfs.model.data}
\docType{data}
\title{
Temperature and relative humidity in North Carolina.
}
\description{
Temperature and relative humidity at ground level and 800 mb of the region around North Carolina, USA from the 0.5 degree resolution Global Forecast System model.
This is the 12 hour forecast for July 28, 2013 at 18:00 GMT.
}
\usage{data(GFS)}
\source{
\url{http://nomads.ncep.noaa.gov/}
}
\examples{
data(GFS)
gfs.array <- ModelGrid(gfs.model.data)
contour(x = gfs.array$x, y = gfs.array$y, gfs.array$z[2,1,,] - 273.15,
   xlab = "Latitude", ylab = "Longitude",
   main = paste("North Carolina Temperatures for", 
   gfs.array$fcst.date, "GMT in Celsius"))

}
\keyword{datasets}
