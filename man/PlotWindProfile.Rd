\name{PlotWindProfile}
\alias{PlotWindProfile}
\title{
Plot wind speed and direction
}
\description{
Creates a polar plot showing the azimuth, elevation, and magnitude of winds.  
}
\usage{
PlotWindProfile(zonal.wind, meridional.wind, height, magnitude = NULL,
    magnitude.range = c(0, 50), height.range = c(0, 50000), points = TRUE, lines = FALSE,
    radial.axis = TRUE, elev.circles = NULL, elev.labels = NULL, radial.lines = NULL,
    colorbar = TRUE, colorbar.label = NULL, north.label = TRUE, invert = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zonal.wind}{
     A vector or list of vectors of zonal (East-West) winds, west negative.
}
  \item{meridional.wind}{
     A vector or list of vectors of meridional (North-South) winds, south negative.
}
  \item{height}{
     A vector or list of vectors of height at which each wind measurement was taken.
}
   \item{magnitude}{
      A vector or list of vectors of magnitudes to plot at each wind azimuth, instead of using the wind magnitudes.
      This allows plotting of other data (such as the speed of sound) along the wind vectors. 
      Defaults to \code{NULL}.
} 
   \item{magnitude.range}{
      Ranges of wind speed to plot. Values greater or lesser than these will be saturated. Defaults to \code{c(0, 50)}.
}
   \item{height.range}{
      Ranges of heights to plot, values outside of this will not appear. Defaults to \code{c(0, 50000)}.
}
   \item{points}{
     Whether to plot measurements as points.  Defaults to \code{TRUE}.
}
  \item{lines}{
     Whether to connect measurements together with lines.  Defaults to \code{FALSE}.
}
  \item{radial.axis}{
     Whether to plot an axis at the outer edge of the diagram.  Defaults to \code{TRUE}.
}
  \item{elev.circles}{
     Plot a dashed circles as elevation scales.  Defaults to \code{NULL}, which plots nothing.
}
  \item{elev.labels}{
     What labels to put on the elevation scales.  Defaults to \code{NULL}, which means no labels.
}
  \item{radial.lines}{
     Plot lines radiating from the center of the plot showing azimuth directions.  Defaults to \code{NULL}, which plots nothing.
}
  \item{colorbar}{
     If \code{TRUE}, plot a color bar.  Defaults to \code{TRUE}.
}
  \item{colorbar.label}{
     What label to put on the colorbar.  Defaults to \code{NULL}, which means no label.
}
   \item{north.label}{
      If \code{TRUE}, put a label denoting the north direction.  Defaults to \code{TRUE}.
}
   \item{invert}{
      Reverses the edge and the center of the plot, making elevations decrease towards the center.  Defaults to \code{FALSE}.
}
   \item{\dots}{This function supports some optional parameters as well:
   \itemize{
      \item{r.axis}{ - radius of plot axis}
      \item{tick.len}{ - length of azimuth ticks}
      \item{r.axis.ticks}{ - Whether or not to put tick marks on the outer axis}
      \item{max.az}{ - If plotting lines and the difference between two segments is greater than this value, interpolate between them to make things smooth}
      \item{color.map}{ - A list of colors to use, defaults to \code{rainbow(n.cols, start=0, end=5/6)}}
      \item{n.cols}{ - Number of color bins in color map}
      \item{sub.col}{ - Color of internal (elevation and azimuth) axes as a vector of length 2}
      \item{sub.lty}{ - Type of internal axes, as a vector of length 2}
      \item{sub.lwd}{ - Width of internal axes, as a vector of length 2}
      \item{elev.labels.az}{ - Which azimuth to plot elevation labels on} 
      \item{point.cex}{ - size of points, if plotted}
      \item{pch}{ - Plot character of points, if plotted}
      \item{lty}{ - Line style, if lines are selected}
      \item{lwd}{ - Line thickness, if lines are selected}
      \item{colorbar.tick}{ - Where to put labels on colorbar}
   }
   }

}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}

\examples{

\dontrun{
download.file("http://www.gmail.com/~haksaeng/rNOMADS/myTA.RDATA", 
   destfile = "myTA.RDATA")
load("myTA.RDATA")
#Find the latest Global Forecast System model run
model.urls <- GetDODSDates("gfs_0p50")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)

#Get model nodes

lons <- seq(0, 359.5, by = 0.5)
lats <- seq(-90, 90, by = 0.5)
lon.ind <- which(lons <= (max(myTA$lon + 360) + 1) & lons >= (min(myTA$lon + 360) - 1))
lat.ind <- which(lats <= (max(myTA$lat) + 1) & lats >= (min(myTA$lat) - 1))
levels <- c(0, 46)
time <- c(0, 0)

#Get data 
variables <- c("hgtprs", "ugrdprs", "vgrdprs")
model.data <- DODSGrab(latest.model, latest.model.run,
   variables, time, c(min(lon.ind), max(lon.ind)), 
   c(min(lat.ind), max(lat.ind)), levels)

#Build profiles
profile <- BuildProfile(model.data, myTA$lon + 360, myTA$lat, 
    spatial.average = FALSE)

#Build profiles
zonal.wind      <- NULL
meridional.wind <- NULL 
height          <- NULL

for(k in 1:length(profile)) {
   hgt  <- profile[[k]]$profile.data[, which(profile[[k]]$variables == "hgtprs"),]
   ugrd <- profile[[k]]$profile.data[, which(profile[[k]]$variables == "ugrdprs"),]
   vgrd <- profile[[k]]$profile.data[, which(profile[[k]]$variables == "vgrdprs"),]

   synth.hgt <- seq(min(hgt),
       max(hgt), length.out = 1000)
   ugrd.spline <- splinefun(hgt, ugrd, method = "natural")
   vgrd.spline <- splinefun(hgt, vgrd, method = "natural")
   zonal.wind[[k]] <- ugrd.spline(synth.hgt)
   meridional.wind[[k]] <- vgrd.spline(synth.hgt)
   height[[k]] <- synth.hgt
}

#Plot them all
PlotWindProfile(zonal.wind, meridional.wind, height, lines = TRUE, 
    points = FALSE, elev.circles = c(0, 25000, 50000), elev.labels = c(0, 25, 50), 
    radial.lines = seq(45, 360, by = 45), colorbar = TRUE, invert = FALSE, 
    point.cex = 2, pch = 19, lty = 1, lwd = 1, 
    height.range = c(0, 50000), colorbar.label = "Wind Speed (m/s)")
}
}
\keyword{ hplot }
