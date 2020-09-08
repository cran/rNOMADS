\name{SubsetNOMADS}
\alias{SubsetNOMADS}
\title{
Subset model data output
}
\description{
Subset data structure returned by \code{\link{ReadGrib}} or \code{\link{DODSGrab}} by variables, levels, etc.
}
\usage{
SubsetNOMADS(model.data, levels = NULL, variables = NULL, lon = NULL, 
lat = NULL, ensembles = NULL, forecast.date = NULL, model.run.date = NULL)
}
\arguments{
  \item{model.data}{
  Data structure from \code{\link{ReadGrib}} or \code{\link{DODSGrab}}}
  \item{levels}{
  Vector of levels to keep}
  \item{variables}{
  Vector of variables to keep}
  \item{lon}{
  Vector of longitudes of model nodes to keep}
  \item{lat}{
  Vector of latitudes of model nodes to keep}
  \item{ensembles}{
  Vector of ensemble runs to keep}
  \item{forecast.date}{
  Vector of forecast dates to keep}
  \item{model.run.date}{
  Vector of model run dates to keep} 
}
\value{
    \item{model.data.sub}{A subset of \code{model.data}.}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}

\note{
Multiple elements in each argument vector are obviously OR (i.e. variables are ``tmpprs'' OR ``hgtprs'') but multiple subset vectors are AND.
Thus it is simple to construct a \code{model.data.sub} with variables: tmpprs and hgptprs only from ensemble runs 3 and 4, for example.
}
\seealso{
\code{\link{ReadGrib}}, \code{\link{DODSGrab}}, \code{\link{ModelGrid}}, \code{\link{BuildProfile}}
}
\examples{

\dontrun{
#Plot winds from 20 GENS model runs

#Get the latest ensemble model run
model.urls <- GetDODSDates("gens")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
model.run <- tail(model.runs$model.run[grepl("all", 
    model.runs$model.run)], 1)

#Define region of interest: Chapel Hill, NC
lon <- -79.052104
lat <- 35.907553

lons <- seq(0, 359, by = 1)
lats <- seq(-90, 90, by = 1)

lon.diff <- abs(lon + 360 - lons)
lat.diff <- abs(lat - lats)

model.lon.ind <- which(lon.diff == min(lon.diff)) - 1
model.lat.ind <- which(lat.diff == min(lat.diff)) - 1

#Set up call to NOMADS
time <- c(0, 0)      #Analysis(?) model only
node.lon  <- c(model.lon.ind - 2, model.lon.ind + 2)  #Longitude grid
node.lat  <- c(model.lat.ind - 2, model.lat.ind + 2)   #Latitude grid
variables <- c("ugrdprs", "vgrdprs", "hgtprs") #Wind speeds, and geopotential height
levels    <- c(0, 25) #All available levels
ensembles <- c(0, 20)  #All available ensembles

model.data <- DODSGrab(latest.model, model.run, variables, time, 
    node.lon, node.lat, levels = levels, ensembles = ensembles)

#Plot winds
zonal.wind <- NULL
merid.wind <- NULL
height     <- NULL

for(k in ((ensembles[1]:ensembles[2] + 1))) {
    model.data.sub <- SubsetNOMADS(model.data, ensembles = c(k), 
    variables = c("hgtprs", "ugrdprs", "vgrdprs"))
    profile <- BuildProfile(model.data.sub, lon + 360, lat)
    hgt     <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "hgtprs"),]
    ugrd    <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "ugrdprs"),]
    vgrd    <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "vgrdprs"),]

   synth.hgt <- seq(min(hgt),
       max(hgt), length.out = 1000)
   ugrd.spline <- splinefun(hgt, ugrd, method = "natural")
   vgrd.spline <- splinefun(hgt, vgrd, method = "natural")
   zonal.wind[[k]] <- ugrd.spline(synth.hgt)
   merid.wind[[k]] <- vgrd.spline(synth.hgt)
   height[[k]] <- synth.hgt
}

PlotWindProfile(zonal.wind, merid.wind, height, lines = TRUE,
    points = FALSE, elev.circles = c(0, 15000, 30000), elev.labels = c(0, 15, 30),
    radial.lines = seq(45, 360, by = 45), colorbar = TRUE, invert = FALSE,
    point.cex = 2, pch = 19, lty = 1, lwd = 1,
    height.range = c(0, 30000), colorbar.label = "Wind Speed (m/s)")
}
}
\keyword{ manip }
