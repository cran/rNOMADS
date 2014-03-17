\name{RTModelProfile}
\alias{RTModelProfile}
\title{
Get an atmospheric profile for a list of locations.
}
\description{
This routine simplifies the rapid generation of data for specific points on the Earth's surface.
}
\usage{
RTModelProfile(model.url, pred, levels, variables, lon, lat, resolution,
    grid.type, model.domain = NULL, spatial.average = FALSE, verbose = TRUE)
}
\arguments{
  \item{model.url}{
      The address of a model download page, probably from \code{\link{CrawlModels}}.
}
  \item{pred}{
      The requested model prediction. 
}
\item{levels}{
  A list of model levels to get for the profile.
}
  \item{variables}{
  A list of model variables to download.
}
  \item{lon}{
  Longitudes of points of interest.
}
  \item{lat}{
  Latitudes of points of interest.
}
  \item{resolution}{Resolution of model, in degrees if Lat/Lon, in kilometers if cartesian, as a 2 element vector (ZONAL, MERIDIONAL)
}
  \item{grid.type}{If the model is gridded in Lat/Lon or cartesian units.  Use \code{"latlon"} if Lat/Lon, \code{"cartesian"} if cartesian.
}
  \item{model.domain}{A four element vector of latitudes and longitudes that defines a rectangular area to get data for.
     If \code{NULL}, the model domain will be 1 degree past the maximum and minimum defined by \code{lon} and \code{lat} arguments.
}
  \item{spatial.average}{If \code{TRUE}, perform nearest neighbor interpolation for 4 grid nodes to get average profile at a specific point.
      If \code{FALSE}, get data from nearest grid node.
      Default \code{FALSE}.
}
   \item{verbose}{If \code{TRUE}, provide information on the download process.
       Default \code{TRUE}.
}
}
\details{
It is much more efficient to download a large chunk of data and extract profile points from that as opposed to downloading individual small model chunks in the vicinity of each point of interest.
That is why I developed this function.
}
\value{
\item{profile$profile.data}{Table of requested values, with rows corresponding to the requested levels.}
\item{profile$spatial.averaging}{What kind of spatial interpolation was used, if any, for the profile calculations.}
\item{profile$pred}{The model prediction used for generating the profile.}
\item{profile$model.date}{When the model was run.}
\item{profile$variables}{Model variables, in the order presented in \code{profile$profile.data}}
\item{profile$levels}{Model levels, in the order presented in \code{profile$profile.data}}
}
\author{
Daniel C. Bowman \email{daniel.bowman@unc.edu}
}
\seealso{
\code{\link{GetClosestGFSForecasts}}, \code{\link{BuildProfile}}
}
\examples{

#Get temperature profiles in Pantego, Chapel Hill, and Asheville, NC 

#First, define each location
lon <- c(-76.662819, -79.052094, -82.550011) 
lat <- c(35.589446, 35.907562, 35.591994)

#Get latest GFS 0.5 model, use analysis forecast
\dontrun{
model.url <- CrawlModels(abbrev = "gfs0.5", depth = 1)[1]
pred <- ParseModelPage(model.url)$pred[1]
}

#Get levels
pressure <- c(1, 2, 3, 5, 7,
10, 20, 30, 50, 70,
seq(100, 1000, by = 25))
levels <- paste(pressure, " mb", sep = "")

#Variables - temperature and height only
variables <- c("TMP", "HGT")

#Resolution of GFS is 0.5 x 0.5 degree
resolution <- c(0.5, 0.5)
grid.type <- "latlon"

#Get data
\dontrun{profile <- RTModelProfile(model.url, pred, levels, variables,
    lon, lat, resolution, grid.type, spatial.average = TRUE)

#Plot it
plot(c(-100, 50), c(0, 50000), type = "n", xlab = "Temperature (C)",
   ylab = "Height (m)", main = paste("GFS", profile$model.date, 
   "GMT Analysis Forecast"))

for(k in seq_len(3)) {
   points(profile$profile.data[[k]][,2] - 273.15,
       profile$profile.data[[k]][,1], pch = k, col = k)
}
legend("topright", pch = 1:3, col = 1:3, legend = c(
   "Pantego, NC", "Chapel Hill, NC", "Asheville, NC"))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}
