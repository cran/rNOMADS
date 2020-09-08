\name{BuildProfile}
\alias{BuildProfile}
\title{
Get model data at a specific point.
}
\description{
Takes the output of \code{\link{ReadGrib}} or \code{\link{DODSGrab}} and extracts data at a specific point, performing interpolation if required.
}
\usage{
BuildProfile(model.data, lon, lat, spatial.average, points = 4)
}
\arguments{
  \item{model.data}{
      Data structure returned by \code{\link{ReadGrib}} or \code{\link{DODSGrab}}.
}
  \item{lon}{
     Longitudes of points of interest.
}
  \item{lat}{
     Latitudes of points of interest.
}
  \item{spatial.average}{
     Whether to interpolate data using b-splines to obtain value at the requested point (\code{spatial.average = TRUE}) or use the nearest model node (\code{spatial.average = FALSE}).
}
   \item{points}{
       How many points to include in the interpolation, will be ignored if \code{spatial.average = FALSE}.
}
}
\details{
It is much more efficient to download a large chunk of data and extract profile points from that as opposed to downloading individual small model chunks in the vicinity of each point of interest.
}
\value{
\item{profile}{A list with as many elements as profile points, with fields:}
\item{profile$profile.data}{A levels x variables x time matrix with data at a specific point}
\item{profile$location}{The location for which the profile is generated}
\item{profile$forecast.date}{What dates and times the profile contains}
\item{profile$variables}{The variables the profile contains}
\item{profile$levels}{The levels the profile contains}
}
\note{
The ``outside of model domain'' warning may occur if the profile point is very close to the prime meridian (in the case of a 0-360 longitude definition) or the antipode of the prime meridian (+/- 180 degrees longitude in the case of -180 to 180 degrees definition).
For a full download of a global model (e. g. the GFS), this does not mean the point is actually outside of the model domain.
For example, the 0.5 degree GFS has nodes from 0 to 359.5 degrees longitude; a profile point requested at 359.9 degrees longitude will cause this warning even though it is within the global GFS model domain.
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}
\seealso{
\code{\link{ReadGrib}}, \code{\link{DODSGrab}}
}
\examples{

#Get temperature profile in Chapel Hill, NC.

#First, define each location
lon <- -79.052094
lat <- 35.907562

#Get second to latest GFS 0.5 model, use analysis forecast
#(this ensures the data's fully up on NOMADS)

\dontrun{
model.url <- CrawlModels(abbrev = "gfs_0p50", depth = 2)[2]
pred <- ParseModelPage(model.url)$pred[1]
}

#Get levels
pressure <- c(1, 2, 3, 5, 7,
10, 20, 30, 50, 70,
seq(100, 1000, by = 25))
levels <- paste(pressure, " mb", sep = "")

#Variables - temperature and height only
variables <- c("TMP", "HGT")

\dontrun{
grib.info <- GribGrab(model.url, pred, levels, variables,
   model.domain = c(-85, -75, 37, 32))
grib.data <- ReadGrib(grib.info[[1]]$file.name, levels, variables)

profile <- BuildProfile(grib.data, lon, lat, TRUE, points = 8)
plot(profile[[1]]$profile.data[,2, 1] - 272.15, 
   profile[[1]]$profile.data[,1, 1], xlab = "Temperature (C)",
   ylab = "Height (m)", main = "Temperature Profile above Chapel Hill, NC")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}
