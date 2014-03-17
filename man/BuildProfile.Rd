\name{BuildProfile}
\alias{BuildProfile}
\title{
Get model data at a specific point.
}
\description{
Takes the output of \code{\link{ModelGrid}} and extracts data at a specific point, performing interpolation if required.
}
\usage{
BuildProfile(gridded.data, lon, lat, spatial.average)
}
\arguments{
  \item{gridded.data}{
      Data structure returned by \code{\link{ModelGrid}}.
}
  \item{lon}{
     Longitude of point of interest.
}
  \item{lat}{
     Latitude of point of interest.
}
  \item{spatial.average}{
     Whether to interpolate data using b-splines to obtain value at the requested point (\code{spatial.average = TRUE}) or use the nearest model node (\code{spatial.average = FALSE}).
}
}
\details{
It is much more efficient to download a large chunk of data and extract profile points from that as opposed to downloading individual small model chunks in the vicinity of each point of interest.
}
\value{
\item{profile.data}{A levels x variables matrix with data for a given point.}
}
\author{
Daniel C. Bowman \email{daniel.bowman@unc.edu}
}
\seealso{
\code{\link{ModelGrid}}, \code{\link{BuildProfile}}
}
\examples{

#Get temperature profile in Chapel Hill, NC.

#First, define each location
lon <- -79.052094
lat <- 35.907562

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

\dontrun{
grib.info <- GribGrab(model.url, pred, levels, variables,
   model.domain = c(-85, -75, 37, 32))
grib.data <- ReadGrib(grib.info$file.name, levels, variables)
gridded.data <- ModelGrid(grib.data, c(0.5, 0.5))

profile <- BuildProfile(gridded.data, lon, lat, TRUE)
plot(profile[,2] - 273.15, profile[,1], xlab = "Temperature (C)",
   ylab = "Height (m)", main = "Temperature Profile above Chapel Hill, NC")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}
