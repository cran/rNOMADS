\name{DODSGrab}
\alias{DODSGrab}
\title{
Download model data from the NOMADS server using the GrADS-DODS system.
}
\description{
This function interfaces with the NOMADS server to download weather, ocean, and sea ice data.
The available models can be viewed by calling \code{\link{NOMADSRealTimeList}} and \code{\link{NOMADSArchiveList}}.
The data arrives in ascii format, so this function can be used to retrieve data on any operating system.
}
\usage{
DODSGrab(model.url, model.run, variables, time, lon, lat,
    levels = NULL, ensembles = NULL, display.url = TRUE, 
    verbose = FALSE, request.sleep = 1)
}
\arguments{
  \item{model.url}{
      A model URL for a specific date, probably from \code{\link{GetDODSDates}}.
}
  \item{model.run}{
      A specific model run to get, probably from \code{\link{GetDODSModelRuns}}.
}
  \item{variables}{
  A list of the data types to get.
}
  \item{time}{
  A two component vector denoting which time indices to get.
}
  \item{lon}{
  A two component vector denoting which longitude indices to get.
}
  \item{lat}{
  A two component vector denoting which latitude indices to get.
}
  \item{levels}{
  A two component vector denoting which levels to get, if applicable.
}
  \item{ensembles}{
  A two component vector denoting which ensemble runs to get, if applicable.
}

  \item{display.url}{
    If \code{TRUE}, print out the URL for the data request.
} 
  \item{verbose}{
    If \code{TRUE}, give a very detailed description of the download.  Default \code{FALSE}.
}
  \item{request.sleep}{
   If multiple requests are to be sent to the server, pause by this many seconds between them.
   This is courteous and also helps prevent timeouts.
}
}
\value{
\item{model.data}{A structure with a series of elements containing data extracted from GrADS-DODS system.}
}
\references{
\url{http://nomads.ncep.noaa.gov/}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}

\seealso{
\code{\link{GetDODSDates}}, \code{\link{GetDODSModelRuns}}, \code{\link{GetDODSModelRunInfo}}
}
\examples{

#An example for the Global Forecast System 0.5 degree model
#Make a world temperature map for the latest model run

\dontrun{
#Figure out which model is most recent
model.urls <- GetDODSDates("gfs_0p50")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)

#Download worldwide temperature data at 2 m
variable <- "tmp2m"
time <- c(0, 0) #Analysis run, index starts at 0
lon <- c(0, 719) #All 720 longitude points
lat <- c(0, 360) #All 361 latitude points
model.data <- DODSGrab(latest.model, latest.model.run,
   variable, time, lon, lat)

#Make it into a nice array and plot it
model.grid <- ModelGrid(model.data, c(0.5, 0.5))
image(model.grid$z[1,1,,])
}
}

\keyword{ connection }
