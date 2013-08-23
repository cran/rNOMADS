\name{GetModelRunHour}
\alias{GetModelRunHour}
\title{Get the time and date for the most recent GFS model run
}
\description{Determine the most recent GFS model run to a given date and time.
This ensures that the forecast to download is the most accurate and up to date.
}
\usage{
GetModelRunHour(model.date = Sys.time(), fcst.date = Sys.time(), 
    url.to.check = c(
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_hd.pl?dir=\%2Fgfs.", 
    "\%2Fmaster"), 
    attempts = 10)
}
\arguments{
  \item{model.date}{
  The date when the GFS model was run, in POSIX format.    
}
  \item{fcst.date}{
  The date to get the weather forecast for, in POSIX format.
}
  \item{url.to.check}{
  The URL that points to the model download page. 
}
  \item{attempts}{
  How many times to try contacting the model download page before giving up.
}
}
\details{
\code{\link{GribGrab}} calls \code{GetModelRunHour} to determine which model files to download.
\code{model.date} and \code{fcst.date} are in POSIX format (use \code{as.POSIXct}).
}
\value{
  \item{model.hour}{What GMT time the latest model run took place for the date specified in \code{model.date}.}
  \item{model.date}{The GMT date of the latest model run}
  \item{url.tested}{The URL that was checked to determine that the model hour is valid}
  \item{fcst.tdiff}{Time difference between model date and forecast date (i.e. how far in the future the forecast is from the model run that's available) in hours}
  \item{fcst.back}{The model forecast run immediately before the requested forecast date}
  \item{fcst.fore}{The model forecast run immediately after the requested forecast date.}
}
\references{
\url{http://nomads.ncep.noaa.gov/}
}
\author{
Daniel Bowman \email{daniel.bowman@unc.edu}
}
\note{
This function requires an Internet connection to work.
}

\seealso{
\code{\link{GribGrab}}
}
\examples{

#Get the latest model run
model.date <- Sys.time() 

#Get the forecast 6 hours from now - addition is defined in seconds
fcst.date <- model.date + 6 * 3600 

#Figure out what model files to download
model.params <- GetModelRunHour(model.date = model.date, fcst.date = fcst.date)

print(model.params$model.hour) #When the GFS model was run last
print(model.params$fcst.back) #The forecast hour before the requested forecast date
print(model.params$fcst.fore) #The forecast hour after the requested forecast date
}
\keyword{ connection }
