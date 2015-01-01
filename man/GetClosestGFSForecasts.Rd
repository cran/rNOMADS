\name{GetClosestGFSForecasts}
\alias{GetClosestGFSForecasts}
\title{
Get the GFS forecast time closest to a given date
}
\description{
This function determines which GFS forecast is closest to a given date.
It returns which forecast precedes the date, and which forecast follows the date.
Thus a user can average the two forecasts together to provide a precise forecast for a given date.
}
\usage{
GetClosestGFSForecasts(forecast.date, model.date = "latest",
   depth = NULL, verbose = TRUE)
}
\arguments{
  \item{forecast.date}{
   What date you want a forecast for, as a date/time object.
   It must be in the GMT time zone.
}
\item{model.date}{
   Which model run to use, in YYYYMMDDHH, where HH is 00, 06, 12, 18. 
   Defaults to \code{"latest"}, which gets the most recent model uploaded to the server.
}
\item{depth}{
  How many model instances to return.
  This avoids having to download the entire model list (sometimes several hundred) if only the first few instances are required.
  Defaults to \code{NULL}, which returns everything.
  This input only makes sense when \code{model.date != "latest"}.
}

\item{verbose}{
  Gives a detailed account of progress.
  Defaults to \code{TRUE}.
}
}

\value{
\item{forecasts$model.url}{URL to send to \code{GribGrab} for downloading data.}
\item{forecasts$model.run.date}{When the model was run.}
\item{forecasts$back.forecast}{Nearest forecast behind requested date.}
\item{forecasts$fore.forecast}{Nearest forecast after requested date.}
\item{forecasts$back.hr}{How many hours the back forecast is behind the requested date.}
\item{forecasts$fore.hr}{How many hours the fore forecast is in front of the requested date.}
}
\author{
Daniel C. Bowman \email{daniel.bowman@unc.edu}
}
\seealso{
\code{\link{RTModelProfile}}, \code{\link{BuildProfile}}, \code{\link{GribGrab}}
}
\examples{

#Get the exact temperature profile of Chapel Hill, NC
#by performing a weighted average of GFS model forecasts.

#Figure out which forecasts to use
forecast.date <- as.POSIXlt(Sys.time(), tz = "GMT")

\dontrun{
forecasts <- GetClosestGFSForecasts(forecast.date)
}

#Get levels
pressure <- c(1, 2, 3, 5, 7,
10, 20, 30, 50, 70,
seq(100, 1000, by = 25))
levels <- paste(pressure, " mb", sep = "")

#Variables - temperature and height only
variables <- c("TMP", "HGT")

#Location
lon <- c(-79.052083)
lat <- c(35.907492)

#Get the data for each
resolution <- c(0.5, 0.5)
grid.type <- "latlon"

\dontrun{
back.profile <- RTModelProfile(forecasts$model.url, forecasts$back.forecast,
   levels, variables, lon, lat, resolution = resolution, grid.type = grid.type)

fore.profile <- RTModelProfile(forecasts$model.url, forecasts$fore.forecast,
   levels, variables, lon, lat, resolution = resolution, grid.type = grid.type)

temps <- cbind(back.profile$profile[[1]][,2], fore.profile$profile[[1]][,2]) - 273.15
heights <- cbind(back.profile$profile[[1]][,1], fore.profile$profile[[1]][,1])
time.gap <- forecasts$fore.hr - forecasts$back.hr
exact.temp <- (temps[,1] * abs(forecasts$fore.hr) + temps[,2] * abs(forecasts$back.hr))/time.gap
exact.hgt <- (heights[,1] * abs(forecasts$fore.hr) + heights[,2] * abs(forecasts$back.hr))/time.gap

#Plot results
plot(c(min(temps), max(temps)), c(min(heights), max(heights)), type = "n",
    xlab = "Temperature (C)", ylab = "Height (m)")
points(temps[,1], heights[,1], pch = 1, col = 1)
points(temps[,2], heights[,2], pch = 2, col = 2)
lines(exact.temp, exact.hgt, col = 3, lty = 2)
legend("topleft", pch = c(1, 2, NA), lty = c(NA, NA, 2), col = c(1, 2, 3),
   legend = c(forecasts$back.forecast, forecasts$fore.forecast, as.character(Sys.time())))
}
}
\keyword{ chron}
