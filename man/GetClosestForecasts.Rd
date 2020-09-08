\name{GetClosestForecasts}
\alias{GetClosestForecasts}
\title{
Get the forecast time closest to a given date for a given model
}
\description{
This function returns which forecast precedes the date and which forecast follows the date for a given model product.
Thus a user can average the two forecasts together to provide a precise forecast for a given date.
It is optimized for grib file retrieval.
}
\usage{
GetClosestForecasts(abbrev, forecast.date, model.date = "latest",
   depth = NULL, verbose = TRUE)
}
\arguments{
  \item{abbrev}{
  The requested model product
}
  \item{forecast.date}{
   What date you want a forecast for, as a date/time object.
   It must be in the UTC time zone.
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
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}
\seealso{
\code{\link{BuildProfile}}, \code{\link{GribGrab}}
}
\examples{

#Get the exact temperature profile of Chapel Hill, NC
#by performing a weighted average of GFS model forecasts.

#Figure out which forecasts to use
forecast.date <- as.POSIXlt(Sys.time(), tz = "UTC")
abbrev <- "gfs_0p50"

\dontrun{
forecasts <- GetClosestForecasts(abbrev = abbrev, forecast.date)
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
model.domain <- c(lon - 1, lon + 1, lat + 1, lat - 1)

\dontrun{
#Get the data for each
grb.info <- GribGrab(forecasts$model.url, 
   c(forecasts$fore.forecast, forecasts$back.forecast), levels, variables, 
   model.domain = model.domain)

fore.data <- ReadGrib(grb.info[[1]]$file.name, levels, variables) 
back.data <- ReadGrib(grb.info[[2]]$file.name, levels, variables)

back.profile <- BuildProfile(back.data, lon, lat,
   spatial.average = TRUE, points = 8)

fore.profile <- BuildProfile(fore.data, lon, lat,
   spatial.average = TRUE, points = 8)

temps <- cbind(back.profile[[1]]$profile.data[, which(back.profile[[1]]$variables == "TMP"),],
    fore.profile[[1]]$profile.data[, which(fore.profile[[1]]$variables == "TMP"),])
 
heights <-  cbind(back.profile[[1]]$profile.data[, which(back.profile[[1]]$variables == "HGT"),],
    fore.profile[[1]]$profile.data[, which(fore.profile[[1]]$variables == "HGT"),])


time.gap <- forecasts$fore.hr - forecasts$back.hr
exact.temp <- (temps[,1] * abs(forecasts$fore.hr) + temps[,2] * abs(forecasts$back.hr))/time.gap
exact.hgt <- (heights[,1] * abs(forecasts$fore.hr) + heights[,2] * abs(forecasts$back.hr))/time.gap

#Plot results
plot(c(min(temps), max(temps)), c(min(heights), max(heights)), type = "n",
    xlab = "Temperature (C)", ylab = "Height (m)")
points(temps[,1], heights[,1], pch = 1, col = 1)
points(temps[,2], heights[,2], pch = 2, col = 2)
points(exact.temp, exact.hgt, col = 3, lty = 2, pch = 3)
legend("topleft", pch = c(1, 2, 3), col = c(1, 2, 3),
   legend = c(forecasts$back.forecast, forecasts$fore.forecast, as.character(Sys.time())))
}
}
\keyword{ chron}
