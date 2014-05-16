\name{CheckNOMADSArchive}
\alias{CheckNOMADSArchive}
\title{
Check to see if archived data exists.
}
\description{
This function checks to see if data exists for a given date and model.
It checks for both GRIB1 or GRIB2 files.
}
\usage{
CheckNOMADSArchive(abbrev, model.date = NULL)
}
\arguments{
  \item{abbrev}{
      Model abbreviation per \code{\link{NOMADSArchiveList}}.
}
  \item{model.date}{
      The year, month, and day to check for data, in YYYYMMDD format.
      If \code{NULL}, check all available dates in NOMADS archive.
}
}

\value{
\item{available.models$date}{What date the file is for, in YYYYMMDD format.}
\item{available.models$model.run}{At what hour (GMT) the model was run.}
\item{available.models$pred}{What predictions are available}
\item{available.models$file.name}{List of file names for available model dates, runs, and predictions}
}
\references{
\url{http://nomads.ncdc.noaa.gov/}
}
\author{
Daniel C. Bowman \email{daniel.bowman@unc.edu}
}

\seealso{
\code{\link{NOMADSArchiveList}}, \code{\link{ArchiveGribGrab}}
}
\examples{
#See what is available for January 1 2014

abbrev <- "gfs4"
model.date <- 20140101
\dontrun{gfs.available.models <- CheckNOMADSArchive(abbrev, model.date)}

#Determine all available North American Mesoscale models in the archive
#This will take some time
\dontrun{nam.available.models <- CheckNOMADSArchive("namanl")}
}
\keyword{ utilities }
