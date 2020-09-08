\name{ArchiveGribGrab}
\alias{ArchiveGribGrab}
\title{
Download archived model data from the NOMADS server.
}
\description{
This function gives access to archived NOMADS model data.
The available models can be viewed by calling \code{\link{NOMADSArchiveList}} without arguments.
The data arrives in grib (gridded binary) format that can be read with \code{\link{ReadGrib}}.
Some of these files are in GRIB format, others are in GRIB2 format; select the appropriate file type when calling \code{\link{ReadGrib}}.
Note that this is a legacy function that emulates the archive data retrieval capability when NOMADS NCEP used to host past data sets.  
At present the data are hosted on NCEI.  
}
\usage{
ArchiveGribGrab(abbrev, model.date, model.run, preds,
    local.dir = NULL, file.names = NULL, tidy = FALSE,
    verbose = TRUE, download.method = NULL, file.type = "grib2")
}
\arguments{
  \item{abbrev}{
      Model abbreviation per \code{\link{NOMADSArchiveList}}.
}
  \item{model.date}{
      The year, month, and day of the model run, in YYYYMMDD format
}
   \item{model.run}{
       Which hour the model was run (i.e. 00, 06, 12, 18 for GFS)
}
  \item{preds}{
      A scalar or vector of prediction to get (analysis is 00)
  }
  \item{local.dir}{
  Where to save the grib file, defaults to the current directory.
}
  \item{file.names}{
  A list of length \code{length(preds)} of file names to which the 
  downloaded GRIB files are directed to.  
  Default \code{NULL}, meaning files will be named per their model date and time.
}
  \item{tidy}{
  If \code{TRUE}, remove all files with the suffix ".grb" from \code{local.dir} prior to downloading a new grib file.
}
  \item{verbose}{
  If \code{TRUE}, give information on connection status.  Default \code{TRUE}
}
 \item{download.method}{
  Allows the user to set the download method used by \code{download.file}: \code{"internal"}, \code{"wget"} \code{"curl"}, \code{"lynx"}.
  If \code{NULL} (the default), let R decide.
}
  \item{file.type}{
  Determine whether to get GRIB1 (\code{"grib1"}) or GRIB2 (\code{"grib2"}) file formats.
  Sometimes both are available, sometimes only one.
}
}
\value{
\item{grib.info$file.name}{The path and file name of the grib file that was downloaded.}
\item{grib.info$url}{The URL that the grib file was downloaded from}
}
\references{
\url{https://www.ncei.noaa.gov/data/}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}

\seealso{
\code{\link{CheckNOMADSArchive}}, \code{\link{NOMADSArchiveList}}, \code{\link{ReadGrib}}
}
\examples{
#An example for the Global Forecast System
#Get data for January 1 2014
#Temperature at 2 m above ground
#3 hour prediction
# using GRIB

abbrev <- "gfsanl"
model.date <- 20140101
model.run <- 06
preds <- 3

\dontrun{
model.info <- ArchiveGribGrab(abbrev, model.date, 
    model.run, preds, file.type = "grib2")
model.data <- ReadGrib(model.info[[1]]$file.name, c("2 m above ground"), c("TMP"))

#Get surface temperature in Chapel Hill, NC
lat <- 35.907605
lon <- -79.052147

profile <- BuildProfile(model.data, lon, lat, TRUE)
print(paste0("The temperature prediction in Chapel Hill was ", 
    sprintf("\%.0f", profile[[1]]$profile.data[1,1,1] - 272.15), " degrees Celsius."))
}
}
\keyword{ connection }
