\name{NOMADSRealTimeList}
\alias{NOMADSRealTimeList}
\title{
Models available for download through rNOMADS
}
\description{
Scans the NOMADS Real Time web site to generate a list of available model products.
Users can refer to this list to find out more information about the available models, and rNOMADS uses the abbreviations to determine which URLs to scan and download.
}
\usage{
NOMADSRealTimeList(url.type, abbrev = NULL) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url.type}{
  Determine whether to return a URL for extracting GRIB files (\code{"grib"}) or for getting ascii format data directly from the server (\code{"dods"}).}
  \item{abbrev}{
   Return information about the model that this abbreviation refers to.
   Defaults to \code{NULL}, in which case information about all the models available through \code{rNOMADS}.}
}

\value{
    \item{abbrevs}{An abbreviation for each model}
    \item{names}{A full name for each model}
    \item{urls}{The web address of the download page for each model}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}

\note{
A big thanks to user hrbrmstr at Stack Overflow for helping with the NCEP web site parsing code.
}
\seealso{
\code{\link{WebCrawler}}, \code{\link{ParseModelPage}}, \code{\link{NOMADSArchiveList}}, \code{\link{GribGrab}}, \code{\link{DODSGrab}}
}
\examples{

\dontrun{
#Grib filter
model.list <- NOMADSRealTimeList("grib")

#DODS interface
model.list <- NOMADSRealTimeList("dods")
}
}
\keyword{ documentation }
