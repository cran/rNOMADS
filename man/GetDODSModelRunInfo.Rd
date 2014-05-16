\name{GetDODSModelRunInfo}
\alias{GetDODSModelRunInfo}
\title{
Get model coverage and data information for models on GrADS-DODS system.
}
\description{
Given a URL from \code{\link{GetDODSDates}}, find which model runs are available for download on the GrADS - DODS system.
}
\usage{
GetDODSModelRunInfo(model.url, model.run)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model.url}{
  A URL for a model on the GrADS - DODS system, probably returned by \code{\link{GetDODSDates}}.
}
  \item{model.run}{
   A specific model run, probably returned by \code{link{GetDODSModelRuns}}
}
}
\details{
This routine grabs information about the latitude, longitude, and time coverage of a specific model instance.
It also finds data about levels (if present) and lists all the available variables (though they may not have data in them).
The user can refer to this information to construct calls to the DODS system via \code{\link{DODSGrab}}.
}
\value{
\item{model.info}{Information provided by the GrADS - DODS system about the given model instance.}
}
\author{
Daniel C. Bowman \email{daniel.bowman@unc.edu}
}

\seealso{
\code{\link{GetDODSDates}}, \code{\link{GetDODSModelRuns}}, \code{\link{DODSGrab}}
}
\examples{

#An example for the Global Forecast System 0.5 degree model

#Get some information about the latest model url and date, real time server
abbrev <- "gfs_hd"
\dontrun{
urls.out <- GetDODSDates(abbrev)
model.url <- tail(urls.out$url, 1)
model.runs <- GetDODSModelRuns(model.url)
model.info <- GetDODSModelRunInfo(model.url, tail(model.runs$model.run, 1))
print(model.info)}
}
\keyword{ utilities }
