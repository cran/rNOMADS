\name{GetDODSModelRuns}
\alias{GetDODSModelRuns}
\title{
Find available model runs on the GrADS - DODS system.
}
\description{
Given a URL from \code{\link{GetDODSDates}}, find which model runs are available for download on the GrADS - DODS system.
}
\usage{
GetDODSModelRuns(model.url)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model.url}{
  A URL for a model on the GrADS - DODS system, probably returned by \code{\link{GetDODSDates}}.
}
}
\details{
This function determines which dates are available for download for a particular model through the GrADS - DODS system.
Once the user determines which dates are available, the output of this function can be passed to \code{\link{GetDODSModelRuns}} to determine which model runs can be downloaded.
}
\value{
\item{model.run}{A list of model runs available for the requested date.}
\item{model.run.info}{Information provided by the GrADS - DODS system about each model run.}
}
\author{
Daniel C. Bowman \email{danny.c.bowman@gmail.com}
}
\note{
To get model run information for archived analysis models, pass URLs directly from \code{\link{NOMADSArchiveList}} directly to \code{GetDODSModelRuns}.
}

\seealso{
\code{\link{GetDODSDates}}, \code{\link{DODSGrab}}, \code{\link{GetDODSModelRunInfo}}
}
\examples{

#An example for the Global Forecast System 0.5 degree model

#Get the latest model url and date, real time server
abbrev <- "gfs_0p50"
\dontrun{
urls.out <- GetDODSDates(abbrev)
model.url <- tail(urls.out$url, 1)
model.runs <- GetDODSModelRuns(model.url)
print(paste("Latest model run", tail(model.runs$model.run.info, 1)))
}
}
\keyword{ utilities }
