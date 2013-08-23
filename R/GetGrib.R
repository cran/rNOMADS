GribGrab <- function(levels, variables, which.fcst = "back", local.dir = ".", file.name = "fcst.grb", model.date = Sys.time(), fcst.date = Sys.time(), 
    model.domain = NULL, tidy = FALSE, verbose = TRUE)
{
    #Get grib file from the GFS forecast repository
    #INPUTS
    #    LEVELS is the vertical region to return data for,  as vector
    #    VARIABLES is the data to return, as vector
    #    WHICH.FCST determines if you want the forecast BEFORE the requested time ("back") or AFTER the requested time ("forward"), defaults to "back"
    #    LOCAL.DIR is the directory to save the files in
    #    FILE.NAME is the directory path and file name to save the grib file on disk, defaults to "fcst.grb" in current directory
    #    MODEL.DATE is the date and time of the requested model run, in GMT and POSIXlt format, defaults to current system time
    #    FCST.DATE is the requested forecast date, defaults to current system time
    #    MODEL.DOMAIN is a vector of latitudes and longitudes that specify the area to return a forecast for
    #    This is a rectangle with elements: west longitude, east longitude, north latitude, south latitude
    #    Defaults to entire planet
    #    LEVELS is the vertical region to return data for
    #    VARIABLES is the data to return
    #    TIDY asks whether to delete all grib files in the directory specified in FILE.NAME, default FALSE.
    #    This is useful to clear out previous model runs.
    #    It looks for all files named '.grb' and removes them.
    #    VERBOSE gives a blow by blow account of the download. Default TRUE.
    #OUTPUTS
    #    FILE.NAME is the name and location of the grib file that has been downloaded 

   if(tidy) {
        unlink(list.files(local.dir, pattern = "*\\.grb$"))
   }
   levels.str <- paste(gsub(" ", "_", levels), collapse = "=on&lev_")
   variables.str <- paste(variables, collapse = "=on&var_")

   #Convert dates to GMT
   
    model.date <- as.POSIXlt(model.date, tz = "GMT")
    fcst.date <- as.POSIXlt(fcst.date, tz = "GMT")

   #Check for latest model run date
   model.params <- GetModelRunHour(model.date = model.date, fcst.date = fcst.date) 
   if(is.na(model.params$model.hour)) {
       stop("Could not find the latest model run date.  Make sure you have a working Internet connection.  If you do and this code is still not working, it may be that the NOMADS website is down.
           Give it a an hour or so, and try again.")
   }
   if(model.params$model.date > fcst.date) {
      stop("The reqested model date is after the requested forecast date! This means you are trying to access a forecast from a model that has not been run yet.")
   }
   if(which.fcst == "back") {
       fcst.date <- as.POSIXlt(model.params$model.date + 3600 * model.params$fcst.back)
       grb.name <- paste("gfs.t", sprintf("%02d", model.params$model.hour), "z.mastergrb2f",
       sprintf("%02d", model.params$fcst.back), "&", sep = "")
   } else if (which.fcst == "forward") { 
       fcst.date <- as.POSIXlt(model.params$model.date + 3600 * model.params$fcst.fore)
       grb.name <- paste("gfs.t", sprintf("%02d", model.params$model.hour), "z.mastergrb2f",
       sprintf("%02d", model.params$fcst.fore), "&", sep = "")
   } else {
       stop(paste("Did not recognize the forecast designation ", 
       which.fcst, 
       ".  Please use either \"back\" for the nearest forecast time BEFORE the requested time, 
       or \"forward\" for the nearest forecast time AFTER the requested time.", sep = ""))
   }
   grb.dir <- paste("dir=", strsplit(model.params$url.tested, split = "dir=")[[1]][2], sep = "")

   if(!is.null(model.domain)) {
       subregion.str <- paste( "=on&subregion=",
       "&leftlon=", model.domain[1],
       "&rightlon=", model.domain[2],
       "&toplat=", model.domain[3],
       "&bottomlat=", model.domain[4],
       "&", sep = "")
    } else {
       subregion.str <- "=on&" 
    }

   grb.url <- paste("http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_hd.pl?file=",
       grb.name,
       "lev_",
       levels.str,
       "=on&var_",
       variables.str,
       subregion.str,
       grb.dir,
       sep = "") 

      #now write download logic

   download.file(grb.url, paste(local.dir,file.name, sep = "/"), mode = "wb", quiet = !verbose)
   
   return(list(file.name = file.name, model.date = model.params$model.date, fcst.date = fcst.date))
}

NoModelRun <- function(e) 
{
    #Called when code in GetModelRunDate tries to ping a GFS model that has not been run yet
    return ("Failure")
}

GetModelRunHour <- function(model.date = Sys.time(), fcst.date = Sys.time(),
    url.to.check = c("http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_hd.pl?dir=%2Fgfs.", "%2Fmaster"), attempts = 10)
{
    #Checks for and returns the date for the latest model run time for the requested date.
    #By default, check for the system time, and get the closest forecast.
    #If the input is not the current time, get the model forecast closest behind the requested date.
    #
    #INPUTS
    #    MODEL.DATE - a date in POSIXct format saying which GFS model date to use (i.e. when the model was run).  Defaults to the current system time
    #    FCST.DATE = a date in POSIXct format saying what date the forecast should be for.  Defaults to the current system time.
    #    URL.TO.CHECK - what URL to append the GFS formatted model run time to
    #    We use this URL to check if the model has run yet.
    #    Perhaps, if the default fails, the user can modify this argument to make things work
    #    ATTEMPTS - number of model runs to check for before giving up
    #
    #OUTPUTS as list
    #    MODEL.HOUR - The model run hour to download
    #    MODEL.DATE - the full date of the model run
    #    URL.TESTED - The url that was tested to determine the model hour
    #    FCST.TDIFF - Time difference between model date and forecast date (i.e. how far in the future the forecast is from the model run that's available) in hours 
    #    FCST.BACK - The model forecast run immediately before the requested forecast date, in hours, in case that grib file is desired
    #    FCST.FORE - The model forecast run immediately after the requested forecast date, in hours, in case that grib file is desired
    

    model.hour <- seq(0, 18, by = 6)
    fcst.hour <- seq(0, 192, by = 3)
    #Convert to GMT
    model.date <- as.POSIXlt(model.date, tz = "GMT")
    fcst.date <- as.POSIXlt(fcst.date, tz = "GMT")
    
    c = 1
     
    while (1)
    {
       yr <- model.date$year + 1900
       mo <- model.date$mo + 1
       mday <- model.date$mday
       hr <- model.date$hour
       
       hr.diff <- model.hour - hr
       latest.model.run <- model.hour[hr.diff == max(hr.diff[hr.diff <= 0])]
       model.date <- as.POSIXlt(ISOdatetime(yr, mo, mday, latest.model.run, 0, 0, tz = "GMT"))   
       fcst.url <- paste(url.to.check[1], yr, sprintf("%02d", mo), sprintf("%02d", mday), sprintf("%02d", latest.model.run), url.to.check[2], sep = "")
       test <- suppressWarnings(tryCatch(url(fcst.url, open = "rb"), error = NoModelRun))
       
       if(test == "Failure") {
           model.date = as.POSIXlt(model.date - 3600 * 6) #Subtract 6 hours and try again
       }
       else {
           close(test)
           fcst.tdiff <- as.numeric(difftime(fcst.date, model.date, units = "hours"))
           fcst.hour.diff <- fcst.hour - fcst.tdiff
           fcst.back <- fcst.hour[fcst.hour.diff == max(fcst.hour.diff[fcst.hour.diff <=0])]
           fcst.fore <- fcst.hour[fcst.hour.diff == min(fcst.hour.diff[fcst.hour.diff >=0])]
           break
       }

      if (c > attempts) {
          model.hour <- NA
          break
      } 
      c <- c + 1
   } 
   return (list(model.hour = latest.model.run, model.date = as.POSIXlt(ISOdatetime(yr, mo, mday, latest.model.run, 0, 0, tz = "GMT")), 
        url.tested = fcst.url, fcst.tdiff = fcst.tdiff, fcst.back = fcst.back, fcst.fore = fcst.fore))
}
