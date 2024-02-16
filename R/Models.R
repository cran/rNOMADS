#Descriptions of real time and archived models
NOMADSRealTimeList <- function(url.type, abbrev = NULL) {
    #Returns a list of model abbreviations for real time models, a short description, and URL for each model offered by the NOMADS server
    #If a specific model abbreviation is requested, the abbreviation is checked against the model list.
    #If a match is found, information is returned about that model; otherwise an error occurs
    #
    #INPUTS
    #    URL.TYPE determines which URL to return: one for downloading GRIB files (grib) or one for downloading dods data via DODS (dods)
    #    ABBREV is the model abbreviation that rNOMADS uses to figure out which model you want.
    #        if NULL, returns information on all models
    #OUTPUTS
    #    MODEL.LIST - a list of model metadata with elements
    #        $ABBREV - the abbrevation used to call the model in rNOMADS
    #        $NAME - the name of the model
    #        $URL - the location of the model on the NOMADS website

    if (!(url.type %in% c("grib", "dods"))) {
        stop("URL type must be either \"grib\" or \"dods\"!")
    }

   prefix <- "https"

 
   base.url <- paste0(prefix, "://nomads.ncep.noaa.gov/")

   oldlocale <- Sys.setlocale()
   foo <- Sys.setlocale('LC_ALL','C')

   #Grab table rows
   row.regex <- "^\\s*<td.*center.*href.*txt_descriptions"
   row.regex <- "^\\s*<tr.*onClick.*</tr"

   #Read the website code...this is crude, sue me
   ncep.html <- paste(readLines(base.url, warn = FALSE), collapse = "")

   ncep.rows <- unlist(strsplit(ncep.html, "<tr>"))
   model.rows <- ncep.rows[which(grepl("onClick", ncep.rows))]

   #Get grib and dods models
   grib.filter <- c()
   grib.title  <- c()
   gds.alt     <- c()
   gds.title   <- c()

   for(k in 1:length(model.rows)) {

       grib.filter.tmp <- stringr::str_replace_all(
           stringr::str_extract(
              stringr::str_extract(model.rows[k], "\"gribfilter.*grib filter</a></td>"), 
            "\".*\""), 
         "\"", 
        "")

       gds.alt.tmp <- stringr::str_replace_all(
           stringr::str_extract(
               stringr::str_extract(model.rows[k],  "\"dods.*\">OpenDAP"), 
            "\".*\""), 
         "\"", 
        "")

       model.title <- stringr::str_replace(
              stringr::str_replace(
                      stringr::str_extract(model.rows[k], "\">.*</span"),
                   "\">", ""),
              "</span", "")
 

       if(!is.na(grib.filter.tmp)) {
          grib.title  <- c(grib.title, model.title)
          grib.filter <- c(grib.filter, grib.filter.tmp)
       }

       if(!is.na(gds.alt.tmp)) {
           gds.title <- c(gds.title, model.title)
           gds.alt <- c(gds.alt, gds.alt.tmp)
       }

   }

    
   grib.abbrevs <- stringr::str_replace(stringr::str_extract(grib.filter, "=.*$"), "=", "")
   dods.abbrevs <- stringr::str_replace(stringr::str_extract(gds.alt, "/.*$"), "/", "")
   dods.base.url <- paste0(prefix, "://nomads.ncep.noaa.gov:443/dods/")
   if(is.null(abbrev)) {
       if(url.type == "grib") {
          model.list <- list(abbrev = grib.abbrevs, 
               name = grib.title,
               url = paste0(base.url, 
               grib.filter))
       } else {
          model.list <- list(abbrev = dods.abbrevs,
              name = gds.title,
              url = paste0(dods.base.url, 
              basename(gds.alt), "/"))
       }
  } else {
      if(url.type == "grib") {
           abbrev.ind <- which(abbrev == grib.abbrevs)
           if(length(abbrev.ind) > 0) {
               model.list <- list(abbrev = grib.abbrevs[abbrev.ind], 
                   name = grib.title[abbrev.ind], 
                   url = paste0(base.url, grib.filter[abbrev.ind]))
           } else {
                stop(paste0("The model you searched for: \"", 
                    abbrev, 
                    "\" is not included in NOMADS real time ", 
                    url.type,
                     " model products.  Sorry!"))    
           }
      } else {
           abbrev.ind <- which(abbrev == dods.abbrevs)
           if(length(abbrev.ind) > 0) {
               model.list <- list(abbrev = dods.abbrevs[abbrev.ind], 
                   name = gds.title[abbrev.ind], 
                   url = paste0(dods.base.url, basename(gds.alt[abbrev.ind]), "/"))
           } else {
                stop(paste0("The model you searched for: \"", 
                    abbrev, 
                    "\" is not included in NOMADS real time ", 
                    url.type, 
                     " model products.  Sorry!"))
           }
       }
   }
   
   return(model.list)
}
   
NOMADSArchiveList <- function(abbrev = NULL) {
    #Returns a list of model abbreviations for archived models, a short description, and URL for each model offered by the server.
    #This is a legacy function meant to provide the models that used to exist on nomads.ncdc.noaa.gov.
    #They are now at ncei.noaa.gov. 
    #If a specific model abbreviation is requested, the abbreviation is checked against the model list.
    #If a match is found, information is returned about that model; otherwise an error occurs
    #INPUTS
    #    ABBREV is the model abbreviation that rNOMADS uses to figure out which model you want.
    #        If NULL, returns information on all models
    #OUTPUTS
    #    MODEL.LIST - a list of model metadata with elements
    #        $ABBREV - the abbrevation used to call the model in rNOMADS
    #        $NAME - the name of the model
    #        $URL - the location of the model on the NOMADS website

    abbrevs <- c(
        "gfsanl", 
        "namanl",
        "rapidrefresh"
       )

    names <- c(
        "Global Forecast System, Analysis",
        "North American Mesoscale, Analysis",
        "Rapid Refresh Model, Analysis" 
         )

    urls <- c( 
        "https://www.ncei.noaa.gov/data/global-forecast-system/access/historical/analysis/",
        "https://www.ncei.noaa.gov/data/north-american-mesoscale-model/access/historical/analysis/",
        "https://www.ncei.noaa.gov/data/rapid-refresh/access/historical/analysis/")

    if(!is.null(abbrev)) {
        i <- which(abbrevs == abbrev)
        if(length(i) == 0) {
            stop(paste("The model you searched for:\"", abbrev, "\"is not included in rNOMADS.  Sorry!"))
        } else {
            return(list(abbrev = abbrev, name = names[i], url = urls[i]))
        }
    }

    return(list(abbrevs = abbrevs, names = names, url = urls))
}
