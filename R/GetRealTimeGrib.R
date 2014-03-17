CrawlModels <- function(abbrev = NULL, url = NULL, depth = NULL, verbose = TRUE) {
   #A simple web crawler that looks at the specified model directory online and gets information on all runs of the specified model.
   #See the NOMADSRealTimeList function for available models.
   #Alternatively, pass CrawlModels a URL to get a model that I have not included yet.
   #INPUTS
   #    ABBREV - Model abbreviation as defined in NOMADSRealTimeList().  
   #        If NULL, use the url you provided, if you did not provide one, throw error.
   #    URL - Use your own URL and attempt to get model data from it.  
   #        This is in case NOMADS updates its system before I have a chance to update rNOMADS
   #    DEPTH - How many links to return; set this to 1 if you only want the latest model (this will speed things up significantly)
   #    VERBOSE - Print each link you find as you find it
   #OUTPUTS
   #    URLS.OUT is a list of available models from the given ABBREV or URL

   if(is.null(url) & is.null(abbrev)) {
       stop("No models specified.")
   }
   
   if(is.null(url)) {
       model.info <- NOMADSRealTimeList(abbrev) 
       url <- model.info$url[1]
   }   

   urls.out <- unlist(WebCrawler(url, depth = depth, verbose = verbose), recursive = TRUE, use.names = FALSE) 
}

GribGrab <- function(model.url, pred, levels, variables, local.dir = ".", file.name = "fcst.grb", 
    model.domain = NULL, tidy = FALSE, verbose = TRUE, check.url = TRUE, download.method = NULL)
{
    #Get grib file from the GFS forecast repository
    #INPUTS
    #    MODEL.URL is the URL of the model, as one of the elements returned by CrawlModels
    #    PRED is the exact model run you want, generally from ParseModelPage
    #    LEVELS is the vertical region to return data for,  as vector, generally from ParseModelPage
    #    VARIABLES is the data to return, as vector, generally from ParseModelPage
    #    LOCAL.DIR is the directory to save the files in
    #    FILE.NAME is the directory path and file name to save the grib file on disk, defaults to "fcst.grb" in current directory
    #    MODEL.DOMAIN is a vector of latitudes and longitudes that specify the area to return a forecast for
    #    This is a rectangle with elements: west longitude, east longitude, north latitude, south latitude
    #    Defaults to entire planet
    #    TIDY asks whether to delete all grib files in the directory specified in FILE.NAME, default FALSE.
    #    This is useful to clear out previous model runs.
    #    It looks for all files named '.grb' and removes them.
    #    VERBOSE gives a blow by blow account of the download. Default TRUE.
    #    CHECK.URL verifies that MODEL.URL is real and contains variable and level data using ParseModelPage.
    #        If it finds that one or more levels and/or variables are missing from the URL, it throws a warning.
    #        If there is no level or variable information, or PRED is not in the list, it throws an error and exits
    #    DOWNLOAD.METHOD allows the user to set how download.file accesses the Grib file (e. g. using "internal", "wget", "curl", or "lynx"), defaults to NULL (let R decide)
    #OUTPUTS
    #    GRIB.INFO contains information about the downloaded file
    #        $LOCAL.DIR is the directory where the grib file is saved
    #        $FILE.NAME is the local file name where the data is stored
    #        $URL is the url used to retrieve the data 

   if(check.url) {
       test.scan <- ParseModelPage(model.url)
       if(length(test.scan$pred) == 0 & length(test.scan$levels) == 0 & length(test.scan$levels) == 0) {
           stop("There is no prediction, level, or variable data present in the specified model URL.
               Perhaps the model has not been loaded yet; try using an earlier model run.")
       }
       if(!(pred %in% test.scan$pred)) {
           stop(paste0("The requested prediction \"", pred, "\" was not found on the model download website."))
       }
       for(lvl in levels) {
           if(!(stringr::str_replace_all(lvl, " ", "_")  %in% test.scan$levels)) {
               warning(paste0("Requested level \"", lvl, "\" was not found on the model download website."))
           }
       }
       for(var in variables) {
           if(!(var %in% test.scan$var)) {
               warning(paste0("Requested variable \"", var, "\" was not found on the model download website."))
           }
       }
   }
   if(tidy) {
        unlink(list.files(local.dir, pattern = "*\\.grb$"))
   }


   model.str <- strsplit(model.url, "?dir=")[[1]]
   if(length(levels) > 0 & !is.null(levels)) {
        levels.str <- paste0("&lev_", paste(gsub(" ", "_", SanitizeURL(levels)), collapse = "=on&lev_"), "=on")
   } else {
       levels.str <- ""
   }
   variables.str <- paste(SanitizeURL(variables), collapse = "=on&var_")

   if(!is.null(model.domain)) {
       subregion.str <- paste("=on&subregion=",
       "&leftlon=", model.domain[1],
       "&rightlon=", model.domain[2],
       "&toplat=", model.domain[3],
       "&bottomlat=", model.domain[4],
       "&", sep = "")
    } else {
       subregion.str <- "=on&" 
    }

   grb.url <- paste0(paste0(model.str[1], "file=", pred),
       levels.str,
       "&var_",
       variables.str,
       subregion.str,
       paste0("dir=", model.str[2]))

   #now write download logic
   use.curl <- FALSE #May use this as an option in the future 
   if(is.null(download.method) & !use.curl) {#Let R decide how to download the file 
       download.file(grb.url, paste(local.dir,file.name, sep = "/"), mode = "wb", quiet = !verbose)
   }
   if(!is.null(download.method) & !use.curl) { #Download using specific method
       download.file(grb.url, paste(local.dir,file.name, sep = "/"), download.method, mode = "wb", quiet = !verbose) 
   }

   grib.info <- list(local.dir = normalizePath(local.dir), file.name = file.name, url = grb.url) 
   return(grib.info)
}

NOMADSRealTimeList <- function(abbrev = NULL) {
    #Returns a list of model abbreviations for real time models, a short description, and URL for each model offered by the NOMADS server
    #If a specific model abbreviation is requested, the abbreviation is checked against the model list.
    #If a match is found, information is returned about that model; otherwise an error occurs
    #INPUTS
    #    ABBREV is the model abbreviation that rNOMADS uses to figure out which model you want.
    #    if NULL, returns information on all models
    #OUTPUTS
    #    MODEL.LIST - a list of model metadata with elements
    #        $ABBREV - the abbrevation used to call the model in rNOMADS
    #        $NAME - the name of the model
    #        $URL - the location of the model on the NOMADS website

    abbrevs <- c(
    "fnl",
    "gfs1.0",
    "gfs0.5",
    "gfs2.5",
    "gfse_highres",
    "gfse_precip_biasc",
    "gfse_highres_biasc", 
    "gfse_ndgdres_biasc",
    "naefs_hires_biasc",
    "naefs_ndgdres_biasc",
    "ngac2d",
    "ngac3d",
    "ngac_aod",
    "aqm_dm",
    "aqm_hso",
    "hires_ak",
    "hires_econus",
    "hires_guam",
    "hires_hi",
    "hires_pr",
    "hires_wconus",
    "nam12_ak",
    "nam12_conus",
    "nam12_na",
    "nam12_carib_ca",
    "nam12_pa",
    "nam_nest_ak",
    "nam_nest_conus",
    "nam_nest_hi",
    "nam_nest_pr",
    "rtma_ak",
    "rtma_conus",
    "rtma_conus2.5",
    "rtma_guam",
    "rtma_hi",
    "rtma_pr",
    "rap",
    "rap_na32",
    "narre",
    "sref_conus",
    "sref_conus_bc",
    "sref_na32",
    "sref_na16",
    "rtofs_at",
    "rtofs_at_hires",
    "sea_ice",
    "wave",
    "gl_wave",
    "wave_mgrd",
    "wave_hur",
    "wave_nfc",
    "estofs",
    "cmc_en",
    "fnmoc_en")

    names <- c(
    "Final Operational Global Forecast System ",
    "Global Forecast System 1x1 Degree ",
    "Global Forecast System 0.5x0.5 Degree ",
    "Global Forecast System 2.5x2.5 Degree ",
    "Global Forecast System Ensemble",
    "Global Forecast System Ensemble Precipitation Bias Corrected ",
    "Global Forecast System Ensemble Bias Corrected ",
    "Global Forecast System Ensemble National Digital Guidance Database Bias Corrected ",
    "North American Ensemble Forecast System Bias Corrected ",
    "North American Ensemble Forecast System National Digital Guidance Database Bias Corrected ",
    "NOAA Environmental ing System Global Forecast System Aerosol Component 2D",
    "NOAA Environmental ing System Global Forecast System Aerosol Component 3D",
    "NOAA Environmental ing System Global Forecast System Aerosol Optical Depth",
    "Air Quality Daily Maximum",
    "Air Quality Hourly Surface Ozone",
    "High Res Window Alaska",
    "High Res Window - East Continental United States",
    "High Res Window - Guam",
    "High Res Window - Hawaii",
    "High Res Window - Puerto Rico",
    "High Res Window - West Continental United States", 
    "North American Mesoscale 12 km - Alaska",
    "North American Mesoscale 12 km - Continental United States",
    "North American Mesoscale 12 km - North America",
    "North American Mesoscale 12 km - Caribbean and Central America", 
    "North American Mesoscale 12 km - Pacific",
    "North American Mesoscale Nest - Alaska",
    "North American Mesoscale Nest - Continental United States",
    "North American Mesoscale Nest - Hawaii",
    "North American Mesoscale Nest - Puerto Rico",
    "Real-Time Mesoscale Analysis - Alaska",
    "Real Time Mesoscale Analysis - Continental United States",
    "Real Time Mesoscale Analysis - Continental United States 2.5 km Resolution",
    "Real Time Mesoscale Analysis - Guam",
    "Real Time Mesoscale Analysis - Hawaii",
    "Real Time Mesoscale Analysis - Puerto Rico",
    "Rapid Refresh Weather Prediction System",
    "Rapid Refresh Weather Prediction System - 32 km Resolution",
    "North American Rapid Refresh Ensemble",
    "Short Range Ensemble Forecast - Continental United States 40 km",
    "Short Range Ensemble Forecast - Continental United States 40 km Bias Corrected",
    "Short Range Ensemble Forecast - Continental United States 32 km",
    "Short Range Ensemble Forecast - Continental United States 16 km",
    "Real Time Ocean Forecast System - Atlantic",
    "Real Time Ocean Forecast System - Atlantic High Resolution",
    "Sea Ice",
    "Operational Ocean Wave Predictions",
    "Opreational Ocean Wave Predictions - Great Lakes",
    "Multi-grid Wave",
    "Hurricane Wave",
    "Combined Wave Ensemble",
    "Extratropical Surge and Tide Operational Forecast System",
    "Canadian Meterological Center Global Ensemble",
    "Fleet Numerical Meteorology and Oceanography Ensemble Forecast System") 

    urls <- c(
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_fnl.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_hd.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_2p5.pl", 
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_gens.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_gensbc_precip.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_gensbc.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_gensbc_ndgd.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_naefsbc.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_naefsbc_ndgd.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_ngac_a2d.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_ngac_a3d.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_ngac_aod.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_aqm_daily.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_aqm_ozone_1hr.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_hiresak.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_hireseast.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_hiresguam.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_hireshi.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_hirespr.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_hireswest.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_nam_ak.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_nam.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_nam_na.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_nam_crb.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_nam_pac.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_nam_alaskanest.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_nam_conusnest.pl", 
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_nam_hawaiinest.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_nam_priconest.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_akrtma.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_rtma.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_rtma2p5.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_gurtma.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_hirtma.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_prrtma.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_rap.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_rap32.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_narre.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_sref.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_srefbc.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_sref_na.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_sref_132.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_ofs.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_rtofs_hires.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_seaice.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_wave.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_glw.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_wave_multi.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_hurricane_wave.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_nfcens.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_estofs.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_cmcens.pl",
    "http://nomads.ncep.noaa.gov/cgi-bin/filter_fens.pl")

    if(!is.null(abbrev)) {
        i <- which(abbrevs == abbrev)
        if(length(i) == 0) {
            stop(paste("The model you searched for:\"", abbrev, "\"is not included in rNOMADS.  Sorry!"))
        } else {
             return(list(abbrev = abbrev, name = names[i], url = urls[i]))
        }
    }
    
    return(list(abbrevs = abbrevs, names = names, urls = urls))
}

ParseModelPage <- function(model.url) {
#    This function determines the available predictions, levels, and variables for a given model page.
#    It returns a list of these predictions, levels, and variables so that a call to the model can be constructed.
#    INPUTS
#        MODEL.URL is one of the model pages returned by CrawlModels
#    OUTPUTS
#        MODEL.PARAMETERS - a list with elements
#            MODEL.PARAMETERS$PRED - Individual "predictions" - i. e. individual outputs for each model instance
#            MODEL.PARAMETERS$LEVELS - the model levels
#            MODEL.PARAMETERS$VARIABLES - the types of data provided by the models

    html.code <- scrapeR::scrape(model.url, parse = FALSE)
    model.parameters <- list()
    model.parameters$pred <- gsub("option value=\"", "", stringr::str_extract_all(html.code[[1]], "option value=\"[^\"]+")[[1]])
    model.parameters$levels <- gsub("lev_", "", stringr::str_extract_all(html.code[[1]], "lev_[^\"]+")[[1]], fixed = TRUE)
    model.parameters$variables <- gsub("var_", "", stringr::str_extract_all(html.code[[1]], "var_[^\"]+")[[1]], fixed = TRUE)
     
    return(model.parameters)
}

SanitizeURL <- function(bad.strs) {
#    This function replaces illegal characters in levels and variables prior to constructing the model retrieval URL.
#    INPUTS
#        BAD.STRS - A vector of strings, possibly with illegal URL characters
#    OUTPUTS
#        GOOD.STRS - A vector of strings with illegal characters replaced by URL codes

   good.strs <- stringr::str_replace_all(bad.strs,  "\\^", "%5E")
   good.strs <- stringr::str_replace_all(good.strs,  "\\\\\\(", "%5C%28")
   good.strs <- stringr::str_replace_all(good.strs,  "\\\\\\)", "%5C%29")
   good.strs <- stringr::str_replace_all(good.strs,  "\\\\", "%5C")
   good.strs <- stringr::str_replace_all(good.strs, "=","%3D")
   good.strs <- stringr::str_replace_all(good.strs, "/", "%2F")

   return(good.strs)
}

WebCrawler <- function(url, depth = NULL, verbose = TRUE) {
#    This function recursively searches for links in the given url and follows every single link.
#    It returns a list of the final (dead end) URLs.
#    Many thanks to users David F and Adam Smith on stackoverflow for the link parser:
#    http://stackoverflow.com/questions/3746256/extract-links-from-webpage-using-r/3746290#3746290
#    INPUTS
#        URL is the url to start looking in
#    OUTPUTS
#        URLS.OUT are the URLs at the end of the road

    doc <- XML::htmlParse(url)
    links <- XML::xpathSApply(doc, "//a/@href")
    XML::free(doc)
    if(is.null(links)) {
        if(verbose) {
            print(url)
        }
        return(url)
    } else {
        urls.out <- vector("list", length = length(links))
        for(link in links) {
           if(!is.null(depth)) {
               if(length(unlist(urls.out)) >= depth) {
                   break
               }
            }
           urls.out[[link]] <- WebCrawler(link, depth = depth, verbose = verbose)
        }
        return(urls.out)
    }
}
