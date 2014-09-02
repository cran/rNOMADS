#Descriptions of real time and archived models
NOMADSRealTimeList <- function(url.type, abbrev = NULL) {
    #Returns a list of model abbreviations for real time models, a short description, and URL for each model offered by the NOMADS server
    #If a specific model abbreviation is requested, the abbreviation is checked against the model list.
    #If a match is found, information is returned about that model; otherwise an error occurs
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

    abbrevs <- c(
    "fnl",
    "gfs",
    "gfs_hd",
    "gfs_2p5",
    "gens",
    "gensbc_precip",
    "gensbc",
    "gensbc_ndgd",
    "naefsbc",
    "naefsbc_ndgd",
    "ngac_a2d",
    "ngac_a3d",
    "ngac_aod",
    "cfs_flx",
    "cfs_pgb",
    "aqm_daily",
    "aqm_ozone_1hr",
    "hiresak",
    "hireseast",
    "hiresguam",
    "hireshi",
    "hirespr",
    "hireswest",
    "nam_ak",
    "nam",
    "nam_na",
    "nam_crb",
    "nam_pac",
    "nam_alaskanest",
    "nam_conusnest",
    "nam_hawaiinest",
    "nam_priconest",
    "akrtma",
    "rtma",
    "rtma2p5",
    "gurtma",
    "hirtma",
    "prrtma",
    "rap",
    "rap32",
    "narre",
    "sref",
    "srefbc",
    "sref_na",
    "sref_132",
    "ofs",
    "rtofs_hires",
    "seaice",
    "wave",
    "glw",
    "wave_multi",
    "hurricane_wave",
    "nfcens",
    "estofs",
    "cmcens",
    "fens",
    "ncom")

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
    "NOAA Environmental Modeling System Global Forecast System Aerosol Component 2D",
    "NOAA Environmental Modeling System Global Forecast System Aerosol Component 3D",
    "NOAA Environmental Modeling System Global Forecast System Aerosol Optical Depth",
    "Air Quality Daily Maximum",
    "Air Quality Hourly Surface Ozone",
    "Climate Forecast System Flux Products",
    "Climate Forecast System 3D Pressure Products",
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
    "Operational Ocean Wave Predictions - Great Lakes",
    "Multi-grid Wave",
    "Hurricane Wave",
    "Combined Wave Ensemble",
    "Extratropical Surge and Tide Operational Forecast System",
    "Canadian Meterological Center Global Ensemble",
    "Fleet Numerical Meteorology and Oceanography Ensemble Forecast System",
    "U.S. Navy Operational Global Ocean Model") 

    if(!is.null(abbrev)) {
        i <- which(abbrevs == abbrev)
        if(length(i) == 0) {
            stop(paste("The model you searched for:\"", abbrev, "\"is not included in rNOMADS.  Sorry!"))
        } else {
            if(url.type == "grib") {
                 url <- paste0("http://nomads.ncep.noaa.gov/cgi-bin/filter_", abbrev, ".pl")
            } else {
                url <- paste0("http://nomads.ncep.noaa.gov:9090/dods/", abbrev, "/")
            }
             return(list(abbrev = abbrev, name = names[i], url = url))
        }
    }
    
    if(url.type == "grib") {
        url <- paste0("http://nomads.ncep.noaa.gov/cgi-bin/filter_", abbrevs, ".pl") 
    } else {
        url <- paste0("http://nomads.ncep.noaa.gov:9090/dods/", abbrevs, "/")
    }

    return(list(abbrevs = abbrevs, names = names, url = url))
}

NOMADSArchiveList <- function(url.type, abbrev = NULL) {
    #Returns a list of model abbreviations for archived models, a short description, and URL for each model offered by the NOMADS server
    #If a specific model abbreviation is requested, the abbreviation is checked against the model list.
    #If a match is found, information is returned about that model; otherwise an error occurs
    #INPUTS
    #    URL.TYPE determines which URL to return: one for downloading GRIB files (grib) or one for downloading dods data via DODS (dods)
    #    ABBREV is the model abbreviation that rNOMADS uses to figure out which model you want.
    #        If NULL, returns information on all models
    #OUTPUTS
    #    MODEL.LIST - a list of model metadata with elements
    #        $ABBREV - the abbrevation used to call the model in rNOMADS
    #        $NAME - the name of the model
    #        $URL - the location of the model on the NOMADS website

   if (!(url.type %in% c("grib", "dods"))) {
        stop("URL type must be either \"grib\" or \"dods\"!")
    }


    abbrevs <- c(
        "ruc",
        "ruc13",
        "meso-eta-hi",
        "gfs-avn-hi",
        "gfs4",
        "rap252",
        "rap130",
        "gfsanl",
        "rucanl",
        "namanl"
       )

    names <- c(
        "Rapid Update Cycle 20 km grid",
        "Rapid Update Cycle 13 km grid",
        "North American Mesoscale, Near Real Time",
        "Global Forecast System, Near Real Time, 1 degree grid",
        "Global Forecast System, Near Real Time, 0.5 degree grid",
        "Rapid Refresh Weather Prediction System - Near Real Time, 20 km grid",
        "Rapid Refresh Weather Prediction System - Near Real Time, 13 km grid",
        "Global Forecast System, Analysis",
        "Rapid Update Cycle, Analysis",
        "North American Mesoscale, Analysis"
         )

    dods.urls <- c(
        "http://nomads.ncdc.noaa.gov/dods/NCEP_RUC/",
        "NONE",
        "http://nomads.ncdc.noaa.gov/dods/NCEP_NAM/",
        "http://nomads.ncdc.noaa.gov/dods/NCEP_GFS/",
        "NONE",
        "http://nomads.ncdc.noaa.gov/dods/NCEP_RUC/",
        "NONE",
        "http://nomads.ncdc.noaa.gov/dods/NCEP_GFS_ANALYSIS/",
        "NONE",
        "http://nomads.ncdc.noaa.gov/dods/NCEP_NAM_ANALYSIS/"        
    )

    if(!is.null(abbrev)) {
        i <- which(abbrevs == abbrev)
        if(length(i) == 0) {
            stop(paste("The model you searched for:\"", abbrev, "\"is not included in rNOMADS.  Sorry!"))
        } else {
            if(url.type == "grib") {
                 url <- paste0("http://nomads.ncdc.noaa.gov/data/", abbrev, "/")
            } else {
                url <- dods.urls[i]
            }
             return(list(abbrev = abbrev, name = names[i], url = url))
        }
    }

    if(url.type == "grib") {
        url <- paste0("http://nomads.ncdc.noaa.gov/data/", abbrevs, "/")
    } else {
        url <- dods.urls
    }

    return(list(abbrevs = abbrevs, names = names, url = url))
}
