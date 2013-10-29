ReadGrib <- function(file.name, levels, variables) {
    #This is a function to read forecast data from a Grib file
    #INPUTS
    #    FILE.NAME - Grib file name
    #    VARIABLES - data to extract
    #    LEVELS - which levels to extract data from
    #OUTPUTS
    #    MODEL.DATA - the grib model as an array, with columns for the model run date (when the model was run)
    #       the forecast (when the model was for), the variable (what kind of data), the level (where in the atmosphere or the Earth, vertically)
    #       the longitude, the latitude, and the value of the variable.

    #Get specified data from grib file

    match.str <- ' -match "('
    for(var in variables) {
        match.str <- paste(match.str, var, "|", sep = "")
    }

    match.str.lst <- strsplit(match.str, split = "")[[1]]
    match.str <- paste(match.str.lst[1:(length(match.str.lst) - 1)], collapse = "")

    if(length(levels) > 0 & !is.null(levels)) {
        match.str <- paste(match.str, "):(", sep = "")
        for(lvl in levels) {
            match.str <- paste(match.str, lvl, "|", sep = "")
        }
    } else {
        match.str <- paste0(match.str, ")")
   }

    match.str.lst <- strsplit(match.str, split = "")[[1]]
    match.str <- paste(match.str, '"', sep = "")
    match.str <- paste(match.str.lst[1:(length(match.str.lst) - 1)], collapse = "")
    match.str <- paste(match.str, ")\"", sep = "")

    wg2.str <- paste('wgrib2 ',     
        file.name, ' -inv my.inv -csv - -no_header', 
        match.str, sep = "")
    
    #Get the data from the grib file in CSV format
    csv.str <- system(wg2.str, intern = TRUE)

    #HERE IS THE EXTRACTION
    model.data.vector <- strsplit(paste(gsub("\"", "", csv.str), collapse = ","), split = ",")[[1]]
    model.data <- t(array(model.data.vector, dim = c(7, length(model.data.vector)/7)))
    colnames(model.data) <- c("model.run.date", "forecast.date", "variable", "level", "lon", "lat", "value")
    return(model.data)
}

ModelGrid <- function(model.data, levels = NULL, variables = NULL, model.domain = NULL) {
    #Transform model data array into a grid with dimensions levels x variables x lon range x lat range
    #This should reduce the size of the returned data by removing redundant information
    #INPUTS
    #    MODEL.DATA - Data returned by ReadGrib
    #    VARIABLES - variables to include in grid, if NULL, include all of them
    #    LEVELS - levels to include in grid, if NULL, include all of them
    #    MODEL.DOMAIN - vector c(LEFT LON, RIGHT LON, TOP LAT, BOTTOM LAT) of region to include in output. If NULL, include everything.
    #
    #OUTPUTS
    #   FCST.GRID - A list with elements:
    #       $Z An array of dimensions levels x variables x lon x lat; each level x variable contains the model grid of data from that variable and level
    #       $X Vector of longitudes
    #       $Y Vector of latitudes
    #       $VARIABLES - the variables contained in the grid
    #       $LEVELS - the levels in the grid
    #       $MODEL.RUN.DATE - when the forecast model was run
    #       $FCST.DATE - what date the forecast is for
  
    model.run.date <- unique(model.data[,1])

    lat.grid <- unique(round(diff(as.numeric(sort(unique(model.data[,6]))))))
    lon.grid <- unique(round(diff(as.numeric(sort(unique(model.data[,5])))))) 

    if(length(model.run.date) > 1) {
        warning("There appears to be more than one model run date in your model grid!")
    }

    fcst.date <- unique(model.data[,2])

    if(length(fcst.date) > 1) {
        warning("There appears to be more than one model run date in your model grid!")
    }

    data.grid <- matrix(as.numeric(model.data[,5:7]), nrow = nrow(model.data))

    if(is.null(variables)) {
        variables <- unique(model.data[,3]) 
    }
  
    nomatch.ind <- is.na(match(variables, unique(model.data[,3])))
    if(sum(nomatch.ind) > 0) {
        warning(paste("The following variables are NOT present in the model data:", paste(variables[nomatch.ind], collapse = " ")))
        variables <- variables[!nomatch.ind]
    }

 
    if(is.null(levels)) {
        levels <- unique(model.data[,4])
    }

    nomatch.ind <- is.na(match(levels, unique(model.data[,4])))
    if(sum(nomatch.ind) > 0) {
        warning(paste("The following levels are NOT present in the model data:", paste(levels[nomatch.ind], collapse = " ")))
        levels <- levels[!nomatch.ind]
    }


    if(is.null(model.domain)) {
        model.domain <- c(min(data.grid[,1]), max(data.grid[,1]), max(data.grid[,2]), min(data.grid[,2]))
    }

    #Build grid

    lons <- as.numeric(sort(unique(model.data[,5])))
    lats <- as.numeric(sort(unique(model.data[,6])))
    grid <- list(x = lons, y = lats)
    
    fcst.grid <- list(z = array(rep(NA, length(lons) * length(lats) * length(variables) * length(levels)),
        dim = c(length(levels), length(variables), length(lons), length(lats))), 
        x = sort(lons), y = sort(lats), variables = variables, levels = levels, 
        model.run.date = model.run.date, fcst.date = fcst.date)

    #Put variables and levels into a series of layered images
    for(lvl in levels) {
        for(var in variables) {
             mi <- which(var == model.data[,3] & lvl == model.data[,4] &
                   data.grid[,1] >= model.domain[1] & data.grid[,1] <= model.domain[2] &
                  data.grid[,2] <= model.domain[3] & data.grid[,2] >= model.domain[4])
             if(length(mi) > 0) {
                 fcst.grid$z[which(lvl == fcst.grid$levels), which(var == fcst.grid$variables),,] <- as.image(
                     array(
                     data.grid[mi,3],
                     dim = c(length(lons), length(lats))),
                     grid = grid,
                     x = cbind(data.grid[,1], data.grid[,2]))$z
              }
        }
    }

    return(fcst.grid)
}
