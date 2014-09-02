#Functions for doing specific useful tasks with rNOMADS

GetClosestGFSForecasts <- function(forecast.date, model.date = "latest", depth = NULL, verbose = TRUE) {
 #Figure out the closest GFS forecasts to a given date, returns both the closest forecast behind and the closest forecast ahead, as well as how far beind and how far ahead
   #INPUTS
   #    FORECAST.DATE - What date you want a forecast for, as a date/time object, in GMT
   #    MODEL.DATE - Which model run to use, in YYYYMMDDHH, where HH is 00, 06, 12, 18.  Defaults to "latest", which means get the latest model available.
   #    DEPTH - How many model URLS to exame.  This is only taken into account when model.date!="latest".
   #       The option allows users to prevent the code from scanning every single URL.
   #        If MODEL.DATE is not "latest", rNOMADS will scan the entire directory, and this will take time. 
   #    VERBOSE - Give a blow-by-blow account of progress.  Defaults to TRUE.
   #OUTPUTS
   #    MODEL.URL - Which model URL to use
   #    MODEL.RUN.DATE - When the model was run
   #    BACK.FORECAST - Nearest forecast string behind forecast date
   #    FORE.FORECAST - Nearest forecast past forecast date
   #    BACK.HR - How many hours before the requested time the back forecast is
   #    FORE.HR - How many hours after the requested time the forward forecast is

   if(verbose){
       print("Finding model run dates...")
   }
   if(model.date == "latest")  {
       urls.out <- CrawlModels(abbrev = "gfs_hd", depth = 2, verbose = verbose)
       model.parameters <- ParseModelPage(urls.out[1])
       if(length(model.parameters$pred) == 0) { #No data in latest model, try next latest
           model.parameters <- ParseModelPage(urls.out[2]) 
           if(length(model.parameters$pred) == 0) { #Nothing here either, maybe the server is down?
               stop("No data found for most recent GFS model run.  Perhaps the NOMADS server is down?")
           } else {
               url.to.use <- urls.out[2]
           }
       } else {
           url.to.use <- urls.out[1]
       }
    } else {
         urls.out <- CrawlModels(abbrev = "gfs_hd", depth = depth, verbose = verbose)
         model.run.dates <- unlist(stringr::str_extract_all(urls.out, "\\d{10}")) 
         if(model.date %in% model.run.dates) {
              url.to.use <- urls.out[which(model.date == model.run.dates)]
              model.parameters <- ParseModelPage(url.to.use)
         } else {
             stop(paste0("The model run date ", model.date, " does not appear in the list of model runs on the NOMADS server."))
         }
    }

    if(verbose) {
        print("Determining closest forecasts...")
    }
   
    #Get model run date, convert to POSIX date 
    run.date <- stringr::str_match_all(url.to.use, "\\d{10}")[[1]][1,1]
    d.vec <- strsplit(run.date, split = "")[[1]]
    nice.run.date <- strftime(paste0(paste(d.vec[1:4], collapse = ""),
        "-", paste(d.vec[5:6], collapse = ""), "-", paste(d.vec[7:8], collapse = ""),
        " ", paste(d.vec[9:10], collapse = ""), ":00:00", sep = "")) 

   #Figure out time difference between forecast date and model date 
   hr.shift <- as.numeric(difftime(forecast.date, as.POSIXlt(nice.run.date, tz = "GMT")), units = "hours")

   #Figure out the forward (in the future) forecast and back (in the past) forecast using hr.shift
   pred.hrs <- as.numeric(unlist(stringr::str_match_all(model.parameters$pred, "\\d{2,3}$")))
   hr.diff <- pred.hrs - hr.shift

   #Get forecasts
   back.forecast <- model.parameters$pred[which(max(hr.diff[which(hr.diff <=0)]) == hr.diff)]
   fore.forecast <- model.parameters$pred[which(min(hr.diff[which(hr.diff > 0)]) == hr.diff)]
   back.hr <- hr.diff[which(max(hr.diff[which(hr.diff <=0)]) == hr.diff)]
   fore.hr <- hr.diff[which(min(hr.diff[which(hr.diff > 0)]) == hr.diff)] 

   if(verbose) {
       print("Finished determining forecast times.")
   }
   return(list(model.url = url.to.use, model.run.date = nice.run.date, back.forecast = back.forecast, fore.forecast = fore.forecast, back.hr = back.hr, fore.hr = fore.hr))
}

BuildProfile <- function(gridded.data, lon, lat, spatial.average) {
    #This function builds an atmospheric profile, performing spatial interpolation if requested
    #INPUTS
    #    GRIDDED.DATA - Data structure returned by ModelGrid
    #    LON - Longitude of point of interest
    #    LAT - Latitude of point of interest
    #    SPATIAL.AVERAGE - Boolean determining whether to get nearest node value (FALSE) or interpolate using b-splines (TRUE)
    #OUTPUTS
    #    PROFILE.DATA - A levels x variables matrix with atmospheric data for given point
    
    profile.data <- array(rep(0, length(gridded.data$variables) * length(gridded.data$levels)),
        dim = c(length(gridded.data$levels), length(gridded.data$variables)))
    #Project to Cartesian grid
    lons <- t(array(rep(gridded.data$x, length(gridded.data$y)), dim = dim(gridded.data$z)[3:4]))
    lats <- array(rep(rev(gridded.data$y), length(gridded.data$x)), dim = dim(gridded.data$z)[3:4])
    proj <- GEOmap::setPROJ(type = 2, LAT0 = lat, LON0 = lon)
    cart.pts <- GEOmap::GLOB.XY(as.vector(lats), as.vector(lons), proj)
    cart.dist <- array(sqrt(cart.pts$x^2 + cart.pts$y^2), dim = c(length(gridded.data$x), length(gridded.data$y)))
    if(spatial.average) {  #Average of 4 nearest points
        for(k in seq_len(length(gridded.data$levels))) {
            for(j in seq_len(length(gridded.data$variables))) {
                layer.img <- cbind(cart.pts$x, cart.pts$y, as.vector(gridded.data$z[k,j,,]))
                profile.data[k, j] <- MBA::mba.points(layer.img, cbind(0, 0))[[1]][3]
            }
        }
     } else { #Nearest grid node
         node.ind <- which(cart.dist == min(cart.dist), arr.ind = TRUE)
         profile.data <- gridded.data$z[,,node.ind[1], node.ind[2]]
         spatial.average.method <- "Nearest Node"

     }
   return(profile.data)
}

ModelGrid <- function(model.data, resolution, grid.type = "latlon", levels = NULL, variables = NULL, model.domain = NULL, cartesian.nodes = NULL) {
    #Transform model data array into a grid with dimensions levels x variables x lon range x lat range
    #This should reduce the size of the returned data by removing redundant information
    #This will perform interpolation as necessary to fit data to a regular grid - be aware of this!
    #INPUTS
    #    MODEL.DATA - Data returned by ReadGrib
    #    RESOLUTION - Resolution of grid, in degrees if TYPE = "LATLON", in kilometers if TYPE = CARTESIAN, as a 2 element vector c(East-West, North-South)
    #    VARIABLES - variables to include in grid, if NULL, include all of them
    #    LEVELS - levels to include in grid, if NULL, include all of them
    #    MODEL.DOMAIN - vector c(LEFT LON, RIGHT LON, TOP LAT, BOTTOM LAT) of region to include in output. If NULL, include everything.
    #    GRID.TYPE - Whether the grid is in lat/lon or cartesian.  Options "latlon" or "cartesian."
    #    CARTESIAN.NODES - If GRID.TYPE = cartesian, this is the list of x and y values that define the cartesian grid of the model
    #        CARTESIAN.NODES$X - East-west values
    #        CARTESIAN.NODES $Y - North-south values
    #OUTPUTS
    #   FCST.GRID - A list with elements:
    #       $Z An array of dimensions levels x variables x lon x lat; each level x variable contains the model grid of data from that variable and level
    #       $X Vector of longitudes
    #       $Y Vector of latitudes
    #       $VARIABLES - the variables contained in the grid
    #       $LEVELS - the levels in the grid
    #       $MODEL.RUN.DATE - when the forecast model was run
    #       $FCST.DATE - what date the forecast is for
  
    if(length(resolution) != 2) {
        stop("\"resolution\" must be a 2 element vector: c(ZONAL RESOLUTION, MERIDIONAL RESOLUTION)")
    }
 
    if(grid.type == "latlon") {
        nodes.xy <- cbind(model.data$lon, model.data$lat)
    } else if(grid.type == "cartesian") {
        if(is.null(cartesian.nodes) | !(("x" %in% names(cartesian.nodes)) & ("y" %in% names(cartesian.nodes)))) {
           stop("If grid.type = \"cartesian\", you must define the input cartesian.nodes$x=east-west values of your model grid, cartesian.nodes$y=north-south values of your model grid. See ModelGrid documentation.")
        } 
        nodes.xy <- cbind(cartesian.nodes$x, cartesian.nodes$y) 
    } else {
        stop(paste0("Did not recognize grid.type ", grid.type, ".  Available options are \"latlon\" and \"cartesian\""))
    }

    model.run.date <- unique(model.data$model.run.date)
    if(length(model.run.date) > 1) {
        warning("There appears to be more than one model run date in your model grid!")
    }

    fcst.date <- unique(model.data$forecast.date)

    if(length(fcst.date) > 1) {
        warning("There appears to be more than one model run date in your model grid!")
    }

    if(is.null(variables)) {
        variables <- unique(model.data$variables) 
    }
  
    nomatch.ind <- is.na(match(variables, unique(model.data$variables)))
    if(sum(nomatch.ind) > 0) {
        warning(paste("The following variables are NOT present in the model data:", paste(variables[nomatch.ind], collapse = " ")))
        variables <- variables[!nomatch.ind]
    }

 
    if(is.null(levels)) {
        levels <- unique(model.data$levels)
    }

    nomatch.ind <- is.na(match(levels, unique(model.data$levels)))
    if(sum(nomatch.ind) > 0) {
        warning(paste("The following levels are NOT present in the model data:", paste(levels[nomatch.ind], collapse = " ")))
        levels <- levels[!nomatch.ind]
    }


    if(is.null(model.domain)) {
        model.domain <- c(min(nodes.xy[,1]), max(nodes.xy[,1]), max(nodes.xy[,2]), min(nodes.xy[,2]))
    }

    #Build grid

    grid <- list(x = seq(model.domain[1], model.domain[2], by = resolution[1]),
       y = seq(model.domain[4], model.domain[3], by = resolution[2]))
    
    fcst.grid <- list(z = array(rep(NA, length(grid$x) * length(grid$y) * length(variables) * length(levels)),
        dim = c(length(levels), length(variables), length(grid$x), length(grid$y))), 
        x = grid$x, y = grid$y, variables = variables, levels = levels, 
        model.run.date = model.run.date, fcst.date = fcst.date)

    #Put variables and levels into a series of layered images
    for(lvl in levels) {
        for(var in variables) {
             mi <- which(var == model.data$variables & lvl == model.data$levels) 
             if(length(mi) > 0) {
                 fcst.grid$z[which(lvl == fcst.grid$levels), which(var == fcst.grid$variables),,] <- fields::as.image(
                     as.numeric(model.data$value[mi]),
                     grid = grid,
                     x = nodes.xy[mi,])$z
              }
        }
    }

    return(fcst.grid)
}

MagnitudeAzimuth <- function(zonal.wind, meridional.wind) {
   #Given zonal (East-West) and meridional (North-South) wind speeds, calculate magnitude and azimuth.
   #INPUTS
   #    ZONAL.WIND - Wind East West, in meters per second, west negative
   #    MERIDIONAL.WIND - Wind North South, in meters per second, south negative
   #OUTPUTS
   #   MAGNITUDE - Wind magnitude, in meters per second
   #   AZIMUTH  - Wind azimuth, in degrees from north
   
   mag <- sqrt(zonal.wind^2 + meridional.wind^2)
   tmp.az <- (180/pi) * atan2(zonal.wind, meridional.wind)
   az <- tmp.az
   az[tmp.az < 0] <- 360 + tmp.az[tmp.az < 0]
   return(list(magnitude = mag, azimuth = az))
}

PlotWindProfile <- function(zonal.wind, meridional.wind, height, magnitude = NULL, magnitude.range = c(0, 50), 
    height.range = c(0, 50000), points = TRUE, lines = FALSE,
    radial.axis = TRUE, elev.circles = NULL, elev.labels = NULL, 
    radial.lines = NULL, colorbar = TRUE, colorbar.label = NULL, north.label = TRUE, invert = FALSE, ...) {
    #Plot a graphical representation of wind speed and direction, with lowest elevations closest to the center and highest elevations towards the edge.
    #INPUTS
    #    ZONAL.WIND - East-west wind speed.  Can be a vector or a list of vectors
    #    MERIDIONAL.WIND - North-South wind speed.  Can be a vector or a list of vectors.
    #    HEIGHT - Height above reference point.  Can be a vector or list of vectors.
    #    MAGNITUDE - If you want to plot different numeric values along the azimuths specified by ZONAL.WIND and MERIDIONAL.WIND, for example, if you want
    #       along-wind sound velocity.  Default NULL.
    #    MAGNITUDE.RANGE - Ranges of wind speed to plot, defaults to c(0, 50)
    #    HEIGHT.RANGE - Which heights to plot, defaults to c(0, 50000)
    #    POINTS - Do you want to plot data points as points?  You can use optional parameters to set the type and appearance of these points. Default TRUE.
    #    LINES - DO you want to plot your data as lines?  You can use optional parameters to set the type and appearance of these lines. Default FALSE.
    #    RADIAL.AXES - Plot an axis around the figure
    #    ELEV.CIRCLES - A vector of elevations to plot as a grid.  Default NULL
    #    ELEV.LABELS - Labels to put above each elevation circle
    #    RADIAL.LINES - A vector of azimuths to plot.  Default NULL
    #    COLORBAR - Whether to put a colorbar of wind magnitudes
    #    COLORBAR.LABEL - What to label your colorbar
    #    NORTH.LABEL - whether to label north. Default TRUE.
    #    INVERT - Whether to reverse the plot, so that higher elevations are towards the center and lower elevations are towards the outer edges.
    #OPTIONAL INPUTS (via ...)
    #    R.AXIS - radius of plot axis
    #    TICK.LEN - length of azimuth ticks
    #    R.AXIS.TICKS - Whether or not to put tick marks on the outer axis
    #    MAX.AZ - If plotting lines and the difference between two segments is greater than this value, interpolate between them to make things smooth
    #    COLOR.MAP - A list of colors to use, defaults to rainbow(n, 0, 5/6)
    #    N.COLS - Number of color bins in color map
    #    SUB.COL - Color of internal (elevation and azimuth) axes as a vector of length 2
    #    SUB.LTY - Type of internal axes, as a vector of length 2
    #    SUB.LWD - Width of internal axes, as a vector of length 2
    #    ELEV.LABELS.AZ - Which azimuth to plot elevation labels on. 
    #    POINT.CEX - size of points, if plotted
    #    PCH - Plot character of points, if plotted
    #    LTY - Line style, if lines are selected
    #    LWD - Line thickness, if lines are selected
    #    COLORBAR.TICK - Where to put labels on colorbar
 
    opts <- list(...)
    o.n <- names(opts)
    r <- 1

    if(! "r.axis" %in% o.n) {
        r.axis <- 1.2
    } else {
        r.axis <- opts$r.axis
    }

    if(! "tick.len" %in% o.n) {
        tick.len <- r / 20 
    } else {
        tick.len <- opts$tick.len
    }
    
    if(! "r.axis.ticks" %in% o.n) {
        r.axis.ticks <- TRUE
    } else {
        r.axis.ticks <- opts$r.axis.ticks
    }

    if(! "max.az" %in% o.n) {
        max.az <- 5
    } else {
        max.az <- opts$max.az
    }

    if(!"n.cols" %in% o.n) {
        n.cols <- 500
    } else {
        n.cols <- opts$n.cols
    }

    if(!"color.map" %in% o.n) {
       color.map <- rainbow(n.cols, start = 0, end = 5/6)           
    } else {
       color.map <- opts$color.map
    }

    if(!"sub.col" %in% o.n) {
        sub.col <- c("gray50", "gray60")
    } else {
       sub.col <- opts$sub.col
    }

    if(!"sub.lty" %in% o.n) {
        sub.lty <- c(2, 3)
    } else {
       sub.lty <- opts$sub.lty
    }

    if(!"sub.lwd" %in% o.n) {
        sub.lwd <- c(1, 1)
    } else {
        sub.lwd <- opts$sub.lwd
    }

    if(!"elev.labels.az" %in% o.n) {
        elev.labels.az <- 0
    } else {
        elev.labels.az <- opts$elev.labels.az
    }

    if(!"point.cex" %in% o.n) {
        point.cex <- 1
    } else {
        point.cex <- opts$point.cex
    }

    if(!"pch" %in% o.n) {
        pch <- 1
    } else {
       pch <- opts$pch
    }

    if(!"lty" %in% o.n) {
        lty <- 1
    } else {
        lty <- opts$lty
    }

    if(!"lwd" %in% o.n) {
        lwd <- 1
    } else {
       lwd <- opts$lwd
    }

    if(!"colorbar.tick" %in% o.n) {
        colorbar.tick <- seq(magnitude.range[1], magnitude.range[2], length.out = 6)
    } else {
        colorbar.tick <- opts$colorbar.tick
    }

    rng <- max(height.range) - min(height.range)

    plot(c(-r.axis - tick.len * 4, r.axis + tick.len * 4) + r/5, 
        c(-r.axis - tick.len * 4, r.axis + tick.len * 4), asp = 1, 
        type = "n", axes = FALSE, xlab = "", ylab = "") 

    if(radial.axis) {
        angs <- seq(0, 2 * pi, length.out = 1000)
        circ.axis <- list(x = r.axis * cos(angs), y = r.axis * sin(angs))
        lines(circ.axis)
        if(r.axis.ticks) {
            circ.ticks <- seq(0, 360, by = 45)[1:8] * pi/180
            tick.len <- r/20
            segments(r.axis * cos(circ.ticks), r.axis * sin(circ.ticks),
                r.axis * cos(circ.ticks) + tick.len * cos(circ.ticks),
                r.axis * sin(circ.ticks) + tick.len * sin(circ.ticks))
        }
   }

   if(north.label) { 
       text(0, r.axis + tick.len * 2, lab = "North")
   }

   interp.len <- 100  #Number of points in azimuth interpolation, if necessary

   if(! is.list(zonal.wind)) {
       zonal.wind <- list(zonal.wind)
       meridional.wind <- list(meridional.wind)
       height <- list(height)
   }

   color.scale <- seq(magnitude.range[1], magnitude.range[2], length.out = n.cols) #Wind color scale

   for(k in 1:length(zonal.wind)) {
       hgt.ind <- which(height[[k]] >= height.range[1] & height[[k]] <= height.range[2])
       maz <- MagnitudeAzimuth(zonal.wind[[k]][hgt.ind], meridional.wind[[k]][hgt.ind])
       hgt <- height[[k]][hgt.ind] - min(height.range)
       if(is.null(magnitude)) {
           mag <- maz$magnitude
       } else {
           mag <- magnitude[[k]][hgt.ind]
       }

       if(invert) {
           hgt <- rev(hgt)
       }
       if(points) {
           point.col <- c()
           if(is.null(magnitude)) {
               mag <- maz$magnitude
           } else {
               mag <- magnitude
           }
           for(j in 1:length(maz$azimuth)) {
                point.col <- append(point.col, color.map[which(abs(mag[j] - color.scale) == min(abs(mag[j] - color.scale)))])
           }
           points(r * hgt/rng * sin((pi / 180) * maz$azimuth),
               r * hgt/rng * cos((pi / 180) * maz$azimuth), 
               col = point.col, cex = point.cex, pch = pch)
       }
       if(lines) {
           m <- c()
           h <- c()
           azs <- c(maz$azimuth, tail(maz$azimuth, 1))
           tmp.az <- c()
           for(j in 1:length(maz$azimuth)) {
               if(abs(azs[j] - azs[j + 1]) < max.az) {
                   m <- append(m, mag[j])
                   h <- append(h, hgt[j])
                   tmp.az <- append(tmp.az, azs[j])
               } else { #Smooth out large azimuth swings (usually at low wind speeds)
                   #If we do not cross north
                   if((abs(azs[j] - (azs[j + 1] + 360)) > (abs(azs[j] - azs[j +1]))) & (abs(azs[j] + 360 - azs[j + 1]) > abs(azs[j] - azs[j + 1]))) {
                       tmp.az <- append(tmp.az, seq(azs[j], azs[j +1], length.out = interp.len))
                       #If we cross north, going clockwise
                   } else if (abs(azs[j] - (azs[j + 1] + 360)) < abs(azs[j] - azs[j + 1])) {
                       tmp.az <-  append(tmp.az, seq(azs[j], azs[j + 1] + 360, length.out = interp.len))
                   #If we cross north, going anticlockwise
                   } else {
                       tmp.az <-  append(tmp.az, seq(azs[j] + 360, azs[j + 1], length.out = interp.len))
                  }
                  m <- append(m, seq(mag[j], mag[j + 1], length.out = interp.len))
                  h <- append(h, seq(hgt[j], hgt[j + 1], length.out = interp.len))
               }
            }
            x <- r * h/rng * sin((pi / 180) * tmp.az)
            y <- r * h/rng * cos((pi / 180) * tmp.az)
            line.col <- c()
            for(j in 1:length(m)) {
               line.col <- append(line.col, color.map[which(abs(m[j] - color.scale) == min(abs(m[j] - color.scale)))])
            }
            segments(x[2:length(m)], y[2:length(m)], x[1:(length(m) - 1)], y[1:(length(m) - 1)], 
                col = line.col, lty = lty, lwd = lwd)
        }
   }

   #Altitude axes and labels
   text.loc <- elev.labels.az * (pi/180)
   if(!is.null(elev.circles)) {
       for(k in 1:length(elev.circles)) {
           h.axis <- list(x = r * ((elev.circles[k] - height.range[1])/rng) * cos(angs), y = r * ((elev.circles[k] - height.range[1])/rng) * sin(angs)) 
           lines(h.axis, lty = sub.lty[1], lwd = sub.lwd[1], col = sub.col[1])
           if(!is.null(elev.labels)) {
               text(r * sin(text.loc) * ((elev.circles[k] - height.range[1])/rng),  r * cos(text.loc) * ((elev.circles[k] - height.range[1])/rng), elev.labels[k], adj = c(0, 0))
           }
       }
   }

   #Radial axes and labels
   if(!is.null(radial.lines)) {
       segments(0, 0, r.axis * sin(radial.lines * (pi/180)), r.axis * cos(radial.lines * (pi/180)), lty = sub.lty[2], lwd = sub.lwd[2], col = sub.col[2])
   }
 
   #Make colorbar
   if(colorbar) {
       image(seq(r.axis + tick.len * 4, r.axis + tick.len * 5, length.out = 2),
           seq(-r.axis, r.axis, length.out = n.cols),
           array(seq(0, 10, length.out = n.cols),
           dim = c(1, n.cols)), add = TRUE, col = color.map) 
       scaling <- r.axis/(max(colorbar.tick) - min(colorbar.tick))
       text(rep(r.axis + tick.len * 4, length(colorbar.tick)), (colorbar.tick - min(colorbar.tick)) * 2 * scaling - r.axis,
           lab = colorbar.tick, pos = 4 )

       if(!is.null(colorbar.label)) {
           text(r.axis + tick.len * 3, 0, colorbar.label, srt = 90)
       }
    }
}     

 
RTModelProfile <- function(model.url, pred, levels, variables, lon, lat, resolution, grid.type, model.domain = NULL, spatial.average = FALSE, verbose = TRUE) {
   #Get variables and levels for a list of points, utilizing nearest neighbor interpolation if requested
   #INPUTS
   #    MODEL.URL - Where the model is located, returned from CrawlModels
   #    PRED - Which prediction to download
   #    VARIABLES - Model variables to get for profile
   #    LEVELS - Model levels to get for profile
   #    LON - List of longitudes
   #    LAT - List of latitudes
   #    RESOLUTION - Resolution of model, in degrees if Lat/Lon, in kilometers if cartesian, as a 2 element vector (ZONAL, MERIDIONAL)
   #    GRID.TYPE - If the horizontal part of the model is Lat/Lon or cartesian.
   #       "latlon" if Lat/Lon, "cartesian" if cartesian.
   #    MODEL.DOMAIN - a vector of latitudes and longitudes that specify the area to return a forecast for
   #        If NULL, get 1 degree past limits of LON and LAT
   #    SPATIAL.AVERAGE - Perform nearest neighbor interpolation for 4 grid nodes to get average profile at a specific point.  Default FALSE.  If FALSE, get data from nearest grid node.
   #OUTPUTS
   #    PROFILE.DATA - Table of pressures and requested variables
   #    SPATIAL.AVERAGING - What kind of spatial interpolation was used, if any
   #    PRED - is the exact model run you want, generally from ParseModelPage
   #    VARIABLES - Model variables, in the order presented in PROFILE.DATA
   #    LEVELS - Model levels, in the order presented in PROFILE.DATA
   #    MODEL.DATE - When the model was run
   #    DATE - What date was requested for the data



   if(is.null(model.domain)) {
       model.domain <- c(min(lon), max(lon), max(lat), min(lat)) + c(-1, 1, 1, -1)
   }

   if(spatial.average) {
      spatial.average.method <- "Multilevel B-Splines using MBA::mba.points"
   } else {
      spatial.average.method <- "Nearest Node"
   }


   grib.info <- GribGrab(model.url, pred, levels, variables, model.domain = model.domain, verbose = verbose)
   grib.data <- ReadGrib(file.path(grib.info$local.dir, grib.info$file.name), levels, variables)
   gridded.data <- ModelGrid(grib.data, resolution, grid.type = grid.type)

   l.i <- sort(as.numeric(unlist(stringr::str_extract_all(gridded.data$levels, "\\d+"))), index.return = TRUE, decreasing = TRUE)
   profile.data <- NULL
   for(k in seq_len(length(lat))) {
       profile.data[[k]] <- BuildProfile(gridded.data, lon[k], lat[k], spatial.average)[l.i$ix,]
   }
   profile <- list(profile.data = profile.data, spatial.averaging = spatial.average.method,
       variables = gridded.data$variables, lat = lat, lon = lon,
       levels = gridded.data$levels[l.i$ix], model.date = gridded.data$model.run.date, forecast = pred, model.url = model.url,
       forecast.date = gridded.data$forecast.date)
   invisible(profile)
}

