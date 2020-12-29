###################
# library packages#
###################
library(shinyalert)
library(leaflet)
library(colorRamps)
library(lubridate)
#library(rgeos)
library(rgdal)
library(data.table)

##################
# prepare objects#
##################

psts <- NULL

# max numbers of points to render by leaflet in the interactive map
max.n.points <- 50000

# prepare color ramp
colramp <- rev(matlab.like(10))

# init stamps object
stamps <- NULL

####################
# prepare functions#
####################

# load StaMPS export by study site

load.stamps <- function(x){
  dat <- read.csv(paste("input/stusi/", x, ".csv", sep = ""))
  dates.days <- as.vector(t(dat[1, 4:ncol(dat)]))
  dates.date <- as_date(dates.days, origin = "0000-01-01")
  ref.points <- as.vector(t(dat[1, 1:2]))
  dat <- dat[c(2:nrow(dat)), ]
  rownames(dat) <- seq(length = nrow(dat))
  ps.loc <- cbind(uid = 1:nrow(dat),
                  lon = dat[ , 1],
                  lat = dat[ , 2],
                  disp = dat[ , 3],
                  dat[ , 4:ncol(dat)])
  return(list(dates.days=dates.days,
              dates.date=dates.date,
              ref.points=ref.points,
              ps.loc=ps.loc))
}

# render StaMPS points on interactive map

render.stamps <- function(pointsize = 5, setview = TRUE){
  disp <- stamps$ps.loc$disp
  colnum <- colorNumeric(colramp,
                         domain = c(min(disp),
                                    max(disp)))
  if(setview == TRUE){
    # show PS on map
    leafletProxy("map", data = stamps$ps.loc) %>%
      clearGroup("Measurement Points") %>%
      # add PS circles with color ramp
      addCircleMarkers(stamps$ps.loc$lon,
                       stamps$ps.loc$lat,
                       layerId = stamps$ps.loc$uid,
                       radius=pointsize,
                       fillColor = colramp[as.numeric(cut(disp, breaks = 10))],
                       fillOpacity = 0.8, stroke = F,
                       group = "Measurement Points") %>%
      setView(lng = median(stamps$ps.loc$lon),#+0.02,
              #TODO: check if control panel toggle is True if, shift lng 0.02
              lat = median(stamps$ps.loc$lat),
              zoom = 15)%>%
      addLegend("bottomleft", pal = colnum,
                values = disp,
                title = "mm/year",
                layerId = "uid",
                opacity = 1)
  }else{
    # show PS on map
    leafletProxy("map", data = stamps$ps.loc) %>%
      clearGroup("Measurement Points") %>%
      # add PS circles with color ramp
      addCircleMarkers(stamps$ps.loc$lon,
                       stamps$ps.loc$lat,
                       layerId = stamps$ps.loc$uid,
                       radius=pointsize,
                       fillColor = colramp[as.numeric(cut(disp, breaks = 10))],
                       fillOpacity = 0.8, stroke = F,
                       group = "Measurement Points") %>%
      addLegend("bottomleft", pal = colnum,
                values = disp,
                title = "mm/year",
                layerId = "uid",
                opacity = 1)
  }
}

#######################
# set up server script#
#######################

function(input, output, session) {

## Interactive Map ###########################################
  output$stusi <- renderUI({
    lapply(1, function(x) {
      selectInput("stusi", "Select Case Study", stusi,
                  selected = stusi[1], width = "200px")
    })
  })

  
  # Create the base map
  
  # in order to add WMS tiles, see the comments below
    output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE,
                                     zoomControl = FALSE)) %>%
        addTiles(group = "OSM Map") %>%
        addProviderTiles('Esri.WorldImagery',
                         group = "ESRI World Satellite") %>%
        # uncomment the following lines in order to add WMS tiles and config them to your needs
        # here is a example of a WMS url to some well known services
        # https://stackoverflow.com/questions/9394190/leaflet-map-api-with-google-satellite-layer
        # addWMSTiles("https://url/to/WMS",
        #   layers = "0",
        #   group = "WMS Tiles",
        #   options = WMSTileOptions(format = "image/png", transparent = F),
        #   attribution = "a proper attribution to the service") %>%
        addLayersControl(
          baseGroups = c("ESRI World Satellite", "OSM Map"), # add the "WMS Tiles" string defined above with group = to the list 
          overlayGroups = c("Measurement Points", "Custom Geometry"),
          options = layersControlOptions(collapsed = T),
          position = "topleft") %>%
      setView(lng = 0, lat = 0, zoom = 3) %>%
      addScaleBar(position = "topleft",
                  options =scaleBarOptions(maxWidth = 100, metric = T, imperial = F))
    })

#####################adding PS to interactive map#####################
  # adding ps data selected for study site
  # check and read in ps data and make it accessible

  observeEvent(input$stusi, {
    # start and fallback to ---
    if(input$stusi == "---"){
      leafletProxy("map") %>%
        clearGroup("Measurement Points") %>%
        clearGroup("Custom Geometry") %>%
        setView(lng = 0, lat = 0, zoom = 3)
    }else{
      # check the numbers of lines
      n.points <- nrow(data.table::fread(paste("input/stusi/", input$stusi, ".csv", sep = ""), select = 1L))
      if(n.points <= max.n.points){
        stamps <<- load.stamps(input$stusi)
        render.stamps(pointsize = input$pcex)
      }else{
        shinyalert(title = "Number of max points exceeded",
                   text = paste("You try to load ", n.points, " points where the max number of points is ",
                                max.n.points, ". Continue to load all points might slow down and eventually crush the Visualizer. ",
                                "Cancel to make a spatial subset or raise the number of maximum points. ",
                                "For further instructions switch to the Manual tab.",
                                sep = ""),
                   type = "input",
                   inputType = "",
                   showCancelButton = TRUE,
                   showConfirmButton = TRUE,
                   confirmButtonText = "Continue",
                   inputId = "ex.max.n.point")
        output$stusi <- renderUI({
          lapply(1, function(x) {
            selectInput("stusi", "Select Case Study", stusi,
                        selected = stusi[1], width = "200px")
          })
        })
      }
    }
  })
    
  # dynamic point size adjustment  
  observeEvent(input$pcex, {
    req(stamps)
    render.stamps(pointsize = input$pcex, setview = FALSE)
  })

  # upload and show custom geometry
  observeEvent(input$geojson, {
    infile <- input$geojson
    ext <- tools::file_ext(infile$datapath)
    req(infile)
    if(ext != "geojson"){shinyalert("Wrong file format", "Please upload a .geojson file", type = "error")}
    shiny::validate(need(ext %in% c("geojson"), "Please upload a .geojson file"))
    # load infile to object
    geoms <- readOGR(infile$datapath)
    # look up spatial class type and render geometry
    if(class(geoms)[1] == "SpatialPointsDataFrame"){
      leafletProxy("map") %>%
       clearGroup("Custom Geometry") %>%
       addMarkers(data = geoms,
                  group = "Custom Geometry",
                   )
    }
    if(class(geoms)[1] == "SpatialLinesDataFrame"){
      leafletProxy("map") %>%
       clearGroup("Custom Geometry") %>%
       addPolylines(data = geoms, color = "red",
                    group = "Custom Geometry")
    }
    if(class(geoms)[1] == "SpatialPolygonsDataFrame"){
       leafletProxy("map") %>%
       clearGroup("Custom Geometry") %>%
       addPolygons(data = geoms, color = "red",
                   fill = FALSE, weight = 4,
                   group = "Custom Geometry")
    }
    })

###################plot TS for specific MP

observe({
  # date input
  in.date.event <- input$date
  orig.date <- as_date("0000-01-01")
  date.event <- as.numeric(in.date.event - orig.date)
  # marker input
  click.map <<- input$map_marker_click
  if(is.null(click.map)){
    # create plot
    output$psts <- renderPlot({
      plot(1, 0, type = "n", axes = FALSE,
           ylab = c(""),
           xlab = c(""))
      text(1, 0, "Select a point on the map to plot a deformation time series")
    })
  }else{if(click.map$group == "Custom Geometry"){
    shinyalert("No measurment point",
               "You have selected a custom geometry marker, click on a StaMPS measurement point in order to plot deformation time series",
               type = "error")}else{
      if(click.map$id > nrow(stamps$ps.loc)){
        click.map$id <- 1}else{click.map$id <- click.map$id}
      last.click.id <<- click.map$id
      text <- paste('You have selected point', click.map$id, 'from case study', input$stusi, sep = ' ' )
      psts <<- stamps$ps.loc[click.map$id, 5:ncol(stamps$ps.loc)]
      # create plot
      output$psts <- renderPlot({
        plot(t(psts) ~ stamps$dates.days, type = "n",
             ylab = "LOS displacement [mm]", xlab ="Date",
             xlim = c(min(stamps$dates.days),
                      max(stamps$dates.days)),
             ylim = c(min(stamps$ps.loc[, 5:ncol(stamps$ps.loc)]),
                      max(stamps$ps.loc[, 5:ncol(stamps$ps.loc)])),
             axes = F)
        axis(side = 2)
        plot.info <- par("xaxp")
        tick.pos <- seq(plot.info[1], plot.info[2], length = plot.info[3]+1)
        xlabs <- as_date(tick.pos, origin = "0000-01-01")
        axis(side = 1, labels = xlabs, at = tick.pos)
        grid(col = "grey64")
        if(input$add.trend == 'ctrend'){
          lines(t(psts) ~ stamps$dates.days)}else{
            if(input$add.trend == 'ltrend'){
              lmfit <- lm(t(psts) ~ stamps$dates.days)
              abline(lmfit, col = 'black')}else{
                if(input$add.trend == 'ptrend'){
                  date.d <- stamps$dates.days
                  pfit <- lm(t(psts) ~ poly(date.d, 2, raw = TRUE))
                  xx <- seq(range(date.d)[1],
                            range(date.d)[2],
                            length.out = 250)
                  lines(xx, predict(pfit, data.frame(date.d = xx)), col = 'black')
                }else{return()}
              }
          }
        points(t(psts) ~ stamps$dates.days,
               pch = 19)
        abline(v = date.event, lty = 2,
               col = "red")
        box(which = "plot")
      })
      # render output text
      output$Click_text <- renderText({text})
    }
  }
})

##################plot subtracted offset TS for specific MP

observeEvent(input$sub.offset, {
  # date input
  in.date.event <- input$date
  orig.date <- as_date("0000-01-01")
  date.event <- as.numeric(in.date.event - orig.date)
  if(is.null(click.map) | is.null(psts)){
    shinyalert("No TS selected",
               "You have not selected a deformation time series, click on a StaMPS measurement point in order to plot deformation time series",
               type = "error")}else{
      offset <- psts[1,1]
      psts.off <- psts - offset
      # create plot
      output$psts <- renderPlot({
        plot(t(psts.off) ~ stamps$dates.days, type = "n",
             ylab = "cumulative LOS displacement [mm]", xlab ="Date",
             xlim = c(min(stamps$dates.days),
                      max(stamps$dates.days)),
             ylim = c(min(stamps$ps.loc[, 5:ncol(stamps$ps.loc)])-offset,
                      max(stamps$ps.loc[, 5:ncol(stamps$ps.loc)])-offset),
             axes = F)
        axis(side = 2)
        plot.info <- par("xaxp")
        tick.pos <- seq(plot.info[1], plot.info[2], length = plot.info[3]+1)
        xlabs <- as_date(tick.pos, origin = "0000-01-01")
        axis(side = 1, labels = xlabs, at = tick.pos)
        grid(col = "grey64")
        if(input$add.trend == 'ctrend'){
          lines(t(psts.off) ~ stamps$dates.days)}else{
            if(input$add.trend == 'ltrend'){
              lmfit <- lm(t(psts.off) ~ stamps$dates.days)
              abline(lmfit, col = 'black')}else{
                if(input$add.trend == 'ptrend'){
                  date.d <- stamps$dates.days
                  pfit <- lm(t(psts.off) ~ poly(date.d, 2, raw = TRUE))
                  xx <- seq(range(date.d)[1],
                            range(date.d)[2],
                            length.out = 250)
                  lines(xx, predict(pfit, data.frame(date.d = xx)), col = 'black')
                }else{return()}
              }
          }
        points(t(psts.off) ~ stamps$dates.days,
               pch = 19)
        abline(v = date.event, lty = 2,
               col = "red")
        box(which = "plot")
      })}
})
#######################Baseline Plot#####################
# TODO: Baseline and image combination report for export
# TODO: Export plot as .svg

observeEvent(input$blopt, {
  if(input$blopt == "ps.a"){
    # dynamic Input for temp baseline in PS mode
    output$bl.temp <- renderUI({
      lapply(1, function(x) {
        sliderInput(inputId = "bl.temp", label = "Temporal Baseline Threshold",
                    min = 1, max = 1000, value = 500)
      })
    })
  }else{
      # dynamic Input for temp baseline in SBAS mode
      output$bl.temp <- renderUI({
        lapply(1, function(x) {
          sliderInput(inputId = "bl.temp", label = "Temporal baseline threshold",
                      min = 1, max = 365, value = 48)
        })
      })
    }
  })

observe({
  # inputs
  in.bl.info <- input$bl.file
  in.bl.temp <- input$bl.temp
  in.bl.spat <- input$bl.spat
  
  dat <- read.csv(paste("input/baseline_info/", in.bl.info, ".csv", sep = ""))
  # TODO: check input file if 1 row is prime image by 0 baseline info
  # TODO: check if input is in general the right format
  if(input$blopt == "ps.a"){
    
        # subset data for baseline thresholds
    pstab.i <- abs(dat$PerpBaseline.m.) <= in.bl.spat & abs(dat$TempBaseline.days.) <= in.bl.temp
    pstab <- dat[pstab.i, ]
    # info text
    supprime <- substr(dat$Product[1], 18, 25)
    out.bl.text <- paste("The ", nrow(dat), " images were combined to ", nrow(pstab)-1,
                         " interferograms, using ", in.bl.spat, " m ",
                         "and ",
                         in.bl.temp, " days as thresholds.", 
                         " The prime image is from ",
                         supprime, ".", sep = "")
    
    output$bl.plot <- renderPlot({
      # plot PS combinations
      plot(dat$PerpBaseline.m. ~ dat$TempBaseline.days.,
           xlab = "temporal baseline [days]",
           ylab = "perpendicular baseline [m]",
           type = "n")
      
      for(i in 2:nrow(pstab)){
        x.cord <- c(pstab$TempBaseline.days.[c(1, i)])
        y.cord <- c(pstab$PerpBaseline.m.[c(1, i)])
        lines(y.cord ~ x.cord, col = "grey80")
      }
      
      points(dat$PerpBaseline.m. ~ dat$TempBaseline.days.,
             pch = 19, col = "orangered")
      points(dat$PerpBaseline.m.[1] ~ dat$TempBaseline.days.[1],
             pch = 19, col = "royalblue1")
    })
    # render output text
    output$bl.text <- renderText({out.bl.text})
  }else{
  #####SBAS Plot######
  # temporal baseline thresholding
  tbase <-
    dat$TempBaseline.days. %>%
    dist %>%
    as.matrix 
  tbase <- tbase <= in.bl.temp
  tbase <- lower.tri(tbase)*tbase
  
  # perp baseline thresholding
  pbase <-
    dat$PerpBaseline.m. %>%
    dist %>%
    as.matrix 
  pbase <- pbase <= in.bl.spat
  pbase <- lower.tri(pbase)*pbase
  sbas.base <- pbase*tbase
  
  sbas.base <- which(sbas.base == 1, arr.ind = T)
  
  # info text
  supprime <- substr(dat$Product[1], 18, 25)
  out.bl.text <- paste("The ", nrow(dat), " images were combined to ", nrow(sbas.base),
                       " interferograms, using ", in.bl.spat, " m ",
                       "and ",
                       in.bl.temp, " days as thresholds.", 
                       " The prime image is from ",
                       supprime, ".", sep = "")
  
  output$bl.plot <- renderPlot({
    # plot SBAS combinations
    plot(dat$PerpBaseline.m. ~ dat$TempBaseline.days.,
         xlab = "temporal baseline [days]",
         ylab = "perpendicular baseline [m]",
         type = "n")

    for(i in 1:nrow(sbas.base)){
      x.cord <- c(dat$TempBaseline.days.[sbas.base[i,1:2]])
      y.cord <- c(dat$PerpBaseline.m.[sbas.base[i,1:2]])
      lines(y.cord ~ x.cord, col = "grey80")
    }
    
    points(dat$PerpBaseline.m. ~ dat$TempBaseline.days.,
           pch = 19, col = "orangered")
    points(dat$PerpBaseline.m.[1] ~ dat$TempBaseline.days.[1],
           pch = 19, col = "royalblue1")
  })
  # render output text
  output$bl.text <- renderText({out.bl.text})
 }
})
# end of server function
}