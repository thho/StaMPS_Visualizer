library(shinyalert)
library(leaflet)
library(colorRamps)
library(lubridate)
library(rgeos)
library(rgdal)

function(input, output, session) {
  
  #make stusiBy accessible outside observeEvent
  #stusiBy <- NULL

  #stusitscBy <- NULL
  makeReactiveBinding("stusitscBy")
  psts <- NULL
  
  # output$ts1 <- renderUI({
  #   stusitsc.ind <- which(stusi == stusitscBy)
  #   selectInput('ts11', '1. Time Series',
  #               c("Select TS" = NA, ps.loc[[stusitsc.ind]]$uid),
  #               selected = NA)
  # })
  # output$ts2 <- renderUI({
  #   stusitsc.ind <- which(stusi == stusitscBy)
  #   selectInput('ts22', '2. Time Series',
  #               c("Select TS" = NA, ps.loc[[stusitsc.ind]]$uid),
  #               selected = NA)
  # })
  # output$ts3 <- renderUI({
  #   stusitsc.ind <- which(stusi == stusitscBy)
  #   selectInput('ts33', '3. Time Series',
  #               c("Select TS" = NA, ps.loc[[stusitsc.ind]]$uid),
  #               selected = NA)
  # })
  # output$ts4 <- renderUI({
  #   stusitsc.ind <- which(stusi == stusitscBy)
  #   selectInput('ts44', '4. Time Series',
  #               c("Select TS" = NA, ps.loc[[stusitsc.ind]]$uid),
  #               selected = NA)
  # })
  # output$ts5 <- renderUI({
  #   stusitsc.ind <- which(stusi == stusitscBy)
  #   selectInput('ts55', '5. Time Series',
  #               c("Select TS" = NA, ps.loc[[stusitsc.ind]]$uid),
  #               selected = NA)
  # })
  # 
  # observe({
  #   #date input
  #   in.date.eventsc <- input$datesc
  #   orig.datesc <- as_date("0000-01-01")
  #   date.eventsc <- as.numeric(in.date.eventsc - orig.datesc)
  #   #studysite and ts data
  #   stusitscBy <<- input$stusitsc
  #   stusitsc.ind <- which(stusi == stusitscBy)
  #   tsdata1 <- ps.loc[[stusitsc.ind]][input$ts11, 5:ncol(ps.loc[[stusitsc.ind]])]
  #   tsdata2 <- ps.loc[[stusitsc.ind]][input$ts22, 5:ncol(ps.loc[[stusitsc.ind]])]
  #   tsdata3 <- ps.loc[[stusitsc.ind]][input$ts33, 5:ncol(ps.loc[[stusitsc.ind]])]
  #   tsdata4 <- ps.loc[[stusitsc.ind]][input$ts44, 5:ncol(ps.loc[[stusitsc.ind]])]
  #   tsdata5 <- ps.loc[[stusitsc.ind]][input$ts55, 5:ncol(ps.loc[[stusitsc.ind]])]
  #   
  #   output$tscomp <- renderPlot({
  #     plot(t(tsdata1) ~ dates.days[[stusitsc.ind]], type = "n",
  #          ylab = "mm", xlab ="Date",
  #          xlim = c(min(dates.days[[stusitsc.ind]]),
  #                   max(dates.days[[stusitsc.ind]])),
  #          ylim = c(min(ps.loc[[stusitsc.ind]][, 5:ncol(ps.loc[[stusitsc.ind]])]),
  #                   max(ps.loc[[stusitsc.ind]][, 5:ncol(ps.loc[[stusitsc.ind]])])),
  #          axes = F)
  #     
  #     axis(side = 2)
  #     plot.info <- par("xaxp")
  #     tick.pos <- seq(plot.info[1], plot.info[2], length = plot.info[3]+1)
  #     xlabs <- as_date(tick.pos, origin = "0000-01-01")
  #     axis(side = 1, labels = xlabs, at = tick.pos)
  #     grid(col = "grey64")
  #     
  #     lines(t(tsdata1) ~ dates.days[[stusitsc.ind]], col = "red")
  #     points(t(tsdata1) ~ dates.days[[stusitsc.ind]],
  #            pch = 19, col = "red")
  #     lines(t(tsdata2) ~ dates.days[[stusitsc.ind]], col = "blue")
  #     points(t(tsdata2) ~ dates.days[[stusitsc.ind]],
  #            pch = 19, col = "blue")
  #     lines(t(tsdata3) ~ dates.days[[stusitsc.ind]], col = "darkgreen")
  #     points(t(tsdata3) ~ dates.days[[stusitsc.ind]],
  #            pch = 19, col = "darkgreen")
  #     lines(t(tsdata4) ~ dates.days[[stusitsc.ind]], col = "black")
  #     points(t(tsdata4) ~ dates.days[[stusitsc.ind]],
  #            pch = 19, col = "black")
  #     lines(t(tsdata5) ~ dates.days[[stusitsc.ind]], col = "cyan")
  #     points(t(tsdata5) ~ dates.days[[stusitsc.ind]],
  #            pch = 19, col = "cyan")
  #     abline(v = date.eventsc, lty = 2,
  #            col = "red")
  #     box(which = "plot")
  #     })
  # 
  # })
  
  ## Interactive Map ###########################################
  
  # Create the map
  
  #in order to add WMS tiels, see the comments below
    output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE,
                                     zoomControl = FALSE)) %>%
        addTiles(group = "OSM Map") %>%
        addProviderTiles('Esri.WorldImagery',
                         group = "ESRI World Satellite") %>%
        #uncomment the following lines in order to add WMS tiels and config them to your needs
        #here is a example of a WMS url to some well known services
        #https://stackoverflow.com/questions/9394190/leaflet-map-api-with-google-satellite-layer
        # addWMSTiles("https://url/to/WMS",
        #   layers = "0",
        #   group = "WMS Tiles",
        #   options = WMSTileOptions(format = "image/png", transparent = F),
        #   attribution = "a propper attribution to the service") %>%
        addLayersControl(
          baseGroups = c("ESRI World Satellite", "OSM Map"), #add the "WMS Tiles" string defined above with group = to the list 
          overlayGroups = c("Measurement Points", "Custom Geometry"),
          options = layersControlOptions(collapsed = T),
          position = "topleft") %>%
      setView(lng = 0, lat = 0, zoom = 3) %>%
      addScaleBar(position = "topleft",
                  options =scaleBarOptions(maxWidth = 100, metric = T, imperial = F))
    })

#####################adding PS to interactive map#####################
  # adding ps data selected for study site
  # make stusi initial excessible 
  observe({
  stusiBy <<- input$stusi
  })
  
  observe({
    pointsize <<- input$pcex
    stusi.ind <- which(stusi == stusiBy)
    disp <- ps.loc[[stusi.ind]]$disp
    colramp <- rev(matlab.like(10))
    colnum <- colorNumeric(colramp,
                           domain = c(min(ps.loc[[stusi.ind]]$disp),
                                      max(ps.loc[[stusi.ind]]$disp)))
    #show PS on map
    leafletProxy("map", data = ps.loc[[stusi.ind]]) %>%
      clearGroup("Measurement Points") %>%
      #add PS circles with color ramp
      addCircleMarkers(ps.loc[[stusi.ind]]$lon,
                       ps.loc[[stusi.ind]]$lat,
                       layerId = ps.loc[[stusi.ind]]$uid,
                       radius=pointsize,
                       fillColor = colramp[as.numeric(cut(disp, breaks = 10))],
                       fillOpacity = 0.8, stroke = F,
                       group = "Measurement Points") %>%
      addLegend("bottomleft", pal = colnum,
                values = ps.loc[[stusi.ind]]$disp,
                title = "mm/year",
                layerId = "uid", opacity = 1)
  })
  
  observe({
    #prepare PS stusi data
    stusiBy <<- input$stusi
    stusi.ind <- which(stusi == stusiBy)
    disp <- ps.loc[[stusi.ind]]$disp
    colramp <- rev(matlab.like(10))
    colnum <- colorNumeric(colramp,
                           domain = c(min(ps.loc[[stusi.ind]]$disp),
                                      max(ps.loc[[stusi.ind]]$disp)))
    #show PS on map
    leafletProxy("map", data = ps.loc[[stusi.ind]]) %>%
      clearGroup("Measurement Points") %>%
      #add PS circles with color ramp
      addCircleMarkers(ps.loc[[stusi.ind]]$lon,
                       ps.loc[[stusi.ind]]$lat,
                       layerId = ps.loc[[stusi.ind]]$uid,
                       radius=pointsize,
                       fillColor = colramp[as.numeric(cut(disp, breaks = 10))],
                       fillOpacity = 0.8, stroke = F,
                       group = "Measurement Points") %>%
      setView(lng = median(ps.loc[[stusi.ind]]$lon),
              lat = median(ps.loc[[stusi.ind]]$lat),
              zoom = 15)%>%
      setView(lng = median(ps.loc[[stusi.ind]]$lon),
              lat = median(ps.loc[[stusi.ind]]$lat),
              zoom = 15)%>%
      addLegend("bottomleft", pal = colnum,
                values = ps.loc[[stusi.ind]]$disp,
                title = "mm/year",
                layerId = "uid", opacity = 1)
  })
  
  observe({
     #upload and show custom geometry
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

##################plot TS for specific MP

observe({
  #date input
  in.date.event <- input$date
  orig.date <- as_date("0000-01-01")
  date.event <- as.numeric(in.date.event - orig.date)
  #stusi input
  stusi.ind <- which(stusi == stusiBy)
  #marker input
  click.map <<- input$map_marker_click
  if(is.null(click.map)){
    #create plot
    output$psts <- renderPlot({
      plot(1, 0, type = "n", axes = FALSE,
           ylab = c(""),
           xlab = c(""))
      text(1, 0, "Click on a point in order to plot deformation time series")
    })
  }else{if(click.map$group == "Custom Geometry"){
    shinyalert("No measurment point",
               "You have selected a custom geometry marker, click on a StaMPS measurement point in order to plot deformation time series",
               type = "error")}else{
      if(click.map$id > nrow(ps.loc[[stusi.ind]])){
        click.map$id <- 1}else{click.map$id <- click.map$id}
      last.click.id <<- click.map$id
      text <- paste('You have selected point', click.map$id, 'from case study', stusiBy, sep = ' ' )
      psts <<- ps.loc[[stusi.ind]][click.map$id, 5:ncol(ps.loc[[stusi.ind]])]
      #create plot
      output$psts <- renderPlot({
        plot(t(psts) ~ dates.days[[stusi.ind]], type = "n",
             ylab = "mm", xlab ="Date",
             xlim = c(min(dates.days[[stusi.ind]]),
                      max(dates.days[[stusi.ind]])),
             ylim = c(min(ps.loc[[stusi.ind]][, 5:ncol(ps.loc[[stusi.ind]])]),
                      max(ps.loc[[stusi.ind]][, 5:ncol(ps.loc[[stusi.ind]])])),
             axes = F)
        axis(side = 2)
        plot.info <- par("xaxp")
        tick.pos <- seq(plot.info[1], plot.info[2], length = plot.info[3]+1)
        xlabs <- as_date(tick.pos, origin = "0000-01-01")
        axis(side = 1, labels = xlabs, at = tick.pos)
        grid(col = "grey64")
        if(input$add.trend == 'ctrend'){
          lines(t(psts) ~ dates.days[[stusi.ind]])}else{
            if(input$add.trend == 'ltrend'){
              lmfit <- lm(t(psts) ~ dates.days[[stusi.ind]])
              abline(lmfit, col = 'black')}else{
                if(input$add.trend == 'ptrend'){
                  date.d <- dates.days[[stusi.ind]]
                  pfit <- lm(t(psts) ~ poly(date.d, 2, raw = TRUE))
                  xx <- seq(range(date.d)[1],
                            range(date.d)[2],
                            length.out = 250)
                  lines(xx, predict(pfit, data.frame(date.d = xx)), col = 'black')
                }else{return()}
              }
          }
        points(t(psts) ~ dates.days[[stusi.ind]],
               pch = 19)
        abline(v = date.event, lty = 2,
               col = "red")
        box(which = "plot")
      })
      #render output text
      output$Click_text <- renderText({text})
    }
  }
})


##################plot subtracted offset TS for specific MP

observeEvent(input$sub.offset, {
  #date input
  in.date.event <- input$date
  orig.date <- as_date("0000-01-01")
  date.event <- as.numeric(in.date.event - orig.date)
  #stusi input
  stusi.ind <- which(stusi == stusiBy)
  if(is.null(click.map) | is.null(psts)){
    shinyalert("No TS selected",
               "You have not selected a deformation time series, click on a StaMPS measurement point in order to plot deformation time series",
               type = "error")}else{
      offset <- psts[1,1]
      psts.off <- psts - offset
      #create plot
      output$psts <- renderPlot({
        plot(t(psts.off) ~ dates.days[[stusi.ind]], type = "n",
             ylab = "mm", xlab ="Date",
             xlim = c(min(dates.days[[stusi.ind]]),
                      max(dates.days[[stusi.ind]])),
             ylim = c(min(ps.loc[[stusi.ind]][, 5:ncol(ps.loc[[stusi.ind]])])-offset,
                      max(ps.loc[[stusi.ind]][, 5:ncol(ps.loc[[stusi.ind]])])-offset),
             axes = F)
        axis(side = 2)
        plot.info <- par("xaxp")
        tick.pos <- seq(plot.info[1], plot.info[2], length = plot.info[3]+1)
        xlabs <- as_date(tick.pos, origin = "0000-01-01")
        axis(side = 1, labels = xlabs, at = tick.pos)
        grid(col = "grey64")
        if(input$add.trend == 'ctrend'){
          lines(t(psts.off) ~ dates.days[[stusi.ind]])}else{
            if(input$add.trend == 'ltrend'){
              lmfit <- lm(t(psts.off) ~ dates.days[[stusi.ind]])
              abline(lmfit, col = 'black')}else{
                if(input$add.trend == 'ptrend'){
                  date.d <- dates.days[[stusi.ind]]
                  pfit <- lm(t(psts.off) ~ poly(date.d, 2, raw = TRUE))
                  xx <- seq(range(date.d)[1],
                            range(date.d)[2],
                            length.out = 250)
                  lines(xx, predict(pfit, data.frame(date.d = xx)), col = 'black')
                }else{return()}
              }
          }
        points(t(psts.off) ~ dates.days[[stusi.ind]],
               pch = 19)
        abline(v = date.event, lty = 2,
               col = "red")
        box(which = "plot")
      })}
})

# end of server function
}