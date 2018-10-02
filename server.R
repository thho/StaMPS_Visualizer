library(leaflet)
library(colorRamps)
library(lubridate)

function(input, output, session) {
  
  output$ts1 <- renderUI({
    stusitscBy <- input$stusitsc
    stusitsc.ind <- which(stusi == stusitscBy)
    selectInput('ts11', '1. Time Series',
                c("Select TS" = NA, ps.loc[[stusitsc.ind]]$uid),
                selected = NA)
  })
  output$ts2 <- renderUI({
    stusitscBy <- input$stusitsc
    stusitsc.ind <- which(stusi == stusitscBy)
    selectInput('ts22', '2. Time Series',
                c("Select TS" = NA, ps.loc[[stusitsc.ind]]$uid),
                selected = NA)
  })
  output$ts3 <- renderUI({
    stusitscBy <- input$stusitsc
    stusitsc.ind <- which(stusi == stusitscBy)
    selectInput('ts33', '3. Time Series',
                c("Select TS" = NA, ps.loc[[stusitsc.ind]]$uid),
                selected = NA)
  })
  output$ts4 <- renderUI({
    stusitscBy <- input$stusitsc
    stusitsc.ind <- which(stusi == stusitscBy)
    selectInput('ts44', '4. Time Series',
                c("Select TS" = NA, ps.loc[[stusitsc.ind]]$uid),
                selected = NA)
  })
  output$ts5 <- renderUI({
    stusitscBy <- input$stusitsc
    stusitsc.ind <- which(stusi == stusitscBy)
    selectInput('ts55', '5. Time Series',
                c("Select TS" = NA, ps.loc[[stusitsc.ind]]$uid),
                selected = NA)
  })
  
  observe({
    #date input
    in.date.eventsc <- input$datesc
    orig.datesc <- as_date("0000-01-01")
    date.eventsc <- as.numeric(in.date.eventsc - orig.datesc)
    #studysite and ts data
    stusitscBy <- input$stusitsc
    stusitsc.ind <- which(stusi == stusitscBy)
    tsdata1 <- ps.loc[[stusitsc.ind]][input$ts11, 5:ncol(ps.loc[[stusitsc.ind]])]
    tsdata2 <- ps.loc[[stusitsc.ind]][input$ts22, 5:ncol(ps.loc[[stusitsc.ind]])]
    tsdata3 <- ps.loc[[stusitsc.ind]][input$ts33, 5:ncol(ps.loc[[stusitsc.ind]])]
    tsdata4 <- ps.loc[[stusitsc.ind]][input$ts44, 5:ncol(ps.loc[[stusitsc.ind]])]
    tsdata5 <- ps.loc[[stusitsc.ind]][input$ts55, 5:ncol(ps.loc[[stusitsc.ind]])]
    
    output$tscomp <- renderPlot({
      plot(t(tsdata1) ~ dates.days[[stusitsc.ind]], type = "n",
           ylab = "mm", xlab ="Date",
           xlim = c(min(dates.days[[stusitsc.ind]]),
                    max(dates.days[[stusitsc.ind]])),
           ylim = c(min(ps.loc[[stusitsc.ind]][, 5:ncol(ps.loc[[stusitsc.ind]])]),
                    max(ps.loc[[stusitsc.ind]][, 5:ncol(ps.loc[[stusitsc.ind]])])),
           axes = F)
      
      axis(side = 2)
      plot.info <- par("xaxp")
      tick.pos <- seq(plot.info[1], plot.info[2], length = plot.info[3]+1)
      xlabs <- as_date(tick.pos, origin = "0000-01-01")
      axis(side = 1, labels = xlabs, at = tick.pos)
      grid(col = "grey64")
      
      lines(t(tsdata1) ~ dates.days[[stusitsc.ind]], col = "red")
      points(t(tsdata1) ~ dates.days[[stusitsc.ind]],
             pch = 19, col = "red")
      lines(t(tsdata2) ~ dates.days[[stusitsc.ind]], col = "blue")
      points(t(tsdata2) ~ dates.days[[stusitsc.ind]],
             pch = 19, col = "blue")
      lines(t(tsdata3) ~ dates.days[[stusitsc.ind]], col = "darkgreen")
      points(t(tsdata3) ~ dates.days[[stusitsc.ind]],
             pch = 19, col = "darkgreen")
      lines(t(tsdata4) ~ dates.days[[stusitsc.ind]], col = "black")
      points(t(tsdata4) ~ dates.days[[stusitsc.ind]],
             pch = 19, col = "black")
      lines(t(tsdata5) ~ dates.days[[stusitsc.ind]], col = "cyan")
      points(t(tsdata5) ~ dates.days[[stusitsc.ind]],
             pch = 19, col = "cyan")
      abline(v = date.eventsc, lty = 2,
             col = "red")
      box(which = "plot")
      })

  })
  
  ## Interactive Map ###########################################
  
  # Create the map
    output$map <- renderLeaflet({
    leaflet() %>% 
    addTiles(group = "OSM Map") %>%
    addProviderTiles('Esri.WorldImagery', group = "Satellite") %>% 
        addLayersControl(
        baseGroups = c("Satellite", "OSM Map"),
          overlayGroups = c("Measurement Points"),
          options = layersControlOptions(collapsed = T),
          position = "topleft"
        ) %>%
      setView(lng = 0, lat = 0, zoom = 3) %>%
      addScaleBar(position = "topright", 
                  options =scaleBarOptions(maxWidth = 100, metric = T, imperial = F))
    })

  
  #adding ps data selected for study site
  observe({
    stusiBy <- input$stusi
    stusi.ind <- which(stusi == stusiBy)
    disp <- ps.loc[[stusi.ind]]$disp
    colramp <- rev(matlab.like(10))
    colnum <- colorNumeric(colramp, 
                           domain = c(min(ps.loc[[stusi.ind]]$disp),
                                      max(ps.loc[[stusi.ind]]$disp)))
    leafletProxy("map", data = ps.loc[[stusi.ind]]) %>%
      #clearShapes() %>%
      clearMarkers() %>%
      addCircleMarkers(ps.loc[[stusi.ind]]$lon,
                 ps.loc[[stusi.ind]]$lat,
                 layerId = ps.loc[[stusi.ind]]$uid,
                 radius=5,
                 fillColor = colramp[as.numeric(cut(disp ,breaks = 10))],
                 fillOpacity = 0.8, stroke = F,
                 group = "Measurement Points") %>%
              setView(lng = median(ps.loc[[stusi.ind]]$lon),
              lat = median(ps.loc[[stusi.ind]]$lat),
              zoom = 15)%>%
    addLegend("bottomleft", pal = colnum, 
              values = ps.loc[[stusi.ind]]$disp,
              title = "mm/year",
              layerId = "uid", opacity = 1)
  })
  
  observe({
    #date input
    in.date.event <- input$date
    orig.date <- as_date("0000-01-01")
    date.event <- as.numeric(in.date.event - orig.date)
    #studysite input
    stusiBy <- input$stusi
    stusi.ind <- which(stusi == stusiBy)
    click<-input$map_marker_click
    if(is.null(click))
      return()
    text2<-paste("You've selected point ", click$id)
    ts <- ps.loc[[stusi.ind]][click$id, 5:ncol(ps.loc[[stusi.ind]])]
    
    output$psts <- renderPlot({
      plot(t(ts) ~ dates.days[[stusi.ind]], type = "n",
           ylab = "mm", xlab ="Date",
           xlim = c(min(dates.days[[stusi.ind]]),
                    max(dates.days[[stusi.ind]])),
           ylim = c(min(ps.loc[[stusi.ind]][, 5:ncol(ps.loc[[stusi.ind]])]),
                    max(ps.loc[[stusi.ind]][, 5:ncol(ps.loc[[stusi.ind]])])),
           axes = F
      )
      
      axis(side = 2)
      plot.info <- par("xaxp")
      tick.pos <- seq(plot.info[1], plot.info[2], length = plot.info[3]+1)
      xlabs <- as_date(tick.pos, origin = "0000-01-01")
      axis(side = 1, labels = xlabs, at = tick.pos)
      
      grid(col = "grey64")
      lines(t(ts) ~ dates.days[[stusi.ind]])
      points(t(ts) ~ dates.days[[stusi.ind]],
             pch = 19)
      abline(v = date.event, lty = 2,
             col = "red")
      box(which = "plot")
    })
    
    output$Click_text <- renderText({
      text2
    })
  })
}
