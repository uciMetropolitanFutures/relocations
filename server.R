setwd("E:/Documents/MFI/webmap/relocations")
library(shiny)
library(leaflet)
library(sp)
library(maptools)

# Reads in shapefiles, generates a data frame version of it to use for data display 'dfsub' and converts zero values to NA
sub <- readShapePoly("SoCal_place_2010_all")
dfsub <- data.frame(sub)
fips = unique(unlist(dfsub$plfips))


shinyServer(function(input, output) {
  
  # (1) REACTIVE FUNCTION FOR ZOOM-TO-CITY. Default coords are Irvine.
  center <- reactiveValues(x_coord=-117.7736, y_coord=33.67801)
  observeEvent(input$go, {
    center$x_coord = as.numeric(as.character(dfsub$INTPTLON10[dfsub$NAME10==input$city]))
    center$y_coord = as.numeric(as.character(dfsub$INTPTLAT10[dfsub$NAME10==input$city]))
  })
  
  # (2) REACTIVE FUNCTION FOR GRABBING USER INPUT AS 'options$choose'
  options = reactiveValues(choose="iall36770")
  observeEvent(input$go, {
    ind_link = switch(input$industry, "All"="all", "Retail"="ret", "Education/Healthcare"="hlt",
                      "FIRE"="fir", "Professional"="prf", "Service"="svc", "Industrial"="ind")
    tofromlink = switch(input$inout, "relocating to"="i", "relocating from"="o")
    cityfips = dfsub$plfips[dfsub$NAME10==input$city]
    options$choose = paste(tofromlink, ind_link, cityfips, sep="")
  })
  
  # (3) MAKE THE MAP AS A REACTIVE TO USER INPUTS
  finalMap <- reactive ({
    withProgress(message='Please Wait: Map Loading', {
    choice = dfsub[,grep(options$choose, colnames(dfsub))]
    choice2 = choice
    choice[choice==0] <- NA
    pal <- colorBin("YlOrRd", choice, bins=c(1, 5, 10, 25, 50, 100, 25000), na.color="#FFFFFF")
    
    
    # Create map. Invoke leaflet command on data object 'sub' to create map object 'm'
    m = leaflet(sub) %>%  setView(lng=center$x_coord, lat=center$y_coord, zoom=11) %>% addTiles() %>%
      addPolygons(data=sub, stroke=T, weight=1.1, fillColor = ~pal(choice), color="black", fillOpacity=0.5, 
                  opacity=1, popup=~paste(NAME10, " (", choice2, ")",sep=""))  %>%
      # legend takes object 'pal' as argument for color palette (also called pal...), and 'choice' for values.
      addLegend("bottomleft", pal=pal, values=~choice, opacity=0.75, na.label=~"None",
                title=~paste("Business", input$inout, input$city))
    })
  })
  
  # (4) PASS THE REACTIVE MAP TO AN OUTPUT OBJECT CALLED 'output$myMap' 
  output$myMap = renderLeaflet(finalMap())

  # (5) TEXT OUTPUT
  output$within <- renderText({
    paste("Total businesses relocating WITHIN ", input$city, ":", dfsub[,grep(options$choose, colnames(dfsub))][dfsub$NAME10==input$city])
  })
  output$total <- renderText({
    paste("Total businesses ", input$inout, input$city, ":", sum(dfsub[,grep(options$choose, colnames(dfsub))][dfsub$NAME10!=input$city]))
  })
  output$tablelabel <- renderText({
    paste("Top 5 cities which businesses", input$inout, input$city, switch(input$inout, "relocating to"="come from:", "relocating from"="go to:"))
  })
  output$topfive <- renderTable({
    a <- cbind(
      as.character(dfsub[order(-dfsub[,grep(options$choose, colnames(dfsub))]),][2:6,5]),
      as.numeric(dfsub[order(-dfsub[,grep(options$choose, colnames(dfsub))]),][2:6,grep(options$choose, colnames(dfsub))])  )
  })
  
})

