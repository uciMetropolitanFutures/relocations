library(shiny)
library(leaflet)
library(maptools)

sub <- readShapePoly("SoCal_place_2010_all")
dfsub <- data.frame(sub)
cats <- read.csv("SoCal_place_2010_allcats.csv")

shinyServer(function(input, output) {
  #### (1) REACTIVE FUNCTION FOR ZOOM-TO-CITY. Default coordinates are Irvine.
  center <- reactiveValues(x_coord=-117.7736, y_coord=33.67801)
  observeEvent(input$go, {
    center$x_coord = as.numeric(as.character(dfsub$INTPTLON10[dfsub$NAME10==input$city]))
    center$y_coord = as.numeric(as.character(dfsub$INTPTLAT10[dfsub$NAME10==input$city]))
  })
  #### (2) REACTIVE FUNCTION FOR GRABBING USER INPUT AS 'options$choose'
  options = reactiveValues(choose="iall36770")
  observeEvent(input$go, {
    tofromlink = switch(input$inout, "relocating to"="i", "relocating from"="o")
    cityfips = dfsub$plfips[dfsub$NAME10==input$city]
    options$choose = paste(tofromlink, "all", cityfips, sep="")
  })
  #### (3) MAKE THE MAP AS A REACTIVE TO USER INPUTS
  finalMap <- reactive ({
    #withProgress(message='Please Wait: Map Loading', {
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
    #})
  })
  
  #### (4) PASS THE REACTIVE MAP TO AN OUTPUT OBJECT CALLED 'output$myMap' 
  output$myMap = renderLeaflet(finalMap())
  
  #### (5) TEXT OUTPUT - MAP PAGE
  output$within <- renderText(paste("Relocations WITHIN ", input$city, " (2010-2014):", dfsub[,grep(options$choose, colnames(dfsub))][dfsub$NAME10==input$city]))
  output$total <- renderText(paste("Businesses ", input$inout, input$city, " (2010-2014):", sum(dfsub[,grep(options$choose, colnames(dfsub))][dfsub$NAME10!=input$city])))
  output$tablelabel <- renderText(paste("Top 5 cities which businesses", input$inout, input$city, switch(input$inout, "relocating to"="come from (2010-2014):", "relocating from"="go to:")))
  output$topfive <- renderTable({
    a <- cbind(as.character(dfsub[order(-dfsub[,grep(options$choose, colnames(dfsub))]),][2:6,5]),
      as.numeric(dfsub[order(-dfsub[,grep(options$choose, colnames(dfsub))]),][2:6,grep(options$choose, colnames(dfsub))])  )
  })
  
  #### (6) REACTIVE SELECTION - MULTIPLE INDUSTRIES PAGE 
  options2 = reactiveValues(choose2="iall36770")
  observeEvent(input$go2, {
    ind_link2 = switch(input$industry2, "All"="all", "Retail"="ret", "Education/Healthcare"="hlt",
                      "FIRE"="fir", "Professional"="prf", "Service"="svc", "Industrial"="ind")
    tofromlink2 = switch(input$inout2, "relocating to"="i", "relocating from"="o")
    cityfips2 = cats$plfips[cats$NAME10==input$city2]
    options2$choose2 = paste(tofromlink2, ind_link2, cityfips2, sep="")
  })
  
  #### (7) TEXT OUTPUT - MULTIPLE INDUSTRIES PAGE 
  output$var_desc <- renderText({
    data_notes = switch(input$industry2,
                        "All" = "all business establishments.",
                        "Retail" = "Retail Trade (NAICS 44-45) and Accommodation and Food Service (NAICS 72).",
                        "Education/Healthcare" = "Educational Services (NAICS 61) and Health Care and Social Assistance (NAICS 62).",
                        "FIRE" = "Finance and Insurance (NAICS 52) and Real Estate and Rental and Leasing (NAICS 53).",
                        "Professional" = "Information (NAICS 51), Professional, Scientific, and Technical Services (NAICS 54), and Management of Companies and Enterprises (NAICS 55).",
                        "Service" = "Administrative and Support and Waste Management and Remediation Services (NAICS 56) and Other Services except Public Administration (NAICS 81).",
                        "Industrial" = "Utilities (NAICS 22), Manufacturing (NAICS 31-33), Wholesale Trade (NAICS 42), and Transportation and Warehousing (NAICS 48-49).")
    paste(" -- ", input$industry2, " includes ", data_notes, sep="")
  })
  output$within2 <- renderText(paste("Relocations WITHIN ", input$city2, " (2010-2014):", cats[,grep(options2$choose2, colnames(cats))][cats$NAME10==input$city2]))
  output$total2 <- renderText(paste("Businesses ", input$inout2, input$city2, " (2010-2014):", sum(cats[,grep(options2$choose2, colnames(cats))][cats$NAME10!=input$city2])))
  output$tablelabel2 <- renderText(paste("Top Ten cities which", input$industry2, "businesses", input$inout2, input$city2, switch(input$inout2, "relocating to"="come from  (2010-2014):", "relocating from"="go to:")))
  output$topfive2 <- renderTable({
    b <- cbind(as.character(cats[order(-cats[,grep(options2$choose2, colnames(cats))]),][2:11,6]),
      as.numeric(cats[order(-cats[,grep(options2$choose2, colnames(cats))]),][2:11,grep(options2$choose2, colnames(cats))])  )
  })
})

