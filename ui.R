library(shiny)
library(leaflet)
library(sp)
library(maptools)

# Files must be read in here if ui.R calls something from them (in this case, 'names')
sub <- readShapePoly("SoCal_place_2010all.shp")
dfsub <- data.frame(sub)
names = as.character(unique(unlist(dfsub$NAME10[dfsub$oall>9 & dfsub$iall>9])))

# This app uses a tab panel with a custom .css style. The whole UI is within the parentheses following div()
shinyUI(navbarPage("Business Relocations in Southern California", id="nav",
  tabPanel("2010-2014", div(class="outer", tags$head(includeCSS("styles.css")),
        # Indicates where to put the leaflet (map) output, and for it to fill the page                                                       
        leafletOutput("myMap", width="100%", height="100%"),
        # This is the right-side user input panel 
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                     draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto", width = 330, height = "auto",
                     br(""),
                     radioButtons("inout", label=strong("Select Businesses which are"), choices=list("relocating to", "relocating from"), selected="relocating to", inline=T),
                     br(""),
                                                      
                     selectInput("city", label=strong("Select City:"), selected="Irvine", choices = names[order(names)]),
                     
                     selectInput("industry", label=strong("Select Industry: "), selected="All",
                                 choices = list("All", "Retail", "Education/Healthcare", "FIRE", "Professional", "Service", "Industrial")),  
                     
                     actionButton("go", label="Click to Refresh After Changing Selection"),
                     br(""),
                     textOutput("within"),
                     br(""),
                     textOutput("tablelabel"),
                     tableOutput("topfive")
                     ),
        # this is a small left-side panel with some additional info. 
        absolutePanel(id = "controls", class="panel panel-default", fixed = TRUE,
                     draggable=TRUE, top=110, left=10, right="auto", bottom="auto", width=160, height="auto",
                     p("Data Notes:"),
                     h6("-- Please click to refresh after making new selections to ensure correct map and legend are displayed."),
                     h6("-- A relocation is defined as a business establishment that moves at least 0.1 miles in a given year."),
                     #h6(textOutput("var_desc")),
                     #h6(textOutput("var_desc2")),
                     h6("-- See", a("our website", href="http://mfi.soceco.uci.edu", target="_blank"), "for details.")
                                                               )
))))


