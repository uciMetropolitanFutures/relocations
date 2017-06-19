library(shiny)
library(leaflet)
library(sp)
library(maptools)

# Files must be read in here if ui.R calls something from them (in this case, 'names')
sub <- readShapePoly("SoCal_place_2010_all")
dfsub <- data.frame(sub)
names = as.character(unique(unlist(dfsub$NAME10[dfsub$oall>9 & dfsub$iall>9 & dfsub$CLASSFP10=='C1'])))

# This app uses a tab panel with a custom .css style. The whole UI is within the parentheses following div()
shinyUI(navbarPage("Business Relocations in Southern California", id="nav",
  tabPanel("2010-2014", div(class="outer", tags$head(includeCSS("styles.css")),
        # Indicates where to put the leaflet (map) output, and for it to fill the page                                                       
        leafletOutput("myMap", width="100%", height="100%"),
        # This is the right-side user input panel 
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                     draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto", width = 330, height = "auto",
                     br(""),
                     radioButtons("inout", label=strong("Select Businesses which are:"), choices=list("relocating to", "relocating from"), selected="relocating to", inline=T),
                     selectInput("city", label=strong("Select City:"), selected="Irvine", choices = names[order(names)]),
                     
                     selectInput("industry", label=strong("Select Industry: "), selected="All",
                                 choices = list("All", "Retail", "Education/Healthcare", "FIRE", "Professional", "Service", "Industrial")),  
                     
                     actionButton("go", label="Click to Refresh After Changing Selection"),
                     br(""),
                     textOutput("within"),
                     textOutput("total"),
                     textOutput("tablelabel"),
                     tableOutput("topfive")
                     ),
        # this is a small left-side panel with some additional info. 
        absolutePanel(id = "controls", class="panel panel-default", fixed = TRUE,
                     draggable=TRUE, top=110, left=10, right="auto", bottom="auto", width=180, height="auto",
                     p(strong("Data Notes:")),
                     h6("-- This webmap shows where businesses are moving to, or where they came from."),
                     h6("-- Choose a city and whether to display businesses that relocated to or from that city."),
                     h6("-- Please click to refresh after making new selections to ensure correct map and legend are displayed."),
                     h6("-- Clicking on a city displays the number of businesses moving into/from the city selected on the right side panel."),
                     h6("-- A relocation is defined as a business establishment that moves at least 0.1 miles in a given year."),
                     h6("-- Please be patient while the map loads!"),
                     #h6(textOutput("var_desc")),
                     h6("-- By the UCI", a("Metropolitan Futures Initiative.", href="http://mfi.soceco.uci.edu", target="_blank"), 
                        "See ", a("the full report here.", href="http://mfi.soceco.uci.edu/category/quarterly-report", target="_blank"))
                    )
))))


