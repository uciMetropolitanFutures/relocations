library(shiny)
library(leaflet)
library(maptools)

sub <- readShapePoly("SoCal_place_2010_all")
dfsub <- data.frame(sub)
names = as.character(unique(unlist(dfsub$NAME10[dfsub$oall>9 & dfsub$iall>9 & dfsub$CLASSFP10=='C1'])))

shinyUI(navbarPage("Business Relocations in Southern California, 2010-2014", id="nav",
  tabPanel("Total (Map)", div(class="outer", tags$head(includeCSS("styles.css")),
        leafletOutput("myMap", width="100%", height="100%"),
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                     draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto", width = 330, height = "auto",
                     br(""),
                     selectInput("city", label=strong("(1) Select a City:", style="color:blue"), selected="Irvine", choices = names[order(names)]),
                     radioButtons("inout", label=strong("(2) Select Businesses which are:", style="color:blue"), choices=list("relocating to", "relocating from"), selected="relocating to", inline=T),
                     actionButton("go", label=a("Click to Refresh After Changing Selection"), style="color:red"),
                     br(""),
                     textOutput("within"),
                     textOutput("total"),
                     textOutput("tablelabel"),
                     tableOutput("topfive")
                     ),
        absolutePanel(id = "controls", class="panel panel-default", fixed = TRUE,
                     draggable=TRUE, top=110, left=10, right="auto", bottom="auto", width=180, height="auto",
                     h6(strong("This webmap shows where businesses are moving to, or where they came from.", style="color:blue")),
                     h6("-(1)- Select a city"),
                     h6("-(2)- Choose to analyze businesses that are relocating TO or relocating FROM the selected city."),
                     h6("-(3)- Click 'Refresh' after changing selections."),
                     h6("-- Clicking on the map displays the number of businesses moving into/from the city selected at right."),
                     h6("-- A relocation is defined as a business establishment that moves at least 0.1 miles in a given year."),
                     h6("-- Please be patient while the map loads!"),
                     h6("-- By the UCI", a("Metropolitan Futures Initiative.", href="http://mfi.soceco.uci.edu", target="_blank"), 
                        "See ", a("the full report here.", href="http://mfi.soceco.uci.edu/category/quarterly-report", target="_blank"))
                    )
)),
  tabPanel("By Industry (Tables)", div(class="outer", tags$head(includeCSS("styles.css")),
          sidebarLayout(
            sidebarPanel(
              selectInput("city2", label=strong("(1) Select a City:", style="color:blue"), selected="Irvine", choices = names[order(names)]),
              selectInput("industry2", label=strong("(2) Select an Industry: ", style="color:blue"), selected="All",
                          choices = list("All", "Retail", "Education/Healthcare", "FIRE", "Professional", "Service", "Industrial")),  
              radioButtons("inout2", label=strong("(3) Select Businesses which are:", style="color:blue"), choices=list("relocating to", "relocating from"), selected="relocating to", inline=T),
              actionButton("go2", label=a("Click to Refresh After Changing Selection", style="color:red")),
              br(""),
              textOutput("var_desc")
            ),
            mainPanel(
              h2(strong("Relocations by Industry", style="color:blue")),
              p(em("This panel displays statistics for business relocations by industry, which are not able to be shown in the map panel due to spatial data limitations.", style="color:blue")),
              textOutput("within2"),
              textOutput("total2"),
              textOutput("tablelabel2"),
              tableOutput("topfive2")
            ))))
))


