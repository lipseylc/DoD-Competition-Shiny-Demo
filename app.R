################################################################################ 
# R Shiny Demonstration: Competition Data
# L. Lipsey for DIIG, May 2016
#
# This is a demonstration of interactive charts for the web using R Shiny,
# developed as part of the Developing Data Products Coursera course with
# data from DIIG.
#
# See a good Shiny tutorial at: http://shiny.rstudio.com/tutorial/
#
# Some data sources are required for this demo, and should be in the same
# folder as this script:
#     Competition.csv
#     cb_2014_us_state_20m.dbf
#     cb_2014_us_state_20m.shp
#     cb_2014_us_state_20m.shx from 
#
# To do:
#     * Rework color scaling to
#           - show differences better at the low end (e.g. log scale)
#           - re-calibrate based on mean of subcategories (so it's not so
#                 dark when looking at breakout categories)
#     * Add citations for ggmap package and data source
#     * Dynamically enable/disable year slider based on "all years" checkbox
#     * Enable ranges in years slider
#     * Add comparison feature
#           - via side-by-side separately adjustible maps, or
#           - via overlay of different year/category layers
#     * Add mouseover tooltips for states, using Hover input
################################################################################


# Shiny is the only package that is always required;
library(shiny)

# The rest are case-specific to this demonstration 
# (mostly used to draw graphics)
library(ggplot2)
library(rgdal)
library(scales)
library(ggmap)
library(rgeos)
library(maptools)
library(dplyr)


################################################################################
# Visual Interface settings go here.
#
# This interface, featuring fluidRow() is non-standard, though not particularly
# customized or complex.  Simpler default interfaces using a sidebar and main
# panel are even easier to build: see tutorial listed at the top of this script.
################################################################################

ui <- fluidPage(
      title = "Competition in DoD Contracting 2000-2014",
      fluidRow(
            column(4,
                   helpText("This chart shows Department of Defense contract",
                            "obligations by state from 2000-2014"),
                   helpText("'Competition rate' is the percentage of",
                            "size-weighted",
                            "contracts that received two or more bids."),
                   a("Code and documentation on GitHub", 
                     href="https://github.com/lipseylc/DoD-Competition-Shiny-Demo")
                   # checkboxInput('allyears', 'All Years', value = TRUE),
                  ),
            column(3,
                   radioButtons('disp', 'Displayed Variable',
                                                     c("Competition rate",
                                                    "Spending (constant 2014 $)",
                                                    "Spending (% of state GDP)"))
                  ),
            column(4,
                   selectInput('cat', 'Category', 
                               c("All","Products","R&D","Services")),
                   sliderInput('year', 'Fiscal Year', min=2000, max=2014,
                               value=2000, step=1, animate = TRUE, sep="")
                   )
            
      ),
      plotOutput('plot')
)
      

################################################################################
# Server block below is the code that builds the map for display;
# Everything within the "server" function is run once as the Shiny loads.
#
# Code in reactive blocks is run when the user changes a setting via widgets.
################################################################################

server <- function(input, output){
      
      ##########################################################################
      # Read in data and subset/aggregate it into several different data frames.
      # 
      # This is done up front so the subsetting only needs to be done once,
      # and the data frames are ready to go if the user asks for them.
      #
      # Doing it this way should add a bit to initial load time, but save time
      # in redrawing the map when the user changes options via the widgets.
      ##########################################################################
      
      # read in data - this is already broken out by contract category
      compdata <- read.csv("Competition.csv")
      
      # create a second data set ("aggcompdata") with contract categories 
      # aggregated (for use when "All" categories option is selected)
      aggcompdata <- aggregate(Amount ~ Year + id, data = compdata, sum)
      aggmultibid <- aggregate(Multibid ~ Year + id, data = compdata, sum)
      agggsppct <- aggregate(GSPpct ~ Year + id, data = compdata, sum)     
      aggdeflated <- aggregate(deflated ~ Year + id, data= compdata, sum)
      
      aggcompdata$Multibid <- aggmultibid$Multibid
      aggcompdata$GSPpct <- agggsppct$GSPpct
      aggcompdata$deflated <- aggdeflated$deflated
      aggcompdata$PercentMultibid <- aggcompdata$Multibid / aggcompdata$Amount
      
      # create third and fourth data sets with years aggregated but not
      # contract categories ("alldata") and with everything aggregated
      # ("aggalldata")
      alldata <- aggregate(Amount ~ id + Category, data = compdata, sum)
      allmultibid <- aggregate(Multibid ~ id + Category, data = compdata, sum)
      allgsp <- aggregate(gsp ~ id + Category, data = compdata, sum)
      alldeflated <- aggregate(deflated ~ id + Category, data = compdata, sum)
      
      alldata$Multibid <- allmultibid$Multibid
      alldata$GSPpct <- alldata$Amount / (allgsp$gsp * 10000)
      alldata$deflated <- alldeflated$deflated
      alldata$PercentMultibid <- alldata$Multibid / alldata$Amount
      
      aggalldata <- aggregate(Amount ~ id, data = alldata, sum)
      aggallmultibid <- aggregate(Multibid ~ id, data = alldata, sum)
      aggallgsppct <- aggregate(GSPpct ~ id, data = alldata, sum)     
      aggalldeflated <- aggregate(deflated ~ id, data= alldata, sum)
      
      aggalldata$Multibid <- aggallmultibid$Multibid
      aggalldata$GSPpct <- aggallgsppct$GSPpct
      aggalldata$deflated <- aggalldeflated$deflated
      aggalldata$PercentMultibid <- aggalldata$Multibid / aggalldata$Amount
      
       
      # read in map layers
      tract <- readOGR(".", layer = "cb_2014_us_state_20m")
      tract <- fortify(tract, region = "NAME")
      
      # Remove Puerto Rico
      tract <- tract[!(tract$id == "Puerto Rico"),]
      
      # Add competition data to map layers
      compdata$id <- as.character(compdata$id)
      aggcompdata$id <- as.character(aggcompdata$id)
      
      plotdata <- left_join(tract, compdata)
      aggplotdata <- left_join(tract, aggcompdata)
      
      # Remove Alaska and Hawaii for better visual scale
      plotdata <- plotdata[!(plotdata$id == "Alaska" |
                                   plotdata$id == "Hawaii"),]
      aggplotdata <- aggplotdata[!(aggplotdata$id == "Alaska" |
                                   aggplotdata$id == "Hawaii"),]
      
      ##########################################################################
      # Reactive blocks to pass the desired data to the map function
      # Based on user input, passes the:
      #     - Desired year
      #     - Desired subset of contracts (i.e. Products, Services, All)
      #     - Desired output variable (i.e. Raw Spending, Deflated Spending,
      #       Percent Effective Competition)
      ##########################################################################
      
      # tells the map what data to use
      dataset <- reactive({
            if(input$cat == "All"){
                  shown <- aggplotdata
            }
            else{shown <- plotdata[plotdata$Category == input$cat,]}
            
            shown <- shown[shown$Year == input$year,]
            shown
      })
      
      # tells the map what visual settings to use
      plotsettings <- reactive({
            if(input$disp == "Competition rate"){
                  p <- ggplot() +
                        geom_polygon(data = dataset(),
                                     aes(x = long, y = lat,
                                         group = group,
                                         fill = PercentMultibid),
                                     color = "black", size = 0.2) +
                        coord_map() +
                        theme_nothing(legend = TRUE) +
                        scale_fill_gradient2(limits= c(0,1),
                                            breaks=c(0.25,0.5,0.75),
                                            labels=c("25%","50%","75%"),
                                            midpoint=0.5,
                                            low="#990000",
                                            mid="white",
                                            high="#0000cc") +
                        labs(fill = "")    
            }
            else if(input$disp == "Spending (% of state GDP)"){
                  p <- ggplot() +
                        geom_polygon(data = dataset(),
                                     aes(x = long, y = lat,
                                         group = group,
                                         fill = GSPpct),
                                     color = "black", size = 0.2) +
                        coord_map() +
                        theme_nothing(legend = TRUE) +
                        scale_fill_gradient2(limits= c(0,7),
                                            breaks=c(1,2,3,4,5,6),
                                            labels=c("1%","2%","3%",
                                                     "4%","5%","6%"),
                                            midpoint = 5,
                                            low = "black",
                                            mid = "#eeee00",
                                            high = "#ffff00") +
                        labs(fill = "")
            }
            else if(input$disp == "Spending (constant 2014 $)"){
                  p <- ggplot() +
                        geom_polygon(data = dataset(),
                                     aes(x = long, y = lat,
                                         group = group,
                                         fill = deflated),
                                     color = "black", size = 0.2) +
                        coord_map() +
                        theme_nothing(legend = TRUE) +
                        scale_fill_gradientn(limits = c(0,50000000000),
                                             breaks = c(1000000000,
                                                        5000000000,
                                                        10000000000,
                                                        20000000000,
                                                        40000000000),
                                             labels = c("$1B",
                                                        "$5B",
                                                        "$10B",
                                                        "$20B",
                                                        "$40B"),
                                             colors = c("#000000",
                                                        "#003300",
                                                        "#008000",
                                                        "#00cc00",
                                                        "#00ff00"),
                                             values = rescale(c(0,
                                                        1000000000,
                                                        5000000000,
                                                        12000000000,
                                                        30000000000),
                                                        to = c(0,1))
                                             ) +
                        labs(fill = "")
            }
            
            p
      })      
   
      ##########################################################################
      # Draw the plot on the plot panel in browser
      ##########################################################################
      output$plot <- renderPlot({
            
            print(plotsettings())
            
      }, height=800)   
}

# Runs the app
shinyApp(ui= ui, server = server)