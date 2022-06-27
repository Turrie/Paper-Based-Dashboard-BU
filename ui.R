library(shinydashboard)
library(dplyr)
library(tidyr)
require(plyr)
library(rgdal)
library(raster)
library(dismo)
library(maptools)
library(rgeos)
library(shinyalert)
require(RColorBrewer)
require(graphics)
require(rasterVis)
library(sp)
library(ggthemes)
require(ggplot2)
library(gridExtra) 
library(hexbin)
library(viridis)
library(sf)
library(ggspatial)
library(grid)
require(ggrepel)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(grid)
#library(formattable)
library(shinycssloaders)
library(shinybusy)



### SHINY UI ###
ui <- bootstrapPage(
  # tags$head(includeHTML("gtag.html")),

  # navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,inverse = TRUE,
  # 
  #            #darkly,cyborg.flatly, slate, cosmo
  #            "Rwanda", id="nav",

  navbarPage(windowTitle ="Burundi",
             title = div(img(src = "https://akilimo.sirv.com/images/bu.png","Burundi", height = "60", style="margin-top: -14px;
                                padding-right:10px;
                               padding-bottom:10px")),
             theme = shinytheme("sandstone"), collapsible = TRUE,inverse = TRUE,id="nav",
             tabPanel("Use case mapper",
                      shinyalert::useShinyalert(), 
                      
                      
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          tmapOutput(outputId = "tmapplot", width="100%", height="100%"),
 
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        tags$head(includeCSS("styles.css")),
                                        top = 75, left = 70, width = 320, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                      uiOutput("province"),
                      uiOutput("unit_loc"),
                       conditionalPanel(
                        condition = "input.unit_loc == 'ha'",
                        uiOutput("FCY_ha")
                        
                      ),
                      
                      conditionalPanel(
                        condition = "input.unit_loc == 'acre'",
                        uiOutput("FCY_acre")
                        
                      ),
                      
                      
                      uiOutput("plntmth"),
                      uiOutput("selection2"),
                      #uiOutput("costs")
                      
                       ),
                      
                      #span(tags$i(h4("Give price information here.")), style="color:#045a8d"),
                      
                      #PRICE INCLUSION
                      # conditionalPanel(
                      # condition =  "input.costs == 'Yes' ",
                      # absolutePanel(id = "controls", class = "panel panel-default",
                      #               bottom = 75, left = 405, width = 250, fixed=TRUE,
                      #               draggable = TRUE, height = "auto",
                      #               
                      #               #"input.country == 'Nigeria'" &
                      #               # conditionalPanel(
                      #               # condition =  "input.costs == 'Yes' ",
                      #               # paste(span(tags$i(h4("Give price information here (Naira)")), style="color:##008000")))
                      #               # 
                      #               # conditionalPanel(
                      #               #   condition = "input.country == 'Tanzania'",
                      #               #   span(tags$i(h4("Give price information here (TZS)")), style="color:##008000")),
                      #               
                      #               uiOutput("CassavaPrice"),
                      #               uiOutput("FOMI_TOTAHAZAPrice"),
                      #               uiOutput("FOMI_IMBURAPrice"),
                      #               uiOutput("FOMI_BAGARAPrice"),
                      #               shinyalert::useShinyalert(),                 
                      #               
                      #               
                      # )),
                    
                      absolutePanel(id = "logo", class = "card", bottom = 100, right = 200, width = 200, fixed=TRUE, draggable = FALSE, 
                                    tags$img(src='https://akilimo.sirv.com/images/akilimo_icon.png',height = 290, width = 300
                                    ),
                                    tags$a(href="https://akilimo.org", "Learn more...")
                                    
                      ),
                     
                      absolutePanel(id = "go", class = "panel panel-default",
                                    bottom = 5, left = 405, width = 150, fixed=TRUE,
                                    draggable = TRUE, height = "auto",
                      uiOutput("btn_go")
                      )),
                      
                      #conditionalPanel(condition = "input.buton > 0", p("I'm a dashboard"))
                      conditionalPanel(condition = ("input.btn_go > 0"),
                      absolutePanel(id = "fr", class = "panel panel-default",
                                    bottom = 5, right = 100, width = 200, fixed=TRUE,
                                    draggable = TRUE, height = "auto",
                                    
                                    downloadButton("downloadDatafr", "Download printable guide")
                                    #uiOutput("downloadDatafr")
                      ),
                      
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "100",
                                        actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')",
                                                                       "https://twitter.com/ACAI_IITA")))
                       ),
             ),

             tabPanel("View maps side by side",width = "100%",
                    
                      textOutput("sidetext"),
                      br(),
                      
                      fluidRow(
                        column(6,
                               downloadButton("downloadData", "Download pdf of maps")
                        ),
                        column(6,
                               #downloadButton("downloadDatafr", "Download printable guide")
                        )
                      ),
                      fluidRow(
                        shinydashboard::box(width = 4,title = "FOMI-TOTAHAZA",withSpinner(tmapOutput(outputId = "FOMI_TOTAHAZAplot", width = 450, height = 600))),
                        shinydashboard::box(width = 4,title = "FOMI-IMBURA ",withSpinner(tmapOutput(outputId = "FOMI_IMBURAplot", width = 450, height = 600))),
                        shinydashboard::box(width = 4,title = "FOMI-BAGARA",withSpinner(tmapOutput(outputId = "FOMI_BAGARAplot", width = 450, height = 600))),
                        shinydashboard::box(width = 4,title = "Yield",withSpinner(tmapOutput(outputId = "yieldplot", width = 450, height = 600))))
                        
             ),

             
            
             tabPanel("View Table",
                     textOutput("tabletext_bu"),
                     
                     box(class = "mybg",
                         br(),
                         width = NULL, status = 'primary',
                         DT::dataTableOutput('mytable', width = "100%")
                         
                     )
                     
                    #  conditionalPanel(condition="input.costs == 'No'",
                    #                  box(class = "mybg",
                    #                      br(),
                    #                      width = NULL, status = 'primary',
                    #                      DT::dataTableOutput('mytable', width = "100%")
                    #                      
                    #                  )
                    # ),
                    # conditionalPanel(condition="input.costs == 'Yes'",
                    #                  box(class = "mybg",
                    #                      br(),
                    #                      width = NULL, status = 'primary',
                    #                      DT::dataTableOutput('mytable2', width = "100%")
                    #                      
                    #                  )
                    # )
                    
                    
                      ),
             
             tabPanel(
               uiOutput("tabers")
             ),

    tags$style(HTML(".navbar-header { width:45% }
                   .navbar-brand { width: 45%; font-size: 23px}")) # cen    
                      
             
  )          
)


