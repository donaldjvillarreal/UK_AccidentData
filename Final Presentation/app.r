#
# Traffic Accidents
# Richard Gower
# 
#

#devtools::install_github("dgrtwo/gganimate")
library(data.table)
library(shiny)
library(shinythemes)
library(scales)
library(ggplot2)
library(lubridate)
library(caret)
library(plyr)
library(DT)
library(RColorBrewer)
library(hexbin)
library(tableHTML)
library(tidyverse)


#library(gganimate)

uk <- map_data('world')[map_data('world')$region == 'UK',]
uk <- uk[uk$subregion != 'Northern Ireland',]


# Define UI 
ui = navbarPage("", 
                theme = shinytheme("cerulean"),

                
                tabPanel("",
                         fluidRow(column(width = 4,
                                                   plotOutput("introMap_all")),
                                            column(width = 4,
                                                   HTML("<center><H3>PREDICTING SEVERITY OF AN ACCIDENT</H3>"),
                                                   tags$br(),
                                                   tags$h4("UK Road Accidents and Safety Statistics"),
                                                   tags$h4("UK Department for Transportation"),  
                                                   tags$h4("2005 - 2016"),
                                                   img(src='logo.gif'),
                                                   tags$h4("Team 4"),
                                                   HTML("<p>Aaron Wright</p><p>Donald Villarreal</p><p>Richard Gower</p></center>"),  
                                                   HTML("</center>")),
                                            column(width = 4,
                                                   plotOutput("introMap_fatal"))
                         )
                ),
                
                tabPanel("Intro",
                        sidebarLayout(sidebarPanel(width = 1),
                                      mainPanel(width = 11,
                                         fluidRow(column(width = 6, style = "background-color:white;",
                                                                          htmlOutput("overview_slide1"),
                                                                          htmlOutput("overview_slide2")
                                                             ),
                                                  column(width = 6,style = "background-color:white;",
                                                                 img(src = "presentation.jpg", 
                                                                  width = 460)
                                                         )
                                                   )
                                                )
                                )
                          ),               
                
                
                tabPanel("Raw Data",
                         tabsetPanel(type = "tabs",
                                     tabPanel("The Data", 
                                              sidebarLayout(
                                                sidebarPanel(width = 1,
                                                             radioButtons("theData", "",
                                                                          c("Origin" = "1",
                                                                            "Files" = "2",
                                                                            "Look ups" = "3")
                                                             )
                                                             ),
                                                mainPanel(width = 11,
                                                          fluidRow(column(width = 6, style = "background-color:white;",
                                                                          htmlOutput("data_slide1"),
                                                                          htmlOutput("data_slide2")
                                                                    ),
                                                                   column(width = 6,
                                                                          conditionalPanel(condition = "input.theData == '1'",
                                                                                            tags$iframe(style="height:600px; width:100%", 
                                                                                            src="stats19.pdf")
                                                                          )
                                                                          ),
                                                                   column(width = 5,
                                                                          conditionalPanel(condition = "input.theData == '2'",
                                                                                            img(src = "dataERD.jpg",
                                                                                              width = 200)
                                                                          )
                                                                    ),
                                                                   column(width = 5,
                                                                          conditionalPanel(condition = "input.theData == '3'",
                                                                                           img(src = "lookups.jpg",
                                                                                               width = 300)
                                                                          )
                                                                   )
                                                          )
                                                )
                                              )
                                     ),
                                     tabPanel("Tables",
                                              sidebarLayout(
                                                sidebarPanel(width=2,
                                                             selectInput("tables_table",
                                                                         label = "Table",
                                                                         choices = c("Accidents" = "Accidents",
                                                                                     "Casualties" = "Casualties",
                                                                                     "Vehicles" = "Vehicles"),
                                                                         selected = "1"),
                                                             numericInput("table_numCols", "Columns", 0)
                                                ),
                                                mainPanel(width = 10,            
                                                          fluidRow( column(12,
                                                                           htmlOutput("table_outFields"),
                                                                           tags$head(tags$style("#table_outFields{
                                                                                                font-size: 11px;
                                                                                                }"
                                                                                          )
                                                                           )
                                                                           )
                                                                           )
                                                          )
                                                          ) 
                                                ),
                                     tabPanel("Response Variable",
                                              sidebarLayout(
                                                sidebarPanel(width = 1,
                                                             tags$style(make_css(list('.well', 'border-width', '0px'))),
                                                             tags$style(make_css(list('.well', 'padding', '2px'))),
                                                             tags$style(make_css(list('.well', 'background-color', '#ffffff'))),
                                                             radioButtons("response", "",
                                                                          c("Tot" = "1",
                                                                            "Yr" = "2",
                                                                            "%" = "3")
                                                             )
                                                ),
                                                mainPanel(width = 11,
                                                          fluidRow(column(width = 6,
                                                                          htmlOutput("response_1"),
                                                                          plotOutput("response_plot1")

                                                                          
                                                          ),
                                                          column(width = 6,
                                                                 verbatimTextOutput("response_table1") 
                                                                 
                                                          )
                                                          )
                                                )
                                              )
                                     ),

                                     tabPanel("Predictors",
                                              sidebarLayout(
                                                sidebarPanel(width=3,
                                                             radioButtons("investigate", "",
                                                                          c("Distribution" = "1",
                                                                            "Missing" = "2"),
                                                                          selected = "1"
                                                             ),

                                                             selectInput("da_table",
                                                                         label = "Table",
                                                                         choices = c("Accidents" = "Accidents",
                                                                                     "Casualties" = "Casualties",
                                                                                     "Vehicles" = "Vehicles"
                                                                         ),
                                                                         selected = "Accidents"
                                                             ),
                                                             selectInput("da_field",
                                                                         label = "Field",
                                                                         choices = c(""),
                                                                         selected = "1"
                                                             ),
                                                             radioButtons("include", "Field Inclusion",
                                                                          c("All" = "1",
                                                                            "Initial Cut" = "2",
                                                                            "Final Selection" = "3")
                                                             )
                                                ),
                                                mainPanel(width = 9,            
                                                          fluidRow(column(6,
                                                                           htmlOutput("outField"),
                                                                           htmlOutput("outDescription"),
                                                                           tags$head(tags$style("#outDescription{
                                                                                                font-size: 10px;
                                                                                               }")),
                                                                           tableOutput("outDistribution"),
                                                                           tags$head(tags$style("#outDistribution{
                                                                                               font-size: 10px;
                                                                                               }"))
                                                                           ),
                                                                   column(6,
                                                                          plotOutput("outDistPlot")         
                                                                           )
                                                                  )

                                                          )
                                                )
                                              ),
                                     tabPanel("Near Zero",
                                              sidebarLayout(
                                                sidebarPanel(width = 3,
                                                             selectInput("nz_table",
                                                                         label = "Table",
                                                                         choices = c("Accidents" = "Accidents",
                                                                                     "Casualties" = "Casualties",
                                                                                     "Vehicles" = "Vehicles"
                                                                         ),
                                                                         selected = "Accidents"
                                                             ),
                                                             selectInput("nz_field",
                                                                         label = "Field",
                                                                         choices = c("")
                                                             ),
                                                             checkboxInput("hideList", 
                                                                           label = "Hide List", 
                                                                           FALSE),
                                                             HTML("<pre><p style=font-size:90%;><b>freqRatio:</b><br />freq of most/freq of 2nd<br /><b>%Unique:</b><br /># unique values/# samples</p></pre>")

                                                ),
                                                mainPanel(width = 9,
                                                          fluidRow(column(6,
                                                                          htmlOutput("nz_outTable"),  
                                                                          tableOutput("nz_outDetailTable"),
                                                                          tags$head(tags$style("#nz_outDetailTable{
                                                                                                font-size: 9px;
                                                                               }")),
                                                                          plotOutput("outDistPlot2") 
                                                                ),
                                                          column(6,
                                                                  
                                                                 htmlOutput("nz_outField"),
                                                                 tags$head(tags$style("#nz_outField{
                                                                                                font-size: 9px;
                                                                               }")),
                                                                 htmlOutput("nz_outMax"),
                                                                 tags$head(tags$style("#nz_outMax{
                                                                                                font-size: 9px;
                                                                               }")),
                                                                 
                                                                 tableOutput("nz_outTopNZV"),
                                                                 tags$head(tags$style("#nz_outTopNZV{
                                                                                                font-size: 9px;
                                                                               }"))
                                                          )
                                                          )
                                                          
                                                )
                                              )
                                     ),
                                     tabPanel("Modifications", 
                                              sidebarLayout(
                                                sidebarPanel(width = 1),
                                                mainPanel(width = 11,
                                                          fluidRow(column(width = 8, style = "background-color:white;",
                                                                          htmlOutput("modifications_slide1")
                                                          ),
                                                          column(width = 4)
                                                          )
                                                          
                                                )
                                              )
                                     ),
                                     tabPanel("Summary",
                                              sidebarLayout(
                                                sidebarPanel(width = 2,
                                                             selectInput("summary_table",
                                                                         label = "Table",
                                                                         choices = c("Accidents" = "Accidents",
                                                                                     "Casualties" = "Casualties",
                                                                                     "Vehicles" = "Vehicles"
                                                                         ),
                                                                         selected = "Accidents"
                                                             )),
                                                mainPanel(width = 10, 
                                                          htmlOutput("summary_title"),
                                                          fluidRow(
                                                            column(width = 6, style = "background-color:white;",

                                                              tags$h5("Fields Used In Models"),
                                                              tableOutput("summary_keep"),
                                                              tags$head(tags$style("#summary_keep{
                                                                                                font-size: 9px;
                                                                               }")),
                                                          
                                                              tags$h5("Fields Used In Models With Modifications"),
                                                              tableOutput("summary_modify"),
                                                              tags$head(tags$style("#summary_modify{
                                                                                                font-size: 9px;
                                                                               }"))

                                                            ), 
                                                            column(width = 6, 
                                                              tags$h5("Fields Not Used In Models"),
                                                              tableOutput("summary_getridof"),
                                                              tags$head(tags$style("#summary_getridof{
                                                                                                font-size: 9px;
                                                                               }"))
                                                          )
                                                )
                                                )
                                              )
                                     )
       
                                     )),
                tabPanel("Feature Creation",
                         tabsetPanel(type = "tabs",
                                     tabPanel("New Features", 
                                              sidebarLayout(
                                                sidebarPanel(width = 2,
                                                             radioButtons("decisions", "Slide",
                                                                          c("1" = "1",
                                                                            "2" = "2",
                                                                            "3" = "3",
                                                                            "4" = "4")
                                                             )
                                                ),
                                                mainPanel(width = 10,
                                                          fluidRow(column(width = 6, style = "background-color:white;",
                                                                          htmlOutput("decisions_slide1"),
                                                                          htmlOutput("decisions_slide2")
                                                                          ),
                                                                  column(width = 6)
                                                              )
                                                          
                                                          )
                                              )
                                     )
                         )
                ),
                tabPanel("The Models",
                         tabsetPanel(type = "tabs",
                                     tabPanel("Data Preparation", 
                                              sidebarLayout(
                                                sidebarPanel(width = 1),
                                                mainPanel(width = 11, fluidRow(column(width = 6, style = "background-color:white;",
                                                                                      htmlOutput("modelprep_slide1")
                                                                                      )
                                                                          )
                                                          )
                                              )
                                     ),                                    
                                     
                                     tabPanel("Intro", 
                                              sidebarLayout(
                                                sidebarPanel(width = 1),
                                                mainPanel(width = 11, fluidRow(column(width = 6, style = "background-color:white;",
                                                                                      htmlOutput("modeli_slide1"),
                                                                                      htmlOutput("modeli_slide2")

                                                                                      )
                                                                              )
                                                          )
                                                )
                                     ),
                                     tabPanel("Model 1", 
                                              sidebarLayout(
                                                sidebarPanel(width = 1),
                                                mainPanel(width = 11, fluidRow(column(width = 6, style = "background-color:white;",
                                                                                      htmlOutput("model1_slide1"),
                                                                                      htmlOutput("model1_slide2"),
                                                                                      img(src = "randomforest2.jpg", 
                                                                                          width = 460)
                                                                                      ),
                                                                               column(width = 6,style = "background-color:white;",
                                                                                      img(src = "randomforest1.jpg", 
                                                                                          width = 460)
                                                                                      )
                                                                              )
                                                          )
                                                )
                                     ),
                                     tabPanel("Model 2", 
                                              sidebarLayout(
                                                sidebarPanel(width = 1),
                                                mainPanel(width = 11, fluidRow(column(width = 6, style = "background-color:white;",
                                                                                      htmlOutput("model2_slide1"),
                                                                                      htmlOutput("model2_slide2")
                                                                                      ),
                                                                               column(width = 6,style = "background-color:white;",
                                                                                      img(src = "penalizedmultinomial1.jpg", 
                                                                                          width = 460)
                                                                               )
                                                                              )
                                                          )
                                              )
                                     ),
                                     tabPanel("Model 3", 
                                              sidebarLayout(
                                                sidebarPanel(width = 1, 
                                                             radioButtons("model3", "Slide",
                                                                                   c("1" = "1",
                                                                                     "2" = "2")
                                                              )
                                                ),
                                                mainPanel(width = 10, fluidRow(column(width = 6, style = "background-color:white;",
                                                                                      htmlOutput("model3_slide1"),
                                                                                      htmlOutput("model3_slide2")
                                                                                      ),
                                                                               column(width = 6,style = "background-color:white;",
                                                                                      conditionalPanel(condition = "input.model3 == '2'",
                                                                                                      img(src = "regression1.jpg", 
                                                                                                        width = 460)
                                                                                      )
                                                                               )
                                                                              )
                                                        )
                                              )
                                     ),
                                     tabPanel("Model Selection", 
                                              sidebarLayout(
                                                sidebarPanel(width = 1,
                                                             radioButtons("selection", "",
                                                                          c("Test" = "1",
                                                                            "Final" = "2")
                                                             )
                                                             ),
                                                mainPanel(width = 11, fluidRow(column(width = 6, style = "background-color:white;",
                                                                                      htmlOutput("comparison_slide1"),
                                                                                      htmlOutput("comparison_slide2")
                                                                                      ),
                                                                               column(width = 6,
                                                                                      htmlOutput("comparison_title1"),
                                                                                      verbatimTextOutput("comparison_table1",placeholder = FALSE),
                                                                                      tags$head(tags$style("#comparison_table1{
                                                                                                font-size: 10px;
                                                                               }")),
                                                                                      htmlOutput("comparison_title2"),
                                                                                      verbatimTextOutput("comparison_table2",placeholder = FALSE), 
                                                                                      tags$head(tags$style("#comparison_table2{
                                                                                                font-size: 10px;
                                                                               }")),
                                                                                      htmlOutput("comparison_title3"),
                                                                                      verbatimTextOutput("comparison_table3",placeholder = FALSE),
                                                                                      tags$head(tags$style("#comparison_table3{
                                                                                                font-size: 10px;
                                                                                                           }"))
                                                                               )
                                                
                                                                                )
                                                          )
                                                  )
                                            )
                            )
                         
                ),
                tabPanel("Summary",
                         sidebarLayout(
                           sidebarPanel(width = 1),
                           mainPanel(width = 10, fluidRow(column(width = 7, style = "background-color:white;",
                                                                 htmlOutput("summary_slide1"),
                                                                 htmlOutput("summary_slide2")
                                                                  )
                           
                                                          )
                                    )
                            )
                ),
                # tabPanel("Prediction",
                #          sidebarLayout(
                #            sidebarPanel(width = 4,"Predictors",
                #                         numericInput("predict_numVehicles", "Num Vehicles", 
                #                                      1, min = 1, max = 100, step = 1,
                #                                      width = 75),
                #                         selectInput("predict_vehicles", "Vehicles:",
                #                                     c("Car" = "carl",
                #                                       "Truck" = "truck",
                #                                       "Horse" = "horser"),
                #                                     multiple = TRUE),
                #                         numericInput("predict_numCasualties", "Num Casualties", 
                #                                      1, min = 1, max = 100, step = 1,
                #                                      width = 75),
                #                         div(style="display: inline-block;horizontal-align:top; width: 100px;",
                #                             checkboxInput("predict_fog", "Fog", value = FALSE),
                #                             checkboxInput("predict_2", "Ice", value = FALSE)),
                #                         div(style="display: inline-block;horizontal-align:top; width: 100px;",
                #                             checkboxInput("predict_3", "Rain", value = FALSE),
                #                             checkboxInput("predict_4", "Wind", value = FALSE)),
                #                         checkboxGroupInput("predict_elections", ":",
                #                                            c("Rain" = "rain",
                #                                              "Snow" = "snow",
                #                                              "Ice" = "ice")),
                #                         selectInput("state", "Choose a state:",
                #                                     list(`East Coast` = c("NY", "NJ", "CT"),
                #                                          `West Coast` = c("WA", "OR", "CA"),
                #                                          `Midwest` = c("MN", "WI", "IA"))
                #                         ),
                #                         actionButton("predict_go", "Predict" )
                #            ),
                #            mainPanel(width = 6,
                #                      conditionalPanel(condition = "output$predict_result == ''",
                #                                       img(src = "pleasewait.gif", 
                #                                           width = 360)
                #                      ),
                #                      textOutput("predict_title"),
                #                      textOutput("predict_result")
                #                      
                #            )
                #          )
                # ),

                tabPanel("Tools",
                         tabsetPanel(type = "tabs",
                                     tabPanel("R Functions",
                                              sidebarLayout(
                                                sidebarPanel(width = 3,
                                                             selectInput("ql_table",
                                                                         label = "Table",
                                                                         choices = c("Accidents" = "Accidentsc",
                                                                                     "Casualties" = "Casualties",
                                                                                     "Vehicles" = "Vehicles"
                                                                         ),
                                                                         selected = "1"
                                                             ),
                                                             radioButtons("ql_type", 
                                                                          "Report:",
                                                                          c("Table Structure" = "str",
                                                                            "Top 10 Rows" = "head"))
                                                ),
                                                mainPanel(width = 9,
                                                          verbatimTextOutput("outStr")
                                                          
                                                )
                                              )
                                     )
                                    )
                )

        )




# Define server logic required to draw a histogram
server = function(input, output, session) {

  catPlot<-function(df, var, lbl='', sevField = 'Accident_Severity'){
    if (sevField != "") {
        plotData<-data.frame(byvar = levels(factor(df[[var]])),   # Get list of level names
                         freq = summary(factor(df[[var]])),   # Calculate frequency for each level
                         slight = tapply(df[[sevField]] == 'Slight', df[[var]], mean) / mean(df[[sevField]] == 'Slight'),
                         serious = tapply(df[[sevField]] == 'Serious', df[[var]], mean) / mean(df[[sevField]] == 'Serious'),
                         fatal = tapply(df[[sevField]] == 'Fatal', df[[var]], mean) / mean(df[[sevField]] == 'Fatal'))
    } else {
      plotData<-data.frame(byvar = levels(factor(df[[var]])),   # Get list of level names
                           freq = summary(factor(df[[var]])))   # Calculate frequency for each level

    }
    par(mar=c(7,5,2,5), cex=1.0) # bottom, left, top, right
    
    #---------------------------------------------------------------------------- 
    # Create bar chart of relative frequencies.  Factor levels are printed on the
    # horizontal axis
    # mp = The midpoint of each bar (Used to position points of line plot)
    #----------------------------------------------------------------------------
    if (sevField != "") {
        mp<-with(plotData,barplot(height=100*freq/sum(freq), col='lightblue',
                              xlim=c(0,1.2*nrow(plotData)+.2), 
                              ylim = c(0,100), 
                              las=1,
                              names.arg=byvar, 
                              axes = F))
    } else {
      mp<-with(plotData,barplot(height=100*freq/sum(freq), col='lightblue',
                                xlim=c(0,1.2*nrow(plotData)+.2), 
                                #ylim = c(0,100), 
                                ylim = c(0,max(100*freq/sum(freq))+5),
                                las=1,
                                names.arg=byvar, 
                                axes = F))
    }
    
    #--- create right axis ---
    axis(side=4)
    mtext(side=4, line=3, '% of total accidents', cex=1)
    
    #---------------------------------------------------------------------------- 
    # Overlay line plot
    #----------------------------------------------------------------------------
    if (sevField != "") {
      par(new=T)
      plot(x=c(0,max(mp)+.5), y=c(0,max(plotData[,3:5])), type='n', 
         axes=F, xlim = c(0, 1.2*nrow(plotData)+.2), 
         xlab=NA, ylab=NA, main=lbl)
      lines(x=mp,y=plotData$slight, type='b', col='darkgreen', pch=16)
      lines(x=mp,y=plotData$serious, type='b', col='gold', pch=16)
      lines(x=mp,y=plotData$fatal, type='b', col='red', pch=16)
    
    #--- create left axis --- 
      axis(side=2)
      mtext(side=2, line=3, 'Casualty Rate Relative to Mean', cex=1)
    
    #----------------------------------------------------------------------------
    # Create legend
    #----------------------------------------------------------------------------  
      par(xpd=T, new=T)
      plot(x=c(0,1), y=c(0,1), xlim=c(0,1), ylim=c(0,1), type='n', axes=F, xlab=NA, ylab=NA)
      legend(x=-0.1, y=-.2, legend=c('slight',"serious",'fatal'),
           text.col=c("darkgreen","gold",'red'), lty=1, 
           col=c("darkgreen","gold",'red'), cex=1,
           xpd=T, bty='o', pch=16, horiz=TRUE)
    }
  }
  
  map.uk <- function(df, title=''){
    ggplot(data=df, aes(x=Longitude, y=Latitude)) +
      
      # Tan background of British map
      geom_polygon(data=uk, aes(x=long, y=lat, group=group), fill='tan') +
      
      # Create heatmap of hexagonal bin counts
      stat_binhex(bins=200) +
      
      # Black outline of British map
      geom_polygon(data=uk, aes(x=long, y=lat, group=group), fill='NA', color='black', size=.2) +
      
      # Fix the aspect ratio (y/x)
      coord_fixed(1.3) +
      
      # Change the color theme and adjust the breakpoints
      scale_fill_gradientn(colours=brewer.pal(n=9, name='YlOrRd'), trans='log') +
      
      # Remove gridlines, axis titles and text, color water blue
      theme(axis.ticks=element_blank(), 
            axis.title=element_blank(),
            axis.text=element_blank(),
            panel.grid = element_blank(),
            panel.background=element_rect(fill='lightblue', 
                                          color='black')) +
      
      # Add plot title
      ggtitle(title)
  }
  
  
    print(paste("Prepping data",Sys.time())) 

    print(paste("fReading acc csv",Sys.time())) 
    acc = data.frame(fread(file="datafiles\\acc.csv"))
    
    print(paste("fReading veh csv",Sys.time())) 
    veh = data.frame(fread(file="datafiles\\veh.csv"))

    print(paste("fReading cas csv",Sys.time())) 
    cas = data.frame(fread(file="datafiles\\cas.csv"))
    
    print(paste("Reading fieldDescriptions",Sys.time())) 
    load(file="datafiles\\fieldDescriptions.rda")
    
    print(paste("Reading lookups",Sys.time())) 
    load(file="datafiles\\lookups.rda")
    print(paste("Completed data prep",Sys.time())) 

     theme_richard = theme_dark() +
                      theme(axis.text.x = element_text(color="black",size=12,face="plain"),
                      axis.text.y = element_text(color="black",size=12,angle=0,face="plain"),  
                      axis.title.x = element_text(color="black",size=14,angle=0,hjust=0.5,vjust=1,face="plain"),
                      axis.title.y = element_text(color="black",size=14,angle=90,hjust=0.5,vjust=0.6,face="plain"),
                      plot.title = element_text(size = 20, colour="black"),
                      plot.background = element_rect(fill = "white"), 
                      panel.background = element_rect(fill = "white", color = "white", size = 0.5, linetype = "solid"),
                      panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "grey30"), 
                      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', color = "grey30"))
    
    options(DT.options = list(pageLength = 5, language = list(search = 'Filter:')))

    # observe({
    #   tableName = input$da_table
    #   if (tableName == "Accidents"){
    #     choiceFields = colnames(acc)      
    #   }
    #   else if (tableName == "Vehicles"){
    #     choiceFields = colnames(veh)
    #   }
    #   else if (tableName == "Casualties"){
    #     choiceFields = colnames(cas)
    #   }
    # 
    #   updateSelectInput(session, inputId = "da_field", choices = choiceFields)
    # })

#    
# intro
#

    output$introMap_all = renderPlot({
      print(paste("Map total start",Sys.time()))
      map.uk(acc, 'Total Auto Casualties - 2005 to 2016')
    })
    output$introMap_fatal = renderPlot({
      print(paste("Map fatal start",Sys.time()))
      map.uk(acc[acc$Accident_Severity == 1,], 'Auto Fatalities - 2005 to 2016')
    }) 
    

# RAW DATA    
        
#
# Overview
#    
    output$overview_slide1 = renderUI({
      tagList(
        tags$h3("Project Purpose"),
        tags$ul(
          tags$li("Accident data from UK from 2005-2016"), 
          tags$li("Predict the severity level - Fatal, Severe or Slight"), 
          tags$li("Challenge with multinomial response variable"), 
          tags$li("Focus on the accident parameters not the location"), 
          tags$li("Useful for setting initial insurance reserves or fraud") 
        )
      )
    }
    )
#
# The Data    
#        
    output$data_slide1 = renderUI({
      if (input$theData == 1){
        tagList(
          tags$h3("Origin of the Data"),
          tags$ul(
            tags$li("Accident data from UK Department for Transportation"),
            tags$li("Data from the STATS19 form, filled in by police or self filled"),
            tags$li("Form is 4 pages, 78 Questions"),
            tags$li("Different files targeting specific information"),
            tags$li("Data has been cleaned"),            
            tags$li("Many different files targeting specific information")
          )
        )
      }
      else
      if (input$theData == 2){
        tagList(
          tags$h3("The Data Files"),
          tags$ul(
            tags$li("We chose three key file - Accidents, Vehicles and Casualties"),
            tags$li("Files for each for 2005-2014, 2015, 2016 - same formats"),
            tags$li("Files in CSV format"),
            tags$li("Read in the files and merged the data"),
            tags$li("1,917,274 accident records with 32 columns"),
            tags$li("2,584,293 casualty records with 15 columns"),
            tags$li("3,514,770 vehicle records with 22 columns")
          )
        )
      }
      else
        if (input$theData == 3){
          tagList(
            tags$h3("Look up Tables"),
            tags$ul(
              tags$li("Most of the data is categorical, converted into numbers"),
              tags$li("Typically -1 is for unknown or missing data"),
              tags$li("There is a spreadsheet of look up values for the data"),
              tags$li("One sheet per field"),
              tags$li("Not all field names matched perfectly"),
              tags$li("Needed some manual clean up")
            )
          )
        }
    })

#
# Response Variable
#
    
    output$response_1 = renderUI({
      tagList(
        tags$h3("Severity of the Accident"),
        tags$ul(
          tags$li("Multinomial response variable"), 
          tags$li("Three values: Fatal, Serious, Slight"),
          tags$li("Highly imbalanced - 1.3%, 13.8%, 85%"),
          tags$li("Severity is also in the Casualties records")
          
        )
      )
    })
 
       
 #   observe({
      print(paste("Prepping response variable data",Sys.time()))
      sevByDate = acc[,c("Accident_Severity","Date")]
      sevByDate$Accident_Severity = factor(sevByDate$Accident_Severity, levels=c(1,2,3), labels = c("Fatal","Serious","Slight"))
      sevByDate$Year = as.integer(substr(sevByDate$Date,start=7, stop=10))
      library(dplyr)
      sevByYear = sevByDate %>%
        count(Year, Accident_Severity)
      names(sevByYear)[3] = "Count"
      print(paste("Completed response variable data",Sys.time()))
      print(paste("Prebuilding graphs",Sys.time()))      
      plot_severity_total = ggplot(data=sevByYear, aes(x=Accident_Severity, y=Count, fill=Accident_Severity)) + 
        geom_col() +
        labs(color = "Severity") +
        scale_fill_manual(labels = c("Fatal", "Serious", "Slight"), values = c("red", "gold", "darkgreen")) +
        theme_richard +
        #     theme(legend.justification=c(0,0), legend.position=c(0.3,0.79), legend.direction = "vertical") +
        theme(panel.background = element_rect(fill = "grey"))+
        theme(legend.background = element_rect(fill = "grey")) +
        theme(legend.key = element_rect(fill = "grey")) + # element_blank()) +
        theme(legend.title = element_text(colour="black", size=13)) +
        theme(legend.text = element_text(colour="black", size = 10)) +
        theme(legend.key.width = unit(0.45, "cm")) +
        theme(legend.key.height = unit(0.3, "cm"))
      
      dfSevByYear = transform(sevByYear, TotCount = ave(Count, Year, FUN = sum))
      plot_severity_by_year = ggplot(data=dfSevByYear, aes(x=Year, y=Count, col=Accident_Severity)) + 
        geom_point(size=3) +
        labs(color = "Severity") +
        
        scale_color_manual(labels = c("Fatal", "Serious", "Slight"), values = c("red", "gold", "darkgreen")) +
        theme_richard +
        theme(legend.justification=c(0,0), legend.position=c(0.3,0.79), legend.direction = "horizontal") +
        theme(panel.background = element_rect(fill = "grey"))+
        theme(legend.background = element_rect(fill = "grey")) +
        theme(legend.key = element_rect(fill = "grey")) + # element_blank()) +
        theme(legend.title = element_text(colour="black", size=13)) +
        theme(legend.text = element_text(colour="black", size = 10)) +
        theme(legend.key.width = unit(0.45, "cm")) +
        theme(legend.key.height = unit(0.3, "cm")) +
        scale_y_continuous(limits = c(-5000, 200000)) +
        scale_x_continuous(breaks = seq(2005, 2017, 2))
      
      dfSevByYear$Percent = (dfSevByYear$Count/dfSevByYear$TotCount)*100
      plot_severity_percent_by_year = ggplot(data=dfSevByYear, aes(x=Year, y=Percent, col=Accident_Severity)) + 
        geom_point(size=3) +
        labs(color = "Severity") +
        
        scale_color_manual(labels = c("Fatal", "Serious", "Slight"), values = c("red", "gold", "darkgreen")) +
        theme_richard +
        theme(legend.justification=c(0,0), legend.position=c(0.3,0.881), legend.direction = "horizontal") +
        theme(panel.background = element_rect(fill = "grey"))+
        theme(legend.background = element_rect(fill = "grey")) +
        theme(legend.key = element_rect(fill = "grey")) + # element_blank()) +
        theme(legend.title = element_text(colour="black", size=13)) +
        theme(legend.text = element_text(colour="black", size = 10)) +
        theme(legend.key.width = unit(0.45, "cm")) +
        theme(legend.key.height = unit(0.3, "cm")) +
        scale_y_continuous(limits = c(0, 100)) +
        scale_x_continuous(breaks = seq(2005, 2017, 2))
      print(paste("Completed graphs",Sys.time()))    
#    })


    
# Response variable table - count by year    
    output$response_table1 = renderPrint({
      if (input$response == 1) {
        Count = xtabs(~ Accident_Severity, data=sevByDate)    
        Percent= round(prop.table(xtabs(~ Accident_Severity, data=sevByDate))*100 ,2)
        c = rbind(Count,Percent)
        noquote(format(addmargins(c,2), 
               digits=2, nsmall=0, big.mark=",", trim=TRUE, scientific = FALSE))
      }
      else
      if (input$response == 2) {
        #addmargins(xtabs(~ Year+Accident_Severity, data=sevByDate))   # add row/col summary (default is sum)
      
        noquote(format(addmargins(xtabs(~ Year+Accident_Severity, data=sevByDate)), 
              digits=2, nsmall=0, big.mark=",", trim=TRUE))
      }
      else
      if (input$response == 3) {
        #addmargins(xtabs(~ Year+Accident_Severity, data=sevByDate))   # add row/col summary (default is sum)
        
        noquote(format(addmargins(prop.table(xtabs(~ Year+Accident_Severity, data=sevByDate),1)*100,2), 
               digits=3, nsmall=0, big.mark=",", trim=TRUE))
      }
    })
    
 # response variable - plot of accident severity by year
    output$response_plot1 = renderPlot({
      if (input$response == 1) {
        plot_severity_total

      }
      else
      if (input$response == 2 ) {
        plot_severity_by_year

      }
      else
        if (input$response == 3 ) {
          plot_severity_percent_by_year
          
        }
    }
    , height = 250
    )

#    
# Tables    
#
    output$table_outFields = renderTable({
      tempActionTable = fieldDescriptions[fieldDescriptions$Table == input$tables_table,
                                          c("FieldName", "Description", "Values", "Action" = "InitialSelection")]
      #tempActionTable$Actions = gsub("\n", "<br/>", tempActionTable$Actions)
      tempActionTable = tempActionTable [tempActionTable$FieldName != "",]
      tempActionTable[order(tempActionTable$FieldName),]
    })

    
  observeEvent(input$tables_table, {
    theTable = input$tables_table
    colcount = 0
    if (theTable == "Accidents"){
      colcount = ncol(acc)
    }
    else
      if (theTable == "Vehicles"){
        colcount = ncol(veh)
      }
    else
      if (theTable == "Casualties"){
        colcount = ncol(cas)
      }
    updateNumericInput(session, "table_numCols", value = colcount)
    
  })
    

#
# Analysis
#     
  
   observe({
     tableName = input$da_table
     whatToInclude = input$include
     if (whatToInclude == 1) {
       choiceFields = fieldDescriptions[fieldDescriptions$Table == tableName, "FieldName"]
       choiceFields = sort(choiceFields[choiceFields != ""])
     } else if (whatToInclude == 2)
     {
       choiceFields = fieldDescriptions[fieldDescriptions$InitialSelection != "Not used" & fieldDescriptions$Table == tableName, "FieldName"]
       choiceFields = sort(choiceFields[choiceFields != ""])
     } else if (whatToInclude == 3)
     {
       choiceFields = fieldDescriptions[fieldDescriptions$Delete == "N" & fieldDescriptions$Table == tableName, "FieldName"]
       choiceFields = sort(choiceFields[choiceFields != ""])       
     }
     # sort(colnames(acc[,fields]))  
     # if (tableName == "Accidents"){
     #   choiceFields = sort(colnames(acc))      
     # }
     # else if (tableName == "Vehicles"){
     #   choiceFields = sort(colnames(veh))
     # }
     # else if (tableName == "Casualties"){
     #   choiceFields = sort(colnames(cas))
     # }
     updateSelectInput(session, inputId = "da_field", choices = choiceFields, selected = choiceFields[1])
   })
  
   output$outField = renderUI({
     tableName = input$da_table
     fieldName = input$da_field
     if (input$investigate == 1) {
       tagList(
         tags$h5(fieldName)
       )
     } else if (input$investigate == 2) {
       tagList(
         tags$h5(tableName)
       )
     }
   })

    output$outDescription = renderUI({
      tableName = input$da_table
      fieldName = input$da_field
      if (input$investigate == 1) {
        # General predictor descriptions
        fieldDesc = fieldDescriptions[fieldDescriptions$Table == tableName & fieldDescriptions$FieldName == fieldName,"Description"]
        print(fieldDesc)
        if (tableName == "Accidents"){
          theDF = acc
        } else if (tableName == "Vehicles"){
          theDF = veh
        } else 
          if (tableName == "Casualties"){
          theDF = cas
        }
        uniqueValues = length(unique(theDF[ , fieldName])) 
        totalValues = nrow(theDF)
        actionsValue = fieldDescriptions[fieldDescriptions$Table == tableName & fieldDescriptions$FieldName == fieldName,"Actions"]
        desc = paste("<b>","Description:", "</b>", fieldDesc)
        unique = paste("<br />","<b>","Number of unique values:", "</b>", format(uniqueValues, decimal.mark = ".", big.mark = ",", small.mark = ","))
        total = paste("<br />","<b>","Total Observations:","</b>", format(totalValues, decimal.mark = ".", big.mark = ",", small.mark = ","))
        actions = paste("<br />","<b>","Actions:","</b>", actionsValue)
      
        HTML(paste(desc, actions, total, unique, "<br />"))
        
      } else if (input$investigate == 2){
        # Missing Data Text
        totalRows = 2
        whatToInclude = input$include
        print(paste("Missing",tableName))
        if (whatToInclude == 1) {
          print("What = 1")
          choiceFields = fieldDescriptions[fieldDescriptions$Table == tableName, "FieldName"]
          choiceFields = sort(choiceFields[choiceFields != ""])
        } else if (whatToInclude == 2)
        {
          print("What = 2")
          choiceFields = fieldDescriptions[fieldDescriptions$InitialSelection != "Not used" & fieldDescriptions$Table == tableName, "FieldName"]
          choiceFields = sort(choiceFields[choiceFields != ""])
        } else if (whatToInclude == 3)
        {
          print("What = 3")
          choiceFields = fieldDescriptions[fieldDescriptions$Delete == "N" & fieldDescriptions$Table == tableName, "FieldName"]
          choiceFields = sort(choiceFields[choiceFields != ""])       
        }

        print(paste("Num fields:",length(choiceFields)))
        if (tableName == "Accidents") {
          totalRows = nrow(acc) 
          totalRowsWithMissingValue = length(unique(which(acc[, choiceFields] == -1, arr.ind=TRUE)[,1]))
        } else if (tableName == "Casualties") {
          totalRows = nrow(cas) 
          totalRowsWithMissingValue = length(unique(which(cas[, choiceFields] == -1, arr.ind=TRUE)[,1]))
        } else if (tableName == "Vehicles") {
          totalRows = nrow(veh) 
          totalRowsWithMissingValue = length(unique(which(veh[, choiceFields] == -1, arr.ind=TRUE)[,1]))
        }
        #totalRowsWithMissingValue = length(unique(tempMissingRows[, 1]))
        
        line1 = paste("<b>", "Total rows:", "</b>", format(totalRows, decimal.mark = ".", big.mark = ",", small.mark = ","))
        line2 = paste("<b>", "Rows missing 1+ fields:", "</b>", format(totalRowsWithMissingValue, decimal.mark = ".", big.mark = ",", small.mark = ","))
        line3 = paste("<b>", "Percent of rows missing:", "</b>", round((100*totalRowsWithMissingValue)/totalRows,1),"%")
        HTML(paste(line1,line2,line3, sep="<br/>"))
      }
    })
    
    output$outDistribution = renderTable({

      tableName = input$da_table
      fieldName = input$da_field  
      if (input$investigate == 1) {
        if (tableName == "Accidents"){
          numUnique = length(unique(acc[ , fieldName]))
        } else
          if (tableName == "Vehicles"){
          numUnique = length(unique(veh[, fieldName]))
        } else 
          if (tableName == "Casualties"){
            numUnique = length(unique(cas[, fieldName]))
        }      
        print(paste("NumUnique:",numUnique))

        if (numUnique < 200){
          if (tableName == "Accidents"){
            tempTable = table(acc[ , fieldName])
          } else 
            if (tableName == "Vehicles"){
            tempTable = table(veh[, fieldName])
          } else 
            if (tableName == "Casualties"){
            tempTable = table(cas[, fieldName])
          }  

          tempDF = data.frame(tempTable)

          colnames(tempDF) = c("Value", "Frequency")
          tempDF$Percent = round((tempDF$Frequency*100)/sum(tempDF$Frequency),2)
          tempLookups = lookups[lookups$field == fieldName,c("numCode", "label")]
          colnames(tempLookups) = c("Value", "Lookup")
          tempDF = join(tempDF, tempLookups, by = "Value")
          tempDF = subset(tempDF, select=c(Value, Lookup, Frequency, Percent))
          rm(tempTable)
          rm(tempLookups)
          tempDF
        } else
        { 
          if (tableName == "Accidents"){
            SampleValues = acc[1:8, fieldName]
          } else if (tableName == "Vehicles"){
            SampleValues = veh[1:8, fieldName]
          } else if (tableName == "Casualties"){
            SampleValues = cas[1:8, fieldName]
          }  
          data.frame(SampleValues)
        }
      } else if (input$investigate == 2) {
        whatToInclude = input$include

        print(paste("Missing2",tableName))
        if (whatToInclude == 1) {
          print("What = 1")
          choiceFields2 = fieldDescriptions[fieldDescriptions$Table == tableName & 
                                              fieldDescriptions$MissingCount > 0 , 
                                            c("FieldName","MissingCount", "MissingPercent")]
          missingFields = choiceFields2
        } else if (whatToInclude == 2)
        {
          print("What = 2")
          choiceFields2 = fieldDescriptions[fieldDescriptions$InitialSelection != "Not used" & 
                                              fieldDescriptions$Table == tableName &
                                              fieldDescriptions$MissingCount > 0,
                                            c("FieldName","MissingCount", "MissingPercent")]
          missingFields = choiceFields2
        } else if (whatToInclude == 3)
        {
          print("What = 3")
          choiceFields2 = fieldDescriptions[fieldDescriptions$Delete == "N" &
                                              fieldDescriptions$Table == tableName &
                                              fieldDescriptions$MissingCount > 0, 
                                            c("FieldName","MissingCount", "MissingPercent")]
          missingFields = choiceFields2     
        }
        if (tableName == "Accidents"){
          totalRows2 = nrow(acc) 
        } else if (tableName == "Vehicles"){
          totalRows2 = nrow(veh) 
        } else if (tableName == "Casualties"){
          totalRows2 = nrow(cas) 
        }  
        
        missingFields$MissingPercent = round(100*as.integer(missingFields$MissingCount)/totalRows2,2)
        colnames(missingFields) = c("Field Name","Missing", "%")

        missingFields

        
        # if (tableName == "Accidents") {
        #   totalRows = nrow(acc) 
        #   #nrow(acc[acc$Junction_Control == -1,])
        #   
        #   #rows = unique(which(acc[, choiceFields2] == -1, arr.ind=TRUE)[,1])
        #   #sum(acc[rows, choiceFields2])
        #   
        #   datatable = acc[, choiceFields2]%>%
        #     gather(x, value, choiceFields2)%>%
        #     group_by(x)%>%
        #     tally(value == -1)
        #   
        # } else if (tableName == "Casualties") {
        #   totalRows = nrow(cas) 
        #   datatable = cas[, choiceFields2]%>%
        #     gather(x, value, choiceFields2)%>%
        #     group_by(x)%>%
        #     tally(value == -1)
        # } else if (tableName == "Vehicles") {
        #   totalRows = nrow(veh) 
        #   datatable = veh[, choiceFields2]%>%
        #     gather(x, value, choiceFields2)%>%
        #     group_by(x)%>%
        #     tally(value == -1)
        # }
        # datatable

      }
    })    

    output$outDistPlot = renderPlot({
      tableName = input$da_table
      fieldName = input$da_field 
      if (input$investigate == 1) {
        if (substr(fieldName,nchar(fieldName)-9+1,nchar(fieldName)) == "_Severity"){
          return(NULL)
        }
        if (tableName == "Accidents"){
          numUnique = length(unique(acc[, fieldName]))
        } else 
          if (tableName == "Vehicles"){
          numUnique = length(unique(veh[, fieldName]))
          #return(NULL)
        } else 
          if (tableName == "Casualties"){
          numUnique = length(unique(cas[, fieldName]))
        }  

        if (numUnique < 40){
          if (tableName == "Accidents"){
            df = acc[,c("Accident_Severity", fieldName)]
            df$Accident_Severity <- factor(df$Accident_Severity,
                                         levels= c(1,2,3),
                                         labels= c("Fatal", "Serious", "Slight"))
            severityField = "Accident_Severity"
          } else if (tableName == "Vehicles"){
            df = veh[,c( fieldName)]
            #df$Vehicle_Severity <- factor(df$Vehicle_Severity,
            #                               levels= c(1,2,3),
            #                               labels= c("Fatal", "Serious", "Slight"))
            severityField = ""
          } else if (tableName == "Casualties"){
            df = cas[,c("Casualty_Severity", fieldName)]
            df$Casualty_Severity <- factor(df$Casualty_Severity,
                                         levels= c(1,2,3),
                                         labels= c("Fatal", "Serious", "Slight"))
            severityField = "Casualty_Severity"
          }  
  
          # df_levels = lookups[lookups$field == fieldName, c("numCode","label")]
          # uniqueVals = unique(df[, fieldName])
          # df_levels = df_levels[df_levels$numCode %in% uniqueVals, ]
          # if (nrow(df_levels) > 0) {
          #   df[, 2] <- factor(df[,2],
          #                   levels= df_levels$numCode,
          #                   labels= df_levels$label)
          # }

          myTitle = paste(gsub("_", " ",  fieldName), "Indicator") 
          catPlot(df, fieldName, myTitle, sevField=severityField)
        } else { 
          NULL
        }
      } else if (input$investigate == 2) {
        NULL
      }
      
    }, height=400) 
  
    output$outHTML = renderUI({
      str1 = "This is line 1"
      str2 = "This is line 2"
      HTML(paste(str1,str2,sep="<br/>"))
    })    

#
# Missing Data
#

    
    output$miss_missing = renderDT({
      tempMissingFields = fieldDescriptions[fieldDescriptions$MissingCount > 0 & ! is.na(fieldDescriptions$MissingCount), c("Table", "FieldName","MissingCount", "MissingPercent")]
      tempMissingFields = tempMissingFields[order(-tempMissingFields$MissingCount),]
      
      choiceFields = tempMissingFields$FieldName
      updateSelectInput(session, inputId = "missing_field", choices = choiceFields)
      
      datatable(tempMissingFields, rownames = FALSE, 
                options = list(
                  dom = 'ftp',
                  pageLength = 5,
                  selection = 'single',
                  lengthMenu = c( 10, 15, 20, 50)
                )) # %>% formatPercentage('MissingPercent', 2) %>% formatCurrency('MissingCount', currency="", digits=0 )
    })
    

#
# Near Zero
#
    output$nz_outTable = renderUI({
      inTable = input$nz_table
      tagList(
        tags$h5(inTable)
      )
    })
    
    output$nz_outField = renderUI({
      inTable = input$nz_table
      inField = input$nz_field
      fieldDesc = fieldDescriptions[fieldDescriptions$FieldName == inField &
                                    fieldDescriptions$Table == inTable , 
                                    "Description"]
      initialSelection = fieldDescriptions[fieldDescriptions$FieldName == inField & 
                                           fieldDescriptions$Table == inTable , 
                                           "InitialSelection"]
      actions = fieldDescriptions[fieldDescriptions$FieldName == inField &
                                    fieldDescriptions$Table == inTable , 
                                  "Actions"]
      deleteField =  fieldDescriptions[fieldDescriptions$FieldName == inField &
                                         fieldDescriptions$Table == inTable , 
                                       "Delete"]
      selectedField =  fieldDescriptions[fieldDescriptions$FieldName == inField &
                                           fieldDescriptions$Table == inTable , 
                                         "Selected"]
      if (selectedField == "N") {
        selectedField = "No"
      } else {
        selectedField = "Yes"
      }
      outField = paste("<h5>", inField, "</h5>")
      
      outDesc = paste("<b>","Description:", "</b>", fieldDesc)
      outInitial = paste("<br /><b>","Initial Selection:", "</b>", initialSelection, "<b>","  Final Selection?:", "</b>", selectedField)
      HTML(paste(outField, outDesc, outInitial, "<br />"))
    })     
    
    output$nz_outDetailTable = renderTable({
      inTable = input$nz_table
      if (input$hideList == FALSE) {
        tempNZV = fieldDescriptions[is.na(fieldDescriptions$nzv) == FALSE 
                                  & fieldDescriptions$Table == inTable 
                                  & fieldDescriptions$nzv == TRUE,
                                  c("Table", "FieldName", "freqRatio", "percentUnique")]
        choiceFields = tempNZV$FieldName
        updateSelectInput(session, inputId = "nz_field", choices = choiceFields)
        colnames(tempNZV) = c("Table", "FieldName", "freqRatio", "%Unique")
        tempNZV
      } else {
        NULL
      }
    })
 
    output$nz_outMax = renderUI({

      inTable = input$nz_table
      inField = input$nz_field
      if (inTable == "Accidents") {
        tempNZVTable = table(acc[, inField])
        numUnique2 = length(tempNZVTable)
        maxCount = max(tempNZVTable)
        maxValue = names(tempNZVTable[tempNZVTable == maxCount])
      } else if (inTable == "Casualties") {
        tempNZVTable = table(cas[, inField])
        numUnique2 = length(tempNZVTable)
        maxCount = max(tempNZVTable)
        maxValue = names(tempNZVTable[tempNZVTable == maxCount])
      } else if (inTable == "Vehicles") {
        tempNZVTable = table(veh[, inField])
        numUnique2 = length(tempNZVTable)
        maxCount = max(tempNZVTable)
        maxValue = names(tempNZVTable[tempNZVTable == maxCount])
      }
      
     # numUnique2 = format(numUnique2, decimal.mark = ".", big.mark = ",", small.mark = ",")
      maxValue = format(maxValue, decimal.mark = ".", big.mark = ",", small.mark = ",")
      maxCount = format(maxCount, decimal.mark = ".", big.mark = ",", small.mark = ",")
      print(paste("numunique:",numUnique2))
      print(paste("maxValue:",maxValue))
      print(paste("maxCount:",maxCount))
      str1 = paste("<b>","Number of unique values:","</b>", numUnique2)
      str2 = paste("<b>","Max Value:", "</b>",maxValue, "<b>","  Num Records:", "</b>",maxCount)
      HTML(paste(str1,str2,sep="<BR/>"))
    }) 
    
    output$nz_outTopNZV = renderTable({
      inTable = input$nz_table
      inField = input$nz_field
      if (inTable == "Accidents") {
        tempNZVTable = table(acc[, inField])
      } else if (inTable == "Casualties"){
        tempNZVTable = table(cas[, inField])        
      } else if (inTable == "Vehicles"){
        tempNZVTable = table(veh[, inField]) 
      }
      
      maxReads = min(10, nrow(tempNZVTable))
      totalCount = sum(tempNZVTable)
      tempTopNZV = data.frame(tempNZVTable[order(-tempNZVTable)][1:maxReads])

      colnames(tempTopNZV) = c("TopValues","Count")
      if(inField %in% lookups$field){
        valuesInTable = unique(tempTopNZV$TopValues)
        tempTopNZV$Lookup = lookups[lookups$field == inField & lookups$numCode %in% valuesInTable, "label"]  
      } else {
        tempTopNZV$Lookup = "NA"
      }
      tempTopNZV = subset(tempTopNZV, select=c(TopValues, Lookup, Count))
      tempTopNZV$Percent = round(((tempTopNZV$Count*100)/totalCount),2)
      tempTopNZV$Count = format(tempTopNZV$Count, decimal.mark = ".", big.mark = ",", small.mark = ",")
      tempTopNZV
      
    })
    
    output$outDistPlot2 = renderPlot({
      print("Plotting 2")
      tableName = input$nz_table
      fieldName = input$nz_field 
      if (tableName == "Accidents"){
        numUnique = length(unique(acc[, fieldName]))
      } else if (tableName == "Vehicles"){
        numUnique = length(unique(veh[, fieldName]))
      } else if (tableName == "Casualties"){
        numUnique = length(unique(cas[, fieldName]))
      }  
        
      if (numUnique < 40){
        if (tableName == "Accidents"){
          df = acc[,c("Accident_Severity", fieldName)]
          df$Accident_Severity <- factor(df$Accident_Severity,
                                         levels= c(1,2,3),
                                         labels= c("Fatal", "Serious", "Slight"))
          severityField = "Accident_Severity"
        } else if (tableName == "Vehicles"){
          df = veh[,c( fieldName)]
          #df$Vehicle_Severity <- factor(df$Vehicle_Severity,
          #                               levels= c(1,2,3),
          #                               labels= c("Fatal", "Serious", "Slight"))
          severityField = ""
        } else if (tableName == "Casualties"){
          df = cas[,c("Casualty_Severity", fieldName)]
          df$Casualty_Severity <- factor(df$Casualty_Severity,
                                         levels= c(1,2,3),
                                         labels= c("Fatal", "Serious", "Slight"))
          severityField = "Casualty_Severity"
        }  
          
        # df_levels = lookups[lookups$field == fieldName, c("numCode","label")]
        # uniqueVals = unique(df[, fieldName])
        # df_levels = df_levels[df_levels$numCode %in% uniqueVals, ]
        # if (nrow(df_levels) > 0) {
        #   df[, 2] <- factor(df[,2],
        #                     levels= df_levels$numCode,
        #                     labels= df_levels$label)
        # }
        myTitle = paste(gsub("_", " ",  fieldName), "Indicator") 
        catPlot(df, fieldName, myTitle, sevField=severityField)
      } else { 
        NULL
      }

    }, height=340)

#
# Summary
#  
  output$summary_title = renderUI({
    tableName = input$summary_table
    tagList(
      tags$h4(tableName)
    )
    })
  
  output$summary_keep = renderTable({
    tableName = input$summary_table
    keepTable = fieldDescriptions[fieldDescriptions$Selected == "Y" & 
                                        fieldDescriptions$Table == tableName ,c("FieldName","Actions")]
    keepTable
  })
  
  output$summary_getridof = renderTable({
    tableName = input$summary_table
    getridofTable = fieldDescriptions[fieldDescriptions$Selected == "N" & 
                                    fieldDescriptions$Table == tableName ,c("FieldName","Actions")]
    getridofTable
  })
  
  output$summary_modify = renderTable({
    tableName = input$summary_table
    modifyTable = fieldDescriptions[fieldDescriptions$Selected == "A" & 
                                    fieldDescriptions$Table == tableName ,c("FieldName","Actions")]
    modifyTable
  })
  
  output$modifications_slide1 = renderUI({
    tagList(
      tags$h3("Modifications"),
      tags$ul(
        tags$li("Converted data to US format and created a Month number"),
        tags$li("Several variables had values that seemed overly complex"),
        tags$li("For example Weather Conditions included the following:"),
        tags$ul(        
          tags$li("Fine no high winds"),
          tags$li("Raining no high winds"), 
          tags$li("Snowing no high winds"),
          tags$li("Fine + high winds"),
          tags$li("Raining + high winds"),
          tags$li("Snowing + high winds"),
          tags$li("etc")
        ),
        tags$li("We simplified by creating new fields with Yes/No:"),
        tags$ul(        
          tags$li("Fine"),
          tags$li("Rain"), 
          tags$li("Wind"),
          tags$li("Snow"),
          tags$li("etc")
        ), 
        tags$li("Did this for:"),
        tags$ul(        
          tags$li("Weather Conditions"),
          tags$li("Road Surface Conditions"), 
          tags$li("Special Conditions on Site"),
          tags$li("Carriageway Hazards")
        )
      )
    )
  }) 
#  
# Feature Creation  
#  
  output$decisions_slide1 = renderUI({
    tagList(
      tags$h3("Additions and Creations"),
      tags$ul(
        tags$li("Looked at variables from other files to add predictive lift"),
        tags$li("Vehicles and Casualties were a many-to-1 relationship with accidents"),
        tags$li("Reviewed and brainstormed available fields"),
        tags$li("Different strategies to incorporate these fields"),
        tags$ul(        
          tags$li("Age of passengers is available - chose to indicate a child was present"),
          tags$li("Vehicles involved - was a motorcycle involved?"), 
          tags$li("Was a stationary vehicle invovled?"),
          tags$li("Was vehicle hit in rear?"),
          tags$li("Was there a particular hazard?") 
        ),
        tags$li("Indicator variables created, summarized ot accident level, merged into accident data frame")
      )
    )
  }) 
  
#
# Model Results
# 
  
  output$m_results_header = renderUI({
    
    HTML(paste("General text about the models"))
  })
  

  output$modelprep_slide1 = renderUI({
    tagList(
      tags$h3("Holdout, Training and Testing Data"),
      tags$ul(
        tags$li("We read that there was a change in reporting in 2016"),
        tags$li("Held out 2015 and 2016 data for final model to determine the predictive power"),
        tags$li("Remaining data was partitioned as:"),
        tags$ul(        
          tags$li("70% - training the models and tuning the parameters"),
          tags$li("30% - testing and comparing the models") 
        )
      )
    )
  })
  
  output$modeli_slide1 = renderUI({
    tagList(
      tags$h3("Generation of Multiple Models"),
      tags$ul(
        tags$li("Looked at 3 different model types"),
        tags$ul(
          tags$li("Random Forest"),
          tags$li("Penalized Multinomial Regression"),
          tags$li("One-vs-Rest Logistic Regression")),
          
        tags$li("First two are available in the caret package"),
        tags$li("Challenge with imbalanced response"),
        tags$li("Slight - 85%, Fatal - 2% ... Initial prediction -> Slight (85% Accuracy)"),
        tags$li("Downsampling provided more reasonable numbers, although with less overall accuracy") 

      )
    )
  })
  
  output$model1_slide1 = renderUI({
    tagList(
      tags$h3("Random Forest"),
      tags$ul(
        tags$li("Used caret package"),
        tags$li("Ran 5-fold cross-validation"),
        tags$li("mtry - +/-1 of square root of predictors (5-7)"),
        tags$li("Results:"),
        tags$ul(
          tags$li("Best - mtry = 6, splitrule = gini, min.node.size = 20")
        )

      )
    )
  })
  output$model2_slide1 = renderUI({
    tagList(
      tags$h3("Penalized Multinomial Regression"),
      tags$ul(
        tags$li("Used caret package"),
        tags$li("Used multinom package - fits multinomial log-linear models via neural networks"),
        tags$li("Ran 5-fold cross-validation"),
        tags$li("Default decay parameters"),
        tags$ul(
          tags$li("AARON _ DONT KNOW IF YOU REALLY WANT THESE"),
          tags$li("rang - 0.7"),
          tags$li("weight decay - 0"),    
          tags$li("max iterations - 100")
        ),
        tags$li("Results:"),
        tags$ul(
          tags$li("Do we want to summarize the results in the slide?")
        )
      )
    )
  })
  output$model3_slide1 = renderUI({
    if (input$model3 == 1) {
      tagList(
        tags$h3("One-vs-Rest Classification"),
        tags$ul(
          tags$li("Created 2 separate logistic models to mesh together for final model"),
          tags$li("Model 1 - predict fatal versus non-fatal"),
          tags$ul(
            tags$li("Same variables as used in previous models"),
            tags$li("Fitted model and ran prediction on training data"),
            tags$li("Used coord function of pROC library to optimize sensitivity"),
            tags$li("Sensitivity was optimized with probability cutoff of 0.987")
          ),
          tags$li("Model 2 - predict serious versus non-serious"),
          tags$ul(
            tags$li("Same variables as used in previous models"),
            tags$li("Fitted model and ran prediction on training data"),
            tags$li("Used coord function of pROC library to optimize sensitivity"),
            tags$li("Sensitivity was optimized with probability cutoff of 0.867")
          )
        )
      )
    }
    else
      if (input$model3 == 2) {  
        tagList(
          tags$h3("Merging the Two Models"),
          tags$li("Obtained probability vectors for each model run on training set"),
          tags$li("Identified as fatal if probability was less than optimized cutoff for fatal"),
          tags$li("Identified as serious if probability was less than optimized cutoff for serious"),
          tags$li("All others classified as slight"),
          tags$li("Running predictions through a confusion matrix gave final metrics for training data"),
          tags$li("Achieved accuracy of about 51%")
        )
      }
  })
 
  output$comparison_slide1 = renderUI({
    if (input$selection == 1){
      tagList(
        tags$h3("Model Comparison"),
        tags$ul(
          tags$li("Results in confusion matrix are percentages of overall number of observations"),
          tags$li("Being multinomial, difficul to choose a model"),
          tags$li("Looking at accuracy, the models are within 4%"),
          tags$li("However, we cannot look at accuracy alone"),       
          tags$li("Need to compare the categories of most interest - Fatal and Serious"),
          tags$li("Logistic and Random Forest were close for Fatal "),
          tags$li("Random Forest was best for Serious "),
        

          tags$li("Need to say we did/didn't see any difference for 2016")
        )
      )
    } else if (input$selection == 2){
      tagList(
        tags$h3("Model Selection"),
        tags$ul(
          tags$li("Selected the Random Forest model"),
          tags$li("Ran the model on the houldout data (2015 and 2016)"),
          tags$li("Returned an accuracy of 58.7%"),
          tags$li("Prediction on Fatal was worse than on training data"),       
          tags$li("Prediction on Serious was slightly better than on training data.s"),
          tags$li("Ran data on 2015 and 2016 due to change in way accidents reported"),
          tags$li("No significant difference was found in the predictions")
        )
      )     
      
    }
  })  
  
  
  # Confusion Matrix model 1  
  output$comparison_title1 = renderPrint({
    if (input$selection == 1){
      tagList(
        tags$h5("Random Forest on Test Data")
      )
    } else if (input$selection == 2){
      tagList(
        tags$h5("Random Forest on Test Data")
      )
    }

  })
    
  output$comparison_table1 = renderPrint({
    if (input$selection == 1){
      Fatal = c(Fatal=0.9, Serious=4.7, Slight=15.9)
      Serious = c(0.3, 5.7, 22.9)
      Slight = c(0.1,3.2,46.4)
      blank = ""
      Accuracy = c("53.0%","","")
      RandomForest = rbind(Fatal, Serious, Slight,Accuracy)
      noquote(format(RandomForest, 
                     digits=2, nsmall=0, big.mark=",", trim=TRUE, scientific = FALSE))
    } else if (input$selection == 2){
      Fatal = c(Fatal=0.5, Serious=2.5, Slight=7.6)
      Serious = c(0.4, 6.9, 24.7)
      Slight = c(0.3, 5.7, 51.3)
      blank = ""
      Accuracy = c("58.7%","","")
      RandomForest = rbind(Fatal, Serious, Slight,Accuracy)
      noquote(format(RandomForest, 
                     digits=2, nsmall=0, big.mark=",", trim=TRUE, scientific = FALSE))  
    }
    })  
  
  output$comparison_title2 = renderPrint({
    if (input$selection == 1){
      tagList(
        tags$h5("Penalized Multinomial Regression on Test Data")
      )
    } else if (input$selection == 2){
      invisible()
    }
  })
  
  # Confusion Matrix model 2   
  output$comparison_table2 = renderPrint({
    if (input$selection == 1){
      Fatal = c(Fatal=0.8, Serious=4.2, Slight=15.2)
      Serious = c(0.3, 4.8, 20.0)
      Slight = c(0.2,4.6,49.9)
      Accuracy = c("55.5%","","")
      Multinomial = rbind(Fatal, Serious, Slight, Accuracy)
      noquote(format(Multinomial, 
                   digits=2, nsmall=0, big.mark=",", trim=TRUE, scientific = FALSE))
    } else if (input$selection == 2){
      invisible()
    }
  })  
  
  
  
  output$comparison_title3 = renderPrint({
    if (input$selection == 1){
      tagList(
        tags$h5("Logistic on Test Data")
      )
    }
    else if (input$selection == 2){
      invisible()
    }
  })
  # Confusion Matrix model 3    
  output$comparison_table3 = renderPrint({
    if (input$selection == 1){
      Fatal = c(Fatal=0.9, Serious=6.0, Slight=20.9)
      Serious = c(0.2, 3.8, 17.5)
      Slight = c(0.2,3.9,46.8)
      Accuracy = c("51.5%","","")
      Regression = rbind(Fatal, Serious, Slight, Accuracy)
      noquote(format(Regression, 
                   digits=2, nsmall=0, big.mark=",", trim=TRUE, scientific = FALSE))
    } else    if (input$selection == 2){
      invisible()
    }
  })  
   
  output$line_report_PDF <- renderUI({
    PDFfile="stats19.pdf"
    tags$iframe(
      src=PDFfile,
      width="100%",
      height="800px")
  })
  

#
# Summary
#
  output$summary_slide1 = renderUI({
    tagList(
      tags$h3("Lessons Learned"),
      tags$ul(
        tags$li("Challenge with methods to predict multi class responses"),
        tags$li("How to pick the BEST model"),
        tags$li("Running large datasetson small machines")
      ),
      tags$h3("Other areas to investigate:"),
      tags$ul(        
        tags$li("Ensemble approach to see if three methods produce better results"),
        tags$li("Create a simulated business case"),
        tags$li("Bring in variables from the other datasets"),
        tags$li("Investigate the accidents from a location and/or GPS perspective")
      )
    )
  }) 
  
#
# Prediction
#

  v <- reactiveValues(clearPlot = TRUE)

  observeEvent(c(input$predict_numVehicles,
               input$predict_vehicles),{ 
                 print("Clearing prediction")
                 v$clearPlot = TRUE
               }, priority = 10)

  observeEvent(input$predict_go, {
    print("Button pressed")
    v$clearPlot <- FALSE
  }, priority = 10)

  output$predict_result = renderText({ 
    print("Predicting")
    if(v$clearPlot)
      return("")
    else
    {
      Sys.sleep(5)
      a = sample(1:3,1,replace=F)
      if (a == 1){
        pred = "Fatal"
      } 
      else if (a == 2) {
        pred = "Severe"
      }
      else if (a == 3) {
        pred = "Slight"
      }
      return(pred)
    }
  })
# End of Prediction
  
#
# Tools 
#
  #
  # R functions
  #
  output$outStr = renderPrint({
    if (input$ql_type == "str"){
      if (input$ql_table == "Accidents"){
        str(acc)
      }
      else if (input$ql_table == "Vehicles"){
        str(veh)
      }
      else if (input$ql_table == "Casualties"){
        str(cas)
      }
    }
    else if (input$ql_type == "head"){
      if (input$ql_table == "Accidents"){
        head(acc, 10)
      }
      else if (input$ql_table == "Vehicles"){
        head(veh, 10)
      }
      else if (input$ql_table == "Casualties"){
        head(cas, 10)
      }
    }
    
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

