#
# Movies The Sequel
# Richard Gower
# 
#

#devtools::install_github("dgrtwo/gganimate")
library(shiny)
library(shinythemes)
library(scales)
library(ggplot2)
library(lubridate)
library(caret)

#library(gganimate)


# Define UI 
ui = navbarPage(theme = shinytheme("superhero"),
               "UK Traffic Accidents",

#  
  tabPanel("Intro",
           fluidRow( )
          ),
  tabPanel("Quick Look",
         sidebarLayout(
           sidebarPanel(width = 2,
                        selectInput("ql_table",
                                    label = "Table",
                                    choices = c("Accidents" = "acc",
                                                "Casualties" = "cas",
                                                "Vehicles" = "veh"
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
  ),
                        
  tabPanel("Pair Wise",
           sidebarLayout(
             sidebarPanel(width = 2),
              mainPanel(width = 10,

                     htmlOutput("pair_outHTML")
                     
                      )
            )
        ), 
  tabPanel("Missing",
         sidebarLayout(
           sidebarPanel(width = 2,
                        selectInput("missing_field",
                                    label = "Field",
                                    choices = c("")
                        )
                        ),
           mainPanel(width = 10,
                     fluidRow(column(12,
                                     fluidRow(column(4,
                                                     htmlOutput("miss_outRowsAccidents")
                                                     ),
                                              column(4,
                                                     htmlOutput("miss_outRowsCasualties")
                                                     ),
                                              column(4,
                                                     htmlOutput("miss_outRowsVehicles")
                                                     )
                                               ),
                                      fluidRow(column(5,
                                                      tableOutput("miss_missing")
                                                      ),
                                               column(7,
                                                      htmlOutput("miss_analysis")
                                                      )
                                                )
                                        
                                      )
                            )
                     )
         )
    ), 
    tabPanel("Field Analysis",
             sidebarLayout(
               sidebarPanel(width=3,
                            selectInput("da_table",
                                        label = "Table",
                                        choices = c("Accidents" = "accidents",
                                                    "Casualties" = "casualties",
                                                    "Vehicles" = "vehicles"
                                        ),
                                        selected = "1"
                            ),
                            selectInput("da_field",
                                        label = "Field",
                                        choices = c("Accident_Severity",
                                                    "Day_of_Week"
                                        ),
                                        selected = "1"
                            )
                            
                            ),
               mainPanel(width = 9,            
                 fluidRow( column(12,
                                  fluidRow(column(6,
                                    htmlOutput("outField"),
                                    htmlOutput("outDescription"),
                                    textOutput("outTotal"),
                                    textOutput("outMissing"),
                                    textOutput("outUnique")
                                  ),
                                  column(6,
                                         htmlOutput("outActions")
                                         )
                                  )
                 
                 
                          ,
                          fluidRow(
                            column(6,
                                   tableOutput("outDistribution")
                                   
                            ),
                            column(width = 6,
                                   plotOutput("outDistPlot")
                            )
                          )
                     )
                 )
               )
         ) ),
        tabPanel("Near Zero",
         sidebarLayout(
           sidebarPanel(width = 2,
                        selectInput("nz_table",
                                    label = "Table",
                                    choices = c("Accidents" = "accidents",
                                                "Casualties" = "casualties",
                                                "Vehicles" = "vehicles"
                                    ),
                                    selected = "1"
                        ),
                        selectInput("nz_field",
                                    label = "Field",
                                    choices = c("")
                        )
                        
           ),
           mainPanel(width = 9,
                     tableOutput("nz_outTable"),
                     htmlOutput("nz_outField"),
                     htmlOutput("nz_outMax"),

                     tableOutput("nz_outTopNZV")
                     
           )
         )
      ),
      tabPanel("Actions",
         sidebarLayout(
           sidebarPanel(width = 2),
                      
           mainPanel(width = 9,
                     htmlOutput("action_action"),
                     htmlOutput("action_keep"),
                     htmlOutput("action_getRidOf"),
                     htmlOutput("action_create")
                     
                      )
          )
        )

    )




# Define server logic required to draw a histogram
server = function(input, output, session) {
  
  catPlot<-function(df, var, lbl=''){
    plotData<-data.frame(byvar = levels(factor(df[[var]])),   # Get list of level names
                         freq = summary(factor(df[[var]])),   # Calculate frequency for each level
                         slight = tapply(df[['Accident_Severity']] == 'Slight', df[[var]], mean) / mean(df[['Accident_Severity']] == 'Slight'),
                         serious = tapply(df[['Accident_Severity']] == 'Serious', df[[var]], mean) / mean(df[['Accident_Severity']] == 'Serious'),
                         fatal = tapply(df[['Accident_Severity']] == 'Fatal', df[[var]], mean) / mean(df[['Accident_Severity']] == 'Fatal'))
    par(mar=c(7,5,2,5), cex=1.0) # bottom, left, top, right
    
    #---------------------------------------------------------------------------- 
    # Create bar chart of relative frequencies.  Factor levels are printed on the
    # horizontal axis
    #  mp = The midpoint of each bar (Used to position points of line plot)
    #----------------------------------------------------------------------------
    mp<-with(plotData,barplot(height=freq/sum(freq), col='yellow',
                              xlim=c(0,1.2*nrow(plotData)+.2), 
                              ylim = c(0,1), las=2,
                              names.arg=byvar, 
                              axes = F))
    
    #--- create right axis ---
    axis(side=4)
    mtext(side=4, line=3, '% of total accidents', cex=1)
    
    #---------------------------------------------------------------------------- 
    # Overlay line plot
    #----------------------------------------------------------------------------
    par(new=T)
    plot(x=c(0,max(mp)+.5), y=c(0,max(plotData[,3:5])), type='n', 
         axes=F, xlim = c(0, 1.2*nrow(plotData)+.2), 
         xlab=NA, ylab=NA, main=lbl)
    lines(x=mp,y=plotData$slight, type='b', col='#e41a1c', pch=16)
    lines(x=mp,y=plotData$serious, type='b', col='#377eb8', pch=16)
    lines(x=mp,y=plotData$fatal, type='b', col='black', pch=16)
    
    #--- create left axis --- 
    axis(side=2)
    mtext(side=2, line=3, 'Casualty Rate Relative to Mean', cex=1)
    
    #----------------------------------------------------------------------------
    # Create legend
    #----------------------------------------------------------------------------  
    par(xpd=T, new=T)
    plot(x=c(0,1), y=c(0,1), xlim=c(0,1), ylim=c(0,1), type='n', axes=F, xlab=NA, ylab=NA)
    legend(x=-.18, y=-.1, legend=c('slight',"serious",'fatal'),
           text.col=c("#e41a1c","#377eb8",'black'), lty=1, 
           col=c("#e41a1c","#377eb8",'black'), cex=1,
           xpd=T, bty='o', pch=16)
  }
  
  
  
    filepath = "C://Users/rchrd/OneDrive/Data Analytics/Masters/6.0 Data Analytics Applications/Project/"
    load(file="datafiles\\accidents.raw.rda")
    acc = accidents.raw
    rm(accidents.raw)
    load(file="datafiles\\vehicles.raw.rda")
    veh = vehicles.raw
    rm(vehicles.raw)
    load(file="datafiles\\casualties.raw.rda")
    cas = casualties.raw
    rm(casualties.raw)    
    load(file="datafiles\\fieldDescriptions.rda")
    load(file="datafiles\\lookups.rda")
    
    # richards.theme = theme_dark() +
    #                  theme(axis.text.x = element_text(color="white",size=16,face="plain"),
    #                  axis.text.y = element_text(color="white",size=16,angle=0,face="plain"),  
    #                  axis.title.x = element_text(color="white",size=16,angle=0,hjust=0.5,vjust=1,face="plain"),
    #                  axis.title.y = element_text(color="white",size=16,angle=0,hjust=0.5,vjust=0.6,face="plain"),
    #                  plot.title = element_text(size = 20, colour="white"),
    #                  plot.background = element_rect(fill = "black"), 
    #                  panel.background = element_rect(fill = "black", color = "black", size = 0.5, linetype = "solid"),
    #                  panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "grey40"), 
    #                  panel.grid.minor = element_line(size = 0.25, linetype = 'solid', color = "grey40"))
    
    profit_colors = c("Lost money" = "red", "Made money" = "lawngreen")
    series_colors = c("1" = "red", "2" = "turquoise3", "3" = "royalblue2" , "4" = "purple3")

    choiceFields = colnames(acc)
    updateSelectInput(session, inputId = "da_field", choices = choiceFields)
    

    
        
#
# Quick View
#
    output$outStr = renderPrint({
      if (input$ql_type == "str"){
        if (input$ql_table == "acc"){
          str(acc)
        }
        else if (input$ql_table == "veh"){
          str(veh)
        }
        else if (input$ql_table == "cas"){
          str(cas)
        }
      }
      else if (input$ql_type == "head"){
        if (input$ql_table == "acc"){
          head(acc, 10)
        }
        else if (input$ql_table == "veh"){
          head(veh, 10)
        }
        else if (input$ql_table == "cas"){
          head(cas, 10)
        }
      }
      
    })
     
    
    
    
    
#
# Analysis
#     
    output$outField = renderUI({
      fieldName = input$da_field
      HTML(paste(fieldName))
    })
    
    output$outDescription = renderUI({
      fieldName = input$da_field
      fieldDesc = fieldDescriptions[fieldDescriptions$Table == "accidents" & fieldDescriptions$FieldName == fieldName,"Explanation"]
      HTML(paste(fieldDesc))
    })
    
    output$outDecisions = renderUI({
      fieldName = input$da_field
      decisions = fieldDescriptions[fieldDescriptions$Table == "accidents" & fieldDescriptions$FieldName == fieldName,"Actions"]
      HTML(paste(decisions))
    })
    output$outUnique = renderText({
      #inField = input$da_field 
      #tempLookups = lookups[lookups$field == inField,]
      #tempLookups
      inField = input$da_field     
      a = length(unique(acc[,inField]))
      paste("Number of unique values:", a)
    })
    
    output$outTotal = renderText({
      totalValues = nrow(acc)
      paste("Total Observations:", totalValues)
    })  
    
    output$outMissing = renderText({
      print(input$da_field)
      totalMissing = sum(is.na(acc[,input$da_field]))
      paste("Missing Values:", totalMissing)
    }) 
    
    output$outDistribution = renderTable({
      inField = input$da_field     
      a = length(unique(acc[,inField]))
      if (a < 200){
        tempTable = table(acc[,inField])
        tempDF = data.frame(tempTable)

        colnames(tempDF) = c("Value", "Frequency")
        tempDF$Percent = round((tempDF$Frequency*100)/sum(tempDF$Frequency),2)
        tempLookups = lookups[lookups$field == inField,c("numCode", "label")]
        colnames(tempLookups) = c("Value", "Lookup")
        tempDF = join(tempDF, tempLookups, by = "Value")
        tempDF = subset(tempDF, select=c(Value, Lookup, Frequency, Percent))
        rm(tempTable)
        rm(tempLookups)
        tempDF
      }
      else
      { 
        SampleValues = acc[1:10,inField]
        tempDF = data.frame(SampleValues)
        tempDF
      }
    })    
    
    output$outDistPlot = renderPlot({
      inField = input$da_field     
      a = length(unique(acc[,inField]))
      if (a < 200){
        df = acc[,c("Accident_Severity", inField)]
        df$Accident_Severity <- factor(df$Accident_Severity,
                                       levels= c(1,2,3),
                                       labels= c("Fatal", "Serious", "Slight"))
        
        df_levels = lookups[lookups$field==inField, c("numCode","label")]
        uniqueVals = unique(df[,inField])
        df_levels = df_levels[df_levels$numCode %in% uniqueVals, ]
        if (nrow(df_levels) > 0) {
          df[, 2] <- factor(df[,2],
                            levels= df_levels$numCode,
                            labels= df_levels$label)
        }
        myTitle = paste(gsub("_", " ", inField), "Indicator") 
        catPlot(df,inField,myTitle)
      }
      else
      { NULL}
      
    }, height=400) 
    
    
    output$outHTML = renderUI({
      str1 = "This is line 1"
      str2 = "This is line 2"
      HTML(paste(str1,str2,sep="<br/>"))
    })    

#
# Missing Data
#
    output$miss_missing = renderTable({
      tempMissingFields = fieldDescriptions[fieldDescriptions$MissingCount > 0 & ! is.na(fieldDescriptions$MissingCount), c("Table", "FieldName","MissingCount", "MissingPercent")]
      tempMissingFields = tempMissingFields[order(-tempMissingFields$MissingCount),]
      
      choiceFields = tempMissingFields$FieldName
      updateSelectInput(session, inputId = "missing_field", choices = choiceFields)
      
      tempMissingFields
    })
    
    output$miss_outRowsAccidents = renderUI({
      totalRows = nrow(acc) 
      tempMissingRows = which(acc[,c(7:32)] == -1, arr.ind=TRUE)
       
      totalRowsWithMissingValue = unique(tempMissingRows[1])
      line1 = paste("Total rows:",totalRows)
      line2 = paste("Rows missing 1+ fields:",totalRowsWithMissingValue)
      line3 = paste("Percent of rows missing:", round((100*totalRowsWithMissingValue)/totalRows),"%")
        
      HTML(paste("Accident",line1,line2,line3, sep="<br/>"))
    })       
 
    output$miss_outRowsCasualties = renderUI({
      totalRows = nrow(cas) 
      tempMissingRows = which(cas[,c(4:15)] == -1, arr.ind=TRUE)
      totalRowsWithMissingValue = unique(tempMissingRows[1])
      line1 = paste("Total rows:",totalRows)
      line2 = paste("Rows missing 1+ fields:",totalRowsWithMissingValue)
      line3 = paste("Percent of rows missing:", round((100*totalRowsWithMissingValue)/totalRows),"%")

      HTML(paste("Casualties", line1,line2,line3, sep="<br/>"))
    })  
    
    output$miss_outRowsVehicles = renderUI({
      totalRows = nrow(veh) 
      tempMissingRows = which(veh[,c(3:22)] == -1, arr.ind=TRUE)
      
      totalRowsWithMissingValue = unique(tempMissingRows[1])
      line1 = paste("Total rows:",totalRows)
      line2 = paste("Rows missing 1+ field2:",totalRowsWithMissingValue)
      line3 = paste("Percent of rows missing:", round((100*totalRowsWithMissingValue)/totalRows),"%")
        
      HTML(paste("<b>Vehicles</b>", line1,line2,line3, sep="<br/>"))
    })  
#
# Near Zero
#
  
    
    output$nz_outTable = renderTable({
      inTable = input$nz_table  
      tempNZV = fieldDescriptions[is.na(fieldDescriptions$nzv) == FALSE & fieldDescriptions$Table == inTable & fieldDescriptions$nzv == TRUE,c("Table", "FieldName", "Sample", "nzv", "freqRatio", "percentUnique")]
      choiceFields = tempNZV$FieldName
      updateSelectInput(session, inputId = "nz_field", choices = choiceFields)
      tempNZV
    })    
    
    output$nz_outField = renderUI({
      inField = input$nz_field
      description = fieldDescriptions[fieldDescriptions$FieldName == inField, "Description"]
      HTML(paste(inField, description, sep="<br/>"))
    })    
    
    output$nz_outMax = renderUI({
      inTable = input$nz_table
      inField = input$nz_field
      if (inTable == "accidents") {
        numUnique = length(unique(acc[,inField]))
        tempNZVTable = table(acc[, inField])
        maxCount = max(tempNZVTable)
        maxValue = names(tempNZVTable[tempNZVTable == maxCount])
      }
      if (inTable == "casualties") {
        numUnique = length(unique(cas[,inField]))
        tempNZVTable = table(cas[, inField])
        maxCount = max(tempNZVTable)
        maxValue = names(tempNZVTable[tempNZVTable == maxCount])
      }
      if (inTable == "vehicless") {
        numUnique = length(unique(veh[,inField]))
        tempNZVTable = table(veh[, inField])
        maxCount = max(tempNZVTable)
        maxValue = names(tempNZVTable[tempNZVTable == maxCount])
      }
      
      str1 = paste("Number of unique values: ", numUnique)
      str2 = paste("Max Value:", maxValue, "Dup count:", maxCount)
      HTML(paste(str1,str2,sep="<BR/>"))
    }) 
    
    output$nz_outTopNZV = renderTable({
      inTable = input$nz_table
      inField = input$nz_field
      if (inTable == "accidents") {
        tempNZVTable = table(acc[, inField])
      }
      else if (inTable == "casualties"){
        tempNZVTable = table(cas[, inField])        
      }
      else if (inTable == "vehicles"){
        tempNZVTable = table(veh[, inField]) 
      }
      
      maxReads = min(10, nrow(tempNZVTable))
      totalCount = sum(tempNZVTable)
      tempTopNZV = data.frame(tempNZVTable[order(-tempNZVTable)][1:maxReads])

      colnames(tempTopNZV) = c("TopValues","Frequency")
      if(inField %in% lookups$field){
        valuesInTable = unique(tempTopNZV$TopValues)
        tempTopNZV$Lookup = lookups[lookups$field == inField & lookups$numCode %in% valuesInTable, "label"]  
      }
      else {
        tempTopNZV$Lookup = NA
      }
      tempTopNZV = subset(tempTopNZV, select=c(TopValues, Lookup, Frequency))

      tempTopNZV$Percent = round(((tempTopNZV$Frequency*100)/totalCount),2)
      tempTopNZV
      
    })

#
# Actions
#  
  output$action_action = renderUI({
      
      HTML(paste("Main next steps:"))
    })
  output$action_keep = renderUI({
    
    HTML(paste("Keep the following fields"))
  })
  output$action_getRidOf = renderUI({
    
    HTML(paste("Ignore the following fields"))
  })
  
  output$action_create = renderUI({
    
    HTML(paste("Create the following fields"))
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

