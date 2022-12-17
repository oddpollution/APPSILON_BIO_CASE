#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(tidyverse)
library(tidyr)
library(readr)
library(readxl)
library(data.table)
library(ff)
library(plotly)
library(writexl)
library(knitr)
library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyAce)
library(rsconnect)
library(shinythemes)
library(DBI)
library(knitr)
library(kableExtra)
library(openssl)
library(DT)
library(stringr)
library(shinybusy)
library(openxlsx)

species_poland <- read.csv("species_poland.csv", header = T)
multimedia_poland <- read.csv("multimedia_poland.csv", header = T)

species_poland$eventTime <- as.numeric(gsub(pattern = ":",".", species_poland$eventTime))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #### MAP PLOT ####
  
  map_process <- eventReactive(input$submit, {
  
      dataset <- species_poland
    
      kingdom <- c(dataset$kingdom)
      family <- c(dataset$family)
      scientificName <- c(dataset$scientificName)
      vernacularName <- c(dataset$vernacularName)
      locality <- c(dataset$locality)
      
      dataset <- dataset %>% 
      dplyr::filter(kingdom %in% if(is.null(input$KING)) 
                                    {kingdom} else 
                                      {input$KING},
                    family %in% if(is.null(input$FAM)) 
                                    {family} else 
                                      {input$FAM},
                    scientificName %in% if(is.null(input$SCI)) 
                                    {scientificName} else 
                                      {input$SCI},
                    vernacularName %in% if(is.null(input$VERN)) 
                                    {vernacularName} else 
                                      {input$VERN},
                    locality %in% if(is.null(input$LOC)) 
                                    {locality} else 
                                      {input$LOC},
                    if(is.na(input$DATES[1]) & is.na(input$DATES[2])) 
                      {eventDate <= Sys.Date()} else
                    {if (is.na(input$DATES[1])) {eventDate <= input$DATES[2]} else
                    {if(is.na(input$DATES[2])) {eventDate >= input$DATES[1]} else
                    {eventDate >= input$DATES[1] & eventDate <= input$DATES[2]}}},
                    eventTime >= input$TIME[1] & eventTime <= input$TIME[2])
    
    g <- list(
      scope = 'europe',
      showland = TRUE,
      landcolor = toRGB("LightGreen"),
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("grey"),
      showlakes = TRUE,
      
      lakecolor = toRGB("Blue"),
      showocean = TRUE,
      
      oceancolor = toRGB("LightBlue"),
      showsubunits = TRUE,
      showcountries = TRUE,
      
      showrivers = TRUE,
      rivercolor = toRGB("Blue"),
      
      resolution = 50,
      projection = list(
        type = 'mercator',
        rotation = list(lon = -100)
      ),
      lonaxis = list(
        showgrid = TRUE,
        gridwidth = 1,
        range = c(14,26),
        dtick = 5
      ),
      lataxis = list(
        showgrid = TRUE,
        gridwidth = 1,
        range = c(49,56),
        dtick = 5
      )
    )
    
    fig <- plot_geo(dataset, lat = ~latitudeDecimal, lon = ~longitudeDecimal)
    fig <- fig %>% add_markers(
      text = ~paste(paste("Kingdom:", kingdom), paste("Family:", family), 
                    paste("Sci Name:", scientificName),paste("Vern Name:", vernacularName), 
                    paste("Life Stage:", lifeStage), paste("Sex:", sex), 
                    paste("Individual:", individualCount),
                    paste("Observations per Day:", timesObserved),sep = "<br />"),
      color = ~timesObserved, symbol = I("square"), size = I(2), hoverinfo = "text"
    )
    # fig <- fig %>% colorbar(title = "Observed Species<br />per day")
    fig <- fig %>% layout(geo = g,
                          showlegend = FALSE,
                          height = 700)

    return(fig)
  
  })

  output$maps <- renderPlotly({map_process()})
  
  #### SPECIFICS DATA TABLE ####
  
  specifics_process <- eventReactive(input$submit, {
    
    dataset1 <- multimedia_poland[,-(1:2)] 
    
    kingdom <- c(dataset1$kingdom)
    family <- c(dataset1$family)
    scientificName <- c(dataset1$scientificName)
    vernacularName <- c(dataset1$vernacularName)
    
    dataset1 <- dataset1 %>% 
      dplyr::filter(kingdom %in% if(is.null(input$KING)) 
      {kingdom} else 
      {input$KING},
      family %in% if(is.null(input$FAM)) 
      {family} else 
      {input$FAM},
      scientificName %in% if(is.null(input$SCI)) 
      {scientificName} else 
      {input$SCI},
      vernacularName %in% if(is.null(input$VERN)) 
      {vernacularName} else 
      {input$VERN})
    
    validate(
      need(!is.na(dataset1), "Sorry, no media avaible for the selected species")
    )
    
    dataset1$acessURI <- paste('<img src=\'',dataset1$accessURI,' height=\'200\'</img>')
    
    return(dataset1)
    
    })
  
  output$table <- DT::renderDataTable({DT::datatable({data <- specifics_process()},
                                                      escape = FALSE,
                                                      rownames = FALSE,
                                                      options = list(columnDefs = list(list(className = 'dt-center', targets = 0))),
                                                      selection = 'single')})
  
})
