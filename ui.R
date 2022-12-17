#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

gc()

shinyUI(fluidPage(
  theme = shinytheme("journal"),
  add_busy_spinner(spin = "fading-circle"),
  # Application title
  titlePanel("BIO REPORT - FAUNA & FLORA MONITOR"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      p("SETTINGS PANEL"),
      
      pickerInput("KING", "Kingdom:", 
                  choices = c(unique(species_poland$kingdom)), 
                  selected =  c(unique(species_poland$kingdom)), 
                  options = list(`actions-box` = TRUE, 
                                 `selected-text-format` = "count > 1",
                                 `count-selected-text` = "{0}/{1} Selected",
                                 `live-search` = TRUE), 
                  multiple = TRUE),
      
      pickerInput("FAM", "Family:", 
                  choices = c(unique(species_poland$family)), 
                  selected = c(unique(species_poland$family)), 
                  options = list(`actions-box` = TRUE, 
                                 `selected-text-format` = "count > 1",
                                 `count-selected-text` = "{0}/{1} Selected",
                                 `live-search` = TRUE), 
                  multiple = TRUE),
      
      pickerInput("SCI", "Scientific Name:", 
                  choices = c(unique(species_poland$scientificName)), 
                  selected = c(unique(species_poland$scientificName)), 
                  options = list(`actions-box` = TRUE, 
                                 `selected-text-format` = "count > 1",
                                 `count-selected-text` = "{0}/{1} Selected",
                                 `live-search` = TRUE), 
                  multiple = TRUE),
      
      pickerInput("VERN", "Vernacular Name:", 
                  choices = c(unique(species_poland$vernacularName)), 
                  selected = c(unique(species_poland$vernacularName)), 
                  options = list(`actions-box` = TRUE, 
                                 `selected-text-format` = "count > 1",
                                 `count-selected-text` = "{0}/{1} Selected",
                                 `live-search` = TRUE), 
                  multiple = TRUE),
      
      pickerInput("LOC", "Locality:", 
                  choices = (c(unique(species_poland$locality))), 
                  selected = c(unique(species_poland$locality)), 
                  options = list(`actions-box` = TRUE, 
                                 `selected-text-format` = "count > 1",
                                 `count-selected-text` = "{0}/{1} Selected",
                                 `live-search` = TRUE),
                  multiple = TRUE),
    
      dateRangeInput("DATES", label = h3("Date range"), 
                     start = min(species_poland$eventDate), 
                     end = max(species_poland$eventDate)),
    
      sliderInput("TIME", label = h3("Time Range"), 
                  min = 0,
                  max = 24,
                  value = c(0, 24)),
    
      actionButton("submit","Update")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Geo Tracking", plotlyOutput("maps",width = "auto")),
        tabPanel("Specifics", dataTableOutput("table")),
        )
      )
    )
  )
)
