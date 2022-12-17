###################################### PACKAGES DOWNLOAD ######################################

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

########################################### GET WD ###########################################

dir <- "C:/Users/danie/OneDrive/Documentos/R/APSILON CASE"
dir1 <- "C:/Users/danie/OneDrive/Documentos/R/APSILON CASE/SPECIES_DASH"

######################################### DATA IMPORT #########################################

### OCCURRENCES

datatable1 <- read_excel(paste0(dir,"/","OCCURRENCE_POLAND.xlsx"))

datatable1$chk <- 1
timesObserved <- aggregate(chk ~ scientificName + locality + eventDate,data = datatable1, sum)

timesObserved <- dplyr::rename(timesObserved, timesObserved = chk)

datatable1 <- merge(datatable1,timesObserved, by = c("scientificName","locality", "eventDate"), all.x = TRUE)

species1 <- datatable1 %>% dplyr::select("id",
                                           "basisOfRecord",
                                           "timesObserved",
                                           "scientificName",
                                           "taxonRank",
                                           "kingdom",
                                           "family",
                                           "vernacularName",
                                           "individualCount",
                                           "lifeStage",
                                           "sex",
                                           "longitudeDecimal",
                                           "latitudeDecimal",
                                           "continent",
                                           "country",
                                           "locality",
                                           "eventDate",
                                           "eventTime",
                                           "references",
                                           "rightsHolder",
                                           "license",
                                           "modified")

species1$eventDate <- as.Date.numeric(species1$eventDate,origin = "1899-12-30")
species1$modified <- as.Date.numeric(species1$modified,origin = "1899-12-30")

species1$longitudeDecimal <- as.double(species1$longitudeDecimal)
species1$latitudeDecimal <- as.double(species1$latitudeDecimal)

species1$eventTime[is.na(datatable1$eventTime)] <- 0



### MULTIMEDIA

datatable <- read.csv(paste0(dir,"/","multimedia.csv"), header = T)

datatable <- datatable %>% filter(license != "All rights reserved") %>% dplyr::group_by(CoreId)

datatable <- datatable[!duplicated(datatable[c('CoreId')]), ]

datatable <- dplyr::rename(datatable,id = CoreId)

datatable1_compl <- datatable1 %>% dplyr::select("id",
                                         "scientificName",
                                         "kingdom",
                                         "family",
                                         "vernacularName")

species_images <- merge(datatable,datatable1_compl,by = "id", all.x = TRUE)

species_images <- species_images %>% dplyr::filter(!is.na(scientificName))

######################################### DATA EXPORT #########################################

### MAP SET

write_xlsx(x = species1,path = paste0(dir1,"/","species_poland.xlsx"))

write.csv(species1,paste0(dir1,"/","species_poland.csv"))

### PIC SET

write_xlsx(x = species_images,path = paste0(dir1,"/","multimedia_poland.xlsx"))

write.csv(species_images, paste0(dir1,"/","multimedia_poland.csv"))

########################################### PLOT ###########################################

### option 1

# g <- list(
#   scope = 'poland',
#   projection = list(type = 'conic equal area'),
#   showland = TRUE,
#   landcolor = toRGB("gray95"),
#   subunitcolor = toRGB("gray85"),
#   countrycolor = toRGB("gray85"),
#   countrywidth = 1,
#   subunitwidth = 1
# )

g <- list(
  scope = 'europe',
  showland = TRUE,
  landcolor = toRGB("black"),
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("grey"),
  showlakes = TRUE,
  lakecolor = toRGB("blue"),
  showsubunits = TRUE,
  showcountries = TRUE,
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

fig <- plot_geo(species1, lat = ~latitudeDecimal, lon = ~longitudeDecimal)
fig <- fig %>% add_markers(
  text = ~paste(paste("Kingdom:", kingdom), paste("Family:", family), 
                paste("Sci Name:", scientificName),paste("Vern Name:", vernacularName), 
                paste("Life Stage:", lifeStage), paste("Sex:", sex), 
                paste("Individual:", individualCount),
                paste("Observations per Day:", timesObserved),sep = "<br />"),
  color = ~timesObserved, symbol = I("square"), size = I(2), hoverinfo = "text"
)
fig <- fig %>% colorbar(title = "Observed Species<br />per day")
fig <- fig %>% layout(
  title = 'Poland - Data over fauna & flora<br />(Hover for details)', geo = g)

fig

### option 2
# 
# fig <- species1 
# fig <- fig %>%
#   plot_ly(
#     lat = ~latitudeDecimal,
#     lon = ~longitudeDecimal,
#     marker = list(color = "fuchsia"),
#     type = 'scattermapbox',
#     hovertext = species1[,"scientificName"]) 
# fig <- fig %>%
#   layout(
#     mapbox = list(
#       style = 'dark',
#       zoom =2.5,
#       center = list(lon = 21, lat = 52))) 
# fig <- fig %>%
#   config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))
# 
# fig

######################################### DATA TABLE #########################################

species_images$pic <- sprintf(species_images$accessURI)

kable(species_images)

View(species_images)


######################################### DISPOSAL #########################################

# teste <- datatable1 %>% dplyr::filter(Identifier == "https://observation.org/observation/100010495/")

# datatable1 <- read.csv(paste0(dir,"/","occurence.csv"), header = T,nrows = 1000000)
# 
# datatable <- datatable1 %>% dplyr::filter(country == "Poland")
# 
# names <- c(colnames(datatable))

# i <- 0

# while (nrow(datatable1) > 0) {
#   
# i <- i + 1000000  
#   
#   datatable1 <- read.csv(paste0(dir,"/","occurence.csv"), header = F,nrows = 1000000,skip = i)
#   
#   colnames(datatable1) <- names
#   
#   datatable1 <- datatable1 %>% dplyr::filter(country == "Poland")
#   
#   datatable <- rbind(datatable,datatable1)
#   
#   }


# x <- read.csv.ffdf(file=paste0(dir,"/","occurence.csv"), header=TRUE, VERBOSE=TRUE, 
#                    first.rows=10000, next.rows=50000, colClasses=NA)
# x <- read.csv.ffdf(file=paste0(dir,"/","occurence.csv"), header=TRUE, VERBOSE=TRUE,
#                    nrows=100000, first.rows=10000, next.rows=10000, colClasses=NA)
# 
# x <- as.data.frame(x)
# 
# print(str(x))
# teste <- as.data.frame(unique(x$country))
# teste <- teste %>% dplyr::filter(unique(x$country) == "Poland")


# us_cities = read.csv("https://raw.githubusercontent.com/plotly/datasets/master/us-cities-top-1k.csv")
# df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')
# quakes = read.csv('https://raw.githubusercontent.com/plotly/datasets/master/earthquakes-23k.csv')
