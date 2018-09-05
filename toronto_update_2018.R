#Shiny app

library(readr)
library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(leaflet)
library(ggmap)
library(jsonlite)
library(plyr)
library(ggrepel)
library(rgdal)
library(sp)

#Toronto DSO data
SMIS_Daily_Occupancy_2017 <- read_csv("/Users/Emirry/data-science/Final-project/SMIS_Daily_Occupancy_2017.csv")
toronto <- SMIS_Daily_Occupancy_2017
json_file <- "/Users/Emirry/Desktop/extractssha.json"
toronto_update <- fromJSON(paste(readLines(json_file), collapse=""))

#Drop-in shelters data
shape <- readOGR(dsn = path.expand("/Users/Emirry/shelters/"), layer = "shelters")

#Neighbourhood data
neighbourhood <- readOGR(dsn = path.expand("/Users/Emirry/neighbourhood/"), layer = "neighbourhood")
neighbourhood_density <- read_excel("data-science/Final-project/SHU_DensityByNeighbourhood.xlsx")

#Toronto data
toronto_update <- toronto_update[c("OCCUPANCY_DATE",
                                   "ORGANIZATION_NAME",
                                   "SHELTER_NAME",
                                   "SHELTER_ADDRESS",
                                   "SHELTER_CITY",
                                   "SHELTER_PROVINCE",
                                   "SHELTER_POSTAL_CODE",
                                   "FACILITY_NAME",
                                   "PROGRAM_NAME",
                                   "SECTOR",
                                   "OCCUPANCY",
                                   "CAPACITY")]

#Convert dates to Y-M-D format
toronto$OCCUPANCY_DATE <- dmy(toronto$OCCUPANCY_DATE)

toronto_update$OCCUPANCY_DATE <- as.Date(toronto_update$OCCUPANCY_DATE,
                                         format = "%Y-%m-%d")

toronto <- rbind.fill(toronto, toronto_update)




#For Birchwood
toronto[toronto$SHELTER_NAME == str_subset(toronto$SHELTER_NAME, "Birch"),][7] <- "M1N 1S4"

#For Downsview
toronto[toronto$SHELTER_NAME == str_subset(toronto$SHELTER_NAME, "Down"),][7] <- "M3L 1M4"

#For Fort York
toronto[toronto$SHELTER_NAME == str_subset(toronto$SHELTER_NAME, "Fort"),][7] <- "M5V 3W3"

#For S2H Assessment and Referral Centre
toronto[toronto$SHELTER_NAME == str_subset(toronto$SHELTER_NAME, "S2H "),][7] <- "M5C 1K6"

#Fix Robertson postal code error
toronto[toronto$SHELTER_NAME == str_subset(toronto$SHELTER_NAME, "Rob"),][7] <- "M5A 2R9"

#Fix Eva's Satellite postal code error
toronto$SHELTER_POSTAL_CODE[toronto$SHELTER_POSTAL_CODE == "M2N OE3"] <- "M2N 0E3"


## Generate longitude and latitude coordinates
longlat <- geocode(unique(toronto$SHELTER_POSTAL_CODE))
longlat <- data.frame(longlat)
longlat$Postal <- unique(toronto$SHELTER_POSTAL_CODE)

#Fill NA's with long and lat coordinates
while (sum(is.na(longlat$lon)) != 0) { 
  postal <- longlat[is.na(longlat$lon),][3]
  longlat[is.na(longlat$lon),][1:2] <- geocode(postal[,1])
}


#Add longitude and latitude coordinates to Toronto dataframe
toronto$longitude <- longlat$lon[match(toronto$SHELTER_POSTAL_CODE, longlat$Postal)]
toronto$latitude <- longlat$lat[match(toronto$SHELTER_POSTAL_CODE, longlat$Postal)]



##SEGMENTING DATA
#Total shelter capacity over time
totcapacity.18 <- toronto %>%
  group_by(OCCUPANCY_DATE) %>%
  summarise(total = sum(CAPACITY, na.rm = TRUE))

#Total shelter occupancy over time
totoccupancy.18 <- toronto %>% 
  group_by(OCCUPANCY_DATE) %>%
  summarise(total = sum(OCCUPANCY, na.rm = TRUE))

#Occupancy over time broken down by sector
type.occupancy.18 <- toronto %>%
  group_by(SECTOR, OCCUPANCY_DATE) %>%
  summarise(total = sum(OCCUPANCY))


#Graph data
total_occupancy.18 <- ggplot(data=totoccupancy.18, aes(x=OCCUPANCY_DATE)) + 
  geom_area(aes(y=total), alpha =0.5, colour = "purple", fill = "purple") 


#Total occupancy by sector type graph
occupancy_type.18 <- ggplot(data=type.occupancy.18, aes(x=OCCUPANCY_DATE)) + 
  geom_area(aes(y=total, fill = SECTOR, colour = SECTOR), alpha =0.5) + guides(colour = FALSE)


#total capacity and occupancy numbers for December 31, 2017
lastdate.18 <- totcapacity.18[365,]
lastdate.o.18 <- totoccupancy.18[365,]

#Total occupancy vs. total capacity
total_occupancy.18 + 
  geom_line(data=totcapacity.18, aes(x=OCCUPANCY_DATE, y=total), colour = "red", alpha = 0.8) +
  ylab("Total occupants") + xlab("") + ggtitle("Total occupancy vs. total capacity in Toronto (2017-18)") +  
  theme_linedraw() +
  theme(
    legend.title=element_blank(),
    plot.title = element_text(size = 15, face="bold")
  )

#Total occupancy segmented by shelter sector vs. total capacity
occupancy_type.18 + 
  geom_line(data=totcapacity.18, aes(x=OCCUPANCY_DATE, y=total, fill = "Capacity"), colour = "red", alpha = 1) +
  ylab("Total occupants") + xlab("") + 
  ggtitle("Total occupancy by shelter sector vs. total capacity in Toronto (2017-18)") +
  theme_linedraw() +
  theme(
    legend.title=element_blank(),
    plot.title = element_text(size = 15, face="bold")
  )

#### FAMILY OCCUPANCY #####
## Compare family capacity with family shelter occupancy over time
family.information <- toronto %>%
  group_by(OCCUPANCY_DATE) %>%
  filter(SECTOR == "Families") %>%
  summarise(total.o = sum(OCCUPANCY, na.rm = TRUE), total.c = sum(CAPACITY, na.rm = TRUE))


family.shelter <- toronto %>%
  group_by(OCCUPANCY_DATE, SHELTER_NAME) %>%
  filter(SECTOR == "Families") %>%
  summarise(total.o = sum(OCCUPANCY, na.rm = TRUE), total.c = sum(CAPACITY, na.rm = TRUE))

#The total family occupants for every shelter that has sector family 

#Family occupancy graph
family.o.graph <- ggplot(data=family.information, aes(x=OCCUPANCY_DATE)) + 
  geom_area(aes(y=total.o), colour = "blue", fill = "blue", alpha =0.5)

costi_label <- subset(family.shelter, total.o == max(total.o))#data for labelling Costi
sojourn_label <- filter(family.shelter, OCCUPANCY_DATE == "2018-07-01",
                        total.o == 265)

sojourn_spike <- family.shelter$OCCUPANCY_DATE[3704]

fam_shelter <- ggplot(data=family.shelter, aes(x=OCCUPANCY_DATE, y=total.o)) + 
  geom_line(aes(colour = SHELTER_NAME)) +
  ylab("Total occupants") + xlab("") + ggtitle("Occupancy for the family sector of individual shelters (2017-18)") +
  theme_linedraw() +
  geom_label_repel(data=costi_label, aes(y= total.o + 30, label = SHELTER_NAME),
                   colour = "black", size = 3.5) +
  geom_label_repel(data=sojourn_label, aes(y= total.o + 50, label = SHELTER_NAME),
                   colour = "black", size = 3.5) +
  theme(
    legend.title=element_blank(),
    plot.title = element_text(size = 15, face="bold")
  ) +
  geom_vline(xintercept=sojourn_spike, colour = "black", alpha = 0.5)




##LEAFLET MAPPING STUFF ###
summary_toronto <- toronto %>%
  group_by(SHELTER_POSTAL_CODE, SHELTER_NAME, SECTOR) %>%
  summarise(OCCUPANCY = mean(OCCUPANCY),
            CAPACITY = mean(CAPACITY))

#Create IDs for sectors
sector_id <- data.frame("sector" = unique(summary_toronto$SECTOR), 
                        "id"= c(1, 2, 3, 4, 5))

#Add sector ID to data
summary_toronto$id <- sector_id$id[match(summary_toronto$SECTOR, sector_id$sector)]

#Add long/lat coordinates to data
summary_toronto$longitude <- longlat$lon[match(summary_toronto$SHELTER_POSTAL_CODE, longlat$Postal)]
summary_toronto$latitude <- longlat$lat[match(summary_toronto$SHELTER_POSTAL_CODE, longlat$Postal)]

#Define colours
colorFactors = colorFactor(c('red', 'orange', 'purple', 'blue', 'pink'),
                           domain=summary_toronto$id)

toronto_map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=summary_toronto$longitude, 
             lat=summary_toronto$latitude,
             popup = summary_toronto$SHELTER_NAME,
             stroke=F,
             fillOpacity=0.75,
             radius = 6,
             color = colorFactors(summary_toronto$id),
             group = "Sector")

#Drop-in locations
leaflet(shape) %>%
  addTiles() %>%
  addCircleMarkers(lng=shape$coords.x1,
                   lat=shape$coords.x2,
                   stroke=F,
                   fillOpacity=0.75,
                   radius=6,
                   color="red")

##Toronto neighbourhood data
neighbourhood_data <- neighbourhood@data %>%
  separate(AREA_NAME, c("NAME"), sep="\\(")

neighbourhood_data$Neighbourhood <- as.numeric(neighbourhood_data$AREA_S_CD)


#neighbourhood density
neighbourhood_density$Neighbourhood <- as.numeric(
  neighbourhood_density$Neighbourhood)

shu <- merge(neighbourhood_data, neighbourhood_density, by = "Neighbourhood")
neighbourhood@data <- shu

#continuous palette function

pal = colorNumeric(
  palette = "Blues",
  domain = neighbourhood$Units
)

leaflet(neighbourhood) %>%
  addProviderTiles("Stamen.TonerLite",
                   group = "Toner Lite") %>%
  addPolygons(stroke=F,
              smoothFactor=1,
              fillOpacity=0.75,
              color = ~pal(neighbourhood$Units)) %>%
  addCircleMarkers(lng=summary_toronto$longitude, 
                   lat=summary_toronto$latitude,
                   popup = summary_toronto$SHELTER_NAME,
                   stroke=F,
                   fillOpacity=0.75,
                   radius = 6,
                   color = colorFactors(summary_toronto$id),
                   group = "Sector")


