#Title: Cleaning Toronto data

library(lubridate)
library(stringr)
library(ggplot2)
library(ggmap)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(leaflet)
library(plotly)


SMIS_Daily_Occupancy_2017 <- read_csv("data-science/Final project/SMIS_Daily_Occupancy_2017.csv")
toronto <- SMIS_Daily_Occupancy_2017
View(toronto)

#Convert dates to Y-M-D format
toronto$OCCUPANCY_DATE <- dmy(toronto$OCCUPANCY_DATE)

#Add missing postal codes
shelters <- toronto[is.na(toronto$SHELTER_POSTAL_CODE),]
table(shelters$SHELTER_NAME) #generates the names of the shelters missing Postal codes


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

toronto$month <- month(toronto$OCCUPANCY_DATE)

write.csv(toronto, "toronto_data.csv", row.names =F)

### DATA VISUALIZATION ###

#Geographical plot of average occupancy in 2017
sheltermeans <- toronto %>%
  group_by(SHELTER_NAME) %>%
  summarise(mean = mean(OCCUPANCY, na.rm = TRUE)) 

toronto$shelter_means <- sheltermeans$mean[match(toronto$SHELTER_NAME, sheltermeans$SHELTER_NAME)]

to.map <- get_map(location='Toronto', zoom= 13, maptype='roadmap')

ggmap(to.map) + 
  geom_point(data = toronto, alpha = 0.08, aes(x = longitude, y = latitude, size = OCCUPANCY, colour = ORGANIZATION_NAME))


##SEGMENTING DATA
#Total shelter capacity over time
totcapacity <- toronto %>%
  group_by(OCCUPANCY_DATE) %>%
  summarise(total = sum(CAPACITY, na.rm = TRUE))

#Total shelter occupancy over time
totoccupancy <- toronto %>% 
  group_by(OCCUPANCY_DATE) %>%
  summarise(total = sum(OCCUPANCY, na.rm = TRUE))

#Occupancy over time broken down by sector
type.occupancy <- toronto %>%
  group_by(SECTOR, OCCUPANCY_DATE) %>%
  summarise(total = sum(OCCUPANCY))

##PLOTTING DATA
#Total occupancy graph
total_occupancy <- ggplot(data=totoccupancy, aes(x=OCCUPANCY_DATE)) + 
  geom_area(aes(y=total), alpha =0.5, colour = "purple", fill = "purple") 

#Total occupancy by sector type graph
occupancy_type <- ggplot(data=type.occupancy, aes(x=OCCUPANCY_DATE)) + 
  geom_area(aes(y=total, fill = SECTOR, colour = SECTOR), alpha =0.5) + guides(colour = FALSE)


#total capacity and occupancy numbers for December 31, 2017
lastdate <- totcapacity[365,]
lastdate.o <- totoccupancy[365,]

#Total occupancy vs. total capacity
total_occupancy + 
  geom_line(data=totcapacity, aes(x=OCCUPANCY_DATE, y=total), colour = "red", alpha = 0.8) +
  geom_text(data=lastdate, aes(label = total, y = total + 100), size = 3, colour = "red") +
  geom_text(data=lastdate.o, aes(label = total, y = total + 100), size = 3, colour = "purple") +
  ylab("Total occupants") + xlab("") + ggtitle("Total occupancy vs. total capacity in Toronto (2017)") +  
  theme_linedraw() +
  theme(
    legend.title=element_blank(),
    plot.title = element_text(size = 15, face="bold")
  )


#Total occupancy segmented by shelter sector vs. total capacity
occupancy_type + 
  geom_line(data=totcapacity, aes(x=OCCUPANCY_DATE, y=total, fill = "Capacity"), colour = "red", alpha = 1) +
  geom_text(data=lastdate, aes(label = total, y = total + 100), size = 3, colour = "red") +
  geom_text(data=lastdate.o, aes(label = total, y = total + 100), size = 3, colour = "orange") +
  ylab("Total occupants") + xlab("") + 
  ggtitle("Total occupancy by shelter sector vs. total capacity in Toronto (2017)") +
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

#The total family occupants for every shelter that has sector family 

#Family occupancy graph
family.o.graph <- ggplot(data=family.information, aes(x=OCCUPANCY_DATE)) + 
  geom_area(aes(y=total.o), colour = "blue", fill = "blue", alpha =0.5)

costi_label <- subset(family.shelter, total == max(total)) #data for labelling Costi

family.o.graph + 
  geom_line(aes(x=OCCUPANCY_DATE, y=total.c), colour = "red") +
  geom_text(data=family.c[365,], aes(label = total, y = total + 50), size = 3, colour = "red") +
  geom_text(data=family.o[365,], aes(label = total, y = total + 40), size = 3, colour = "blue") +
  ylab("Total occupants") + xlab("") + ggtitle("Family sector occupancy vs. family sector capacity in Toronto (2017)")

fam_shelter <- ggplot(data=family.shelter, aes(x=OCCUPANCY_DATE, y=total)) + 
  geom_line(aes(colour = SHELTER_NAME)) +
  ylab("Total occupants") + xlab("") + ggtitle("Occupancy for the family sector of individual shelters (2017)") +
  theme_linedraw() +
  geom_label_repel(data=costi_label, aes(y= total, label = SHELTER_NAME),colour = "black", size = 3.5) +
  theme(
    legend.title=element_blank(),
    plot.title = element_text(size = 15, face="bold")
  )

## Look at COSTI reception centre specifically
costi <- toronto %>%
  filter(SHELTER_NAME == "COSTI Reception Centre") %>%
  group_by(OCCUPANCY_DATE) %>%
  summarise(total.o = sum(OCCUPANCY, na.rm = TRUE), total.c = sum(CAPACITY, na.rm = TRUE))

costi_names <- data.frame(names = c(676, 771), date = costi[365,1]) #Numbers for Dec 31, 2017
costi_spike <- costi$OCCUPANCY_DATE[83]
costi_tweet <- costi$OCCUPANCY_DATE[28]
costi_dates <- data.frame(costi_spike, costi_tweet)

#Plotting trends in the COSTI centre
costi_plot <- ggplot(costi, aes(x=OCCUPANCY_DATE)) + 
  geom_line(aes(y=total.o), colour = "purple") + #line plot for occupancy
  geom_line(aes(y=total.c), colour = "red") + #line plot for capacity
  geom_vline(xintercept=costi_spike, colour = "red", alpha = 0.5) +  #date intercepts
  geom_vline(xintercept=costi_tweet, colour = "black", alpha = 0.5) +
  geom_text(data=costi_names, aes(y=names + 20, label = names)) +
  geom_label_repel(data=costi_dates, aes(x = costi_dates[1], y=600, label = "March 24")) + #label the dates
  geom_label_repel(data=costi_dates, aes(x = costi_dates[2], y=600, label = "January 28")) +
  ylab("Total occupants") + xlab("") + ggtitle("Occupancy vs. capacity for families at the COSTI Reception Centre") +
  theme_linedraw() +
  theme(
    legend.title=element_blank(),
    plot.title = element_text(size = 15, face="bold")
  )

# Plotting occupancy and capacity together
costi_total <- total_occupancy + 
  geom_area(data=costi, aes(x=OCCUPANCY_DATE, y=total.o, fill = "Refugees")) +
  ylab("Total occupants") + xlab("") + ggtitle("Proportion of occupants that are refugees") +
  theme_linedraw() +
  theme(
    legend.title=element_blank(),
    plot.title = element_text(size = 15, face="bold")
  )






