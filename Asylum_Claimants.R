#Asylum claims
#Author: Emily Xing
#Date: 24-06-2017

library(readxl)
library(ggplot2)
library(ggmap)
library(ggthemes)
library(tidyverse)
library(lubridate)
library(zoo)
library(ggrepel)
library(wesanderson)

IRCC_M_AC_0007_E <- read_excel("data-science/Final project/Asylum seekers/IRCC_M_AC_0007_E.xls")
asylum <- IRCC_M_AC_0007_E
View(asylum)

#Drop Q totals
asylum <- asylum[c(-19, -36, -42, -43)]
asylum <- asylum[-(seq(6,38,4))]

#Fill in missing years
asylum[2,4:14] <- 2016
asylum[2,16:26] <- 2017
asylum[2, 28:30] <- 2018

#Extract months
Month <- asylum[4, 3:30] %>%
  gather(key = Var, value = Month)

#Extract years
year <- asylum[2, 3:30] %>%
  gather(key = Var, value = Year)

#Extract totals
total <- asylum[233, 3:30] %>%
  gather(key = Var, value = Total)

#Extract Haiti
haiti <- asylum[13,]
haiti <- haiti[-2] %>%
  gather(key = Var, value = Total)
haiti <- haiti[-1,]


#Data frame containing the totals for each month from 2016-2018
asylum_clean <- merge(year, Month, by="Var") %>%
  merge(total, by = "Var") %>%
  merge(haiti, by = "Var")

asylum_clean <- asylum_clean[,2:5]

asylum_clean <- asylum_clean %>%
  unite(Date, Year:Month, sep="-", remove = FALSE)

asylum_clean$Date <- as.Date(as.yearmon(sprintf("%s", asylum_clean$Date), "%Y-%b"))
asylum_clean$Total <- as.numeric(asylum_clean$Total.x)
asylum_clean$Haiti <- as.numeric(asylum_clean$Total.y)

tweet <- as.Date("2017-01-28", format = "%Y-%m-%d")
tweetdate <- data.frame(tweet, 5000)
increase <- asylum_clean[10,]
date <- as.Date("2018-04-01", format = "%Y-%m-%d")
date.haiti <- data.frame(date, 300)

ggplot(asylum_clean, aes(x=Date, y=Total)) + 
  geom_area(aes(y=Total, fill = Year, colour = Year), alpha = 0.6) +
  geom_line(aes(y=Haiti)) +
  geom_vline(xintercept=tweet, colour = "black", alpha = 0.5) +
  geom_point(data=increase, aes(x=Date, y=Total)) +
  geom_label_repel(data=tweetdate, aes(x=tweet, y=5000, label = "Justin Trudeau's tweet"), size = 3.5) +
  geom_label_repel(data=increase, aes(x=Date, y=Total, label = Month), size = 3.5) +
  geom_label_repel(data=date.haiti, aes(x=date, y=200), label = "Haiti", size = 3.5) +
  ylab("Number of asylum claims") + xlab("") + ggtitle("Number of asylum claims submitted every month (2016-18)") +
  theme_linedraw() +
  scale_color_manual(values=wes_palette(n=4, name="Darjeeling1")) +
  scale_fill_manual(values=wes_palette(n=4, name="Darjeeling1")) +
  theme(
    legend.title = element_blank(),
    plot.title =element_text(size = 15, face="bold")
  )


