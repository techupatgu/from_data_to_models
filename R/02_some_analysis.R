library(tidyverse)
library(lubridate)
library(reshape2)

data <- read.csv("sample_1k.csv")
predictions <- read.csv("predictions.csv")

data <- cbind(data,predictions[,3:6])

data$monthyear <- floor_date(as.Date(data$StartDate),"month")

month_year <- data %>% group_by(monthyear) %>%
  summarise(voting = mean(voting,na.rm=T),
            woman = mean(grepl("Woman",face),na.rm=T),
            white = 1-mean(grepl("Non-White",face),na.rm=T),
            positive = mean(grepl("Positive",stance),na.rm=T),
            negative = mean(grepl("Negative",stance),na.rm=T),)

month_year <- melt(month_year,id.vars="monthyear")

ggplot(month_year,aes(monthyear,value))+
  geom_point()+
  facet_wrap(~variable)+
  theme_bw()+
  geom_smooth()