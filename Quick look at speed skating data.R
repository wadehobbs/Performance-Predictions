library(tidyverse)
library(lubridate)

#Data can be found here
#https://www.kaggle.com/seniorwx/shorttrack
#Look at one athlete from speed skating - want to find a time series of races that are equaly spaced

speed_skate_athlete_confortola <- filter(speed_skate_500m, Name == "Yuri CONFORTOLA")
#Fix date columns
form.dates<-ymd(paste(speed_skate_athlete_confortola$Year, speed_skate_athlete_confortola$Month, speed_skate_athlete_confortola$Day, sep="-"))
speed_skate_athlete_confortola$Date <- form.dates
speed_skate_athlete_confortola <- select(speed_skate_athlete_confortola, -.data$Year, -.data$Month, -.data$Day)
#move date to first column
speed_skate_athlete_confortola <- select(speed_skate_athlete_confortola, Date, everything())
#Order by date
speed_skate_athlete_confortola <- arrange(speed_skate_athlete_confortola, Date)

#Definitely not an equally spaced time series. Cant even get a time for each quarter of the year. 
#Wont be able to do time series forecasting on this data. Can still explore a little
#Overall view of data coloured by season
ggplot(speed_skate_athlete_confortola, aes(x = Date, y = Time, colour = Season)) +
        geom_point()

#Obviously 2 outliers (probably crashed out) - want to remove from the graph these to better see results
ggplot(filter(speed_skate_athlete_confortola, Time < 50), aes(x = Date, y = Time, colour = Season)) +
        geom_point()

#No clear pattern with date, times are randomly dispersed through seasons.
#Will come back to this when I think of something interesting to do with it.