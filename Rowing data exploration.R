#Exploring large rowing data set
rowing_world_championships <- read_csv("~/Dropbox/Personal/Jobs/Performance Predictions/rowing_world_championships.csv")

library(tidyverse)
library(DataExplorer)
library(summarytools)
library(lubridate)
library(reshape2)
library(ggrepel)

#Couple of cool ways of seeing the data
view(dfSummary(rowing_world_championships))     #Quick summary shown in html (webpage)
create_report(rowing_world_championships)       #Can be slow with large data set - didnt work

#Explore
#Want a reference id for each row (each row represents a run ie one athletes run in a race)
rowing_world_championships$row_id <- c(1:nrow(rowing_world_championships))
#Has to be a factor to act as a reference when the data is transformed (with melt)
rowing_world_championships$row_id <- as.factor(rowing_world_championships$row_id)
#Isolate one event to explore
#Chose the men's single scull as it has the most entries
mss <- filter(rowing_world_championships, event_category == "Men's Single Sculls")
mss$race_date <- dmy(mss$race_date)
mss <- arrange(mss, race_date)

#Remove useless variables - note spelling mistake in 'event_cateogry_abbreviation'
mss <- select(mss, -c(Year, lane_sl, event_cateogry_abbreviation, event_num, race_number, coxswain_birthday:third_name))
#Melt data sample to plot race progress - dont like this as it turns date into one of the variables
data_melt <- melt(mss)
#Isolate 1 race
race_1 <- filter(data_melt, race_id == 'ROM012901' & championship_name_description == 'Slovenia 28 Aug - 4 Sept 2011""')
#Isolate the race splits
race_1_splits <- filter(race_1, variable == 'split_1_time' | variable == 'split_2_time' | variable == 'split_3_time' | variable == 'split_4_time')
#Plot Race 1 progression. Bit boring
ggplot(race_1_splits, aes(x = variable, y = value, group = bow_name, colour = bow_name)) +
    geom_line() #+ 
    #theme(legend.position="none")
#Look at one athlete over time
synek <- filter(data_melt, bow_name == 'SYNEK Ondrej')
synek <- filter(synek,variable == 'split_1_time' | variable == 'split_2_time' | variable == 'split_3_time' | variable == 'split_4_time')

#Plot 1 athlete's progress over time - ie overlay all split progressions through all races in set
ggplot(synek, aes(x = variable, y = value, group = row_id, colour = row_id)) +
        geom_line()
#Cool plot to show grouping of times based on round type (heat, quarter, semi or final)
ggplot(synek, aes(x = variable, y = value, group = row_id, colour = round_type)) +
        geom_line()
#Interesting that the lines are so straight. Only a very slight kink at split 3. 
#So to plot the race through checkpoints, need to create a run id (called row_id) then melt the data, then isolate the variable of interest
#In this case it was splits, plotting the speed or stroke data would work too but more difficult to choose all the variables. 
#Could use a call like 'starts_with' or something, then order in size to viz the speed at each 50m interval or stroke rate
#Could viz stroke rate on y and speed on x. 
#Ultimately I want to predict split time from the other variables, so i can manipulate stroke rate to see how it effects the split time 
#Could start with average speed and stroke rate to predict split 4 and build from there. 
#What other predictors would be useful? round_type (maybe a test to see if it is different), date? maybe time of year effects times. 
#Even just plotting average times per month or something could be interesting. 

#Ultimately build a shiny app that allows you to change stroke rate and speed (and date, round, anything else?)to see how it effects end result

#Get a new variable called month
mss$month <- month(mss$race_date)
mss$month <- as.factor(mss$month)
data_melt <- melt(mss)
synek <- filter(data_melt, bow_name == 'SYNEK Ondrej')
synek <- filter(synek,variable == 'split_1_time' | variable == 'split_2_time' | variable == 'split_3_time' | variable == 'split_4_time')
ggplot(synek, aes(x = variable, y = value, group = row_id, colour = month)) +
        geom_line()
#Cool plot that shows there was a range of times across months (only 3 months in the set) but the majority of good times came in sept
#while most of the slower times were in august. There was only 4 races in october so doesnt show up much. Although these are world champs itmes 
#So presumably its peak  performance each race and round type is probably more telling.

mss$year <- year(mss$race_date)
mss$year <- as.factor(mss$year)
#Confirms there are 16 races from each year, 2011, 2013, 2014, 2015, 2017. So looking at date is not a worthwhile thing to do. 
data_melt <- melt(mss)
synek <- filter(data_melt, bow_name == 'SYNEK Ondrej')
synek <- filter(synek,variable == 'split_1_time' | variable == 'split_2_time' | variable == 'split_3_time' | variable == 'split_4_time')
ggplot(synek, aes(x = variable, y = value, group = row_id, colour = year)) +
        geom_line()
#This makes for a more interesting date based analysis - how times change over the years. Also just focus in on the final time to see changes clearer
ggplot(filter(synek, variable == 'split_4_time'), aes(x = variable, y = value, group = row_id, colour = year, shape = round_type)) +
        geom_jitter(width = 0.02, size = 5)
#Not quite as clear as id hope but shows finals are quicker - maybe too many splits (year and round type)
#From year - 2014 had the slowest and fastest times, slowest was quarter, fastest was final. Semi was also very quick that year 
#Overall this is a good approach to zoom in on a single athlete in a single race but the dataset is much richer than this. 
#Think of how to expand this type of exploration with the larger dataset. will depend on how melt performs. 


#### Larger scale exploration of race results (splits) ####
#Will recycle some of the above code applied to all races and athletes
#Start again by looking at the one race type
rowing_world_championships$row_id <- c(1:nrow(rowing_world_championships))
rowing_world_championships$row_id <- as.factor(rowing_world_championships$row_id)
mss <- filter(rowing_world_championships, event_category == "Men's Single Sculls")
mss$race_date <- dmy(mss$race_date)
mss <- arrange(mss, race_date) #Found data is pretty inconsequential outside of year so will only consider year
mss$year <- year(mss$race_date)
mss$year <- as.factor(mss$year)
mss <- select(mss, -c(Year, lane_sl, event_cateogry_abbreviation, event_num, race_number, coxswain_birthday:third_name))
data_melt <- melt(mss)
melted_mss_splits <- filter(data_melt,variable == 'split_1_time' | variable == 'split_2_time' | variable == 'split_3_time' | variable == 'split_4_time')
#plot
ggplot(melted_mss_splits, aes(x = variable, y = value, group = row_id, colour = year)) +
        geom_line()
#check races per year
table(melted_mss_splits$year)
#Most races were in2015 but in plot are under the 2017 races so cant be seen. 2014 had the fewest but by far the largest spread
#2014 looks to be the fastest but also close to the slowest year - large spread. 
ggplot(filter(melted_mss_splits, variable == 'split_4_time'), aes(x = variable, y = value, group = row_id, colour = year)) +
        geom_jitter(width = 0.1, size = 5, alpha = 0.5)
#This plot gives a good sense of the spread of results, 2015 had very little spread, 2013 looked slower overall, 2015 and 17 had large spread.

#Lets look at speed through the race
melted_mss_speed <- data_melt %>%
        filter(str_detect(variable, 'speed'))
#Set the variable col to the speed distance only. ie remove 'speed_' from string
melted_mss_speed$variable <- parse_number(melted_mss_speed$variable) #parse_number is a great function for getting the number out of a string
#order based on run (row_id) and speed
melted_mss_speed <- arrange(melted_mss_speed, row_id, variable)
#Now data is ordered by row_id so one run after another and in smallest to largest speed distance ie start of race to end. 
#plot all races looking at speed across the race grouped by row_id
ggplot(melted_mss_speed, aes(x = variable, y = value, group = row_id, colour = year)) +
        geom_line()
#319 rows contained missing data - will look at this
#seems in some runs the speed was very low after 50m, and some runs it stayed very low - possible data collection issue? 
#speeds are between 3 and 6, with mean sitting at approx 4.25. Could disregard any runs with < 3 speed after 250m
ggplot(filter(melted_mss_speed, variable >= 250 & value > 3), aes(x = variable, y = value, group = row_id, colour = year)) +
        geom_line()
#Can see where the 'wrong' runs are comming in. Problem with viz is the 2017 values blocking everything else. 
#fit a basic lm
ggplot(filter(melted_mss_speed, variable >= 250 & value > 3), aes(x = variable, y = value, group = year, colour = year)) +
        geom_jitter(alpha = 0.2) +
        geom_smooth(method = loess, size = 2) +
        geom_text_repel(data = . %>% filter(x == max(x)), aes(x = x, y = y, label = group))
#Shows the average speed in the local area for each year's line. 
#Seems 2013 start, middled and ended slower
#2014 started fasted, middled high but faded towards the end
#2015 started 2nd fasted, maintained and finished strong. 
#This is all based on average speed across years. Could look at average speed for different countries or per athlete across all years

