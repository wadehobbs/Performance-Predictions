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
ggplot(filter(melted_mss_speed, variable >= 250 & value > 3), aes(x = variable, y = value, group = team, colour = team)) +
        geom_jitter(alpha = 0.2) +
        geom_smooth(method = loess, size = 2) +
        geom_text_repel(data = . %>% filter(variable == max(variable)), aes(x = variable, y = value, label = team))
#Shows the average speed in the local area for each year's line. 
#Seems 2013 start, middled and ended slower
#2014 started fasted, middled high but faded towards the end
#2015 started 2nd fasted, maintained and finished strong. 
#This is all based on average speed across years. Could look at average speed for different countries or per athlete across all years
#Plot shows mean speed profiles per country with labels- labels overlap but cant get geom_text_repel to work
ggplot(filter(melted_mss_speed, variable >= 250 & value > 3), aes(x = variable, y = value, group = team, colour = team, label = team)) +
        geom_smooth(method = loess, size = 2) +
        geom_label(data = group_by(melted_mss_speed,team) %>%
                           do(augment(loess(value~variable, .))) %>%
                           filter(variable == max(variable)),
                           aes(variable, .fitted), nudge_x = 100)

#Time to chuck it all into a model
#Tricky part will be setting variables - speed over the race is an important variable but in the data it is 40 different columns
#When data is melted we lose the split times, ultimately what we want to predict
#Want a linear regression style model that takes variables speed, stroke rate, and athlete name and allows us to fit the data to split times - 
#Then holding all variables constant, look at how one variable affects split times ie increasing stroke rate during the first 500m
melted_mss_stroke <- data_melt %>%
        filter(str_detect(variable, 'strokes'))

#Create one data set with both speed and stroke rate 
melted_mss_stroke$variable <- parse_number(melted_mss_stroke$variable) 
melted_mss_stroke <- arrange(melted_mss_stroke, row_id, variable)
row_pred_data <- melted_mss_speed
colnames(row_pred_data)[22] = 'distance'
colnames(row_pred_data)[23] = 'speed'
row_pred_data$stroke <- melted_mss_stroke$value
###Accidently did this twice###
#To so this i need to model all the data for this race
mss_model_data <- data.frame(cbind(melted_mss_speed, stroke = melted_mss_stroke$value))
#Change col names
mss_model_data <- rename(mss_model_data, distance = 23, speed = 24)
rm(mss_model_data)


#Try a different tact - map the values
melted_mss_splits <- arrange(melted_mss_splits, row_id)
melted_mss_splits$distance <- rep(c(500,1000,1500,2000), length(unique(melted_mss_splits$row_id)))



#Just work on one run
run <- row_pred_data[1:40,]
splits <- melted_mss_splits[1:4,]
run$split <- splits$value[match(run$distance, splits$distance)]
#model
mod <- lm(split ~ distance + speed + stroke, run)
#Playing around with predictors
predict(mod, newdata = transform(run, stroke = 40))
predict(mod, newdata = transform(run, stroke = 40, speed = 4.9))
#fill gaps in splits
new_data <- data.frame(distance = seq(0, 2000, 50), speed = run$speed, stroke = run$stroke)
pred <- predict(mod, newdata = new_data)
run$split3 <- pred
#Plot - very straight line, no surprise, but pretty much what a race looks like. 
ggplot(run, aes(x = distance, y = split)) +
        geom_line()
#See how speed changes over the course of a race
ggplot(run, aes(x = distance, y = speed)) +
        geom_line()
#See how stroke rate changes
ggplot(run, aes(x = distance, y = stroke)) +
        geom_line()
#Stroke and speed reach lowest level at around 1000m. stroke steadly increases from there, speed does too - 
#though scale is less senstive. 

#IDEA: set up a shiny app that selects an athlete, builds model based on their races, and then you can manipulate
#speed or stroke. Can plot more than 1 athlete and see how stroke and speed affect different athlets or all athletes
#So for each athlete, when selected (test filter) it builds 1 model on their races - 4 races per year for top athletes. 
#Should also generate plots that show speed and stroke rate over course of race

#Using the row_pred_data created earlier
#Want to set up the larger data set like the run set

test <- row_pred_data[,20:25]
tmp <- data.frame()
df <- data.frame()
row_id <- unique(row_pred_data$row_id)
for(i in row_id){
        df <- test[ test$row_id==i, ]
        split <- melted_mss_splits[ melted_mss_splits$row_id == i, ]
        df$split <- split$value[match(df$distance, split$distance)]
        tmp <- rbind(df, tmp)
}
#Bit slow (a few seconds) using for loop but does the job
tmp <- arrange(tmp, row_id)
row_pred_data$split <- tmp$split


#basic lm model
all_data_mod <- lm(split ~ distance + speed, row_pred_data)
#Obvious that distance is highly predictive of split times - speed also is significant, stroke not sig. try removing distance
mod2 <- lm(split ~ speed + stroke, row_pred_data)
#Speed is highly sig, stroke a little sig, but r-squared is 0.02. not a good fit. 

new_data <- data.frame(distance = row_pred_data$distance, speed = row_pred_data$speed, stroke = row_pred_data$stroke)
new_data$pred_split <- predict(mod, newdata = new_data)
#Ends up with a very restricted and poor prediction of final split time. 
#This is because (i think) speed is most important predictor, but its not very precise (only 1 decimal place)
#So runs end up looking much the same with broad speeds used to predict. Might try adding in place
melted_mss_place <- data_melt %>%
        filter(str_detect(variable, 'rank_final'))
melted_mss_place$value <- as.factor(melted_mss_place$value)

#This hasnt worked well, including all runs just averages everything out leading to bad results.
#I just want a model for each run - so make a list of data frames, each being a run and fit a model to each item in list
#but for now, I still need to figure out how to model the race result when you change a variable. may be an optimisation problem?
#Clearly distance is highly pred, and constrains the model a lot, lets take it out
#This was a quick fail. It needs distance or it just generates random numbers based on speed, doesnt account for race progression
mod <- lm(split2 ~ distance + speed + stroke, run)
predict(mod, newdata = transform(run[1:10,], speed = 4.9))
new_data <- data.frame(speed = run$speed, stroke = run$stroke)
new_data$split_pred <- predict(mod, newdata = new_data)

ggplot(run_slow, aes(x = distance, y = split)) +
        geom_smooth(method = lm)

#try predicting a slow race from fast race data
#just saw two runs with obviously wrong data so delete
row_pred_data <- filter(row_pred_data, row_id != "768")
row_pred_data <- filter(row_pred_data, row_id != "767")
row_pred_data <- filter(row_pred_data, row_id != "770")
row_pred_data <- filter(row_pred_data, row_id != "773")

run_fast <- row_pred_data[81:120,]
run_slow <- row_pred_data[801:840,]

mod_slow <- lm(split ~ distance + speed, run_slow)
mod_fast <- lm(split ~ distance + speed, run_fast)

new_data <- data.frame(distance = run_fast$distance, speed = run_fast$speed, stroke = run_fast$stroke)
pred_og <- predict(mod_slow, newdata = run_slow)
slow_pred_by_fast$split_og <- pred_og
slow_pred_by_fast <- cbind(run_slow, pred)
fast_pred_by_slow$split2 <- pred
fast_pred_by_slow <- cbind(run_slow, pred)

ggplot(fast_pred_by_slow) +
        geom_line(aes(x = distance, y = split)) +
        geom_line(aes(x = distance, y = pred))+
        geom_line(aes(x = distance, y = split2))

#What did i learn? using just speed is better, not always as accurate but less variation. smaller misses. 
#predicting the result of a slow race using a model trained on fast data shows a pretty good approx when only using speed
#fast race predicted from slow model def changes the end result to a much faster time than what the model was trained on - showing speed is 
#doing its job in the model, but misses the actual result by about 13 seconds, as opposed to 6 seconds in the first case. 
#Maybe rerun this stuff and make it more clear the effect of each added variable on predicted outcomes. 
#also play with made up data - did this, works pretty well. guess the main thing is, to make a new model for each run, or for each athlete at least. 

#Retry modeling all data
all_data_mod <- lm(split ~ distance + speed, row_pred_data)
#Fast run
new_data <- data.frame(distance = run_fast$distance, speed = run_fast$speed, stroke = run_fast$stroke)
pred_all_fast <- predict(all_data_mod, newdata = new_data)
#Slow run
new_data_slow <- data.frame(distance = run_slow$distance, speed = run_slow$speed)
pred_all_slow <- predict(all_data_mod, newdata = new_data_slow)
#normal run
new_data_norm <- data.frame(distance = run$distance, speed = run$speed)
pred_all_norm <- predict(mod_fast, newdata = new_data_norm)
#Does an ok job of predicting the 3 runs. but not that accurate. May be a good general model by modeling each race is still a good idea
#make some fake data
data <- data.frame(speed = c(4.9,4.7,4.8,5), distance = c(500,1000,1500,2000))
data$pred <- predict(mod_fast, newdata = data)
#This gets the same result. each point on the distance scale is taken along with the speed to determine time at that point
#This is independent of all other time points or distance. There is no effect of the last check point on the time at the end. 
#How can this be improved? 

