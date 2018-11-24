#Exploring large rowing data set
rowing_world_championships <- read_csv("~/Dropbox/Personal/Jobs/Performance Predictions/rowing_world_championships.csv")

library(tidyverse)
library(DataExplorer)
library(summarytools)
library(lubridate)
library(reshape2)
library(ggrepel)
library(magrittr)
library(broom)
library(RColorBrewer)
#devtools::install_github('thomasp85/gganimate')
#devtools::install_github("thomasp85/transformr")
library(transformr)
library(gganimate)

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
race_2 <- data_melt %>% filter(race_id == 'ROM012901' & 
                        championship_name_description == 'Slovenia 28 Aug - 4 Sept 2011""') %>%
                                filter(variable == 'split_1_time' | variable == 'split_2_time' | 
                                           variable == 'split_3_time' | variable == 'split_4_time')
#Plot Race 1 progression. Bit boring
ggplot(race_1_splits, aes(x = variable, y = value, group = bow_name, colour = bow_name)) +
    geom_line() #+ 
    #theme(legend.position="none")
#Look at one athlete over time
synek <- data_melt %>% filter(bow_name == 'SYNEK Ondrej') %>%
        filter(synek,variable == 'split_1_time' | variable == 'split_2_time' | 
                       variable == 'split_3_time' | variable == 'split_4_time')

#Plot 1 athlete's progress over time - ie overlay all split progressions through all races in set
ggplot(synek, aes(x = variable, y = value, group = row_id, colour = year)) +
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
synek_year <- ggplot(synek, aes(x = variable, y = value, group = row_id, colour = year)) +
        geom_line()
synek_round <- ggplot(synek, aes(x = variable, y = value, group = row_id, colour = round_type)) +
        geom_line()
#This makes for a more interesting date based analysis - how times change over the years. Also just focus in on the final time to see changes clearer
synek_times <- ggplot(filter(synek, variable == 'split_4_time'), aes(x = variable, y = value, group = row_id, colour = year, shape = round_type)) +
        geom_jitter(width = 0.02, size = 5)

library(gridExtra)
grid.arrange(synek_year, synek_round)
#Not quite as clear as id hope but shows finals are quicker - maybe too many splits (year and round type)
#From year - 2014 had the slowest and fastest times, slowest was quarter, fastest was final. Semi was also very quick that year 
#Overall this is a good approach to zoom in on a single athlete in a single race but the dataset is much richer than this. 
#Think of how to expand this type of exploration with the larger dataset. will depend on how melt performs. 


#### Larger scale exploration of race results (splits) ####
#Will recycle some of the above code applied to all races and athletes
#Start again by looking at the one race type
rowing_world_championships$row_id <- c(1:nrow(rowing_world_championships))
rowing_world_championships$row_id <- as.factor(rowing_world_championships$row_id)
mss <- filter(rowing_world_championships, event_cateogry_abbreviation == "M1x")
mss$race_date <- ymd(mss$race_date)
mss <- arrange(mss, race_date) #Found data is pretty inconsequential outside of year so will only consider year
mss$year <- year(mss$race_date)
mss$year <- as.factor(mss$year)
mss <- select(mss, -c(Year, lane_sl, event_category, event_num, race_number, coxswain_birthday:third_name))
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
        geom_line(alpha = 0.3)
#319 rows contained missing data - will look at this
#seems in some runs the speed was very low after 50m, and some runs it stayed very low - possible data collection issue? 
#speeds are between 3 and 6, with mean sitting at approx 4.25. Could disregard any runs with < 3 speed after 250m
ggplot(filter(melted_mss_speed, variable >= 250 & value > 3), aes(x = variable, y = value, group = row_id, colour = year)) +
        geom_line(alpha = 0.3) 

#Can see where the 'wrong' runs are comming in. Problem with viz is the 2017 values blocking everything else. 
#fit a basic lm
ggplot(filter(melted_mss_speed, variable >= 250 & value > 3), aes(x = variable, y = value, group = team, colour = team)) +
        geom_jitter(alpha = 0.2) +
        geom_smooth(method = loess, size = 2, se = FALSE) 
        #geom_text_repel(data = . %>% filter(variable == max(variable)), aes(x = variable, y = value, label = team))
#Shows the average speed in the local area for each year's line. 
#Seems 2013 start, middled and ended slower
#2014 started fasted, middled high but faded towards the end
#2015 started 2nd fasted, maintained and finished strong. 
#This is all based on average speed across years. Could look at average speed for different countries or per athlete across all years
#Plot shows mean speed profiles per country with labels- labels overlap but cant get geom_text_repel to work
ggplot(filter(melted_mss_speed, variable >= 250 & value > 3), 
       aes(x = variable, y = value, group = team, colour = team, label = team)) +
                geom_smooth(method = loess, size = 2, se = FALSE, alpha = 0.3) +
                geom_label(data = group_by(melted_mss_speed, team) %>%
                           do(augment(loess(value~variable, .))) %>%
                           filter(variable == max(variable)) %>%
                           #filter(top_n(team, 10)),
                           aes(variable, .fitted), nudge_x = 100, inherit.aes = T) +
                theme(legend.position="none")

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
colnames(row_pred_data)[22] = 'year'
colnames(row_pred_data)[23] = 'distance'
colnames(row_pred_data)[24] = 'speed'
row_pred_data$stroke <- melted_mss_stroke$value
###Accidently did this twice###
#To so this i need to model all the data for this race
mss_model_data <- data.frame(cbind(melted_mss_speed, stroke = melted_mss_stroke$value))
#Change col names
mss_model_data <- rename(mss_model_data, distance = 23, speed = 24)
rm(mss_model_data)


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
speed_dis <- ggplot(run, aes(x = distance, y = speed)) +
        geom_line()
#See how stroke rate changes
stroke_dis <- ggplot(data = run) +
        geom_line(aes(x = distance, y = stroke))
#Stroke and speed reach lowest level at around 1000m. stroke steadly increases from there, speed does too - 
#though scale is less senstive. 

#IDEA: set up a shiny app that selects an athlete, builds model based on their races, and then you can manipulate
#speed or stroke. Can plot more than 1 athlete and see how stroke and speed affect different athlets or all athletes
#So for each athlete, when selected (test filter) it builds 1 model on their races - 4 races per year for top athletes. 
#Should also generate plots that show speed and stroke rate over course of race

#Using the row_pred_data created earlier
#Want to set up the larger data set like the run set
#Try a different tact - map the values
melted_mss_splits <- arrange(melted_mss_splits, row_id)
melted_mss_splits$distance <- rep(c(500,1000,1500,2000), length(unique(melted_mss_splits$row_id)))

data <- row_pred_data[,20:25]
tmp <- data.frame()
df <- data.frame()
row_id <- unique(row_pred_data$row_id)
for(i in row_id){
        df <- data[ data$row_id==i, ]
        split <- melted_mss_splits[ melted_mss_splits$row_id == i, ]
        df$split <- split$value[match(df$distance, split$distance)]
        tmp <- rbind(df, tmp)
}
#Bit slow (a few seconds) using for loop but does the job
tmp <- arrange(tmp, row_id)
row_pred_data$split <- tmp$split


#basic lm model
all_data_mod <- lm(split ~ distance + speed + stroke, row_pred_data)
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

new_data <- run[,22:24]
new_data_mod <- new_data[1:10,2]+0.2
new_data_mod2 <- new_data
new_data_mod2[1:10,2] <- new_data_mod

pred1 <- predict(mod, new_data)
pred2 <- predict(mod, new_data_mod2)
pred_test <- cbind(new_data, pred1, pred2)

ggplot(pred_test) +
        geom_line(aes(x = distance, y = pred1)) +
        geom_line(aes(x = distance, y = pred2))

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

#Maybe time-series analysis? But i dont want to get too far into that.
#Use this lm to show how speed can impact the outcome.
#Maybe look at adding rank into a random forest just to do it. 
#Then look at using maybe random forest to set up funnels. This would work by classifying the finals 
#Winning times as the outcome of interest.

#From mitch's idea - use split time 1 to predict split 2; split 2 for split 3 and so on.
#Overall viz of this idea
split1_2 <- ggplot(mss, aes(x = split_1_time, y = split_2_time, group = championship_name_description, colour = championship_name_description)) +
        geom_point(alpha = 0.3) +
        geom_smooth(method = lm) +
        theme(legend.position="none")
split2_3 <- ggplot(mss, aes(x = split_2_time, y = split_3_time, group = championship_name_description, colour = championship_name_description)) +
        geom_point(alpha = 0.3) +
        geom_smooth(method = lm) +
        theme(legend.position="none")
split3_4 <- ggplot(mss, aes(x = split_3_time, y = split_4_time, group = championship_name_description, colour = championship_name_description)) +
        geom_point(alpha = 0.3) +
        geom_smooth(method = lm)+
        theme(legend.position="none")
grid.arrange(split1_2, split2_3, split3_4, ncol = 1)
#Clearly some outliers to remove
rowing_world_championships <- filter(rowing_world_championships, split_1_time < 250)
rowing_world_championships <- filter(rowing_world_championships, split_2_time < 550)

#Prediction: Final rank predicted from heat, semi, prelim rank (or time)
#seems to be more finals than any other type of race
table(mss$round_type)
#Change to factor and order so they appear in order. 
mss$round_type <- as.factor(mss$round_type)
mss$round_type <- ordered(mss$round_type, levels = c('heat', 'repecharge', 'quarterfinal', 'semifinal', 'final'))
#plot
ggplot(mss, aes(x = round_type)) +
        geom_bar()
#Group by competition
ggplot(mss, aes(x = round_type, fill = championship_name_description)) +
        geom_bar(position = 'dodge')
#seems like there were more races in general in 2015 and less in 2014. 
#Plot finishing time by round type
ggplot(mss, aes(x = round_type, y = split_4_time)) + 
        geom_boxplot()
#Overall semi is the fastest race followed by quater, final is no faster than heats and repecharge much slower than all others
#Means finals are not faster races generally. Maybe conditions play a bigger part than expected
ggplot(mss, aes(x = round_type, y = split_4_time, colour = championship_name_description)) + 
        geom_boxplot()
#Viz is off, try something else
ggplot(mss, aes(x = year, y = split_4_time, colour = round_type)) + 
        geom_boxplot() + scale_color_brewer(palette ="Paired")

#Better - no clear pattern but something to the idea semi is fastest. 
#Came across a problem with the years. There is no data from 2010, 2012, 2016 - using event category was a bad choice
mss <- filter(rowing_world_championships, event_cateogry_abbreviation == "M1x")
#final times have by far the widest spread. people giving up at the end? 
#Remove runs that took longer than 500 seconds? 
mss <- filter(mss, split_4_time < 500)
#No clear pattern, semis are faster in later years but not so in first couple of years. TIME NOT A GOOD VARIABLE
library(lattice)
xyplot(split_4_time ~ rank_final | round_type, data = mss, layout = c(5,1))
#interesting plot showing spread of times for each placing for each round_type. Big spread in finals, very tight in quarters. 
#Change order of plots in lattice
xyplot(split_4_time ~ rank_final | round_type, group = as.factor(year), data = mss, layout = c(5,1), 
       auto.key=list(space="right", points=T), index.cond = list(c(2,4,3,5,1)))
#Cool plot. shows how useless time is. Also shows there are multiple 'final' races in each year- whats this about? 
#Tells me rank may be the best predictor for future result. does heat rank predict quater rank? 
mss <- filter(mss, dnf == 'FALSE')
mss_rank <- select(mss, .data$team, .data$rank_final, .data$progression, .data$round, .data$round_type, .data$year, .data$row_id)
melted_mss_rank <- melt(mss_rank)
#Trying to understand this data: Round and round type
#Finals: FA, FB, FC, FD, FE, FF
#FA is the medal final, the others contain losers from semis, quarters, rep etc
#So want to predict place in FA from other variables
#First fix some of the data

melted_mss_rank$round <- str_replace(melted_mss_rank$round, "QAD ", "Q")
melted_mss_rank$round <- str_replace(melted_mss_rank$round, "QEH ", "Q")
#For some reason the round_type is all out, not needed
melted_mss_rank <- select(melted_mss_rank, -starts_with('round_type'))
melted_mss_rank <- select(melted_mss_rank, -starts_with('progression'))

#Trying to get data to a point where i can use rank in prior races to predict final rank. Not working
test <- spread(mss_rank, key = 'year', value = 'rank_final')
mx1_2017 <- test[,c(1,2,3,9)]
#Whats the point of predicting who makes the final based on rank? obviously if you finish higher you have more chance to make finals
#What about a plot that shows a rowers progression through the competition
names(mx1_2017)[4] <- 'rank'
mx1_2017$rank <- as.numeric(mx1_2017$rank)
mx1_2017 <- filter(mx1_2017, complete.cases(mx1_2017))

ggplot(mx1_2017, aes(x = round, y = rank, group = team, colour = team)) +
        geom_line()
#Good start - now make all heats and repecharges the same (ie H instead of H1, H2 etc) then order the rounds
#Something with a 'starts with 'H'' type argument would be better but couldnt find. Dont like grep functions - too hard to read. 
mx1_2017$round <- mx1_2017$round %<>%
        str_replace("H1", "H") %>%
        str_replace("H2", "H") %>%
        str_replace("H3", "H") %>%
        str_replace("H4", "H") %>%
        str_replace("H5", "H") %>%
        str_replace("H6", "H") %>%
        str_replace("H7", "H") %>%
        str_replace("H8", "H") %>%
        str_replace("R1", "R") %>%
        str_replace("R2", "R") %>%
        str_replace("R3", "R") %>%
        str_replace("R4", "R") %>%
        str_replace("R5", "R") %>%
        str_replace("R6", "R") %>%
        str_replace("R7", "R") %>%
        str_replace("R8", "R")
#Order the rounds from earliest to latest
mx1_2017$round2 <- ordered(mx1_2017$round2, levels = c('H', 'R', 'Q4', 'Q3', 'Q2', 'Q1', 'SE/F/G 3', 'SE/F/G 2', 
                                                       'SE/F/G 1', 'SC/D 2', 'SC/D 1', 'SA/B 2', 'SA/B 1', 'FG', 
                                                       'FF', 'FE', 'FD', 'FC', 'FB', 'FA'))
#Select teams that made it to Finals A (medal finals)
teams <- mx1_2017[mx1_2017$round == "FA",1]
#Plot each Finals A team through each round
ggplot(filter(mx1_2017, team %in% teams), aes(x = round, y = rank, group = team, colour = team)) +
        geom_line(size = 1.5, alpha = 0.6) +
        geom_point()
#Now that i have a plan and code lets try this for the whole data set
#THis code is bad and redundant 
# spread_mss_rank <- spread(mss_rank, key = 'year', value = 'rank_final')
# names(spread_mss_rank) <- c('team', 'round', 'rank', 'rank', 'rank', 'rank','rank', 'rank', 'rank')
# r1 <- spread_mss_rank[,c(1,2,4)]
# r1 <- filter(r1, complete.cases(r1))
# r1$year <- 2010
# r2 <- spread_mss_rank[,c(1,2,5)]
# r2 <- filter(r2, complete.cases(r2))
# r2$year <- 2011
# r3 <- spread_mss_rank[,c(1,2,6)]
# r3 <- filter(r3, complete.cases(r3))
# r3$year <- 2013
# r4 <- spread_mss_rank[,c(1,2,7)]
# r4 <- filter(r4, complete.cases(r4))
# r4$year <- 2014
# r5 <- spread_mss_rank[,c(1,2,8)]
# r5 <- filter(r5, complete.cases(r5))
# r5$year <- 2015
# r6 <- spread_mss_rank[,c(1,2,9)]
# r6 <- filter(r6, complete.cases(r6))
# r6$year <- 2017
# mx1_prog <- rbind(r1,r2,r3,r4,r5,r6)

mx1_prog <- mss_rank %>%
        select(team, round, rank_final, year) %>%
        arrange(year, team)
mx1_prog$team <- as.factor(mx1_prog$team)
#Whats the point of predicting who makes the final based on rank? obviously if you finish higher you have more chance to make finals
#Good start - now make all heats and repecharges the same (ie H instead of H1, H2 etc) then order the rounds
#Something with a 'starts with 'H'' type argument would be better but couldnt find. Dont like grep functions - too hard to read. 
mx1_prog[grep('H', mx1_prog$round), 3] <- 'H'
mx1_prog[grep('R', mx1_prog$round), 3] <- 'R'
#Order the rounds from earliest to latest
mx1_prog$round <- ordered(mx1_prog$round, levels = c('H', 'R', 'Q4', 'Q3', 'Q2', 'Q1', 'SE/F/G 3', 'SE/F/G 2', 
                                                       'SE/F/G 1','SE/F 1','SE/F 2', 'SC/D 2', 'SC/D 1', 'SA/B 2', 'SA/B 1', 'FG', 
                                                       'FF', 'FE', 'FD', 'FC', 'FB', 'FA'))


teams2010 <- mx1_prog[mx1_prog$round == "FA" & mx1_prog$year == '2010',1]
teams2011 <- mx1_prog[mx1_prog$round == "FA" & mx1_prog$year == '2011',1]
teams2013 <- mx1_prog[mx1_prog$round == "FA" & mx1_prog$year == '2013',1]
teams2014 <- mx1_prog[mx1_prog$round == "FA" & mx1_prog$year == '2014',1]
teams2015 <- mx1_prog[mx1_prog$round == "FA" & mx1_prog$year == '2015',1]
teams2017 <- mx1_prog[mx1_prog$round == "FA" & mx1_prog$year == '2017',1]


#What about a plot that shows a rowers progression through the competition
test_plot <- ggplot(filter(mx1_prog, team %in% teams2017 & year == '2017'), 
       aes(x = round, y = rank, group = team, colour = team)) +
        geom_line(size = 1.5, alpha = 0.6) +
        geom_point()

ggplot(mx1_prog, aes(x = round, y = rank, group = team, colour = team)) +
        geom_line(size = 1.5, alpha = 0.6) +
        geom_point()

#maybe this could be a table on the shiny app.
rank1 <- mx1_prog[mx1_prog$round == 'FA' & mx1_prog$rank == 1,]

#gganimate plot - had to make the rounds numeric so lose information there. 
#Shows how a rower progresses through a competition in terms of finishing position in each round
mx1_prog$progression <- unclass(mx1_prog$round)
ggplot(filter(mx1_prog, team %in% teams2017 & year == '2017'), 
       aes(x = progression, y = rank, group = team, colour = team)) +
        geom_line(size = 1.5, alpha = 0.6) +
        transition_reveal(team, progression)

#I think this is good. if i can get it to work with all gold medalists for example that would be cool. 
#And a better way to represent round. 


#Trying bens idea of animating a race
fa_2017 <- filter(row_pred_data, year == '2017' & round == 'FA')
fa_2017[fa_2017$distance == '50',26] <- 0
fa_2017 <- select(fa_2017, c(bow_name, distance, split)) %>%
        na.omit()
fa_2017$rank <- as.factor(c(6,6,6,6,6,4,4,4,4,4,1,1,1,1,1,3,3,3,3,3,2,2,2,2,2,5,5,5,5,5))

race_sim_2017 <- ggplot(fa_2017, aes(x = distance, y = bow_name, group = bow_name, colour = rank)) +
        geom_point(size = 4) +
        geom_line(size = 4) +
        theme(legend.position="none") +
        scale_color_brewer(palette ="Greens", direction = -1) +
        labs(x = 'Distance' , y = '') +
        geom_vline(xintercept = 2000, linetype="dotted", size = 1) +
        transition_reveal(bow_name, split)
animate(race_sim_2017, width = 1000, height = 500, duration = 15)
#diff animation thats a bit smoother
animate(race_sim_2017, width = 1000, height = 500, duration = 3, fps = 60)
#Saves the last created animation 
anim_save('race_sim_2017_60fps.gif')



r_2010 <- mss_rank %>%
        filter(year %in% '2010') %>%
        select(team, round, rank_final, year) %>%
        arrange(team)

####Model attempt 2####
#Predict split 2 from split 1 for an athlete
#This athlete has 20 races to draw from
#Data set-up: need a row for each split value so spread the original dataset
synek_spt_pred <- spread(synek, key = variable, value = value)
#make lm model
synek_1.2_lm <- lm(split_2_time ~ split_1_time, synek_spt_pred)
synek_2.3_lm <- lm(split_3_time ~ split_2_time, synek_spt_pred)
synek_3.4_lm <- lm(split_4_time ~ split_3_time, synek_spt_pred)
#Predict time at split 2 from given split 1 - predict needs a dataframe specifying the predictor variable name for new data argument
predict(synek_1.2_lm, newdata = data.frame(split_1_time = 100))

#viz of mod 1
ggplot(synek_spt_pred, aes(split_1_time, split_2_time, colour = round_type)) +
        geom_point(size = 4) +
        geom_smooth(aes(split_1_time, split_2_time), method = lm, inherit.aes = FALSE)

#viz of mod 2
ggplot(synek_spt_pred, aes(split_2_time, split_3_time, colour = round_type)) +
        geom_point(size = 4) +
        geom_smooth(aes(split_2_time, split_3_time), method = lm, inherit.aes = FALSE)

#viz of mod 3
ggplot(synek_spt_pred, aes(split_3_time, split_4_time, colour = round_type)) +
        geom_point(size = 4) +
        geom_smooth(aes(split_3_time, split_4_time), method = lm, inherit.aes = FALSE)

#Multiple lm
synek_1.4_lm <- lm(split_4_time ~ split_1_time + split_2_time + split_3_time, synek_spt_pred) 
synek_1.3_lm <- lm(split_3_time ~ split_1_time + split_2_time, synek_spt_pred) 
#Take away: using the 1.3 model, need to feed it inputs at split 1 and split 2 kind of defeating the purpose, 
#want the model to predict time at s2 from s1 then s3 from s2 which is achieved as follows: 
predict(synek_2.3_lm, newdata = data.frame(split_2_time = 
                                                   predict(synek_1.2_lm, newdata = data.frame(split_1_time = 100))))

fournier <- data_melt %>% filter(bow_name == 'FOURNIER RODRIGUEZ Angel') %>%
        filter(variable == 'split_1_time' | variable == 'split_2_time' | variable == 'split_3_time' | variable == 'split_4_time')
fournier_spt_pred <- spread(fournier, key = variable, value = value)

fournier_1.2_lm <- lm(split_2_time ~ split_1_time, fournier_spt_pred)
fournier_2.3_lm <- lm(split_3_time ~ split_2_time, fournier_spt_pred)
fournier_3.4_lm <- lm(split_4_time ~ split_3_time, fournier_spt_pred)

fournier_1.3_lm <- lm(split_3_time ~ split_1_time + split_2_time, fournier_spt_pred)

ggplot(fournier_spt_pred, aes(split_1_time, split_2_time, colour = year)) +
        geom_point(size = 4) +
        geom_smooth(aes(split_1_time, split_2_time), method = lm, inherit.aes = FALSE)

#viz of mod 2
ggplot(fournier_spt_pred, aes(split_3_time, split_4_time, colour = year)) +
        geom_point(size = 4) +
        geom_smooth(aes(split_3_time, split_4_time), method = lm, inherit.aes = FALSE)

predict(fournier_2.3_lm, newdata = data.frame(split_2_time = 
                                                   predict(fournier_1.2_lm, newdata = data.frame(split_1_time = 98))))

#Try to improve by adding variables
#Adding round and year had the best results but probably over fitting since there are only 1 example for each round mostly. 
#Remove the heat/ semi number and it may make it more generalisable but probably more complext than it needs to be.
summary(lm(split_2_time ~ split_1_time + round_type, fournier_spt_pred))
summary(lm(split_3_time ~ split_2_time + round_type, fournier_spt_pred))

#Next thing im going to do is try plotting the seperate model lines on the same plot. then plot the average line and an individuals line to show if they are better or worse than average
synek1.2_line <- coef(synek_1.2_lm)
synek2.3_line <- coef(synek_2.3_lm)
fournier1.2_line <- coef(fournier_1.2_lm)
fournier2.3_line <- coef(fournier_2.3_lm)

#manual colour for legend
cols = c('Fournier' = '#F78181', 'Synek' = '#04B4AE')


plot_13 <- ggplot(data = mss, aes(split_1_time, split_2_time)) +
        geom_point(aes(split_1_time, split_2_time), alpha = 0.2, inherit.aes = FALSE) +
        geom_point(aes(split_1_time, split_2_time), alpha = 0.2, inherit.aes = FALSE) + 
        geom_point(data = synek_spt_pred, aes(colour = 'Synek'), size = 2) +
        geom_point(data = fournier_spt_pred, aes(colour = 'Fournier'), size = 2) +
        geom_abline(slope = synek1.2_line[[2]], intercept = synek1.2_line[[1]], colour = '#04B4AE', size = 1) +
        geom_abline(slope = fournier1.2_line[[2]], intercept = fournier1.2_line[[1]], colour = '#F78181', size = 1)

row_mod_plotly <- ggplotly(plot_13)
api_create(row_mod_plotly, filename = "row_mod_plotly")
#interesting plot - suggests fournier is a slow starter? His line is flatter so at slower s1 runs, it has less effect on his s2 time
#but synek tends to be faster in general with more runs at <100 s1. Synek line influenced by an outlier too. 
#so if Fournier goes 102.5 at s1 he will be faster than synek at s2 if synek also went 102.5 but thats unlikely. 
#still only s1 and s2. the outlier for synek was a slow race for everyone by the looks and synek ended up beating fournier despite being 4 seconds behind at s2
#where the line crosses is of note - < 100 synek faster, > 100 Fournier faster. but need to know what is realistic. main point is Fournier is quick between s1 and s2

#This plot shows the same but with an average line added from all data
mss_s2.s3_plot <- ggplot(data = mss, aes(split_2_time, split_3_time, group = year, text = paste('round type:', round_type))) +
        geom_point(alpha = 0.2) +
        geom_point(data = synek_spt_pred, aes(colour = 'Synek'), size = 3) +
        geom_point(data = fournier_spt_pred, aes(colour = 'Fournier'), size = 3) +
        geom_smooth(aes(split_2_time, split_3_time), method = lm, se = F, inherit.aes = FALSE) #+
        # geom_abline(slope = synek2.3_line[[2]], intercept = synek2.3_line[[1]], colour = '#04B4AE') +
        # geom_abline(slope = fournier2.3_line[[2]], intercept = fournier2.3_line[[1]], colour = '#F78181')
       
ggplotly(mss_s2.s3_plot)
        
#points below the avg line are faster than average
#Move on, spent enough time on this. 

#s1 by s4, not a great plot, hard to interpret. 
ggplot(mss, aes(split_1_time, split_4_time)) +
        geom_point(alpha = 0.3) +
        geom_smooth(method = lm)

#Probably a story here about identifying rowers who perform better or worse than average. 
#below line means they got to the 1500m mark faster than average based on 1000m time.
#lower on the line show overall speed through the race. ie being below the line is less important the further you get from the fast times. 
#having interactivity that lets you hover on the point to see rower, year and round would be good. Pretty much got this but not perfect

####classification model on ranks####
rank_pred_data <- mss %>% select(team, rank_final, split_4_time, round, bow_name, bow_birthday, row_id, year)
rank_pred_2010 <- filter(rank_pred_data, year == '2010')
rank_pred_2010[grep('H', rank_pred_2010$round), 4] <- 'H'
rank_pred_2010[grep('R', rank_pred_2010$round), 4] <- 'R'
rank_pred_2010[grep('SC', rank_pred_2010$round), 4] <- 'SC/D'
rank_pred_2010[grep('SA', rank_pred_2010$round), 4] <- 'SA/B'
rank_pred_2010$rank_overall <- 0
#Order the rounds from earliest to latest
rank_pred_2010$round <- as.factor(rank_pred_2010$round)
rank_pred_2010$round <- ordered(rank_pred_2010$round, levels = c('H', 'R', 'Q4', 'Q3', 'Q2', 'Q1', 'SE/F/G 3', 'SE/F/G 2', 
                                                                 'SE/F/G 1','SE/F 1','SE/F 2', 'SC/D 2', 'SC/D 1', 'SA/B 2', 'SA/B 1', 'FG', 
                                                                 'FF', 'FE', 'FD', 'FC', 'FB', 'FA'))
rank_pred_2010 <- arrange(rank_pred_2010, round, split_4_time)


#Previous attempt to predict final rank based on heat rank just doesnt work
#Now will try to predict performance based of past performances
#So could get 1 rowers performances and get an overall rank (time based from all heats, semis etc) - might hit the same problem
#Maybe just look at the rowers final finishing place overall from the final rounds. 
#Filter all F_ races and rank from 1:n based on time. 
#Want to find if that rank from 2010 can predict 2011 finishing position. So column will need to be (see notebook)



