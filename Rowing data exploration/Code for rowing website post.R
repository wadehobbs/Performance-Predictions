library(tidyverse)
library(DataExplorer)
library(summarytools)
library(lubridate)
library(reshape2)
library(ggrepel)
library(magrittr)
library(gridExtra)
library(broom)
library(RColorBrewer)
library(plotly)
library(transformr) # install by running code below
library(gganimate) # install by running code below

devtools::install_github('thomasp85/gganimate')
devtools::install_github("thomasp85/transformr")

rowing_world_championships <- read_csv("rowing_world_championships.csv")
view(dfSummary(rowing_world_championships))  

####Data cleaning and wrangling####
#Chose the men's single scull as it has the most entries
mss <- filter(rowing_world_championships, event_cateogry_abbreviation == "M1x")
mss$race_date <- dmy(mss$race_date)
mss <- arrange(mss, race_date) #Found date is pretty inconsequential outside of year so will only consider year
mss$year <- year(mss$race_date) #year function from lubridate package
mss$year <- as.factor(mss$year) #Make the year variable a factor for plotting

#Add unique row id for each rower within each race - comes in handy later.
mss$row_id <- as.factor(c(1:nrow(mss)))

#Remove useless variables - note spelling mistake in 'event_cateogry_abbreviation'
mss <- select(mss, -c(Year, lane_sl, event_cateogry_abbreviation, event_num, race_number, coxswain_birthday:third_name))

data_melt <- melt(mss)
#select one race
race_1 <- data_melt %>% filter(race_id == 'ROM012901' & 
                                       championship_name_description == 'Slovenia 28 Aug - 4 Sept 2011""') %>%
        filter(variable == 'split_1_time' | variable == 'split_2_time' | 
                       variable == 'split_3_time' | variable == 'split_4_time')

#The melt function collapses the 4 splits columns into one called variable (along with speed, rank, stroke and others)
#With corresponding value of the given variable in the value column
race_1 %>% 
        select(race_id, championship_name_description, variable, value) %>%
        head()

##Viz##
ggplot(race_1, aes(x = variable, y = value, group = bow_name, colour = bow_name)) +
        geom_line()

synek <- data_melt %>% filter(bow_name == 'SYNEK Ondrej') %>%
        filter(variable == 'split_1_time' | variable == 'split_2_time' | 
                       variable == 'split_3_time' | variable == 'split_4_time')

#plot of athlete's race progression across numerous years
ggplot(synek, aes(x = variable, y = value, group = row_id, colour = year)) +
        geom_line()

#plot of athletes race progression by round type (ie heat, semi, final)
ggplot(synek, aes(x = variable, y = value, group = row_id, colour = round_type)) +
        geom_line()

#plot shows finishing time for each race of each year, coloured by year and shape shows round type
ggplot(filter(synek, variable == 'split_4_time'), 
       aes(x = variable, y = value, group = row_id, colour = year, shape = round_type)) +
        geom_jitter(width = 0.02, size = 5)

#Result of last plot suggests finals are fastest and heats generally slowest, 
#but with variability across years and round types.

#Subset rows from data_melt for the split times - excluding the other features in variable column (speed, stroke etc)
melted_mss_splits <- filter(data_melt,variable == 'split_1_time' | variable == 'split_2_time' | 
                                    variable == 'split_3_time' | variable == 'split_4_time')

#Split 4 plotted across years
ggplot(filter(melted_mss_splits, variable == 'split_4_time'),
       aes(x = variable, y = value, group = row_id, colour = year)) +
        geom_jitter(width = 0.1, size = 5, alpha = 0.5)

####Speed####
melted_mss_speed <- data_melt %>%
        filter(str_detect(variable, 'speed'))   #str_detect function finds rows containing the given string

#Set the variable column to the speed distance only. ie remove 'speed_' from string
#parse_number extracts a number from a string ie 'speed_100' becomes 100
melted_mss_speed$variable <- parse_number(melted_mss_speed$variable)   

#order based on row_id and speed
melted_mss_speed <- arrange(melted_mss_speed, row_id, variable)

#plot all races looking at speed across the race - each year has a different colour
ggplot(melted_mss_speed, aes(x = variable, y = value, group = row_id, colour = year)) +
        geom_line()

ggplot(filter(melted_mss_speed, variable >= 250 & value > 3), 
       aes(x = variable, y = value, group = row_id, colour = year)) +
        geom_line() 

#Plot shows mean speed profiles per country with labels- labels overlap but cant get geom_text_repel to work
ggplot(filter(melted_mss_speed, variable >= 250 & value > 3), 
       aes(x = variable, y = value, group = team, colour = team, label = team)) +
        geom_smooth(method = loess, size = 2, se = FALSE) +           #This creates the model lines
        geom_label(data = group_by(melted_mss_speed, team) %>%       #This section creates the labels (straight from stackoverflow)
                           do(augment(loess(value~variable, .))) %>%
                           filter(variable == max(variable)),
                   aes(variable, .fitted), nudge_x = 100, inherit.aes = T) +
        theme(legend.position="none")


####Modeling####
melted_mss_stroke <- data_melt %>%
        filter(str_detect(variable, 'strokes'))

#Create one data set with both speed and stroke rate 
melted_mss_stroke$variable <- parse_number(melted_mss_stroke$variable) 
melted_mss_stroke <- arrange(melted_mss_stroke, row_id, variable)
row_pred_data <- melted_mss_speed
colnames(row_pred_data)[23] = 'distance'
colnames(row_pred_data)[24] = 'speed'
row_pred_data$stroke <- melted_mss_stroke$value

#Create splits data
melted_mss_splits <- data_melt %>%
        filter(str_detect(variable, 'split'))
melted_mss_splits <- arrange(melted_mss_splits, row_id)
#Add distance value for each split so it can be mapped to distance in the row_pred_data set. 
melted_mss_splits$distance <- rep(c(500,1000,1500,2000), length(unique(melted_mss_splits$row_id)))

#Takes a subset of the row_pred_data, maps the ith rowers splits to distance and adds to a dataset (tmp)
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
#tmp now has all the splits mapped and the split column is added to the target data - row_pred_data
tmp <- arrange(tmp, row_id)
row_pred_data$split <- tmp$split

#MODEL 1
all_data_mod <- lm(split ~ distance + speed + stroke, row_pred_data) 
summary(all_data_mod) 

#MODEL 2 - check
#Remove any serious outliers
rowing_world_championships <- filter(rowing_world_championships, split_1_time < 250)
rowing_world_championships <- filter(rowing_world_championships, split_2_time < 550)

#Create plots
split1_2 <- ggplot(mss, aes(x = split_1_time, y = split_2_time, group = year, colour = year)) +
        geom_point() +
        geom_smooth(method = lm) +
        theme(legend.position="none")
split2_3 <- ggplot(mss, aes(x = split_2_time, y = split_3_time, group = year, colour = year)) +
        geom_point() +
        geom_smooth(method = lm) +
        theme(legend.position="none")
split3_4 <- ggplot(mss, aes(x = split_3_time, y = split_4_time, group = year, colour = year)) +
        geom_point() +
        geom_smooth(method = lm)+
        theme(legend.position="none")

#This arranges multiple plots in the same image
grid.arrange(split1_2, split2_3, split3_4, ncol = 1)

#Data set-up: need a row for each split time so spread the original dataset
synek_spt_pred <- spread(synek, key = variable, value = value)
#make lm model
synek_1.2_lm <- lm(split_2_time ~ split_1_time, synek_spt_pred)
synek_2.3_lm <- lm(split_3_time ~ split_2_time, synek_spt_pred)
synek_3.4_lm <- lm(split_4_time ~ split_3_time, synek_spt_pred)

#Predict time at split 2 from given split 1 
#predict needs a dataframe specifying the predictor variable name for new data argument
predict(synek_1.2_lm, newdata = data.frame(split_1_time = 100))

#viz of mod
ggplot(synek_spt_pred, aes(split_1_time, split_2_time, colour = round_type)) +
        geom_point(size = 4) +
        geom_smooth(aes(split_1_time, split_2_time), method = lm, inherit.aes = FALSE)

predict(synek_2.3_lm, newdata = data.frame(split_2_time = 
                                                   predict(synek_1.2_lm, newdata = data.frame(split_1_time = 100))))

#Test with another athlete
fournier <- data_melt %>% filter(bow_name == 'FOURNIER RODRIGUEZ Angel') %>%
        filter(variable == 'split_1_time' | variable == 'split_2_time' | variable == 'split_3_time' | variable == 'split_4_time')
fournier_spt_pred <- spread(fournier, key = variable, value = value)

#models
fournier_1.2_lm <- lm(split_2_time ~ split_1_time, fournier_spt_pred)
fournier_2.3_lm <- lm(split_3_time ~ split_2_time, fournier_spt_pred)
fournier_3.4_lm <- lm(split_4_time ~ split_3_time, fournier_spt_pred)

#Save the intercept and slope from each model for the plot
synek1.2_line <- coef(synek_1.2_lm)
synek2.3_line <- coef(synek_2.3_lm)
fournier1.2_line <- coef(fournier_1.2_lm)
fournier2.3_line <- coef(fournier_2.3_lm)

#set colours
cols = c('Fournier' = '#F78181', 'Synek' = '#04B4AE')

#make plot
mod_plot <- ggplot(data = mss, aes(split_1_time, split_2_time)) +
        geom_point(aes(split_1_time, split_2_time), alpha = 0.2, inherit.aes = FALSE) +
        geom_point(aes(split_1_time, split_2_time), alpha = 0.2, inherit.aes = FALSE) + 
        geom_point(data = synek_spt_pred, aes(colour = 'Synek'), size = 2) +
        geom_point(data = fournier_spt_pred, aes(colour = 'Fournier'), size = 2) +
        geom_abline(slope = synek1.2_line[[2]], intercept = synek1.2_line[[1]], colour = '#04B4AE') +
        geom_abline(slope = fournier1.2_line[[2]], intercept = fournier1.2_line[[1]], colour = '#F78181')
ggplotly(mod_plot)


####Data exploration - Extended####
#remove rowers who slowed before end of the race
mss <- filter(mss, split_4_time < 500)

#Change to factor and order so the rounds appear in order from heat to final. 
mss$round_type <- as.factor(mss$round_type)
mss$round_type <- ordered(mss$round_type, levels = c('heat', 'repecharge', 'quarterfinal', 'semifinal', 'final'))

#Plot finishing time by round type
ggplot(mss, aes(x = round_type, y = split_4_time)) + 
        geom_boxplot()

ggplot(mss, aes(x = year, y = split_4_time, colour = round_type)) + 
        geom_boxplot() + scale_color_brewer(palette ="Set1") 

#First step is to get the data - this is simply sub-setting columns from the mss_rank data set and arranging 
#by year and team for ease of use - not strictly necessary.
mss_rank <- select(mss, .data$team, .data$rank_final, .data$progression, .data$round, .data$round_type, .data$year, .data$row_id)

mx1_prog <- mss_rank %>%
        select(team, round, rank_final, year) %>%
        arrange(year, team)
mx1_prog$team <- as.factor(mx1_prog$team)

#Next step is to set up the 'round' column so its clear when an athlete progresses from heat to semi to final for example. 
#For this, I made all heats "H" instead of "H1", "H2" etc. Same for repecharge rounds. 
#The heat number is essentially meaningless - the quarter, semi and final numbers do have meaning. 
#The following code subsets rows of the data frame that I want to change, then assigns those rows in the 4th column the string 'H' or 'R'. 
#grep function pattern matches 'H' in the given rows.  
mx1_prog[grep('H', mx1_prog$round), 2] <- 'H'
mx1_prog[grep('R', mx1_prog$round), 2] <- 'R'

#Next step is to reorder the levels of the factor so they appear on a plot in order from heat to final
mx1_prog$round <- ordered(mx1_prog$round, levels = c('H', 'R', 'Q4', 'Q3', 'Q2', 'Q1', 'SE/F/G 3', 'SE/F/G 2', 
                                                     'SE/F/G 1','SE/F 1','SE/F 2', 'SC/D 2', 'SC/D 1', 'SA/B 2', 'SA/B 1', 'FG', 
                                                     'FF', 'FE', 'FD', 'FC', 'FB', 'FA'))

#I then create a data frame that contains the team names of the teams that reached the A final in a given year. 
#This data frame will be used to subset the mx1_prog data set in a plot. 
teams2010 <- mx1_prog[mx1_prog$round == "FA" & mx1_prog$year == '2010',1]
teams2011 <- mx1_prog[mx1_prog$round == "FA" & mx1_prog$year == '2011',1]
teams2013 <- mx1_prog[mx1_prog$round == "FA" & mx1_prog$year == '2013',1]
teams2014 <- mx1_prog[mx1_prog$round == "FA" & mx1_prog$year == '2014',1]
teams2015 <- mx1_prog[mx1_prog$round == "FA" & mx1_prog$year == '2015',1]
teams2017 <- mx1_prog[mx1_prog$round == "FA" & mx1_prog$year == '2017',1]

#The teams20xx vector is then used in to specify which countries to follow through the rounds and the corresponding year is called in the filter function. Note: need to use teams20xx$team to get the vector 
#This gives us the A finalists data, which is then plotted by round and rank to show finishing position of each finalist through each round of the given competition
ggplot(filter(mx1_prog, team %in% teams2017$team & year == '2017'), 
       aes(x = round, y = rank_final, group = team, colour = team)) +
        geom_line(size = 1.5, alpha = 0.6) +
        geom_point()



####GIF####
#Test ben's idea of animating a race. Get the data first
fa_2017 <- filter(row_pred_data, year == '2017' & round == 'FA')

#Add a start distance to the data or the race would start at 500m in the plot
fa_2017[fa_2017$distance == '50',26] <- 0

#Reduce data to only the rows with split times
fa_2017 <- select(fa_2017, c(bow_name, distance, split)) %>%
        na.omit()

#Manually add rank - this could be automated but can't be bothered right now
fa_2017$rank <- as.factor(c(6,6,6,6,6,4,4,4,4,4,1,1,1,1,1,3,3,3,3,3,2,2,2,2,2,5,5,5,5,5))

#The plot. 
#The first 7 lines are just a normal static plot. The animation comes from the transition_reveal function from gganimate package
#This makes the data gradually appear based on a time dimension. The first argument is the 'id' argument which is the row 
#note the help file suggests the id is usually the same as the group asthetic for lines/points, this is the case for me. 
#The second argument is 'along', meaning my plot gradually reveals data according to the split times, giving it the appearance of the dots going faster or slower. 

race_sim_2017 <- ggplot(fa_2017, aes(x = distance, y = bow_name, group = bow_name, colour = rank)) +
        geom_point(size = 4) +
        geom_line(size = 4) +
        theme(legend.position="none") +
        scale_color_brewer(palette ="Greens", direction = -1) +
        labs(x = 'Distance' , y = '') +
        geom_vline(xintercept = 2000, linetype="dotted", size = 1) +
        transition_reveal(bow_name, split)

#The animate function lets you adjust width, height, duration etc. The gif made from this call is very jumpy (ie not smooth), in which case we can adjust the fps (frames per second). 
#This has a weird affect on the duration, so it was trial and error to make it work. 
animate(race_sim_2017, width = 1000, height = 500, duration = 15)

#gif animation that's a bit smoother
animate(race_sim_2017, width = 1000, height = 500, duration = 3, fps = 60)

#Saves the last created animation to working directory
anim_save('race_sim_2017_60fps.gif')
