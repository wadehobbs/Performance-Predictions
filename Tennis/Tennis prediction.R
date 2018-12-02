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
library(transformr)
library(gganimate)

#Fixed the data in excel - each row was a match with both players, and all the stats. Need a row per athlete with corresponding stats
#Im sure there is a way to do this in R but in the interests of time I just did some copy/paste in excel 
#Import
ausopen_men_2013 <- read_csv("AusOpen-men-2013.csv")
#Now each row contains an athlete, so a match is spread across two rows - one per athlete. 
#The athlete that wins has a 1 in thew Result col, the loser has a 0. 


#Add code to test git

and again