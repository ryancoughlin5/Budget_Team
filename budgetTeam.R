# SOURCE: http://www.seanlahman.com/baseball-archive/statistics/
rm(list = ls())

library(ggplot2)
library(scales)
library(tidyverse)

# read in the different datasets
batting <- read.csv("mlb_Batting.csv", stringsAsFactors = FALSE)
fielding <- read.csv("mlb_Fielding.csv", stringsAsFactors = FALSE)
players <- read.csv("mlb_Players.csv", stringsAsFactors = FALSE)
salaries <- read.csv("mlb_Salaries.csv", stringsAsFactors = FALSE)
teams <- read.csv("mlb_Teams.csv", stringsAsFactors = FALSE)
pitching <- read.csv("mlb_Pitching.csv", stringsAsFactors = FALSE)

# only keep the instances where yearID is 2016
batting <- subset(batting, yearID == 2016)
fielding <- subset(fielding, yearID == 2016)
salaries <- subset(salaries, yearID == 2016)
teams <- subset(teams, yearID == 2016)
pitching <- subset(pitching, yearID == 2016)

#get rid of year, team, and league IDs
batting$yearID <- NULL
batting$teamID <- NULL
batting$lgID <- NULL

fielding$yearID <- NULL
fielding$teamID <- NULL
fielding$lgID <- NULL

salaries$yearID <- NULL

teams$yearID <- NULL

pitching$yearID <- NULL
pitching$teamID <- NULL
pitching$lgID <- NULL



#### Aggregate Data ####

# aggregate batting data frame
batting <- aggregate(. ~ playerID, batting, sum)
batting$`1B` <- batting$H-(batting$X2B+batting$X3B+batting$HR)
batting$TB <- batting$`1B`+(batting$X2B*2)+(batting$X3B*3)+(batting$HR*4)

# create a temporary dataframe containing the positions so I can aggregate the fielding data frame
temp <- fielding[c(1, 3, 6)]
fielding$POS <- NULL

# change NA to zero, so there aren't any NAs in the dataframe (all catching stats)
fielding$PB <- ifelse(is.na(fielding$PB), 0, fielding$PB)
fielding$WP <- ifelse(is.na(fielding$WP), 0, fielding$WP)
fielding$SB <- ifelse(is.na(fielding$SB), 0, fielding$SB)
fielding$CS <- ifelse(is.na(fielding$CS), 0, fielding$CS)

# remove ZR because they are all NA
sum(is.na(fielding$ZR))
fielding$ZR <- NULL

#aggregate and merge POS back into fielding

fielding <- aggregate(. ~ playerID, fielding, sum)

temp <- arrange(temp, temp$playerID, desc(temp$InnOuts))
temp <- temp[!duplicated(temp$playerID), ]

fielding <- left_join(x = fielding, y = temp, by = 'playerID')
fielding <- fielding[c(1, 14, 2:13)]
colnames(fielding)[6] <- 'InnOuts'
rm(temp)

# aggregate pitching
pitching <- aggregate(. ~ playerID, pitching, sum)

# make first name and last name into one columns
players$Name <- paste(players$nameFirst, players$nameLast)

# all of these guys are DH with < 85 games
b1 <- anti_join(batting, fielding[, c('playerID', 'POS')], by = 'playerID')
b1 <- left_join(b1, players, by = 'playerID')
rm(b1)
batting <- left_join(batting, fielding[, c('playerID', 'POS')], by = 'playerID')
batting <- batting[complete.cases(batting), ]

salaries <- left_join(salaries, fielding[, c('playerID', 'POS')], by = 'playerID')
players <- left_join(players, salaries[, c('playerID', 'POS')], by = 'playerID')
players <- players[complete.cases(players$POS), ]




# get rid of the pitchers and make the games played the same number see if these dataframes have all the same players
fielding$G <- batting$G
batting <- subset(batting, POS != 'P' & G > 80)
fielding <- subset(fielding, POS != 'P' & G > 80)
identical(batting[['playerID']], fielding[['playerID']])

# merge players and pitching and only keep the columns that were originally in players
# doing this to make sure the number of players matches up with the number of salaries
players <- left_join(x = players, y = salaries, by = 'playerID')
players <- players[complete.cases(players$salary), ]
colnames(players)[26] <- 'POS'
players_pitch <- subset(players, POS == 'P')
players_nonpitch <- subset(players, POS != 'P')

# join the batting and fielding data frames with the non pitcher dataframe; keeping only the matching entries
batting <- semi_join(batting, players_nonpitch[, c('playerID', 'salary', 'POS')], by = 'playerID')
fielding <- semi_join(fielding, players_nonpitch[, c('playerID', 'salary', 'POS')], by = 'playerID')
identical(batting[['playerID']], fielding[['playerID']])

# join the pitching data frames with the  pitcher dataframe; keeping only the matching entries
pitching <- semi_join(pitching, players_pitch[, c('playerID', 'salary')], by = 'playerID')

# for loop to create the Innings Pitched per Start variable
pitching$IP_Start <- NA
for (i in 1:nrow(pitching)) {
  if (pitching$GS[i] >= 3) {
    pitching$IP_Start[i] <- (pitching$IPouts[i]/3)/pitching$GS[i]
  }else {is.na(pitching$IP_Start[i])
  } 
}

# see what the average innings pitched is for starters
mean(pitching$IP_Start, na.rm = TRUE)

# turn P into RP for a reliever, SP for a starter, or NA if it doesn't match the criteria
pitching$POS <- NA
for (i in 1:nrow(pitching)) {
  if ((pitching$GS[i] <= 3) && (pitching$G[i] >= 25)) {
    pitching$POS[i] <- 'RP'
  }else if (pitching$GS[i] > 14 & pitching$G[i] > 14) {
    pitching$POS[i] <- 'SP'
  }else {pitching$POS[i] <- NA}
}

# get rid of any NA in POS
pitching <- pitching[complete.cases(pitching$POS), ]

# join the batting and pitching data frames with players
players <- left_join(x = players, y = pitching, by = 'playerID')
players <- left_join(x = players, y = batting, by = 'playerID')

# combine the positions of the position players with the positions of the pitchers into one column
players$POS_Combined <- NA
for (i in 1:nrow(players)) {
  if (!is.na(players$POS.y.y[i])) {
    players$POS_Combined[i] <- players$POS.y.y[i]
  }
  if (!is.na(players$POS[i])) {
    players$POS_Combined[i] <- players$POS[i]
  }
}

# get rid of any NA in the combined column
players <- players[complete.cases(players$POS_Combined), ]

# reorder/remove unneccessary columns
players <- players[c(1, 25, 80, 19, 20, 29)]
batting <- batting[c(1, 22, 4, 6, 13, 16, 21, 18)]
fielding <- fielding[c(1, 2, 7, 8, 6)]
pitching <- pitching[c(1, 29, 14, 11, 10)]

# rename POS_Combined to POS
colnames(players)[3] <- 'POS'

# OPS formula
batting$OPS <- NA
batting$OPS <- round(((batting$H+batting$BB+batting$HBP)/(batting$AB+batting$BB+batting$SF+batting$HBP) + batting$TB/batting$AB),
                     3)

# range factor formula
fielding$RF <- NA
fielding$RF <- round(((fielding$PO+fielding$A)*9)/(fielding$InnOuts/3), 3)

# WHIP formula
pitching$WHIP <- NA
pitching$WHIP <- round((pitching$BB+pitching$H)/(pitching$IPouts/3), 3)

# join remaining data frames and keep only the specified column from y
players <- left_join(players, batting[c('playerID', 'OPS')], by = 'playerID')
players <- left_join(players, fielding[c('playerID', 'RF')], by = 'playerID')                  
players <- left_join(players, pitching[c('playerID', 'WHIP')], by = 'playerID')

players <- players %>%
  arrange(POS)

write.csv(players, 'budget_team.csv')

qplot(players$OPS, players$salary, geom = 'point') + scale_y_continuous(labels = dollar)
qplot(players$RF, players$salary, geom = 'point') + scale_y_continuous(labels = dollar)
qplot(players$WHIP, players$salary, geom = 'point') + scale_y_continuous(labels = dollar)
