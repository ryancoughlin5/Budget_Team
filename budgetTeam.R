# SOURCE: http://www.seanlahman.com/baseball-archive/statistics/
rm(list = ls())

library(ggplot2)

# read in the different datasets
batting <- read.csv("mlb_Batting.csv", stringsAsFactors = FALSE)
fielding <- read.csv("mlb_Fielding.csv", stringsAsFactors = FALSE)
players <- read.csv("mlb_Players.csv", stringsAsFactors = FALSE)
salaries <- read.csv("mlb_Salaries.csv", stringsAsFactors = FALSE)
teams <- read.csv("mlb_Teams.csv", stringsAsFactors = FALSE)
pitching <- read.csv("mlb_Pitching.csv", stringsAsFactors = FALSE)

batting <- merge(x = batting, y = fielding[, c('playerID', 'POS')], by = 'playerID', all.x = TRUE)
salaries <- merge(x = salaries, y = fielding[, c('playerID', 'POS')], by = 'playerID', all.x = TRUE)
players <- merge(x = batting, y = fielding[, c('playerID', 'POS')], by = 'playerID', all.x = TRUE)

# only keep the instances where yearID is 2016
batting <- subset(batting, yearID == 2016 & batting$POS != 'P')
fielding <- subset(fielding, yearID == 2016 & fielding$POS != 'P')
salaries <- subset(salaries, yearID == 2016)
teams <- subset(teams, yearID == 2016)
pitching <- subset(pitching, yearID == 2016)

#get rid of year, team, and league IDs
batting$yearID <- NULL
fielding$yearID <- NULL
salaries$yearID <- NULL
teams$yearID <- NULL
pitching$yearID <- NULL
pitching$teamID <- NULL
pitching$lgID <- NULL
batting$teamID <- NULL
batting$lgID <- NULL
fielding$teamID <- NULL
fielding$lgID <- NULL


#### Aggregate Data ####

# aggregate batting data frame
batting <- aggregate(. ~ playerID, batting, sum)

# create a temporary dataframe containing the positions so I can aggregate the fielding data frame
temp <- fielding[c(1, 3)]
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
fielding <- merge(x = fielding, y = temp, by = 'playerID')
fielding <- fielding[c(1, 14, 2:13)]

# aggrgate pitching and average ERA and Opponents BA bc they were avaerages to begin with
pitching <- aggregate(. ~ playerID, pitching, sum)
pitching$ERA <- pitching$ERA/2
pitching$BAOpp <- pitching$BAOpp/2

# merge players and pitching and only keep the columns that were originally in players
# doing this to make sure the number of players matches up with the number of salaries
players <- merge(x = players, y = salaries, by = 'playerID')
players <- players[c(1, 25, 2:24)]
batting <- merge(x = batting, y = salaries, by = 'playerID')
batting <- 
fielding <- merge(x = fielding, y = salaries, by = 'playerID')


# make first name and last name into one columns
players$Name <- paste(players$nameFirst, players$nameLast)

# remove/reorder columns
players <- players[c(1, 2, 26, 20, 21)]

batting <- batting[c(1, )]




