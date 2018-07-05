# SOURCE: http://www.seanlahman.com/baseball-archive/statistics/

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

# make first name and last name into one columns
players$Name <- paste(players$nameFirst, players$nameLast)


# remove columns
players <- players[c(1, 25, 19, 20)]


