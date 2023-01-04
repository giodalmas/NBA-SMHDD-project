#===========================================================================================================================
# Load the dataset of the players
# The 2k player dataset
data_2k <- read.csv('2K20_ratings.csv')

# 2021 Player dataset
data_player_game <- read.csv('nba2021_per_game.csv')
data_player_adv <- read.csv('nba2021_advanced.csv')

#=======================================================================================

# Load the team data
data_team <- read.csv('nba_team_stats_00_to_21.csv')
data_team_playoff <- read.csv('nba_team_stats_playoffs_00_to_21.csv')

# Extract the data of season 20/21
data_team <- data_team[data_team$SEASON == '2020-21', ]
data_team_playoff <- data_team_playoff[data_team_playoff$season == '2020-21', ]

# Load the library for the select function
library(dplyr)
########################################################################################
########################################################################################
#=======================================================================================
# Merge the player data
data <- left_join(data_player_game, data_player_adv, by = "Player", suffix = c("", ".y")) %>%
  select(-ends_with(".y"))

data <- left_join(data, data_2k, by = "Player", suffix = c("", ".y")) %>%
  select(-ends_with(".y"))

#=======================================================================================
# Merge the player and team dataset by the team names
#---------------------------------------------------------------------------------------
# Change the name of the teams to their abbreviations(on data_team dataset)
# Change name of the column 'Tm' of data to 'TEAM'
colnames(data)[4] <- 'TEAM'

# Order the data_team dataset by team names
data_team <- data_team %>% arrange(TEAM)

# Extract all the team names of both dataset
team_name <- data_team['TEAM']
name_abb <- unique(data$TEAM)
length(name_abb)
#---------------------------------------------------------------------------------------
# Notice that the length of name_abb is 31 which means that it includes one more category
# (TOT) which means the player plays for different teams in that season, we should remove
# the players with TOT.
#---------------------------------------------------------------------------------------
data <- subset(data, TEAM != 'TOT')

# Sort the team names of the general dataset (data dataset)
name_abb <- unique(data$TEAM)   # update the name_abb
name_abb <- sort(name_abb)

#---------------------------------------------------------------------------------------
# Notice that the listed names have the correct order of the full and their abbreviations 
# except the Charlotte Hornets and Chicago Bulls, thus we have to swap them (team_name).
#---------------------------------------------------------------------------------------
cho <- data_team[4, ]    # 4th row of data_team is Charlotte, should be Chicago 
data_team[4, ] <- data_team[5, ]
data_team[5, ] <- cho

data_team  # check again the names

# Assign the names of teams of team dataset into its abbreviations
data_team$TEAM <- name_abb
data_team$TEAM

#=======================================================================================
# MERGE TEAM DATASET(DATA_TEAM) WITH PLAYER DATASET(DATA)
#---------------------------------------------------------------------------------------
# remove the column of teamstatspk of data_team
data_team <- select(data_team, -c(teamstatspk, SEASON))
data_team
#---------------------------------------------------------------------------------------
# Merge two dataset(data and data_team)
data <- left_join(data, data_team, by = "TEAM")  # .x: Player data
# .y: Team data

########################################################################################
########################################################################################
#=======================================================================================
# Remove the redundant columns
data <- data[,!duplicated(names(data))]
# Remove the redundant rows
data <- data[!duplicated(data['Player']),]

#=======================================================================================
# Check the number of the NA on each row and column
# each column
colSums(is.na(data))
# each row
rowSums(is.na(data))
#---------------------------------------------------------------------------------------
# The columns related to the 2K dataset have a lot of NA values, and by row we see a lot 
# of rows with 36 NA values which is the number of features of 2K dataset, thus the reason
# might be some players are not listed in the 2K dataset, and we can check it by one of the 
# player.

# EX. Check if Jaylen Adams present in the2K dataset 
jaylen <- data_2k[data_2k$Player == 'Jaylen Adams']
jaylen
# We can see that jaylen is not present in the dataset
#=======================================================================================

# Remove all the players that are not in the 2K dataset
data <- data[rowSums(is.na(data)) == 0, ]

# Check if there is NA values
colSums(is.na(data))


########################################################################################
########################################################################################
#=======================================================================================
# PLAYER_METRIC dataset
#player_metrics <- read.csv('player_metrics.csv')
#---------------------------------------------------------------------------------------

# Combine last and first name as Player and remove the useless columns
#player_metrics$Player <- paste(player_metrics$fnm, player_metrics$lnm, sep=' ')
#player_metrics <- player_metrics %>% select(-c('pid', 'fnm', 'lnm'))

# Merge the metric dataset and the player dataset
#data <- left_join(data, player_metrics, by='Player')

# Check if there is NA values
#colSums(is.na(data))
#rowSums(is.na(data))
#--------------------------------------------------------------------------------------
# The player metrics has too many NA values
########################################################################################
########################################################################################
#=======================================================================================




data_salary <- read.csv('nba2k-full.csv')
View(data_salary)

salary <- data_salary$salary  # Get the salary of the players

# remove the $ sign
salary = as.numeric(gsub("\\$", "", salary))

library(stringr)
weight <- str_split_fixed(data_salary$weight, "/", 2)[, 2] # Get the weight in kg
weight <- as.numeric(sub('kg.', '', weight))  # Remove kg. and change it to numeric values

#========================================================================================
# Merge the dataset
Player <- data_salary[, 'full_name'] # Get the name of the players of salary dataset

# Create a new dataset for weight and salary
new_data <- data.frame(Player, weight, salary)

data <- left_join(data, new_data, by = 'Player')

data <- data[complete.cases(data),]   # Remove NA by rows

write.csv(data, "project_data_with_salary.csv", row.names=FALSE)  # save the data to file




rm(list = ls())
