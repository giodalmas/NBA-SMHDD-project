#===========================================================================================================================
# Load the dataset of the players
# The 2k player dataset
data_2k <- read.csv('C:\\Users\\Acer\\Desktop\\2_1\\Statistica\\Project_NBA\\2K20_ratings.csv')


# 2021 Player dataset
data_player_game <- read.csv('C:\\Users\\Acer\\Desktop\\2_1\\Statistica\\Project_NBA\\nba2021_per_game.csv')
data_player_adv <- read.csv('C:\\Users\\Acer\\Desktop\\2_1\\Statistica\\Project_NBA\\nba2021_advanced.csv')

#=======================================================================================
# Load the library for the select function
library(dplyr)
#=======================================================================================
data <- left_join(data_player_game, data_player_adv, by = "Player", suffix = c("", ".y")) %>%
  select(-ends_with(".y"))

data <- left_join(data, data_2k, by = "Player", suffix = c("", ".y")) %>%
  select(-ends_with(".y"))

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






rm(list = ls())
