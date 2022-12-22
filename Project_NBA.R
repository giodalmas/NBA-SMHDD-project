#===========================================================================================================================
# Load the dataset of the player, shooting motion and the teams
# The general info about the player
data_player_gen <- read.csv('C:\\Users\\Acer\\Desktop\\2_1\\Statistica\\Project_NBA\\nba_2016_2017_100.csv')
data_player_sal <- read.csv('C:\\Users\\Acer\\Desktop\\2_1\\Statistica\\Project_NBA\\nba_2017_players_with_salary_wiki_twitter.csv')


# Merge the info about the players
# --------------------------------------------------------------------------------------
# Load the library for the select function
library(dplyr)
# --------------------------------------------------------------------------------------
# Remove the player_ID from the player_gen dataset
data_player_gen <- data_player_gen %>% select(-'ï..PLAYER_ID')

# Remove the X and Rk from the player_sal dataset
data_player_sal <- data_player_sal %>% select(-c('X', 'Rk'))
# Merge the dataset about the players
data_player <- cbind(data_player_gen, data_player_sal, )


# The general info about the teams with elo and cluster(By K_mean)
data_team_gen <- read.csv('C:\\Users\\Acer\\Desktop\\2_1\\Statistica\\Project_NBA\\nba_2017_att_val_elo_with_cluster.csv')
# Get the team evaluation (Million dollars)
data_team_eva <- read.csv('C:\\Users\\Acer\\Desktop\\2_1\\Statistica\\Project_NBA\\nba_2017_team_valuations.csv')

# Merge the dataset of the team
data_team <- merge(data_team_gen, data_team_eva, all = FALSE)

# Check the dimension before and after the merging procedure
dim(data_team_gen)
dim(data_team)
dim(data_team_eva)
str(data_player_gen)

data_player_gen['ELO']

rm(list=ls())
