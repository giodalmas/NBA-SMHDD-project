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

# Path dataset with group-by function
path <- read.csv('path_detail.csv')
path_def <- read.csv('path_detail_defended.csv')
path_open <- read.csv('path_detail_open.csv')
path_made <- read.csv('path_detail_made.csv')
path_miss <- read.csv('path_detail_miss.csv')

#---------------------------------------------------------------------------------------
# Check for the number of NA values 
colSums(is.na(path))
rowSums(is.na(path))

colSums(is.na(path_open))
rowSums(is.na(path_open))

colSums(is.na(path_def))
rowSums(is.na(path_def))

colSums(is.na(path_made))
rowSums(is.na(path_made))

colSums(is.na(path_miss))
rowSums(is.na(path_miss))
#---------------------------------------------------------------------------------------
# Notice that by columns, all the columns have a lot of NA values, however, if we look by
# the rows, there is a lot of rows that don't have NA values, hence, we will try to work
# with the rows instead of the columns.
#---------------------------------------------------------------------------------------
# Select the rows of path dataset that does not contain any NA values
path <- path[complete.cases(path),]
length(path)

path_made <- path_made[complete.cases(path_made),]
length(path_made)

path_miss <- path_miss[complete.cases(path_miss),]
length(path_miss)

path_open <- path_open[complete.cases(path_open),]
length(path_open)

path_def <- path_def[complete.cases(path_def),]
length(path_def)

#=======================================================================================
# Remove the redundant columns
path$Player <- paste(path$fnm, path$lnm, sep=' ')
path <- path %>% select(-c('pid', 'fnm', 'lnm', 'hght', 't', 'ddst'))

path_def$Player <- paste(path_def$fnm, path_def$lnm, sep=' ')
path_def <- path_def %>% select(-c('pid', 'fnm', 'lnm', 'hght', 't', 'ddst'))

path_open$Player <- paste(path_open$fnm, path_open$lnm, sep=' ')
path_open <- path_open %>% select(-c('pid', 'fnm', 'lnm', 'hght', 't', 'ddst'))

path_made$Player <- paste(path_made$fnm, path_made$lnm, sep=' ')
path_made <- path_made %>% select(-c('pid', 'fnm', 'lnm', 'hght', 't', 'ddst'))

path_miss$Player <- paste(path_miss$fnm, path_miss$lnm, sep=' ')
path_miss <- path_miss %>% select(-c('pid', 'fnm', 'lnm', 'hght', 't', 'ddst'))

#=======================================================================================
# Group different dataset by Player with mean statistics
#---------------------------------------------------------------------------------------
path = path %>% group_by(Player)%>%
  summarise(across(everything(), mean),
            .groups = 'drop')  %>%
  as.data.frame()

path_def = path_def %>% group_by(Player)%>%
  summarise(across(everything(), mean),
            .groups = 'drop')  %>%
  as.data.frame()

path_open = path_open %>% group_by(Player)%>%
  summarise(across(everything(), mean),
            .groups = 'drop')  %>%
  as.data.frame()

path_made = path_made %>% group_by(Player)%>%
  summarise(across(everything(), mean),
            .groups = 'drop')  %>%
  as.data.frame()

path_miss = path_miss %>% group_by(Player)%>%
  summarise(across(everything(), mean),
            .groups = 'drop')  %>%
  as.data.frame()

#======================================================================================
#--------------------------------------------------------------------------------------
# Merge the datasets
data <- left_join(data, path, by='Player')   # .x: Path
data <- left_join(data, path_def, by='Player')   # .y: Path_def
data <- left_join(data, path_open, by='Player')   # .x.x: Path_open
data <- left_join(data, path_made, by='Player')   # .y.y: Path_made
data <- left_join(data, path_miss, by='Player')   # : Path_miss
#======================================================================================

data <- data[complete.cases(data),]
rowSums(is.na(data))

# Get the names of the players in the dataset
player <- sort(data$Player) 

#======================================================================================
#--------------------------------------------------------------------------------------
# Turn Pos and Team into one hot encoding
# Load the package
library(mltools) #library for one_hot function
library(data.table)
#--------------------------------------------------------------------------------------

# put them as a factor (they where as characters)
data$TEAM <- as.factor(data$TEAM)
data$Pos <- as.factor(data$Pos)

# use one_hot function
data <- one_hot(as.data.table(data))



data$Player

View(data)


rm(list = ls())

