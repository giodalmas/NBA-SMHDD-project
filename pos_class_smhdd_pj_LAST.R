#===========================================================================================================================
library(data.table)
library(Matrix)
library(glmnet)


# Load the dataset of the players
# The 2k player dataset
data_2k <- read.csv('2K20_ratings.csv')

# 2021 Player dataset
data_player_game <- read.csv('nba2021_per_game.csv')
data_player_adv <- read.csv('nba2021_advanced.csv')

#---------------------------------------------------------------------------------------
# Merge the player data
library(dplyr)
data_pl <- left_join(data_player_game, data_player_adv, by = "Player", suffix = c("", ".y")) %>%
  select(-ends_with(".y"))

data_pl <- left_join(data_pl, data_2k, by = "Player", suffix = c("", ".y")) %>%
  select(-ends_with(".y"))
#---------------------------------------------------------------------------------------
# Change the name of the teams to their abbreviations(on data_team dataset)
# Change name of the column 'Tm' of data to 'TEAM'
colnames(data_pl)[4] <- 'TEAM'

#Extract team names
name_abb <- unique(data_pl$TEAM)
length(name_abb)
#---------------------------------------------------------------------------------------
# Notice that the length of name_abb is 31 which means that it includes one more category
# (TOT) which means the player plays for different teams in that season, we should remove
# the players with TOT.
#---------------------------------------------------------------------------------------
data_pl <- subset(data_pl, TEAM != 'TOT')

# Sort the team names of the general dataset (data dataset)
name_abb <- unique(data_pl$TEAM)   # update the name_abb
name_abb <- sort(name_abb)

#---------------------------------------------------------------------------------------

# Remove the redundant columns
data_pl <- data_pl[,!duplicated(names(data_pl))]
# Remove the redundant rows
data_pl <- data_pl[!duplicated(data_pl['Player']),]

#---------------------------------------------------------------------------------------
# Check the number of the NA on each row and column
# each column
colSums(is.na(data_pl))
# each row
rowSums(is.na(data_pl))
#---------------------------------------------------------------------------------------
# The columns related to the 2K dataset have a lot of NA values, and by row we see a lot 
# of rows with 36 NA values which is the number of features of 2K dataset, thus the reason
# might be some players are not listed in the 2K dataset, and we can check it by one of the 
# player.

#---------------------------------------------------------------------------------------

# Remove all the players that are not in the 2K dataset
data_2k_pl <- data_pl[rowSums(is.na(data_pl)) == 0, ]

# Check if there is NA values
colSums(is.na(data_2k_pl))

#=======================================================================================
# Add data on salary, height and weight

data_salary <- read.csv('nba2k-full_salary.csv')
#View(data_salary)

salary <- data_salary$salary  # Get the salary of the players

# remove the $ sign
salary = as.numeric(gsub("\\$", "", salary))

library(stringr)
weight <- str_split_fixed(data_salary$weight, "/", 2)[, 2] # Get the weight in kg
weight <- as.numeric(sub('kg.', '', weight))  # Remove kg. and change it to numeric values

height <- str_split_fixed(data_salary$height, "/", 2)[,2] #Get the height in cm
height <- as.numeric(height) #change it to numeric

#---------------------------------------------------------------------------------------
# Merge the dataset
Player <- data_salary[, 'full_name'] # Get the name of the players of salary dataset

# Create a new dataset for height, weight and salary
new_data <- data.frame(Player, height, weight, salary)

data_2k_pl_full <- left_join(data_2k_pl, new_data, by = 'Player')

data_2k_pl_full <- data_2k_pl_full[complete.cases(data_2k_pl_full),]   # Remove NA by rows

# Remove the redundant columns
data_2k_pl_full <- data_2k_pl_full[,!duplicated(names(data_2k_pl_full))]
# Remove the redundant rows
data_2k_pl_full <- data_2k_pl_full[!duplicated(data_2k_pl_full['Player']),]


#View(data_2k_pl_full)

# Create all_data with only removing categorical values and the salary
# Create all_data_num where we convert all integer values to numerical values 
# (we will need numerical type data for the function to balance data)

all_data <- select(data_2k_pl_full, -c(Player, TEAM, salary))
all_data_num <- select(all_data, -Pos)
#all_data_num <- lapply(all_data_num, as.numeric)

all_data_num[, 1:84] <- apply(all_data_num[, 1:84], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
str(all_data_num)
#=======================================================================================

# We check the number of players we have for each position
table(all_data$Pos)

# C = 19, PF = 18, PG = 19, SF = 8, SG = 13
# the classes are quite imbalanced
# class SF has only 8 obs while class SG has 13

# we oversample the under represented classes 

install.packages('imbalance')
library(imbalance)
library(dplyr)
library(ROSE)

# Balance the classes using RACOG to oversample the minority classes

all_data_num$Pos <-  all_data$Pos

# to create the subsets we are selecting each time a majority class (e.g. 'C')
# and one of the classes with fewer observations (SF' and 'SG'  in this case)

#=======================================================================================
# RACOG works for discrete attributes. 
# It generates new examples with respect to an approximated distribution using a Gibbs Sampler scheme

# subset with players on position 'C' and 'SF'
SF_C <- rbind(all_data_num[all_data_num$Pos == 'C',], all_data_num[all_data_num$Pos == 'SF',])

# We generate 10 new examples for SF
# SF position
set.seed(123)
racog_C_SF <- racog(SF_C, numInstances = 30, classAttr = 'Pos')

SF <- rbind(all_data_num[all_data_num$Pos == 'SF',], racog_C_SF)

#---------------------------------------------------------------------------------------
# We do the same for the SG position
# SG position

SG_C <- rbind(all_data_num[all_data_num$Pos == 'C',], all_data_num[all_data_num$Pos == 'SG',])

# We generate 26 new examples for SG (13 is the starting number of obseravtions we had)
set.seed(123)
racog_C_SG <- racog(SG_C, numInstances = 26, classAttr = 'Pos')

SG <- rbind(racog_C_SG, all_data_num[all_data_num$Pos == 'SG',])

#---------------------------------------------------------------------------------------
# Since overall also the other positions do not have a satisfying amount of observations 
# We generate some more for them as well and see how cross validation perform

# PG position

PG_SG <- rbind(all_data_num[all_data_num$Pos == 'PG',], SG)

# We generate 21 new examples for PG (19 is the starting number of obseravtions we had)
set.seed(123)
racog_SG_PG <- racog(PG_SG, numInstances = 21, classAttr = 'Pos')

PG <- rbind(racog_SG_PG, all_data_num[all_data_num$Pos == 'PG',])

#---------------------------------------------------------------------------------------
# PF position

PF_SG <- rbind(all_data_num[all_data_num$Pos == 'PF',], SG)

# We generate 21 new examples for PF (18 is the starting number of obseravtions we had)
set.seed(123)
racog_SG_PF <- racog(PF_SG, numInstances = 21, classAttr = 'Pos')

PF <- rbind(racog_SG_PF, all_data_num[all_data_num$Pos == 'PF',])

#---------------------------------------------------------------------------------------
# C position

C_SG <- rbind(all_data_num[all_data_num$Pos == 'C',], SG)

# We generate 21 new examples for SG (19 is the starting number of obseravtions we had)
set.seed(123)
racog_SG_C <- racog(C_SG, numInstances = 21, classAttr = 'Pos')

C <- rbind(racog_SG_C, all_data_num[all_data_num$Pos == 'C',])

# Let's add the examples created for the two positions to our dataset
data_balanced <- rbind(SF, SG, PG, PF, C)

# This is how the dataset looks now
table(data_balanced$Pos)

# C = 19, PF = 18, PG = 19, SF = 18, SG = 19
# way better
dim(data_balanced)

#=======================================================================================
# Train-Test split
# We can proceed now with the split in train and test set
set.seed(123)

train_idx <- sample(1:nrow(data_balanced), size = 0.7 * nrow(data_balanced))

train_data <- data_balanced[train_idx, ]
test_data <- data_balanced[-train_idx, ]

# Create X_tr by removing response variable (Pos)
# y_tr response variable


X_tr <- select(train_data, -Pos)
y_tr <- train_data$Pos

X_test <- test_data
y_test <- test_data$Pos

#=======================================================================================
# MULTICLASS CLASSIFICATION
#=======================================================================================
# MULTINOMIAL LOGISTIC REGRESSION
#Classify players position based on stats and nba 2k attributes

library(glmnet)

# get lambda sequence
grid = 10^seq(1,-10,length=50) 

# Fit multinomial log reg model
fit <- glmnet(data.matrix(X_tr), y_tr, family = "multinomial", nlambda = 50)

# Plot of each coefficient for each class
par(mfrow=c(2,3));plot(fit,xvar="dev", label = TRUE) 

set.seed(123)

cv.fit <- cv.glmnet(data.matrix(X_tr), y_tr, family= 'multinomial', type = 'mse', lambda = grid,  nfolds = 5)

# Plot the mean sq error for the cross validated fit as a function
par(mfrow = c(1, 1))
plot(cv.fit)	
coef(cv.fit)

cv.fit$lambda.min
cv.fit$lambda.1se

#c(lambda.min = log(cv.fit$lambda.min), lambda.1se = log(cv.fit$lambda.1se))

plot(log(cv.fit$lambda), cv.fit$cvm)
plot(cv.fit)
coef(cv.fit, c(cv.fit$lambda.min, cv.fit$lambda.1se))

# Let’s take a look at the non-zero coefficients in the model for lambda min
temp <- coef(cv.fit, s = cv.fit$lambda.min)
beta <- Reduce(cbind, temp)
beta <- beta[apply(beta != 0, 1, any),]
colnames(beta) <- names(temp)
beta

#non.zero coeff for lambda 1se
temp <- coef(cv.fit, s = cv.fit$lambda.1se)
beta <- Reduce(cbind, temp)
beta <- beta[apply(beta != 0, 1, any),]
colnames(beta) <- names(temp)
beta

##### TO CHECK

# Make predictions on the test set 
pred = predict(cv.fit, newx= data.matrix(X_test), type="response")
mean(pred!= y_test)
####
predictions <- predict(fit, X_test$Pos)

# Calculate the accuracy
accuracy <- mean(predictions == y_te)

# Plot the accuracy
plot(fit, xvar = "lambda", label = TRUE)
abline(h = accuracy, col = "red")
par(mfrow= c(2, 3), mar = c(1, 1, 1, 1))

# Extract the coefficients from the model fit
coefs <- coef(fit)

# Extract the names of the features
feature_names <- colnames(X_tr)

# Plot the coefficients as a function of lambda
plot(fit, xvar = "lambda", label = TRUE)

# Add a legend with the feature names
legend("topright", legend = paste(1:length(feature_names), feature_names, sep = " - "), 
       col = 1:length(feature_names), lty = 1)

text(x = 1:length(feature_names), y = rep(1, length(feature_names)), 
     labels = 1:length(feature_names), cex = 0.8, font = 2)



#convert the coeff to a matrix and then to a data frame 
#to pass it as data argument in the ggplot function
matrix_coef = as.matrix(coef(fit, complete = TRUE))
df_coef <- as.data.frame(matrix_coef)

lambda <- fit$lambda #vector of lambdas

ggplot(df_coef, aes(y = lambda, x = 'value', color = 'variable'))
+geom_line() + 
  facet_wrap(~ variable, scales= "free_y")

plot_ly(data = df_coef, x = ~lambda, y = ~value, color = ~variable) %>%
  add_markers()


