library(dplyr)
library(glmnet)

#load the datasets
#final_sm8.csv
data_player_game <- read.csv('nba2021_per_game.csv')
path <- read.csv('final_sm8.csv')
path_coor <- read.csv('final_coor.csv')
path_vel <- read.csv('final_vel.csv')
path_acc <- read.csv('final_acc.csv')
path_av <- read.csv('final_va.csv')

#THERE ARE 83 PLAYERS THAT WE HAVE THE SEASON STATS AND THE SHOOTING MOTION

#clean the path rows with NA
path <- na.omit(path)
path_coor <- na.omit(path_coor)
path_vel <- na.omit(path_vel)
path_acc <- na.omit(path_acc)
path_av <- na.omit(path_av)

#drop useless columns ('X', 'index', 'index.1', 'pid')
path <- path %>% select(-c('X', 'index', 'index.1', 'pid'))
path_coor <- path_coor %>% select(-c('X', 'index', 'pid'))
path_vel <- path_vel %>% select(-c('X', 'index', 'pid'))
path_acc <- path_acc %>% select(-c('X', 'index', 'pid'))
path_av <- path_av %>% select(-c('X', 'index', 'pid'))

#just take the response X3P.
data_player_game <- select(data_player_game, c('Player','X3P.'))
colnames(data_player_game)[1] = "player"
#merge with season data

data <- left_join(data_player_game,path, by='player')   
data_coor <- left_join(data_player_game,path_coor, by='player')
data_vel <- left_join(data_player_game,path_vel, by='player')
data_acc <- left_join(data_player_game,path_acc, by='player')
data_av <- left_join(data_player_game,path_av, by='player')

#clean the new dataset
data <- na.omit(data)
data_coor <- na.omit(data_coor)
data_vel <- na.omit(data_vel)
data_acc <- na.omit(data_acc)
data_av <- na.omit(data_av)

#drop the 12 and 260,126 rows, for being an outlier
data <- data[!(row.names(data) %in% c("12","260", "126")),]
data_coor <- data_coor[!(row.names(data_coor) %in% c("12","260", "126")),]
data_vel <- data_vel[!(row.names(data_vel) %in% c("12","260", "126")),]
data_acc <- data_acc[!(row.names(data_acc) %in% c("12","260", "126")),]
data_av <- data_av[!(row.names(data_av) %in% c("12","260", "126")),]


#turn rt to positive
data$rt <- -1 * data$rt
data_coor$rt <- -1 * data_coor$rt
data_vel$rt <- -1 * data_vel$rt
data_acc$rt <- -1 * data_acc$rt
data_av$rt <- -1 * data_av$rt
#===============================================================================
#LASSSO REGRESSION
trainx <- select(data, -c('player', 'X3P.'))
trainx_coor <- select(data_coor, -c('player', 'X3P.'))
trainx_vel <- select(data_vel, -c('player', 'X3P.'))
trainx_acc <- select(data_acc, -c('player', 'X3P.'))
trainx_av <- select(data_av, -c('player', 'X3P.'))

library(glmnet)

grid = 10^seq(1,-10,length=50) ##get lambda sequence

fit = glmnet(data.matrix(trainx),data$X3P.*100, nlambda = 200)#, lambda = grid)#, nlambda = 500)	# fit the model
fit_coor = glmnet(data.matrix(trainx_coor),data_coor$X3P.*100, nlambda = 200)
fit_vel = glmnet(data.matrix(trainx_vel),data_vel$X3P.*100, nlambda = 200)
fit_acc = glmnet(data.matrix(trainx_acc),data_acc$X3P.*100, nlambda = 200)
fit_av = glmnet(data.matrix(trainx_av),data_av$X3P.*100, nlambda = 200)

plot(fit,label = TRUE) # Plot the paths for the fit
plot(fit,xvar="lambda", main="") 
plot(fit,xvar="dev", main="") 

par(mfrow=c(2,2)) 
plot(fit_coor,label = TRUE, main = "coordinates")
plot(fit_vel,label = TRUE, main = "velocities")
plot(fit_acc,label = TRUE, main = "accelerations")
plot(fit_av,label = TRUE, main = "velocity and acceleration")

par(mfrow=c(1,1)) 


fit		    # look at the fit for each lambda


cv.fit <- cv.glmnet(data.matrix(trainx), data$X3P.*100, lambda = grid, nfolds = 20)	# Perform cross validation on the fitted model

plot(cv.fit)	# Plot the mean sq error for the cross validated fit as a function
  
coef(cv.fit)[coef(cv.fit)!=0]

cv.fit$lambda.min
cv.fit$lambda.1se

plot(log(cv.fit$lambda), cv.fit$cvm)
plot(cv.fit)
coef(cv.fit, c(cv.fit$lambda.min, cv.fit$lambda.1se))

print(paste(cv.fit$lambda.min, log(cv.fit$lambda.min)))
print(paste(cv.fit$lambda.1se, log(cv.fit$lambda.1se)))
#===============================================================================
#RIDGE REGRESSION
lambdas <- 10^seq(9, -2, by = -.1)

fit_coor_ridge = glmnet(data.matrix(trainx_coor),data_coor$X3P.*100, alpha = 0, lambda = lambdas)
fit_vel_ridge  = glmnet(data.matrix(trainx_vel),data_vel$X3P.*100, alpha = 0, lambda = lambdas)
fit_acc_ridge  = glmnet(data.matrix(trainx_acc),data_acc$X3P.*100, alpha = 0, lambda = lambdas)
fit_av_ridge  = glmnet(data.matrix(trainx_av),data_av$X3P.*100, alpha = 0, lambda = lambdas) 
fit_ridge  <- glmnet(data.matrix(trainx), data$X3P.*100, alpha = 0, lambda = lambdas)

plot(fit_ridge ,label = TRUE)

par(mfrow=c(2,2)) 
plot(fit_coor_ridge ,label = TRUE, main = "coordinates")
plot(fit_vel_ridge ,label = TRUE, main = "velocities")
plot(fit_acc_ridge ,label = TRUE, main = "accelerations")
plot(fit_av_ridge ,label = TRUE, main = "velocity and acceleration")

par(mfrow=c(1,1)) 

cv_fit_ridge  <- cv.glmnet(data.matrix(trainx), data$X3P.*100, alpha = 0, lambda = lambdas)
plot(cv_fit_ridge)

#===============================================================================
#GROUP LASSO USING EACH VARIABLE AS DIFFERENT GROUP
#create the groups
library(gglasso)

groups = c(1,2)
for (i in 1:100) {
  for (j in 3:17){
    groups <- append(groups,j)
  }
}

fit_group = gglasso(data.matrix(trainx), data$X3P.*100,group = groups)

plot(fit_gorup, label = TRUE)

gr_cv = cv.gglasso(x=data.matrix(trainx), y=data$X3P.*100, group=groups, nfolds=5)
plot(gr_cv)
#===============================================================================
#GROUP LASSO USING EACH TIME AS DIFFERENT GROUP

#create the groups
groups = c(1,1)
for (i in 1:100) {
  for (j in 1:15){
    groups <- append(groups,i)
  }
}

fit_group = gglasso(data.matrix(trainx), data$X3P.*100,group = groups)

plot(fit_gorup, label = TRUE)

gr_cv = cv.gglasso(x=data.matrix(trainx), y=data$X3P.*100, group=groups, nfolds=5)
plot(gr_cv)

#===============================================================================
best_ridge_coef <- as.numeric(coef(cv_fit_ridge, s = cv_fit_ridge$lambda.min))[-1]

## Perform adaptive LASSO with 10-fold CV
alasso_cv = cv.glmnet(x = data.matrix(trainx), y =data$X3P.*100, type.measure = "mse", nfold = 10,alpha = 1, penalty.factor = 1 / abs(best_ridge_coef),keep = TRUE)
## Penalty vs CV MSE plot
plot(alasso_cv)

alasso_cv$lambda.min
coef_adp <- coef(alasso_cv, s = alasso_cv$lambda.min)
coef_adp
