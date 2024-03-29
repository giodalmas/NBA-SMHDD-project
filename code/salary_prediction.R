#========================================================================================
library(dplyr)
library(glmnet)   # load the library
library(caTools)
library(caret)

data <- read.csv('data_final.csv')
data <- data %>% relocate(weight, Age)  # move weight to the first column


data <- select(data, -Pos_SF.PF)
# Get ind. and response variables
X <- select(data, -c(Player, salary))
Y <- select(data, salary)
Y <- Y/1000000   # get the response in million
data_net <- select(data, -Player)  # dataset for elastic net
data_net$salary <- data_net$salary/1000000

# train test split
set.seed(1)   # set the seed
sample <- sample.split(data$Pos_C, SplitRatio = 0.7)
X_train  <- X[sample, ] %>% as.matrix()
X_test   <- X[!sample, ] %>% as.matrix()
Y_train <- Y[sample, ] %>% as.matrix()
Y_test <- Y[!sample, ] %>% as.matrix()
X_train_net <- data_net[sample,] %>% as.matrix()
X_test_net <- data_net[!sample, ] %>% as.matrix()

########################################################################################
########################################################################################
# find the best model (with cross-validation)
lasso_fit <- cv.glmnet(as.matrix(X_train), as.matrix(Y_train), alpha=1)  # k-fold is selected randomly
plot(lasso_fit)
str(lasso_fit)
lambda_lasso <- lasso_fit$lambda.min

coef_lasso <- coef(lasso_fit, s='lambda.min')   # coefficient selected by lasso
coef_lasso



model_lasso <- glmnet(X_train, Y_train, lambda = lambda_lasso, alpha=1)
pred_lasso <- predict(model_lasso, as.matrix(X_test), alpha=1)




# mse
mse_lasso <- round(mean((pred_lasso - Y_test)^2), 2)


#=====================================================================================
# Ridge regression
cv_ridge_default <- cv.glmnet(X_train, Y_train, alpha = 0)
plot(cv_ridge_default)    # the default sequence of lambda is at the edge of the plot
lambda_ridge_default <- cv_ridge_default$lambda.min
i <- which(cv_ridge_default$lambda == cv_ridge_default$lambda.min)
mse_min_default <- cv_ridge_default$cvm[i]

grid = 10^seq(10,-2,length=1000) ##get lambda sequence
cv_ridge_grid <- cv.glmnet(X_train, Y_train, alpha = 0, lambda = grid)
lambda_ridge_grid <- cv_ridge_grid$lambda.min   # we can get a smaller lambda
plot(cv_ridge_grid)

j <- which(cv_ridge_grid$lambda == cv_ridge_grid$lambda.min)
mse_min_grid <- cv_ridge_grid$cvm[j]
lambda_ridge_grid <- cv_ridge_grid$lambda.min
abline(h=mse_min_default, v=log(lambda_ridge_default),col= 'blue', lty=2)  # selected by default lambda
abline(h=mse_min_grid, v=log(lambda_ridge_grid),col= 'green', lty=2)  # selected by defined grid of lambda
legend(17, 185, legend=c("default", "search_grid"),
       col=c("blue", "green"), lty=1:2, cex=0.8)

# We can get a better lambda
coef_ridge <- coef(cv_ridge_grid)    
coef_ridge


lambda_ridge <- cv_ridge_grid$lambda.min
model_ridge <- glmnet(X_train, Y_train, lambda = lambda_ridge, alpha = 0)
pred_ridge <- predict(model_ridge, newx = X_test)

abs(pred_ridge-Y_test)/Y_test



mse_ridge <- round(mean((pred_ridge - Y_test)^2), 2)


#=====================================================================================
# elastic net

# Build the model using the training set
model_net <- train(
  salary ~ ., data = X_train_net, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Best tuning parameter
alpha_net <- model_net$bestTune[1]
lambda_net <- model_net$bestTune[2]

# Model coefficients
coef_net <- coef(model_net$finalModel, model_net$bestTune$lambda)
# Make predictions
pred_net <- model_net %>% predict(X_test_net)



mse_net <- round(mean((pred_net - Y_test)^2), 2)

############################################################################################
############################################################################################

#=============================================================================================

best_ridge_coef <- as.numeric(coef(cv_ridge_grid, s = cv_ridge_grid$lambda.min))[-1]

## Perform adaptive LASSO with 10-fold CV
alasso_cv <- cv.glmnet(x = X_train, y = Y_train,
                        ## type.measure: loss to use for cross-validation.
                        type.measure = "mse",
                        ## K = 10 is the default.
                        nfold = 10,
                        ## ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
                        alpha = 1,
                        ##
                        ## penalty.factor: Separate penalty factors can be applied to each
                        ##           coefficient. This is a number that multiplies ‘lambda’ to
                        ##           allow differential shrinkage. Can be 0 for some variables,
                        ##           which implies no shrinkage, and that variable is always
                        ##           included in the model. Default is 1 for all variables (and
                        ##           implicitly infinity for variables listed in ‘exclude’). Note:
                        ##           the penalty factors are internally rescaled to sum to nvars,
                        ##           and the lambda sequence will reflect this change.
                        penalty.factor = 1 / abs(best_ridge_coef),
                        ## prevalidated array is returned
                        keep = TRUE)
## Penalty vs CV MSE plot
plot(alasso_cv)

lambda_alasso <- alasso_cv$lambda.min
coef_adp <- coef(alasso_cv, s = alasso_cv$lambda.min)
coef_adp

pred_adp <- alasso_cv %>% predict(X_test)


mse_adp <- round(mean((pred_adp - Y_test)^2), 2)



coef_adp
coef_lasso
coef_net
coef_ridge


round(lambda_ridge_grid, 2)
round(lambda_lasso, 2)
round(lambda_alasso, 2)
round(lambda_net, 2)
round(alpha_net, 2)


data_compare <- data %>% relocate(salary)  # move salary to the first column
data_compare
data_compare[data_compare$Tm_GSW == 1, ]
data_compare[data_compare$Tm_MEM == 1, ]   # get all the players of MEM
data_compare[data_compare$Tm_TOR == 1, ]   # get all the players of TOR 
data_compare[data_compare$Tm_PHO == 1, ]   # get all the players of TOR 


mean_salary <- mean(data_compare$salary)

data_compare[data_compare$Pos_SF == 1, ]   # SF

mean(data_compare[data_compare$FT. > 0.85, ]$salary)
mean(data_compare$FT.)

data_compare_FT <- data_compare %>% relocate(FT.)  # move FT to the first column

player_high_paid <- data_compare_FT[data_compare_FT$salary > 20000000, ]
mean(player_high_paid$FTA)
mean(data_compare_FT$FTA)


mean(data_compare_FT[data_compare_FT$FT. > 0.72 , ]$salary)

mse_lasso
mse_min_grid
mse_net
mse_ridge
mse_adp

rm(list = ls())
