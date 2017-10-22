library(ggplot2)
library(gridExtra)
library(Hmisc)

## @knitr exploratory_data_analysis 
device.data <- fread('~/device_failure.csv')
Hmisc:::describe(device.data)

## @knitr data_visualization
device.data <- fread('~/device_failure.csv')
device.data$day <- weekdays(as.Date(device.data$date))
device.data$device.substr <- substr(device.data$device,1,2)
failed.states <- device.data[which(device.data$failure ==1)]

g <- ggplot(failed.states, aes(device.substr))
g + geom_bar()

ggplot(failed.states, aes(day)) + geom_bar()

p2 <- ggplot(device.data, aes(x = attribute2, y = failure)) + geom_point(shape=1) 
p3 <- ggplot(device.data, aes(x = attribute3, y = failure)) + geom_point(shape=1) 
p4 <- ggplot(device.data, aes(x = attribute4, y = failure)) + geom_point(shape=1) 
p5 <- ggplot(device.data, aes(x = attribute5, y = failure)) + geom_point(shape=1)
p6 <- ggplot(device.data, aes(x = attribute6, y = failure)) + geom_point(shape=1) 
p7 <- ggplot(device.data, aes(x = attribute7, y = failure)) + geom_point(shape=1) 
p8 <- ggplot(device.data, aes(x = attribute8, y = failure)) + geom_point(shape=1) 
p9 <- ggplot(device.data, aes(x = attribute9, y = failure)) + geom_point(shape=1) 
gridExtra:::grid.arrange(p2, p3, p4, p5, ncol =2, nrow = 2, top= "Failure by Attributes")
gridExtra:::grid.arrange(p6, p7, p8, p9, ncol =2, nrow = 2)

## @knitr outlier_treatment
outlier_variables <- c("attribute3", "attribute9")

for(outlier in outlier_variables){
  device.data.out <- device.data[-which(device.data[, outlier, with = FALSE] 
                                  > quantile(device.data[[outlier]], probs = .9999 )[["99.99%"]])]
}

## @knitr lasso_probability_model

##oversampling
set.seed(100)
good.states  <- device.data.out[-which(device.data$failure ==1),]
bad.train.id <- sample(nrow(failed.states), 53, replace=FALSE)
bad.train    <- failed.states[bad.train.id,]
bad.test     <- failed.states[-bad.train.id,]
good.subset  <- good.states[sample(nrow(good.states), .02*nrow(good.states), replace = FALSE),]
good.train.id <- sample(nrow(good.subset), .5*nrow(good.subset), replace = FALSE)
good.train    <- good.subset[good.train.id,]
good.test     <- good.subset[-good.train.id,]
train <- rbind(bad.train, good.train)
test  <- rbind(bad.test, good.test)


##convert the categorical variables to factor
names_factors = c('day', 'device.substr')
for (col in names_factors){
  e = substitute(X := as.factor(X), list(X = as.symbol(col)))
  train[ , eval(e)]
  test[ , eval(e)]
  device.data.out[, eval(e)]
}

# model development
library(glmnet)

x <- model.matrix(failure ~ . - date - device, train)[, -1] # model matrix
y <- train$failure # model response

grid = 10^seq(10, -2, length=100) # grid of penalties
lasso.logit <- glmnet(x, y, family="binomial", alpha=.1, lambda = grid)
cv.out.logit <- cv.glmnet(x, y, family="binomial", nfolds = 5,  alpha=.1) # optimal penalty
bestlam <- cv.out.logit$lambda.min # optimal penalty

lasso.logit.pred <- predict(lasso.logit, s=bestlam, newx=x, type="response") # predict full data set
lasso.logit.coef <- predict(lasso.logit, type="coefficients", s=bestlam)[seq(ncol(x)+1),]
lasso.logit.vars <- lasso.logit.coef[lasso.logit.coef != 0]

subset.lasso.logit.vars <- names(lasso.logit.vars)[-1]
##print selected variables
subset.lasso.logit.vars

failure.rate.os <- length(which(train$failure == 1))/nrow(train)
train$lasso.pred <- ifelse(lasso.logit.pred > failure.rate.os*1.2, 1, 0)

##validation on out of sample population
x.test <- model.matrix(failure ~ . - date - device, test)[, -1] # model matrix
lasso.logit.pred.test <- predict(lasso.logit, s=bestlam, newx=x.test, type="response")
test$lasso.pred      <- ifelse(lasso.logit.pred > failure.rate.os*1.2, 1, 0)



## chaos matrix
mytable.train <- table(actual.failure = train$failure, predicted.failure = train$lasso.pred)
print(mytable.train)

## chaos matrix
mytable.test <- table(actual.failure = test$failure, predicted.failure = test$lasso.pred)
print(mytable.test)

##applying to the entire dataset
x <- model.matrix(failure ~ . - date - device, device.data.out)[, -1]
failure.rate <- length(which(device.data.out$failure == 1))/nrow(device.data.out)
lasso.logit.pred.total <- predict(lasso.logit, s=bestlam, newx=x, type="response")/(failure.rate.os/failure.rate)
device.data.out$lasso.pred  <- ifelse(lasso.logit.pred.total > failure.rate*20, 1, 0)

mytable.total <- table(actual.failure = device.data.out$failure, predicted.failure = device.data.out$lasso.pred)
print(mytable.total)


