########################## Uptake Work Sample Project ##################################
library(Hmisc)


## @knitr exploratory_data_analysis

##convert all missing and empty fields to NA values for imputation later
train <- fread('~/Interview_Projects/train.csv', na.string = c("NA", " ", ""))
test  <- fread('~/Interview_Projects/test.csv', na.string = c("NA", " ", ""))

##a sample of some descriptive statistics
Hmisc:::describe(train[,
                              sample(ncol(train), 10, replace = FALSE), with = FALSE])


## @knitr data_conversions

##anything with less than 10 unique values will be treated as categorical
level.threshold <- 10

###identify the categorical variable candidates
unique.levels <- function(a){
  
  return(length(unique(a)))
}
variables.to.remove <-  c(paste0("adate_", c(2:24)), 
                          paste0("rfa_", c(2:24)), 
                          paste0("rdate_", c(3:24)), 
                          paste0("ramnt_", c(3:24)),
                          "id",
                          "date")
train <- train[, -variables.to.remove, with = FALSE]

data.unique.levels <- lapply(train, unique.levels)

cat.vars <- c("source", "title", "state", "zip", "mailcode", "has_chapter", 
              "noexch", "recinhse", "recp3", "recpgvg", "recsweep", "mdmaud",
              "domain", "cluster", "ageflag", "homeownr","gender", "child03",
              "child07", "child12","child18",
              "mbcraft", "mbgarden", "mbcolect", "magfem", "magmale", "pubgardn",  
              "pubculin", "pubphoto", "datasrce", "solp3" ,"solih", "major" ,"geocode",     
              "collect1" ,"veterans", "bible", "catlg", "homee", "pets" , "cdplay",      
              "stereo" , "pcowners" , "photo" , "crafts", "fisher", "gardenin", "boats",
              "walker" , "kidstuff" , "cards" , "plates", "lifesrc", "pepstrfl", "mhuc2",
              "rfa_2r" , "rfa_2f" , "rfa_2a", "mdmaud_r", "mdmaud_f", "mdmaud_a",  
              "geocode2")
num.vars <- names(data.unique.levels)[!(names(data.unique.levels) %in% cat.vars)]

##impute a 0 for the amount field if they didn't donate
train[which(is.na(train$amount)), "amount"] <- 0

##The categorical variables will be treated as factor and then turned into dummies
##any missing categorical variables will be imputed a value of 999

for(var in cat.vars){
  train.na.id <- which(is.na(train[,var, with = FALSE]))
  
  if(length(train.na.id != 0)){
    train[train.na.id, var] <- "999"
  }
}

for (col in c(cat.vars, "source", "domain", "mdmaud")){
  e = substitute(X := as.factor(X), list(X = as.symbol(col)))
  train[ , eval(e)]
}

##I will convert the other variables to numeric. NAs will also be introduced at this point
for (col in num.vars){
  e = substitute(X := as.numeric(X), list(X = as.symbol(col)))
  train[ , eval(e)]
}

for(var in num.vars){
  train.na.id <- which(is.na(train[,var, with = FALSE]))
  
  if(length(train.na.id) != 0){
    train[train.na.id, var] <- mean(train[,var, with = FALSE][[1]], 
                                           na.rm = TRUE)
  }
}

#response rate in the training population


## @knitr lasso_probability_model
library(glmnet)
###use model.matrix to auto-create dummies
remove.variables <- c(
  "zip",
  "amount",
  "mdmaud",
  "rfa_2r",
  "source", 
  "numchld",
  "state",
  "title",
  "cluster",
  "minrdate",
  "maxrdate",
  "maxadate",
  "age",
  "dob",
  "market"
)

set.seed(100)
train.id <- sample(nrow(train), .1*nrow(train), replace = FALSE)
train    <- train[train.id, -remove.variables, with = FALSE]

##x <- model.matrix(responded ~ . ,  train)[, -1] # model matrix
##save(x, file = "~/Interview_Projects/Uptake/independent_vars.RData")
load("~/Interview_Projects/independent_vars.RData") ## had knitr error

##The same factors don't exist in the train and test set, so I have the remove 
##some so that I can score the model. Glmnet can't handle diff dimensions
factors.to.remove <-  c("noexch999","genderA","genderC","mbcraft5.0","mbcraft6.0",
                        "mbcolect5.0","mbcolect6.0","magfem5.0","magmale4.0",
                        "pubculin5.0", "pubculin6.0","solih06" )
x <- x[,-which(colnames(x) %in% factors.to.remove)]

y <- train$responded   # model response

grid = 10^seq(10, -2, length=100) # grid of penalties
lasso.logit <- glmnet(x, y, family="binomial", alpha=.1, lambda = grid)
## cv.out.logit <- cv.glmnet(x, y, family="binomial", nfolds = 5,  alpha=.1) # optimal penalty min
bestlam.min <- .02423974 # optimal penalty
bestlam.1se <- .1293602

response.rate <- length(which(train$responded == 1))/nrow(train)  ## ~5%
lasso.logit.pred <- predict(lasso.logit, s=bestlam.min, newx=x, type="response") # predict full 
lasso.logit.coef <- predict(lasso.logit, 
                            type="coefficients", 
                            s=bestlam.min)[seq(ncol(x)+1),]
lasso.logit.vars <- lasso.logit.coef[lasso.logit.coef != 0]

subset.lasso.logit.vars <- names(lasso.logit.vars)[-1]
subset.lasso.logit.vars


## @knitr confusion_matrix
train$lasso.pred      <- ifelse(lasso.logit.pred > response.rate*1.2, 1, 0)
mytable.train <- table(actual.response = train$responded, 
                       predicted.response = train$lasso.pred)

print(mytable.train)

## @knitr validation_out_of_sample

##data munging on the test set
for(var in cat.vars){
  
  test.na.id <- which(is.na(test[,var, with = FALSE]))
  
  if(length(test.na.id != 0)){
    test[test.na.id, var] <- "999"
  }
}

for (col in c(cat.vars, "source", "domain", "mdmaud")){
  e = substitute(X := as.factor(X), list(X = as.symbol(col)))
  test[ , eval(e)]
}

##remove amount and responded from the list
num.vars.test <- num.vars[!num.vars %in% c("responded", "amount")]

##I will convert the other variables to numeric. NAs will also be introduced 
for (col in num.vars.test){
  e = substitute(X := as.numeric(X), list(X = as.symbol(col)))
  test[ , eval(e)]
}

for(var in num.vars.test){
  test.na.id <- which(is.na(test[,var, with = FALSE]))
  
  if(length(test.na.id) != 0){
    test[test.na.id, var] <- mean(test[,var, with = FALSE][[1]], 
                                         na.rm = TRUE)
  }
}

###run the model off the training set on the test set and score the population
mod <- test[, -variables.to.remove, with = FALSE]
test <- mod[, -remove.variables[!remove.variables %in% c("amount", "responded")], with = FALSE]

##this works outside of knitr
##x.test <- model.matrix( ~ . ,  test)[, -1] # model matrix
##lasso.logit.pred.test <- predict(lasso.logit, s=bestlam.min, newx=x.test, type="response")
##test$market    <- ifelse(lasso.logit.pred.test > response.rate*1.2, 1, 0)
##write.csv(test, '~/Interview_Projects/Uptake/test.csv')

