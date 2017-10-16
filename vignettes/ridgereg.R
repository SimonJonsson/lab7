## ----setup, include=FALSE------------------------------------------------
library(caret)
library(mlbench)
library(leaps)
library(lab7)
library(MASS)
theSeed <- 1337
set.seed(theSeed)
trainC <- caret::trainControl("cv")
data("BostonHousing")

## ----instantiation data--------------------------------------------------
trainIndex <-
  caret::createDataPartition(BostonHousing$crim,
                             p = 0.8,
                             times = 1,
                             list = FALSE)
trainDat <- BostonHousing[trainIndex, ]
testDat <- BostonHousing[-trainIndex, ]
form <- tax ~ .


## ----instantiation models------------------------------------------------
linMod <- caret::train(form,
                       trainDat,
                       method = "lm",
                       trControl = trainC)
linFMod <- caret::train(form,
                        trainDat,
                        method = "leapForward",
                       trControl = trainC)

## ----evaluation----------------------------------------------------------
linMod$results$RMSE
linFMod$results$RMSE

## ----instant model thingy, include=FALSE---------------------------------
# Acknowledgement to Eric Herwin and Albin Vasterlund
ridgeMod  <- list(type = "Regression",
                    library = "lab7",
                    loop = NULL,
                    prob = NULL)


  ridgeMod$parameters <- data.frame(parameter = "lambda",
                                    class = "numeric",
                                    label = "Ridge Regression")


  ridgeMod$grid <- function (x, y, len = NULL, search = "grid"){
    data.frame(lambda = lambda)
  }

  ridgeMod$fit <- function (x, y, wts, param, lev, last, classProbs, ...) {
    dat <- if (is.data.frame(x))
      x
    else as.data.frame(x)
    dat$.outcome <- y
    out <- ridgereg(.outcome ~ ., data=dat ,lambda = param$lambda, ...)

    return(out)
  }

  ridgeMod$predict <- function (modelFit, newdata, submodels = NULL) {
    if (!is.data.frame(newdata))
      newdata <- as.data.frame(newdata)
    newdata <- scale(newdata)
    return(modelFit$pred(newdata))
  }



## ----find lambda---------------------------------------------------------
set.seed(theSeed)
res <- c()
for(lambda in seq(0, 20, by = 1)) {
  temp <- caret::train(form,
                       data = trainDat,
                       ridgeMod)
  res[lambda * 10] <- temp$results$RMSE
}
bestLambda <- which.min(res) / 10
bestRMSE <- res[which.min(res)]
bestLambda
bestRMSE

## ----hej, include=FALSE--------------------------------------------------
temp <- caret::train(form,
                       data = trainDat,
                       ridgeMod)

## ----crossval------------------------------------------------------------
# Acknowledgement to Eric Herwin and Albin Vasterlund
set.seed(theSeed)
fold_count <- 10
lambda <- seq(10,20,by=1)
fitControl <- caret::trainControl(method = "repeatedcv",
                                  number = fold_count,
                                  ## repeated ten times
                                  repeats = fold_count)
ridgeMod <- caret::train(form,
                         data = trainDat,
                         method = ridgeMod,
                         trControl = fitControl)
ridgeMod

## ----eval values---------------------------------------------------------
lin <- list(RMSE = linMod$results$RMSE, RSquared = linMod$results$Rsquared)
linF <- list(RMSE = linFMod$results$RMSE[3], RSquared = linFMod$results$Rsquared[3])
ridgeM <- list(RMSE = ridgeMod$results$RMSE[7], RSquared = ridgeMod$results$Rsquared[7])
temp <- cbind(lin,linF,ridgeM)
colnames(temp) <- c("Lin. Reg", "Lin. Reg. F", "Ridge Reg.")
temp

