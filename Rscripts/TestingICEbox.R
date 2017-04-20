library(ICEbox)
library(randomForest)
library(MASS)
data(Boston)
X <-  Boston
y <- X$medv
X$medv <- NULL
bh_rf <- randomForest(X,y)

###Create an ICE object with the predictor "age":
bh.ice <- ice(object = bh_rf, X = X, y = y, predictor ="age", frac_to_build = .1)
###Cluster the curves into 2 groups
clusterICE(bh.ice, nClusters = 2, plot_legend = T)
###Cluster the curves into 3 groups, start all at 0
clusterICE(bh.ice, nClusters = 3, plot_legend = T, centered = T)

bh.dice <- dice(bh.ice)
summary(bh.dice)
plot(bh.dice)
print(bh.dice)

####classification example
data(Pima.te) ###Pima Indian diabetes classification
y <- Pima.te$type
X <- Pima.te
X$type <- NULL
pima_rf <- randomForest(x = X, y = y)
###Create an ICE object for the predictor "skin"
pima.ice <- ice(object = pima_rf, X = X, predictor = "skin", logodds = T, predictfcn = function(object, newdata){
  predict(object, newdata, type = "prob")[, 2]
})

###make a dice object
pima.dice <- dice(pima.ice)
summary(pima.dice)
plot(pima.dice)
print(pima.dice)



library(datarobot)
#ConnectToDataRobot()

concreteFrame <- read.csv(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/concrete.csv")

concreteFrame$cement <- concreteFrame$ï..cement
concreteFrame$ï..cement <- NULL

ConnectToDataRobot(endpoint ='https://app.datarobot.com/api/v2', token='dqmtAG9B7pB7wIuxtmQ81s4BF0mWxZOi') ###Zugang muss bezahlt werden

myDRProject <- SetupProject(concreteFrame, "ConcreteProject")
