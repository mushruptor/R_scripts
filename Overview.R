setwd("/home/marcel/Daten/Work/R_scripts/")

install.packages("readxl")
library(readxl)

stroke <- data.frame(read_excel("Testdaten_Legler.xlsx"))
keeps <- c("MedA", "MedB", "MedC", "MedD", "MedF", "KoA", "KoB", "KoC", "KoD", "KoF", "Age", "Sex", "Outcome")
stroke <- stroke[keeps]


#--- plotmo ---------------------------------------------------------
install.packages("plotmo")
library(plotmo)
library(rpart)

drops <- c("Outcome")
strokenew <- stroke[ , !(names(stroke) %in% drops)]
stroke.names <- colnames(strokenew)
for (i in 1:length(stroke.names)) {
  for (j in 1:length(stroke.names)) {
    if (j <= i){
      next
    }
    factors <- c (stroke.names[i], stroke.names[j])
    formula <- as.formula(paste("Outcome~", paste(factors, collapse="+")))
    rpart.model <- rpart(formula, data=stroke)
    #requires that the folder "plotmo" already exists in the working directory
    png(filename = gsub("[[:space:]]", "", paste("plotmo/", deparse(formula), ".png"))) 
    plotmo(rpart.model)
    dev.off()
  }
}


#--- nomogram -------------------------------------------------------
install.packages("VRPM")
install.packages("kernlab")
library(VRPM)
library(kernlab)

#--- Variant 1 --- Stroke
drops <- c("MedF") #kann keine konstanten Parameter
strokenew <- stroke[ , !(names(stroke) %in% drops)]
svmmodel <- ksvm(Outcome~., data=strokenew, prob.model=TRUE, type="C-svc", kpar=list(0.03), C=100)
newmodel <- preplotperf(svmmodel, strokenew, indy=12, zerolevel="min")
obs1=data.frame(MedA=1,MedB=0,MedC=0)
colplot(newmodel,filename="nomogram/strokeges",zerolevel="min",coloroptions=1)
ccchart(newmodel,obs=obs1) #only working for particular observations

#--- Variant 1 --- Iris
data(iris)
levels(iris$Species)[levels(iris$Species)=="setosa"] <- "other"
levels(iris$Species)[levels(iris$Species)=="virginica"] <- "other"
names(iris)=c("SL","SW","PL","PW","Species")
set.seed(100)
model <-ksvm(Species ~ ., data = iris,prob.model=TRUE,kpar=list(0.03),C=10)
obs1=data.frame(SL=5.2,SW=3.0,PL=1.5,PW=0.3)
# The plot should be based on all training data, so the following code should be used:
newmodel=preplotperf(model,iris,indy=5,zerolevel="min")
colplot(newmodel,filename="nomogram/IRIS2",zerolevel="min", coloroptions=5)
ccchart(newmodel,obs=obs1,filename="nomogram/iris_ccchart",risklabel="Chance on versicolor",
        zerolevel="median")

#--- Variant 2 --- Iris
data(iris)
levels(iris$Species)[levels(iris$Species)=="setosa"] <- "other"
levels(iris$Species)[levels(iris$Species)=="virginica"] <- "other"
drops <- c("Species")
irisnew <- iris[ , !(names(iris) %in% drops)]
iris.names <- colnames(irisnew)
for (i in 1:length(iris.names)) {
  for (j in 1:length(iris.names)) {
    if (j <= i){
      next
    }
    factors <- c (iris.names[i], iris.names[j])
    formula <- as.formula(paste("Species~", paste(factors, collapse="+")))
    svmmodel <- ksvm(formula, data=iris, prob.model=TRUE, type="C-svc", kpar=list(0.03), C=10)
    newmodel <- preplotperf(svmmodel, iris[,c(i,j,5)], indy=3, zerolevel="min")
    colplot(newmodel,filename=gsub("[[:space:]]", "", paste("nomogram/", deparse(formula), ".png")),
            zerolevel="min",coloroptions=1)
  }
}

#--- Variant 2 --- Stroke
drops <- c("MedF") #kann keine konstanten Parameter
strokenew <- stroke[ , !(names(stroke) %in% drops)]
stroke.names <- colnames(strokenew)
for (i in 1:(length(stroke.names) - 1)) {
  for (j in 1:(length(stroke.names) - 1)) {
    if (j <= i){
      next
    }
    factors <- c (stroke.names[i], stroke.names[j])
    formula <- as.formula(paste("Outcome~", paste(factors, collapse="+")))
    svmmodel <- ksvm(formula, data=strokenew, prob.model=TRUE, type="C-svc", kpar=list(0.03), C=10)
    newmodel <- preplotperf(svmmodel, strokenew[,c(i,j,length(stroke.names))], indy=3, zerolevel="min") #indy = column number of outcome
    colplot(newmodel,filename=gsub("[[:space:]]", "", paste("nomogram/", deparse(formula))),
            zerolevel="min",coloroptions=1)
  }
}
#keeps running only for a few formulas?!?!


#--- mdsplot --------------------------------------------------------
#?Regression on 2 Outcomeoptions?
install.packages("randomForest")
library(randomForest)

stroke.rf <- randomForest(Outcome~., stroke, proximity=TRUE, keep.forest=FALSE)
MDSplot(stroke.rf, stroke$Outcome)

#--- icebox ---------------------------------------------------------
install.packages("ICEbox")
library(ICEbox)
library(randomForest)

#--- stroke ---
y <- as.factor(stroke$Outcome)
X <- stroke
stroke.rf <- randomForest(X,y)
stroke.ice <- ice(object = stroke.rf, X = X, predictor = "Age", predictfcn = function(object, newdata){
  predict(object, newdata, type = "prob")[, 2]})
stroke.dice <- dice(stroke.ice)
plot(stroke.ice)
plot(stroke.dice)

#--- iris ---
data(iris)
iris$Species <- as.numeric(iris$Species)
y <- iris$Species
X <- iris
iris.rf <- randomForest(X,y)
getTree(iris.rf, 1, labelVar=TRUE)

iris.ice <- ice(object = iris.rf, X = X, y = y, predictor = "Species")
iris.dice <- dice(iris.ice)
plot(iris.dice)

#--- caret ----------------------------------------------------------
install.packages("caret")
library(caret)

stroke.rf <- randomForest(Outcome~., stroke)
caretrf <- train(Outcome~., data=stroke, method="rf", importance=TRUE)
caretpredict.train <- predict(caretrf)
strokeImp <- varImp(caretrf, scale=FALSE)
dotPlot(strokeImp)

#--- C5.0 -----------------------------------------------------------
install.packages("C50")
library(C50)

#--- iris ---
data(iris)
iris.c50 <- C5.0(Species~.,data=iris)
plot(iris.c50, subtree = 3)

iris.rules <- C5.0(Species~., data=iris, rules=TRUE)
iris.rules
summary(iris.rules)

#--- stroke ---
stroke$Outcome <- as.factor(stroke$Outcome)
stroke.c50 <- C5.0(Outcome~., data=stroke)
plot(stroke.c50, subtree = 5)

stroke.rules <- C5.0(Outcome~., data=stroke, rules=TRUE)
stroke.rules
summary(stroke.rules)

#--- explainvis -----------------------------------------------------
install.packages("ExplainPrediction")
library(ExplainPrediction)

trainIdxs <- sample(x=nrow(iris), size=0.7*nrow(iris), replace=FALSE)
testIdxs <- c(1:nrow(iris))[-trainIdxs]
modelRF <- CoreModel(Species ~ ., iris[trainIdxs,], model="rf",
                     selectionEstimator="MDL",minNodeWeightRF=5,
                     rfNoTrees=100, maxThreads=1)
explainVis(modelRF, iris[trainIdxs,], iris[testIdxs,], method="EXPLAIN",visLevel="both",
           problemName="iris", fileType="none",
           naMode="avg", explainType="WE", classValue=1, displayColor="color")

#--- cubist ---------------------------------------------------------
install.packages("Cubist")
library(Cubist)

#--- stroke ---
stroke.cub <- cubist(x = stroke[, -13], y = as.numeric(stroke$Outcome))
predict(stroke.cub, stroke[1:12, -13], neighbors = 5)

trainIdxs <- sample(1:nrow(stroke), floor(0.8*nrow(stroke)))
trainPred <- stroke[trainIdxs, -13]
testPred <- stroke[-trainIdxs, -13]
trainOutcome <- stroke$Outcome[trainIdxs]
testOutcome <- stroke$Outcome[-trainIdxs]
modelTree <- cubist(x = trainPred, y = as.numeric(trainOutcome))
modelTree
summary(modelTree)

#--- iris ---
data(iris)
iris$Species <- as.numeric(iris$Species)
iris.cub <- cubist(x = iris[, -5], y = iris$Species)
predict(iris.cub, iris[1:4, -5], neighbors = 3)
summary(iris.cub)
