setwd("/home/marcel/Work")

install.packages("readxl")
library(readxl)

stroke <- data.frame(read_excel("Testdaten_Legler.xlsx"))
keeps <- c("MedA", "MedB", "MedC", "MedD", "MedF", "KoA", "KoB", "KoC", "KoD", "KoF", "Age", "Sex", "Outcome")
stroke <- stroke[keeps]

#--- plotmo ---
install.packages("plotmo")
library(plotmo)
library(rpart)

rpart.model <- rpart(Outcome~., data=stroke)
plotmo(rpart.model)

#--- nomogram ---
install.packages("VRPM")
install.packages("kernlab")
library(VRPM)
library(kernlab)

sdata <- stroke[c("MedA","MedB","MedC","Outcome")]

svmmodel <- ksvm(Outcome~MedA+MedB+MedC, data=sdata, prob.model=TRUE, type="C-svc", kpar=list(0.03), C=100)
newmodel <- preplotperf(svmmodel, sdata, indy=4, zerolevel="min")

obs1=data.frame(MedA=0.5,MedB=0.5,MedC=0.5)
colplot(newmodel,filename="stroke1",zerolevel="min",coloroptions=1)
ccchart(newmodel,obs=obs1) #not yet working

#--- mdsplot --- ?Regression on 2 Outcomeoptions?
install.packages("RandomForest")
library(randomForest)

stroke.rf <- randomForest(Outcome~., stroke, proximity=TRUE, keep.forest=FALSE)
MDSplot(stroke.rf, stroke$Outcome)

#--- icebox ---
install.packages("ICEbox")
library(ICEbox)
library(randomForest)

stroke.rf <- randomForest(Outcome~., stroke, proximity=TRUE, keep.forest=FALSE)
stroke.ice <- ice(object = stroke.rf, X = stroke, predictor = "MedB", frac_to_build = 0.1)

#--- caret ---
install.packages("caret")
library(caret)

stroke.rf <- randomForest(Outcome~., stroke)
caretrf <- train(Outcome~., data=stroke, method="rf", importance=TRUE)
caretpredict.train <- predict(caretrf)
strokeImp <- varImp(caretrf, scale=FALSE)
dotPlot(strokeImp)

#--- C5.0 ---
install.packages("C50")
library(C50)

modelRule <- C5.0(age~., data=stroke, rules=TRUE)
#convert to factors first


#--- explainvis ---
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
