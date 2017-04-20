data(iris)
library(rms)
library(plyr)
library(Hmisc)
library(xtable)
library(lattice)
library(rms) 

data("iris")
dd = datadist(iris)
options(datadist="dd")
attach(iris)


library(plyr)
iris$Species <- revalue(iris$Species, c("setosa"="0", "virginica"="0", "versicolor"="1"))

class(iris$Species)

mod1 = lrm(as.factor(iris$Species) ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris, x=TRUE, y=TRUE) # +Sepal.Length*Petal.Length+Sepal.Length

#mod1 = lrm(as.factor(iris$Species) ~ ., data=iris, x=TRUE, y=TRUE)
mod1
summary(mod1)

plot(anova(mod1), what='proportion chisq') # relative importance
plot(Predict(mod1, fun=plogis)) # predicted values
rms::validate(mod1, method="boot", B=500) # bootstrapped validation
my.calib <- rms::calibrate(mod1, method="boot", B=500) # model calibration
plot(my.calib, las=1)

penalty <- pentrace(mod1, penalty=c(0.5,1,2,3,4,6,8,12,16,24), maxit=25)
mod1_pen <- update(mod1, penalty=penalty$penalty)
effective.df(mod1_pen)
anova(mod1_pen)
mod1_pen
summary(mod1_pen)

nom = nomogram(mod1_pen, fun=plogis) #or
nom = nomogram(mod1_pen, fun = function ( x ) 1 / (1+ exp ( -x ) ), # o r p l o g i s
               fun.at = c ( seq ( .1 , .9 , by = .1 ) , .95 , .99 ),
               funlabel = " Probability of 1 "
               )          # interact = list(Sepal.Length = seq(seq(4,8,by=0.1)))
print(plot(nom))

table(iris$Sepal.Length)


p <- Predict(mod1_pen)

# cancer.lrm.Status <- Glm(cancer$status~.,data=cancer, family=binomial(link='logit'),
#                       control = list(maxit = 50))
# 
# #iris_pred <- Predict(iris.lrm.Sepal,data=iris[1:4])
# 
# nom = nomogram(iris.lrm.Sepal, fun=plogis, fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999), funlabel="Risk of Death")
# 
# nom = nomogram(cancer.lrm.Status, fun=plogis, fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999), funlabel="Probability of 1")


nom = nomogram(iris.lrm.Sepal, fun = function ( x ) 1 / (1+ exp ( -x ) ), # o r p l o g i s
               fun.at = c ( seq ( .1 , .9 , by = .1 ) , .95 , .99 ),
               funlabel = " Probability of CAD ")
plot ( nom ,
       varname.label = FALSE , ia.space =1 , xfrac = .46 , lmgp = .2 )
plot(nom, xfrac=.45)
print(nom)


# Let us now build a logistic regression model using the lrm function, plot the expected probabilities, and evaluate the model. We also use the pentrace function to perform logistic regression with penalized maximum likelihood estimation.

# mod1 = lrm(as.factor(bm) ~ age + sbp + rx, data=prostate, x=TRUE, y=TRUE)
# mod1
# summary(mod1)
# 
# plot(anova(mod1), what='proportion chisq') # relative importance
# plot(Predict(mod1, fun=plogis)) # predicted values
# rms::validate(mod1, method="boot", B=500) # bootstrapped validation
# my.calib <- rms::calibrate(mod1, method="boot", B=500) # model calibration
# plot(my.calib, las=1)
# 
# penalty <- pentrace(mod1, penalty=c(0.5,1,2,3,4,6,8,12,16,24), maxit=25)
# mod1_pen <- update(mod1, penalty=penalty$penalty)
# effective.df(mod1_pen)
# mod1_pen
# 
# ?nomogram
# 
# class(iris.lrm.Sepal)

require(MASS)
exp(cbind(coef(x), confint(x))) 
glmFit <- glm(Yfac ~ X1 + X2 + X3, family=binomial(link="logit"))
glm0 <- glm(Yfac ~ 1, family=binomial(link="logit"))
anova(glm0, glmFit, test="Chisq")

data(iris)
library(caret)
library(randomForest)
library(ExplainPrediction)
library(data.table)
# use iris data set, split it randomly into a training and testing set
trainIdxs <- sample(x=nrow(iris), size=0.7*nrow(iris), replace=FALSE)
testIdxs <- c(1:nrow(iris))[-trainIdxs]
# build random forests model with certain parameters
modelRF <- CoreModel(Species ~ ., iris[trainIdxs,], model="rf",
                     selectionEstimator="MDL",minNodeWeightRF=5,
                     rfNoTrees=100, maxThreads=1)
# generate model explanation and visualization
# turn on history in the visualization window to see all graphs
explainVis(modelRF, iris[trainIdxs,], iris[testIdxs,], method="EXPLAIN",visLevel="both",
           problemName="iris", fileType="none",
           naMode="avg", explainType="WE", classValue=1, displayColor="color")



setwd("C:/Users/datzmant/Documents/BigData/ML/RData")

m1 <- fread("monks-1.train")
m2 <- fread("monks-1.test")
m3 <- rbind(m1,m2)
m3$V8 <- NULL
continuousFeatures <- m3
labels <- m3$V1
trainIdxs_vec <- createDataPartition(y=labels, p=.70, list=FALSE)
trainIdxs <- continuousFeatures[trainIdxs_vec,]
trainingLabels <- labels[trainIdxs_vec]

#The remaining 25% become testing examples 
testingLabels <- labels[-trainIdxs_vec]
testIdxs <- continuousFeatures[-trainIdxs_vec,]

trainIdxs <- as.data.frame(lapply(trainIdxs, factor))
testIdxs <- as.data.frame(lapply(testIdxs, factor))

#trainIdxs$V1 <- as.factor(trainIdxs$V1)
#testIdxs$V1 <- as.factor(testIdxs$V1)

l <- seq(from=1.6,to=388.2, by=((1-0.5)*2))
d <- l/2
e <- d[1:166]
?seq
rep(1.6:8.2, each=20)
trainIdxs$V8 <- seq(from=1.6,to=391.2, by=((1-0.5)*2))
testIdxs$V8 <- e  
testlabel <- testIdxs$V1
trainlabel <- trainIdxs$V1
testIdxs$V1 <- NULL
trainIdxs$V1 <- NULL 
#testIdxs <- as.data.frame(sapply(testIdxs, factor))

# testIdxs$V2 <- as.factor(testIdxs$V2)
# testIdxs$V3 <- as.factor(testIdxs$V3)
# testIdxs$V4 <- as.factor(testIdxs$V4)
# testIdxs$V5 <- as.factor(testIdxs$V5)
# testIdxs$V6 <- as.factor(testIdxs$V6)
# testIdxs$V7 <- as.factor(testIdxs$V7)
# trainIdxs$V2 <- as.factor(trainIdxs$V2)
# trainIdxs$V3 <- as.factor(trainIdxs$V3)
# trainIdxs$V4 <- as.factor(trainIdxs$V4)
# trainIdxs$V5 <- as.factor(trainIdxs$V5)
# trainIdxs$V6 <- as.factor(trainIdxs$V6)
# trainIdxs$V7 <- as.factor(trainIdxs$V7)




# build random forests model with certain parameters
modelRF <- CoreModel(as.factor(trainlabel) ~ ., trainIdxs, model="rf",
                     selectionEstimator="MDL",minNodeWeightRF=5,
                     rfNoTrees=100, maxThreads=1)


explainVis(modelRF, trainIdxs, testIdxs, method="EXPLAIN",visLevel="both",
           problemName="monks", fileType="none",
           naMode="avg", explainType="WE", classValue=1, displayColor="color", noDecimalsInValueName = 4, normalizeTo = 1, modelVisCompact = T) # , displayThreshold = 0.1 
# , classValue=2 gives trainIdxs class 1 , see sidebar

explainVis(modelRF, trainIdxs, testIdxs, method="EXPLAIN",visLevel="model",
           problemName="monks", fileType="none",
           naMode="avg", explainType="infGain", classValue=1, displayColor="color", noDecimalsInValueName = 4, normalizeTo = 100, modelVisCompact = F)
# 
# The weight of evidence is shown on the horizontal axis. The vertical axis contains
# names of the attributes on the left-hand side and their values for the chosen instance on
# the right-hand side. The probability for class ”survived” returned by the method
# for the given instance x is reported on the top (0.26).  The lengths of the thicker bars
# correspond to the influences of the given attribute values in the model, expressed by
# (3).  The positive weight of evidence is given on the right-hand side and the negative
# one is on the left-hand side.  Thinner bars above the explanation bars indicate the av-
# erage value of the weight of evidence over all training instances for the corresponding
# attribute value.


# Clean up, otherwise the memory is still taken
destroyModels(modelRF) 

# evaluate features in given data set with selected method
# instead of formula interface one can provide just 
# the name or index of target variable
trainIdxs$V1 <- trainlabel
estReliefF <- attrEval("V1", trainIdxs, 
                       estimator="ReliefFexpRank", ReliefIterations=30)
print(estReliefF)

# evaluate ordered features with ordEval
profiles <- ordDataGen(200)
est <- ordEval(class ~ ., profiles, ordEvalNoRandomNormalizers=100)
# print(est)  

