library(randomForest)
data <- iris
data$Species <- factor(ifelse(data$Species=='virginica','virginica','other'))
mod <- randomForest(Species~., data=data) # Sepal.Length+Sepal.Width
library(plotmo)
plotmo(mod, type="prob")


library(forestFloor)
library(randomForest)
#simulate data
obs=1000
vars = 18
X = data.frame(replicate(vars,rnorm(obs)))
Y = with(X, X1^2 + sin(X2*pi) + 2 * X3 * X4 + 1 * rnorm(obs))
#grow a forest, remeber to include inbag
rfo=randomForest(X,Y,keep.inbag = TRUE,sampsize=250,ntree=50)
#compute topology
ff = forestFloor(rfo,X)
#ggPlotForestFloor(ff,1:9)
plot(ff,1:9,col=fcol(ff))

#mean(c(rep(100,12), 33.3))


# Good visualization !
#Setup a binary classification problem
require(randomForest)
data(iris)
iris
set.seed(1)
dat <- iris
dat$Species <- factor(ifelse(dat$Species=='virginica','virginica','other'))

# test a binary factor variable
dat$test <- ifelse(dat$Species=="other",0,1)
# seq1 <- seq(by=0,length.out = 75)
# seq2 <- seq1-1
# seqn <- c(seq1,seq2)
# dat$Petal.Length <- as.factor(seqn)
dat$Petal.Length <- as.factor(dat$test)
dat$test <- NULL

# build train and test set
#set.seed(2)
trainrows <- runif(nrow(dat)) > 0.3
train <- dat[trainrows,]
test <- dat[!trainrows,]

#Build a decision tree
require(rpart)
model.rpart <- rpart(Species~., train)

model.rpart

model.rf <- randomForest(Species~., train, ntree=25, proximity=TRUE, importance=TRUE, nodesize=5)
getTree(model.rf, k=1, labelVar=TRUE)

# Furthermore, no model is truly a black box, because you can examine predicted responses vs actual responses for each variable in the dataset. This is a good idea regardless of what sort of model you are building:

library(ggplot2)
pSpecies <- predict(model.rf,test,'vote')[,2]
plotData <- lapply(names(test[,1:4]), function(x){
  out <- data.frame(
    var = x,
    type = c(rep('Actual',nrow(test)),rep('Predicted',nrow(test))),
    value = c(test[,x],test[,x]),
    species = c(as.numeric(test$Species)-1,pSpecies)
  )
   out$value <- out$value-min(out$value) #Normalize to [0,1]
   out$value <- out$value/max(out$value)
   out
})
plotData <- do.call(rbind,plotData)
qplot(value, species, data=plotData, facets = type ~ var, geom='smooth', span = 0.6) # change span for visualization

# I've normalized the variables (sepal and petal length and width) to a 0-1 range. The response is also 0-1, where 0 is other and 1 is virginica. As you can see the random forest is a good model, even on the test set.

importance(model.rf, type=1)

plot(model.rf)
plot(margin(model.rf)) 
MDSplot(model.rf, iris$Species, k=5)

### Outlier detection via RF - values exceed ten should be investigated

plot(outlier(model.rf), type="h", ylim=c(0,10), col=c("red", "green")[as.numeric(dat$Species)]) # needs proximity equals TRUE statement # ,xlim=c(90,95),ylim =c(0,50) , "blue"

# the score reflects the distance of an object from the centroid of its predicted class; the higher the score the further the object is away form the class centroid 

library(CORElearn)
model.rf2 <- CoreModel(Species~., train,
          model=c("rf"))  # ,"rfNear","tree","knn","knnKernel","bayes","regTree"
    
rfv <- rfOutliers(model.rf2, data=train)

plot(abs(rfv))

#for a nicer display try
plot(model.rf2, train, graphType="outliers")
destroyModels(model.rf2) # clean up

library(DMwR)
# remove "Species", which is a categorical column
iris2 <- iris[,1:4]
outlier.scores <- lofactor(iris2, k=5)
plot(density(outlier.scores))

#iris2 <- iris[,1:4]

n <- nrow(iris2)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(iris2), cex=.8, xlabs=labels)

pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(iris2, pch=pch, col=col)




