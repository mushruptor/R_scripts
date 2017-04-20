library(randomForest)
library(RColorBrewer)
data(mtcars)
mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree=1000, keep.forest=FALSE,
                          importance=TRUE)
plot(mtcars.rf, log="y")
varImpPlot(mtcars.rf)

set.seed(1)
data(iris)
iris.rf <- randomForest(Species ~ ., iris, proximity=TRUE,
                        keep.forest=FALSE)
MDSplot(iris.rf, iris$Species)
x <- MDSplot(iris.rf, iris$Species)
x 

#add legend
legend("topleft", legend=levels(iris.rf$predicted), 
       fill=brewer.pal(length(levels(iris.rf$predicted)), "Set1"))

# need to identify points?
text(x = x$points[,1],y=x$points[,2]+0.02,labels=attr(x$points,"dimnames")[[1]], cex=0.5)

attr(x$points,"dimnames")[[2]]

setosafilter <- rownames(iris)[iris$Species=="setosa"]
x$points[setosafilter,]



x$points[,1]<(-0.4)


result <- x$points[,1]>0 & x$points[,2]>0.4

length(which(result))
rownames(iris)[result]
iris$Species[result]

getTree(randomForest(iris[,-5], iris[,5], ntree=10), 3, labelVar=TRUE)

#Small extension about plots: plot.randomForest shows how OOB error and in-class OOB error evolved with increasing number of trees; varImpPlot shows attribute 
#importance measures for top attributes and MDSplot all objects plotted on the 2D projection of RF object proximity measure.

####not really meaningful
to.dendrogram <- function(dfrep,rownum=1,height.increment=0.1){
  
  if(dfrep[rownum,'status'] == -1){
    rval <- list()
    
    attr(rval,"members") <- 1
    attr(rval,"height") <- 0.0
    attr(rval,"label") <- dfrep[rownum,'prediction']
    attr(rval,"leaf") <- TRUE
    
  }else{##note the change "to.dendrogram" and not "to.dendogram"
    left <- to.dendrogram(dfrep,dfrep[rownum,'left daughter'],height.increment)
    right <- to.dendrogram(dfrep,dfrep[rownum,'right daughter'],height.increment)
    rval <- list(left,right)
    
    attr(rval,"members") <- attr(left,"members") + attr(right,"members")
    attr(rval,"height") <- max(attr(left,"height"),attr(right,"height")) + height.increment
    attr(rval,"leaf") <- FALSE
    attr(rval,"edgetext") <- dfrep[rownum,'split var']
  }
  
  class(rval) <- "dendrogram"
  
  return(rval)
}

mod <- randomForest(Species ~ .,data=iris)
tree <- getTree(mod,1,labelVar=TRUE)

d <- to.dendrogram(tree)
str(d)
plot(d,center=TRUE,leaflab='none',edgePar=list(t.cex=1,p.col=NA,p.lty=0))
######################################################################################

library(party)
cf_rf <- cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

(ct = ctree(Species ~ ., data = iris))
plot(ct, main="Conditional Inference Tree")

#Table of prediction errors
table(predict(ct), iris$Species)

# Estimated class probabilities
tr.pred = predict(ct, newdata=iris, type="prob")

###############
## EVTREE (Evoluationary Learning)
library(evtree)

ev.raw = evtree(Species ~ ., data=iris)
plot(ev.raw)
table(predict(ev.raw), iris$Species)
1-mean(predict(ev.raw) == iris$Species)

##################
## randomForest
library(randomForest)
fit.rf = randomForest(Species ~ ., data=iris)
print(fit.rf)
importance(fit.rf)
plot(fit.rf)
plot( importance(fit.rf), lty=2, pch=16)
lines(importance(fit.rf))
imp = importance(fit.rf)
impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op = par(mfrow=c(1, 3))
for (i in seq_along(impvar)) {
  partialPlot(fit.rf, iris, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(0, 1))
}

##################
## varSelRF package
library(varSelRF)
x = matrix(rnorm(25 * 30), ncol = 30)
x[1:10, 1:2] = x[1:10, 1:2] + 2
cl = factor(c(rep("A", 10), rep("B", 15)))
rf.vs1 = varSelRF(x, cl, ntree = 200, ntreeIterat = 100,
                  vars.drop.frac = 0.2)

rf.vs1
plot(rf.vs1)

## Example of importance function show that forcing x1 to be the most important
## while create secondary variables that is related to x1.
x1=rnorm(500)
x2=rnorm(500,x1,1)
y=runif(1,1,10)*x1+rnorm(500,0,.5)
my.df=data.frame(y,x1,x2,x3=rnorm(500),x4=rnorm(500),x5=rnorm(500))
rf1 = randomForest(y~., data=my.df, mtry=2, ntree=50, importance=TRUE)
importance(rf1)
cor(my.df)

###############
## OBLIQUE.TREE
library(oblique.tree)
library(MASS)

attach(crabs)
head(crabs)

###############
## OBLIQUE.TREE
library(oblique.tree)

aug.crabs.data = data.frame( g=factor(rep(1:4,each=50)),
                             predict(princomp(crabs[,4:8]))[,2:3])
plot(aug.crabs.data[,-1],type="n")
text( aug.crabs.data[,-1], col=as.numeric(aug.crabs.data[,1]), labels=as.numeric(aug.crabs.data[,1]))
ob.tree = oblique.tree(formula = g~.,
                       data = aug.crabs.data,
                       oblique.splits = "only")
plot(ob.tree);text(ob.tree)


##################
## CORElearn

library(CORElearn)
## Random Forests

fit.rand.forest = CoreModel(Species ~ ., data=iris, model="rf", selectionEstimator="MDL", minNodeWeightRF=5, rfNoTrees=100)
plot(fit.rand.forest)

## decision tree with naive Bayes in the leaves
fit.dt = CoreModel(Species ~ ., data=iris, model="tree", modelType=4)
plot(fit.dt, iris)

petal.sub = subset(iris, !is.na(iris$Species))
fit.rt = CoreModel(as.numeric(iris$Species)~., petal.sub, model="regTree", modelTypeReg=1)
summary(fit.rt)
plot(fit.rt, petal.sub, graphType="prototypes")

pred = predict(fit.rt, petal.sub)
print(pred)
plot(pred)

##################
##longRPart
library(longRPart)

data(pbkphData)
pbkphData$Time=as.factor(pbkphData$Time)
long.tree = longRPart(pbkph~Time,~age+gender,~1|Subject,pbkphData,R=corExp(form=~time))
lrpTreePlot(long.tree, use.n=T, place="bottomright")
?lrpTreePlot

data(iris)
iris$Species=as.factor(iris$Species)
long.tree = longRPart(Sepal.Width~Petal.Length+Petal.Width,~1|Species,iris,R=corExp(form=~Time))
lrpTreePlot(long.tree, use.n=T, place="bottomright")
?lrpTreePlot