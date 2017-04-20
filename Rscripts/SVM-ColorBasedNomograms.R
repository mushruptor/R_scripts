library(VRPM)
library(kernlab)
data(iris)
levels(iris$Species)[levels(iris$Species)=="setosa"] <- "other"
levels(iris$Species)[levels(iris$Species)=="virginica"] <- "other"
names(iris)=c("SL","SW","PL","PW","Species")
set.seed(100)
model <-ksvm(Species ~ ., data = iris,prob.model=TRUE,kpar=list(0.03),C=10)
obs1=data.frame(SL=5.2,SW=3.0,PL=1.5,PW=0.3)
# The plot should be based on all training data, so the following code should be used:
newmodel=preplotperf(model,iris,indy=5,zerolevel="min")
# individual plot
ccchart(newmodel,obs=obs1,filename="iris_ccchart",risklabel="Chance on versicolor",
        zerolevel="median")
getwd()

library(kernlab)
data(iris)
levels(iris$Species)[levels(iris$Species)=="setosa"] <- "other"
levels(iris$Species)[levels(iris$Species)=="virginica"] <- "other"
names(iris)=c("SL","SW","PL","PW","Species")
# RBF kernel
model <-ksvm(Species ~ ., data = iris,prob.model=TRUE,kpar=list(0.03),C=10)
# The plot should be based on all training data, so the following code should be used:
newmodel=preplotperf(model,iris,indy=5,zerolevel="min")
colplot(newmodel,filename="IRIS2",zerolevel="min",coloroptions=1)

# head(newmodel,n=10)


