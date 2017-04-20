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

plot ( nom ,
       varname.label = FALSE , ia.space =1 , xfrac = .46 , lmgp = .2 )
plot(nom, xfrac=.45)
print(nom)


