library(hdnom)
library(survival)
library(rms)
library(caret)
library(e1071)

data(smart)
x = as.matrix(smart[, -c(1,2)])
time = smart$TEVENT

event = smart$EVENT
y = Surv(time,event)

fit = hdcox.alasso(x, y, nfolds = 3, rule = c("lambda.1se"), seed = c(5,11))

dat(iris)

fit <- lm(as.numeric(iris$Species)~.,data=iris) 

fit_pred <- predict(fit, data=iris[1:4])
round(fit_pred)

tab <- table(predicted=round(fit_pred),obs=as.numeric(iris$Species))
classAgreement(tab)

x.df = as.data.frame(iris[1:4])
dd = datadist(x.df)
options(datadist = "dd")

lamb <- fit$alasso_best_lambda

nom = hdnom.nomogram(fit$alasso_model, model.type = "alasso",
                     x, time, event, x.df, pred.at = 365,
                     funlabel = "1 year old", lambda = lamb)


fitrms <- lrm(as.numeric(iris$Species)~.,data=iris)

nom <- nomogram(fitrms, fun=function(x)1/(1+exp(-x)),  # or fun=plogis
                fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
                funlabel="Risk of Death")

nom <- nomogram(fitrms, fun=plogis)

nom <- nomogram(fitrms, fun=list('Prob Y<=1'=plogis))

plot(nom)

fun(5)

nom <- nomogram(fit, datadist=dd)


plot(nom)
print(nom)

library(DynNom)
?DynNom


library(kernlab)
library(VRPM)
library(mltools)
data(iris)
levels(iris$Species)[levels(iris$Species)=="setosa"] <- "other"
levels(iris$Species)[levels(iris$Species)=="virginica"] <- "other"
names(iris)=c("SL","SW","PL","PW","Species")
# good model
model <-ksvm(Species ~ ., data = iris,prob.model=TRUE,kpar=list(0.03),C=10)
# bad model
model2 <-ksvm(Species ~ ., data = iris,prob.model=TRUE,kpar=list(10),C=10)
# plot latent variables of approximation and SVM
plotperf(model,iris,indy=5,type="lp",filename="iris")
plotperf(model2,iris,indy=5,type="lp",filename="iris2")
# plot contributions of approximation and SVM
# good model: rest term is small in comparison with other contributions and lpmodel
# (latent variable of SVM)
plotperf(model,iris,indy=5,type="contributions",filename="iris")
# bad model: rest term is large in comparison with other contributions and lpmodel
# (latent variable of SVM)
plotperf(model2,iris,indy=5,type="contributions",filename="iris2")
# plot latent variables of approximation and SVM
plotperf(model,iris,indy=5,type="outcomes",filename="iris")
plotperf(model2,iris,indy=5,type="outcomes",filename="iris2")

# Generalized regression models
data2 =as.data.frame(Titanic)
model3 <- glm(Survived ~ Age + Class + Sex, data = data2, weights = Freq,
              family = binomial("probit"))
DynNom(model3, data2, clevel = 0.9)


library(rms)
n <- 1000    # define sample size
set.seed(17) # so can reproduce the results
age            <- rnorm(n, 50, 10)
blood.pressure <- rnorm(n, 120, 15)
cholesterol    <- rnorm(n, 200, 25)
sex            <- factor(sample(c('female','male'), n,TRUE))


# Specify population model for log odds that Y=1
L <- .4*(sex=='male') + .045*(age-50) +
  (log(cholesterol - 10)-5.2)*(-2*(sex=='female') + 2*(sex=='male'))
# Simulate binary y to have Prob(y=1) = 1/[1+exp(-L)]
y <- ifelse(runif(n) < plogis(L), 1, 0)


ddist <- datadist(age, blood.pressure, cholesterol, sex)
options(datadist='ddist')


f <- lrm(y ~ lsp(age,50)+sex*rcs(cholesterol,4)+blood.pressure)
nom <- nomogram(f, fun=function(x)1/(1+exp(-x)),  # or fun=plogis
                fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
                funlabel="Risk of Death")
#Instead of fun.at, could have specified fun.lp.at=logit of
#sequence above - faster and slightly more accurate
plot(nom, xfrac=.45)


rm(list = ls())
# Import libraries
require(Hmisc)
require(rms)

set.seed(12345)
n <- 1000 # define sample size
age <- rnorm(n, 0, 10)
sbp <- rnorm(n, 120, 15)
sex <- factor(sample(c(0,1), n, TRUE), levels=c(0,1),
              labels=c('female','male'))
# Specify population model for log odds that Y=1
L <- 0.5*(sex=="male") + 0.01*age + 0.12*sbp + 0.02*sbp*age + rnorm(n, 0,
                                                                    10)
# Simulate binary y to have Prob(y=1) = 1/[1+exp(-L)]
y <- ifelse(runif(n) < plogis(L), 1, 0)

# Create a dataframe and apply nice labels to each variable.
mydata <- data.frame(age, sbp, sex, y)
var.labels <- c(age = "Patient Age - 50 years",
                sbp = "Systolic blood pressure",
                sex = "Sex",
                y = "Outcome")
label(mydata) = lapply(names(var.labels),
                       function(x) label(mydata[,x]) <- var.labels[x])
label(mydata) # show the labels.

# Fit a model
ddist <- datadist(mydata)
options(datadist="ddist")
f <- rms::Glm(y ~ age*sbp + sex) # or
f <- rms::lrm(y ~ age*sbp + sex)
f
f$coefficients

dd <- datadist(iris)


f2 <- rms::Glm(as.numeric(iris$Species)~.,data=iris)

# Create a nomogram.
nom <- nomogram(f2, fun=plogis, lp=F, nint=10, maxscale=100,
                interact = list( sbp=c(100,140,180) ),
                fun.at=c(.01,.05,seq(.1,.9,by=.1),.95,.99),
                funlabel="Risk of Death", vnames="labels")
plot(nom)

# end
options(datadist=NULL)

