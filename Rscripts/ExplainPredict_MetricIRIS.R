library(kernlab)
library(ExplainPrediction)
library(e1071)

data("iris")
m <- iris
levels(m$Species)[levels(m$Species)=="setosa"] <- 1 
levels(m$Species)[levels(m$Species)=="versicolor"] <- 2 
levels(m$Species)[levels(m$Species)=="virginica"] <- 3 
randvec <- as.numeric(round(runif(150, -1.5, 1.5))) 
h <- as.numeric(m$Species) 
randvec <- sapply(randvec + h, factor) 
m$Met <- randvec 


trainIdxs <- sample(x=nrow(m), size=0.7*nrow(m), replace=FALSE) 
testIdxs <- c(1:nrow(m))[-trainIdxs]

modelsvm <- svm(Species ~ ., data = m, kernel = "radial", probability =
                  TRUE)
modelep <- wrap4Explanation(modelsvm)

explainVis(modelep, m[trainIdxs,], m[testIdxs,], method="EXPLAIN",visLevel="both",
           problemName="met", fileType="pdf",
           naMode="avg", explainType="WE", classValue=1,
           displayColor="color")
