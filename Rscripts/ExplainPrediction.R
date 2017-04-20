library(ExplainPrediction)

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
## Not run:
#store instance explanations to file
explain <- explainVis(modelRF, iris[trainIdxs,], iris[testIdxs,], method="EXPLAIN", visLevel="instance",
           problemName="iris", fileType="pdf",
           naMode="avg", explainType="WE", classValue=1, displayColor="color")
destroyModels(modelRF) # clean up
# build a regression tree
trainReg <- regDataGen(100)
testReg <- regDataGen(20)
modelRT <- CoreModel(response~., trainReg, model="regTree", modelTypeReg=1)
# generate both model and instance explanations using the defaults
explainVis(modelRT, trainReg, testReg) # don't forget to switch on the history
destroyModels(modelRT) #clean up
## End(Not run)

explain$explainInfo$pAV$Sepal.Width
