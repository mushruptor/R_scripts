###data preparation for bootstrapping
###build individual datasets for every level of the outcome variable

total <- readRDS(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/total.rds")
total[1:10,]
total$date <- as.numeric(total$date)
total$Temp <- total$Temp.
total$Temp. <- NULL

###random stratified sampling test data
library(caret)
set.seed(300)
data_part <- createDataPartition(y = total$class, 
                                 p = 0.005, list = F)
test_data <- total[data_part,]
total <- total[-data_part,]
###prediction
###prepare test data
classlabels_obs <- test_data$class
test_data$class <- NULL
########################################

posvec <- total$class=="background"
filter_posvec <- which(posvec)
background_data <- total[filter_posvec, ] #background level
background_data$class <- droplevels(background_data$class)

posvec <- total$class=="banana"
filter_posvec <- which(posvec)
banana_data <- total[filter_posvec, ] #banana level
banana_data$class <- droplevels(banana_data$class)

posvec <- total$class=="wine"
filter_posvec <- which(posvec)
wine_data <- total[filter_posvec, ] #wine level
wine_data$class <- droplevels(wine_data$class)

a <- 300
set.seed(a)
###sampling with replacement - bootstrap taxa separate for each level of outcome variable
backg_bs <- background_data[sample(nrow(background_data), 500, replace=TRUE), ] #1000 random examples with replacement
banan_bs <- banana_data[sample(nrow(banana_data), 500, replace=TRUE), ] 
wine_bs <- wine_data[sample(nrow(wine_data), 500, replace=TRUE), ] 

###merge datasets - have only one dataset for all RF runs

df1 <- rbind(backg_bs,banan_bs)
df_f <- rbind(df1,wine_bs)

out <- df_f$class
outcome <- df_f[2]
df_f2 <- df_f
df_f$class <- NULL


library(randomForest)
library(doParallel)


workers <- detectCores()-2
cl <- makePSOCKcluster(workers)
registerDoParallel(cl)


i=1
#################################################


# create vector
rf_parlist <- list()

for (i in 1:4) {
  
  
  x <- df_f
  y <- out
  ntree <- 5000
  
  # rf_par <- foreach(n=rep(ceiling(ntree/workers), workers),
  #                   .combine=combine, .multicombine=TRUE,
  #                   .packages='randomForest') %dopar% {
  #                     randomForest(x, y, ntree=n, prox=T, importance = T) #, prox=T, importance = T
  #                   }
  
  rf_parlist[[i]] <- foreach(n=rep(ceiling(ntree/workers), workers),
                             .combine=combine, .multicombine=TRUE,
                             .packages='randomForest') %dopar% {
                               randomForest(x, y, ntree=n, prox=T, importance = T) #, prox=T, importance = T
                             }
  # 
  # run <- 0+i
  # 
  # setwd("C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/")
  # saveRDS(rf_par, file = paste("rf_par",run,'.rds', sep = ""),compress = F)
  #saveRDS(outcome, file = paste("C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/outcome",i,'.rds', sep = ""),compress = F)
}
#######################################################################################

saveRDS(rf_parlist, file = paste("C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/rf_parlist",i,'.rds', sep = ""),compress = F)






rf_com <- combine(rf_parlist[[1]], rf_parlist[[2]], rf_parlist[[3]], rf_parlist[[4]])

###PCA scatterplot of proximity values of randomForest object
x <- MDSplot(rf_com, out, k=2, palette=NULL, pch=20, bty='L')
#add legend
library(RColorBrewer)
par(xpd=TRUE)
#plot(1:10)
legend("topleft", legend=levels(rf_com$predicted), 
       fill=brewer.pal(length(levels(rf_com$predicted)), "Set1"))

# need to identify points?
text(x=x$points[,1],y=x$points[,2]+0.02,labels=attr(x$points,"dimnames")[[1]], cex=0.5)


###get into the MDS plot
attr(x$points,"dimnames")[[1]]
attr(x$points,"dimnames")[[2]]

winefilter <- rownames(df_f2)[df_f2$class=="wine"]
length(winefilter)
y <- x$points[winefilter,]
length(x$points)
length(y)
nrow(y)
str(y)
####check
backg <- x$points[,1]<(-0.46)

vectorbackg <- names(which(backg))

vectorbackg2 <- names(backg[backg==TRUE]) # vector of identifiers for selected area with x values smaller than -0.46

identical(vectorbackg,vectorbackg2)

x$points[,1]<quantile(x$points[,1], 0.05)
x$points[,2]<quantile(x$points[,2], 0.05)
x$points[,2]<quantile(x$points[,2], 0.95)

#############x-column#########y<-column#####
result <- x$points[,1]>0 & x$points[,2]>0.4


length(which(result))

###Prediction
set.seed(300)
test_pred <- predict(rf_parlist, test_data, prox = T, importance = T)
str(test_pred)
testvector <- unlist(test_pred[[1]][1])
table(testvector, classlabels_obs)

stopCluster(cl)

###proximity for prediction run assign function, better lapply onto test_pred object generates new list with matrices
testlist1 <- test_pred[[1]][2][[1]]
testlist2 <-test_pred[[2]][2][[1]]
testlist3 <-test_pred[[3]][2][[1]]
testlist4 <-test_pred[[4]][2][[1]]

class(test_pred[[1]][2][[1]])



alist <- list(testlist1, testlist2)
class(alist)
str(alist)

blist <- Reduce('+', alist)
blist <- blist/length(alist)

###Variable importance
varimp <- importance(rf_com, scale=F) # type=1 permutation importance, type=2 Gini importance

library(caret)
varImp(rf_com)
###VIMP plot
barplot(t(varimp/sum(varimp)), cex.names=1.0, cex.main = 1.2, horiz = T, main="Banana/Wine Classification - Variable influence", ylab="Features", xlab="importance (0-1)", las=1,
        xlim=c(0.0,0.26), border="red", density=40)

###mean decrease in node impurity (and not the mean decrease in accuracy)
varImpPlot(rf_com,type=2)

head(rf_com$proximity)
sort(rownames(rf_com$proximity))
sort(rownames(label))
nrow(rf_com$votes)
nrow(rf_com$proximity)
length(rownames(out1))
rownames(out1)

cor(as.numeric(rownames(rf_com$proximity)),as.numeric(rownames(out1)))
rownames(rf_com$proximity)
max(rf_com$proximity, na.rm=TRUE)
colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMax(rf_com$proximity)
colSort <- function(data, ...) sapply(data, sort, ...)
colSort(rf_com$proximity, decreasing = TRUE)
sort(rf_com$proximity, decreasing = TRUE)


# sortvector <- which(sort(rf_com$proximity, decreasing = TRUE) == 1)
# 
# rf_com$proximity[sortvector,sortvector]
# 
# sortvector <- row(rf_com$proximity)[rf_com$proximity==max(rf_com$proximity)]
# 
# proximity <- rf_com$proximity[-sortvector,-sortvector]
# 
# sort(proximity, decreasing = TRUE)
# 
# b <- as.data.frame(cbind(row.names(rf_com$proximity),apply(rf_com$proximity,1,function(x)
#   names(rf_com$proximity)[which(x==max(x))])))
# 
# cor(as.numeric(b), as.numeric(rownames(rf_com$proximity)))
# b
# 
# which.max(rf_com$proximity)
# 
# diagonal <- upper.tri(rf_com$proximity, diag = FALSE)


###build proximity matrix with diagonal elements and lower part with NAs
rf_com$proximity[lower.tri(rf_com$proximity, diag = T)] <- 0
head(rf_com$proximity)[1:20]

b <- as.data.frame(cbind(row.names(rf_com$proximity),apply(rf_com$proximity,1,function(x)
  names(rf_com$proximity)[which(x==max(x))])))
library(matrixStats)

na.omit(rowRanges(rf_com$proximity))

library(qlcMatrix)

#rf_com$proximity <- na.omit(rf_com$proximity)

rowMax(rf_com$proximity, which = FALSE, ignore.zero = TRUE)[1:2000]


beep()

test_cluster <- kmeans(test_pred$proximity,3,nstart = 20)
head(table(test_cluster$cluster,classlabels_obs))
