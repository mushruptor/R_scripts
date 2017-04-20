###data preparation for bootstrapping
###build individual datasets for every level of the outcome variable

#total <- readRDS(file = "/home/datze/R/Datasets/total.rds")
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


library(randomForest)
library(doParallel)


workers <- detectCores()-1
cl <- makePSOCKcluster(workers)
registerDoParallel(cl)


i=1
#################################################


# create vector
rf_parlist <- list()

for (i in 1:4) {
  a <- 300+i
  set.seed(a)
  ###sampling with replacement - bootstrap taxa separate for each level of outcome variable
  backg_bs <- background_data[sample(nrow(background_data), 500, replace=TRUE), ] #1000 random examples with replacement
  banan_bs <- banana_data[sample(nrow(banana_data), 500, replace=TRUE), ] 
  wine_bs <- wine_data[sample(nrow(wine_data), 500, replace=TRUE), ] 
  
  ###merge datasets
  
  df1 <- rbind(backg_bs,banan_bs)
  df_f <- rbind(df1,wine_bs)
  
  out <- df_f$class
  outcome <- df_f[2]
  df_f2 <- df_f
  df_f$class <- NULL
  
  x <- df_f
  y <- out
  ntree <- 100
  
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
   saveRDS(outcome, file = paste("C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/outcome",i,'.rds', sep = ""),compress = F)
   saveRDS(df_f2, file = paste("C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/df",i,'.rds', sep = ""),compress = F)
}
#######################################################################################

library(beepr)
beep()

pllist <- list()

for (i in 1:4) {
pllist[[i]] <-readRDS(file = paste("C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/rf_par",i,'.rds', sep = ""))
  }


# pl1 <- readRDS(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/rf_par1.rds")
# pl2 <- readRDS(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/rf_par2.rds")
# pl3 <- readRDS(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/rf_par3.rds")
# pl4 <- readRDS(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/rf_par4.rds")  

# i=1
# pl1 <- paste("rf_par",i, sep = "",'.rds')  
# pl2 <- paste("rf_par",i, sep = "",'.rds')  
# pl3 <- paste("rf_par",i, sep = "",'.rds')  
# pl4 <- paste("rf_par",i, sep = "",'.rds')  

# rf1 <- readRDS(pl1)
# rf2 <- readRDS(pl2)
# rf3 <- readRDS(pl3)
# rf4 <- readRDS(pl4)
rf_com <- combine(pllist[[1]], pllist[[2]], pllist[[3]], pllist[[4]])
#rf_com <- combine(pl1,pl2,pl3,pl4) # always first element is used for proximity measures ->out1 labels - only 1500 observations of proximity measures
saveRDS(rf_com, file = paste("rf_com",1,'.rds', sep = ""),compress = F)
#rf_com <- combine(rf1,rf2,rf3,rf4)


###Prepare outcome labels for combined run
out1 <- readRDS(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/outcome1.rds")
out2 <- readRDS(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/outcome2.rds")
out3 <- readRDS(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/outcome3.rds")
out4 <- readRDS(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/outcome4.rds")  

filtervector <- c(rownames(out1), rownames(out2), rownames(out3), rownames(out4))



#filtervector <- sort(unique(filtervector)) # do not sort, otherwise the separation breaks
##test ohne sort
filtervector <- unique(filtervector)

total <- readRDS(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/total.rds")
total[1:10,]
total$date <- as.numeric(total$date)
total$Temp <- total$Temp.
total$Temp. <- NULL

###filter out labels of all 4 runs

total_red <- total[filtervector,]
nrow(total_red)
head(total_red)

label <- total_red[2]
lab <- total_red$class

###PCA scatterplot of proximity values of randomForest object
x <- MDSplot(rf_com, lab, k=2, palette=NULL, pch=20, bty='L')

#add legend
library(RColorBrewer)
par(xpd=TRUE)
#plot(1:10)
legend("topleft", legend=levels(rf_com$predicted), 
       fill=brewer.pal(length(levels(rf_com$predicted)), "Set1"))

# need to identify points?
text(x=x$points[,1],y=x$points[,2]+0.02,labels=attr(x$points,"dimnames")[[1]], cex=0.5)

head(x$points)
attr(x$points,"dimnames")[[2]]

df1 <- readRDS(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/df1.rds")
# df2 <- readRDS(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/df2.rds")
# df3 <- readRDS(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/df3.rds")
# df4 <- readRDS(file="C:/Users/datzmant/Documents/R/Skripte/ExampleDataML/df4.rds")  

# df_all <- rbind(df1,df2,df3,df4)
# df_all <- unique(df_all)

attr(x$points,"dimnames")[[1]]
winefilter <- rownames(df1)[df1$class=="wine"]
length(winefilter)
y <- x$points[winefilter,]
length(x$points)
length(y)
nrow(y)

####check
x$points[,1]<(-0.4)


result <- x$points[,1]>0 & x$points[,2]>0.4

length(which(result))
rownames(iris)[result]
iris$Species[result]


###Prediction
set.seed(300)
test_pred <- predict(rf_com, test_data, prox = T, importance = T)
table(test_pred$predicted, classlabels_obs)
head(test_pred$proximity)
plot(test_pred$proximity)


stopCluster(cl)

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
