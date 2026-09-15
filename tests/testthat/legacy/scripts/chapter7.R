# ====================================================
# Chapter 7 : Visualization and Clustering for Bivariate Case
# ====================================================

# ---- 0. Load the data and set the parameters ----

load("data/chap7/dataMultiple.RData")
tm <- 90
s <- 2
dyad <- 120

# ---- 1. Probabilities transition matrix and distance matrix ----

ProbBivariate <- matrix(NA, nrow = nrow(data), ncol = s^5)

for(d in as.numeric(unique(data$dyad))){
  
  #X variable 
  p1 <- c(mleEstimation(countEmpBivariate(states = s, #X1
                                          chainFM_V1 = as.numeric(data[(2*d - 1),1:tm]), 
                                          chainSM_V1 = as.numeric(data[(2*d),1:tm]),
                                          chainFM_V2 = as.numeric(data[240+(2*d - 1),1:tm]), 
                                          chainSM_V2 = as.numeric(data[240+(2*d),1:tm])
  )))
  p2 <- c(mleEstimation(countEmpBivariate(states = s, #X2
                                          chainFM_V1 = as.numeric(data[(2*d),1:tm]), 
                                          chainSM_V1 = as.numeric(data[(2*d - 1),1:tm]),
                                          chainFM_V2 = as.numeric(data[240+(2*d),1:tm]), 
                                          chainSM_V2 = as.numeric(data[240+(2*d - 1),1:tm])
  )))
  
  #Y variable 
  p3 <- c(mleEstimation(countEmpBivariate(states = s, #Y1
                                          chainFM_V1 = as.numeric(data[240+(2*d - 1),1:tm]), 
                                          chainSM_V1 = as.numeric(data[240+(2*d),1:tm]),
                                          chainFM_V2 = as.numeric(data[(2*d - 1),1:tm]), 
                                          chainSM_V2 = as.numeric(data[(2*d),1:tm])
  )))
  p4 <- c(mleEstimation(countEmpBivariate(states = s, #Y2
                                          chainFM_V1 = as.numeric(data[240+(2*d),1:tm]), 
                                          chainSM_V1 = as.numeric(data[240+(2*d - 1),1:tm]),
                                          chainFM_V2 = as.numeric(data[(2*d),1:tm]), 
                                          chainSM_V2 = as.numeric(data[(2*d - 1),1:tm])
                                          
  )))
  
  ProbBivariate[(2*d - 1),] <- p1
  ProbBivariate[(2*d),] <- p2
  ProbBivariate[(240+(2*d - 1)),] <- p3
  ProbBivariate[(240+(2*d)),] <- p4
  
}

ProbBivariate <- as.data.frame(ProbBivariate, row.names = c(1:nrow(data)))

dim(ProbBivariate)
names(ProbBivariate)
name <- vector()
for(i in 1:length(names(ProbBivariate))){
  name <- append(name, paste(names(ProbBivariate)[i], "bi", sep = "_"))
}
colnames(ProbBivariate) <- name
names(ProbBivariate)

dataComplete <- cbind(data, ProbBivariate)

dissMatBivariate <- dist(ProbBivariate)

# ---- 2. MDS ----

cmd <- cmdscale(dissMatBivariate, eig = TRUE, k = 2)

x <- cmd$points[,1]
y <- cmd$points[,2]

dataComplete$x <- x
dataComplete$y <- y

plot(x, y, xlim = c(-3,3), ylim = c(-2,1.5), 
     xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1, 
     cex.lab = 1.5, 
     cex.axis = 1.2, 
     cex.main = 1.5,
     sub = "", 
     main = "")

# ---- 3. Clustering ----

res <- hclust(dissMatBivariate, method = "ward.D2")

plot(res, labels = FALSE, hang = -1, xlab = "Clusters", 
     cex.lab = 1.5, 
     cex.axis = 1.2, 
     cex.main = 1.5,
     sub = "", 
     main = "")

opt <- NbClust(ProbBivariate, dist(ProbBivariate, method = "euclidean"), distance = NULL, method = "ward.D2", index = "ch")
nbclustOPT <- opt$Best.nc[1]
nbclustOPT
optimalNb <- 4

clustBivariate <- cutree(res, optimalNb)

dataComplete <- cbind(dataComplete, clusterBivariate = clustBivariate)

table(dataComplete$caseSimulated, dataComplete$clusterBivariate)

plot(x, y, xlim = c(-3,3), ylim = c(-2,1.5),
     xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     col = c("red", "blue", "green", "purple")[dataComplete$clusterBivariate], 
     cex.lab = 1.5, 
     cex.axis = 1.2, 
     cex.main = 1.5,
     sub = "", 
     main = "")
legend(x = "topright", 
       legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"), 
       col = c("red", "blue", "green", "purple"), pch = 16)

# ---- 4. Pattern analysis ----

global <- vector()
local <- vector()

for(i in 1:(length(data$dyad)/4)){
  
  #chains
  X_FM <- as.numeric(data[data$dyad==i & data$variable=="X" & data$members=="FM",1:tm])
  X_SM <- as.numeric(data[data$dyad==i & data$variable=="X" & data$members=="SM",1:tm])
  Y_FM <- as.numeric(data[data$dyad==i & data$variable=="Y" & data$members=="FM",1:tm])
  Y_SM <- as.numeric(data[data$dyad==i & data$variable=="Y" & data$members=="SM",1:tm])
  
  #empirical count
  X_FM.count <- countEmpBivariate(states = s, chainFM_V1 = X_FM, chainSM_V1 = X_SM, chainFM_V2 = Y_FM, chainSM_V2 = Y_SM)
  X_SM.count <- countEmpBivariate(states = s, chainFM_V1 = X_SM, chainSM_V1 = X_FM, chainFM_V2 = Y_SM, chainSM_V2 = Y_FM)
  Y_FM.count <- countEmpBivariate(states = s, chainFM_V1 = Y_FM, chainSM_V1 = Y_SM, chainFM_V2 = X_FM, chainSM_V2 = X_SM)
  Y_SM.count <- countEmpBivariate(states = s, chainFM_V1 = Y_SM, chainSM_V1 = Y_FM, chainFM_V2 = X_SM, chainSM_V2 = X_FM)
  
  #global test 
  global_XFM <- bivariateCase(empirical = X_FM.count, alpha = 0.05)$case
  global_XSM <- bivariateCase(empirical = X_SM.count, alpha = 0.05)$case
  global_YFM <- bivariateCase(empirical = Y_FM.count, alpha = 0.05)$case
  global_YSM <- bivariateCase(empirical = Y_SM.count, alpha = 0.05)$case
  
  #global assignation 
  global[(i*2)-1] <- global_XFM
  global[(i*2)] <- global_XSM
  global[240+((i*2)-1)] <- global_YFM
  global[240+(i*2)] <- global_YSM 
  
  #local test and assignation for X_FM
  if(isTRUE(global_XFM=="trivial")){
    local[(i*2)-1] <- "trivial"
  }
  
  if(isTRUE(global_XFM=="univariate")){
    uni <- univariatePattern(states = s, chainFM = X_FM, chainSM = X_SM, alpha = 0.05)
    local[(i*2)-1] <- uni$pattern
    
  }
  
  if(isTRUE(global_XFM=="partial")){
    local[(i*2)-1] <- partialPattern(empirical = X_FM.count)$pattern
  }
  
  if(isTRUE(global_XFM=="complete")){
    local[(i*2)-1] <- completePattern(empirical = X_FM.count)$pattern
  }
  
  #local test and assignation for X_SM
  if(isTRUE(global_XSM=="trivial")){
    local[(i*2)] <- "trivial"
  }
  
  if(isTRUE(global_XSM=="univariate")){
    uni <- univariatePattern(states = s, chainFM = X_SM, chainSM = X_FM, alpha = 0.05)
    local[(i*2)] <- uni$pattern
  }
  
  if(isTRUE(global_XSM=="partial")){
    local[(i*2)] <- partialPattern(empirical = X_SM.count)$pattern
  }
  
  if(isTRUE(global_XSM=="complete")){
    local[(i*2)] <- completePattern(empirical = X_SM.count)$pattern
  }
  
  #local test and assignation for Y_FM
  if(isTRUE(global_YFM=="trivial")){
    local[240+((i*2)-1)] <- "trivial"
  }
  
  if(isTRUE(global_YFM=="univariate")){
    uni <- univariatePattern(states = s, chainFM = Y_FM, chainSM = Y_SM, alpha = 0.05)
    local[240+((i*2)-1)] <- uni$pattern
  }
  
  if(isTRUE(global_YFM=="partial")){
    local[240+((i*2)-1)] <- partialPattern(empirical = Y_FM.count)$pattern
  }
  
  if(isTRUE(global_YFM=="complete")){
    local[240+((i*2)-1)] <- completePattern(empirical = Y_FM.count)$pattern
  }
  
  #local test and assignation for Y_SM
  if(isTRUE(global_YSM=="trivial")){
    local[240+(i*2)] <- "trivial"
  }
  
  if(isTRUE(global_YSM=="univariate")){
    uni <- univariatePattern(states = s, chainFM = Y_SM, chainSM = Y_FM, alpha = 0.05)
    local[240+(i*2)] <- uni$pattern
  }
  
  if(isTRUE(global_YSM=="partial")){
    local[240+(i*2)] <- partialPattern(empirical = Y_SM.count)$pattern
  }
  
  if(isTRUE(global_YSM=="complete")){
    local[240+(i*2)] <- completePattern(empirical = Y_SM.count)$pattern
  }
}

dataComplete <- cbind(dataComplete, global, local)

table(dataComplete$caseSimulated, dataComplete$global)
table(dataComplete$caseSimulated, dataComplete$local)

# ---- 6. Analysis of the dependence structure and clustering ----

table(dataComplete$global, dataComplete$local)

table(dataComplete$clusterBivariate, dataComplete$global)
table(dataComplete$clusterBivariate, dataComplete$local)

table(dataComplete[dataComplete$global=="complete",]$clusterBivariate, dataComplete[dataComplete$global=="complete",]$local)
table(dataComplete[dataComplete$global=="partial",]$clusterBivariate, dataComplete[dataComplete$global=="partial",]$local)

table(dataComplete[dataComplete$global=="partial",]$local)

plot(x, y, xlim = c(-3,3), ylim = c(-2,1.5),
     xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     col = c("red", "blue","green", "purple")[as.factor(dataComplete$global)], 
     cex.lab = 1.5, 
     cex.axis = 1.2, 
     cex.main = 1.5,
     sub = "", 
     main = "")
legend(x = "topright", 
       legend = c("complete", "partial", "trivial", "univariate"), 
       col = c("red", "blue", "green", "purple"), pch = 16)

plot(x, y, xlim = c(-3,3), ylim = c(-2,1.5),
     xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     col = c("lightblue", "aquamarine", "red", "pink","purple", "green", 
             "salmon", "plum", "cornsilk3", "blue", "brown", 
             "grey")[as.factor(dataComplete$local)], 
     cex.lab = 1.5, 
     cex.axis = 1.2, 
     cex.main = 1.5,
     sub = "", 
     main = "")

# ---- 5. Mean transition matrix ----

meanC1 <- matrix(apply(dataComplete[dataComplete$clusterBivariate==1,names(dataComplete)[95:126]], 2, FUN = mean), ncol = s, byrow = FALSE) #cluster 1 
meanC2 <- matrix(apply(dataComplete[dataComplete$clusterBivariate==2,names(dataComplete)[95:126]], 2, FUN = mean), ncol = s, byrow = FALSE) #cluster 2 
meanC3 <- matrix(apply(dataComplete[dataComplete$clusterBivariate==3,names(dataComplete)[95:126]], 2, FUN = mean), ncol = s, byrow = FALSE) #cluster 3 
meanC4 <- matrix(apply(dataComplete[dataComplete$clusterBivariate==4,names(dataComplete)[95:126]], 2, FUN = mean), ncol = s, byrow = FALSE) #cluster 4 

round(meanC1, 3) 
round(meanC2, 3) 
round(meanC3, 3) 
round(meanC4, 3) 
