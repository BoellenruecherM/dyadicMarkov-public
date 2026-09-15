# ====================================================
# Chapter 6 : Visualization and Clustering for Univariate Case
# ====================================================

# ---- 0. Set the parameters ----

s <- 2
tm <- 90
extra <- 5
xaxes <- c(-1.5,1.5)
yaxes <- c(-1.5,0.5)

# ---- 1. Actor-only pattern ----

## ---- 1.1 P in [0.9 0.1] ----

### ---- 1.1.0 Load the data ----

load("data/chap6/AM09.RData") #load each database
data <- AM9

### ---- 1.1.1 Probabilities transition matrix and distance matrix ----

Prob <- NULL
for(d in 1:(nrow(data)/2)){
  p1 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d - 1), 1:tm])), as.numeric(unlist(data[(2*d), 1:tm])))))
  p2 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d), 1:tm])), as.numeric(unlist(data[(2*d - 1), 1:tm])))))
  Prob <- rbind(rbind(Prob, p1), p2)
}

dissMat <- dist(as.data.frame(Prob))

### ---- 1.1.2 MDS ----

cmd <- cmdscale(dissMat, eig=TRUE, k=2)

x <- cmd$points[,1]
y <- cmd$points[,2]

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

### ---- 1.1.3 Clustering ----

optimalNb <- 2

res <- hclust(dissMat, method = "ward.D2")

plot(res, labels = FALSE, hang = -1, xlab = "Clusters",
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

clust <- cutree(res, optimalNb)

data <- cbind(data, cluster = clust)

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     col = c("red", "blue")[data$cluster],
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

table(data$cluster, data$case)
table(data$cluster, data$caseComb)
ftable(data$cluster, data$case, data$caseComb)

### ---- 1.1.4 Mean transition matrix ----

completeData <- cbind(data, as.data.frame(Prob))

meanTPC1 <- matrix(apply(completeData[completeData$cluster==1,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC2 <- matrix(apply(completeData[completeData$cluster==2,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC1
meanTPC2

## ---- 1.2 P in [0.8 0.2] ----

### ---- 1.2.0 Load the data ----

load("data/chap6/AM08.RData") #load each database
data <- AM8

### ---- 1.2.1 Probabilities transition matrix and distance matrix ----

Prob <- NULL
for(d in 1:(nrow(data)/2)){
  p1 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d - 1), 1:tm])), as.numeric(unlist(data[(2*d), 1:tm])))))
  p2 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d), 1:tm])), as.numeric(unlist(data[(2*d - 1), 1:tm])))))
  Prob <- rbind(rbind(Prob, p1), p2)
}

dissMat <- dist(as.data.frame(Prob))

### ---- 1.2.2 MDS ----

cmd <- cmdscale(dissMat, eig=TRUE, k=2)

x <- cmd$points[,1]
y <- cmd$points[,2]

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

### ---- 1.2.3 Clustering ----

optimalNb <- 2

res <- hclust(dissMat, method = "ward.D2")

plot(res, labels = FALSE, hang = -1, xlab = "Clusters",
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

clust <- cutree(res, optimalNb)

data <- cbind(data, cluster = clust)

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     col = c("red", "blue")[data$cluster],
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

table(data$cluster, data$case)
table(data$cluster, data$caseComb)
ftable(data$cluster, data$case, data$caseComb)

### ---- 1.2.4 Mean transition matrix ----

completeData <- cbind(data, as.data.frame(Prob))

meanTPC1 <- matrix(apply(completeData[completeData$cluster==1,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC2 <- matrix(apply(completeData[completeData$cluster==2,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC1
meanTPC2

## ---- 1.3 P in [0.7 0.3] ----

### ---- 1.3.0 Load the data ----

load("data/chap6/AM07.RData") #load each database
data <- AM7

### ---- 1.3.1 Probabilities transition matrix and distance matrix ----

Prob <- NULL
for(d in 1:(nrow(data)/2)){
  p1 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d - 1), 1:tm])), as.numeric(unlist(data[(2*d), 1:tm])))))
  p2 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d), 1:tm])), as.numeric(unlist(data[(2*d - 1), 1:tm])))))
  Prob <- rbind(rbind(Prob, p1), p2)
}

dissMat <- dist(as.data.frame(Prob))

### ---- 1.3.2 MDS ----

cmd <- cmdscale(dissMat, eig=TRUE, k=2)

x <- cmd$points[,1]
y <- cmd$points[,2]

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

### ---- 1.3.3 Clustering ----

optimalNb <- 2

res <- hclust(dissMat, method = "ward.D2")

plot(res, labels = FALSE, hang = -1, xlab = "Clusters",
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

clust <- cutree(res, optimalNb)

data <- cbind(data, cluster = clust)

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     col = c("red", "blue")[data$cluster],
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

table(data$cluster, data$case)
table(data$cluster, data$caseComb)
ftable(data$cluster, data$case, data$caseComb)

### ---- 1.3.4 Mean transition matrix ----

completeData <- cbind(data, as.data.frame(Prob))

meanTPC1 <- matrix(apply(completeData[completeData$cluster==1,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC2 <- matrix(apply(completeData[completeData$cluster==2,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC1
meanTPC2

## ---- 1.4 P in [0.6 0.4] ----

### ---- 1.4.0 Load the data ----

load("data/chap6/AM06.RData") #load each database
data <- AM6

### ---- 1.4.1 Probabilities transition matrix and distance matrix ----

Prob <- NULL
for(d in 1:(nrow(data)/2)){
  p1 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d - 1), 1:tm])), as.numeric(unlist(data[(2*d), 1:tm])))))
  p2 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d), 1:tm])), as.numeric(unlist(data[(2*d - 1), 1:tm])))))
  Prob <- rbind(rbind(Prob, p1), p2)
}

dissMat <- dist(as.data.frame(Prob))

### ---- 1.4.2 MDS ----

cmd <- cmdscale(dissMat, eig=TRUE, k=2)

x <- cmd$points[,1]
y <- cmd$points[,2]

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

### ---- 1.4.3 Clustering ----

optimalNb <- 2

res <- hclust(dissMat, method = "ward.D2")

plot(res, labels = FALSE, hang = -1, xlab = "Clusters",
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

clust <- cutree(res, optimalNb)

data <- cbind(data, cluster = clust)

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     col = c("red", "blue")[data$cluster],
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

table(data$cluster, data$case)
table(data$cluster, data$caseComb)
ftable(data$cluster, data$case, data$caseComb)

### ---- 1.4.4 Mean transition matrix ----

completeData <- cbind(data, as.data.frame(Prob))

meanTPC1 <- matrix(apply(completeData[completeData$cluster==1,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC2 <- matrix(apply(completeData[completeData$cluster==2,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC1
meanTPC2

# ---- 2. Partner-only pattern ----

## ---- 2.1 DF = 1 ----

### ---- 2.1.1 P in [0.9 0.1] ----

#### ---- 2.1.1.0 Load the data ----

load("data/chap6/PM09.RData") #load each database
data <- PM9

#### ---- 2.1.1.1 Probabilities transition matrix and distance matrix ----

Prob <- NULL
for(d in 1:(nrow(data)/2)){
  p1 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d - 1), 1:tm])), as.numeric(unlist(data[(2*d), 1:tm])))))
  p2 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d), 1:tm])), as.numeric(unlist(data[(2*d - 1), 1:tm])))))
  Prob <- rbind(rbind(Prob, p1), p2)
}

dissMat <- dist(as.data.frame(Prob))

#### ---- 2.1.1.2 MDS ----

cmd <- cmdscale(dissMat, eig=TRUE, k=2)

x <- cmd$points[,1]
y <- cmd$points[,2]

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

#### ---- 2.1.1.3 Clustering ----

optimalNb <- 2

res <- hclust(dissMat, method = "ward.D2")

plot(res, labels = FALSE, hang = -1, xlab = "Clusters",
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

clust <- cutree(res, optimalNb)

data <- cbind(data, cluster = clust)

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     col = c("red", "blue")[data$cluster],
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

table(data$cluster, data$case)
table(data$cluster, data$caseComb)
ftable(data$cluster, data$case, data$caseComb)

#### ---- 2.1.1.4 Mean transition matrix ----

completeData <- cbind(data, as.data.frame(Prob))

meanTPC1 <- matrix(apply(completeData[completeData$cluster==1,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC2 <- matrix(apply(completeData[completeData$cluster==2,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC1
meanTPC2

### ---- 2.1.2 P in [0.8 0.2] ----

#### ---- 2.1.2.0 Load the data ----

load("data/chap6/PM08.RData") #load each database
data <- PM8

#### ---- 2.1.2.1 Probabilities transition matrix and distance matrix ----

Prob <- NULL
for(d in 1:(nrow(data)/2)){
  p1 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d - 1), 1:tm])), as.numeric(unlist(data[(2*d), 1:tm])))))
  p2 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d), 1:tm])), as.numeric(unlist(data[(2*d - 1), 1:tm])))))
  Prob <- rbind(rbind(Prob, p1), p2)
}

dissMat <- dist(as.data.frame(Prob))

#### ---- 2.1.2.2 MDS ----

cmd <- cmdscale(dissMat, eig=TRUE, k=2)

x <- cmd$points[,1]
y <- cmd$points[,2]

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

#### ---- 2.1.2.3 Clustering ----

optimalNb <- 2

res <- hclust(dissMat, method = "ward.D2")

plot(res, labels = FALSE, hang = -1, xlab = "Clusters",
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

clust <- cutree(res, optimalNb)

data <- cbind(data, cluster = clust)

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     col = c("red", "blue")[data$cluster],
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

table(data$cluster, data$case)
table(data$cluster, data$caseComb)
ftable(data$cluster, data$case, data$caseComb)

#### ---- 2.1.2.4 Mean transition matrix ----

completeData <- cbind(data, as.data.frame(Prob))

meanTPC1 <- matrix(apply(completeData[completeData$cluster==1,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC2 <- matrix(apply(completeData[completeData$cluster==2,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC1
meanTPC2

### ---- 2.1.3 P in [0.7 0.3] ----

#### ---- 2.1.3.0 Load the data ----

load("data/chap6/PM07.RData") #load each database
data <- PM7

#### ---- 2.1.3.1 Probabilities transition matrix and distance matrix ----

Prob <- NULL
for(d in 1:(nrow(data)/2)){
  p1 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d - 1), 1:tm])), as.numeric(unlist(data[(2*d), 1:tm])))))
  p2 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d), 1:tm])), as.numeric(unlist(data[(2*d - 1), 1:tm])))))
  Prob <- rbind(rbind(Prob, p1), p2)
}

dissMat <- dist(as.data.frame(Prob))

#### ---- 2.1.3.2 MDS ----

cmd <- cmdscale(dissMat, eig=TRUE, k=2)

x <- cmd$points[,1]
y <- cmd$points[,2]

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

#### ---- 2.1.3.3 Clustering ----

optimalNb <- 2

res <- hclust(dissMat, method = "ward.D2")

plot(res, labels = FALSE, hang = -1, xlab = "Clusters",
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

clust <- cutree(res, optimalNb)

data <- cbind(data, cluster = clust)

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     col = c("red", "blue")[data$cluster],
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

table(data$cluster, data$case)
table(data$cluster, data$caseComb)
ftable(data$cluster, data$case, data$caseComb)

#### ---- 2.1.3.4 Mean transition matrix ----

completeData <- cbind(data, as.data.frame(Prob))

meanTPC1 <- matrix(apply(completeData[completeData$cluster==1,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC2 <- matrix(apply(completeData[completeData$cluster==2,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC1
meanTPC2

### ---- 2.1.4 P in [0.6 0.4] ----

#### ---- 2.1.4.0 Load the data ----

load("data/chap6/PM06.RData") #load each database
data <- PM6

#### ---- 2.1.4.1 Probabilities transition matrix and distance matrix ----

Prob <- NULL
for(d in 1:(nrow(data)/2)){
  p1 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d - 1), 1:tm])), as.numeric(unlist(data[(2*d), 1:tm])))))
  p2 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d), 1:tm])), as.numeric(unlist(data[(2*d - 1), 1:tm])))))
  Prob <- rbind(rbind(Prob, p1), p2)
}

dissMat <- dist(as.data.frame(Prob))

#### ---- 2.1.4.2 MDS ----

cmd <- cmdscale(dissMat, eig=TRUE, k=2)

x <- cmd$points[,1]
y <- cmd$points[,2]

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

#### ---- 2.1.4.3 Clustering ----

optimalNb <- 2

res <- hclust(dissMat, method = "ward.D2")

plot(res, labels = FALSE, hang = -1, xlab = "Clusters",
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

clust <- cutree(res, optimalNb)

data <- cbind(data, cluster = clust)

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     xlim = xaxes, ylim = yaxes,
     col = c("red", "blue")[data$cluster],
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

table(data$cluster, data$case)
table(data$cluster, data$caseComb)
ftable(data$cluster, data$case, data$caseComb)

#### ---- 2.1.4.4 Mean transition matrix ----

completeData <- cbind(data, as.data.frame(Prob))

meanTPC1 <- matrix(apply(completeData[completeData$cluster==1,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC2 <- matrix(apply(completeData[completeData$cluster==2,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC1
meanTPC2

## ---- 2.2 DF = 2 ----

### ---- 2.2.1 Load the data ----

load("data/chap6/PMDF2.RData") #load each database
data <- PMDF2

### ---- 2.2.2 Probabilities transition matrix and distance matrix ----

Prob <- NULL
for(d in 1:(nrow(data)/2)){
  p1 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d - 1), 1:tm])), as.numeric(unlist(data[(2*d), 1:tm])))))
  p2 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d), 1:tm])), as.numeric(unlist(data[(2*d - 1), 1:tm])))))
  Prob <- rbind(rbind(Prob, p1), p2)
}

dissMat <- dist(as.data.frame(Prob))

### ---- 2.2.3 MDS ----

cmd <- cmdscale(dissMat, eig=TRUE, k=2)

x <- cmd$points[,1]
y <- cmd$points[,2]

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

### ---- 2.2.4 Clustering ----

res <- hclust(dissMat, method = "ward.D2")

plot(res, labels = FALSE, hang = -1, xlab = "Clusters",
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

optimalNb <- 2

clust <- cutree(res, optimalNb)

data <- cbind(data, cluster = clust)

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     col = c("red", "blue")[data$cluster],
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

table(data$cluster, data$case)

### ---- 2.2.5 Mean transition matrix ----

completeData <- cbind(data, as.data.frame(Prob))

meanTPC1 <- matrix(apply(completeData[completeData$cluster==1,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC2 <- matrix(apply(completeData[completeData$cluster==2,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC1
meanTPC2

# ---- 3. Illustration ----

## ---- 3.0 Load the data ----

load("data/chap6/dataIllustration.RData")

## ---- 3.1 Pattern identification ----

patternTest <- vector()

for(i in unique(data$dyad)){

  fm <- as.numeric(data[data$dyad==i & data$members=="FM",1:tm])
  sm <- as.numeric(data[data$dyad==i & data$members=="SM",1:tm])

  fm.test <- univariatePattern(states = s, chainFM = fm, chainSM = sm, alpha = 0.05)
  sm.test <- univariatePattern(states = s, chainFM = sm, chainSM = fm, alpha = 0.05)

  patternTest <- append(patternTest, c(fm.test$pattern, sm.test$pattern))

}

patternTest[1:60]
patternTest[61:120]
patternTest[121:180]
patternTest[181:240]

data$patternTest <- patternTest

table(data$patternTest)

## ---- 3.2 Probabilities transition matrix and distance matrix ----

Prob <- NULL
for(d in 1:(nrow(data)/2)){
  p1 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d - 1), 1:tm])), as.numeric(unlist(data[(2*d), 1:tm])))))
  p2 <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[(2*d), 1:tm])), as.numeric(unlist(data[(2*d - 1), 1:tm])))))
  Prob <- rbind(rbind(Prob, p1), p2)
}

dissMat <- dist(as.data.frame(Prob))

## ---- 3.3 MDS ----

cmd <- cmdscale(dissMat, eig = TRUE, k = 2)

x <- cmd$points[,1]
y <- cmd$points[,2]

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5)

## ---- 3.4 Clustering ----

res <- hclust(dissMat, method = "ward.D2")

plot(res, labels = FALSE, hang = -1, xlab = "Clusters",
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

opt <- NbClust(as.data.frame(Prob), dist(as.data.frame(Prob), method = "euclidean"), distance = NULL, method = "ward.D2", index = "ch")
nbclustOPT <- opt$Best.nc[1]
nbclustOPT
optimalNb <- 5

clust <- cutree(res, optimalNb)

data <- cbind(data, cluster = clust)

table(data$cluster, data$patternTest)

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     col = c("red", "blue", "green", "purple", "pink")[data$cluster],
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5)
legend(x = "topright",
       legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"),
       col = c("red", "blue", "green", "purple", "pink"), pch = 16)

## ---- 3.5 Mean transition matrix ----

completeData <- cbind(data, as.data.frame(Prob))
str(completeData)

meanTPC1 <- matrix(apply(completeData[completeData$cluster==1,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC2 <- matrix(apply(completeData[completeData$cluster==2,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC3 <- matrix(apply(completeData[completeData$cluster==3,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC4 <- matrix(apply(completeData[completeData$cluster==4,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)
meanTPC5 <- matrix(apply(completeData[completeData$cluster==5,(ncol(completeData)-8+1):ncol(completeData)], 2, FUN = mean),
                   ncol = 2, byrow = FALSE)

round(meanTPC1, digits = 3)
round(meanTPC2, digits = 3)
round(meanTPC3, digits = 3)
round(meanTPC4, digits = 3)
round(meanTPC5, digits = 3)

