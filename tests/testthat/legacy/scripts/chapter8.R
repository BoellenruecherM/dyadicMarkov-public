# ====================================================
# Chapter 8 : Extension I: Stress communication and Dyadic Coping
# ====================================================

# ---- 0. Load the data and set the parameter ----

load("tests/testthat/legacy/data/chap8/data.RData")
s <- 2

# ---- 1. Function to compute theoretical APM ----

countTheoAPM <- function(empirical){
  gamma <- rowSums(empirical)
  countTheo <- matrix(nrow = 4, ncol = 2)
  for(i in 1:nrow(empirical)){
    countTheo[i,] <- (gamma[i]*empirical[i,])
  }
  countTheo[is.nan(countTheo)] <- 0
  return(countTheo)
}

# ---- 2. Pattern identification ----

patternTest <- NULL

for(i in unique(df$dyad)){

  fm <- as.numeric(df[df$dyad==i & df$members=="FM",1:48])
  sm <- as.numeric(df[df$dyad==i & df$members=="SM",1:48])

  fm.test <- univariatePattern(states = s, chainFM = fm, chainSM = sm, alpha = 0.05)
  sm.test <- univariatePattern(states = s, chainFM = sm, chainSM = fm, alpha = 0.05)

  patternTest <- rbind(patternTest, c(fm.test$pattern, sm.test$pattern))

}

table(patternTest[, 1])
table(patternTest[, 2])

table(patternTest[, 1], patternTest[, 2])

# ---- 3. Probabilities transition matrix, distance matrix and MDS ----

## ---- 3.1 Probabilities transition matrix ----

Prob <- NULL
for(i in unique(df$dyad)){
  pFM <- c(mleEstimation(countEmp(s, as.numeric(unlist(df[df$dyad==i & df$members=="FM", 1:48])), as.numeric(unlist(df[df$dyad==i & df$members=="SM", 1:48])))))
  pSM <- c(mleEstimation(countEmp(s, as.numeric(unlist(df[df$dyad==i & df$members=="SM", 1:48])), as.numeric(unlist(df[df$dyad==i & df$members=="FM", 1:48])))))
  Prob <- rbind(rbind(Prob, pFM), pSM)
}

## ---- 3.2 Analysis of the women ----

PFM <- Prob[seq(1, 128, by = 2),] #first member = woman

pFM <- unlist(PFM)

Prob1 <- data.frame(PFM, pattern = patternTest[, 1], dyad = df$dyad[1:64])
ord <- order(Prob1$pattern, Prob1$X1, Prob1$X2)
Prob1.ord <- Prob1[ord,]
Prob1.ord

dissMat1 <- dist(as.data.frame(Prob1[,1:8]))
cmd1 <- cmdscale(dissMat1, eig = TRUE, k = 2)
x1 <- cmd1$points[,1]
y1 <- cmd1$points[,2]
plot(x1, y1, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1, col = as.factor(Prob1$pattern),
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")
legend("topright",
       legend = levels(as.factor(Prob1$pattern)),
       col = 1:length(levels(as.factor(Prob1$pattern))),
       pch = 16,
       title = "Patterns")

## ---- 3.3 Analysis of the men ----

PSM <- Prob[seq(2, 128, by = 2),] #second member = man

pSM <- unlist(PSM)

Prob2 <- data.frame(PSM, pattern = patternTest[, 2], dyad = df$dyad[1:64])
ord <- order(Prob2$pattern, Prob2$X1, Prob2$X2)
Prob2.ord <- Prob2[ord,]
Prob2.ord

dissMat2 <- dist(as.data.frame(Prob2[,1:8]))
cmd2 <- cmdscale(dissMat2, eig = TRUE, k = 2)
x2 <- cmd2$points[,1]
y2 <- cmd2$points[,2]
plot(x2, y2, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1, col = as.factor(Prob2$pattern),
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")
legend("topright",
       legend = levels(as.factor(Prob2$pattern)),
       col = 1:length(levels(as.factor(Prob2$pattern))),
       pch = 16,
       title = "Patterns")

# ---- 4. Pattern analysis for the women ----

## ---- 4.1 Actor-only pattern ----

### ---- 4.1.1 Empirical matrix ----

Prob1.AM.EMP <- Prob1[Prob1[, "pattern"] == "AM (A2)",]

Prob1.AM.EMP$mat <- rep("emp", nrow(Prob1.AM.EMP))

### ---- 4.1.2 Theoretical matrix ----

Prob1.AM.THEO <- matrix(NA, nrow = nrow(Prob1.AM.EMP), ncol = 8, byrow = TRUE)

for(i in 1:nrow(Prob1.AM.EMP)){
  emp <- matrix(unlist(as.numeric(Prob1.AM.EMP[i,c(1:8)])), ncol = s)
  theo <- countTheo(empirical = emp, pattern = "AM")
  Prob1.AM.THEO[i,] <- as.numeric(theo)
} #computation of theoretical matrices

Prob1.AM.THEO <- data.frame(Prob1.AM.THEO)
Prob1.AM.THEO$pattern <- rep("AM (A2)", nrow(Prob1.AM.THEO))
Prob1.AM.THEO$dyad <- Prob1.AM.EMP$dyad
Prob1.AM.THEO$mat <- rep("theo", nrow(Prob1.AM.THEO))

### ---- 4.1.3 MDS ----

#theoretical representation
dissMat1 <- dist(Prob1.AM.THEO[,1:8])
cmd1 <- cmdscale(dissMat1, eig = TRUE, k = 2)
x1 <- cmd1$points[,1]
y1 <- cmd1$points[,2]
plot(x1, y1, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

ind_left <- which.min(x1)
val_left <- x1[ind_left]
ind_left
val_left
ind_right <- which.max(x1)
val_right <- x1[ind_right]
ind_right
val_right
matrix(Prob1.AM.THEO[ind_left,1:8], ncol = 2, byrow = FALSE)
matrix(Prob1.AM.THEO[ind_right,1:8], ncol = 2, byrow = FALSE)

#EMP + THEO graphical representation
Prob1.AM <- rbind(Prob1.AM.EMP, Prob1.AM.THEO)
dissMat1 <- dist(Prob1.AM[,1:8])
cmd1 <- cmdscale(dissMat1, eig = TRUE, k = 2)
x1 <- cmd1$points[,1]
y1 <- cmd1$points[,2]

n <- length(x1)/2

plot(x1, y1, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")
points(x1[1:n], y1[1:n], type="p", col="red") #red for EMP
points(x1[(n + 1):(2*n)], y1[(n + 1):(2*n)], type="p", col="green") #green for THEO
arrows(x1[1:n], y1[1:n], x1[(n + 1):(2*n)], y1[(n + 1):(2*n)], length = 0.1, col = "blue", lwd = 0.3) #arrows

### ---- 4.1.4 PCA and axes ----

DF <- data.frame(
  p1 = Prob1.AM.THEO$X1, #p1
  p2 = Prob1.AM.THEO$X7, #p2
  x1 = x1[(n + 1):(2*n)], #theoretical points in X (women AM)
  y1 = y1[(n + 1):(2*n)]) #theoretical points in Y (women AM)

res <- PCA(DF, graph = FALSE)
fviz_eig(res, addlabels = TRUE, ylim = c(0,100)) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = "", subtitle = "")

plot.PCA(res, choix = "var",
         cex.lab = 1.5,
         cex.axis = 1.2,
         cex.main = 1.5,
         sub = "",
         main = "")
plot.PCA(res, choix = "ind",
         cex.lab = 1.5,
         cex.axis = 1.2,
         cex.main = 1.5,
         sub = "",
         main = "")

fit1 <- lm(x1 ~ p1 + p2, data = DF)
summary(fit1)

fit2 <- lm(y1 ~ p1 + p2, data = DF)
summary(fit2)

s3d <- scatterplot3d(z = DF$x1, x = DF$p1, y = DF$p2, type = "h", pch = 16,
                     xlab = "p1", ylab = "p2", zlab = "x1")
s3d$plane3d(fit1)

s3d <- scatterplot3d(z = DF$y1, x = DF$p1, y = DF$p2, type = "h", pch = 16,
                     xlab = "p1", ylab = "p2", zlab = "y1")
s3d$plane3d(fit2)

## ---- 4.2 Partner-only pattern ----

### ---- 4.2.1 Empirical matrix ----

Prob1.PM.EMP <- Prob1[Prob1[, "pattern"] == "PM (A3)",]

Prob1.PM.EMP$mat <- rep("emp", nrow(Prob1.PM.EMP))

### ---- 4.2.2 Theoretical matrix ----

Prob1.PM.THEO <- matrix(NA, nrow = nrow(Prob1.PM.EMP), ncol = 8, byrow = TRUE)

for(i in 1:nrow(Prob1.PM.EMP)){
  emp <- matrix(unlist(as.numeric(Prob1.PM.EMP[i,c(1:8)])), ncol = s)
  theo <- countTheo(empirical = emp, pattern = "PM")
  Prob1.PM.THEO[i,] <- as.numeric(theo)
}

Prob1.PM.THEO <- data.frame(Prob1.PM.THEO)
Prob1.PM.THEO$pattern <- rep("PM (A3)", nrow(Prob1.PM.THEO))
Prob1.PM.THEO$dyad <- Prob1.PM.EMP$dyad
Prob1.PM.THEO$mat <- rep("theo", nrow(Prob1.PM.THEO))

### ---- 4.2.3 MDS ----

dissMat1 <- dist(Prob1.PM.THEO[,1:8])
cmd1 <- cmdscale(dissMat1, eig = TRUE, k = 2)
x1 <- cmd1$points[,1]
y1 <- cmd1$points[,2]
plot(x1, y1, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

ind_left <- which.min(x1)
val_left <- x1[ind_left]
ind_left
val_left
ind_right <- which.max(x1)
val_right <- x1[ind_right]
ind_right
val_right
matrix(Prob1.PM.THEO[ind_left,1:8], ncol = 2, byrow = FALSE)
matrix(Prob1.PM.THEO[ind_right,1:8], ncol = 2, byrow = FALSE)

Prob1.PM <- rbind(Prob1.PM.EMP, Prob1.PM.THEO)
dissMat1 <- dist(Prob1.PM[,1:8])
cmd1 <- cmdscale(dissMat1, eig = TRUE, k = 2)
x1 <- cmd1$points[,1]
y1 <- cmd1$points[,2]

n <- length(x1)/2

plot(x1, y1, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")
points(x1[1:n], y1[1:n], type="p", col="red")
points(x1[(n + 1):(2*n)], y1[(n + 1):(2*n)], type="p", col="green")
arrows(x1[1:n], y1[1:n], x1[(n + 1):(2*n)], y1[(n + 1):(2*n)], length = 0.1, col = "blue", lwd = 0.3)

### ---- 4.2.4 PCA and axes ----

DF <- data.frame(
  p1 = Prob1.PM.THEO$X1, #p1
  p2 = Prob1.PM.THEO$X6, #p2
  x1 = x1[(n + 1):(2*n)], #theoretical points in X (women PM)
  y1 = y1[(n + 1):(2*n)]) #theoretical points in Y (women PM)

res <- PCA(DF, graph = FALSE)
fviz_eig(res, addlabels = TRUE, ylim = c(0,100)) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = "", subtitle = "")

plot.PCA(res, choix = "var",
         cex.lab = 1.5,
         cex.axis = 1.2,
         cex.main = 1.5,
         sub = "",
         main = "")
plot.PCA(res, choix = "ind",
         cex.lab = 1.5,
         cex.axis = 1.2,
         cex.main = 1.5,
         sub = "",
         main = "")

fit1 <- lm(x1 ~ p1 + p2, data = DF)
summary(fit1)

fit2 <- lm(y1 ~ p1 + p2, data = DF)
summary(fit2)

s3d <- scatterplot3d(z = DF$x1, x = DF$p1, y = DF$p2, type = "h", pch = 16,
                     xlab = "p1", ylab = "p2", zlab = "x1")
s3d$plane3d(fit1)

s3d <- scatterplot3d(z = DF$y1, x = DF$p1, y = DF$p2, type = "h", pch = 16,
                     xlab = "p1", ylab = "p2", zlab = "y1")
s3d$plane3d(fit2)

## ---- 4.3 Actor-partner pattern ----

### ---- 4.3.1 Empirical matrix ----

Prob1.APM.EMP <- Prob1[Prob1[, "pattern"] == "APM (A1)",]
Prob1.APM.EMP$mat <- rep("emp", nrow(Prob1.APM.EMP))

### ---- 4.3.2 Theoretical matrix ----

Prob1.APM.THEO <- matrix(NA, nrow = nrow(Prob1.APM.EMP), ncol = 8, byrow = TRUE) #matrice vide

for(i in 1:nrow(Prob1.APM.EMP)){
  emp <- matrix(unlist(as.numeric(Prob1.APM.EMP[i,c(1:8)])), ncol = s)
  theo <- countTheoAPM(empirical = emp)
  Prob1.APM.THEO[i,] <- as.numeric(theo)
}

Prob1.APM.THEO <- data.frame(Prob1.APM.THEO)
Prob1.APM.THEO$pattern <- rep("APM (A1)", nrow(Prob1.APM.THEO))
Prob1.APM.THEO$dyad <- Prob1.APM.EMP$dyad
Prob1.APM.THEO$mat <- rep("theo", nrow(Prob1.APM.THEO))

### ---- 4.3.3 MDS ----

dissMat1 <- dist(Prob1.APM.THEO[,1:8])
cmd1 <- cmdscale(dissMat1, eig = TRUE, k = 2)
x1 <- cmd1$points[,1]
y1 <- cmd1$points[,2]
plot(x1, y1, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

ind_left <- which.min(x1)
val_left <- x1[ind_left]
ind_left
val_left
ind_right <- which.max(x1)
val_right <- x1[ind_right]
ind_right
val_right
matrix(Prob1.APM.THEO[ind_left,1:8], ncol = 2, byrow = FALSE)
matrix(Prob1.APM.THEO[ind_right,1:8], ncol = 2, byrow = FALSE)

Prob1.APM <- rbind(Prob1.APM.EMP, Prob1.APM.THEO)
dissMat1 <- dist(Prob1.APM[,1:8])
cmd1 <- cmdscale(dissMat1, eig = TRUE, k = 4)
x1 <- cmd1$points[,1]
y1 <- cmd1$points[,2]
z1 <- cmd1$points[,3]
w1 <- cmd1$points[,4]

n <- length(x1)/2

plot(x1, y1, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")
points(x1[1:n], y1[1:n], type="p", col="red")
points(x1[(n + 1):(2*n)], y1[(n + 1):(2*n)], type="p", col="green")
arrows(x1[1:n], y1[1:n], x1[(n + 1):(2*n)], y1[(n + 1):(2*n)], length = 0.1, col = "blue", lwd = 0.3)

### ---- 4.3.4 PCA and axes ----

DF <- data.frame(
  p1 = Prob1.APM.THEO$X1, #p1
  p2 = Prob1.APM.THEO$X2, #p2
  p3 = Prob1.APM.THEO$X3, #p3
  p4 = Prob1.APM.THEO$X8, #p4
  x1 = x1[(n + 1):(2*n)], #theoretical points in X (women APM)
  y1 = y1[(n + 1):(2*n)], #theoretical points in Y (women APM)
  z1 = z1[(n + 1):(2*n)], #theoretical points in Z (women APM)
  w1 = w1[(n + 1):(2*n)]) #theoretical points in W (women APM)

res <- PCA(DF, graph = FALSE)
fviz_eig(res, addlabels = TRUE, ylim = c(0,100)) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = "", subtitle = "")

plot.PCA(res, choix = "var",
         cex.lab = 1.5,
         cex.axis = 1.2,
         cex.main = 1.5,
         sub = "",
         main = "")
plot.PCA(res, choix = "ind",
         cex.lab = 1.5,
         cex.axis = 1.2,
         cex.main = 1.5,
         sub = "",
         main = "")

fit1 <- lm(x1 ~ p1 + p2 + p3 + p4, data = DF)
summary(fit1)

fit2 <- lm(y1 ~ p1 + p2 + p3 + p4, data = DF)
summary(fit2)

fit3 <- lm(z1 ~ p1 + p2 + p3 + p4, data = DF)
summary(fit3)

fit4 <- lm(w1 ~ p1 + p2 + p3 + p4, data = DF)
summary(fit4)

# ---- 5. Pattern analysis for the women ----

## ---- 5.1 Actor-only pattern ----

### ---- 5.1.1 Empirical matrix ----

Prob2.AM.EMP <- Prob2[Prob2[, "pattern"] == "AM (A2)",]

Prob2.AM.EMP$mat <- rep("emp", nrow(Prob2.AM.EMP))

### ---- 5.1.2 Theoretical matrix ----

Prob2.AM.THEO <- matrix(NA, nrow = nrow(Prob2.AM.EMP), ncol = 8, byrow = TRUE)

for(i in 1:nrow(Prob2.AM.EMP)){
  emp <- matrix(unlist(as.numeric(Prob2.AM.EMP[i,c(1:8)])), ncol = s)
  theo <- countTheo(empirical = emp, pattern = "AM")
  Prob2.AM.THEO[i,] <- as.numeric(theo)
}

Prob2.AM.THEO <- data.frame(Prob2.AM.THEO)
Prob2.AM.THEO$pattern <- rep("AM (A2)", nrow(Prob2.AM.THEO))
Prob2.AM.THEO$dyad <- Prob2.AM.EMP$dyad
Prob2.AM.THEO$mat <- rep("theo", nrow(Prob2.AM.THEO))

### ---- 5.1.3 MDS ----

dissMat2 <- dist(Prob2.AM.THEO[,1:8])
cmd2 <- cmdscale(dissMat2, eig = TRUE, k = 2)
x2 <- cmd2$points[,1]
y2 <- cmd2$points[,2]
plot(x2, y2, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

ind_left <- which.min(x2)
val_left <- x2[ind_left]
ind_left
val_left
ind_right <- which.max(x2)
val_right <- x2[ind_right]
ind_right
val_right
matrix(Prob2.AM.THEO[ind_left,1:8], ncol = 2, byrow = FALSE)
matrix(Prob2.AM.THEO[ind_right,1:8], ncol = 2, byrow = FALSE)

Prob2.AM <- rbind(Prob2.AM.EMP, Prob2.AM.THEO)
dissMat2 <- dist(Prob2.AM[,1:8])
cmd2 <- cmdscale(dissMat2, eig = TRUE, k = 2)
x2 <- cmd2$points[,1]
y2 <- cmd2$points[,2]

n <- length(x2)/2

plot(x2, y2, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")
points(x2[1:n], y2[1:n], type="p", col="red")
points(x2[(n + 1):(2*n)], y2[(n + 1):(2*n)], type="p", col="green")
arrows(x2[1:n], y2[1:n], x2[(n + 1):(2*n)], y2[(n + 1):(2*n)], length = 0.1, col = "blue", lwd = 0.3)

### ---- 5.1.4 PCA and axes ----

DF <- data.frame(
  p1 = Prob2.AM.THEO$X1, #p1
  p2 = Prob2.AM.THEO$X7, #p2
  x2 = x2[(n + 1):(2*n)], #theoretical points in X (men AM)
  y2 = y2[(n + 1):(2*n)]) #theoretical points in Y (men AM)

res <- PCA(DF, graph = FALSE)
fviz_eig(res, addlabels = TRUE, ylim = c(0,100)) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = "", subtitle = "")

plot.PCA(res, choix = "var",
         cex.lab = 1.5,
         cex.axis = 1.2,
         cex.main = 1.5,
         sub = "",
         main = "")
plot.PCA(res, choix = "ind",
         cex.lab = 1.5,
         cex.axis = 1.2,
         cex.main = 1.5,
         sub = "",
         main = "")

fit1 <- lm(x2 ~ p1 + p2, data = DF)
summary(fit1)

fit2 <- lm(y2 ~ p1 + p2, data = DF)
summary(fit2)

s3d <- scatterplot3d(z = DF$x2, x = DF$p1, y = DF$p2, type = "h", pch = 16,
                     xlab = "p1", ylab = "p2", zlab = "x1")
s3d$plane3d(fit1)

s3d <- scatterplot3d(z = DF$y2, x = DF$p1, y = DF$p2, type = "h", pch = 16,
                     xlab = "p1", ylab = "p2", zlab = "y1")
s3d$plane3d(fit2)

## ---- 5.2 Partner-only pattern ----

### ---- 5.2.1 Empirical matrix ----

Prob2.PM.EMP <- Prob2[Prob2[, "pattern"] == "PM (A3)",]

Prob2.PM.EMP$mat <- rep("emp", nrow(Prob2.PM.EMP))

### ---- 5.2.2 Theoretical matrix ----

Prob2.PM.THEO <- matrix(NA, nrow = nrow(Prob2.PM.EMP), ncol = 8, byrow = TRUE)

for(i in 1:nrow(Prob2.PM.EMP)){
  emp <- matrix(unlist(as.numeric(Prob2.PM.EMP[i,c(1:8)])), ncol = s)
  theo <- countTheo(empirical = emp, pattern = "PM")
  Prob2.PM.THEO[i,] <- as.numeric(theo)
}

Prob2.PM.THEO <- data.frame(Prob2.PM.THEO)
Prob2.PM.THEO$pattern <- rep("PM (A3)", nrow(Prob2.PM.THEO))
Prob2.PM.THEO$dyad <- Prob2.PM.EMP$dyad
Prob2.PM.THEO$mat <- rep("theo", nrow(Prob2.PM.THEO))

### ---- 5.2.3 MDS ----

dissMat2 <- dist(Prob2.PM.THEO[,1:8])
cmd2 <- cmdscale(dissMat2, eig = TRUE, k = 2)
x2 <- cmd2$points[,1]
y2 <- cmd2$points[,2]
plot(x2, y2, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

ind_left <- which.min(x2)
val_left <- x2[ind_left]
ind_left
val_left
ind_right <- which.max(x2)
val_right <- x2[ind_right]
ind_right
val_right
matrix(Prob2.PM.THEO[ind_left,1:8], ncol = 2, byrow = FALSE)
matrix(Prob2.PM.THEO[ind_right,1:8], ncol = 2, byrow = FALSE)

Prob2.PM <- rbind(Prob2.PM.EMP, Prob2.PM.THEO)
dissMat2 <- dist(Prob2.PM[,1:8])
cmd2 <- cmdscale(dissMat2, eig = TRUE, k = 2)
x2 <- cmd2$points[,1]
y2 <- cmd2$points[,2]

n <- length(x2)/2

plot(x2, y2, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1)
points(x2[1:n], y2[1:n], type="p", col="red")
points(x2[(n + 1):(2*n)], y2[(n + 1):(2*n)], type="p", col="green")
arrows(x2[1:n], y2[1:n], x2[(n + 1):(2*n)], y2[(n + 1):(2*n)], length = 0.1, col = "blue", lwd = 0.3)

### ---- 5.2.4 PCA and axes ----

DF <- data.frame(
  p1 = Prob2.PM.THEO$X1, #p1
  p2 = Prob2.PM.THEO$X6, #p2
  x2 = x2[(n + 1):(2*n)], #theoretical points in X (men PM)
  y2 = y2[(n + 1):(2*n)]) #theoretical points in Y (men PM)

res <- PCA(DF, graph = FALSE)
fviz_eig(res, addlabels = TRUE, ylim = c(0,100)) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = "", subtitle = "")

plot.PCA(res, choix = "var",
         cex.lab = 1.5,
         cex.axis = 1.2,
         cex.main = 1.5,
         sub = "",
         main = "")
plot.PCA(res, choix = "ind",
         cex.lab = 1.5,
         cex.axis = 1.2,
         cex.main = 1.5,
         sub = "",
         main = "")

fit1 <- lm(x2 ~ p1 + p2, data = DF)
summary(fit1)

fit2 <- lm(y2 ~ p1 + p2, data = DF)
summary(fit2)

s3d <- scatterplot3d(z = DF$x2, x = DF$p1, y = DF$p2, type = "h", pch = 16,
                     xlab = "p1", ylab = "p2", zlab = "x1")
s3d$plane3d(fit1)

s3d <- scatterplot3d(z = DF$y2, x = DF$p1, y = DF$p2, type = "h", pch = 16,
                     xlab = "p1", ylab = "p2", zlab = "y1")
s3d$plane3d(fit2)

## ---- 5.3 Actor-partner pattern ----

### ---- 5.3.1 Empirical matrix ----

Prob2.APM.EMP <- Prob2[Prob2[, "pattern"] == "APM (A1)",]

Prob2.APM.EMP$mat <- rep("emp", nrow(Prob2.APM.EMP))

### ---- 5.3.2 Theoretical matrix ----

Prob2.APM.THEO <- matrix(NA, nrow = nrow(Prob2.APM.EMP), ncol = 8, byrow = TRUE) #matrice vide

for(i in 1:nrow(Prob2.APM.EMP)){
  emp <- matrix(unlist(as.numeric(Prob2.APM.EMP[i,c(1:8)])), ncol = s)
  theo <- countTheoAPM(empirical = emp)
  Prob2.APM.THEO[i,] <- as.numeric(theo)
}

Prob2.APM.THEO <- data.frame(Prob2.APM.THEO)
Prob2.APM.THEO$pattern <- rep("APM (A1)", nrow(Prob2.APM.THEO))
Prob2.APM.THEO$dyad <- Prob2.APM.EMP$dyad
Prob2.APM.THEO$mat <- rep("theo", nrow(Prob2.APM.THEO))

### ---- 5.3.3 MDS ----

dissMat2 <- dist(Prob2.APM.THEO[,1:8])
cmd2 <- cmdscale(dissMat2, eig = TRUE, k = 2)
x2 <- cmd2$points[,1]
y2 <- cmd2$points[,2]
plot(x2, y2, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")

ind_left <- which.min(x2)
val_left <- x2[ind_left]
ind_left
val_left
ind_right <- which.max(x2)
val_right <- x2[ind_right]
ind_right
val_right
matrix(Prob2.APM.THEO[ind_left,1:8], ncol = 2, byrow = FALSE)
matrix(Prob2.APM.THEO[ind_right,1:8], ncol = 2, byrow = FALSE)

Prob2.APM <- rbind(Prob2.APM.EMP, Prob2.APM.THEO)
dissMat2 <- dist(Prob2.APM[,1:8])
cmd2 <- cmdscale(dissMat2, eig = TRUE, k = 4)
x2 <- cmd2$points[,1]
y2 <- cmd2$points[,2]
z2 <- cmd2$points[,3]
w2 <- cmd2$points[,4]

n <- length(x2)/2

plot(x2, y2, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1,
     cex.lab = 1.5,
     cex.axis = 1.2,
     cex.main = 1.5,
     sub = "",
     main = "")
points(x2[1:n], y2[1:n], type="p", col="red")
points(x2[(n + 1):(2*n)], y2[(n + 1):(2*n)], type="p", col="green")
arrows(x2[1:n], y2[1:n], x2[(n + 1):(2*n)], y2[(n + 1):(2*n)], length = 0.1, col = "blue", lwd = 0.3)

### ---- 5.3.4 PCA and axes ----

DF <- data.frame(
  p1 = Prob2.APM.THEO$X1, #p1
  p2 = Prob2.APM.THEO$X2, #p2
  p3 = Prob2.APM.THEO$X3, #p3
  p4 = Prob2.APM.THEO$X8, #p4
  x2 = x2[(n + 1):(2*n)], #theoretical points in X (men APM)
  y2 = y2[(n + 1):(2*n)], #theoretical points in Y (men APM)
  z2 = z2[(n + 1):(2*n)], #theoretical points in Z (men APM)
  w2 = w2[(n + 1):(2*n)]) #theoretical points in W (men APM)

res <- PCA(DF, graph = FALSE)
fviz_eig(res, addlabels = TRUE, ylim = c(0,100)) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = "", subtitle = "")

plot.PCA(res, choix = "var",
         cex.lab = 1.5,
         cex.axis = 1.2,
         cex.main = 1.5,
         sub = "",
         main = "")
plot.PCA(res, choix = "ind",
         cex.lab = 1.5,
         cex.axis = 1.2,
         cex.main = 1.5,
         sub = "",
         main = "")

fit1 <- lm(x2 ~ p1 + p2 + p3 + p4, data = DF)
summary(fit1)

fit2 <- lm(y2 ~ p1 + p2 + p3 + p4, data = DF)
summary(fit2)

fit3 <- lm(z2 ~ p1 + p2 + p3 + p4, data = DF)
summary(fit3)

fit4 <- lm(w2 ~ p1 + p2 + p3 + p4, data = DF)
summary(fit4)
