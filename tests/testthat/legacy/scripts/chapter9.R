# ====================================================
# Chapter 9 : Independence Pattern 
# ====================================================

# ---- 0. Load the data and set the parameter ----

load("data/chap9/diary.RData")
s <- 2

# ---- 1. Pattern identification ----

patternTest <- NULL

for(i in unique(data$dyad)){
  
  fm <- as.numeric(data[data$dyad==i & data$members=="FM",3:30])
  sm <- as.numeric(data[data$dyad==i & data$members=="SM",3:30])
  
  fm.test <- univariatePattern(states = s, chainFM = fm, chainSM = sm, alpha = 0.05)
  sm.test <- univariatePattern(states = s, chainFM = sm, chainSM = fm, alpha = 0.05)
  
  patternTest <- rbind(patternTest, c(fm.test$pattern, sm.test$pattern))
  
}

table(patternTest)

# ---- 2. Probabilities transition matrix and distance matrix ----

Prob <- NULL
for(i in unique(data$dyad)){
  pFM <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[data$dyad==i & data$members=="FM", 3:30])), as.numeric(unlist(data[data $dyad==i & data $members=="SM", 3:30])))))
  pSM <- c(mleEstimation(countEmp(s, as.numeric(unlist(data[data$dyad==i & data$members=="SM", 3:30])), as.numeric(unlist(data[data$dyad==i & data$members=="FM", 3:30])))))
  Prob <- rbind(rbind(Prob, pFM), pSM)
}
Prob <- data.frame(Prob, pattern = c(t(patternTest)), members = data$members, dyad = data$dyad) #base de données avec les probabilités des hommes uniquement 

dissMat <- dist(Prob[,1:8])

# ---- 3. MDS ----

cmd <- cmdscale(dissMat, eig = TRUE, k = 2)
x <- cmd$points[,1]
y <- cmd$points[,2]

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", asp = 1, 
     cex.lab = 1.5, 
     cex.axis = 1.2, 
     cex.main = 1.5,
     sub = "", 
     main = "") 

# ---- 4. Analysis of IM  ----

## ---- 4.1 Database ----

group <- y
group[y  >= .3] <- 1
group[y  < -.65] <- -1
group[y < .3 & y >= -.65] <- 0

df <- data.frame(
  x = x,
  y = y,
  group = group
)

data <- cbind(data, df, Prob[,1:9])
str(data)
table(data$members, data$pattern)
dataIM <- data[data$pattern == "IM (A0)", ] #remove the individuals with others patterns than IM 
str(dataIM)

## ---- 4.2 Graphical representation ----

plot(x, y, col = factor(group), 
     cex.lab = 1.5, 
     cex.axis = 1.2, 
     cex.main = 1.5,
     sub = "", 
     main = "")

## ---- 4.3 Mean transition matrix ----

meanTPM1 <- matrix(apply(dataIM[dataIM$group ==  1, 34:41], 2, mean), 
                   ncol = 2, byrow = FALSE)

meanTP0 <- matrix(apply(dataIM[dataIM$group ==  0, 34:41], 2, mean), 
                  ncol = 2, byrow = FALSE)

meanTP1 <- matrix(apply(dataIM[dataIM$group == -1, 34:41], 2, mean), 
                  ncol = 2, byrow = FALSE)

round(meanTPM1,3)
round(meanTP0,3)
round(meanTP1,3)

round(meanTPM1,2)
round(meanTP0,2)
round(meanTP1,2)

## ---- 4.4 Effective analysis ----

#computation of the effective 
effectif <- NULL
for(i in unique(data$dyad)){
  pFM <- c(countEmp(s, as.numeric(unlist(data[data$dyad==i & data$members=="FM", 3:30])), as.numeric(unlist(data[data$dyad==i & data$members=="SM", 3:30]))))
  pSM <- c(countEmp(s, as.numeric(unlist(data[data$dyad==i & data$members=="SM", 3:30])), as.numeric(unlist(data[data$dyad==i & data$members=="FM", 3:30]))))
  effectif <- rbind(rbind(effectif, pFM), pSM)
}

apply(effectif, 1, which.max)
table(apply(effectif, 1, which.max))

data <- cbind(data, effectif)
dataIM1 <- data[data$pattern == "IM (A0)", ] #remove the individuals with others patterns than IM 
str(dataIM1)

#IM theorerical transition matrix 
n1 <- apply(dataIM1[, c("1", "2", "3", "4")], 1, sum)
n2 <- apply(dataIM1[, c("5", "6", "7", "8")], 1, sum)
p1 <- n1/(n1 + n2)
p2 <- n2/(n1 + n2)
Prob1 <- data.frame(p1, p1, p1, p1, p2, p2, p2, p2)

#graphic representation of the theoretical matrix IM 
dissMat1 <- dist(Prob1)
cmd1 <- cmdscale(dissMat1, eig = TRUE, k = 2)
x <- cmd1$points[,1]
y <- cmd1$points[,2]
plot(x, jitter(y, amount = 0.1), 
     xlab = "Coordinate 1", 
     ylab = "Coordinate 2", 
     asp = 1, 
     cex.lab = 1.6, 
     cex.axis = 1.4)

#PCA and analysis 
rownames(dataIM1) <- 1:184
res <- PCA(dataIM1[,c("1", "2", "3", "4", "5", "6", "7", "8")], scale.unit = FALSE, graph = FALSE)
fviz_pca_ind(res, repel = TRUE) +
  theme(
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14),   
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = "", subtitle = "")
fviz_pca_ind(res, label = "none") +
  theme(
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14),   
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = "", subtitle = "")

fviz_pca_ind(res, repel = TRUE) +
  labs(x = "Dimension 1", y = "Dimension 2") +
  theme(
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14),   
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = "", subtitle = "")

fviz_contrib(res, choice = "ind", axes = 1, top = 15) +
  theme(
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14),   
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = "", subtitle = "")
fviz_contrib(res, choice = "ind", axes = 2, top = 15) +
  theme(
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14),   
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = "", subtitle = "")

coordDim1 <- res$ind$coord[, 1]
indPositifDim1 <- names(which.max(coordDim1))  
indPositifDim1
table(as.factor(unlist(dataIM1[27, 3:30])))
matrix(dataIM1[27, 43:50], ncol = 2, byrow = FALSE)
matrix(dataIM1[27, 34:41], ncol = 2, byrow = FALSE)
indNegatifDim1 <- names(which.min(coordDim1))
indNegatifDim1
table(as.factor(unlist(dataIM1[19, 3:30])))
matrix(dataIM1[19, 43:50], ncol = 2, byrow = FALSE)
matrix(dataIM1[19, 34:41], ncol = 2, byrow = FALSE)

coordDim2 <- res$ind$coord[, 2]
indPositifDim2 <- names(which.max(coordDim2))  
indPositifDim2
table(as.factor(unlist(dataIM1[13, 3:30])))  
matrix(dataIM1[13, 43:50], ncol = 2, byrow = FALSE)
matrix(dataIM1[13, 34:41], ncol = 2, byrow = FALSE)
indNegatifDim2 <- names(which.min(coordDim2))
indNegatifDim2
table(as.factor(unlist(dataIM1[86, 3:30]))) 
matrix(dataIM1[86, 43:50], ncol = 2, byrow = FALSE)
matrix(dataIM1[86, 34:41], ncol = 2, byrow = FALSE)

coord_ind <- res$ind$coord[, 1:2]  # Dim1 et Dim2
indCentre <- names(which.min(rowSums(coord_ind^2)))
indCentre
table(as.factor(unlist(dataIM1[49, 3:30]))) 
matrix(dataIM1[49, 43:50], ncol = 2, byrow = FALSE)
matrix(dataIM1[49, 34:41], ncol = 2, byrow = FALSE)

colors <- rep("gray", nrow(coord_ind))  
names(colors) <- rownames(coord_ind)    
colors[indCentre] <- "red"      
colors[indPositifDim1] <- "brown"   
colors[indNegatifDim1] <- "blue"  
colors[indPositifDim2] <- "purple" 
colors[indNegatifDim2] <- "orange" 

fviz_pca_ind(res, repel = TRUE, col.ind = colors, pointshape = 16) +  
  scale_color_identity(guide = "none") +
  theme(
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14),   
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = "", subtitle = "")

## ---- 4.5 Confidence interval ----

mat19 <- matrix(dataIM1[19, 43:50], ncol = 2, byrow = FALSE)
mat19 <- apply(mat19, 2, as.numeric)

#Column1
x1 <- mat19[,1]
ciCP1 <- binom.confint(x1, n, methods = "exact", conf.level = 0.95)
ciWilson1 <- binom.confint(x1, n, methods = "wilson", conf.level = 0.95)

df1 <- data.frame(
  row = factor(rep(1:length(x1), 2)),
  mean = c(ciCP1$mean, ciWilson1$mean),
  lower = c(ciCP1$lower, ciWilson1$lower),
  upper = c(ciCP1$upper, ciWilson1$upper),
  method = rep(c("Clopper-Pearson", "Wilson"), each = length(x1)),
  Column = "Column1"
)

#Column2
x2 <- mat19[,2]
ciCP2 <- binom.confint(x2, n, methods = "exact", conf.level = 0.95)
ciWilson2 <- binom.confint(x2, n, methods = "wilson", conf.level = 0.95)

df2 <- data.frame(
  row = factor(rep(1:length(x2), 2)),
  mean = c(ciCP2$mean, ciWilson2$mean),
  lower = c(ciCP2$lower, ciWilson2$lower),
  upper = c(ciCP2$upper, ciWilson2$upper),
  method = rep(c("Clopper-Pearson", "Wilson"), each = length(x2)),
  Column = "Column2"
)

plotDF <- rbind(df1, df2)
ggplot(plotDF, aes(x = row, y = mean, color = method)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.3),
                width = 0.2, size = 0.7) +  # lignes plus fines
  facet_grid(. ~ Column) +
  scale_color_manual(values = c("Clopper-Pearson" = "blue", "Wilson" = "gray50")) +
  labs(x = "Matrix rows",
       y = "Proportion") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(face = "bold")) +
  ylim(0, 1)
