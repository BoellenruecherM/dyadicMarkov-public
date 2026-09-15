# ====================================================
# Chapter 4 : Sensitive analysis
# ====================================================

# ---- 1. Univariate, S = 2 ----

## ---- 1.1 Load data  ----

load("data/chap4/amAccuracyHomoDF1_0.9.RData")
load("data/chap4/pmAccuracyHomoDF1_0.9.RData")
load("data/chap4/apmAccuracyHomoDF1_0.9.RData")
load("data/chap4/amAccuracyHomoDF1_0.6.RData")
load("data/chap4/pmAccuracyHomoDF1_0.6.RData")
load("data/chap4/apmAccuracyHomoDF1_0.6.RData")

load("data/chap4/amAccuracyHomoDF2_Alt.RData")
load("data/chap4/pmAccuracyHomoDF2_Alt.RData")
load("data/chap4/apmAccuracyHomoDF2.RData")

load("data/chap4/amAccuracyHeteroDF1_Alt.RData")
load("data/chap4/pmAccuracyHeteroDF1_Alt.RData")
load("data/chap4/apmAccuracyHeteroDF1.RData")

load("data/chap4/amAccuracyHeteroDF2_Alt.RData")
load("data/chap4/pmAccuracyHeteroDF2_Alt.RData")
load("data/chap4/apmAccuracyHeteroDF2.RData")

load("data/chap4/apmAccuracyHomoDF3.RData")
load("data/chap4/apmAccuracyHomoDF4.RData")

load("data/chap4/apmAccuracyHeteroDF13.RData")
load("data/chap4/apmAccuracyHeteroDF14.RData")

## ---- 1.2 RMSE analysis ----

### ---- 1.2.1 Graphical representation ----

#### ---- 1.2.1.1 Homogeneous case ----

#DF = 1

amAccuracyHomoDF1_0.9$Type_sim <- rep("Actor", nrow(amAccuracyHomoDF1_0.9))
pmAccuracyHomoDF1_0.9$Type_sim <- rep("Partner", nrow(pmAccuracyHomoDF1_0.9))
apmAccuracyHomoDF1_0.9$Type_sim <- rep("Actor Partner", nrow(apmAccuracyHomoDF1_0.9))

data <- rbind(amAccuracyHomoDF1_0.9, pmAccuracyHomoDF1_0.9, apmAccuracyHomoDF1_0.9)
data$Type_sim = factor(data$Type_sim, levels=c("Actor", "Partner", "Actor Partner")) #Set levels to chaange the order of the subplots

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_boxplot(show.legend = FALSE) +
  facet_grid(cols = vars(Type_sim)) +
  geom_hline(yintercept = 0.2, linetype="dashed", color = "blue") +
  ylim(0, 0.6) +
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_jitter(alpha = 0.4, width = 0.2, show.legend = FALSE) +  # Scatter points with slight jitter
  facet_grid(cols = vars(Type_sim)) +  # Facet by Type_sim
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "blue") + # Add horizontal threshold line
  stat_summary(fun = median, geom = "point", size = 3, color = "red") + # Add median points (red) and connect them with a line
  stat_summary(fun = median, geom = "line", aes(group = Type_sim), color = "red", linewidth = 1) +
  ylim(0, 1) + 
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

amAccuracyHomoDF1_0.6$Type_sim <- rep("Actor", nrow(amAccuracyHomoDF1_0.6))
pmAccuracyHomoDF1_0.6$Type_sim <- rep("Partner", nrow(pmAccuracyHomoDF1_0.6))
apmAccuracyHomoDF1_0.6$Type_sim <- rep("Actor Partner", nrow(apmAccuracyHomoDF1_0.6))

data <- rbind(amAccuracyHomoDF1_0.6, pmAccuracyHomoDF1_0.6, apmAccuracyHomoDF1_0.6)
data$Type_sim = factor(data$Type_sim, levels=c("Actor", "Partner", "Actor Partner")) #Set levels to chaange the order of the subplots

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_boxplot(show.legend = FALSE) +
  facet_grid(cols = vars(Type_sim)) +
  geom_hline(yintercept = 0.2, linetype="dashed", color = "blue") +
  ylim(0, 0.6) +
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_jitter(alpha = 0.4, width = 0.2, show.legend = FALSE) +  # Scatter points with slight jitter
  facet_grid(cols = vars(Type_sim)) +  # Facet by Type_sim
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "blue") + # Add horizontal threshold line
  stat_summary(fun = median, geom = "point", size = 3, color = "red") + # Add median points (red) and connect them with a line
  stat_summary(fun = median, geom = "line", aes(group = Type_sim), color = "red", linewidth = 1) +
  ylim(0, 0.6) + 
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))




#DF = 2

amAccuracyHomoDF2_Alt$Type_sim <- rep("Actor", nrow(amAccuracyHomoDF2_Alt))
pmAccuracyHomoDF2_Alt$Type_sim <- rep("Partner", nrow(pmAccuracyHomoDF2_Alt))
apmAccuracyHomoDF2$Type_sim <- rep("Actor Partner", nrow(apmAccuracyHomoDF2))

data <- rbind(amAccuracyHomoDF2_Alt, pmAccuracyHomoDF2_Alt, apmAccuracyHomoDF2)
data$Type_sim = factor(data$Type_sim, levels=c("Actor", "Partner", "Actor Partner")) #Set levels to chaange the order of the subplots

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_boxplot(show.legend = FALSE) +
  facet_grid(cols = vars(Type_sim)) +
  geom_hline(yintercept = 0.2, linetype="dashed", color = "blue") +
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_jitter(alpha = 0.4, width = 0.2, show.legend = FALSE) +  # Scatter points with slight jitter
  facet_grid(cols = vars(Type_sim)) +  # Facet by Type_sim
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "blue") + # Add horizontal threshold line
  stat_summary(fun = median, geom = "point", size = 3, color = "red") + # Add median points (red) and connect them with a line
  stat_summary(fun = median, geom = "line", aes(group = Type_sim), color = "red", linewidth = 1) +
  ylim(0, 1) + 
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

#DF = 3

ggplot(apmAccuracyHomoDF3, aes(x = as.factor(Length), y = RMSE)) +
  geom_boxplot(show.legend = FALSE) +
  geom_hline(yintercept = 0.2, linetype="dashed", color = "blue") +
  ylim(0, 0.6) +
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_jitter(alpha = 0.4, width = 0.2, show.legend = FALSE) +  # Scatter points with slight jitter
  facet_grid(cols = vars(Type_sim)) +  # Facet by Type_sim
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "blue") + # Add horizontal threshold line
  stat_summary(fun = median, geom = "point", size = 3, color = "red") + # Add median points (red) and connect them with a line
  stat_summary(fun = median, geom = "line", aes(group = Type_sim), color = "red", linewidth = 1) +
  ylim(0, 1) + 
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

#DF = 4

ggplot(apmAccuracyHomoDF4, aes(x = as.factor(Length), y = RMSE)) +
  geom_boxplot(show.legend = FALSE) +
  geom_hline(yintercept = 0.2, linetype="dashed", color = "blue") +
  ylim(0, 1) +
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_jitter(alpha = 0.4, width = 0.2, show.legend = FALSE) +  # Scatter points with slight jitter
  facet_grid(cols = vars(Type_sim)) +  # Facet by Type_sim
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "blue") + # Add horizontal threshold line
  stat_summary(fun = median, geom = "point", size = 3, color = "red") + # Add median points (red) and connect them with a line
  stat_summary(fun = median, geom = "line", aes(group = Type_sim), color = "red", linewidth = 1) +
  ylim(0, 1) + 
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

#### ---- 1.2.1.2 Heterogeneous case ----

#DF = 1

amAccuracyHeteroDF1_Alt$Type_sim <- rep("Actor", nrow(amAccuracyHeteroDF1_Alt))
pmAccuracyHeteroDF1_Alt$Type_sim <- rep("Partner", nrow(pmAccuracyHeteroDF1_Alt))
apmAccuracyHeteroDF1$Type_sim <- rep("Actor Partner", nrow(apmAccuracyHeteroDF1))

data <- rbind(amAccuracyHeteroDF1_Alt, pmAccuracyHeteroDF1_Alt, apmAccuracyHeteroDF1)
data$Type_sim = factor(data$Type_sim, levels=c("Actor", "Partner", "Actor Partner")) #Set levels to chaange the order of the subplots

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_boxplot(show.legend = FALSE) +
  facet_grid(vars(Members), vars(Type_sim)) +
  geom_hline(yintercept = 0.2, linetype="dashed", color = "blue") +
  ylim(0, 1) +
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_jitter(alpha = 0.4, width = 0.2, show.legend = FALSE) +  # Scatter points with slight jitter
  facet_grid(cols = vars(Type_sim)) +  # Facet by Type_sim
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "blue") + # Add horizontal threshold line
  stat_summary(fun = median, geom = "point", size = 3, color = "red") + # Add median points (red) and connect them with a line
  stat_summary(fun = median, geom = "line", aes(group = Type_sim), color = "red", linewidth = 1) +
  ylim(0, 1) + 
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

#DF = 2

amAccuracyHeteroDF2_Alt$Type_sim <- rep("Actor", nrow(amAccuracyHeteroDF2_Alt))
pmAccuracyHeteroDF2_Alt$Type_sim <- rep("Partner", nrow(pmAccuracyHeteroDF2_Alt))
apmAccuracyHeteroDF2$Type_sim <- rep("Actor Partner", nrow(apmAccuracyHeteroDF2))

data <- rbind(amAccuracyHeteroDF2_Alt, pmAccuracyHeteroDF2_Alt, apmAccuracyHeteroDF2)
data$Type_sim = factor(data$Type_sim, levels=c("Actor", "Partner", "Actor Partner")) #Set levels to chaange the order of the subplots

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_boxplot(show.legend = FALSE) +
  facet_grid(vars(Members), vars(Type_sim)) +
  geom_hline(yintercept = 0.2, linetype="dashed", color = "blue") +
  ylim(0, 0.6) +
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_jitter(alpha = 0.4, width = 0.2, show.legend = FALSE) +  # Scatter points with slight jitter
  facet_grid(cols = vars(Type_sim)) +  # Facet by Type_sim
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "blue") + # Add horizontal threshold line
  stat_summary(fun = median, geom = "point", size = 3, color = "red") + # Add median points (red) and connect them with a line
  stat_summary(fun = median, geom = "line", aes(group = Type_sim), color = "red", linewidth = 1) +
  ylim(0, 0.6) + 
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

#DF = 3

ggplot(apmAccuracyHeteroDF13, aes(x = as.factor(Length), y = RMSE)) +
  geom_boxplot(show.legend = FALSE) +
  facet_grid(vars(Members)) +
  geom_hline(yintercept = 0.2, linetype="dashed", color = "blue") +
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_jitter(alpha = 0.4, width = 0.2, show.legend = FALSE) +  # Scatter points with slight jitter
  facet_grid(cols = vars(Type_sim)) +  # Facet by Type_sim
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "blue") + # Add horizontal threshold line
  stat_summary(fun = median, geom = "point", size = 3, color = "red") + # Add median points (red) and connect them with a line
  stat_summary(fun = median, geom = "line", aes(group = Type_sim), color = "red", linewidth = 1) +
  ylim(0, 0.6) + 
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

#DF = 4

ggplot(apmAccuracyHeteroDF14, aes(x = as.factor(Length), y = RMSE)) +
  geom_boxplot(show.legend = FALSE) +
  facet_grid(vars(Members)) +
  geom_hline(yintercept = 0.2, linetype="dashed", color = "blue") +
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_jitter(alpha = 0.4, width = 0.2, show.legend = FALSE) +  # Scatter points with slight jitter
  facet_grid(cols = vars(Type_sim)) +  # Facet by Type_sim
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "blue") + # Add horizontal threshold line
  stat_summary(fun = median, geom = "point", size = 3, color = "red") + # Add median points (red) and connect them with a line
  stat_summary(fun = median, geom = "line", aes(group = Type_sim), color = "red", linewidth = 1) +
  ylim(0, 0.6) + 
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

### ---- 1.2.2 Inferential analysis: global ----

#### ---- 1.2.2.1 Homogeneous case ----

##### ---- 1.2.2.1.1 DF = 1 ----
###### ---- 1.2.2.1.1.1 P = 0.9 ----

#APM
x <- c(apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==30 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE, 
       apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==90 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE,
       apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==720 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==30 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),
             length(apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==90 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),
             length(apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==720 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==30 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==30 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==30 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==90 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==90 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==90 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==720 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==720 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$Length==720 & apmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

#AM
x <- c(amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==30 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE, 
       amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==90 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE,
       amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==720 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==30 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),
             length(amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==90 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),
             length(amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==720 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE)))))
#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==30 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==30 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==30 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==90 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==90 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==90 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==720 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==720 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHomoDF1_0.9[amAccuracyHomoDF1_0.9$Length==720 & amAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

#PM
x <- c(pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==30 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE, 
       pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==90 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE,
       pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==720 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==30 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),
             length(pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==90 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),
             length(pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==720 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==30 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==30 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==30 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==90 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==90 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==90 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==720 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==720 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHomoDF1_0.9[pmAccuracyHomoDF1_0.9$Length==720 & pmAccuracyHomoDF1_0.9$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

###### ---- 1.2.2.1.1.2 P = 0.6 ----

#APM
x <- c(apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==30 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE, 
       apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==90 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE,
       apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==720 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==30 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),
             length(apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==90 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),
             length(apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==720 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==30 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==30 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==30 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==90 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==90 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==90 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==720 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==720 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF1_0.6[apmAccuracyHomoDF1_0.6$Length==720 & apmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

#AM
x <- c(amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==30 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE, 
       amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==90 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE,
       amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==720 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==30 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),
             length(amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==90 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),
             length(amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==720 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==30 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==30 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==30 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==90 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==90 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==90 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==720 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==720 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHomoDF1_0.6[amAccuracyHomoDF1_0.6$Length==720 & amAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

#PM
x <- c(pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==30 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE, 
       pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==90 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE,
       pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==720 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==30 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),
             length(pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==90 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),
             length(pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==720 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==30 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==30 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==30 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==90 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==90 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==90 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==720 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==720 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHomoDF1_0.6[pmAccuracyHomoDF1_0.6$Length==720 & pmAccuracyHomoDF1_0.6$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

##### ---- 1.2.2.1.2 DF = 2 ----

#APM
x <- c(apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==30 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE, 
       apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==90 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE,
       apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==720 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==30 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE),
             length(apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==90 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE),
             length(apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==720 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==30 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==30 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==30 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==90 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==90 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==90 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==720 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==720 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF2[apmAccuracyHomoDF2$Length==720 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)


#AM
x <- c(amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==30 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE, 
       amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==90 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE,
       amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==720 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==30 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),
             length(amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==90 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),
             length(amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==720 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==30 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==30 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==30 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==90 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==90 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==90 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==720 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==720 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHomoDF2_Alt[amAccuracyHomoDF2_Alt$Length==720 & amAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

#PM
x <- c(pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==30 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE, 
       pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==90 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE,
       pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==720 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==30 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),
             length(pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==90 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),
             length(pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==720 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==30 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==30 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==30 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==90 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==90 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==90 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==720 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==720 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHomoDF2_Alt[pmAccuracyHomoDF2_Alt$Length==720 & pmAccuracyHomoDF2_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

##### ---- 1.2.2.1.3 DF = 3 ----

x <- c(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==30 & apmAccuracyHomoDF3$NbDyad==1000,]$RMSE, 
       apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==90 & apmAccuracyHomoDF3$NbDyad==1000,]$RMSE,
       apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==720 & apmAccuracyHomoDF3$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==30 & apmAccuracyHomoDF3$NbDyad==1000,]$RMSE),
             length(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==90 & apmAccuracyHomoDF3$NbDyad==1000,]$RMSE),
             length(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==720 & apmAccuracyHomoDF2$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==30 & apmAccuracyHomoDF3$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==30 & apmAccuracyHomoDF3$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==30 & apmAccuracyHomoDF3$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==90 & apmAccuracyHomoDF3$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==90 & apmAccuracyHomoDF3$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==90 & apmAccuracyHomoDF3$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==720 & apmAccuracyHomoDF3$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==720 & apmAccuracyHomoDF3$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==720 & apmAccuracyHomoDF3$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

##### ---- 1.2.2.1.4 DF = 4 ----

x <- c(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==30 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE, 
       apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==90 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE,
       apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==720 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==30 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE),
             length(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==90 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE),
             length(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==720 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==30 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==30 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==30 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==90 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==90 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==90 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==720 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==720 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==720 & apmAccuracyHomoDF4$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

#### ---- 1.2.2.2 Heterogeneous case ----

##### ---- 1.2.2.2.1 DF = 1 ----

#APM
x <- c(apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==30 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE, 
       apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==90 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE,
       apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==720 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==30 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE),
             length(apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==90 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE),
             length(apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==720 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==30 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==30 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==30 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==90 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==90 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==90 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==720 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==720 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHeteroDF1[apmAccuracyHeteroDF1$Length==720 & apmAccuracyHeteroDF1$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

#AM
x <- c(amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==30 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE, 
       amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==90 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE,
       amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==720 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==30 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),
             length(amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==90 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),
             length(amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==720 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test 
round(median(amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==30 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==30 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==30 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==90 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==90 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==90 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==720 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==720 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHeteroDF1_Alt[amAccuracyHeteroDF1_Alt$Length==720 & amAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

#PM
x <- c(pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==30 & pmAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE, 
       pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==90 & pmAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE,
       pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==720 & pmAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==30 & pmAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),
             length(pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==90 & pmAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),
             length(pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==720 & pmAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==30 & pmAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==30 & pmAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==30 & pmAccuracyHeteroDF1_Alt$NbDyad==30,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==90 & pmAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==90 & pmAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==90 & pmAccuracyHeteroDF1_Alt$NbDyad==30,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==720 & pmAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==720 & pmAccuracyHeteroDF1_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHeteroDF1_Alt[pmAccuracyHeteroDF1_Alt$Length==720 & pmAccuracyHeteroDF1_Alt$NbDyad==30,]$RMSE, 
            alternative = "less", mu = 0.2)

##### ---- 1.2.2.2.2 DF = 2 ----

#APM
x <- c(apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==30 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE, 
       apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==90 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE,
       apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==720 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==30 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE),
             length(apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==90 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE),
             length(apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==720 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==30 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==30 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==30 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==90 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==90 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==90 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==720 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==720 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHeteroDF2[apmAccuracyHeteroDF2$Length==720 & apmAccuracyHeteroDF2$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)
#AM
x <- c(amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==30 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE, 
       amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==90 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE,
       amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==720 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==30 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),
             length(amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==90 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),
             length(amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==720 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==30 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==30 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==30 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==90 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==90 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==90 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==720 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==720 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(amAccuracyHeteroDF2_Alt[amAccuracyHeteroDF2_Alt$Length==720 & amAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)


#PM
x <- c(pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==30 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE, 
       pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==90 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE,
       pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==720 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==30 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),
             length(pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==90 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),
             length(pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==720 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE)))))

#anova 
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==30 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==30 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==30 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==90 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==90 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==90 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==720 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),3)
round(IQR(pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==720 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE),3)
wilcox.test(pmAccuracyHeteroDF2_Alt[pmAccuracyHeteroDF2_Alt$Length==720 & pmAccuracyHeteroDF2_Alt$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)
##### ---- 1.2.2.2.3 DF = 3 ----

x <- c(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==30 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE, 
       apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==90 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE,
       apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==720 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==30 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE),
             length(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==90 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE),
             length(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==720 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==30 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==30 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==30 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==90 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==90 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==90 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==720 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==720 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==720 & apmAccuracyHeteroDF13$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

##### ---- 1.2.2.2.4 DF = 4 ----

x <- c(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==30 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE, 
       apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==90 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE,
       apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==720 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==30 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE),
             length(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==90 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE),
             length(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==720 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==30 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==30 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==30 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==90 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==90 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==90 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==720 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE),3)
round(IQR(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==720 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE),3)
wilcox.test(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==720 & apmAccuracyHeteroDF14$NbDyad==1000,]$RMSE, 
            alternative = "less", mu = 0.2)

### ---- 1.2.3 Inferential analysis: apm ----

#### ---- 1.2.3.1 Homogeneous case ----

##### ---- 1.2.3.1.1 Data management ----

apmAccuracyHomoDF1 <- apmAccuracyHomoDF1_0.9[apmAccuracyHomoDF1_0.9$NbDyad == 1000, ]
apmAccuracyHomoDF2 <- apmAccuracyHomoDF2[apmAccuracyHomoDF2$NbDyad == 1000, ]
apmAccuracyHomoDF3 <- apmAccuracyHomoDF3[apmAccuracyHomoDF3$NbDyad == 1000, ]
apmAccuracyHomoDF4 <- apmAccuracyHomoDF4[apmAccuracyHomoDF4$NbDyad == 1000, ]

apmAccuracyHomoDF1$DF <- 1
apmAccuracyHomoDF2$DF <- 2
apmAccuracyHomoDF3$DF <- 3
apmAccuracyHomoDF4$DF <- 4

apmAccuracyHomoDF1$Simulation <- rep(1:1000, each = 2, times = 3) # 1000 dyades, 2 membres, 3 longueurs
apmAccuracyHomoDF2$Simulation <- rep(1:1000, each = 2, times = 3)
apmAccuracyHomoDF3$Simulation <- rep(1:1000, each = 2, times = 3)
apmAccuracyHomoDF4$Simulation <- rep(1:1000, each = 2, times = 3)

apmAccuracyHomo <- rbind(apmAccuracyHomoDF1, apmAccuracyHomoDF2,
                         apmAccuracyHomoDF3, apmAccuracyHomoDF4)

apmAccuracyHomo$Length <- factor(apmAccuracyHomo$Length, levels = c(30, 90, 720))
apmAccuracyHomo$DF <- factor(apmAccuracyHomo$DF, levels = c(1,2,3,4))
apmAccuracyHomo$Simulation <- factor(apmAccuracyHomo$Simulation)

##### ---- 1.2.3.1.2 Model fitting ----

model_homo_noCorr <- gls(RMSE ~ Length * DF, data = apmAccuracyHomo,
                         method = "ML") #homogeneous variance, without intradyad correlation

model_homo_Corr <- gls(RMSE ~ Length * DF, data = apmAccuracyHomo,
                       correlation = corCompSymm(form = ~1|DF/Simulation),
                       method = "ML") #homogeneous variance, with intradyad correlation

model_hetero_noCorr <- gls(RMSE ~ Length * DF, data = apmAccuracyHomo,
                           weights = varIdent(form = ~1 | Length),
                           method = "ML") #heterogenous variance, without intradyad correlation

model_hetero_Corr <- gls(RMSE ~ Length * DF, data = apmAccuracyHomo,
                         correlation = corCompSymm(form = ~1|DF/Simulation),
                         weights = varIdent(form = ~1 | Length),
                         method = "ML") #heterogenous variance, with intradyad correlation

##### ---- 1.2.3.1.3 Model comparision ----

AIC(model_homo_noCorr, model_homo_Corr, model_hetero_noCorr, model_hetero_Corr)
min(AIC(model_homo_noCorr, model_homo_Corr, model_hetero_noCorr, model_hetero_Corr))

##### ---- 1.2.3.1.4 Model refit and analysis ----

modelREML <- gls(RMSE ~ Length * DF, data = apmAccuracyHomo,
                 correlation = corCompSymm(form = ~1|DF/Simulation),
                 weights = varIdent(form = ~1 | Length),
                 method = "REML")

summary(modelREML)

anova(modelREML)

#### ---- 1.2.3.2 Heterogeneous case ----

#no heterogeneous case because it does not have sense because of the 
#structure of the data (df)

## ---- 1.3 Pattern analysis ----

### ---- 1.3.1 Homogeneous case ----

#### ---- 1.3.1.1 DF = 1 ----

##### ---- 1.3.1.1.1 P = 0.9 ----

###### ---- 1.3.1.1.1.0 Data management ----

#add predictor
amAccuracyHomoDF1_0.9$predictor <- factor(rep("amMatrix", nrow(amAccuracyHomoDF1_0.9)))
pmAccuracyHomoDF1_0.9$predictor <- factor(rep("pmMatrix", nrow(pmAccuracyHomoDF1_0.9)))
apmAccuracyHomoDF1_0.9$predictor <- factor(rep("apmMatrix", nrow(apmAccuracyHomoDF1_0.9)))
#concatenation
data <- rbind(amAccuracyHomoDF1_0.9, pmAccuracyHomoDF1_0.9, apmAccuracyHomoDF1_0.9)
#construct predictor wrt the pattern
data$predictor_AM <- recode(data$predictor, "amMatrix" = 1, "pmMatrix" = 0, "apmMatrix" = 0)
data$predictor_PM <- recode(data$predictor, "amMatrix" = 0, "pmMatrix" = 1, "apmMatrix" = 0)
data$predictor_APM <- recode(data$predictor, "amMatrix" = 0, "pmMatrix" = 0, "apmMatrix" = 1)
#transform into factor
data$TYPE <- as.factor(data$TYPE)
#recode TYPE
data$TYPE_AM <- recode(data$TYPE, "AM" = 1, "APM" = 0, "PM" = 0, "IM" = 0)
data$TYPE_PM <- recode(data$TYPE, "AM" = 0, "APM" = 0, "PM" = 1, "IM" = 0)
data$TYPE_APM <- recode(data$TYPE, "AM" = 0, "APM" = 1, "PM" = 0, "IM" = 0)
#transform into factor
data$TYPE_aic <- as.factor(data$TYPE_aic)
#recode TYPE
data$TYPE_aic_AM <- recode(data$TYPE_aic, "actor" = 1, "actor partner" = 0, "partner" = 0, "independence" = 0)
data$TYPE_aic_PM <- recode(data$TYPE_aic, "actor" = 0, "actor partner" = 0, "partner" = 1, "independence" = 0)
data$TYPE_aic_APM <- recode(data$TYPE_aic, "actor" = 0, "actor partner" = 1, "partner" = 0, "independence" = 0)

###### ---- 1.3.1.1.1.1 D = -1 ----

###### ---- A) L = -1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_AM),
                as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_PM))[2])

###### ---- B) L = 0 ----

#APM 
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_AM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_PM))[2])

###### ---- C) L = 1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_AM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_PM))[2])

###### ---- 1.3.1.1.1.2 D = 1 ----

###### ---- A) L = -1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_PM))[2])

###### ---- B) L = 0 ----

#APM 
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_PM))[2])

###### ---- C) L = 1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_AM))[2])


#PM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_PM))[2])


##### ---- 1.3.1.1.2 P = 0.6 ----

###### ---- 1.3.1.1.2.0 Data management ----

#add predictor
amAccuracyHomoDF1_0.6$predictor <- factor(rep("amMatrix", nrow(amAccuracyHomoDF1_0.6)))
pmAccuracyHomoDF1_0.6$predictor <- factor(rep("pmMatrix", nrow(pmAccuracyHomoDF1_0.6)))
apmAccuracyHomoDF1_0.6$predictor <- factor(rep("apmMatrix", nrow(apmAccuracyHomoDF1_0.6)))
#concatenation
data <- rbind(amAccuracyHomoDF1_0.6, pmAccuracyHomoDF1_0.6, apmAccuracyHomoDF1_0.6)
#construct predictor wrt the pattern
data$predictor_AM <- recode(data$predictor, "amMatrix" = 1, "pmMatrix" = 0, "apmMatrix" = 0)
data$predictor_PM <- recode(data$predictor, "amMatrix" = 0, "pmMatrix" = 1, "apmMatrix" = 0)
data$predictor_APM <- recode(data$predictor, "amMatrix" = 0, "pmMatrix" = 0, "apmMatrix" = 1)
#transform into factor
data$TYPE <- as.factor(data$TYPE)
#recode TYPE
data$TYPE_AM <- recode(data$TYPE, "AM" = 1, "APM" = 0, "PM" = 0, "IM" = 0)
data$TYPE_PM <- recode(data$TYPE, "AM" = 0, "APM" = 0, "PM" = 1, "IM" = 0)
data$TYPE_APM <- recode(data$TYPE, "AM" = 0, "APM" = 1, "PM" = 0, "IM" = 0)
#transform into factor
data$TYPE_aic <- as.factor(data$TYPE_aic)
#recode TYPE
data$TYPE_aic_AM <- recode(data$TYPE_aic, "actor" = 1, "actor partner" = 0, "partner" = 0, "independence" = 0)
data$TYPE_aic_PM <- recode(data$TYPE_aic, "actor" = 0, "actor partner" = 0, "partner" = 1, "independence" = 0)
data$TYPE_aic_APM <- recode(data$TYPE_aic, "actor" = 0, "actor partner" = 1, "partner" = 0, "independence" = 0)

###### ---- 1.3.1.1.2.1 D = -1 ----

###### ---- A) L = -1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_AM),
                as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_PM))[2])

###### ---- B) L = 0 ----

#APM 
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_AM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_PM))[2])

###### ---- C) L = 1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_AM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_PM))[2])

###### ---- 1.3.1.1.2.2 D = 1 ----

###### ---- A) L = -1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_PM))[2])

###### ---- B) L = 0 ----

#APM 
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_PM))[2])

###### ---- C) L = 1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_PM))[2])


#### ---- 1.3.1.2 DF = 2 ----

##### ---- 1.3.1.2.0 Data management ----

#add predictor
amAccuracyHomoDF2_Alt$predictor <- factor(rep("amMatrix", nrow(amAccuracyHomoDF2_Alt)))
pmAccuracyHomoDF2_Alt$predictor <- factor(rep("pmMatrix", nrow(pmAccuracyHomoDF2_Alt)))
apmAccuracyHomoDF2$predictor <- factor(rep("apmMatrix", nrow(apmAccuracyHomoDF2)))
#concatenation
data <- rbind(amAccuracyHomoDF2_Alt, pmAccuracyHomoDF2_Alt, apmAccuracyHomoDF2)
#construct predictor wrt the pattern
data$predictor_AM <- recode(data$predictor, "amMatrix" = 1, "pmMatrix" = 0, "apmMatrix" = 0)
data$predictor_PM <- recode(data$predictor, "amMatrix" = 0, "pmMatrix" = 1, "apmMatrix" = 0)
data$predictor_APM <- recode(data$predictor, "amMatrix" = 0, "pmMatrix" = 0, "apmMatrix" = 1)
#transform into factor
data$TYPE <- as.factor(data$TYPE)
#recode TYPE
data$TYPE_AM <- recode(data$TYPE, "AM" = 1, "APM" = 0, "PM" = 0, "IM" = 0)
data$TYPE_PM <- recode(data$TYPE, "AM" = 0, "APM" = 0, "PM" = 1, "IM" = 0)
data$TYPE_APM <- recode(data$TYPE, "AM" = 0, "APM" = 1, "PM" = 0, "IM" = 0)
#transform into factor
data$TYPE_aic <- as.factor(data$TYPE_aic)
#recode TYPE
data$TYPE_aic_AM <- recode(data$TYPE_aic, "actor" = 1, "actor partner" = 0, "partner" = 0, "independence" = 0)
data$TYPE_aic_PM <- recode(data$TYPE_aic, "actor" = 0, "actor partner" = 0, "partner" = 1, "independence" = 0)
data$TYPE_aic_APM <- recode(data$TYPE_aic, "actor" = 0, "actor partner" = 1, "partner" = 0, "independence" = 0)

##### ---- 1.3.1.2.1 D = -1 ----

###### ---- A) L = -1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_APM))[2])


#AM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_AM),
                as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_PM))[2])

###### ---- B) L = 0 ----

#APM 
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_AM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_PM))[2])

###### ---- C) L = 1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_AM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_PM))[2])

##### ---- 1.3.1.2.2 D = 1 ----

###### ---- A) L = -1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_AM))[2])


#PM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_PM))[2])

###### ---- B) L = 0 ----

#APM 
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_PM))[2])

###### ---- C) L = 1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_PM))[2])

#### ---- 1.3.1.3 DF = 3 ----

##### ---- 1.3.1.3.0 Data management ----

apmAccuracyHomoDF3$predictor_APM <- rep("apmMatrix", nrow(apmAccuracyHomoDF3))
apmAccuracyHomoDF3$TYPE_APM <- recode(apmAccuracyHomoDF3$TYPE, "AM" = 0, "APM" = 1, "PM" = 0, "IM" = 0)

##### ---- 1.3.1.3.1 D = -1 ----

###### ---- A) L = -1 ----

confMat <- table(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==30 & apmAccuracyHomoDF3$NbDyad==30,]$predictor_APM, 
                 apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==30 & apmAccuracyHomoDF3$NbDyad==30,]$TYPE_APM)

confMat[2]/sum(confMat)


###### ---- B) L = 0 ----

confMat <- table(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==90 & apmAccuracyHomoDF3$NbDyad==30,]$predictor_APM, 
                 apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==90 & apmAccuracyHomoDF3$NbDyad==30,]$TYPE_APM)

confMat[2]/sum(confMat)

###### ---- C) L = 1 ----

confMat <- table(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==720 & apmAccuracyHomoDF3$NbDyad==30,]$predictor_APM, 
                 apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==720 & apmAccuracyHomoDF3$NbDyad==30,]$TYPE_APM)

confMat[2]/sum(confMat)

##### ---- 1.3.1.3.2 D = 1 ----

###### ---- A) L = -1 ----

confMat <- table(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==30 & apmAccuracyHomoDF3$NbDyad==1000,]$predictor_APM, 
                 apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==30 & apmAccuracyHomoDF3$NbDyad==1000,]$TYPE_APM)

confMat[2]/sum(confMat)

###### ---- B) L = 0 ----

confMat <- table(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==90 & apmAccuracyHomoDF3$NbDyad==1000,]$predictor_APM, 
                 apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==90 & apmAccuracyHomoDF3$NbDyad==1000,]$TYPE_APM)

confMat[2]/sum(confMat)

###### ---- C) L = 1 ----

confMat <- table(apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==720 & apmAccuracyHomoDF3$NbDyad==1000,]$predictor_APM, 
                 apmAccuracyHomoDF3[apmAccuracyHomoDF3$Length==720 & apmAccuracyHomoDF3$NbDyad==1000,]$TYPE_APM)

confMat[2]/sum(confMat)

#### ---- 1.3.1.4 DF = 4 ----

##### ---- 1.3.1.4.0 Data management ----

apmAccuracyHomoDF4$predictor_APM <- rep("apmMatrix", nrow(apmAccuracyHomoDF4))
apmAccuracyHomoDF4$TYPE_APM <- recode(apmAccuracyHomoDF4$TYPE, "AM" = 0, "APM" = 1, "PM" = 0, "IM" = 0)

##### ---- 1.3.1.4.1 D = -1 ----

###### ---- A) L = -1 ----

confMat <- table(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==30 & apmAccuracyHomoDF4$NbDyad==30,]$predictor_APM, 
                 apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==30 & apmAccuracyHomoDF4$NbDyad==30,]$TYPE_APM)

confMat[2]/sum(confMat)

###### ---- B) L = 0 ----

confMat <- table(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==90 & apmAccuracyHomoDF4$NbDyad==30,]$predictor_APM, 
                 apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==90 & apmAccuracyHomoDF4$NbDyad==30,]$TYPE_APM)

confMat[2]/sum(confMat)

###### ---- C) L = 1 ----

confMat <- table(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==720 & apmAccuracyHomoDF4$NbDyad==30,]$predictor_APM, 
                 apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==720 & apmAccuracyHomoDF4$NbDyad==30,]$TYPE_APM)

confMat[2]/sum(confMat)

##### ---- 1.3.1.4.2 D = 1 ----

###### ---- A) L = -1 ----

confMat <- table(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==30 & apmAccuracyHomoDF4$NbDyad==1000,]$predictor_APM, 
                 apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==30 & apmAccuracyHomoDF4$NbDyad==1000,]$TYPE_APM)

confMat[2]/sum(confMat)

###### ---- B) L = 0 ----

confMat <- table(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==90 & apmAccuracyHomoDF4$NbDyad==1000,]$predictor_APM, 
                 apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==90 & apmAccuracyHomoDF4$NbDyad==1000,]$TYPE_APM)

confMat[2]/sum(confMat)

###### ---- C) L = 1 ----

confMat <- table(apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==720 & apmAccuracyHomoDF4$NbDyad==1000,]$predictor_APM, 
                 apmAccuracyHomoDF4[apmAccuracyHomoDF4$Length==720 & apmAccuracyHomoDF4$NbDyad==1000,]$TYPE_APM)

confMat[2]/sum(confMat)

### ---- 1.3.2 Heterogeneous case ----

#### ---- 1.3.2.1 DF = 1 ----

##### ---- 1.3.2.1.0 Data management ----

#add predictor
amAccuracyHeteroDF1_Alt$predictor <- factor(rep("amMatrix", nrow(amAccuracyHeteroDF1_Alt)))
pmAccuracyHeteroDF1_Alt$predictor <- factor(rep("pmMatrix", nrow(pmAccuracyHeteroDF1_Alt)))
apmAccuracyHeteroDF1$predictor <- factor(rep("apmMatrix", nrow(apmAccuracyHeteroDF1)))
#concatenation
data <- rbind(amAccuracyHeteroDF1_Alt, pmAccuracyHeteroDF1_Alt, apmAccuracyHeteroDF1)
#construct predictor wrt the pattern
data$predictor_AM <- recode(data$predictor, "amMatrix" = 1, "pmMatrix" = 0, "apmMatrix" = 0)
data$predictor_PM <- recode(data$predictor, "amMatrix" = 0, "pmMatrix" = 1, "apmMatrix" = 0)
data$predictor_APM <- recode(data$predictor, "amMatrix" = 0, "pmMatrix" = 0, "apmMatrix" = 1)
#transform into factor
data$TYPE <- as.factor(data$TYPE)
#recode TYPE
data$TYPE_AM <- recode(data$TYPE, "AM" = 1, "APM" = 0, "PM" = 0, "IM" = 0)
data$TYPE_PM <- recode(data$TYPE, "AM" = 0, "APM" = 0, "PM" = 1, "IM" = 0)
data$TYPE_APM <- recode(data$TYPE, "AM" = 0, "APM" = 1, "PM" = 0, "IM" = 0)
#transform into factor
data$TYPE_aic <- as.factor(data$TYPE_aic)
#recode TYPE
data$TYPE_aic_AM <- recode(data$TYPE_aic, "actor" = 1, "actor partner" = 0, "partner" = 0, "independence" = 0)
data$TYPE_aic_PM <- recode(data$TYPE_aic, "actor" = 0, "actor partner" = 0, "partner" = 1, "independence" = 0)
data$TYPE_aic_APM <- recode(data$TYPE_aic, "actor" = 0, "actor partner" = 1, "partner" = 0, "independence" = 0)

##### ---- 1.3.2.1.1 D = -1 ----

###### ---- A) L = -1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_APM))[2])
#AM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_AM),
                as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_AM))[2])
#PM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_PM))[2])

###### ---- B) L = 0 ----

#APM 
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_APM))[2])
#AM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_AM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_AM))[2])
#PM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_PM))[2])

###### ---- C) L = 1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_APM))[2])
#AM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_AM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_AM))[2])
#PM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_PM))[2])

##### ---- 1.3.2.1.2 D = 1 ----

###### ---- A) L = -1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_APM))[2])
#AM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_AM))[2])
#PM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_PM))[2])

###### ---- B) L = 0 ----

#APM 
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_APM))[2])
#AM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_AM))[2])
#PM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_PM))[2])

###### ---- C) L = 1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_APM))[2])
#AM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_AM))[2])
#PM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_PM))[2])

#### ---- 1.3.2.2 DF = 2 ----

##### ---- 1.3.2.2.0 Data management ----

#add predictor
amAccuracyHeteroDF2_Alt$predictor <- factor(rep("amMatrix", nrow(amAccuracyHeteroDF2_Alt)))
pmAccuracyHeteroDF2_Alt$predictor <- factor(rep("pmMatrix", nrow(pmAccuracyHeteroDF2_Alt)))
apmAccuracyHeteroDF2$predictor <- factor(rep("apmMatrix", nrow(apmAccuracyHeteroDF2)))
#concatenation
data <- rbind(amAccuracyHeteroDF2_Alt, pmAccuracyHeteroDF2_Alt, apmAccuracyHeteroDF2)
#construct predictor wrt the pattern
data$predictor_AM <- recode(data$predictor, "amMatrix" = 1, "pmMatrix" = 0, "apmMatrix" = 0)
data$predictor_PM <- recode(data$predictor, "amMatrix" = 0, "pmMatrix" = 1, "apmMatrix" = 0)
data$predictor_APM <- recode(data$predictor, "amMatrix" = 0, "pmMatrix" = 0, "apmMatrix" = 1)
#transform into factor
data$TYPE <- as.factor(data$TYPE)
#recode TYPE
data$TYPE_AM <- recode(data$TYPE, "AM" = 1, "APM" = 0, "PM" = 0, "IM" = 0)
data$TYPE_PM <- recode(data$TYPE, "AM" = 0, "APM" = 0, "PM" = 1, "IM" = 0)
data$TYPE_APM <- recode(data$TYPE, "AM" = 0, "APM" = 1, "PM" = 0, "IM" = 0)
#transform into factor
data$TYPE_aic <- as.factor(data$TYPE_aic)
#recode TYPE
data$TYPE_aic_AM <- recode(data$TYPE_aic, "actor" = 1, "actor partner" = 0, "partner" = 0, "independence" = 0)
data$TYPE_aic_PM <- recode(data$TYPE_aic, "actor" = 0, "actor partner" = 0, "partner" = 1, "independence" = 0)
data$TYPE_aic_APM <- recode(data$TYPE_aic, "actor" = 0, "actor partner" = 1, "partner" = 0, "independence" = 0)

##### ---- 1.3.2.2.1 D = -1 ----

###### ---- A) L = -1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_AM),
as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_AM), 
positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==30,]$predictor_PM))[2])

###### ---- B) L = 0 ----

#APM 
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_AM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==30,]$predictor_PM))[2])

###### ---- C) L = 1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_APM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_AM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==30,]$TYPE_PM),
                as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==30,]$predictor_PM))[2])

##### ---- 1.3.2.2.2 D = 1 ----

###### ---- A) L = -1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==30 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==30 & data$NbDyad==1000,]$predictor_PM))[2])

###### ---- B) L = 0 ----

#APM 
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==90 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==90 & data$NbDyad==1000,]$predictor_PM))[2])

###### ---- C) L = 1 ----

#APM 
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_APM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_APM))[2])

#AM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_AM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_AM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_AM))[2])

#PM
confusionMatrix(as.factor(data[data$Length==720 & data$NbDyad==1000,]$TYPE_PM),
                as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==720 & data$NbDyad==1000,]$predictor_PM))[2])

#### ---- 1.3.2.3 DF = 3 ----

##### ---- 1.3.2.3.0 Data management ----

apmAccuracyHeteroDF13$predictor_APM <- rep("apmMatrix", nrow(apmAccuracyHeteroDF13))
apmAccuracyHeteroDF13$TYPE_APM <- recode(apmAccuracyHeteroDF13$TYPE, "AM" = 0, "APM" = 1, "PM" = 0, "IM" = 0)

##### ---- 1.3.2.3.1 D = -1 ----

###### ---- A) L = -1 ----

confMat <- table(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==30 & apmAccuracyHeteroDF13$NbDyad==30,]$predictor_APM, 
                 apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==30 & apmAccuracyHeteroDF13$NbDyad==30,]$TYPE_APM)

confMat[2]/sum(confMat)

###### ---- B) L = 0 ----

confMat <- table(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==90 & apmAccuracyHeteroDF13$NbDyad==30,]$predictor_APM, 
                 apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==90 & apmAccuracyHeteroDF13$NbDyad==30,]$TYPE_APM)

confMat[2]/sum(confMat)

###### ---- C) L = 1 ----

confMat <- table(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==720 & apmAccuracyHeteroDF13$NbDyad==30,]$predictor_APM, 
                 apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==720 & apmAccuracyHeteroDF13$NbDyad==30,]$TYPE_APM)

#confMat[2]/sum(confMat)
#1

##### ---- 1.3.2.3.2 D = 1 ----

###### ---- A) L = -1 ----

confMat <- table(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==30 & apmAccuracyHeteroDF13$NbDyad==1000,]$predictor_APM, 
                 apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==30 & apmAccuracyHeteroDF13$NbDyad==1000,]$TYPE_APM)

confMat[2]/sum(confMat)

###### ---- B) L = 0 ----

confMat <- table(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==90 & apmAccuracyHeteroDF13$NbDyad==1000,]$predictor_APM, 
                 apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==90 & apmAccuracyHeteroDF13$NbDyad==1000,]$TYPE_APM)
confMat[2]/sum(confMat)

###### ---- C) L = 1 ----

confMat <- table(apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==720 & apmAccuracyHeteroDF13$NbDyad==1000,]$predictor_APM, 
                 apmAccuracyHeteroDF13[apmAccuracyHeteroDF13$Length==720 & apmAccuracyHeteroDF13$NbDyad==1000,]$TYPE_APM)

confMat[2]/sum(confMat)

#### ---- 1.3.2.4 DF = 4 ----

##### ---- 1.3.2.4.0 Data management ----

apmAccuracyHeteroDF14$predictor_APM <- rep("apmMatrix", nrow(apmAccuracyHeteroDF14))
apmAccuracyHeteroDF14$TYPE_APM <- recode(apmAccuracyHeteroDF14$TYPE, "AM" = 0, "APM" = 1, "PM" = 0, "IM" = 0)

##### ---- 1.3.2.4.1 D = -1 ----

###### ---- A) L = -1 ----

confMat <- table(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==30 & apmAccuracyHeteroDF14$NbDyad==30,]$predictor_APM, 
                 apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==30 & apmAccuracyHeteroDF14$NbDyad==30,]$TYPE_APM)

confMat[2]/sum(confMat)

###### ---- B) L = 0 ----

confMat <- table(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==90 & apmAccuracyHeteroDF14$NbDyad==30,]$predictor_APM, 
                 apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==90 & apmAccuracyHeteroDF14$NbDyad==30,]$TYPE_APM)

confMat[2]/sum(confMat)

###### ---- C) L = 1 ----

confMat <- table(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==720 & apmAccuracyHeteroDF14$NbDyad==30,]$predictor_APM, 
                 apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==720 & apmAccuracyHeteroDF14$NbDyad==30,]$TYPE_APM)

#confMat[2]/sum(confMat)
#1

##### ---- 1.3.2.4.2 D = 1 ----

###### ---- A) L = -1 ----

confMat <- table(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==30 & apmAccuracyHeteroDF14$NbDyad==1000,]$predictor_APM, 
                 apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==30 & apmAccuracyHeteroDF14$NbDyad==1000,]$TYPE_APM)

confMat[2]/sum(confMat)

###### ---- B) L = 0 ----

confMat <- table(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==90 & apmAccuracyHeteroDF14$NbDyad==1000,]$predictor_APM, 
                 apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==90 & apmAccuracyHeteroDF14$NbDyad==1000,]$TYPE_APM)

confMat[2]/sum(confMat)

###### ---- C) L = 1 ----

confMat <- table(apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==720 & apmAccuracyHeteroDF14$NbDyad==1000,]$predictor_APM, 
                 apmAccuracyHeteroDF14[apmAccuracyHeteroDF14$Length==720 & apmAccuracyHeteroDF14$NbDyad==1000,]$TYPE_APM)

confMat[2]/sum(confMat)

# ---- 2. Univariate, S = 3 ----

## ---- 2.1 Load data and data management ----

#univariate, s=3
load("data/chap4/dataS3.RData")

data$predictor_PM <- recode(data$Predictor_sim, "amMatrix" = 0, "pmMatrix" = 1, "apmMatrix" = 0)
data$predictor_APM <- recode(data$Predictor_sim, "amMatrix" = 0, "pmMatrix" = 0, "apmMatrix" = 1)

data$TYPE_PM <- recode(data$TYPE, "AM" = 0, "APM" = 0, "PM" = 1, "IM" = 0)
data$TYPE_APM <- recode(data$TYPE, "AM" = 0, "APM" = 1, "PM" = 0, "IM" = 0)

## ---- 2.2 RMSE analysis ----

### ---- 2.2.1 Graphical representation ----

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_boxplot(show.legend = FALSE) +
  facet_grid(vars(as.factor(Members))) +
  geom_hline(yintercept = 0.2, linetype="dashed", color = "blue") +
  ylim(0, 0.6) +
  xlab("Length of the chain") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))

### ---- 2.2.2 Inferential analysis ----

x <- c(data[data$Length==30,]$RMSE, 
       data[data$Length==90,]$RMSE, 
       data[data$Length==720,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(data[data$Length==30,]$RMSE),
             length(data[data$Length==90,]$RMSE),
             length(data[data$Length==720,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(data[data$Length==30,]$RMSE),3)
round(IQR(data[data$Length==30,]$RMSE),3)
wilcox.test(data[data$Length==30,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(data[data$Length==90,]$RMSE),3)
round(IQR(data[data$Length==90,]$RMSE),3)
wilcox.test(data[data$Length==90,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(data[data$Length==720,]$RMSE),3)
round(IQR(data[data$Length==720,]$RMSE),3)
wilcox.test(data[data$Length==720,]$RMSE, 
            alternative = "less", mu = 0.2)

## ---- 2.3 Pattern analysis ----

### ---- 2.3.1 L = -1 ----

#PM 
confusionMatrix(as.factor(data[data$Length==30,]$TYPE_PM),
                as.factor(data[data$Length==30,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==30,]$predictor_PM))[2])
#APM
confusionMatrix(as.factor(data[data$Length==30,]$TYPE_APM),
                as.factor(data[data$Length==30,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==30,]$predictor_APM))[2])

### ---- 2.3.2 L = 0 ----

#PM 
confusionMatrix(as.factor(data[data$Length==90,]$TYPE_PM),
                as.factor(data[data$Length==90,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==90,]$predictor_PM))[2])
#APM
confusionMatrix(as.factor(data[data$Length==90,]$TYPE_APM),
                as.factor(data[data$Length==90,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==90,]$predictor_APM))[2])

### ---- 2.3.2 L = 1 ----

#PM 
confusionMatrix(as.factor(data[data$Length==720,]$TYPE_PM),
                as.factor(data[data$Length==720,]$predictor_PM), 
                positive = levels(as.factor(data[data$Length==720,]$predictor_PM))[2])
#APM
confusionMatrix(as.factor(data[data$Length==720,]$TYPE_APM),
                as.factor(data[data$Length==720,]$predictor_APM), 
                positive = levels(as.factor(data[data$Length==720,]$predictor_APM))[2])

# ---- 3. Bivariate case ----

## ---- 3.1 Load data and data management ----

#bivariate case
load("data/chap4/dataComplete30.RData")
load("data/chap4/dataComplete90.RData")
load("data/chap4/dataComplete720.RData")

dataComplete30$Length <- rep(30, nrow(dataComplete30))
dataComplete90$Length <- rep(90, nrow(dataComplete90))
dataComplete720$Length <- rep(720, nrow(dataComplete720))

data <- rbind(dataComplete30[,31:37], dataComplete90[,91:97], dataComplete720[,721:727])

data$predictor_B1 <- recode(data$caseSimulated, "B1" = 1, "B2" = 0, "D4" = 0, "E4" = 0)
data$predictor_B2 <- recode(data$caseSimulated, "B1" = 0, "B2" = 1, "D4" = 0, "E4" = 0)
data$predictor_D4 <- recode(data$caseSimulated, "B1" = 0, "B2" = 0, "D4" = 1, "E4" = 0)
data$predictor_E4 <- recode(data$caseSimulated, "B1" = 0, "B2" = 0, "D4" = 0, "E4" = 1)

data$TYPE_B1 <- ifelse(data$local == "partial actor partner", 1, 0)
data$TYPE_B2 <- ifelse(data$local == "partial actor", 1, 0)
data$TYPE_D4 <- ifelse(data$local == "complete actor partner on the main, actor on the second", 1, 0)
data$TYPE_E4 <- ifelse(data$local == "complete actor on both", 1, 0)

## ---- 3.2 RMSE analysis ----

### ---- 3.2.1 Graphical representation ----

ggplot(data, aes(x = as.factor(Length), y = RMSE)) +
  geom_boxplot(show.legend = FALSE) +
  facet_grid(vars(as.factor(members)), vars(as.factor(variable))) +
  geom_hline(yintercept = 0.2, linetype="dashed", color = "blue") +
  ylim(0, 1) +
  xlab("Length of the chain") +
  theme(
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14),   
    axis.text.y = element_text(size = 14) 
  )

### ---- 3.2.2 Inferential analysis ----

x <- c(data[data$Length==30,]$RMSE, 
       data[data$Length==90,]$RMSE, 
       data[data$Length==720,]$RMSE) 
g <- factor(c(
  rep(1:3, c(length(data[data$Length==30,]$RMSE),
             length(data[data$Length==90,]$RMSE),
             length(data[data$Length==720,]$RMSE)))))

#anova
kruskal.test(x, g)

#post hoc test
dunnTest(x ~ g, method = "bonferroni") 

#individual test
round(median(data[data$Length==30,]$RMSE),3)
round(IQR(data[data$Length==30,]$RMSE),3)
wilcox.test(data[data$Length==30,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(data[data$Length==90,]$RMSE),3)
round(IQR(data[data$Length==90,]$RMSE),3)
wilcox.test(data[data$Length==90,]$RMSE, 
            alternative = "less", mu = 0.2)

round(median(data[data$Length==720,]$RMSE),3)
round(IQR(data[data$Length==720,]$RMSE),3)
wilcox.test(data[data$Length==720,]$RMSE, 
            alternative = "less", mu = 0.2)

## ---- 3.3 Pattern analysis ----

### ---- 3.3.1 L = -1 ----

#B1
confusionMatrix(as.factor(data[data$Length==30,]$TYPE_B1),
                as.factor(data[data$Length==30,]$predictor_B1), 
                positive = levels(as.factor(data[data$Length==30,]$predictor_B1))[2])
#B2
confusionMatrix(as.factor(data[data$Length==30,]$TYPE_B2),
                as.factor(data[data$Length==30,]$predictor_B2), 
                positive = levels(as.factor(data[data$Length==30,]$predictor_B2))[2])
#D4
confusionMatrix(as.factor(data[data$Length==30,]$TYPE_D4),
                as.factor(data[data$Length==30,]$predictor_D4),
                positive = levels(as.factor(data[data$Length==30,]$predictor_D4))[2])
#E4
confusionMatrix(as.factor(data[data$Length==30,]$TYPE_E4),
                as.factor(data[data$Length==30,]$predictor_E4), 
                positive = levels(as.factor(data[data$Length==30,]$predictor_E4))[2])

### ---- 3.3.2 L = 0 ----

#B1
confusionMatrix(as.factor(data[data$Length==90,]$TYPE_B1),
                as.factor(data[data$Length==90,]$predictor_B1), 
                positive = levels(as.factor(data[data$Length==90,]$predictor_B1))[2])
#B2
confusionMatrix(as.factor(data[data$Length==90,]$TYPE_B2),
                as.factor(data[data$Length==90,]$predictor_B2), 
                positive = levels(as.factor(data[data$Length==90,]$predictor_B2))[2])
#D4
confusionMatrix(as.factor(data[data$Length==90,]$TYPE_D4),
                as.factor(data[data$Length==90,]$predictor_D4), 
                positive = levels(as.factor(data[data$Length==90,]$predictor_D4))[2])
#E4
confusionMatrix(as.factor(data[data$Length==90,]$TYPE_E4),
                as.factor(data[data$Length==90,]$predictor_E4), 
                positive = levels(as.factor(data[data$Length==90,]$predictor_E4))[2])

### ---- 3.3.3 L = 1 ----

#B1
confusionMatrix(as.factor(data[data$Length==720,]$TYPE_B1),
                as.factor(data[data$Length==720,]$predictor_B1), 
                positive = levels(as.factor(data[data$Length==720,]$predictor_B1))[2])
#B2
confusionMatrix(as.factor(data[data$Length==720,]$TYPE_B2),
                as.factor(data[data$Length==720,]$predictor_B2), 
                positive = levels(as.factor(data[data$Length==720,]$predictor_B2))[2])
#D4
confusionMatrix(as.factor(data[data$Length==720,]$TYPE_D4),
                as.factor(data[data$Length==720,]$predictor_D4), 
                positive = levels(as.factor(data[data$Length==720,]$predictor_D4))[2])
#E4
confusionMatrix(as.factor(data[data$Length==720,]$TYPE_E4),
                as.factor(data[data$Length==720,]$predictor_E4), 
                positive = levels(as.factor(data[data$Length==720,]$predictor_E4))[2])

