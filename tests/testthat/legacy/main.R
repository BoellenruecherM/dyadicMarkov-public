
# Code -----------------------------------------------------------------

## Chapter 2: The Identification of the Pattern in the Univariate Case

### The code is organized into two main parts to identify the patterns
### of the univariate with two and three cases.

## Chapter 3: The Identification of the Pattern in the Bivariate Case

### The code is organized into four main parts to identify the patterns
### of the romantic satisfaction, the emotional distress, and to reproduce the
### sensitive analysis and the comparison between AIC and BIC.

## Chapter 4: The Sensitive Analysis

### The code is organized into three main parts that runs the sensitive analysis
### for the univariate case with two states, the univariate case with three
### states and finally the bivariate case.

## Chapter 6: Clustering Analysis for the Univariate Case

### The code is organized into three main parts. The two first parts show the
### detailed code for the AM and PM patterns. The last part reproduces the
### illustration.

## Chapter 7: Clustering Analysis for the Bivariate Case

### The code is organized into five main parts that follow the classic method
### for clustering.

## Chapter 8: Extension I: Stress communication and Dyadic Coping

### The code is organized into five main parts. The first part shows the
### function to compute the theoretical count matrix for APM. Then, the second
### and third parts identify the pattern and run the classic analysis for MDS.
### The last two parts do the detailed analysis for the pattern for the women
### and the men respectively.

## Chapter 9: Extension II: Stress communication and Dyadic Coping

### The code is organized into four main parts that reproduce the analysis
### for the IM.


# Read me -----------------------------------------------------------------

### Each script can be run independently. Before each script, please run the
### following code.

rm(list = ls())

source("tests/testthat/legacy/FunctionsFile.R")

library(dplyr)
library(ggplot2)
library(caret)
library(FSA)
library(NbClust)
library(FactoMineR)
library(scatterplot3d)
library(factoextra)
library(binom)
library(nlme)
