################################################
# Set Directory
getwd()
setwd("/Users/jialelim/Documents/github/UPBS/newCode")
# Notes : edited from Prof Leroi's final year module Biodiversity Genomics R practical script
# Difficult part: might be too difficult
################################################

# Data source
# Levin, M., Hashimshony, T., Wagner, F., & Yanai, I. (2012). 
# Developmental milestones punctuate gene expression in the Caenorhabditis embryo. 
# Dev Cell, 22, 1101-8. doi:10.1016/j.devcel.2012.04.004

# Here we have some C. elegans transcriptomic data, normalised RNA levels/ expression level of some genes


############# Basic Information  ###############
# import Data
ExpData <- read.csv("../Data/Celegans_transcriptomics.csv", header = TRUE)

# check out data
names(ExpData)
str(ExpData)

############# Convert from wide to long format  ###############
# before we can plot these data, we have to reshape the dataframe into a format that ggplot can understand
require(reshape2) # load the reshape2 package

?melt #check out the melt function
plotData <- melt(ExpData, id=c("time_min", "stage"), 
                       variable.name = "Genes", value.name = "RelativeExpression")
head(plotData)


############# Basic Visualisation  ###############
# we can plot them all on the same plot for a quick look
require(ggplot2)

summaryplot <- ggplot(plotData, aes(x = time_min, y = RelativeExpression, colour = Genes)) + # ggplot(dataframe, what to plot)
  geom_point(alpha = I(0.5), size = 3) +  # plot dots, adjust transparency and size
  geom_line() +  # join dots with lines
  labs(x = "Time / min", y = "Relative gene expression") # x and y axis labels
summaryplot

# can we get a better spread of data by log(Time) ?
summaryplot2 <- ggplot(plotData, aes(x = log(time_min), y = RelativeExpression, colour = Genes)) + # log x axis
  geom_point(size = 3) +  # plot dots, adjust transparency and size
  scale_color_brewer(palette="Paired") +   # change color palette to better distinguish bet genes
  labs(x = "log(Time / min)", y = "Relative gene expression") # x and y axis labels
summaryplot2

# or we can plot them separately in a multifacets plot
summaryplot3 <- summaryplot2 +
  facet_wrap(~Genes, ncol = 4) +  # facet based on Location and maximum 11 plots each row
  theme(legend.position="none") # remove all legends
summaryplot3

# add best fit lines
summaryplot4 <- summaryplot3 +
  geom_smooth(method = "lm",      # add regression lines
             fullrange = TRUE,   # line span full x axis
             alpha = I(0.3))        # increase transparency
summaryplot4

# besides rpl.1 and rps.1, does not seem to fit others very well
summaryplot5 <- summaryplot3 +
  geom_smooth(method=lm, formula=y~poly(x,2),    # add regression lines using y = x^2 + c formula
              fullrange = TRUE,   # line span full x axis
              alpha = 0.1,        # increase transparency
              size=0.5,           # thinner lines
              colour="black")     # colour fitted lines black
summaryplot5

# save plot 
ggsave('../Results/Celegans_summary.pdf', height=5, width=10)

############# Basic Analysis  ############### Difficult part
## Model all gene expression with linear models 
require(plyr)
require(scales)

str(plotData)
# set the basic model
LinearModel<-function(input){lm(RelativeExpression ~ time_min, data = input)}
# function(input) {what to do with input/ linear model relative exp against time, using input data}

# using plyr to iterate over all genes
AllLinearModels<-dlply(plotData,.(Genes), LinearModel) # apply function LinearModel to plotData, categorised by genes
AllLinearModels

# Get all linear coefficients (the slopes)
LinearCoeffs = ldply(AllLinearModels, coefficients)
LinearCoeffs

# label columns
colnames(LinearCoeffs) <- c("gene", "intercept", "linear")

# sort by modulus of linear coefficient
LinearCoeffs <- LinearCoeffs[order(-LinearCoeffs$linear),]
LinearCoeffs

# we can also use the following to find for ex. residuals of lm, esp for huge datasets
Residuals = ldply(AllLinearModels, df.residual)
Residuals

## what if we want to see the summary of the linear models, all adjusted r^2 values? 
ModelSummary<-function(input){summary(lm(input$RelativeExpression ~ input$time_min), data=input)}
# function(input) {what to do with input/ linear model relative exp against time, using input data}

# using plyr to iterate over all genes
AllModelSummary<-dlply(plotData,.(Genes), ModelSummary) # apply function to plotData, in this case input = plotData, categorised by genes
AllModelSummary
# shows all summaries of models

# show bare coefficients
Coeffs = ldply(AllModelSummary, coefficients)
Coeffs

# find only adjusted r values
# ?summary.lm
adjR<-function(input){summary(lm(input$RelativeExpression ~ input$time_min), data=input)$adj.r.squared}
adjRvalues<-dlply(plotData,.(Genes), adjR)
adjRvalues
allRvalues <- as.numeric(as.character(adjRvalues))
allRvalues

## Can we improve adjusted R values with quadratic models?
## Model all gene expression with quadratic models

QuadModel<-function(input){lm(RelativeExpression ~ time_min + I(time_min^2), data = input)}
AllQuadModel<-dlply(plotData, .(Genes), QuadModel)
QuadCoeff = ldply(AllQuadModel,coef)
colnames(QuadCoeff) <- c("year", "intercept", "linear", "quad")

#sort by the quad coefficient
QuadCoeff<- QuadCoeff[order(QuadCoeff$quad),]
head(QuadCoeff)

# adj.R values
adjR<-function(input){summary(lm(input$RelativeExpression ~ input$time_min + I(input$time_min^2)), data=input)$adj.r.squared}
adjRvalues<-dlply(plotData,.(Genes), adjR)
adjRvalues
# generally better except for zip-2 and ceh-24, require more complex models but risk overfitting.
# as we can see from the summary plots


############# Heatmaps!  ###############

# A common method of visualising gene expression data is to display it as a heatmap
# lets see which gene expressions correlate with each other
# Making a distance matrix from multivariate expression data and then a heatmap

# Loading libraries
library(cluster)
library(gplots)
library(gtools)
library(gdata)
library(caTools)
library(bitops)
library(RColorBrewer)

# Make a distance matrix: specify the columns that the data are in.
head(ExpData)
DistanceMatrix<-dist(ExpData[,3:ncol(ExpData)], method = "euclidean", diag = TRUE, upper = TRUE, p = 2)

# see that you've made your datamatrix.  You should see a grid of pairwise distances.
list(DistanceMatrix)

# create your own colour palette
my_palette <- colorRampPalette(c("dark green", "yellow", "red"))(n = 299)

# Make the two-way cluster and heatmap.  Again specify the columns that your data are in. Also specify the label. 
pdf(file = "../Results/Celegans_heatmap.pdf", width=11, height=8)

heatmap<-heatmap.2(data.matrix(ExpData[,3:ncol(ExpData)]), # for more info check out ?heatmap.2
                   col = my_palette,     # alternatively, try col=bluered(25)
                   main = "C. elegans gene expression", # heat map title
                   ylab = "Time/min",
                   xlab = "Genes",
                   density.info = "none",  # turns off density plot inside color legend
                   trace = "none",         # turns off trace lines inside the heat map
                   margins = c(5,5),      # widens margins around plot 
                   colsep = 1:55, sepcolor='white', sepwidth=0.05, # create white separation bet cols
                   symkey = TRUE,
                   dendrogram = "column",
                   labRow = ExpData$time_min, # rows in heatmap refer to time
                   scale = c("col"),
                   srtCol = 45, # rotate labels
                   Rowv = FALSE,
                   hclustfun = function(DistanceMatrix) hclust((DistanceMatrix),method="ward.D2"))

dev.off()

dir("../Results") # check if plot has been saved

# check out ?hclust 
# under "method" you will see various options: "single", "median" etc, try replacing ward.D2 with the various methods

# from the heatmap:
# oma.1, oma.2, mex.5, mex.6 and pie.1 form a cluster -> maternal mRNAs
# large and small ribosomal subunit gene expression levels (rpl-1 and rps-1) naturally grouped tgt
# surprisingly zip-2 and zip-1, despite having sequence similarities, do not show similar expression patterns
# having DNA similarity does not mean they will interact with similar proteins/ may be governed by different upstream proteins
