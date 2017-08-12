# Load packages

library(ggplot2)
library(plyr)
library(reshape2)
library(scales)

################################################
# Set Directory
getwd()
setwd("/Users/jialelim/Documents/github/UPBS/newCode")
################################################

# Data source
# Levin, M., Hashimshony, T., Wagner, F., & Yanai, I. (2012). 
# Developmental milestones punctuate gene expression in the Caenorhabditis embryo. 
# Dev Cell, 22, 1101-8. doi:10.1016/j.devcel.2012.04.004

#################
# import Data
rpl_data <- read.csv("../Data/rpl_data.csv", header = TRUE)

names(rpl_data)
str(rpl_data)

# get rid of the first column, stages
tempData<-rpl_data[c(2:ncol(rpl_data))] # ncol(rpl_data) gives the number of the last column in data

#melt the data into long form

plotData<-melt(tempData, id.var="time_min")
colnames(plotData) <- c("time_min", "gene", "rel_expression")
head(plotData)

# model all the genes as a linear model (giving you genes that low-high or high-low)

#set the basic model
linmod<-function(plotData){lm(rel_expression~time_min,data=plotData)}

#using plyr iterate over all genes
alllinmod<-dlply(plotData,.(gene),linmod)

#get the linear coefficients (the slopes)
lincoef = ldply(alllinmod,coef)
head(lincoef)

colnames(lincoef) <- c("gene", "intercept", "linear")

#sort by the linear coefficient
lincoef <-lincoef [order(-lincoef $linear),]
head(lincoef)

multi1 <- ggplot(plotData, aes(x = log(time_min), y = rel_expression)) +
  geom_point(alpha = I(0.5)) +                 
  labs(x = "log (Prey mass / Predator Mass)") +  # x and y axis labels
  scale_color_brewer(palette="Dark2") +          # change color palette 
  facet_wrap(~gene, ncol = 8) +  # facet based on Location and maximum 11 plots each row
  theme(legend.position="none") # remove all legends
multi1 

# Making a distance matrix from multivariate expression data and then a heatmap

# Loading libraries
library(cluster)
library(gplots)
library(gtools)
library(gdata)
library(caTools)
library(bitops)
library(RColorBrewer)

# Make a distance matrix:  specify the columns that the data are in by mydata[,1:lastcolumnnumber]. This will depend on how many genes you have in your interaction network.

DistanceMatrix<-dist(tempData[,2:ncol(tempData)], method = "euclidean", diag = TRUE, upper = TRUE, p = 2)

# see that you've made your datamatrix.  You should see a grid of pairwise distances.

list(DistanceMatrix)


# Make the two-way cluster and heatmap.  Again specify the columns that your data are in. Also specify the label. 
plot.new()
heatmap<-heatmap.2(data.matrix(tempData[,2:ncol(tempData)]), 
                   col=bluered(25), 
                   trace="none",
                   density.info="none", 
                   mar=c(5,5),
                   symkey=TRUE,
                   dendrogram="column",
                   labRow=plotData$time_min,
                   scale=c("col"),
                   Rowv=FALSE,
                   hclustfun=function(a) hclust((a),method="complete"))

# You should see a heatmap with a single cluster. Save it as a PDF. 

# If you want to see the effect of altering the clustering method from "ward" to something else, go:
?hclust

# under "method" you will see various options: "single", "median" etc.  See if they give you a cleaner separation. 




#pull out some gene

#plot the relatie expression pattern of the gene
ggplotall<-quartz(7,7)
onegeneplot<-ggplot()+
  geom_point(data=onegene, aes(x=time_min, y=rel_expression), size=1, alpha=1, colour="black")+
  geom_smooth(data=onegene, aes(x=time_min, y=rel_expression), method=lm, size=0.5, alpha=0.5, colour="red", se=FALSE)+
  ylab("relative expression")+
  xlab("time (min)")+
  #coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.060))+
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  scale_y_continuous(breaks=pretty_breaks(n=7))+
  ggtitle("relative expression of WBGene00003515_myo.3")+
  theme_classic(base_size = 12, base_family = "")+
  theme(axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
onegeneplot

# model a quadratic function (giving you genes that are low-high-low, or high-low-high)


quadmodel<-function(md){lm(rel_expression~time_min + I(time_min^2),data=md)}
allquadmodel<-dlply(md,.(gene),quadmodel)
quadcoef = ldply(allquadmodel,coef)
colnames(quadcoef) <- c("year", "intercept", "linear", "quad")

#sort by the quad coefficient
quadcoef<- quadcoef[order(+quadcoef$quad),]
head(quadcoef)

onegene<-subset(md, gene=="rpl.25.1")

#plot the relatie expression pattern of the gene
ggplotall<-
  quartz(7,7)
onegeneplot<-ggplot()+
  geom_point(data=onegene, aes(x=time_min, y=rel_expression), size=1, alpha=1, colour="black")+
  geom_smooth(data=onegene, aes(x=time_min, y=rel_expression), method=lm, formula=y~poly(x,2), size=0.5, alpha=0.5, colour="red", se=FALSE)+
  ylab("relative expression")+
  xlab("time (min)")+
  #coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.060))+
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  scale_y_continuous(breaks=pretty_breaks(n=7))+
  ggtitle("relative expression of WBGene00003515_myo.3")+
  theme_classic(base_size = 12, base_family = "")+
  theme(axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
onegeneplot


#So, pick some gene with an interesting pattern or even a gene with an interesting function or both, and go from there. 

