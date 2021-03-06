################################################################
################## Wrangling the Pound Hill Dataset ############
################################################################

################################################
# Set Directory
getwd()
setwd("/Users/jialelim/Documents/github/UPBS/newCode")

# Newly Added, extention of DataWrang.R

############# Load the dataset ###############
# header = false because the raw data don't have real headers
MyData <- as.matrix(read.csv("../Data/PoundHillData.csv",header = F)) 

# header = true because we do have metadata headers
MyMetaData <- read.csv("../Data/PoundHillMetaData.csv",header = T, sep=";", stringsAsFactors = F)

############# Inspect the dataset ###############
head(MyData)
dim(MyData)
str(MyData)

############# Transpose ###############
MyData <- t(MyData)
head(MyData)
dim(MyData)

############# Replace species absences with zeros ###############
MyData[MyData == ""] = 0


############# Convert raw matrix to data frame ###############

TempData <- as.data.frame(MyData[-1,],stringsAsFactors = F) #stringsAsFactors = F is important!
colnames(TempData) <- MyData[1,] # assign column names from original data

############# Convert from wide to long format  ###############
require(reshape2) # load the reshape2 package

?melt #check out the melt function
MyWrangledData <- melt(TempData, id=c("Cultivation", "Block", "Plot", "Quadrat"), 
variable.name = "Species", value.name = "Count")
MyWrangledData[, "Cultivation"] <- as.factor(MyWrangledData[, "Cultivation"])
MyWrangledData[, "Block"] <- as.factor(MyWrangledData[, "Block"])
MyWrangledData[, "Plot"] <- as.factor(MyWrangledData[, "Plot"])
MyWrangledData[, "Quadrat"] <- as.factor(MyWrangledData[, "Quadrat"])
MyWrangledData[, "Count"] <- as.numeric(MyWrangledData[, "Count"])

str(MyWrangledData)
head(MyWrangledData)
dim(MyWrangledData)

############# Start exploring the data! (General Visualisation) ###############

# Let's visualise the spread of Counts for all Species
plot(Count~Species, data = MyWrangledData)
# most x labels cannot be seen due to lack of horizontal space

# Let's rotate the x labels
plot(Count~Species, data = MyWrangledData, las = 2)
# x labels still partially blocked

# Let's increase the margins

?par # what's par?
par(mar=c(12,5,1,1)) # adjust margins as needed
plot(Count~Species, data = MyWrangledData, las = 2)
# x axis label is hidden, lets move it down

par(mar=c(12,5,1,1))
plot(Count~Species, data = MyWrangledData, las = 2, xlab = "") # remove x axis label
mtext("Species", side=1, line=10, las = 1) # add manually
# not very informative, let's move on to other ways of visualising data

############# Calculating Species Richness (Data Manipulation) #############
library(reshape2)
library(plyr)

# Calculating total number of individuals per species per plot
totalcount <- tapply(MyWrangledData$Count, list(MyWrangledData$Plot, MyWrangledData$Species), sum, na.rm = TRUE)
totalcount

# Calculate number of quadrats per plot 
quadno <- ddply(MyWrangledData, c("Plot"), summarise, Quadrat.number = length(unique(Quadrat)))
quadno

# Calculate mean number of counts of each species per quadrat in each plot
# divide all columns by no.of quadrats per plot, multiply by 8 to calculate counts / m^2 for each species for each plot
meancount <- totalcount / quadno[,2] * 8
meancount

# Assign Cultivation and Treatment block to the corresponding Plot
x.variables <- ddply(MyWrangledData, c("Plot"), summarise, Block = unique(Block), Cultivation = unique(Cultivation))
x.variables

# Combine with meancount/ add information to meancount
tempDF <- cbind(meancount, x.variables)
head(tempDF)

# rearrange into Plot Species and Count variables # convert from wide to long format
MyNewDF <- melt(tempDF, id=c("Cultivation", "Block", "Plot"), 
                       variable.name = "Species", value.name = "Count")
head(MyNewDF)
str(MyNewDF)

# export data as .csv file
write.csv(MyNewDF, file = "../Results/PoundHillMean.csv")

# duplicate MyNewDF and replace all counts more than zero with 1 (Presence-Absence Data)
PAData <- MyNewDF
PAData[PAData[,5] > 0, 5] <- 1 #{PADF[,5] > 0}: all values more than 0 in the fifth column of PADF
head(PAData)

# calculate species richness per plot
SpeciesRichness <- ddply(PAData, c("Cultivation", "Block", "Plot"), summarise, SpeciesNo = sum(Count))
SpeciesRichness

############# Calculating Shannon's diversity index (Data Manipulation) #############

# Calculate total number of Counts per Plot
CountPerPlot <- ddply(MyNewDF, c("Plot"), summarise, CountPerPlot = sum(Count))
CountPerPlot

## Method 1
# extract rows in which Plot = '1' and Count > 0
Plot1 <- MyNewDF[which(MyNewDF$Plot=='1' & MyNewDF$Count > 0), ]
Plot1

CountPerPlot[CountPerPlot$Plot=='1', 2] # sum of counts in Plot 1
# divide 'Count' column by the sum of counts in Plot 1
proportion1 <- Plot1[, 5] / CountPerPlot[CountPerPlot$Plot=='1', 2]
proportion1

# calculate shannon diversity index for plot 1, H'=-\sum _{i=1}^{R}p_{i}\ln p_{i} (Wikipedia)
shannon1 <- -sum(proportion1 * log(proportion1))
shannon1

# Calculate shannon diversity index of other plots in exactly the same way, replacing 1 with other numbers.
# for example
Plot2 <- MyNewDF[which(MyNewDF$Plot=='2' & MyNewDF$Count > 0), ]
proportion2 <- Plot2[, 5] / CountPerPlot[CountPerPlot$Plot=='2', 2]
shannon2 <- -sum( proportion2* log(proportion2))

# combine all shannon indexes of the different plots into a vector
# for example
shannon.indexes1 <- c(shannon1, shannon2)
shannon.indexes1

## Method 2
# Otherwise, we can use a for loop to calculate shannon index of all 12 plots.

# find the number of unique levels in Plot and store as Plot_No
Plot_No <- length(unique(CountPerPlot$Plot)) 

ShannonIndexes <- vector(length=Plot_No) # create an empty vector with the length Plot_No

for (i in 1:Plot_No){       # same as: for (i in 1:12){  # to iterate from 1 to 12/ repeat loop for each i value from 1:12
  Plot <- MyNewDF[which(MyNewDF$Plot==i & MyNewDF$Count > 0), ]
  proportion <- Plot[, 5] / CountPerPlot[CountPerPlot$Plot==i, 2]
  ShannonIndexes[i] <- -sum( proportion* log(proportion))    
  # calculate shannon index for plot i and store as shannon.indexes[i]
}

ShannonIndexes
length(ShannonIndexes) # check if there are 12 indexes
length(ShannonIndexes) == 12 # replies if length = 12, if yes = TRUE

# reorder SpeciesRichness rows according to Plot
SpeciesRichness <- SpeciesRichness[order(as.numeric(as.character(SpeciesRichness$Plot))), ]
# as.numeric(as.character(SpeciesRichness$Plot)) allows R to arrange according to ascending order 
# and not according to the first character, ex (1, 10, 11, 12, 2, 3, 4, ...)

PlotInfo <- cbind(SpeciesRichness, ShannonIndexes)
PlotInfo
# export data as .csv file
write.csv(PlotInfo, file = "../Results/PoundHill_PlotInfo.csv")

############# Data Visualisation for Species Richness #############
library(ggplot2)
library(gridExtra)

boxplot1 <- ggplot(PlotInfo, aes(x = Cultivation, y = SpeciesNo)) +
  stat_boxplot(geom = 'errorbar') +           # add whiskers to boxplots
  geom_boxplot(aes(fill = Cultivation), alpha = 0.3)  + 
  geom_jitter(aes(fill = Cultivation), width = 0.3, alpha = 0.5, size = 2) +
  labs(x = "Cultivation month", y = "Mean Number of Species") + # x and y axis labels
  theme(legend.position="none") # remove all legends
boxplot1

boxplot2 <- ggplot(PlotInfo, aes(x = Block, y = SpeciesNo)) +
  stat_boxplot(geom = 'errorbar') +           # add whiskers to boxplots
  geom_boxplot(aes(fill = Block), alpha = 0.3)  + 
  geom_jitter(aes(fill = Block), width = 0.3, alpha = 0.5, size = 2) +
  labs(x = "Treatment Block", y = "Mean Number of Species") + # x and y axis labels
  theme(legend.position="none") # remove all legends
boxplot2

# save plots
ggsave('../Results/PoundHill_SpeciesRichness.pdf', 
       arrangeGrob(boxplot1, boxplot2, ncol = 2), # save both boxplots in one pdf with two columns
       height=5, width=10) # dimensions of pdf

############# Data Visualisation for Shannon Indexes #############

boxplot3 <- ggplot(PlotInfo, aes(x = Cultivation, y = ShannonIndexes)) +
  stat_boxplot(geom = 'errorbar') +           # add whiskers to boxplots
  geom_boxplot(aes(fill = Cultivation), alpha = 0.3)  + 
  geom_jitter(aes(fill = Cultivation), width = 0.3, alpha = 0.5, size = 2) +
  labs(x = "Cultivation month", y = "Shannon Diversity Index") + # x and y axis labels
  theme(legend.position="none") # remove all legends
boxplot3

boxplot4 <- ggplot(PlotInfo, aes(x = Block, y = ShannonIndexes)) +
  stat_boxplot(geom = 'errorbar') +           # add whiskers to boxplots
  geom_boxplot(aes(fill = Block), alpha = 0.3)  + 
  geom_jitter(aes(fill = Block), width = 0.3, alpha = 0.5, size = 2) +
  labs(x = "Treatment Block", y = "Shannon Diversity Index") + # x and y axis labels
  theme(legend.position="none") # remove all legends
boxplot4

# save plots
ggsave('../Results/PoundHill_ShannonIndexes.pdf', 
       arrangeGrob(boxplot3, boxplot4, ncol = 2), # save both boxplots in one pdf with two columns
       height=5, width=10) # dimensions of pdf

