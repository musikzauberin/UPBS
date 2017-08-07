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

############# Calculating Species Richness & Shannon's diversity index (Data Manipulation) #############
library(reshape2)
library(plyr)

# Calculating total number of individuals per plot
totalcount <- tapply(MyWrangledData$Count, list(MyWrangledData$Plot, MyWrangledData$Species), sum, na.rm = TRUE)
totalcount

# Calculate number of quadrats per plot 
quadno <- ddply(MyWrangledData, c("Plot"), summarise, Quadrat.number = length(unique(Quadrat)))
quadno

# Calculate mean number of counts of each species per quadrat in each plot
# divide all columns by no.of quadrat per plot, multiply by 8 to calculate counts / m^2 for each species for each plot
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

# export data as .csv file
write.csv(SpeciesRichness, file = "../Results/PoundHillSpeciesRichness.csv")

# calculate Shannon's diversity index
sum(SpeciesRichness$SpeciesNo)



############# Data Visualisation #############
library(ggplot2)

boxplot <- ggplot(MyDF, aes(x = Type.of.feeding.interaction, y = log(Prey.mass/Predator.mass))) +
  stat_boxplot(geom = 'errorbar') +           # add whiskers to boxplots
  geom_boxplot(aes(fill = Type.of.feeding.interaction), lwd = 0.6)  +                       
  # add boxplot, fill boxplot according to color of feeding interaction and lwd dictates thickness of outlines
  labs(x = "Type of feeding interaction", y = "log (Prey mass / Predator Mass)") + # x and y axis labels
  theme(legend.position="none") # remove all legends
boxplot


ggsave('../Results/PP_boxplot.pdf', height=5, width=10)

