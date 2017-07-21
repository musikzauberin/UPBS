################################################################
################## Wrangling the Pound Hill Dataset ############
################################################################

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

############# Start exploring the data!  ###############

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

# reorganising data
library(reshape2)
library(plyr)
# Calculating total number of individuals per plot
totalcount <- tapply(MyWrangledData$Count, list(MyWrangledData$Plot, MyWrangledData$Species), sum, na.rm = TRUE)
totalcount

# Calculate number of quadrats per plot and store as column in totalcount
quadno <- ddply(MyWrangledData, c("Plot"), summarise, Quadrat.number = length(unique(Quadrat)))
DF <- cbind(totalcount, quadno$Quadrat.number) # combine columns of totalcount and quadrat.no 
colnames(DF)[ncol(DF)] <- "Quadrat.no" # add column name to last column of DF.meancount

DF

# Calculate mean number of counts of each species per quadrat in each plot
# divide all columns except the last column by the last column (no.of quadrat per plot)
# multiply by 8 to calculate counts / m^2 for each species for each plot
DF.meancount <- DF[, 1:(ncol(DF)-1)] / DF[, ncol(DF)] *8 
DF.meancount

# rearrange into Plot Species and Count variables
DF.Count <- melt(DF.meancount, id=c("Plot", "Species"), 
           varnames = c("Plot", "Species") , value.name = "Count")
head(DF.Count)

# reorder using plot number
PlotData <- DF.Count[order(DF.Count$Plot),]

head(PlotData)

# replace Count with presence/absence data # PlotData[,3] 3rd column of PlotData
PlotData[PlotData[,3] > 0, 3] <- 1 

SpeciesPerPlot <- tapply(PlotData$Count, list(PlotData$Plot), sum, na.rm = TRUE)

SpeciesPerPlot

# Plotting
plot(SpeciesPerPlot)

