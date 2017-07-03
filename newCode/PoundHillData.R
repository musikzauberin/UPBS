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

par(mar=c(12,5,1,1))
plot(Count~Species, data = MyWrangledData, las = 2, xlab = "") # remove x axis label
mtext("Species", side=1, line=10, las = 1) # add manually


## Let's look at BlockA
BlockA <- subset(MyWrangledData, Block =='a')
str(BlockA)
# par(mfrow=c(1,2), las=2, mar=c(12,5,1,1))
plot(Count~Species, data = BlockA, las = 2)
# many species not observed in BlockA, shown as zero entries

#remove zero entries
# BlockA[BlockA == 0] <- NA
#BlockA <- BlockA[rowSums(BlockA == 0) == 0,] # why does this work?
#?rowSums

#OR
##Go through each row and determine if a value is zero
row_sub = apply(BlockA, 1, function(row) all(row !=0 ))
row_sub
##Subset as usual
BlockA <- BlockA[row_sub,]

str(BlockA) # 41 levels for Species
BlockA <- droplevels(BlockA)
str(BlockA) # 28 Levels
plot(Count~Species, data = BlockA, las=2, type = "p")


# subset species present in all four treatments
row_sub2 = apply(MyWrangledData, 1, function(row) all(row !=0 ))
row_sub2
##Subset as usual
SpeciesSubset <- MyWrangledData[row_sub2,]
str(SpeciesSubset)
SpeciesSubset <- droplevels(SpeciesSubset)
str(SpeciesSubset)

require(ggplot2)
ggplot(MyWrangledData, aes(x=Species, y=Count, color=Block)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#########################################################################
match('Aphanes australis', MyWrangledData$Species)

all(subset(MyWrangledData, Species == 'Aphanes australis') == 0)

