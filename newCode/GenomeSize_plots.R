# Linear Modelling Practical

# This script uses genome size and morphology data to explore
# t tests and F tests in R
# Added plotting scatterplots using ggplot2 # Newly Added

################################################
# Set Directory
getwd()
setwd("/Users/jialelim/Documents/github/UPBS/newCode")
################################################

######################## (1) ##########################
# LOAD THE DATA FROM THE CSV TEXT FILE INTO A DATA FRAME:
genome <- read.csv('../Data/GenomeSize.csv') #assumes that you are working from your Code directory

######################## (2) ##########################
# LOOK AT AND MANIPULATE THE DATA:

head(genome)
str(genome) # Check what the data columns contain

tapply(genome$BodyWeight, genome$Suborder, mean) # take a quick mean
tapply(genome$BodyWeight, genome$Suborder, mean, na.rm=TRUE)
tapply(genome$BodyWeight, genome$Suborder, length)
tapply(genome$BodyWeight, genome$Suborder, var, na.rm=TRUE)

#You can also remove NA's by subsetting (using weight as an example):
BodyWt_no_NA <- subset(genome, !is.na(BodyWeight))
str(BodyWt_no_NA) 

######################## (3) ##########################
# LOOK AT A SUMMARY OF THE DATA:

summary(genome) #Note that each column gets a separate summary!

######################## (4) ##########################
# VISUALISE THE DISTRIBUTION OF THE SAMPLES OF YOUR FOCAL VARIABLE:

hist(genome$GenomeSize, breaks=10)
plot(density(genome$GenomeSize, bw=0.1))

######################## (5) ##########################
# COMPARE TWO DISTRIBUTIONS OF FACTORS OF INTEREST USING BOXPLOTS:

plot(GenomeSize ~ Suborder, data=genome) 
#You can also use plot(Genome$GenomeSize ~ Genome$Suborder)   

######################## (6) ##########################
# COMPARE THE TWO DISTRIBUTIONS USING DENSITY PLOTS:

# First, get two small datasets, one for each order
Anisoptera <- subset(genome, Suborder=='Anisoptera') #The dragonflies
Zygoptera <- subset(genome, Suborder=='Zygoptera') #The damselflies

# Now plot the first suborder and add a line for the second, adjusting 
# x and y axis limits to accommodate both curves
plot(density(Zygoptera$GenomeSize), xlim=c(0.1, 2.7), ylim=c(0,1.7))
lines(density(Anisoptera$GenomeSize), col='red')

######################## (7) ##########################
# PLOT ONE VARIABLE AGAINST ANOTHER USING SCATTERPLOTS:

hist(genome$TotalLength) #Check the distribution of your new variable of interest
plot(GenomeSize ~ TotalLength, data = genome) #Now plot

plot(GenomeSize ~ BodyWeight, data = genome) #Another example, using weight instead 

######################## (8) ##########################
# SEPARATE SCATTERS BY SUBORDERS

str(genome$Suborder) #Confirm that there are two levels under suborders 
myColours <- c('red', 'blue') # So choose two colours
mySymbols <- c(1,3) # And two different markers

plot(GenomeSize ~ TotalLength , data = genome, #Now plot again
	col = myColours[Suborder], pch = mySymbols[Suborder],
	xlab='Total length (mm)', ylab='Genome size (pg)')
	
legend(40,2, legend=levels(genome$Suborder), #Add legend at coordinate 40,2
	col= myColours, pch = mySymbols)

######################## (9) ##########################
# SAVE FILE AS A PDF:

pdf('../Results/GenomeSize.pdf', height=5, width=6) #Open the pdf file

plot(GenomeSize ~ TotalLength, data = genome, 
	col=myColours[Suborder], pch=mySymbols[Suborder],
	xlab='Total length (mm)', ylab='Genome size (pg)')
legend(40,2, legend=levels(genome$Suborder),
	col= myColours, pch = mySymbols)

dev.off() #Close the pdf file

######################################################
# Newly added
# (9) Plot scatterplots using ggplot2
library(ggplot2)

# basic plot
# differentiate Suborder by shape
p1 <- ggplot(genome, aes(x = TotalLength, y = GenomeSize, shape = Suborder)) +
  geom_point() 
p1

# differentiate Suborder by color
p1 <- ggplot(genome, aes(x = TotalLength, y = GenomeSize, color = Suborder)) +
  geom_point() 
p1

# differentiate Suborder by color and shape
p1 <- ggplot(genome, aes(x = TotalLength, y = GenomeSize, color = Suborder, shape = Suborder)) +
  geom_point() 
p1


# manually adjust size and shape of plot character
p2 <- ggplot(genome, aes(x = TotalLength, y = GenomeSize, shape = Suborder, color = Suborder)) +
  geom_point(size = 3)  + # increase plot character size
  scale_shape_manual(values = c(1,3)) # change plot character shape
p2

# change plot background/theme
p3 <- p2 + theme_set(theme_bw())
p3

# set size of text for axis and for titles
text <- element_text(size = 14)
bold.16.text <- element_text(face = "bold", size = 16)

p4 <- p3 + labs(title = "Damselflies and Dragonflies", x = "Total Length / mm ", y = "Genome Size / pg ") +
  theme(title = bold.16.text, axis.title = bold.16.text, axis.text = text)

p4

# SAVE FILE AS A PDF:
ggsave('../Results/GenomeSize_ggplot.pdf', height=5, width=6)


######################## (10) ##########################
# SAVE THE DATA AND YOUR NEW VARIABLES IN RDATA FORMAT:
save(genome, myColours, mySymbols, file='../Results/GenomeSize.Rda')
