################################################
# Set Directory
getwd()
setwd("/Users/jialelim/Documents/github/UPBS/newCode")

# Notes: I am assuming that NZS-new zealand south, NZN-new zealand north, PNG-Papua New Guinea, NC-coast?
# and that the variable is temperature
################################################

Data <- read.csv(file="../Data/ClimateData.csv", header=TRUE, na.strings=c("","NA"))
# replace "" with NA

str(Data)
head(Data)

############# Basic Visualisation  ###############
# we can plot them all on the same plot for a quick look
require(ggplot2)

# Before we can plot, we have to melt it into a long format
require(reshape2) # load the reshape2 package

?melt #check out the melt function
plotData <- melt(Data, id=c("Year", "Month", "TimePt"), 
                 variable.name = "Location", value.name = "Temperature")
head(plotData)

# plot all datapoints, quick look, categorised by Location
summaryplot <- ggplot(plotData, aes(x = Month, y = Temperature, colour = Location)) + # ggplot(dataframe, what to plot)
  geom_point(alpha = I(0.5), size = 3) +  # plot dots, adjust transparency and size
  labs(x = "Month", y = "Temperature") # x and y axis labels
summaryplot

# Temperature of different Locations seems to be different.
summaryplot2 <- ggplot(plotData, aes(x = Location, y = Temperature, fill = Location)) + # ggplot(dataframe, what to plot)
  stat_boxplot(geom = 'errorbar') +           # add whiskers to boxplots
  geom_boxplot(lwd = 0.6)  +    
  labs(x = "Month", y = "Temperature") # x and y axis labels
summaryplot2

# plot all datapoints, quick look, categorised by Year
summaryplot2 <- ggplot(plotData, aes(x = Location, y = Temperature, colour = Year)) + # ggplot(dataframe, what to plot)
  geom_point(alpha = I(0.5), size = 3) +  # plot dots, adjust transparency and size
  labs(x = "Time / min", y = "Relative gene expression") # x and y axis labels 
summaryplot2

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
