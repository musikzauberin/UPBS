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

ggsave('../Results/Climate_location.pdf', height=5, width=10)

# plot all datapoints, quick look, categorised by Year
summaryplot3 <- ggplot(plotData, aes(x = Location, y = Temperature, colour = Year)) + # ggplot(dataframe, what to plot)
  geom_point(alpha = I(0.5), size = 3) +  # plot dots, adjust transparency and size
  labs(x = "Month", y = "Temperature") # x and y axis labels 
summaryplot3

# How can we make this more clear?
# What if we group the years into decades?
# Data Manipulation

# create 3 year categories # Create a new variable, Decade
str(plotData)

attach(plotData)
plotData$Decade[Year <= 1960] <- "1951-1960"
plotData$Decade[Year > 1960 & Year <= 1970] <- "1961-1970"
plotData$Decade[Year > 1970] <- "1971-1980"
detach(plotData)

head(plotData)
str(plotData) 

# plot all datapoints, quick look, categorised by Year
summaryplot4 <- ggplot(plotData, aes(x = Location, y = Temperature, fill = Decade)) + # ggplot(dataframe, what to plot)
  stat_boxplot(geom = 'errorbar') +           # add whiskers to boxplots
  geom_boxplot(lwd = 0.6)  +    
  labs(x = "Location", y = "Temperature") # x and y axis labels
summaryplot4

ggsave('../Results/Climate_decade.pdf', height=5, width=10)

############# Basic Analysis  ###############
# do temperatures of different locations differ from each other significantly
TemperatureLocationLM <- lm(Temperature ~ Location, data = plotData)
summary(TemperatureLocationLM)
anova(TemperatureLocationLM)

par(mfrow = c(2, 2), mar = c(5, 5, 1.5, 1.5))
plot(TemperatureLocationLM) # model diagnostics

