# load the package
require(ggplot2)

################################################
# Set Directory
getwd()
setwd("/Users/jialelim/Documents/github/UPBS/newCode")
# Newly added
################################################

# load the data
MyDF <- as.data.frame(
  read.csv("../Data/EcolArchives-E089-51-D1.csv"))

## (1) Scatterplot of Prey mass vs Predator mass + lm
p1 <- ggplot(MyDF, aes(x = log(Prey.mass), y = log(Predator.mass), 
                       color = Type.of.feeding.interaction)) +      # color by type of feeding interaction
  geom_point(size = 3, alpha = 0.5, shape = 3)         # dot plot, with transparency of plot character
p1

p2 <- p1 + geom_smooth(method = "lm",      # add regression lines
                       fullrange = TRUE,   # line span full x axis
                       alpha = 0.5)        # increase transparency
p2 

p3 <- p2 + labs(x = "log Prey mass", y = "log Predator mass") +   # change axis titles
  scale_color_brewer(name="Types of feeding interaction",         # change legend title
                     palette="Dark2")                             # change color palette
p3 # quite messy

p4 <- p3 + facet_grid(.~Type.of.feeding.interaction)              # multi-faceted plot
p4

ggsave('../Results/PP_ggplot.pdf', height=5, width=10)

## Ratio between Predator mass and Prey mass

## (2) jitter plot
plot1 <- ggplot(MyDF, aes(x = Type.of.feeding.interaction, y = log(Prey.mass/Predator.mass))) +      
  geom_jitter(size = 3, alpha = 0.5, shape = 3)  
ggsave('../Results/PP_jitterplot.pdf', height=5, width=10)


## (3) boxplot 
boxplot1 <- ggplot(MyDF, aes(x = Type.of.feeding.interaction, y = log(Prey.mass/Predator.mass))) +
  stat_boxplot(geom = 'errorbar') +           # add whiskers to boxplots
  geom_boxplot(aes(fill = Type.of.feeding.interaction), lwd = 0.6)  +                       
  # add boxplot, fill boxplot according to color of feeding interaction and lwd dictates thickness of outlines
  labs(x = "Type of feeding interaction", y = "log (Prey mass / Predator Mass)") + # x and y axis labels
  theme(legend.position="none") # remove all legends
boxplot1
ggsave('../Results/PP_boxplot.pdf', height=5, width=10)

## (4) histogram
histogram1 <- ggplot(MyDF, aes(log(Prey.mass/Predator.mass))) +
  geom_histogram(aes(fill = Type.of.feeding.interaction), binwidth = 1, alpha = I(0.5)) +                 
  labs(x = "log (Prey mass / Predator Mass)") # x and y axis labels
histogram1
ggsave('../Results/PP_histogram.pdf', height=5, width=10)

## (5) density plot
density1 <- ggplot(MyDF, aes(log(Prey.mass/Predator.mass))) +
  geom_density(aes(color = Type.of.feeding.interaction), alpha = I(0.5)) +                 
  labs(x = "log (Prey mass / Predator Mass)") + # x and y axis labels
  scale_color_brewer(palette="Dark2")            # change color palette
density1
ggsave('../Results/PP_density.pdf', height=5, width=10)

## (6) multi-faceted plots
multi1 <- ggplot(MyDF, aes(log(Prey.mass/Predator.mass))) +
  geom_density(aes(color = Type.of.feeding.interaction), alpha = I(0.5)) +                 
  labs(x = "log (Prey mass / Predator Mass)") +  # x and y axis labels
  scale_color_brewer(palette="Dark2") +          # change color palette 
  facet_grid(Type.of.feeding.interaction ~ Location) + # facet into both rows and columns
  theme(legend.position="none") # remove all legends
multi1 

ggsave('../Results/PP_multi.pdf', height=5, width=10)

# lets only look at piscivorous and predacious 
multi2 <- ggplot(subset(MyDF, Type.of.feeding.interaction %in% c("piscivorous", "predacious")), # subset choosing only two feeding interactions
                 aes(log(Prey.mass/Predator.mass))) +
  geom_density(aes(color = Type.of.feeding.interaction), alpha = I(0.5)) +                 
  labs(x = "log (Prey mass / Predator Mass)") +  # x and y axis labels
  scale_color_brewer(palette="Dark2") +          # change color palette 
  facet_wrap(~Location, ncol = 11) # facet based on Location and maximum 11 plots each row
multi2 

ggsave('../Results/PP_multisubset.pdf', height=5, width=10)
