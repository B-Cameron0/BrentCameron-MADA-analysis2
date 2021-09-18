###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidyverse) #for help with managing data
library(scales) #for help with making data more digestible

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######################################
#Data exploration/description
######################################
#I'm using basic R commands here.
#Lots of good packages exist to do more.
#For instance check out the tableone or skimr packages

#summarize data 
mysummary = summary(mydata)

#look at summary
print(mysummary)

#do the same, but with a bit of trickery to get things into the 
#shape of a data frame (for easier saving/showing in manuscript)
summary_df = data.frame(do.call(cbind, lapply(mydata, summary)))

#save data frame table to file for later use in manuscript
summarytable_file = here("results", "summarytable.rds")
saveRDS(summary_df, file = summarytable_file)

#Our outcome of interest is COVID-19 deaths
#Variables that will serve as predictors of outcome include...
#Ethnicity,age group, and whether or not there is an underlying condition

#Plot 1, Plotting COVID-19 deaths by Ethnicity

plot_1 <- mydata %>%
  ggplot(aes(x=ethnicity, y=death_yn)) +
  geom_bar(stat = "identity") +
  ggtitle("COVID-19 Deaths by Ethnicity in the United States")+
  geom_smooth(method='lm')+
  scale_x_discrete(guide = guide_axis(n.dodge = 1, check.overlap = TRUE))+
  scale_y_continuous(labels = comma)

#Examine plot
plot(plot_1)

#Save the plot
figure_file1 = here::here("results","resultfigure1.png")
ggsave(filename = figure_file1, plot = plot_1)

#Plot 2, Plotting COVID-19 deaths by Age Group

plot_2 <-mydata %>%
  ggplot(aes(x=age_group, y=death_yn))+
  geom_bar(stat = "identity") +
  ggtitle("COVID-19 Deaths by Age Group in the United States")+
  geom_smooth(method='lm')+
  scale_y_continuous(labels = comma)


#Examine the Plot
plot(plot_2)

#Save the plot
figure_file2 = here::here("results","resultfigure2.png")
ggsave(filename = figure_file2, plot = plot_2)

#Plot 3, Plotting COVID-19 Deaths with Any Underlying Condition

plot_3 <- mydata %>%
  ggplot(aes(x=underlying_conditions_yn, y=death_yn)) +
  geom_bar(stat = "identity") +
  ggtitle("COVID-19 Deaths with Underlying Condtiions in the United States")+
  geom_smooth(method='lm')+
  scale_x_discrete(guide = guide_axis(n.dodge = 1, check.overlap = TRUE))+
  scale_y_continuous(labels = comma)

#Examine plot
plot(plot_3)

#Save the plot
figure_file3 = here::here("results","resultfigure3.png")
ggsave(filename = figure_file3, plot = plot_3)








