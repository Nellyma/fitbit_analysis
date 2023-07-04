#Install and upload the necessary packages
library("here")
library("skimr")
library("janitor")
library("lubridate")
library("tidyverse")

#Import datasets
activity <- read_csv("Fit base Data/dailyActivity_merged.csv")
steps <- read_csv("Fit base Data/dailySteps_merged.csv")
sleep <- read_csv("Fit base Data/sleepDay_merged.csv")

#Preview datasets
head(activity)
colnames(activity)
skim_without_charts(activity)

head(steps)
skim_without_charts(steps)

head(sleep)
skim_without_charts(sleep)

#Processing and Cleaning
#Here we find out the number of participants per dataset
n_unique(activity$Id)
n_unique(sleep$Id)
n_unique(steps$Id)

#Check for any duplicates
sum(duplicated(activity))
sum(duplicated(sleep))
sum(duplicated(steps))

#Remove duplicates and empty rows
daily_activity <- activity %>%
  distinct() %>%
  drop_na()

daily_sleep <- sleep %>%
  distinct() %>%
  drop_na()

daily_steps <- steps %>%
  distinct() %>%
  drop_na()

#Now we verify that the duplicate data has been removed
sum(duplicated(daily_sleep))

#Formating date format
daily_activity <- daily_activity %>%
  rename(date = ActivityDate) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

head(daily_activity)

daily_sleep <- daily_sleep %>%
  rename(date = SleepDay) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y %I:%M:%S %p"))

head(daily_sleep)

daily_steps <- daily_steps %>%
  rename(date = ActivityDay) %>%
  mutate(date = as_date(date,format ="%m/%d/%Y"))

#Rename Columns 
daily_activity <- rename_with(daily_activity, tolower)

daily_sleep <- rename_with(daily_sleep, tolower)

daily_steps <- rename_with(daily_steps, tolower)

#Merging Data
activity_sleep <- merge(daily_activity, daily_sleep, by=c("id", "date"))

head(activity_sleep)
glimpse(activity_sleep)

#Classification according to activity
#We want to find out the distribution of users according to their activity

#Firstly we will calculate the daily average steps of users and then we
# categorize users according to activity as follows:
  
  #Sedentary - Less than 5000 steps a day.
  #Lightlyly active - Between 5000 and 7499 steps a day.
  #Failyly active - Between 7500 and 9999 steps a day.
  #Very active - More than 10000 steps a day

daily_average_steps <- activity_sleep %>%
  group_by(id) %>%
  summarise (mean_daily_steps = mean(totalsteps), mean_daily_cal = 
               mean(calories), mean_daily_sleep = mean(totalminutesasleep))%>%
  mutate(user_type = case_when(
    mean_daily_steps < 5000 ~ "sedentary",
    mean_daily_steps >= 5000 & mean_daily_steps < 7499 ~ "lightly active", 
    mean_daily_steps >= 7500 & mean_daily_steps < 9999 ~ "fairly active", 
    mean_daily_steps >= 10000 ~ "very active"
  ))

head(daily_average_steps)

#Now we create a new column for the percentages of user type according to activity
# so the we can create a pie chart for better understanding.

user_type_percent <- daily_average_steps %>%
  group_by(user_type) %>%
  summarise(total= n()) %>%
  mutate(total_percent= scales::percent (total/sum(total)))

head(user_type_percent)

pie_colors <- c("orangered4", "orchid4", "palegreen4", "paleturquoise4", "palevioletred4")

ggplot(user_type_percent, aes(x = "", y = total_percent, fill = user_type)) +
  geom_bar(width = 1, linewidth = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(total_percent)),
            position = position_stack(vjust = 0.5)) +
  labs(title="User type distribution") +
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  scale_fill_manual(values = pie_colors)

#Classification according to usage
