## A1 : okCupid EDA
## Author: Martina Aranda Rivera
## Date: March 13, 2023
## Prof. Edward Kwartler
## Hult International School of Business


install.packages("tidytext")
install.packages("ggmap")

# Load required packages
library(tidyr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(maps)
library(ggthemes)
library(leaflet)
library(mapproj)
library(stringr)


# Get the okcupid data as `profiles`
profiles <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A1_OKCupid/profiles.csv')
latlon <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A1_OKCupid/LatLon.csv')
addr <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A1_OKCupid/addr.csv')
census <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A1_OKCupid/sharedCensus2010Vars.csv')

# Understand the Data
str(profiles)

# Counts the amount of nulls in all columns
colSums(is.na(profiles))

# Visual for NA in each column 
plot_missing(profiles) + 
  scale_fill_manual(values = c("gold1", "maroon3", "pink1", "purple1")) +
  theme_minimal()

# Names of columns:
colnames(profiles)

# Count amount of columns: 
length(unique(colnames(profiles)))

# Unique values: 
unique(profiles$offspring)
unique(profiles$education)
unique(profiles$orientation)

# Count unique values in column: 
length(unique(profiles$age))
length(unique(profiles$body_type))
length(unique(profiles$diet)) 
length(unique(profiles$drinks))
length(unique(profiles$drugs))
length(unique(profiles$education))
length(unique(profiles$ethnicity))
length(unique(profiles$height))
length(unique(profiles$income))
length(unique(profiles$job))
length(unique(profiles$last_online))
length(unique(profiles$location))
length(unique(profiles$offspring))
length(unique(profiles$orientation))
length(unique(profiles$pets))
length(unique(profiles$religion))
length(unique(profiles$sex))
length(unique(profiles$sign))
length(unique(profiles$smokes))
length(unique(profiles$speaks))
length(unique(profiles$status))
length(unique(profiles$essay0))

#plot
plot_str(profiles)

# Classes for each column, family of apply
sapply(profiles, class)

# Divide data, int to mean median. and character: class and mode. 
summary(profiles)

# Delete Columns that have too many missing values to assume new
profiles <- profiles %>% 
  select(-starts_with("essay0"), -income, -last_online, -offspring, -diet, -offspring, -height, -religion, -education) # too many values missing
# I beleive they are important for a dating profile, but too many missing values to assign values

###################

####### Sex ####### 
sum(is.na(profiles$age))
unique(profiles$sex)

male <- subset(profiles, sex == "m") # male subset
female <- subset(profiles, sex == "f") # female subset

### INSIGHT 2:  
# Graph per persona:
lgbtq <- profiles %>%
  filter(orientation != "straight")
lgbtq_f <- lgbtq %>%
  filter(sex == "f")
lgbtq_m <- lgbtq %>%
  filter(sex == "m")

# Females consumers part of the LGBTQ+ community:
ggplot(lgbtq_f, aes(x = age_group, fill = orientation)) +
  geom_bar(position = "fill") +
  xlab("Age Group") +
  ylab("Count") +
  ggtitle("Distribution of Age Group by LGBTQ+ Females") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("hotpink", "maroon3")) +
  theme_minimal()

# Males consumers part of the LGBTQ+ community:
ggplot(lgbtq_m, aes(x = age_group, fill = orientation)) +
  geom_bar(position = "fill") +
  xlab("Age Group") +
  ylab("Count") +
  ggtitle("Distribution of Age Group by LGBTQ+ Males") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("goldenrod2", "gold1")) +
  theme_minimal()

# Sexuality in both sexes mostly identify straight. 
ggplot(profiles, aes(x = age_group, fill = orientation)) +
  geom_bar(position = "fill") +
  xlab("Age Group") +
  ylab("Count") +
  ggtitle("Distribution of Age Group by LGBTQ+ Profiles") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("gold1", "maroon3", "blue4")) +
  theme_minimal()

###################


####### Age #######

# Check for missing values
sum(is.na(profiles$age))
 
# Understand Age
length(unique(profiles$age_group))
sum(is.na(profiles$age_group))

#remove outliers: works good
age_group <- profiles[profiles$age >= 18 & profiles$age <= 67,]

# Divide Age in groups/segments bevery 10 years
profiles$age_group <- cut(profiles$age, 
                          breaks = seq(18, 70, 10), 
                          labels = c("18-27", "28-37", "38-47", "48-57", "58-67"), 
                          include.lowest = TRUE)

# See the new age groups
summary(profiles$age_group)

# Remove rows where age is NA
profiles <- profiles[complete.cases(profiles$age_group), ]

# Graph showing that mostly man are part of the app in all ages. Still need to delete the NA
ggplot(profiles, aes(x = age_group, fill = sex)) +
  geom_bar(position = "dodge") +
  xlab("Age Group") +
  ylab("Count") +
  ggtitle("Distribution of Age Group by Sex in OkCupid Dataset") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("pink1", "maroon3")) +
  theme_minimal()

###################


####### Orientation  ####### insight 1

ggplot(profiles, aes(x = orientation, fill = sex)) + 
  geom_bar() + 
  xlab("Gender") + 
  ylab("Count") + 
  ggtitle("Distribution of Sexual Orientations by Gender")

# graphs fividing by age group
ggplot(profiles, aes(x = orientation, fill = age_group)) +
  geom_bar(position = "dodge") +
  xlab("orientation") +
  ylab("Count") +
  ggtitle("Distribution of job by sex in OkCupid Dataset") +
  theme(legend.position = "bottom") +
  facet_wrap(~ age_group, ncol = 2) + 
  scale_fill_manual(values = c("hotpink", "maroon3")) +
  theme_minimal()

 ###################


#######  Drinks #######

# Remove leading/trailing white spaces
profiles$drinks <- trimws(profiles$drinks)

# Replace "sometimes" with "socially"
profiles$drinks <- ifelse(tolower(profiles$drinks) == "sometimes", "socially", profiles$drinks)

# Replace "not at all" with "rarely"
profiles$drinks <- ifelse(tolower(profiles$drinks) == "not at all", "rarely", profiles$drinks)

# Remove anything after comma
profiles$drinks <- sub(",.*", "", profiles$drinks)

# Standardize values
profiles$drinks[profiles$drinks %in% c("socially", "rarely", "often", "very often", "desperately")] <- c("Socially", "Rarely", "Often", "Very often", "Desperately")

# View unique values to verify cleaning
profiles$drinks %>% unique()

# Remove row with NA
profiles <- profiles[complete.cases(profiles$drinks), ]
profiles$drinks %>% unique()

## Graph showing Drinking habits ### fix graph, cannot read the ethnicities properly
ggplot(profiles, aes(x = drinks, fill = sex)) +
  geom_bar(position = "dodge") +
  xlab("Drinking Habits") +
  ylab("Count") +
  ggtitle("Distribution of Drinking Habits by Gender in OkCupid Dataset") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("hotpink", "gold1")) +
  theme_minimal()

###################


####### Ethnicity #######

# EDA and Cleaning Data by removing everything after a ','
profiles$ethnicity <- sub(",.*", "", profiles$ethnicity)
profiles <- profiles[complete.cases(profiles$ethnicity), ]
unique(profiles$ethnicity)

# Graph showing ethnicity ### fix graph, cannot read the ethnicities properly
ggplot(profiles, aes(x = ethnicity, fill = sex)) +
  geom_bar() +
  coord_flip() +
  xlab("Ethnicity") +
  ylab("Count") +
  ggtitle("Distribution of Ethnicity by sex in OkCupid Dataset")

# Graph showing ethnicity ### fix graph, cannot read the ethnicities properly
ggplot(profiles, aes(x = ethnicity, fill = age_group)) +
  geom_bar() +
  coord_flip() +
  xlab("Ethnicity") +
  ylab("Count") +
  ggtitle("Distribution of Ethnicity by age group in OkCupid Dataset")

###################


####### Body Type ####### 

# Clean all NA from the data
profiles <- profiles[complete.cases(profiles$body_type), ]
unique(profiles$body_type)

# insight 2: Dominant pool of ages 18-27 and 28-37 year olds in the app
ggplot(profiles, aes(x = body_type, fill = age_group)) +
  geom_bar(position = "dodge") +
  xlab("body type") +
  ylab("Count") +
  ggtitle("Distribution of body type by age group in OkCupid Dataset") +
  theme(legend.position = "bottom")

# insight 2: Male profiles dominate the app, and in every body type except for thin and curvy, generally words in the female world
ggplot(profiles, aes(x = body_type, fill = sex)) +
  geom_bar(position = "fill") +
  coord_flip() +
  xlab("body type") +
  ylab("Count") +
  ggtitle("Distribution of body type by sex in OkCupid Dataset") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("hotpink", "gold1")) +
  theme(legend.position = "bottom")

###################


####### Job ####### 

#clean and eda
profiles <- profiles[complete.cases(profiles$job), ]
unique(profiles$job)

ggplot(profiles, aes(x = job, fill = sex)) +
  geom_bar(position = "fill") +
  coord_flip() +
  xlab("Job") +
  ylab("Proportion") +
  ggtitle("Distribution of Job by Sex in OkCupid Dataset") +
  theme(legend.position = "bottom")
###################


####### Speaks ####### 

#divide with refular expresions into the first and second language they speak
profiles <- profiles %>% mutate(language1 = gsub(",.*", "", speaks),
                              language2 = gsub("(^[^,]*,)|,.*", "", speaks)) %>% 
  select(-speaks)

# Language 1: remove NA
unique(profiles$language1)
profiles <- profiles[complete.cases(profiles$language1), ]
profiles$language1 <- gsub("\\(.*fluently.*\\)", "", profiles$language1)
profiles$language1 <- gsub("\\s+", "", profiles$language1)

# graoh violins
ggplot(profiles, aes(x = language1, y = age, fill = sex)) +
  geom_violin(trim = FALSE) +
  xlab("Language") +
  ylab("Age") +
  ggtitle("Distribution of Age by Language and Sex in OkCupid Dataset") +
  theme(legend.position = "bottom")


# Language 2: there are around 276 languages spoken with different levels
length(unique(profiles$language2))
profiles$language2 <- sub(",.*", "", profiles$language2)
profiles$language2 <- gsub("\\(.*fluently.*\\)", "", profiles$language2)
profiles$language2 <- gsub("\\(.*okay*\\)", "", profiles$language2)
profiles$language2 <- gsub("\\(.*poorly*\\)", "", profiles$language2)
profiles$language2 <- gsub("\\s+", "", profiles$language2)
unique(profiles$language2)

#terrible looking graph but shows the most spoken second language
ggplot(profiles, aes(x = language2, fill = sex)) +
  geom_bar() +
  coord_flip() +
  xlab("Language") +
  ylab("Count") +
  ggtitle("Distribution of Language by Sex in OkCupid Dataset") +
  theme(legend.position = "bottom") 

### Aggragete
# Grouping the persona based on their thoughts on zodiac signs in order to get the average age per group 
sign_lgbtq <- lgbtq %>% group_by(sign) %>% summarise(sign_lgbtq = mean(age))
  
ggplot(sign_lgbtq, aes(x = sign, fill = sign)) +
  geom_bar() +
  coord_flip() +
  xlab("Language") +
  ylab("Count") +
  ggtitle("Distribution of Language by Sex in OkCupid Dataset") +
  theme(legend.position = "bottom")


####### Drugs ####### 
length(unique(profiles$drugs))
profiles <- profiles[complete.cases(profiles$drugs), ] # Delete NAs
unique(profiles$drugs)

ggplot(profiles, aes(x = drugs, fill = sex)) +
  geom_bar(position = "dodge") +
  xlab("Drugs") +
  ylab("Proportion") +
  ggtitle("Distribution of Job by Sex in OkCupid Dataset") +
  theme(legend.position = "bottom")

###################

####### Smokes ####### 
length(unique(profiles$smokes))
unique(profiles$smokes)

ggplot(profiles, aes(x = job, fill = smokes)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  xlab("Pet Person") +
  ylab("Count") +
  ggtitle("Distribution of job by Smokes in OkCupid Dataset") +
  theme(legend.position = "bottom")

smokes_no <- profiles %>%
  filter(smokes != "no")

smokes_drinks <- profiles %>%
  filter(smokes == "when drinking")

ggplot(smokes_drinks, aes(x = drinks, fill = sex)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  xlab("Pet Person") +
  ylab("Count") +
  ggtitle("Distribution of drinks by Sex in OkCupid Dataset") +
  theme(legend.position = "bottom")

smokes_drinks_age <- profiles %>%
  filter(smokes == "when drinking" & drinks == "socially")

# Graph shows the distribution among male and femal consumers that drink socially and smokes when drinking
ggplot(smokes_drinks, aes(x = age, fill = sex)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ smokes, scales = "free_x") +
  xlab("Age") +
  ylab("Count") +
  ggtitle("Distribution of Smokes-Drinks by Age and Sex in OkCupid Dataset") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("hotpink", "gold1")) +
  theme(legend.position = "bottom")


###################

####### Pets ####### 

# Clean and remove NA
length(unique(profiles$pets))
unique(profiles$pets)
profiles <- profiles[complete.cases(profiles$pets), ]
unique(profiles$pets)

# Grouping "dropped out of space camp", and "dropped out of high school" to be part of "dropped out low education"
profiles$pets[profiles$pets %in% c("likes dogs", "has dogs", "has dogs and dislikes cats", "likes dogs and dislikes cats")] <- "Dog Person"
profiles$pets[profiles$pets %in% c("likes cats", "has cats", "dislikes dogs and likes cats", "dislikes dogs and has cats")] <- "Cat Person"
profiles$pets[profiles$pets %in% c("has dogs and has cats", "likes dogs and likes cats", "likes dogs and has cats", "has dogs and likes cats")] <- "Pet Person"
profiles$pets[profiles$pets %in% c("dislikes dogs", "dislikes cats", "dislikes dogs and dislikes cats")] <- "Not a Pet Person"

# Graph to see pets relationship

#general pet and per job
ggplot(profiles, aes(x = job, fill = pets)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  xlab("Pet Person") +
  ylab("Count") +
  ggtitle("Distribution of Pet Person by Sex in OkCupid Dataset") +
  theme(legend.position = "bottom")

# pets per specific sex
ggplot(profiles, aes(x = pets, fill = sex)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  xlab("Pet Person") +
  ylab("Count") +
  ggtitle("Distribution of Pets by Sex") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("hotpink", "gold1")) +
  theme(legend.position = "bottom")

# pets per specific age_group
ggplot(lgbtq, aes(x = pets, fill = sex)) +
  geom_bar(position = "fill") +
  coord_flip() +
  xlab("Pets") +
  ylab("Count") +
  ggtitle("Distribution of Pets by LGBTQ") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("hotpink1", "gold1", "pink1", "blue4")) +
  theme_minimal()

ggplot(profiles, aes(x = age_group, fill = pets)) +
  geom_bar(position = "dodge") +
  xlab("Pets") +
  ylab("Count") +
  ggtitle("Distribution of Pets by sex Group") +
  theme(legend.position = "bottom") +
  facet_wrap(~ age_group, ncol = 2)+ 
  scale_fill_manual(values = c("gold1", "maroon3", "pink1", "blue4", "purple1")) +
  theme_minimal()

###################


####### status #######
length(unique(profiles$status))
unique(profiles$status)

profiles %>%
  group_by(status) %>%
  summarize(avg_age = round(mean(age, na.rm = TRUE)))


ggplot(profiles, aes(x = status, fill = sex)) +
  geom_bar() +
  coord_flip() +
  xlab("status") +
  ylab("Count") +
  ggtitle("Distribution of status by sex in OkCupid Dataset")

###################



####### Location ####### 
length(unique(profiles$location))

# Left Join location
all_locations <- left_join(profiles, latlon, by = "location") 
all_locations <- left_join(all_locations, addr, by = "location") 

# County and single state
counties <- map_data("county")
CAcounty <- subset(counties, region == "california")# change to california smal c
onlyCA   <- subset(all_locations, all_locations$state =='California') # change to California this is from your DF
# if you change the name of onlyMA make sure to do it undernethe as well

# State and county outlines
ggCA <- ggplot() + 
  geom_map(data  =  CAcounty, map = CAcounty,
           aes(x = long, y = lat, 
               map_id = region, group = group), 
           fill = 'white', color = 'magenta', size = 0.25) + 
  coord_map('albers', lat0 = 39, lat1 = 45) +
  theme_map()

# Examine
ggCA

# Add points layer
ggCA +
  geom_point(data = onlyCA, 
             aes(x = lon, y = lat), color = 'blue', alpha=0.5) 

# Leaflet layers using %>% pipe
mplot<- leaflet(data=onlyCA) %>%
  addTiles() %>%
  addMarkers( popup = paste("Loc:", onlyCA$Location, "<br>",
                            "SqFt:", onlyCA$Sq..Feet,"<br>",
                            "Type:", onlyCA$Type),
              clusterOptions = markerClusterOptions()) 
mplot

###################

####### Zodiac ####### 

# Clean and remove NA
length(unique(profiles$sign))
unique(profiles$sign)
profiles <- profiles[complete.cases(profiles$sign), ]
unique(profiles$sign)

profiles <- profiles %>% mutate(sign = gsub(" .*", "", sign))





