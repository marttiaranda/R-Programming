# A2: National City Bank National City Bank EDA & Modeling
# Author: Martina Aranda Rivera
# Date: March 22, 2023
# Prof. Edward Kwartler
# Hult International School of Business

##### Sample #####
# Set the working directory
setwd("~/Desktop/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")

# Libraries: 
library(vtreat)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(ModelMetrics)
library(MLmetrics)
library(pROC)
library(ROSE)
library(caret)
library(rpart.plot) #visualizing
library(randomForest)
library(ranger)
library(stringi)
library(tm)
library(qdapRegex)
library(radiant.data)
library(DataExplorer)
options(scipen = 999)

# Data io, just using one CSV but you would want to perform your joins  before this:
current_customers <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/CurrentCustomerMktgResults.csv')
axiom <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/householdAxiomData.csv')
credit <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/householdCreditData.csv')
vehicle <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/householdVehicleData.csv')
data_dictionary <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/dataDictionary.csv')
prospective_customers <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/ProspectiveCustomers.csv')

##### Explore ##### 

## EDA ## 

# Strings: 
str(current_customers)
str(axiom)
str(credit)
str(vehicle)
str(prospective_customers)

# Dimensions:
dim(current_customers)
dim(axiom)
dim(credit)
dim(vehicle)
dim(prospective_customers)

# Summary:
summary(current_customers)
summary(prospective_customers)
summary(axiom)
summary(credit)
summary(vehicle)


# Unique values:

## Current Customers  ##

#count unique values per column:
length(unique(current_customers$dataID))
length(unique(current_customers$HHuniqueID))
length(unique(current_customers$Communication)) #telephone and cellular
length(unique(current_customers$LastContactDay)) #days of month 1-31
length(unique(current_customers$LastContactMonth)) #months jan-dec
length(unique(current_customers$NoOfContacts))
length(unique(current_customers$DaysPassed))
length(unique(current_customers$PrevAttempts))
length(unique(current_customers$past_Outcome))
length(unique(current_customers$CallStart)) #time
length(unique(current_customers$CallEnd)) #time
length(unique(current_customers$Y_AcceptedOffer))

# View groups of data:
unique(current_customers$Communication) #there is NA
unique(current_customers$LastContactDay)
unique(current_customers$LastContactMonth)
unique(current_customers$NoOfContacts)
unique(current_customers$DaysPassed)
unique(current_customers$PrevAttempts)
unique(current_customers$past_Outcome) #there is NA
unique(current_customers$Y_AcceptedOffer)

# Plot to see the missing values:
plot_str(current_customers)
plot_missing(current_customers) # drop: past_outcome(76%) and communication(23%)

# Drop columns to match prospective: CallStart, CallEnd

## Axiom:
#count unique values per column:
length(unique(axiom$HHuniqueID))
length(unique(axiom$headOfhouseholdGender))
length(unique(axiom$annualDonations))
length(unique(axiom$EstRace))
length(unique(axiom$PetsPurchases))
length(unique(axiom$DigitalHabits_5_AlwaysOn))
length(unique(axiom$AffluencePurchases))
length(unique(axiom$Age))
length(unique(axiom$Job))
length(unique(axiom$Marital))
length(unique(axiom$Education))

# View groups of data:
unique(axiom$headOfhouseholdGender)
unique(axiom$EstRace) # there's empty spaces
unique(axiom$PetsPurchases)
unique(axiom$DigitalHabits_5_AlwaysOn)
unique(axiom$AffluencePurchases)
unique(axiom$Age)
unique(axiom$Marital)
unique(axiom$Education) # there's NA

# Plot to see the missing values:
plot_str(axiom)
plot_missing(axiom) # Drop or fill with mean/median/mode education(4.32%) and job (0.48%)
# Check for blank spaces as space, replace with 0 and re clean the data. Keep an eye for 

## Credit:

#count unique values per column:
length(unique(credit$HHuniqueID))
length(unique(credit$DefaultOnRecord))
length(unique(credit$RecentBalance))
length(unique(credit$HHInsurance))
length(unique(credit$CarLoan))

# View groups of data:
unique(credit$DefaultOnRecord) #dummies
unique(credit$HHInsurance) #dummies
unique(credit$CarLoan) #dummies

# Plot to see the missing values:
plot_str(credit)
plot_missing(credit) #nothing is missing.. check for blank spaces

## Vehicle:
#count unique values per column:
length(unique(vehicle$HHuniqueID))
length(unique(vehicle$carMake))
length(unique(vehicle$carModel))
length(unique(vehicle$carYr))

# View groups of data:
unique(vehicle$carMake) #empty spaces
unique(vehicle$carModel) #empty spaces
unique(vehicle$carYr)

# Plot to see the missing values:
plot_str(vehicle)
plot_missing(vehicle) # drop or replace with median/mode/mean for carYr

## Prospective Consumers:
#count unique values per column: 1000 consumers
length(unique(prospective_customers$dataID))
length(unique(prospective_customers$HHuniqueID))
length(unique(prospective_customers$Communication))
length(unique(prospective_customers$LastContactDay))
length(unique(prospective_customers$LastContactMonth))
length(unique(prospective_customers$NoOfContacts))
length(unique(prospective_customers$DaysPassed))
length(unique(prospective_customers$PrevAttempts))
length(unique(prospective_customers$past_Outcome))
length(unique(prospective_customers$Y_AcceptedOffer))

# View groups of data:
unique(prospective_customers$Communication) # there is NA
unique(prospective_customers$LastContactDay)
unique(prospective_customers$LastContactMonth)
unique(prospective_customers$NoOfContacts)
unique(prospective_customers$DaysPassed)
unique(prospective_customers$PrevAttempts)
unique(prospective_customers$past_Outcome) # there is NA
unique(prospective_customers$Y_AcceptedOffer) #there is NA

# Plot to see the missing values:
plot_str(prospective_customers)
plot_missing(prospective_customers) # Match the columns to current and vise versa
# drop: y_accepted(100%)? past_outcome(75.7%)? communication(22.1%)? 

# Find all nulls:
colSums(is.na(current_customers)) # NA: communication and past_outcome
colSums(is.na(prospective_customers)) # NA: communication, past_outcome, and y_acceptedOffer
colSums(is.na(axiom)) # NA: job and education 
colSums(is.na(vehicle)) # NA: carYr
colSums(is.na(credit)) #no NA at all


# Visualize missing and NA values for all:

# Plots to see relationships:
ggplot(axiom, aes(x = Age, y = annualDonations)) + 
  geom_point()


##### Modify #####

# Current Customers: 
# Drop columns to match prospective: CallStart, CallEnd
current_customers <- subset(current_customers, select = -c(CallStart, CallEnd))
colSums(is.na(current_customers)) 

# Left Join tables
current_customers_all <- current_customers %>%
  left_join(axiom, by = "HHuniqueID") %>%
  left_join(credit, by = "HHuniqueID") %>%
  left_join(vehicle, by = "HHuniqueID")

prospective_customers_all <- prospective_customers %>%
  left_join(axiom, by = "HHuniqueID") %>%
  left_join(credit, by = "HHuniqueID") %>%
  left_join(vehicle, by = "HHuniqueID")

# new EDA
head(current_customers_all)
summary(current_customers_all)
sum(is.na(current_customers_all$Communication))
plot_missing(current_customers_all)

### Replace with mode RUN IT LATER
current_customers_all$Communication[is.na(current_customers_all$Communication)] <- names(which.max(table(current_customers_all$Communication)))
head(current_customers_all)

# Replace blanks with NAs - Current Customers
current_customers_all[(current_customers_all) == ""] <- NA
sum(is.na(current_customers_all))
plot_missing(current_customers_all) # NA's present: Job(mode), education(mode), carYr(median), Comunication(mode), past_Outcome(mode)

# Clean Current_customers_all: replace all NA with mode, median. Drop past_Outcome and annual donnation
current_customers_all <- subset(current_customers_all, select = -past_Outcome)
current_customers_all <- subset(current_customers_all, select = -annualDonations)
current_customers_all$Communication[is.na(current_customers_all$Communication)] <- names(which.max(table(current_customers_all$Communication)))
current_customers_all$Education[is.na(current_customers_all$Education)] <- names(which.max(table(current_customers_all$Education)))
current_customers_all$Job[is.na(current_customers_all$Job)] <- names(which.max(table(current_customers_all$Job)))
current_customers_all$Job[is.na(current_customers_all$carYr)] <- names(which.max(table(current_customers_all$carYr)))
current_customers_all$carMake[is.na(current_customers_all$carMake)] <- names(which.max(table(current_customers_all$carMake)))
current_customers_all$carModel[is.na(current_customers_all$carModel)] <- names(which.max(table(current_customers_all$carModel)))
current_customers_all$EstRace[is.na(current_customers_all$EstRace)] <- names(which.max(table(current_customers_all$EstRace)))

# deleted age_group ## dont forget to drop the column!! 

# Replace blanks with NAs - Prospecive customers
prospective_customers_all[(prospective_customers_all) == ""] <- NA
sum(is.na(prospective_customers_all))
plot_missing(prospective_customers_all)

# Clean prospective_customers_all: replace all NA with mode, median. Drop past_Outcome and annual donation
prospective_customers_all <- subset(prospective_customers_all, select = -past_Outcome)
prospective_customers_all <- subset(prospective_customers_all, select = -annualDonations)
prospective_customers_all$Communication[is.na(prospective_customers_all$Communication)] <- names(which.max(table(prospective_customers_all$Communication)))
prospective_customers_all$Education[is.na(prospective_customers_all$Education)] <- names(which.max(table(prospective_customers_all$Education)))
prospective_customers_all$Job[is.na(prospective_customers_all$Job)] <- names(which.max(table(prospective_customers_all$Job)))
prospective_customers_all$Job[is.na(prospective_customers_all$carYr)] <- names(which.max(table(prospective_customers_all$carYr)))
prospective_customers_all$carMake[is.na(prospective_customers_all$carMake)] <- names(which.max(table(prospective_customers_all$carMake)))
prospective_customers_all$carModel[is.na(prospective_customers_all$carModel)] <- names(which.max(table(prospective_customers_all$carModel)))
prospective_customers_all$EstRace[is.na(prospective_customers_all$EstRace)] <- names(which.max(table(prospective_customers_all$EstRace)))


##### VTREAT #####

# Select the variables, we as experts think makes sense
keeps <- c("Communication", "NoOfContacts", "DaysPassed", "PrevAttempts", "Y_AcceptedOffer", "headOfhouseholdGender", 
           "PetsPurchases", "DigitalHabits_5_AlwaysOn", "AffluencePurchases", "Age", 
           "Job", "Marital", "Education", "DefaultOnRecord", "RecentBalance", "HHInsurance", 
           "CarLoan", "carMake", "carModel", "carYr")

keeps2 <- c("Communication", "NoOfContacts", 
           "PrevAttempts", "Y_AcceptedOffer", "headOfhouseholdGender", 
           "EstRace", "PetsPurchases", "DigitalHabits_5_AlwaysOn", "AffluencePurchases", "Age", 
           "Job", "Marital", "Education", "DefaultOnRecord", "RecentBalance", "HHInsurance", 
           "CarLoan", "carMake", "carModel", "carYr")

# 10% variable treatment, 75% training & 15% validation
set.seed(100)
trainPercentRows      <- round(nrow(current_customers_all) %*% .75)
validationPercentRows <- round(nrow(current_customers_all) %*% .15)

# Sample index for training
trainIdx <- sample(1:nrow(current_customers_all), trainPercentRows)

# Identify the rows not in the training set, its the "difference" 
remainingRows <- setdiff(1:nrow(current_customers_all), trainIdx)

# Create another sample but limit the row numbers to only those identified as *not* in training to get the validation index
validationIdx <-sample(remainingRows, validationPercentRows)

# With the two idx vectors of randomly generated numbers, without any overlap you can put them in the "row" position for indexing. 
trainSet      <- current_customers_all[trainIdx,  names(current_customers_all) %in% keeps]
validationSet <- current_customers_all[validationIdx, names(current_customers_all) %in% keeps]
prospectiveSet <- prospective_customers_all[names(prospective_customers_all) %in% keeps] ## prospective testing!!!! 

# Here you combine both the index and put that with a minus.  Essentially removing any rows in training, or validation indexing leaving you with the test set.
prepData <- current_customers_all[-c(trainIdx, validationIdx), names(current_customers_all) %in% keeps]

## THIS IS CLASSIFICATION SO C IS USED
x_vars <- c("Communication", "LastContactDay", "LastContactMonth", "NoOfContacts", "DaysPassed", 
           "PrevAttempts", "Y_AcceptedOffer", "headOfhouseholdGender", 
           "EstRace", "PetsPurchases", "DigitalHabits_5_AlwaysOn", "AffluencePurchases", "Age", 
           "Job", "Marital", "Education", "DefaultOnRecord", "RecentBalance", "HHInsurance", 
           "CarLoan", "carMake", "carModel", "carYr")

plan <- designTreatmentsC(dframe        = prepData, 
                          varlist       = x_vars,
                          outcomename   = "Y_AcceptedOffer",
                          outcometarget = "Accepted")

# Apply the plan to both sections for modeling and evaluation next
treatedTrain      <- prepare(plan, trainSet)
treatedValidation <- prepare(plan, validationSet)
treatedProspective <- prepare(plan, prospectiveSet)

##### Model #####

## Logistic Regression ## 

fit <- glm(as.factor(Y_AcceptedOffer) ~ ., treatedTrain, family='binomial')

# First model
summary(fit)

# Let's make it parsimonious 
parismonyFit <- step(fit, direction = 'backward')

# Some predictions
donationProbability <- predict(parismonyFit, treatedTrain, type='response')

head(donationProbability)

# Organize some test results
predDF <- data.frame(actual = treatedTrain$Y_AcceptedOffer,
                     probs  = donationProbability)
ggplot(predDF, aes(x=probs, group= actual, color = actual))+geom_density()


# Assess - calculate accuracy, plot the ROC and make a confusion matrix etc.  Lots of ways to assess a model!
cutoff <- 0.47
predClass <- ifelse(donationProbability>=cutoff,'Yes','No')
c_matrix <- table(treatedTrain$Y_AcceptedOffer, predClass)
c_matrix
sum(diag(c_matrix))/sum(c_matrix) # Accuracy for this model
## the highest accuracy w/o compromising quality: 0.685

## Decision Tree ##

# Fit a decision tree with caret
set.seed(100)
fit <- train(as.factor(Y_AcceptedOffer) ~., #formula based
             data = treatedTrain, #data in #treated train
             #"recursive partitioning (trees)
             method = "rpart", 
             #Define a range for the CP to test
             tuneGrid = data.frame(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1)), 
             #ie don't split if there are less than 1 record left and only do a split if there are at least 2+ records
             control = rpart.control(minsplit = 1, minbucket = 2)) #tree

# Examine
fit

# Plot the CP Accuracy Relationship to adust the tuneGrid inputs
plot(fit)

# Plot a pruned tree
prp(fit$finalModel, extra = 1)

# Make some predictions on the training set
trainCaret <- predict(fit, treatedTrain)
head(trainCaret)

# Get the conf Matrix
confusionMatrix(trainCaret, as.factor(treatedTrain$Y_AcceptedOffer))

# Now more consistent accuracy & fewer rules!
ValidationCaret <- predict(fit,treatedValidation)
confusionMatrix(ValidationCaret,as.factor(treatedValidation$Y_AcceptedOffer))


## Random Forest ##

# Fit a random forest model with Caret
downSampleFit <- train(Y_AcceptedOffer ~ .,
                       data = treatedTrain,
                       method = "rf",
                       verbose = FALSE,
                       ntree = 3,
                       tuneGrid = data.frame(mtry = 3))
downSampleFit

# Too see probabilities
predProbs   <- predict(downSampleFit,  
                       treatedTrain, 
                       type = c("prob"))

# To get classes with 0.50 cutoff
predClasses <- predict(downSampleFit,  treatedTrain)

# Confusion Matrix; CARET
caret::confusionMatrix(predClasses, as.factor(treatedTrain$Y_AcceptedOffer))

# Other interesting model artifacts
varImp(downSampleFit)
plot(varImp(downSampleFit), top = 20)

# Add more trees to the forest with the randomForest package (caret takes a long time bc its more thorough)
forest1 <- randomForest(as.factor(Y_AcceptedOffer) ~ .,
                           data  = treatedTrain, 
                           ntree = 500,
                           mtry  = 3)

# Confusion Matrix, compare to 3 trees
trainClass <- predict(forest1, treatedTrain)
confusionMatrix(trainClass, as.factor(treatedTrain$Y_AcceptedOffer))

# Look at improved var importance
varImpPlot(forest1)


# plot the RF with a legend
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(forest2, log="y")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(forest2$err.rate),col=1:4,cex=0.8,fill=1:4)

# Look at improved var importance
varImpPlot(forest2)

# Let's optimize # of trees 
forest2 <- randomForest(as.factor(Y_AcceptedOffer) ~ .,
                           data = treatedTrain, 
                           ntree=100,
                           mtry = 3)
 
# Confusion Matrix
trainClass <- predict(forest2, treatedTrain)
confusionMatrix(trainClass, as.factor(treatedTrain$Y_AcceptedOffer))

### Now let's apply to the validation validation set
sample_fit  <- predict(downSampleFit, treatedValidation)
forest1_val <- predict(forest1,    treatedValidation)
forest2_val  <- predict(forest2,    treatedValidation)

# Accuracy Comparison from MLmetrics and natural occurence in the test set
Accuracy(treatedValidation$Y_AcceptedOffer, sample_fit)
Accuracy(treatedValidation$Y_AcceptedOffer, forest1_val)
Accuracy(treatedValidation$Y_AcceptedOffer, forest2_val)
proportions(table(treatedValidation$Y_AcceptedOffer))


##### Assess #####

## Prospective Customer ##

prospectiveSet <- prospective_customers_all[names(prospective_customers_all) %in% keeps] ## prospective testing!!!! 
treatedProspective <- prepare(plan, prospectiveSet)

# Random Forest2 #

# Confusion Matrix
trainClass <- predict(forest2, treatedProspective)
# confusionMatrix(trainClass, as.factor(treatedProspective$Y_AcceptedOffer))

forest2_val  <- predict(forest2, treatedProspective, type = 'prob') #probability of accept and not


# Assess - calculate accuracy, plot the ROC and make a confusion matrix etc.  Lots of ways to assess a model!
cutoff <- 0.47
predClass <- ifelse(forest2_val[,'Accepted']>=cutoff,'Accepted','DidNotAccept')

# Accuracy(treatedProspective$Y_AcceptedOffer, predClass)
proportions(table(treatedProspective$Y_AcceptedOffer))

# Organize some test results
predDF <- data.frame(HHuniqueID = prospective_customers_all$HHuniqueID,
                     actual = predClass,
                     probs  = forest2_val) %>% 
  filter(actual == 'Accepted')
predDF

#desc order for prob, limit 100 
predDF100 <- predDF %>% 
  arrange(desc(probs.Accepted))
head(predDF100, 100)


#### Download the predDF as a csv ####
write.csv(predDF100, file = "top_100_MAR", row.names = FALSE)


##### 2nd EDA After Join#####

## Merge/Join preDF100 ##
prospective_customers_all_100 <- inner_join(predDF100, prospective_customers_all, by = "HHuniqueID")

## Insights & EDA ##
head(prospective_customers_all_100)

# Exploring the data
#count unique values per column: 100 consumers
length(unique(prospective_customers_all_100$dataID))
length(unique(prospective_customers_all_100$HHuniqueID))
length(unique(prospective_customers_all_100$Communication))
length(unique(prospective_customers_all_100$LastContactDay)) # drop
length(unique(prospective_customers_all_100$LastContactMonth)) #drop
length(unique(prospective_customers_all_100$NoOfContacts))
length(unique(prospective_customers_all_100$DaysPassed))
length(unique(prospective_customers_all_100$PrevAttempts))
length(unique(prospective_customers_all_100$past_Outcome))
length(unique(prospective_customers_all_100$actual)) #drop?
length(unique(prospective_customers_all_100$probs.Accepted))
length(unique(prospective_customers_all_100$probs.DidNotAccept))
length(unique(prospective_customers_all_100$Y_AcceptedOffer)) #drop NA

# view groups of data
unique(prospective_customers_all_100$Communication) #clean NA
unique(prospective_customers_all_100$LastContactDay) # drop
unique(prospective_customers_all_100$LastContactMonth) #drop
unique(prospective_customers_all_100$NoOfContacts)
unique(prospective_customers_all_100$PrevAttempts)
unique(prospective_customers_all_100$DaysPassed)
unique(prospective_customers_all_100$PrevAttempts)
unique(prospective_customers_all_100$past_Outcome)
unique(prospective_customers_all_100$actual) #clean NA
unique(prospective_customers_all_100$probs.Accepted)
unique(prospective_customers_all_100$probs.DidNotAccept)
unique(prospective_customers_all_100$Y_AcceptedOffer) #drop NA
unique(prospective_customers_all_100$Age)

# plot missing values or NA
plot_missing(prospective_customers_all_100)
summary(prospective_customers_all_100)

# Replace blanks with NAs - Prospecive customers
prospective_customers_all_100[(prospective_customers_all_100) == ""] <- NA
sum(is.na(prospective_customers_all_100))

# Drop Columns:
prospective_customers_all_100 <- subset(prospective_customers_all_100, select = -Y_AcceptedOffer)

# Fix NAs with mode and median 
prospective_customers_all_100$Communication[is.na(prospective_customers_all_100$Communication)] <- names(which.max(table(prospective_customers_all_100$Communication)))
prospective_customers_all_100$past_Outcome[is.na(prospective_customers_all_100$past_Outcome)] <- names(which.max(table(prospective_customers_all_100$past_Outcome)))
prospective_customers_all_100$carYr[is.na(prospective_customers_all_100$carYr)] <- names(which.max(table(prospective_customers_all_100$carYr)))

# Creating new columns:
prospective_customers_all_100$age_group <- cut(prospective_customers_all_100$Age, 
                                               breaks = c(18, 27, 37, 47, 57, 67, 77, 87, 97, 107), 
                                               labels = c("18-27", "28-37", "38-47", "48-57", "58-67", "68-77", "78-87", "88-97", "98+"), 
                                               include.lowest = TRUE)
##### Insight 1  #####

# Aggregate of average age that is female or male the call more.
result <- aggregate(Age ~ headOfhouseholdGender, data = prospective_customers_all_100, FUN = function(x) round(mean(x), digits = 0))
result <- result %>% 
  arrange(desc(headOfhouseholdGender))
result

# Group_By to see the mean of the Previous attempts per Gender.
mean_attempt_table <- prospective_customers_all_100 %>%
  group_by(headOfhouseholdGender) %>%
  summarise(mean_attempts = mean(PrevAttempts))
mean_attempt_table

# Distribution of Age Group by Head of Household Gender
ggplot(prospective_customers_all_100, aes(x = age_group, fill = headOfhouseholdGender)) +
  geom_bar(position = "fill") +
  xlab("Age Group") +
  ylab("Count") +
  ggtitle("Distribution of Age Group by Head of Household Gender") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("darkgreen", "darkseagreen")) +
  theme_minimal()

# Distribution of PrevAttempts by Head of Household Gender
ggplot(prospective_customers_all_100, aes(x = PrevAttempts, fill=  headOfhouseholdGender)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  xlab("PrevAttempts") +
  ylab("Count") +
  ggtitle("Distribution of PrevAttempts by Head of Household Gender") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("darkgreen", "darkseagreen")) +
  theme_minimal()


##### Insight 2 #####

# Distribution of age_group by Communication
ggplot(prospective_customers_all_100, aes(x = age_group, fill = Communication)) +
  geom_bar(position = "dodge") +
  xlab("Age Group") +
  ylab("Count") +
  ggtitle("Distribution of Age Group by Communication") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("darkgreen", "darkseagreen")) +
  theme_minimal()

# Distribution of Car Make by Head of Household Gender ## NOT A GREAT GRAPH  
ggplot(prospective_customers_all_100, aes(x = carMake, fill = Communication)) +
  geom_bar(position = "fill") +
  coord_flip() +
  xlab("Car Make") +
  ylab("Count") +
  ggtitle("Distribution of Car Make by Head of Household Gender") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("darkgreen", "darkseagreen", "seagreen")) +
  theme_minimal()

##### Insight 3 #####

## Job Influence ##

# Group_By to see the mean of the age people per Job position. 
mean_age_table <- prospective_customers_all_100 %>%
  group_by(Job) %>%
  summarise(mean_age = round(mean(Age)))
mean_age_table

# Distribution of Job by Head of Household Gender
ggplot(prospective_customers_all_100, aes(x = Job, fill = headOfhouseholdGender)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  xlab("Job") +
  ylab("Count") +
  ggtitle("Distribution of Job by Head of Household Gender") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("darkgreen", "darkseagreen")) +
  theme_minimal()

# Messy graph result but helps me understand that the consumers are not all the ages in every Job.
ggplot(prospective_customers_all_100, aes(x = Job, y = headOfhouseholdGender, fill = age_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Job") +
  ylab("Y-axis Label") +
  ggtitle("Distribution of Job by Head of Household Gender") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("darkgreen", "darkseagreen", "chartreuse3", "chartreuse4", "darkolivegreen3", "darkolivegreen", "lightgreen", "olivedrab2")) +
  theme_minimal()

## Marital Status ## 

# Distribution of Marital by Head of Household Gender
ggplot(prospective_customers_all_100, aes(x = Marital, fill = headOfhouseholdGender)) +
  geom_bar(position = "dodge") +
  xlab("Marital") +
  ylab("Count") +
  ggtitle("Distribution of Marital by Head of Household Gender") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("darkgreen", "darkseagreen")) +
  theme_minimal()

# Distribution of Marital by Age and Sex and Head of Household Gender
ggplot(prospective_customers_all_100, aes(x = Marital, y = Age, fill = headOfhouseholdGender)) +
  geom_violin(trim = FALSE) +
  xlab("Marital") +
  ylab("Age") +
  ggtitle("Distribution of Marital by Age and Sex and Head of Household Gender") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("darkgreen", "darkseagreen")) +
  theme_minimal()

## Education ## 

# Distribution of Education by Head of Household Gender
ggplot(prospective_customers_all_100, aes(x = Education, fill = headOfhouseholdGender)) +
  geom_bar(position = "dodge") +
  xlab("Education") +
  ylab("Count") +
  ggtitle("Distribution of Education by Head of Household Gender") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("darkgreen", "darkseagreen")) +
  theme_minimal()

## Pets Purchased ## 

# Distribution of Pets Purchases by Head of Household Gender
ggplot(prospective_customers_all_100, aes(x = PetsPurchases, fill = headOfhouseholdGender)) +
  geom_bar(position = "dodge") +
  xlab("Age Group") +
  ylab("Count") +
  ggtitle("Distribution of Pets Purchases by Head of Household Gender") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("darkgreen", "darkseagreen")) +
  theme_minimal()

## Cars ## 

# Distribution of Car Make by  Head of Household Gender
ggplot(prospective_customers_all_100, aes(x = carMake, fill=  headOfhouseholdGender)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  xlab("CarMake") +
  ylab("Count") +
  ggtitle("Distribution of CarMake by Head of Household Gender") +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("darkgreen", "darkseagreen")) +
  theme_minimal()

