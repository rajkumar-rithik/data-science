library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm


train <- read.csv('E:/Kaggle Exercise/Shelter Animal/train.csv', stringsAsFactors = F)
test <- read.csv('E:/Kaggle Exercise/Shelter Animal/test.csv', stringsAsFactors = F)

# Rename the ID column so train & test match
names(train)[1] <- 'ID'

# And change ID in test to character
test$ID <- as.character(test$ID)

# Combine test & training data
full <- bind_rows(train, test)

full$Year <- year(full$DateTime)
full$Month <- month(full$DateTime)
full$Day <- day(full$DateTime)
full$Hour <- hour(full$DateTime)
full$WeekDays <- wday(full$DateTime)
full$TimeOfDay <- cut(full$Hour,breaks = c(-1,5,11,15,18,23), label = c('Latenight','Morning','Midday','Evening','Night'))

full$IsMultiColor <- grepl('/', full$Color)
full$Color1 <- sapply(full$Color, function(x) strsplit(x,split='/')[[1]][1])
full$Color2 <- sapply(full$Color, function(x) strsplit(x,split='/')[[1]][2])
full$Color2[is.na(full$Color2)] <- 'No'

full$IsMix <- grepl('/', full$Breed)
full$BreedC <- sapply(full$Breed, function(x) gsub(' Mix','',strsplit(x,split='/')[[1]][1]))

full$Sex <- ifelse(grepl('Male',full$SexuponOutcome),'Male',
                   ifelse(grepl('Female',full$SexuponOutcome),'Female','Unknown'))

full$IsIntact <- ifelse(grepl('Intact',full$SexuponOutcome),'Yes',
                        ifelse(grepl('Unknown',full$SexuponOutcome),'Unknown','No'))

# Get the time value:
full$TimeValue <- sapply(full$AgeuponOutcome,  
                         function(x) strsplit(x, split = ' ')[[1]][1])

# Now get the unit of time:
full$UnitofTime <- sapply(full$AgeuponOutcome,  
                          function(x) strsplit(x, split = ' ')[[1]][2])

# Fortunately any "s" marks the plural, so we can just pull them all out
full$UnitofTime <- gsub('s', '', full$UnitofTime)

full$TimeValue  <- as.numeric(full$TimeValue)
full$UnitofTime <- as.factor(full$UnitofTime)

# Make a multiplier vector
multiplier <- ifelse(full$UnitofTime == 'day', 1,
                     ifelse(full$UnitofTime == 'week', 7,
                            ifelse(full$UnitofTime == 'month', 30, # Close enough
                                   ifelse(full$UnitofTime == 'year', 365, NA))))

# Apply our multiplier
full$AgeinWeeks <- floor(full$TimeValue * multiplier / 7)

full$SexuponOutcome[full$SexuponOutcome==''] <- 'Neutered Male'

full$HasName <- ifelse(nchar(full$Name)==0,0,1)

# Use rpart to predict the missing age values
age_fit <- rpart(AgeinWeeks ~ AnimalType + Sex + IsIntact + BreedC + HasName, 
                 data = full[!is.na(full$AgeinWeeks), ], 
                 method = 'anova')

full$AgeinWeeks[is.na(full$AgeinWeeks)] <- predict(age_fit, full[is.na(full$AgeinWeeks), ])

# Use the age variable to make a puppy/kitten variable
full$Lifestage[full$AgeinWeeks < 8] <- 'Baby'
full$Lifestage[full$AgeinWeeks >= 8] <- 'Adult'

factorVars <- c('OutcomeType','OutcomeSubtype','AnimalType',
                'SexuponOutcome','AgeuponOutcome','BreedC','IsMultiColor','Color1','Color2',
                'HasName','IsMix','IsIntact','Sex','TimeOfDay','Lifestage')

full[factorVars] <- lapply(full[factorVars], function(x) as.factor(x))

# Reshape
BreedC <- full %>%
  group_by(BreedC) %>%
  summarise(num_animals = n()) 

# Reshape
Color1 <- full %>%
  group_by(Color1) %>%
  summarise(num_animals1 = n()) 

full <- left_join(full, BreedC)
full <- left_join(full, Color1)

full$ColorF <- ifelse(full$num_animals1<10,'Others',as.character(full$Color1))
full$ColorF <- as.factor(full$ColorF)

full$BreedC1 <- ifelse(full$num_animals<90,'Others',as.character(full$BreedC))
full$BreedC1 <- as.factor(full$BreedC1)

# Split up train and test data
#train <- full[1:26729, ]
#test  <- full[26730:nrow(full), ]
library(caret)
training <- full[1:26729,]
trainSam <- createDataPartition(y=training$AnimalType,p=0.6, list=F)

train <- training[trainSam, ]
cv <- training[-trainSam, ]
test  <- full[26730:nrow(full), ]


# Set a random seed
set.seed(731)
library(party)
library(kernlab)
# Build the model
rf_model <- randomForest(OutcomeType ~ AnimalType+AgeinWeeks+IsIntact+HasName+Hour+WeekDays+TimeOfDay+ColorF+IsMix+Sex+Month+Lifestage+BreedC1, 
                            data = train, 
                            ntree = 600,
                            importance = TRUE)

svm_model <- ksvm(OutcomeType ~ AnimalType+AgeinWeeks+IsIntact+HasName+Hour+WeekDays+TimeOfDay+ColorF+IsMix+Sex+Month+Lifestage+BreedC1, 
     data=train,type="C-svc",
     kernel=polydot(degree=3),C=10,prob.model=TRUE)

# Predict using the test set
prediction_rf <- predict(rf_model, train,  OOB=TRUE, type = "response")
prediction_svm <- predict(svm_model, train, type = 'response')
prediction_svm <- predict(svm_model, test, type = 'probabilities')

confusionMatrix(prediction_svm,train$OutcomeType)
confusionMatrix(prediction_rf,train$OutcomeType)

# Save the solution to a dataframe
solution <- data.frame('ID' = test$ID, prediction_svm)

# Write it to file
write.csv(solution, 'rf_sol6.csv', row.names = F)

rf_mod <- cforest(OutcomeType ~ AnimalType+AgeinWeeks+IsIntact+HasName+Hour+WeekDays+TimeOfDay+ColorF+IsMix+Sex+Month+Lifestage+BreedC1, 
                  data = train, 
                  controls=cforest_unbiased(ntree=600, mtry=3))

class(table(full$AnimalType,full$OutcomeType))


