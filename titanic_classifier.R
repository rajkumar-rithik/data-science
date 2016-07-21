known <- read.csv("E:/Kaggle Exercise/Titanic Survival/train.csv", stringsAsFactors=FALSE)
unknown <- read.csv("E:/Kaggle Exercise/Titanic Survival/test.csv", stringsAsFactors=FALSE)

unknown$Survived <- NA
tot <- rbind(known,unknown)

tot$Title <- sub('^.*, ([A-Za-z0-9 ]*)\\..*$','\\1',tot$Name)
tot$Title[tot$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
tot$Title[tot$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
tot$Title[tot$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
tot$Title <- factor(tot$Title)

tot$FamilySize <- tot$SibSp + tot$Parch + 1

tot$Surname <- sapply(tot$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
tot$FamilyID <- paste(as.character(tot$FamilySize), tot$Surname, sep="")
tot$FamilyID[tot$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(tot$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

tot$FamilyID[tot$FamilyID %in% famIDs$Var1] <- 'Small'
tot$FamilyID <- factor(tot$FamilyID)

library(rpart);
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=tot[!is.na(tot$Age),], 
                method="anova")
tot$Age[is.na(tot$Age)] <- predict(Agefit, tot[is.na(tot$Age),])

tot$Embarked[c(62,830)] = "S"
tot$Embarked <- factor(tot$Embarked)

#tot$Fare[1044] <- median(tot$Fare, na.rm=TRUE)
tot$Fare[1044] <- 20.2125

tot$FamilyID2 <- tot$FamilyID
tot$FamilyID2 <- as.character(tot$FamilyID2)
tot$FamilyID2[tot$FamilySize <= 3] <- 'Small'
tot$FamilyID2 <- factor(tot$FamilyID2)

tot$Sex <- factor(tot$Sex)
tot$Ticket <- factor(tot$Ticket)

known <- tot[1:dim(known)[1],]
unknown <- tot[892:1309,]

train <- known[1:700,]
cv <- known[701:800,]
test <- known[801:891,]

library(party)
set.seed(450)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID,
               data = known, 
               controls=cforest_unbiased(ntree=1500, mtry=4))
Prediction <- predict(fit, unknown, OOB=TRUE, type = "response")

submit <- data.frame(PassengerId = unknown$PassengerId, Survived = Prediction)
write.csv(submit, file = "E:/Kaggle Exercise/Titanic Survival/cforest3.csv", row.names = FALSE)

write.csv(known, file = "E:/Kaggle Exercise/Titanic Survival/known.csv", row.names = FALSE)
write.csv(tot, file = "E:/Kaggle Exercise/Titanic Survival/tot.csv", row.names = FALSE)
data1 <- rbind(train,cv)
