# Logistic Regression#
library(Amelia)
library(ggplot2)
library(dplyr)
library(caTools)
df.train <- read.csv('R/titanic_train.csv')
#print(head(df.train))

#print('          ')
#print(str(df.train))

#Check and visualize missing data
#missmap(df.train, main = 'missing map', col = c('yellow','black'), legend = FALSE)
ggplot(df.train,aes(Survived)) + geom_bar(aes(fill=factor(Survived)))
ggplot(df.train,aes(Pclass)) + geom_bar(aes(fill = factor(Pclass)))
ggplot(df.train,aes(Sex)) + geom_bar(aes(fill = factor(Sex)))
ggplot(df.train, aes(Age)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
ggplot(df.train, aes(SibSp)) + geom_bar()
ggplot(df.train, aes(Pclass,Age)) + geom_boxplot(aes(group = Pclass, fill = factor(Pclass)), alpha = 0.5) + scale_y_continuous(breaks = seq(min(0),max(80),by = 2)) + theme_bw()

##Inputation of age based on class
imputed_age <- function(age,class){
  out <- age
  
  for (i in 1:length(age)) {
    
    if (is.na(age[i])) {
      if (class[i] == 1){
        out[i] <- 37
      }else if (class [i] == 2){
        out[i] <- 29
      }else{
        out[i] <- 24
      }
    }else{
      out[i] <- age[i]
    }
  }
  return(out)
}

fixed.age <- imputed_age(df.train$Age,df.train$Pclass)
df.train$Age <- fixed.age
# Check for null values
#missmap(df.train, main = 'Imputation Check', col = c('yellow','black'), legend = FALSE)

# Remove the unwanted features
df.train <- df.train %>% select(-c('PassengerId','Name','Ticket','Cabin'))

# Change the some features to factor
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
#df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)

#str(df.train)


# Training the model with logistic regression
#log.model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = df.train)
# Call summary on the trained model
#summary(log.model)



#### Now for the test dataset, i'm going to do the samething i've done on the train set on the test set
### i.e cleaning among others
# Load the dataset
#df.test <- read.csv('R/titanic_test.csv')
#print(df.test)
# Check and visualize missing data
#missmap(df.test, main = 'missing map', col = c('yellow','black'), legend = FALSE)
#print(colSums(is.na(df.test)))

##Inputation of age based on class
#imputed_age2 <- function(age,class){
 # out <- age
  
  #for (i in 1:length(age)) {
    
   # if (is.na(age[i])) {
    #  if (class[i] == 1){
     #   out[i] <- 37
      #}else if (class [i] == 2){
       # out[i] <- 29
      #}else{
       # out[i] <- 24
      #}
    #}else{
     # out[i] <- age[i]
    #}
  #}
  #return(out)
#}

#fixed.age2 <- imputed_age2(df.test$Age,df.test$Pclass)
#df.test$Age <- fixed.age2

# Drop the na values for fare
#df.test <- na.omit(df.test)

# Check for null values
#missmap(df.test, main = 'Imputation Check', col = c('yellow','black'), legend = FALSE)
#print(colSums(is.na(df.test)))

# Remove the unwanted features
#df.test <- df.test %>% select(-c('PassengerId','Name','Ticket','Cabin'))

# Change the some features to factor
#df.test$Pclass <- factor(df.test$Pclass)
#df.test$Parch <- factor(df.test$Parch)
#df.test$SibSp <- factor(df.test$SibSp)

#str(df.test)
# Predict using the test data
#fitted.probabilities <- predict(log.model,df.test,type = 'response')
#fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
# Using the cbind to bind the predicted values with the test data and changing the column name to survived
#titanic.test <- cbind(df.test,fitted.results)
#colnames(titanic.test)[colnames(titanic.test) == 'fitted.results'] <- 'Survived' 


### Using the train dataset to create the test data
set.seed(101)
split <- sample.split(df.train$Survived, SplitRatio = 0.7)
final.train <- subset(df.train, split == TRUE)
final.test <- subset(df.train, split == FALSE)

final.log.model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = final.train)
summary(final.log.model)

# Predict using the test data
fitted.probabilities <- predict(final.log.model,final.test,type = 'response')
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)

# To calculate misclassification error and accuracy
misClassError <- mean(fitted.results != final.test$Survived)
print(misClassError)
accuracy <- 1 - misClassError
print(accuracy)

# Confusion Matrix
cf <- table(final.test$Survived,fitted.probabilities>0.5)
print(cf)
