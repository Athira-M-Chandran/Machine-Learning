#Packages
#data visualization
library(ggplot2)
library(gridExtra)
library(grid)
#split data
library(caTools)
#Decision tree
library(rpart)
library(rpart.plot)
#confusion matrix
library(caret)
library(lattice)
#Naive bayes
library(klaR) 
library(e1071)
#KNN
library(class)

#-----------------------------------------------------------------------------#
                           #import dataset #

dataset <- read.csv("c:/data/processed_cleveland.csv")
#cleaning
dataset <- na.omit(dataset)
sum(is.na(dataset))

#target variable to logic values
for (i in 1:303) {
  if(dataset$num[i]>=1)
    dataset$num[i] <- 1
}

dataset$num <- as.factor(dataset$num)
dataset$ca <- as.factor(dataset$ca)
dataset$thal <- as.factor(dataset$thal)

#-----------------------------------------------------------------------------#
                      # Data exploration #
head(dataset)
str(dataset)
summary(dataset)
View(dataset)

#data visualization
ggplot(dataset, aes(x=num, fill=num)) + 
  geom_bar() +
  xlab("Heart Disease") +
  ylab("Count") +
  ggtitle("Analysis of Presence and Absence of Heart Disease") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absence", "Presence"))

age <- ggplot(dataset, aes(x=age , fill = num )) +
  geom_bar(position = position_dodge()) + 
  xlab("age in years") + 
  ylab("count")
 
sex <- ggplot(dataset, aes(sex , fill = num )) +
  geom_bar(position = position_dodge()) + 
  xlab("sex (1 = male; 0 = female)") + 
  ylab("count")

cp <- ggplot(dataset, aes(cp , fill = num )) +
  geom_bar(position = position_dodge()) + 
  xlab("chest pain") + 
  ylab("count")

trestbps <- ggplot(dataset, aes(trestbps , fill = num))+
  geom_bar(position = position_dodge()) + 
  xlab("resting blood pressure ") + 
  ylab("count")

chol <- ggplot(dataset, aes(chol , fill = num )) +
  geom_bar(position = position_dodge()) + 
  xlab("serum cholestoral in mg/dl") + 
  ylab("count")

fbs <- ggplot(dataset, aes(fbs , fill = num)) +
  geom_bar(position = position_dodge()) + 
  xlab("(fasting blood sugar)(0:false; 1:true )") + 
  ylab("count")

restecg <- ggplot(dataset, aes(restecg , fill = num )) +
  geom_bar(position = position_dodge()) + 
  xlab("resting electrocardiographic results") + 
  ylab("count")

thalach <- ggplot(dataset, aes(thalach , fill = num )) +
  geom_bar(position = position_dodge()) + 
  xlab("maximum heart rate achieved") + 
  ylab("count")

exang<- ggplot(dataset, aes(exang , fill = num )) +
  geom_bar(position = position_dodge()) + 
  xlab("exercise induced angina(1:yes;0: no)") + 
  ylab("count")

oldpeak <- ggplot(dataset, aes(oldpeak , fill = num )) +
  geom_bar(position = position_dodge()) + 
  xlab("ST depression induced by exercise relative to rest") + 
  ylab("count")

slope  <- ggplot(dataset, aes(slope , fill = num )) +
  geom_bar(position = position_dodge()) + 
  xlab("the slope of the peak exercise ST segment") + 
  ylab("count")


ca  <- ggplot(dataset, aes(ca , fill = num )) +
  geom_bar(position = position_dodge()) + 
  xlab("number of major vessels(0-3)") + 
  ylab("count")

thal <- ggplot(dataset, aes(thal , fill = num )) +
  geom_bar(position = position_dodge()) + 
  xlab("3:normal;6:fixed defect;7:reversable defect") + 
  ylab("count")

grid.arrange(age, 
             sex, 
             cp, 
             trestbps,
             chol,
             fbs,
             restecg,
             thalach,
             exang,
             oldpeak,
             slope,
             ca,
             thal,
             nrow = 5,
             top = textGrob("Heart disease", 
                            gp=gpar(fontsize=14, face = "bold"))
            
)

#-----------------------------------------------------------------------------#
                      #Split the dataset#


# initializes the random number generator
set.seed(2000)

# split the data into training and test sets.
sample_data = sample.split(dataset, SplitRatio = 0.75)
#creates a training set named train_data with rows which are marked as TRUE
train_data <- subset(dataset, sample_data == TRUE) 
#creates a testing set named test_data with rows which are marked as FALSE
test_data <- subset(dataset, sample_data == FALSE)

nrow(train_data)
nrow(test_data)

#-----------------------------------------------------------------------------#
                            #  DECISION TREE #

#rpart() package is used to create the tree. 
decision_tree<- rpart(num ~ ., method = "class", data = train_data, 
                      control = rpart.control(cp = 0),
                      parms = list(split="information"))

#plot using rpart
rpart.plot(decision_tree,type= 4 , extra=1)

#Prediction
heart_pred <- predict(object = decision_tree,
                     newdata = test_data,
                     type = "class")
#Confusion matrix
confusionMatrix(data = as.factor(heart_pred),
                reference = as.factor(test_data$num))

#table view of above
table(as.factor(heart_pred), as.factor(test_data$num))

# Accuracy is 78%.

#-----------------------------------------------------------------------------#
                          # Naive Bayes Model #

naive_bayes <- naiveBayes(num ~ ., data = train_data)
naive_bayes

# Predicting on test data'
pred_bayes <- predict(naive_bayes, newdata = test_data)

# Model Evaluation
confusionMatrix(data = as.factor(pred_bayes),
                reference = as.factor(test_data$num ))

#table view of above
table(as.factor(pred_bayes) , as.factor(test_data$num))

# Accuracy is 82%.

#-----------------------------------------------------------------------------#
                               # KNN #

train_data$ca <- as.numeric(train_data$ca)
train_data$thal <- as.numeric(train_data$thal)
test_data$ca <- as.numeric(test_data$ca)
test_data$thal <- as.numeric(test_data$thal)

# Feature Scaling
train_scale <- scale(train_data[, 1:13])

test_scale <- scale(test_data[, 1:13])
test_scale
# Model Evaluation - Choosing K
# k = 1 to 25 
for (i in 1:25) {
  classifier_knn <- knn(train = train_scale,
                        test = test_scale,
                        cl = train_data$num,
                        k = i)
  misClassError <- mean(classifier_knn != test_data$num)
  print(paste('Accuracy = k = ',i," = ", (1-misClassError) * 100))
  
}

# Fitting KNN Model 
# on training dataset
pred_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_data$num,
                      k = 12)

# Confusiin Matrix
confusionMatrix(data = as.factor(pred_knn),
                reference = as.factor(test_data$num ))

#table view of above
table(test_data$num, pred_knn)

# Accuracy is 83%.

#-----------------------------------------------------------------------------#
                      # Logistic Regression #

#fit logistic regression model
logistic <- glm(num ~., family="binomial", data=dataset)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(logistic)
logistic_reg = glm(num ~ sex + cp + trestbps + thalach + exang + 
                     slope +ca , family="binomial", data=train_data)
summary(logistic_reg)

pred_log <- predict(object = logistic_reg,
                           newdata = test_data,
                           type = "response")
pred_log
pred_glm <- ifelse(pred_log > 0.5, 1, 0)
#for checking accuracy
confusionMatrix(data = as.factor(pred_glm),
                reference = as.factor(test_data$num ))

#table view of above
table( as.factor(pred_glm), as.factor(test_data$num))

# Accuracy is 82%.

#-----------------------------------------------------------------------------#

