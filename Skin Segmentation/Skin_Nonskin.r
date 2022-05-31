##############  PACKAGES  #############

library(ggplot2)
library(grid)
library(gridExtra)

library(lattice)
library(caret)

library(rpart)
library(rpart.plot)


###########  DATA EXPLORATION  ##########

skin <- read.csv("Skin_NonSkin.csv")
View(skin)
str(skin)     #displays internal structure
summary(skin) #displays descriptive statistics of every variable in the dataset
head(skin)    #displays top 6 values


skin$skin_color <- factor(skin$skin_color, 
                          levels = c (1, 2), 
                          labels = c (0,1)) 
# 0- skin
# 1- non skin


###########  DATA VISUALIZATION  ##########


#Histogram representation
# Blue component
B <- ggplot(data=skin, aes(x=B))+
  geom_histogram(binwidth=5, color = "black", aes(fill=skin_color)) + 
  xlab("Blue Pixel") +  
  ylab("Frequency") + 
  theme(legend.position="none")+
  ggtitle("Histogram of Blue")

# Green
G <- ggplot(data=skin, aes(x=G))+
  geom_histogram(binwidth=5, color="black", aes(fill=skin_color)) + 
  xlab("Green pixel") +  
  ylab("Frequency") + 
  theme(legend.position="none")+
  ggtitle("Histogram of Green")

# Red
R <- ggplot(data=skin, aes(x=R))+
  geom_histogram(binwidth=5, color="black", aes(fill = skin_color)) + 
  xlab("Red Pixel") +  
  ylab("Frequency") + 
  theme(legend.position="right")+
  ggtitle("Histogram of Red")

#pos <- theme(legend.position="right")
# Plot all visualizations
grid.arrange(B,
             G ,
             R ,
             nrow = 3,
             top = textGrob("Skin Non-skin Frequency Histogram",gp=gpar(fontsize=15))
)






#############  DECISION TREE   ############

skin_decision <- rpart(formula = skin_color ~.,
                       data = skin,
                       method = "class",
                       control = rpart.control(minsplit=1),
                       parms = list(split = "information"))
summary(skin_decision)

# Plot
rpart.plot(skin_decision,type= 4 , extra= 1)

newdata <- data.frame(B=74, G=85, R= 123)
print(newdata)                      
# Predicting on test data
skin_pred <- predict(object = skin_decision,
                     newdata = newdata,
                     type = "prob")
print(skin_pred)

skin_predict <- predict(object = skin_decision,
                     newdata = skin,
                     type = "class")
# Model Evaluation
skin_dec_matrix <- table(skin$skin_color, skin_predict)

# Confusion Matrix
confusionMatrix(skin_dec_matrix)

# Accuracy : 98%



############ Logistic Regression ###################



skin_log <- glm(skin_color ~ ., family = binomial(link = "logit") , data = skin)
summary(skin_log)

#newdata <- data.frame(B=120, G=100, R= 220)
newdata <- data.frame(B=74, G=85, R= 123)
print(newdata)                      
# Predicting on test data
skin_pred <- predict(object = skin_log,
                     newdata = newdata,
                     type = "response")
print(skin_pred)


skin_pred_log <- predict(object = skin_log,
                         newdata = skin,
                         type = "response")
skin_pred_log
skin_prediction <- ifelse(skin_pred_log > 0.5, 1,0)


skin_log_matrix <- table(skin$skin_color, skin_prediction)
confusionMatrix(skin_log_matrix)

# Accuracy : 91.88%