rm(list=ls())

library(gains)
library(caret)
library(ROCR)
library(magrittr)
library(tidyverse)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(ggplot2)
library(reshape)
library(gridExtra)

#to random select datasets to avoid conflict with other groups with similar datasets
#numberOfRows <- nrow(health)
#health <- read.csv("healthcare-dataset-stroke-data.csv")
#View(health)
#set.seed(100)
#h1.index <- sample(numberOfRows, numberOfRows*0.85)
#health1 <- health[h1.index, ]
#View(health1)
#write.csv(health1, "Stroke.csv")

## Loading data ##
stroke.df <- read.csv("Stroke.csv")
View(stroke.df)
str(stroke.df)
summary(stroke.df)

## Data Cleaning ##

# Deleting the record where gender = "other" #
stroke.df <- stroke.df[-c(2871),]

# Removing Id column from the dataframe #
stroke.df <- stroke.df[, -c(1)]

# Formatting the variables #

stroke.df$gender <- factor(stroke.df$gender)
stroke.df$hypertension <- factor(stroke.df$hypertension)
stroke.df$heart_disease <- factor(stroke.df$heart_disease)
stroke.df$ever_married <- factor(stroke.df$ever_married)
stroke.df$work_type <- factor(stroke.df$work_type)
stroke.df$Residence_type <- factor(stroke.df$Residence_type)
stroke.df$bmi <- as.numeric(stroke.df$bmi)
stroke.df$smoking_status <- factor(stroke.df$smoking_status)
## Include the below line of code for exploratory analysis
##stroke.df$stroke <- factor(stroke.df$stroke)

# Summary Statistics #
summary(stroke.df)

# Number of records with NAs in bmi column #
nrow(stroke.df[is.na(stroke.df$bmi),])

# Imputing NA values with mean value in bmi column #

avgbmi <- stroke.df%>%
  group_by(gender)%>%
  summarise(avg_bmi = mean(bmi,na.rm = TRUE), n=n())

avgbmi

stroke.df$bmi <- ifelse(is.na(stroke.df$bmi)==TRUE,
                        avgbmi$avg_bmi[avgbmi$gender %in% stroke.df$gender],
                        stroke.df$bmi)

View(stroke.df)

# New Summary Statistics #
summary(stroke.df)

#Descriptive Summary
stroke.df1 <- stroke.df[, -c(1,3,4,5,6,7,10,11)]
str(stroke.df)

h1_summary_df <- data.frame( mean=sapply(stroke.df1, mean,na.rm=TRUE), 
                             median=sapply(stroke.df1, median,na.rm=TRUE), 
                             sd=sapply(stroke.df1, sd,na.rm=TRUE), 
                             variance=sapply(stroke.df1, var,na.rm=TRUE),
                             min=sapply(stroke.df1, min,na.rm=TRUE), 
                             max=sapply(stroke.df1, max,na.rm=TRUE), 
                             count=sapply(stroke.df1, length),
                             miss.val=sapply(stroke.df1, function(x)
                               sum(length(which(is.na(x))))))

summary (h1_summary_df)
print(h1_summary_df)
view(h1_summary_df)

# ___________________ Not Necessary to show in presentation _________________________ #

# Age distribution of the data #
ggplot(stroke.df) + geom_histogram(aes(x = age),color = "black", fill = "red", binwidth = 3)

# Average glucose level distribution of the data  # 
ggplot(stroke.df) + geom_histogram(aes(x = avg_glucose_level), color = "black", fill = "red", binwidth = 8)

# BMI distribution of the data  #
ggplot(stroke.df) + geom_histogram(aes(x = bmi), color = "black", fill = "red", binwidth = 3)

# _____________________________________________________________________________________ #

# Correlation Matrix # 
stroke_corr <- stroke.df[,-c(1,3,4,5,6,7,10,11)]
cor.mat <- round(cor(stroke_corr),2) # rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))

# scatter plot matrix with GGally 
library(GGally)
ggpairs(stroke.df[, c(2,8,9)])

# Age distribution of the patients who suffered stroke #
stroke.df%>%
  filter(stroke == 1)%>%
  group_by(age)%>%
  count(stroke)%>%
  ggplot(aes(x = age,weight = n))+geom_histogram(fill = 'red',color = 'black', binwidth = 3)+labs(
    title = 'Age Distribution of Patients Who suffered Strokes',
    y = "Frequency"
  )

# BMI distribution of the patients who suffered stroke # 
stroke.df%>%
  filter(stroke == 1)%>%
  group_by(bmi)%>%
  count(stroke)%>%
  ggplot(aes(x = bmi,weight = n))+geom_histogram(fill = 'red',color = 'black', binwidth = 2)+labs(
    title = 'BMI Distribution of Patients Who suffered Strokes',
    y = "Frequency"
  )

# Average glucose level of the patients who suffered stroke #
stroke.df%>%
  filter(stroke == 1)%>%
  group_by(avg_glucose_level)%>%
  count(stroke)%>%
  ggplot(aes(x = avg_glucose_level,weight = n))+geom_histogram(fill = 'red',color = 'black', binwidth = 8)+labs(
    title = 'Average Glucose Distribution of Patients Who suffered Strokes',
    y = "Frequency"  )


# Count of Levels for each categorical variable #
p1 <- stroke.df %>%
  ggplot(aes(x = gender, fill = stroke)) +
  geom_bar()

p2 <- stroke.df %>%
  ggplot(aes(x = hypertension, fill = stroke)) +
  geom_bar()

p3 <- stroke.df %>%
  ggplot(aes(x = heart_disease, fill = stroke)) +
  geom_bar()

p4 <- stroke.df %>%
  ggplot(aes(x = ever_married, fill = stroke)) +
  geom_bar()

p5 <- stroke.df %>%
  ggplot(aes(x = work_type, fill = stroke)) +
  geom_bar()

p6 <- stroke.df %>%
  ggplot(aes(x = Residence_type, fill = stroke)) +
  geom_bar()

p7 <- stroke.df %>%
  ggplot(aes(x = smoking_status, fill = stroke)) +
  geom_bar()

grid.arrange(grobs = list(p1, p2, p3,
                          p4, p5, p6,
                          p7),
             ncol = 3,
             top = "Count of Levels for Each Categorical Variable"
)

#Proportion of categorical variables contributing to stroke
dat_prop <- stroke.df %>%
  group_by(gender) %>%
  summarise(prop = sum(stroke == "1")/length(gender))

p1 <- dat_prop %>%
  ggplot(aes(x = gender, y = prop)) +
  geom_col(fill = "#00BFC4")

dat_prop <- stroke.df %>%
  group_by(hypertension) %>%
  summarise(prop = sum(stroke == "1")/length(hypertension))

p2 <- dat_prop %>%
  ggplot(aes(x = hypertension, y = prop)) +
  geom_col(fill = "#00BFC4")

dat_prop <- stroke.df %>%
  group_by(heart_disease) %>%
  summarise(prop = sum(stroke == "1")/length(heart_disease))

p3 <- dat_prop %>%
  ggplot(aes(x = heart_disease, y = prop)) +
  geom_col(fill = "#00BFC4")

dat_prop <- stroke.df %>%
  group_by(ever_married) %>%
  summarise(prop = sum(stroke == "1")/length(ever_married))

p4 <- dat_prop %>%
  ggplot(aes(x = ever_married, y = prop)) +
  geom_col(fill = "#00BFC4")

dat_prop <- stroke.df %>%
  group_by(work_type) %>%
  summarise(prop = sum(stroke == "1")/length(work_type))

p5 <- dat_prop %>%
  ggplot(aes(x = work_type, y = prop)) +
  geom_col(fill = "#00BFC4")

dat_prop <- stroke.df %>%
  group_by(Residence_type) %>%
  summarise(prop = sum(stroke == "1")/length(Residence_type))

p6 <- dat_prop %>%
  ggplot(aes(x = Residence_type, y = prop)) +
  geom_col(fill = "#00BFC4")

dat_prop <- stroke.df %>%
  group_by(smoking_status) %>%
  summarise(prop = sum(stroke == "1")/length(smoking_status))

p7 <- dat_prop %>%
  ggplot(aes(x = smoking_status, y = prop)) +
  geom_col(fill = "#00BFC4")

grid.arrange(grobs = list(p1, p2, p3,
                          p4, p5, p6,
                          p7),
             ncol = 3,
             top = "Proportion of Strokes for Each Factor"
)

#Boxplot of Stroke and Age Across Factors
p1 <- stroke.df %>%
  ggplot(aes(x = gender, y = age, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p2 <- stroke.df %>%
  ggplot(aes(x = hypertension, y = age, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p3 <- stroke.df %>%
  ggplot(aes(x = heart_disease, y = age, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p4 <- stroke.df %>%
  ggplot(aes(x = ever_married, y = age, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p5 <- stroke.df %>%
  ggplot(aes(x = work_type, y = age, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p6 <- stroke.df %>%
  ggplot(aes(x = Residence_type, y = age, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p7 <- stroke.df %>%
  ggplot(aes(x = smoking_status, y = age, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

grid.arrange(grobs = list(p1, p2, p3,
                          p4, p5, p6,
                          p7), 
             ncol = 3,
             top = "Stroke and Age Across Factors"
)


#Stroke and Average Glucose Level Across Factors
p1 <- stroke.df %>%
  ggplot(aes(x = gender, y = avg_glucose_level, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p2 <- stroke.df %>%
  ggplot(aes(x = hypertension, y = avg_glucose_level, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p3 <- stroke.df %>%
  ggplot(aes(x = heart_disease, y = avg_glucose_level, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p4 <- stroke.df %>%
  ggplot(aes(x = ever_married, y = avg_glucose_level, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p5 <- stroke.df %>%
  ggplot(aes(x = work_type, y = avg_glucose_level, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p6 <- stroke.df %>%
  ggplot(aes(x = Residence_type, y = avg_glucose_level, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p7 <- stroke.df %>%
  ggplot(aes(x = smoking_status, y = avg_glucose_level, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

grid.arrange(grobs = list(p1, p2, p3,
                          p4, p5, p6,
                          p7), 
             ncol = 3,
             top = "Stroke and Average Glucose Level Across Factors"
)


#"Stroke and BMI Across Factors"
p1 <- stroke.df %>%
  ggplot(aes(x = gender, y = bmi, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p2 <- stroke.df %>%
  ggplot(aes(x = hypertension, y = bmi, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p3 <- stroke.df %>%
  ggplot(aes(x = heart_disease, y = bmi, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p4 <- stroke.df %>%
  ggplot(aes(x = ever_married, y = bmi, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p5 <- stroke.df %>%
  ggplot(aes(x = work_type, y = bmi, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p6 <- stroke.df %>%
  ggplot(aes(x = Residence_type, y = bmi, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

p7 <- stroke.df %>%
  ggplot(aes(x = smoking_status, y = bmi, color = stroke)) +
  geom_boxplot() +
  theme(legend.position="none")

grid.arrange(grobs = list(p1, p2, p3,
                          p4, p5, p6,
                          p7), 
             ncol = 3,
             top = "Stroke and BMI Across Factors"
)

## Partitioning the data ##

set.seed(2)
numberOfRows <- nrow(stroke.df)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- stroke.df[train.index,]
valid.df <- stroke.df[-train.index,]

## Running Logistic Regression Model
logit.reg <- glm(stroke ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)

# use predict() with type = "response" to compute predicted probabilities. #
logit.reg.pred <- predict(logit.reg, valid.df[,-c(11)], type = "response") 

# first 10 actual and predicted records #
data.frame(actual = valid.df$stroke[1:10], predicted = logit.reg.pred[1:10])

# Confusion Matrix #

confusionMatrix(table(round(predict(logit.reg, newdata = valid.df, type="response"),1) >= 0.5, valid.df$stroke == 1))

gain <- gains(as.numeric(valid.df$stroke), logit.reg.pred, groups=10)

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$stroke))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$stroke))~c(0, dim(valid.df)[1]), lty=2)


# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(valid.df$stroke)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

# add labels to columns #
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

export.df <- data.frame(valid.df, logit.reg.pred)
t.df <- data.frame("Predicted" = logit.reg.pred, "Label" = valid.df$stroke)
View(t.df)

write.csv(export.df, file = "LR_model.csv")

pred <- prediction(t.df$Predicted, t.df$Label)
perf <- performance( pred, "tpr", "fpr" )
plot( perf )

## Oversampling the data

data_oversample <- ovun.sample(stroke~. , data = train.df, method = 'over', p = 0.5 )$data
View(data_oversample)

table(data_oversample$stroke)

## Undersampling the data 

data_downsample <- ovun.sample(stroke~. , data = train.df, method = 'under', p = 0.5 )$data
View(data_downsample)

table(data_downsample$stroke)

# Both Under and Over sampling the data 

data_bothsample <- ovun.sample(stroke~. , data = train.df, method = 'both', p = 0.5 )$data
View(data_bothsample)

table(data_bothsample$stroke)

## Running Logistic Regression Model on down sampled training data
logit.reg1 <- glm(stroke ~ ., data = data_downsample, family = "binomial") 
options(scipen=999)
summary(logit.reg1)

# use predict() with type = "response" to compute predicted probabilities. #
logit.reg.pred1 <- predict(logit.reg1, valid.df[,-c(11)], type = "response") 

# first 10 actual and predicted records #
data.frame(actual = valid.df$stroke[1:10], predicted = logit.reg.pred1[1:10])

# Confusion Matrix #

confusionMatrix(table(predict(logit.reg1, newdata = valid.df, type="response") >= 0.5, valid.df$stroke == 1))

## Running Logistic Regression Model on over sampled training data
logit.reg2 <- glm(stroke ~ ., data = data_oversample, family = "binomial") 
options(scipen=999)
summary(logit.reg1)

# use predict() with type = "response" to compute predicted probabilities. #
logit.reg.pred2 <- predict(logit.reg2, valid.df[,-c(11)], type = "response") 

# first 10 actual and predicted records #
data.frame(actual = valid.df$stroke[1:10], predicted = logit.reg.pred2[1:10])

# Confusion Matrix #

confusionMatrix(table(predict(logit.reg2, newdata = valid.df, type="response") >= 0.5, valid.df$stroke == 1))

## Running Logistic Regression Model on both sampled training data
logit.reg3 <- glm(stroke ~ ., data = data_bothsample, family = "binomial") 
options(scipen=999)
summary(logit.reg1)

# use predict() with type = "response" to compute predicted probabilities. #
logit.reg.pred3 <- predict(logit.reg3, valid.df[,-c(11)], type = "response") 

# first 10 actual and predicted records #
data.frame(actual = valid.df$stroke[1:10], predicted = logit.reg.pred3[1:10])

# Confusion Matrix #

confusionMatrix(table(predict(logit.reg3, newdata = valid.df, type="response") >= 0.5, valid.df$stroke == 1))

## Running Random Forest Model ##

rf <- randomForest(as.factor(stroke) ~ ., data = train.df, 
                   ntree = 500, mtry = 5 , nodesize = 1, importance = TRUE) 

# plot the variables by order of importance #
varImpPlot(rf, type = 1)

#create a confusion matrix
valid.df$stroke <- factor(valid.df$stroke)
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, valid.df$stroke)

## Running Random Forest Model on down sampled taining data

rf1 <- randomForest(as.factor(stroke) ~ ., data = data_downsample, 
                    ntree = 500, mtry = 5 , nodesize = 1, importance = TRUE) 

# plot the variables by order of importance #
varImpPlot(rf1, type = 1)

#create a confusion matrix
valid.df$stroke <- factor(valid.df$stroke)
rf.pred1 <- predict(rf1, valid.df)
confusionMatrix(rf.pred1, valid.df$stroke)

## Running Random Forest Model on over sampling taining data

rf2 <- randomForest(as.factor(stroke) ~ ., data = data_oversample, 
                    ntree = 500, mtry = 5 , nodesize = 1, importance = TRUE) 

# plot the variables by order of importance #
varImpPlot(rf2, type = 1)

#create a confusion matrix
valid.df$stroke <- factor(valid.df$stroke)
rf.pred2 <- predict(rf2, valid.df)
confusionMatrix(rf.pred2, valid.df$stroke)

## Running Random Forest Model on both sampled training data

rf3 <- randomForest(as.factor(stroke) ~ ., data = data_bothsample, 
                    ntree = 500, mtry = 5 , nodesize = 1, importance = TRUE) 

# plot the variables by order of importance #
varImpPlot(rf3, type = 1)

#create a confusion matrix
valid.df$stroke <- factor(valid.df$stroke)
rf.pred3 <- predict(rf3, valid.df)
confusionMatrix(rf.pred3, valid.df$stroke)

## Running Decision Tree Model

.ct <- rpart(stroke ~ . , data = train.df, method = "class", cp = 0.001, maxdepth = 12, minsplit = 1)

# plot tree
prp(.ct, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)
printcp(.ct)

# classify records in the validation data using the classification tree.
# set argument type = "class" in predict() to generate predicted class membership.

ct.pred <- predict(.ct, valid.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(ct.pred, as.factor(valid.df$stroke))

## Running Decision Tree Model on down sampled data

.ct1 <- rpart(stroke ~ . , data = data_downsample, method = "class", cp = 0.001, maxdepth = 12, minsplit = 1)

# plot tree
prp(.ct1, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)
printcp(.ct1)

# classify records in the validation data using the classification tree.
# set argument type = "class" in predict() to generate predicted class membership.

ct.pred1 <- predict(.ct1, valid.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(ct.pred1, as.factor(valid.df$stroke))

## Running Decision Tree Model on over sampled data

.ct2 <- rpart(stroke ~ . , data = data_oversample, method = "class", cp = 0.001, maxdepth = 12, minsplit = 1)

# plot tree
prp(.ct2, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)
printcp(.ct2)

# classify records in the validation data using the classification tree.
# set argument type = "class" in predict() to generate predicted class membership.

ct.pred2 <- predict(.ct2, valid.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(ct.pred2, as.factor(valid.df$stroke))


## Running Decision Tree Model on both sampled data

.ct3 <- rpart(stroke ~ . , data = data_bothsample, method = "class", cp = 0.001, maxdepth = 12, minsplit = 1)

# plot tree
prp(.ct3, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)
printcp(.ct3)

# classify records in the validation data using the classification tree.
# set argument type = "class" in predict() to generate predicted class membership.

ct.pred3 <- predict(.ct3, valid.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(ct.pred3, as.factor(valid.df$stroke))




