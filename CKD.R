# Load Data ---------------------------------------------------------------
library(readxl)
library(caret)
library(corrplot)
data <- read_excel("Chronic Kidney Disease Dataset.xls", sheet = "All Data")
attach(ckd_Data)

# Transforming variables --------------------------------------------------
#racegroup <- data.frame(Racegrp = c('white','hispa','black','other'))
dummy <- dummyVars(" ~ .", data = data)
data<- data.frame(predict(dummy, newdata = data))



# Data Sampling -----------------------------------------------------------
train <- data[1:6000, ]
test <- data[6001:8819, ]


# Check for missing values ------------------------------------------------

sapply(data,function(x) sum(is.na(x)))
contrasts(data$Female)


# Linear model ------------------------------------------------------------

lm_mod <- lm(CVD ~., data = data)
summary(lm_mod)
View(ckd_Data)



# Logistic regression -----------------------------------------------------

log_demographic <- glm(CKD ~ ., family= binomial(),data = train)
summary(log_demographic)

logistic_model <- glm(formula = CKD ~ .,family = binomial(link='logit'),data=train)
summary(logistic_model)

#Prediction Trainset
pred<-predict(logistic_model, test, type = 'response')


#Misclassification error training
pred1<-ifelse(pred>0.5,1,0)
misclass <- mean(pred1 != test$CKD)
print(paste('Accuracy', 1-misclass))

#Confustion Matrix
confusion_all<-table(Predicted=pred1, Actual=test$CKD)
confusion_all
n = sum(confusion_all) # number of instances
diag = diag(confusion_all) # number of correctly classified instances per class 
rowsums = apply(confusion_all, 2, sum) # number of instances per class
colsums = apply(confusion_all, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy


