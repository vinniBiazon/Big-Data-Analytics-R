# Customer Churn Analytics 

# Step 1 - Defining the working directory and Loading the packages

# Defining the working directory
setwd("D:/Documentos/FCD/BigDataRAzure/Cap06")
getwd()


# Loading the packages
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)


# Step 2 - Loading and Cleaning Data

churn <- read.csv('Telco-Customer-Churn.csv')
View(churn)
str(churn)

# Removing all lines with missing values.
sapply(churn, function(x) sum(is.na(x)))
churn <- churn[complete.cases(churn), ]

# Changing "No internet service" to "No" in six columns:
# "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "streamingTV", "streamingMovies".
cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}


# Changing "No phone service" to "No" on “MultipleLines” column
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))


# Grouping "tenure" into five categories:
# “0-12 Month”, “12–24 Month”, “24–48 Month”, “48–60 Month”, “> 60 Month”
min(churn$tenure); max(churn$tenure)

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)


# Changing “SeniorCitizen” column values from 0 or 1 to “No” or “Yes”.
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))



# Removing unnecessary columns for analysis.
churn$customerID <- NULL
churn$tenure <- NULL
View(churn)


# Step 3 - Exploratory data analysis

# Numeric variables correlation
numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Graph for Numeric Variables", method="number")


# Removing Total Charges to avoid overfitting
churn$TotalCharges <- NULL


# Categorical variable bar graphs
p1 <- ggplot(churn, aes(x=gender)) + ggtitle("Gender") + xlab("Sexo") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p2 <- ggplot(churn, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p3 <- ggplot(churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Parceiros") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p4 <- ggplot(churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependentes") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)


p5 <- ggplot(churn, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Telefonia") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p6 <- ggplot(churn, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Múltiplas Linhas") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p7 <- ggplot(churn, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p8 <- ggplot(churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)


p9 <- ggplot(churn, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p10 <- ggplot(churn, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p11 <- ggplot(churn, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p12 <- ggplot(churn, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, ncol=2)


p13 <- ggplot(churn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p14 <- ggplot(churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p15 <- ggplot(churn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p16 <- ggplot(churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p17 <- ggplot(churn, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
grid.arrange(p13, p14, p15, p16, p17, ncol=2)


# All categorical variables will be maintained because
# appear to have a reasonably wide distribution.



# Step 4 - Feature Selection with Random Forest

rfModel <- randomForest(Churn ~., data = training)
pred_rf <- predict(rfModel, testing)
varImpPlot(rfModel, sort=T, n.var = 10, main = 'Top 10 Feature Importance')


# Step 5 - Predictive Modeling: Logistic Regression


# Dividing data into training and test - 70:30 ratio
intrain <- createDataPartition(churn$Churn,p=0.7,list=FALSE)
training <- churn[intrain,]
testing <- churn[-intrain,]

# Logistic regression model fitting
LogModel <- glm(Churn ~ MonthlyCharges+tenure_group+Contract+PaymentMethod+InternetService, family=binomial(link="logit"), data=training)
print(summary(LogModel))

# Accuracy
testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))


# Logistic Regression Confusion Matrix
print("Logistic Regression - Confusion Matrix"); table(testing$Churn, fitted.results > 0.5)


# Odds Ratio

exp(cbind(OR=coef(LogModel), confint(LogModel)))

# For each unit increase in the Monthly Charge,
# there is a 2.5% reduction in the probability of the customer canceling the subscription.


# Step 6 - Predictive Modeling: Decision Tree

tree <- ctree(Churn ~ MonthlyCharges+tenure_group+Contract+PaymentMethod+InternetService, training)
plot(tree, type='simple')

# The Contract is the most important variable for predicting customer churn.
# If a customer is on a monthly contract,
# and in the 0 to 12 month tenure group, and using PaperlessBilling,
# he is more likely to cancel the subscription.


# Decision Tree Confusion Matrix
pred_tree <- predict(tree, testing)
print("Decision Tree Confusion Matrix"); table(Predicted = pred_tree, Actual = testing$Churn)


# Accuracy
p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))


# Step 7 - Predictive Modeling: Random Forest

rfModel <- randomForest(Churn ~ MonthlyCharges+tenure_group+Contract+PaymentMethod+InternetService, data = training)
print(rfModel)
plot(rfModel)

# The prediction is very good when predicting "No", but the error rate is much higher when predicting "yes".

# Predicting values with test data
pred_rf <- predict(rfModel, testing)

# Confusion Matrix
print("Random Forest - Confusion Matrix"); table(testing$Churn, pred_rf)
