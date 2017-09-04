#Getting the data from working directory
loans <- read.csv('loan_data.csv')
#print the structure to see the structure of the data
print(str(loans))
#printing the summary of the dataset
print(summary(loans))
#After looking at the stucture of the dataset we can factorize the below columns 
#inq.last.6mths: delinq.2yrs: pub.rec: not.fully.paid : credit.policy

loans$credit.policy <- factor(loans$credit.policy)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)

print(str(loans))

#EXPLORATORY DATA ANALYSIS
library(ggplot2)
#Plotting histogram of fico score with colored as not.fully.paid
pl1 <- ggplot(loans, aes(fico)) + geom_histogram(aes(fill=factor(not.fully.paid)), color='black', bins=40, alpha=0.5)+theme_dark()
print(pl1)

# Barplot of purpose counts colored by not fully paid
pl2 <- ggplot(loans, aes(x=factor(purpose)))+ geom_bar(aes(fill=factor(not.fully.paid)), position='dodge',color='black', bins=40, alpha=0.5)+theme_gray()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(pl2)

#scatterplot of fico score vs int.rate

pl3 <- ggplot(loans, aes(x=int.rate, y=fico))+geom_point(aes(color=not.fully.paid), bins=40, alpha=0.2)+theme_light()
print(pl3)

#BUILDING THE MODEL
library(caTools)
spl <- sample.split(loans$not.fully.paid,0.7)
train <- subset(loans, spl==TRUE)
test <- subset(loans, spl==FALSE)

#call the library e1071
library(e1071)
model <- svm(not.fully.paid~. , data=train)
#printing the sumamry of the model
print(summary(model))

#predicting new values and printing the confusion matrix
predicted.values <- predict(model, test[1:13])
print(table(predicted.values,test$not.fully.paid))

#predicted.values    0    1
#                 0 2413  460
#                 1    0    0
#As entire predictions shown in one goup itself which is a bad result, lets tune the data
#We are using wrong cost and wrong parameter values
#Using the tune() function to test out different cost and gamma values

tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial', ranges=list(cost=c(100,200), gamma=0.1))
print(summary(tune.results))
#Now in console, we can see that best parameters are: cost & gamma: 100 &  0.1

#Now using those parameters and predicting the values.
model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)
predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)                   

#predicted.values    0    1
                 #0 2350  425
                 #1   63   35