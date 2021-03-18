#Exploring bank data - Evaluate buyers for PEP - Targeted Email Marketing

bank <- read.csv("K:/Spring2020/IDS 572 - Data Mining/bank-data.csv")
str(bank)
dim(bank)

##600 observations for 12 variables in the data

bank$id <- NULL
head(bank)


bank_pep <- bank[bank$pep == "YES",]
average_income <- mean(bank_pep$income)
#The average income 30644.92

#Lets us consider the variables car and pep for our analysis. Given that both are factor variables.

table(bank$car)
table(bank$pep)
tab <- table(bank$car,bank$pep )
ptab<-prop.table(tab)
ptab
#About 28%  did not buy car or pep
#about 22%  bought PEP and no car
#About 26% bought car and not PEP
#About 23% bought both PEP and car

barplot(ptab, main = "Car v/s Pep", col = c("blue","green"), 
        xlab = "Car and Pep - Yes and No ")
addmargins(round(ptab,2),c(1,2))
legend("topright", c("Cars", "PEP"), lty=1,lwd=4, col=c("blue", "green"), cex=0.7)

##h0: There is no relationship between cars and personal equity plan
##hA: There is a relationship between cars and personal equity plan
# Statistical test of chi square for 2 categorical variables

cp.sq <- chisq.test(bank$car, bank$pep)
cp.sq

## Fail to reject the null hypothesis since theb p -value is greater than alpha value 0.05 (95% confidence) 
## We can establish that there is no relationship between car and pep buying.


bankp_yes <- bank[bank$pep == "YES",]
bankp_no <- bank[bank$pep == "NO", ]

library(dplyr)
bank %>% group_by(pep) %>% summarise(avg = mean(children), median = median(children), sd = sd(children))

#pep     avg     median    sd
#1 NO    1.07       0     1.20 
#2 YES   0.945      1     0.861

#Average number of children for PEP - yes is 0 and for PEP - no is 1 approximately

boxplot(bank$children~bank$pep, main = "Boxplot for Children and PEP", 
        col=c("coral", "lightblue"), xlab = "PEP", ylab = "Number of Children")
#The boxplot describes the number of children for PEP - NO is higher than PEP - YES
#There is one outlier with number of children with PEP - YES

sum(is.na(bank))

# There are no missing values in the data set

str(bank)
#Collecting all numeric data into one data frme
num_cols <- unlist(lapply(bank, is.integer))  
bank_num <- bank[, num_cols]

head(bank_num)

child_out= boxplot(bank_num$children)$out
age_out= boxplot(bank_num$age)$out
income_out = boxplot(bank_num$income)$out

## There are no outliers in the numeric columns of the bank data

set.seed(1234)
indx <- sample(2, nrow(bank), replace = T, prob = c(0.67, 0.33))
train <- bank[indx == 1, ]
test <- bank[indx == 2, ]

library(rpart)
bank_dt <- rpart(pep~., data = train, parms = list(split = "gini"))

summary(bank_dt)

library(rpart.plot)
rpart.plot(bank_dt, uniform = T, main = "Decision tree for bank data")

#From the basic decision tree, we see that 14 leaf values
#Let us now prune the decision tree using the cost parameter

opt <- which.min(bank_dt$cptable[ ,"xerror"])
cp <- bank_dt$cptable[opt, "CP"]
cp

bankdt_prune <- rpart(train$pep~., data = train, control = rpart.control(minsplit= 10, cp = 0.01), parms = list(split = "gini"))

#We are choosing a cost parameter of 0.01 based on the cp table of the first decision tree
#We are taking a value of 10 for minimun split criterion
#The pruned tree is seen as below

print(bankdt_prune)
summary(bankdt_prune)
rpart.plot(bankdt_prune)

table(predict(bankdt_prune, type = "class"), train$pep, dnn = c("Predicted", "Actual"))

#            Actual
#Predicted  NO YES
#NO        203  22
#YES       18 159

library(rattle)
library(RColorBrewer)
fancyRpartPlot(bankdt_prune, caption = NULL)

Prediction = predict(bankdt_prune,test, type = "class")
mean(test$pep == Prediction)

#The model has an accuracy of 85.35 percent

#Variable importance from the decision tree is :
#children      income         age    mortgage    save_act     married      region         sex 
#33          24          12          11          10           6           2           2 

#current_act 
#1 

#For node 1, the primary split is married status, with the variable improving accuracy of the dt by 9.01%

#Q10
library(caret)
##we seperate the target variable and rest of the variables
x = train[,-11]
y = train$pep


bank_nb = train(x, y, 'nb')
bank_nb

##Using the library caret, the naive bayes model has be created.

x_test <- test[, -11]
y_test <- test$pep

bank_nb10 = train(x, y, 'nb', trControl = trainControl(method='cv', number=10))
bank_nb10

pred <- predict(bank_nb10$finalModel,x_test)
tab <- table(pred$class,y_test, dnn = )
tab
conf <- confusionMatrix(tab)
# The accuracy of naive bayes model is 71.72%

library(plyr)
install.packages("C50")
library(C50)
set.seed(123)
form <- "pep ~ ."
folds <- split(train, cut(sample(1:nrow(train)),10))

for (i in 1:length(folds)) {
  model <- C5.0(as.formula(form) , train )
  pred <- predict(model, newdata = test)
  acc_mat <- table(test$pep, tmp.predict)
  accuracy <- sum(diag(acc_mat))/sum(acc_mat)
}
accuracy
#The accuracy of the decision tree is 91.27%

#Q12
pred <- predict(model, newdata = test)
table(pred) 
#from the result of the decision tree, we see that if reach out to 65 people out of 172, the is positive usage
#so out of 300 , 114 people should be targeted to purchase PEP

#out of 172, 107 predicted no purchase, therefore out of 300, we expect that 186 would not purchase PEP

prediction = predict(bankdt_prune,test, type = "class")

computeTF <- function(prediction, test) {
  result <- c()  
  confusion_matrix <- table(prediction,test$pep)
  row.names(confusion_matrix) <- c("No","Yes")
  confusion_matrix 
  
  sen <- confusion_matrix[4]/(confusion_matrix[4] + confusion_matrix[3])
  result[1] <- sen
  spe <- confusion_matrix[1]/(confusion_matrix[1] + confusion_matrix[2])
  
  fpr <- 1 - spe
  result[2] <- fpr
  
  accuracy <- (confusion_matrix[1] + confusion_matrix[4])/sum(confusion_matrix)
  
  return (result)
}

computeTF(prediction, test)


##to perform knn, we first normalize the numeric values
num_cols <- unlist(lapply(bank, is.numeric))
bank_num <- bank[ ,num_cols]

mins <- apply(bank_num, 2, min)
maxs <- apply(bank_num, 2, max)

bank_scaled <- scale(bank_num, center=mins, scale=maxs-mins)
summary(bank_scaled)

#We next have to have convert our categorical variables to numeric by dummy coding
cat <- !num_cols
cat[11] <- FALSE
cat <- as.logical(cat)

library(psych)
cat_bank <- as.data.frame(sapply(bank[ , cat], dummy.code))
cat_bank

#combine both data
bank_norm <- data.frame(bank_scaled, cat_bank, bank$pep)
summary(bank_norm)


##Split the data into train and test
set.seed(1234)
indx <- sample(2, nrow(bank_norm), replace=T, prob = c(0.8,0.2))
train <- bank_norm[indx==1, -20]
test <- bank_norm[indx==2, -20]

ncol(train) #19

trainLabels <- bank_norm[indx==1, 20]
testLabels <- bank_norm[indx==2, 20]

library(FNN)

bank_knn <- knn(train=train, test=test, cl=trainLabels, k=3)
bank_knn #predictions on the test data

##Applying CV to choose best value of k

set.seed(123)
indx <- sample(2, nrow(train), replace=TRUE, prob=c(0.7,0.3))
trainD <- train[indx==1, ]
validD <- train[indx==2, ]

tLabels <- trainLabels[indx==1]
vLabels <- trainLabels[indx==2]

ksize <- c()
acc <- c()
for(i in 1:15)
{
  preds <- knn(train=trainD, test=validD, cl=tLabels, k=i)
  ksize <- c(ksize,i)
  conf <- table(vLabels, preds)
  acc <- c(acc, sum(diag(conf)) / sum(conf))
}

result <- data.frame(ksize, acc)
plot(result)

#Best value of k = 7, 9, 13 approximately.

#The decision tree performs the best with the accuracy of 91% in 10 cv and 0.9 mean squared error with the pruned tree 
#which takes into account all the variables as compared to knn or naive bayes, it performs best with categorical and numeric data with 2 types of cross validation

