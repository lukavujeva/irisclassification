library(caret)
######################################
########## Data Cleaning #############
######################################

#first we load the Iris.csv dataset
df <- read.csv("Iris.csv")

#Here we will split the data set into the training set and the test set
#We will use the first 80% of the data to use as a training set, the rest will be used as a test set
train <- createDataPartition(df$Species, p=.8, list=F)
test <- df[-train,]

######################################
######### Data Exploration ###########
######################################

#Here we will split the data set into the training set and the test set
#We will use the first 80% of the data to use as a training set, the rest will be used as a test set
train <- createDataPartition(df$Species, p=.8, list=F)
test <- df[-train,]
#next check the dimensions of the dataset
dim(df)

#next we go through the types of attributes
sapply(df, class)
head(df)
levels(df$Species)

percentagetable <- prop.table(table(df$Species))*100
percentagetable
cbind(freq=table(df$Species), percentage=percentagetable)

#here we take a summary of the data
summary(df)

######################################
############ Plotting ################
######################################

#First we split the input/output into x and y values
x <- df[,1:4]
y <- df[,5]

par(mfrow=c(1,4))

#next we generate a boxplot to illustrate the data
for(i in 1:4){
  boxplot(x[,i], main=names(iris)[i])
}


######################################
############ Modeling ################
######################################

#Here we will be trying a variety of machine learning algorithms, namely cross validation, linear discriminant analysis, and regression trees

#we first try cross validation
control <- trainControl(method="cv", number=10)
metric = "Accuracy"

#Accuracy is the number of correctly predicted instances in the dataset given as a percentage

#Next we try Linear Discriminant Analysis (LDA)
set.seed(7)
fit.lda <- train(Species~., data=df, method="lda", metric=metric, trControl=control)

#Next we can try the K Nearest Neighbours Method
set.seed(7)
fit.knn <- train(Species~., data=df, method="knn", metric=metric, trControl=control)

# Finally we try Classification and Regression Trees
set.seed(7)
fit.cart <- train(Species~., data=df, method="rpart", metric=metric, trControl=control)

# Now we can put the accuracy of models in a table and determine which has the highest accuracy
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn))
summary(results)

#next we plot the results in a dot plot
dotplot(results)
print(fit.lda)

#given the information in the table, we can clearly see that the Linear Discriminant Analysis was the most accurate, therefore we will proceed with it

#finally we test LDA on validation set to determine our conclusions
predictions <- predict(fit.lda, test)
confusionMatrix(predictions, test$Species)