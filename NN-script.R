library(dplyr)
library(ggplot2)
library(tidyverse)


concrete <- read.csv("Concrete.csv")
glimpse(concrete)


ggplot(data = concrete, aes(x = concrete$Strength)) +
  geom_histogram(aes(y =..density..), binwidth = 4, fill="#DD8888", color = "black")+
  geom_density(alpha = .4, fill = "black")+
  labs(x = "Concrete Strength")+
  ggtitle("Quick Overview of the Raw Output Data") +
  theme(plot.title = element_text(hjust = 0.5))
  
# The ouput data (concrete strength) Looks fairly normal.

# Scaling
# Create a normalization function.
normalize <- function(x){
  y = ((x - min(x))/(max(x)-min(x)))
  return(y)
}

concrete_norm <- as.data.frame(lapply(concrete, normalize)) #Normalizing a dataframe
summary(concrete_norm$Strength)
summary(concrete$Strength)

# partition the data to .75: .25

#-------------------------------------------------------------------------#
#If you do not have an ID per row, use the following code to create an ID
#df <- df %>% mutate(id = row_number()
#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#
#                   approach (dplyr)
#concrete_train <- concrete_norm %>% sample_frac(.75)
#concrete_test <- anti_join(concrete_norm, concrete_train)
#-------------------------------------------------------------------------#

#install.packages("RSNNS")
#install.packages("neuralnet")
library(neuralnet)

set.seed(7)
sampling <- sample(1030, 772)
concrete_train <- concrete_norm[sampling,]
concrete_test <- concrete_norm[-sampling,]

#custom activation function
#soft <- function(x) log(1 + exp(-x))

set.seed(7)
concrete_model <- neuralnet(Strength ~., data = concrete_train, act.fct = "logistic")
concrete_model1<- update(concrete_model, hidden = 5)
plot(concrete_model1)

# model evaluation
model_results <- compute(concrete_model1, concrete_test[-1])
pred_strength <- model_results$net.result

# correlation is used instead of confusion matrix - Not a classification problem

cor(pred_strength,concrete_test$Strength)

ak <- if_else(pred_strength>0.5, "good", "bad") #creating a threshold for a continous dataset

?neuralnet




#----------------------------------------------------#
            #project time#
#---------------------------------------------------#


read.libsvm = function( filename, dimensionality ) {
  
  content = readLines(filename )
  num_lines = length( content )
  yx = matrix( 0, num_lines, dimensionality + 1 )
  
  # loop over lines
  for ( i in 1:num_lines ) {
    
    # split by spaces
    line = as.vector( strsplit( content[i], ' ' )[[1]])
    
    # save label
    yx[i,1] = as.numeric( line[[1]] )
    
    # loop over values
    for ( j in 2:length( line )) {
      
      # split by colon
      index_value = strsplit( line[j], ':' )[[1]]
      
      index = as.numeric( index_value[1] ) + 1		# +1 because label goes first
      value = as.numeric( index_value[2] )
      
      yx[i, index] = value
    }
  }
  
  return(yx)
}


raw_data <- read.libsvm("mnist.scale", 780)
dim(raw_data)
glimpse(raw_data)
summary(raw_data)


raw1 <- as.data.frame(raw_data)%>%
  drop_na()

glimpse(raw1)
dim(raw1)

raw1$V1 %>% head(20) # 10 classes: 0 - 9

#write.csv(raw1, "C:\\Github\\mnist.csv", row.names = FALSE)

mnist <- read.csv("mnist.csv", header = T)

# convert the "label" column to a factor
mnist$Label <- factor(mnist$Label)

# Split the dataset by a ratio of 70:30
train <- mnist %>% sample_frac(.7)
test <- anti_join(mnist, train)

dim(train)


# Binary classification using neural network of 100 hidden layers

# custom activation function - Softplus function: approximation of the RELU, as shown below


softplus <- function(x)log(1+exp(x))
relu <- function(x) sapply(x, function(z) max(0,z))

x <- seq(from=-10, to=10, by=0.2)
library(ggplot2)
library(reshape2)

fits <- data.frame(x=x, softplus = softplus(x), relu = relu(x))
#glimpse(fits)
long <- melt(fits, id.vars="x") 
#glimpse(long)
ggplot(data=long, aes(x=x, y=value, group=variable, colour=variable))+
  geom_line(size=1) +
  ggtitle("ReLU & Softplus") +
  theme(plot.title = element_text(size = 26, hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 18)) #Almost same as ReLU


# The internals of the neuralnet package will try to differentiate any function provided to act.fct.
# The source code block is shown below

# if (is.function(act.fct)) {
#act.deriv.fct <- differentiate(act.fct)
#attr(act.fct, "type") <- "function" }

# The differentiate function is a more complex use of the deriv function which you can also see in the source code above.
# Therefore, it is currently not possible to provide max(0,x) to the act.fct.


#----------------------------------------------------#
            #binary classification#
#---------------------------------------------------#

#write.csv(raw_data, "C:\\Github\\Machine_Learning\\Mnist.csv", row.names = FALSE)




#Class variables function
Classes <- function(data){
  Class_variables <- sapply(data, function(x) class(x)) 
  return(Class_variables)
}
Classes(test)
?neuralnet

train_v <- sample(nrow(train), 1/3 * nrow(train))

train_v1 <- train[train_v, ]

set.seed(7)
nn_mnist <- neuralnet(Label == "9" ~ ., train_v1, linear.output = FALSE, hidden = 100, act.fct = "logistic")

plot(model_bin)


# Binary classification model evaluation

levels(test$Label)

sum(test$Label == "9")

pred_bin <- predict(nn_mnist, test)

summary(pred_bin)

table(test$Label == "9", pred_bin[, 1] > 0.000000000002)


# Multi-class classification

nn_mnist1 <- neuralnet((Label == "0") + (Label == "1") + (Label == "2") + (Label == "3")+(Label == "4") + 
                         (Label == "4") + (Label == "5") + (Label == "6") + (Label == "7") + (Label == "8") +
                         (Label == "9")~ ., train_v1, linear.output = FALSE, hidden = 100, act.fct = "logistic")

pred_bin1 <- predict(nn_mnist1, test)

table(test$Label, apply(pred_bin1, 1, which.max))


# Decision Tree

#Decision Tree Algo using the C5.0 algorithm by J. Ross Quinlanb (Industry Standard) - Divide and Conquer


set.seed(7)

library(C50)
mnist_dt <- C5.0(train[-1], train$Label)
mnist_dt 
#summary(iris_model) #Training error is 2.5%

#Evaluate Model Performance

mnist_pred <- predict(mnist_dt, test
library(gmodels)
CrossTable(iris_test$Species, iris_pred, prop.r = FALSE,
           prop.c = FALSE, prop.chisq = FALSE,
           dnn = c("predicted", "actual"))

(mean(iris_pred == iris_test$Species))*100 #Classification Accuracy is approx. 97%


#Evaluate Model Performance - Using a subset of the training data





















train_idx <- sample(nrow(iris), 2/3 * nrow(iris))
iris_train <- iris[train_idx, ]
iris_test <- iris[-train_idx, ]

# Binary classification
nn <- neuralnet(Species == "setosa" ~ ., iris_train, linear.output = FALSE)
pred <- predict(nn, iris_test)
table(iris_test$Species == "setosa", pred[, 1] > 0.5)

summary(pred)

# Multiclass classification
nn <- neuralnet((Species == "setosa") + (Species == "versicolor") + (Species == "virginica")~ Petal.Length + Petal.Width, iris_train, linear.output = FALSE)
pred <- predict(nn, iris_test)
table(iris_test$Species, apply(pred, 1, which.max))









