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
concrete_model <- neuralnet(Strength ~., data = concrete_train)
concrete_model1<- update(concrete_model, hidden = 5)
plot(concrete_model)

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

drop_na(raw_data)
raw1 <- as.data.frame(raw_data)%>%
  drop_na()

glimpse(raw1)

mnist_train <- raw1 %>% sample_frac(.7)
mnist_test <- anti_join(raw1, mnist_train)
glimpse(mnist_train)
glimpse(mnist_test)





