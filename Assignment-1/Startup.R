# Simple Linear Regression

# Importing the Dataset Startup
dataset = read.csv("50_Startups.csv")

# Splitting the Startup Dataset into Training and Testing set

library(caTools)   # install.packages("caTools")
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 2/3)
print(split)

training_set = subset(dataset, split== TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting the Simple Linear Regression Model using Training Set

regression = lm(formula = Profit ~ State , data = training_set)
print(regression)

# Predicting the Test Set Results
y_pred = predict(regression, newdata = test_set)

print(y_pred)

# Visualizing the Training Set Results

# install.packages("ggplot2")
library(ggplot2)

ggplot() + 
  geom_point(aes(x= training_set$State, 
                 y = training_set$Profit),
             colour = "green") +
  geom_line(aes(x= training_set$State, 
                y = predict(regression, newdata = training_set)),
            colour = "black") +
  ggtitle("Profit Vs State (Training Set Results)") +
  xlab("State") +
  ylab("Profit")

### Visualizing the Testing Set Results
ggplot() + 
  geom_point(aes(x= test_set$State, 
                 y = test_set$Profit),
             colour = "green") +
  geom_line(aes(x= test_set$State, 
                y = y_pred),
            colour = "black") +
  ggtitle("Profit Vs State (Testing Set Results)") +
  xlab("State") +
  ylab("Profit")

