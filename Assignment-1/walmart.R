# Simple Linear Regression

# Importing the Dataset Walmart
dataset = read.csv("Walmart.csv")

# Splitting the walmart Dataset into Training and Testing set

library(caTools)   # install.packages("caTools")
set.seed(123)
split = sample.split(dataset$Weekly_Sales, SplitRatio = 2/3)
print(split)

training_set = subset(dataset, split== TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting the Simple Linear Regression Model using Training Set

regression = lm(formula = Weekly_Sales ~ CPI , data = training_set)
print(regression)

# Predicting the Test Set Results
y_pred = predict(regression, newdata = test_set)

print(y_pred)

# Visualizing the Training Set Results

# install.packages("ggplot2")
library(ggplot2)

ggplot() + 
  geom_point(aes(x= training_set$CPI, 
                 y = training_set$Weekly_Sales),
             colour = "red") +
  geom_line(aes(x= training_set$CPI, 
                y = predict(regression, newdata = training_set)),
            colour = "blue") +
  ggtitle("Weekly_Sales Vs CPI (Training Set Results)") +
  xlab("CPI") +
  ylab("Weekly_Sales")

### Visualizing the Testing Set Results
ggplot() + 
  geom_point(aes(x= test_set$CPI, 
                 y = test_set$Weekly_Sales),
             colour = "red") +
  geom_line(aes(x= test_set$CPI, 
                y = y_pred),
            colour = "blue") +
  ggtitle("Weekly_Sales Vs CPI (Testing Set Results)") +
  xlab("CPI") +
  ylab("Weekly_Sales")
