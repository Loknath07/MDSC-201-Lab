# Importing the Dataset Startup
dataset = read.csv("~/201-lab/2nd assignment/The Rocket propellant Data.csv")

# Splitting the Startup Dataset into Training and Testing set

library(caTools)    # install.packages("caTools")
set.seed(123)
split = sample.split(dataset$Shear.strength,SplitRatio = 2/3)
print(split)

training_set = subset(dataset,split==TRUE)
test_set = subset(dataset,split==FALSE)

# Fitting the Simple Linear Regression Model using Training Set

regressor = lm(formula=Shear.strength ~ Age.of.propellant , data=training_set)
print(regressor) 

# Predicting the Test Set Results
y_pred = predict(regressor, newdata = test_set) 
print(y_pred)    # printing Predicted result

#Training set results plot
y_train_pred = predict(regressor, newdata = training_set)

# Visualizing the Training Set Results

library(ggplot2)    # install.packages("ggplot2")
ggplot() +
  geom_point(aes(x=training_set$Age.of.propellant,
                 y = training_set$Shear.strength),
             color = "red")+
  geom_line(aes(x=training_set$Age.of.propellant,
                y = predict(regressor,newdata=training_set)),
            color="blue")+
  ggtitle("Shear.strength v/s Age.of.propellant")+
  xlab("Age.of.propellant")+
  ylab("Shear.strength")


#Testing set results plot
ggplot() + 
  geom_point(aes(x= test_set$Age.of.propellant, 
                 y = test_set$Shear.strength),
             colour = "red") +
  geom_line(aes(x= test_set$Age.of.propellant, 
                y = y_pred),
            colour = "blue") +
  ggtitle("Shear.strength Vs Age.of.propellant (Testing Set Results)") +
  xlab("Age.of.propellant") +
  ylab("Shear.strength")


#Calculation of Residuals
train_residual = y_train_pred - training_set$Shear.strength
test_residual = y_pred - test_set$Shear.strength

#Plotting Residual line
ggplot() +
  geom_point(aes(x = 1:length(train_residual),
                 y = train_residual),
             colour = "black") + 
  geom_hline(yintercept = 0,
             linetype = "solid",
             color = "green")

ggplot() +
  geom_point(aes(x = 1:length(test_residual),
                 y = test_residual),
             colour = "black") + 
  geom_hline(yintercept = 0,
             linetype = "solid",
             color = "green")

