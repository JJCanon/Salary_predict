# in this script we will do what we did in excel and we will add machine learning 

# Regression Model Project

# Excel to R part

# Load the data
library(readxl)
model_data <- read_excel("C:/Users/jjgca/Documents/Data-Science/Introduccion a la ciencia de datos/Modulo 5/Salary_predict/R/Datos_Modelo.xlsx",sheet = "Datos")

# Model 1: simple linear regression Salary ~ Experience
model_1 <- lm(Salario ~ Experiencia, data = model_data)
# Display model information
summary(model_1)

# Model 2: Multiple linear regression Salary ~ Experience + Education_Years
model_2 <- lm(Salario ~ Experiencia + Años_Educación, data = model_data)
# Display model information
summary(model_2)

# Model 3: Multiple linear regression Salary ~ Experience + Education_Years + Company_Size
model_3 <- lm(Salario ~ Experiencia + Años_Educación + Tamaño_Empresa, data = model_data)
# Display model information
summary(model_3)


# Machine Learning Model

# we need to separate the data in train and test part
# train many models at same time
# evaluate this models with test data
# conclude which one is better in this chance
# by default we will use caret

# Install the packages ( if we haven't installed)
install.packages("tidyverse")
install.packages("caret")


# maybe the program ask you to install random forest, press yes

# Load packages
library(caret)

# Save the data
data <- model_data

# we transform category variable to factor
summary(data)
data$Tamaño_Empresa <- as.factor(data$Tamaño_Empresa)

# we separate the data
set.seed(123) 
# select train indexes
trainIndex <- createDataPartition(data$Salario, p=0.8, list = FALSE)
# prepare the train data
trainData <- data[trainIndex, ]
# prepare the test data
testData <- data[-trainIndex, ]

# train the models whit their own method
# linear regression model
lr_model <- train(Salario ~ ., data = trainData, method = "lm")
# tree decision model
td_model <- train(Salario ~ ., data = trainData, method = "rpart")
# random forest model
rf_model <- train(Salario ~ ., data = trainData, method = "rf")
# K-Nearest Neighbors
KNN_model <- train(Salario ~ .,data = trainData, method = "knn")

# we save all models trained in a list
models <- list(
  "Linear Regression" = lr_model,
  "Tree Decision" = td_model,
  "Random Forest" = rf_model,
  "KNN" = KNN_model
)

# create a data.frame empty to save the results
results <- data.frame(Model = character(), R2 = double(), MAE = double(), RMSE = double())


# evaluate models trained with test data
for (name in names(models)) {
  
  #get the model from the list
  model <- models[[name]]
  # test the model
  prediction <- predict(model,newdata = testData)
  # get results
  r2 <- R2(prediction,testData$Salario)
  mae <- MAE(prediction,testData$Salario)
  rmse <- RMSE(prediction,testData$Salario)
  
  # save the results in the list
  results <- rbind(results,data.frame(Model = name, R2 = r2, MAE = mae, RSME = rmse))
  
}

# display the results
print(results)