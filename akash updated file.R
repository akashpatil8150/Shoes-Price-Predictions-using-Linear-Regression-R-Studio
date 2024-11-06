#Essential Library Required for Analysis
install.packages("ggplot2")
install.packages("dplyr")
install.packages("caret")
install.packages("reshape2")
library(ggplot2)
library(reshape2)
library(caret)    # For splitting data
library(dplyr)

#Data Collection


df=read.csv("C:/Users/adars/OneDrive/Desktop/shoes/Shoes.csv")
head(df)
dim(df)
str(df)
summary(df)
colSums(is.na(df))

#EDA Analysis

num_cols <- df %>% select(Price, Size, Rating)
cor(num_cols, use = "complete.obs")
aggregate(Price ~ Brand, data = df, FUN = mean)
aggregate(Price ~ Type, data = df, FUN = mean)

# Histogram of Price

hist(df$Price, main = "Price Distribution", xlab = "Price", col = "lightblue")

# Density plot of Rating

plot(density(df$Rating, na.rm = TRUE), main = "Rating Density", xlab = "Rating")

# Bar plot for Brand distribution

barplot(table(df$Brand), main = "Brand Distribution", col = "lightgreen")

# Boxplot of Price by Brand

boxplot(Price ~ Brand, data = df, main = "Price by Brand", xlab = "Brand", ylab = "Price", col = "orange")


#Feature Eng

df_cleaned=df %>% select(-Shoe_no, -Model, -Release_Date)

df_cleaned$Brand=as.factor(df_cleaned$Brand)

df_cleaned$Type=as.factor(df_cleaned$Type)

df_cleaned$Color=as.factor(df_cleaned$Color)

df_cleaned$Gender=as.factor(df_cleaned$Gender)

df_cleaned$Material=as.factor(df_cleaned$Material)

df_cleaned$In_Stock=as.factor(df_cleaned$In_Stock)


# Split data into Train test with ratio  80% for train and  20% for test

set.seed(42)

trainIndex <- createDataPartition(df_cleaned$Price, p = 0.8, list = FALSE)

trainData <- df_cleaned[trainIndex, ]

testData <- df_cleaned[-trainIndex, ]

# Building Linear Regression Model with train data

model=lm(Price ~ ., data = trainData)

# Model testing with test data 

predictions=predict(model, newdata = testData)

# Finding error in model

output=data.frame(Prices=testData$Price, Predicted=predictions)
head(output)



mse=mean((testData$Price - predictions)^2)
rm=sqrt(mean((testData$Price - predictions)^2))

print(paste("Mean Squared Error (MSE):", mse))

print(paste("Root Mean Sqaured Error :", rm))

# Assuming you already calculated MSE and RMSE
mse <- mean((testData$Price - predictions)^2)
rmse <- sqrt(mse)

# Plotting the graph
errors <- c(mse, rmse)
labels <- c("MSE", "RMSE")

# Plot the MSE and RMSE
barplot(errors, names.arg = labels, col = c("blue", "red"),
        main = "MSE and RMSE for the Model", ylab = "Error",
        ylim = c(0, max(errors) * 1.2))

# Adding value labels on top of bars
text(x = c(1, 2), y = errors, label = round(errors, 2), pos = 3, cex = 1.2)


# Function to get user input and predict price
predict_price <- function() {
  # Get user inputs
  Brand <- as.factor(readline(prompt = "Enter Brand: "))
  Type <- as.factor(readline(prompt = "Enter Type: "))
  Color <- as.factor(readline(prompt = "Enter Color: "))
  Gender <- as.factor(readline(prompt = "Enter Gender: "))
  Material <- as.factor(readline(prompt = "Enter Material: "))
  In_Stock <- as.factor(readline(prompt = "Is it In Stock? (Yes/No): "))
  Size <- as.numeric(readline(prompt = "Enter Size: "))
  Rating <- as.numeric(readline(prompt = "Enter Rating: "))
  
  # Create a new data frame with the user input
  user_data <- data.frame(Brand, Type, Color, Gender, Material, In_Stock, Size, Rating)
  
  # Ensure the factors are in the same order as the training data
  user_data$Brand <- factor(user_data$Brand, levels = levels(trainData$Brand))
  user_data$Type <- factor(user_data$Type, levels = levels(trainData$Type))
  user_data$Color <- factor(user_data$Color, levels = levels(trainData$Color))
  user_data$Gender <- factor(user_data$Gender, levels = levels(trainData$Gender))
  user_data$Material <- factor(user_data$Material, levels = levels(trainData$Material))
  user_data$In_Stock <- factor(user_data$In_Stock, levels = levels(trainData$In_Stock))
  
  # Make prediction
  predicted_price <- predict(model, newdata = user_data)
  
  # Show the predicted price
  print(paste("The predicted price is:", round(predicted_price, 2)))
}

# Call the function to predict price based on user input
predict_price()







