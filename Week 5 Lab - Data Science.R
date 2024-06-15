# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(DataExplorer)

# Installing DataExplorer packages 
install.packages('DataExplorer') 

#Step 1: PERFORMING EDA

# Load the dataset
df <- read.csv("Churn_Train.csv")

# Display the first few rows of the dataframe
head(df)

# Basic information about the dataset
str(df)

# Summary statistics of numerical columns
summary(df)

# Check for missing values in the dataset
missing_values <- colSums(is.na(df))

# Display missing values
missing_values[missing_values > 0]


#Step 2: HANDLING MISSING VALUES THROUGH SIMPLE VS MICE
#(A) SIMPLE
# Selecting the column which has missing values
df$Total_Charges 

#Replacing the missing values through mean, median, & 0
value_imputed <- data.frame( 
  original = df$Total_Charges, 
  imputed_zero = replace(df$Total_Charges, is.na(df$Total_Charges), 0), 
  imputed_mean = replace(df$Total_Charges, is.na(df$Total_Charges), mean(df$Total_Charges, na.rm = TRUE)), 
  imputed_median = replace(df$Total_Charges, is.na(df$Total_Charges), median(df$Total_Charges, na.rm = TRUE)) 
) 
value_imputed 

#Plotting the Bar Charts to display and visualise the output
h1 <- ggplot(value_imputed, aes(x = original)) + 
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") + 
  ggtitle("Original distribution") + 
  theme_classic() 
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) + 
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") + 
  ggtitle("Zero-imputed distribution") + 
  theme_classic() 
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) + 
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") + 
  ggtitle("Mean-imputed distribution") + 
  theme_classic() 
h4 <- ggplot(value_imputed, aes(x = imputed_median)) + 
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") + 
  ggtitle("Median-imputed distribution") + 
  theme_classic() 

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2) 

#(B) MICE
#Loading mice library 
library(mice) 

#Replacing the missing values through pmm, cart, and lasso methods
mice_imputed <- data.frame( 
  original = df$Total_Charges, 
  imputed_pmm = complete(mice(df, method = "pmm"))$Total_Charges, 
  imputed_cart = complete(mice(df, method = "cart"))$Total_Charges, 
  imputed_lasso = complete(mice(df, method = "lasso.norm"))$Total_Charges) 

mice_imputed

#Plotting the Bar Charts to display and visualise the output
h5 <- ggplot(mice_imputed, aes(x = original)) + 
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") + 
  ggtitle("Original distribution") + 
  theme_classic() 
h6 <- ggplot(mice_imputed, aes(x = imputed_pmm)) + 
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") + 
  ggtitle("Predictive mean matching") + 
  theme_classic() 
h7 <- ggplot(mice_imputed, aes(x = imputed_cart)) + 
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") + 
  ggtitle("Classification and regression trees") + 
  theme_classic() 
h8 <- ggplot(mice_imputed, aes(x = imputed_lasso)) + 
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") + 
  ggtitle("Lasso linear regression") + 
  theme_classic() 

plot_grid(h5, h6, h7, h8, nrow = 2, ncol = 2) 

#Step 3: ENCODING

# Encoding categorical features using one-hot encoding
df_encoded <- df %>% 
  mutate(across(where(is.factor), as.character)) %>%
  mutate(across(where(is.character), ~factor(., levels = unique(.)))) %>%
  mutate(across(where(is.factor), as.numeric))

# Display the first few rows of the encoded dataframe
head(df_encoded)
