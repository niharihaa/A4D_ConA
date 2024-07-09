# Load necessary packages
if(!require("support.CEs")) install.packages("support.CEs", dependencies=TRUE)
if(!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
if(!require("readr")) install.packages("readr", dependencies=TRUE)
if(!require("dplyr")) install.packages("dplyr", dependencies=TRUE)

library(support.CEs)
library(ggplot2)
library(readr)
library(dplyr)

# Load the data
file_path <- "C:/Users/nihar/OneDrive/Desktop/Bootcamp/SCMA 632/DataSet/pizza_data.csv"
pizza_data <- read_csv(file_path)

# Convert columns to factors
pizza_data <- pizza_data %>%
  mutate(across(c(brand, price, weight, crust, cheese, size, toppings, spicy), as.factor))

# Create profiles
profiles <- pizza_data %>% select(-ranking)

# Create preferences dataset
preferences <- data.frame(
  Respondent = rep(1:nrow(pizza_data), each = nrow(profiles)),
  Profile = rep(1:nrow(profiles), times = nrow(pizza_data)),
  Rating = rep(pizza_data$ranking, each = nrow(profiles))
)

# Merge profiles with preferences
conjoint_data <- merge(preferences, profiles, by.x = "Profile", by.y = "row.names")

# Perform conjoint analysis using lm
conjoint_model <- lm(Rating ~ brand + price + weight + crust + cheese + size + toppings + spicy, data = conjoint_data)

# Summary of results
summary(conjoint_model)

# Extract part-worth utilities
part_worths <- coef(conjoint_model)
part_worths_df <- as.data.frame(part_worths)
part_worths_df$Attribute <- rownames(part_worths_df)

# Plot part-worth utilities
ggplot(part_worths_df, aes(x = Attribute, y = part_worths)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Part-Worth Utilities", x = "Attribute", y = "Part-Worth Utility")

# Compute importance
importance <- abs(part_worths[-1]) / sum(abs(part_worths[-1])) * 100
importance_df <- data.frame(Attribute = names(importance), Importance = importance)

# Plot attribute importance
ggplot(importance_df, aes(x = reorder(Attribute, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Attribute Importance", x = "Attribute", y = "Importance")

if(!require("gridExtra")) install.packages("gridExtra", dependencies=TRUE)

library(gridExtra)

# Create boxplots for each attribute
attributes <- c("brand", "price", "weight", "crust", "cheese", "size", "toppings", "spicy")

# Function to create a boxplot for an attribute
create_boxplot <- function(attribute) {
  ggplot(pizza_data, aes(x = !!sym(attribute), y = ranking)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = paste("Distribution of Rankings by", attribute), x = attribute, y = "Ranking")
}

# Generate boxplots for all attributes
plots <- lapply(attributes, create_boxplot)

# Display the plots
do.call(grid.arrange, c(plots, ncol = 2))