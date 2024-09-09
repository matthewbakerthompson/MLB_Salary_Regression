#File:        mlb_salary_regression.py
#Author:      Matthew Thompson - matthewbakerthompson@gmail.com
#Description: Linear Regression Studying Win% and Total Salary in Major League Baseball
#Date:        5.11.24

#-----------------------------------------------------------------------------------------------------
# Methodology
#-----------------------------------------------------------------------------------------------------
# Rather than focusing on the outliers – low payroll teams that do well anyway – I chose to use a more
# comprehensive figure: winning percentage over time. I decided that a dataset of 10 years would be a 
# sufficient start in analyzing WIN% versus the total salary paid. When I first started analyzing the 
# data, I was looking at the whole pool of data, but decided it would be more valuable to look at each 
# year’s data separately as well. To accomplish this, I used a loop.
#
# After the code loops through each year and produces summary statistics, it creates summary statistics 
# for the entire dataset as well. The code adds the combined summary statistics to the last dataframe
# and then completes an R-squared check.

#-----------------------------------------------------------------------------------------------------
# Conclusions
#-----------------------------------------------------------------------------------------------------
# As for the results themselves, the coefficient for the Total_Payroll variable is 5.72e-10, 
# indicating that a one unit increase in total payroll is associated with a 5.72e-10 increase 
# in win percentage. 
#
# The p-value for the Total_Payroll variable is 0.00841, which is less than # the typical significance 
# level of 0.05, indicating that the variable is statistically significant in explaining the variation
# in win percentage.

# The R-squared value of 0.14424 indicates that the model explains 14.42% of the variation in win 
# percentage. This suggests that there are other factors beyond total payroll that affect win percentage 
# in Major League Baseball. Nonetheless, the model provides evidence that total payroll is an important 
# factor to consider when discussing team success in the league.

#
#
#=====================================================================================================


# Read the data
data <- read.csv("c:/dev/r/inf300/data/2023_data.csv")

# Cleaning - Remove the dollar sign and commas from the Total_Payroll column
data$Total_Payroll <- gsub("\\$", "", data$Total_Payroll)
data$Total_Payroll <- gsub(",", "", data$Total_Payroll)

# Cleaning - Convert Total_Payroll to numeric
data$Total_Payroll <- as.numeric(data$Total_Payroll)

# Prep for loop - Get the unique years present in the dataset
unique_years <- unique(data$Year)

# Prep for loop - Create an empty list to store the summary dataframes
summary_list <- list()

# Loop through each year and fit a separate linear regression model
for (year in unique_years) {
  # Filter the data for the specific year
  data_year <- data[data$Year == year, ]
  
  # Fit the linear regression model for Win% ~ Total_Payroll for the specific year
  model <- lm(Win_Percentage ~ Total_Payroll, data = data_year)
  
  # Create a summary dataframe for the model
  summary_df <- data.frame(
    Year = year,
    Coefficient = coef(model)[2],
    Std_Error = summary(model)$coefficients[2, 2],
    P_Value = summary(model)$coefficients[2, 4],
    R_Squared = summary(model)$r.squared,
    Adjusted_R_Squared = summary(model)$adj.r.squared,
    Intercept = coef(model)[1]
  )
  
  # Add the summary dataframe to the list
  summary_list[[year]] <- summary_df
}

# Combine all the summary dataframes into one
summary_all <- do.call(rbind, summary_list)

# Fit the linear regression model for Win% ~ Total_Payroll for all data
model_all <- lm(Win_Percentage ~ Total_Payroll, data = data)

# Create a summary dataframe for the model_all
summary_all_df <- data.frame(
  Year = "All",
  Coefficient = coef(model_all)[2],
  Std_Error = summary(model_all)$coefficients[2, 2],
  P_Value = summary(model_all)$coefficients[2, 4],
  R_Squared = summary(model_all)$r.squared,
  Adjusted_R_Squared = summary(model_all)$adj.r.squared,
  Intercept = coef(model_all)[1]
)

# Add the summary dataframe to the end of summary_all dataframe
summary_all <- rbind(summary_all, summary_all_df)

# Print the combined summary
cat("Regression Analysis:\n\n")
print(summary_all)

# Check the goodness of fit with an R-squared check
cat("\nOverall R-squared value:", summary_all_df$R_Squared, "\n")

# Export the summary to a CSV file
write.csv(summary_all, "c:/dev/r/inf300/final/mlb_regression_results.csv", row.names = FALSE)
