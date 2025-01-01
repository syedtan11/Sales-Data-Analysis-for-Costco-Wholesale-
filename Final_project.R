#Name: Tanveer syed
#Aly-6070
#Final Project Presentation

# Loading the dataset
data <- read.csv("Desktop/D24 MAB FY19P1to7.csv")
# Loading required libraries
library(ggplot2)

#1. Distribution of Transactions Across Regions:
#Question: How are transactions distributed across different regions?
  
# Calculating the count of transactions by region
region_counts <- table(data$REGION)

# Converting the table to a data frame for plotting
region_df <- as.data.frame(region_counts)

# Adding a column for percentages
region_df$percentage <- region_df$Freq / sum(region_df$Freq) * 100

# Ploting bar plot
bar_plot <- ggplot(region_df, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Distribution of Transactions Across Regions",
       x = "Region", y = "Transaction Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(aes(label = Freq), vjust = -0.3, size = 3)  

# Plotting pie chart
pie_chart <- ggplot(region_df, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Transactions Across Regions",
       fill = "Region") +
  theme_void() +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5))

# Printing both plots
print(bar_plot)
print(pie_chart)



#2.	Distribution of Transactions Across Fiscal Periods: 

#Question: How are transactions distributed across different fiscal periods?
  
# Loading required library
library(ggplot2)

# Calculating the count of transactions for each fiscal period
transaction_counts <- table(data$FISCAL_PERIOD)

# Converting the transaction counts to a dataframe
transaction_counts_df <- as.data.frame(transaction_counts)

# Calculating the percentage of transactions for each fiscal period
transaction_counts_df$Transaction_Percentage <- (transaction_counts_df$Freq / sum(transaction_counts_df$Freq)) * 100

# Renaming the columns for better clarity
colnames(transaction_counts_df) <- c("Fiscal_Period", "Transaction_Count", "Transaction_Percentage")

# Creating bar plot with counts
ggplot(data = transaction_counts_df, aes(x = Fiscal_Period, y = Transaction_Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Transaction_Count), vjust = -0.5, size = 3, color = "black") +  # Add count labels on top of bars
  labs(title = "Distribution of Transactions Across Fiscal Periods",
       x = "Fiscal Period",
       y = "Number of Transactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linegraph
# Group transactions by fiscal period and count the number of transactions for each fiscal period
transaction_counts <- table(data$FISCAL_PERIOD)

# Convert the transaction counts to a dataframe
transaction_counts_df <- as.data.frame(transaction_counts)

# Rename the columns for better clarity
colnames(transaction_counts_df) <- c("Fiscal_Period", "Transaction_Count")

# Create line plot
line_plot <- ggplot(data = transaction_counts_df, aes(x = Fiscal_Period, y = Transaction_Count, group = 1)) +
  geom_line(color = "skyblue") +
  geom_point(color = "blue", size = 3) +  # Add points for better visualization
  labs(title = "Distribution of Transactions Across Fiscal Periods",
       x = "Fiscal Period",
       y = "Number of Transactions") +
  theme_minimal()

# Print the line plot
print(line_plot)





















