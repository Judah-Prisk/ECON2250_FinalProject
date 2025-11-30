# Load necessary libraries
# If you don't have these installed, run: install.packages(c("dplyr", "ggplot2", "readr", "stringr"))
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

# Read the data
# The CSV has a weird structure with MLB and NFL data side-by-side
# We'll read it without headers first to inspect, or just read it and split by column index

# Determine path based on execution location
if (file.exists("data/Payroll Data.csv")) {
  data_path <- "data/Payroll Data.csv"
} else if (file.exists("../data/Payroll Data.csv")) {
  data_path <- "../data/Payroll Data.csv"
} else {
  stop("Could not find Payroll Data.csv")
}

data <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE)

# Inspect column names to verify structure
print(colnames(data))

# Split the data
# Columns 1-5: MLB
# Columns 7-11: NFL (Column 6 is likely empty or a separator)

mlb_data <- data[, 1:5]
nfl_data <- data[, 7:11]

# Rename columns for consistency
colnames(mlb_data) <- c("Team", "Year", "Total_Payroll", "Payroll_Adj", "Win_Pct")
colnames(nfl_data) <- c("Team", "Year", "Total_Payroll", "Payroll_Adj", "Win_Pct")

# Function to clean payroll data
clean_payroll <- function(x) {
  # Remove '$', ',', and whitespace
  x <- str_replace_all(x, "[\\$,\\s]", "")
  # Convert to numeric
  as.numeric(x)
}

# Clean MLB Data
mlb_data <- mlb_data %>%
  mutate(
    Total_Payroll = clean_payroll(Total_Payroll),
    Payroll_Adj = as.numeric(Payroll_Adj),
    Win_Pct = as.numeric(Win_Pct),
    Year = as.integer(Year)
  ) %>%
  filter(!is.na(Team) & Team != "") # Remove empty rows

# Clean NFL Data
nfl_data <- nfl_data %>%
  mutate(
    Total_Payroll = clean_payroll(Total_Payroll),
    Payroll_Adj = as.numeric(Payroll_Adj),
    Win_Pct = as.numeric(Win_Pct),
    Year = as.integer(Year)
  ) %>%
  filter(!is.na(Team) & Team != "") # Remove empty rows

# Handle missing Total_Payroll in MLB (e.g., 2015 data seems to be missing Total Payroll in the first few rows)
# We can check how many are missing
cat("Missing MLB Total Payroll:", sum(is.na(mlb_data$Total_Payroll)), "\n")
cat("Missing NFL Total Payroll:", sum(is.na(nfl_data$Total_Payroll)), "\n")

# For analysis, we'll filter out rows with missing payroll or win %
mlb_analysis <- mlb_data %>% filter(!is.na(Total_Payroll) & !is.na(Win_Pct))
nfl_analysis <- nfl_data %>% filter(!is.na(Total_Payroll) & !is.na(Win_Pct))

# --- Analysis ---

# 1. Summary Statistics
cat("\n--- MLB Summary ---\n")
summary(mlb_analysis[, c("Total_Payroll", "Win_Pct")])

cat("\n--- NFL Summary ---\n")
summary(nfl_analysis[, c("Total_Payroll", "Win_Pct")])

# 2. Regression Analysis

# MLB Regression
mlb_model <- lm(Win_Pct ~ Total_Payroll, data = mlb_analysis)
cat("\n--- MLB Regression Results ---\n")
print(summary(mlb_model))

# NFL Regression
nfl_model <- lm(Win_Pct ~ Total_Payroll, data = nfl_analysis)
cat("\n--- NFL Regression Results ---\n")
print(summary(nfl_model))

# 3. Visualization

# Create a combined dataframe for plotting (optional, or plot separately)
mlb_analysis$League <- "MLB"
nfl_analysis$League <- "NFL"
combined_data <- rbind(mlb_analysis, nfl_analysis)

# Plot: Payroll vs Win % by League
p <- ggplot(combined_data, aes(x = Total_Payroll, y = Win_Pct, color = League)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Payroll Spending vs. Team Performance (MLB vs. NFL)",
    x = "Total Payroll ($)",
    y = "Win Percentage",
    caption = "Source: Payroll Data.csv"
  ) +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  theme_minimal()

# Save the plot
ggsave("images/payroll_vs_performance.png", plot = p, width = 10, height = 6)
print("Plot saved to images/payroll_vs_performance.png")

# Separate plots if needed
p_mlb <- ggplot(mlb_analysis, aes(x = Total_Payroll, y = Win_Pct)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "darkblue", se = TRUE) +
  labs(title = "MLB: Payroll vs. Win %", x = "Total Payroll", y = "Win %") +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  theme_minimal()

ggsave("images/mlb_payroll_vs_performance.png", plot = p_mlb, width = 8, height = 6)

p_nfl <- ggplot(nfl_analysis, aes(x = Total_Payroll, y = Win_Pct)) +
  geom_point(color = "red", alpha = 0.6) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(title = "NFL: Payroll vs. Win %", x = "Total Payroll", y = "Win %") +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  theme_minimal()

ggsave("images/nfl_payroll_vs_performance.png", plot = p_nfl, width = 8, height = 6)
