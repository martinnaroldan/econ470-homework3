# Preliminaries -----------------------------------------------------------

# Load necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, modelsummary, AER)

# Load the cleaned dataset
final.data <- read_rds("data/output/TaxBurden_Data.rds")  # Preferred for maintaining data structure


# View structure and first few rows
glimpse(final.data)
head(final.data)

# Check for missing values
colSums(is.na(final.data))

# Summary statistics
summary(final.data)

# Compute mean, standard deviation, and histogram for key variables
sum.vars <- final.data %>% select(
  'Sales per Capita' = sales_per_capita, 
  'Real Price' = price_cpi, 
  'Nominal Price' = cost_per_pack
)

datasummary(All(sum.vars) ~ Mean + SD + Histogram, data = sum.vars)

# Identify years with tax changes
tax_changes <- final.data %>%
  filter(Year >= 1970 & Year <= 1985) %>%
  group_by(Year) %>%
  summarize(proportion = mean(diff(tax_state) != 0, na.rm=TRUE))

# Plot bar graph
ggplot(tax_changes, aes(x = Year, y = proportion)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Proportion of States with a Cigarette Tax Change (1970-1985)",
       x = "Year",
       y = "Proportion of States") +
  theme_bw()

# Compute yearly averages
avg_data <- final.data %>%
  filter(Year >= 1970 & Year <= 2018) %>%
  group_by(Year) %>%
  summarize(avg_tax = mean(tax_dollar, na.rm=TRUE),
            avg_price = mean(price_cpi, na.rm=TRUE))

# Plot both variables on the same graph
ggplot(avg_data, aes(x = Year)) +
  geom_line(aes(y = avg_tax, color = "Tax (2012 dollars)"), size = 1) +
  geom_line(aes(y = avg_price, color = "Price per Pack"), size = 1) +
  labs(title = "Average Cigarette Tax and Price (1970-2018)",
       x = "Year",
       y = "Dollars",
       color = "Legend") +
  theme_bw()

# Compute price increase for each state
price_increase <- final.data %>%
  group_by(state) %>%
  summarize(price_change = max(price_cpi, na.rm=TRUE) - min(price_cpi, na.rm=TRUE)) %>%
  arrange(desc(price_change))

# Select top 5 states
top_5_highest <- head(price_increase, 5)$state

# Filter data for those states and plot
high_states_data <- final.data %>%
  filter(state %in% top_5_highest)

ggplot(high_states_data, aes(x = Year, y = sales_per_capita, color = state)) +
  geom_line(size = 1) +
  labs(title = "Cigarette Sales per Capita for Top 5 States with Highest Price Increase",
       x = "Year",
       y = "Packs per Capita") +
  theme_bw()

# Select bottom 5 states
top_5_lowest <- tail(price_increase, 5)$state

# Filter data and plot
low_states_data <- final.data %>%
  filter(state %in% top_5_lowest)

ggplot(low_states_data, aes(x = Year, y = sales_per_capita, color = state)) +
  geom_line(size = 1) +
  labs(title = "Cigarette Sales per Capita for Top 5 States with Lowest Price Increase",
       x = "Year",
       y = "Packs per Capita") +
  theme_bw()

ggplot() +
  geom_line(data = high_states_data, aes(x = Year, y = sales_per_capita, color = "High Price Increase"), size = 1) +
  geom_line(data = low_states_data, aes(x = Year, y = sales_per_capita, color = "Low Price Increase"), size = 1) +
  labs(title = "Comparison of Cigarette Sales Trends",
       x = "Year",
       y = "Packs per Capita",
       color = "State Group") +
  theme_bw()

# Regression for price elasticity
elasticity_70_90 <- lm(log(sales_per_capita) ~ log(price_cpi), 
                        data = final.data %>% filter(Year >= 1970 & Year <= 1990))
summary(elasticity_70_90)

# IV Regression using tax as instrument
iv_70_90 <- ivreg(log(sales_per_capita) ~ log(price_cpi) | tax_dollar, 
                   data = final.data %>% filter(Year >= 1970 & Year <= 1990))
summary(iv_70_90)

# First-stage regression
first_stage_70_90 <- lm(log(price_cpi) ~ tax_dollar, 
                         data = final.data %>% filter(Year >= 1970 & Year <= 1990))
summary(first_stage_70_90)

# Reduced-form regression
reduced_form_70_90 <- lm(log(sales_per_capita) ~ tax_dollar, 
                          data = final.data %>% filter(Year >= 1970 & Year <= 1990))
summary(reduced_form_70_90)

# Regression for price elasticity (1991-2015)
elasticity_91_15 <- lm(log(sales_per_capita) ~ log(price_cpi), 
                        data = final.data %>% filter(Year >= 1991 & Year <= 2015))
summary(elasticity_91_15)

# IV Regression (1991-2015)
iv_91_15 <- ivreg(log(sales_per_capita) ~ log(price_cpi) | tax_dollar, 
                   data = final.data %>% filter(Year >= 1991 & Year <= 2015))
summary(iv_91_15)

# First-stage regression (1991-2015)
first_stage_91_15 <- lm(log(price_cpi) ~ tax_dollar, 
                         data = final.data %>% filter(Year >= 1991 & Year <= 2015))
summary(first_stage_91_15)

# Reduced-form regression (1991-2015)
reduced_form_91_15 <- lm(log(sales_per_capita) ~ tax_dollar, 
                          data = final.data %>% filter(Year >= 1991 & Year <= 2015))
summary(reduced_form_91_15)

# Print elasticities
cat("Elasticity (1970-1990):", summary(elasticity_70_90)$coefficients[2,1], "\n")
cat("Elasticity (1991-2015):", summary(elasticity_91_15)$coefficients[2,1], "\n")

