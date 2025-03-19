
# Meta --------------------------------------------------------------------

## Title:         CDC Tax Burden on Tobacco
## Author:        Martinna Roldan
## Date Created:  02/24/2025
## Date Edited:   02/24/2025
## Description:   Clean and analyze CDC data 


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

#Load the datasets
cig.data <- read_csv("data/input/The_Tax_Burden_on_Tobacco__1970-2019.csv", col_names = TRUE)
cpi.data <- read_xlsx("data/input/historical-cpi-u-202501.xlsx", skip = 3) #Changed to skip = 3 after printing out the dataset

#Determine which rows to skip from the CPI dataset
print(colnames(cpi.data))
head(cpi.data)

#Some month abbreviations have a "." after the end. To prevent issues, remove dots
print(colnames(cpi.data))
colnames(cpi.data) <- str_replace_all(colnames(cpi.data), "\\.", "")  # Remove dots

#Make sure dots are removed
print(colnames(cpi.data))

#Ensure month columns are numeric
cpi.data <- cpi.data %>%
  mutate(across(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                  "Aug", "Sep", "Oct", "Nov", "Dec"), as.numeric))
head(cpi.data)

#Ensure year column is numeric
cpi.data <- cpi.data %>% mutate(Year = as.numeric(Year))
head(cpi.data)

#Drop NA rows and "Indent Level" column
cpi.data <- cpi.data %>%
  filter(!is.na(Year)) %>%  # Remove rows where Year is NA
  select(-`Indent Level`)   # Drop unnecessary column
head(cpi.data)

# Clean tobacco data --------------------------------------------------------------
cig.data <- cig.data %>%
  mutate(measure = case_when(
    SubMeasureDesc == "Average Cost per pack" ~ "cost_per_pack",
    SubMeasureDesc == "Cigarette Consumption (Pack Sales Per Capita)" ~ "sales_per_capita",
    SubMeasureDesc == "Federal and State tax as a Percentage of Retail Price" ~ "tax_percent",
    SubMeasureDesc == "Federal and State Tax per pack" ~ "tax_dollar",
    SubMeasureDesc == "Gross Cigarette Tax Revenue" ~ "tax_revenue",
    SubMeasureDesc == "State Tax per pack" ~ "tax_state"
  )) %>%
  select(state_abb = LocationAbbr, 
         state = LocationDesc, 
         Year, 
         value=Data_Value, 
         measure)
         
final.data <- pivot_wider(cig.data, 
                          id_cols = c("state", "Year"),  # Remove "measure" from id_cols
                          names_from = "measure",
                          values_from = "value") %>%
  arrange(state, Year)



# Clean CPI data ----------------------------------------------------------
cpi.data <- pivot_longer(cpi.data, 
                         cols=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                         names_to="month",
                         values_to="index")
cpi.data <- cpi.data %>%
  group_by(Year) %>%
  summarize(index=mean(index, na.rm=TRUE))

# Find the average CPI index for the year 2012
cpi_2012 <- cpi.data %>% 
  filter(Year == 2012) %>% 
  pull(index)  # Extract the index value for 2012

# Print the value of CPI for 2012. The value is 229.59
print(paste("The average CPI index for 2012 is:", cpi_2012))

# Form final dataset ------------------------------------------------------
# adjust to 2012 dollars
final.data <- final.data %>%
  left_join(cpi.data, by="Year") %>%
  mutate(price_cpi=cost_per_pack*(230/index))

write_tsv(final.data,"data/output/TaxBurden_Data.txt",append=FALSE,col_names=TRUE)
write_rds(final.data,"data/output/TaxBurden_Data.rds")
