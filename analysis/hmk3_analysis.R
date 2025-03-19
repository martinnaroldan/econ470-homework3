# Preliminaries -----------------------------------------------------------

# Load necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, modelsummary, AER, fixest,kableExtra)


# Read data and set workspace for knitr
cig.data <- readRDS("data/output/TaxBurden_Data.rds")

# Process Data
cig.data <- cig.data %>%
  group_by(state) %>%
  arrange(state, Year) %>%
  mutate(
    tax_change = tax_state - lag(tax_state),
    tax_change_d = ifelse(tax_change == 0, 0, 1),
    price_cpi_2012 = price_cpi,  
    total_tax_cpi_2012 = tax_dollar * (230 / index),
    ln_tax_2012 = log(total_tax_cpi_2012),
    ln_sales = log(sales_per_capita),
    ln_price_2012 = log(price_cpi_2012)
  )

## Tax changes
tax.change.plot <- cig.data %>%
  group_by(Year) %>%
  filter(Year < 1986, Year > 1970) %>%
  summarize(mean_change = mean(tax_change_d)) %>%
  ggplot(aes(x = as.factor(Year), y = mean_change)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Year",
    y = "Share of States"
  ) +
  ylim(0, 0.5)
print(tax.change.plot)

## Tax and price data
tax.price.data <- cig.data %>%
  select(Year, state, total_tax_cpi_2012, price_cpi_2012) %>%
  pivot_longer(cols = c(total_tax_cpi_2012, price_cpi_2012),
               names_to = "var", values_to = "dollars")


tax.price.plot <- tax.price.data %>%
  ggplot(aes(x = Year, y = dollars, color = var)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +  
  labs(
    x = "Year",
    y = "Dollars per Pack (2012 $)",
    color = "Category"  
  ) +
  ylim(0, 10) +
  scale_color_manual(
    values = c("red", "blue"),
    labels = c("Mean Tax", "Mean Price")
  ) + 
  theme_bw() +
  theme(legend.position = "top") +  
  scale_x_continuous(breaks = seq(1970, 2020, 5))

print(tax.price.plot)

## Price Changes
cig.data.change <- cig.data %>%
  ungroup() %>%
  filter(Year == 1970) %>%
  select(state, price_1970 = price_cpi_2012) %>%
  left_join(cig.data %>% filter(Year == 2018) %>%
              select(state, price_2018 = price_cpi_2012), by = "state") %>%
  mutate(price_change = price_2018 - price_1970)

high.change <- cig.data.change %>%
  slice_max(price_change, n = 5) %>%
  mutate(change_group = "high")

low.change <- cig.data.change %>%
  slice_min(price_change, n = 5) %>%
  mutate(change_group = "low")

change.group <- rbind(high.change, low.change)

top.bottom.price <- cig.data %>%
  ungroup() %>%
  inner_join(change.group %>% select(state, change_group), by = "state")

## Figure for high price changes
high.price.plot <- top.bottom.price %>% filter(change_group=="high") %>%
  ggplot(aes(x=Year, y=sales_per_capita, color=state)) +
  stat_summary(fun="mean", geom="line") +
  labs(
    x="Year",
    y="Packs per Capita",
    color="State"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970, 2019, 5))
print(high.price.plot)

## Figure for low price changes
low.price.plot <- top.bottom.price %>% filter(change_group=="low") %>%
  ggplot(aes(x=Year, y=sales_per_capita, color=state)) +
  stat_summary(fun="mean", geom="line") +
  labs(
    x="Year",
    y="Packs per Capita",
    color="State"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970, 2019, 5))
print(low.price.plot)

## Figure for high and low price changes

combined.price.plot <- top.bottom.price %>%
  ggplot(aes(x = Year, y = sales_per_capita, linetype = change_group)) +
  stat_summary(fun = "mean", geom = "line", size = 1.2) +
  labs(
    x = "Year",
    y = "Packs per Capita",
    linetype = "Level of Price Increase"
  ) +
  geom_text(data = annotation_data, 
            aes(x = Year + 2, y = mean_sales, label = change_group), 
            fontface = "bold", size = 5, hjust = 0) +  # Moves text outside the right end
  theme_bw()
print(combined.price.plot)


## Regression results 1970-1990
ols1 <- feols(ln_sales ~ ln_price_2012, data=cig.data %>% filter(Year<1991))
iv1 <- feols(ln_sales ~ 1 | ln_price_2012 ~ ln_tax_2012, data=cig.data %>% filter(Year<1991))
first.stage <- feols(ln_price_2012 ~ ln_tax_2012, data=cig.data %>% filter(Year<1991))
reduced.form <- feols(ln_sales ~ ln_tax_2012, data=cig.data %>% filter(Year<1991))

## Regression results 1991-2015
ols2 <- feols(ln_sales ~ ln_price_2012, data=cig.data %>% filter(Year>1991 & Year<=2015))
iv2 <- feols(ln_sales ~ 1 | ln_price_2012 ~ ln_tax_2012, data=cig.data %>% filter(Year>1991 & Year<=2015))
first.stage2 <- feols(ln_price_2012 ~ ln_tax_2012, data=cig.data %>% filter(Year>1991 & Year<=2015))
reduced.form2 <- feols(ln_sales ~ ln_tax_2012, data=cig.data %>% filter(Year>1991 & Year<=2015))


rm(list=c("tax.price.data"))

f <- function(x) formatC(x, digits = 0, big.mark = ",", format = "f")

modelsummary(list("Estimates" = list("OLS" = ols1, "IV" = iv1, 
                                     "OLS" = ols2, "IV" = iv2),
                  "Reduced Form" = list("IV" = reduced.form, "IV" = reduced.form2),
                  "First Stage" = list("IV" = first.stage, "IV" = first.stage2)),
             shape = "rbind",
             coef_map = c("ln_price_2012" = "Log Price",
                          "fit_ln_price_2012" = "Fitted Log Price",
                          "ln_tax_2012" = "Log Tax"),
             gof_map = list(list("raw" = "N", "clean" = "N", "fmt" = f),
                            list("raw" = "r.squared", "clean" = "R2", "fmt" = 2)),
             output = "kableExtra") %>%
  add_header_above(c(" " = 1, "1970 - 1990" = 2, "1991 - 2015" = 2)) %>%
  kable_styling(latex_options = "hold_position")

