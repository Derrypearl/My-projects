#1. Read in packages
packages <- c("tidyverse",
              "readxl",
              "janitor",
              "openxlsx",
              "lubridate",
              "writexl",
              "knitr",
              "utils",
              "dplyr",
              "zoo",
              "lubridate",
              "readr",
              "forcats",
              "ggplot2",
              "summarytools",
              "scales",
              "naniar"
)
 #install any missing packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

#Load packages
lapply(packages, library, character.only = T)

#2. load dataset
house_prices <- read_excel("C:\\Users\\Dera\\Downloads\\HRMC UK Housing data.xlsx")


#3. view structure and summary of data
str(house_prices)
head(house_prices)

#4. Remove AveragePriceSA column
house_prices <- house_prices |> select(-AveragePriceSA)
view(house_prices)

# 5. Extract date-related features
house_prices <- house_prices %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE),
    YearMonth = format(Date, "%Y-%m")  # useful for plotting time series
  )

# 6. Convert selected columns to factor (categorical)
house_prices <- house_prices %>%
  mutate(
    Tier = as.factor(Tier),
    RegionName = as.factor(RegionName),
    AreaCode = as.factor(AreaCode)
  )

# 7. View factor levels
levels(house_prices$RegionName)
levels(house_prices$Tier)
levels(house_prices$AreaCode)


# 8. Summary statistics
summary(house_prices)

# 9. Count missing values per column
colSums(is.na(house_prices))

# 10. Regional mean for numeric values (to inform imputation strategy — exploratory)
house_prices %>%
  group_by(RegionName) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))

# 11. Count of missing values per numeric column by region
house_prices %>%
  group_by(RegionName) %>%
  summarise(across(where(is.numeric), ~sum(is.na(.x)))) %>%
  arrange(desc(SalesVolume))

# 12. Visualise missing data
print(gg_miss_var(house_prices))

# 13. Regions missing specific property-type prices
house_prices %>%
  group_by(RegionName) %>%
  summarise(missing_semi = sum(is.na(SemiDetachedPrice)),
            missing_detached = sum(is.na(DetachedPrice)),
            missing_terraced = sum(is.na(TerracedPrice)),
            missing_flat = sum(is.na(FlatPrice))) %>%
  arrange(desc(missing_semi + missing_detached + missing_terraced + missing_flat))

# Filter dataset to only include non-overlapping Tier levels
filtered_data_2014 <- house_prices %>%
  filter(
    Year == 2014,
    Tier %in% c(
      "Lower Tier England",
      "Upper Tier England",
      "Local Authority Wales",
      "Local Authority Scotland",
      "Local Authority Northern Ireland"
    )
  )

# 1. Compute the total UK sales for 2014 (KPI)
total_uk_2014 <- filtered_data_2014 %>%
  summarise(total_sales_uk = sum(SalesVolume, na.rm = TRUE)) %>%
  pull(total_sales_uk)

# 2. Display total as a big KPI visual
ggplot() +
  annotate(
    "text", x = 1, y = 1,
    label = paste0("Total UK House Sales in 2014:\n", comma(total_uk_2014)),
    size = 12, fontface = "bold"
  ) +
  theme_void()

# 3. Breakdown of total sales by Tier
sales_2014_by_tier <- filtered_data_2014 %>%
  group_by(Tier) %>%
  summarise(total_sales = sum(SalesVolume, na.rm = TRUE)) %>%
  arrange(desc(total_sales))

# 4. Bar chart showing sales volume by Tier (2014)
ggplot(sales_2014_by_tier, aes(x = fct_reorder(Tier, total_sales), y = total_sales)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Total House Sales by Tier (2014)",
    subtitle = "Filtered to Local Authority and Lower/Upper Tier Units",
    x = "Tier",
    y = "Total Sales Volume"
  ) +
  theme_minimal()

#####Q2
# 1. Filter for 2015 and keep only relevant Tiers to avoid duplication
filtered_cash_2015 <- house_prices %>%
  filter(
    Year == 2015,
    Tier %in% c(
      "Lower Tier England",
      "Upper Tier England",
      "Local Authority Wales",
      "Local Authority Scotland",
      "Local Authority Northern Ireland"
    ),
    !is.na(CashSalesVolume),
    SalesVolume > 0
  )

# 2. Group by Tier and calculate proportion of cash sales
cash_sales_2015 <- filtered_cash_2015 %>%
  group_by(Tier) %>%
  summarise(
    total_cash  = sum(CashSalesVolume, na.rm = TRUE),
    total_sales = sum(SalesVolume, na.rm = TRUE),
    cash_ratio  = total_cash / total_sales,
    .groups = "drop"
  ) %>%
  arrange(desc(cash_ratio))

# 3. View top Tier with highest cash sales proportion
head(cash_sales_2015, 1)

##plot 
ggplot(cash_sales_2015, aes(x = fct_reorder(Tier, cash_ratio), y = cash_ratio)) +
  geom_col(fill = "blue") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Proportion of Cash Sales by Tier (2015)",
    subtitle = "Excludes National/Regional duplication",
    x        = "Geographic Tier",
    y        = "Cash Sales as % of Total Sales"
  ) +
  theme_minimal()

###	In which Region and Month did the largest 12-month percentage change occur 
# Calculate % change in AvgPrice from previous year
house_prices <- house_prices %>%
  arrange(RegionName, Date) %>%
  group_by(RegionName) %>%
  mutate(price_pct_change = (AvgPrice / lag(AvgPrice, 12) - 1) * 100)

# Step 1: Calculate 12-month % change in price
house_prices <- house_prices %>%
  arrange(RegionName, Date) %>%
  group_by(RegionName) %>%
  mutate(price_pct_change = (AveragePrice / lag(AveragePrice, 12) - 1) * 100) %>%
  ungroup()

# Step 2: Find the row with the **maximum change overall**
max_change_row <- house_prices %>%
  filter(!is.na(price_pct_change)) %>%
  slice_max(price_pct_change, n = 1)

# View result
print(max_change_row %>%
        select(RegionName, Date, price_pct_change, AveragePrice))

# Highlight region's price trend over time to show spike
ggplot(house_prices %>% filter(RegionName == max_change_row$RegionName), 
       aes(x = Date, y = AveragePrice)) +
  geom_line(color = "darkblue") +
  geom_point(data = max_change_row, aes(x = Date, y = AveragePrice), color = "red", size = 3) +
  labs(
    title = paste("Average House Price Trend in", max_change_row$RegionName),
    subtitle = paste("Largest 12-month % change in", format(max_change_row$Date, "%b %Y")),
    x = "Date", y = "Average Price (£)"
  ) +
  theme_minimal()


##Q4: How have housing trends changed over time (nationally or by region)?
# National average price trend
avg_trend <- house_prices %>%
  group_by(YearMonth) %>%
  summarise(mean_price = mean(AveragePrice, na.rm = TRUE))

# Line plot
ggplot(avg_trend, aes(x = as.Date(paste0(YearMonth, "-01")), y = mean_price)) +
  geom_line(color = "dodgerblue", size = 1) +
  labs(title = "UK Average Housing Price Trend Over Time",
       x = "Date", y = "Average Price (£)") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

###regional price trends over time
filtered_data_2014 %>%
  group_by(YearMonth, Tier) %>%
  summarise(avg_price = mean(AveragePrice, na.rm = TRUE)) %>%
  ggplot(aes(x = as.Date(paste0(YearMonth, "-01")), y = avg_price, color = Tier)) +
  geom_line(size = 1) +
  labs(title = "Average House Price Trends by Tier", x = "Date", y = "Average Price (£)") +
  theme_minimal()

####Can we make basic projections based on past trends?
# Forecast next 12 months using linear model (national)
model_data <- avg_trend %>%
  mutate(month_num = 1:n())  # create a time index

# First, make sure model_data$YearMonth is also a Date
model_data <- avg_trend %>%
  mutate(
    month_num = 1:n(),
    YearMonth = as.Date(paste0(YearMonth, "-01"))  # convert to Date
  )

combined <- bind_rows(
  model_data %>% mutate(source = "Actual"),
  future_months %>% select(mean_price = predicted_price, YearMonth) %>% mutate(source = "Forecast")
)

ggplot(combined, aes(x = YearMonth, y = mean_price, color = source)) +
  geom_line(size = 1) +
  labs(title = "Average Price Forecast (Next 12 Months)",
       x = "Date", y = "Mean Price (£)",
       color = "Data Type") +
  theme_minimal()














###the total sales volume in 2014, and how did it vary across regions?
#Total UK sales in 2014 (KPI only)
# 1. Compute the total
  total_uk_2014 <- house_prices %>%
    filter(Year == 2014) %>%
    summarise(total_sales_uk = sum(SalesVolume, na.rm = TRUE)) %>%
    pull(total_sales_uk)
 # 2. Plot it as a big text
  ggplot() +
    annotate(
      "text", x = 1, y = 1,
      label = paste0("Total UK Sales in 2014:\n", comma(total_uk_2014)),
      size = 12, fontface = "bold"
    ) +
    theme_void()
  
# 2. Breakdown by Tier (e.g. Country, Region) for context
  sales_2014_by_tier <- house_prices %>%
    filter(Year == 2014) %>%
    group_by(Tier) %>%               # <-- use Tier instead of RegionName
    summarise(total_sales = sum(SalesVolume, na.rm = TRUE)) %>%
    arrange(desc(total_sales))
  
# 3. Bar chart: Total sales by Tier (2014)
  ggplot(sales_2014_by_tier, aes(x = fct_reorder(Tier, total_sales), y = total_sales)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Total House Sales by Tier (2014)",
      subtitle = "Aggregated at the Country / Region level",
      x = "Tier", y = "Sales Volume"
    ) +
    theme_minimal()
  
##Which region had the highest proportion of cash sales in 2015?

  # Filter for 2015, exclude rows where cash data is missing
  cash_prop_2015 <- house_prices %>%
    filter(Year == 2015 & !is.na(CashSalesVolume) & !is.na(SalesVolume)) %>%
    group_by(RegionName) %>%
    summarise(cash_prop = sum(CashSalesVolume, na.rm = TRUE) / sum(SalesVolume, na.rm = TRUE)) %>%
    arrange(desc(cash_prop))
  
  # 1. Compute cash‐to‐total sales proportion by Tier for 2015
  cash_sales_2015 <- house_prices %>%
    filter(
      Year == 2015,                   # only 2015
      !is.na(CashSalesVolume),        # exclude rows where cash data is missing
      SalesVolume > 0                 # ensure total sales > 0 to avoid division by zero
    ) %>%
    group_by(Tier) %>%                # group at Country/Region level
    summarise(
      total_cash  = sum(CashSalesVolume, na.rm = TRUE),
      total_sales = sum(SalesVolume,      na.rm = TRUE),
      cash_ratio  = total_cash / total_sales,
      .groups = "drop"
    ) %>%
    arrange(desc(cash_ratio))
  
#View top Tier
print(head(cash_sales_2015, 5))

# 3. Bar chart of cash‐sales proportion by Tier
ggplot(cash_sales_2015, aes(
  x = fct_reorder(Tier, cash_ratio),
  y = cash_ratio
)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Proportion of Cash Sales by Tier (2015)",
    subtitle = "Filtered to records with cash sales data",
    x        = "Tier (Country/Region)",
    y        = "Cash Sales as % of Total Sales"
  ) +
  theme_minimal()




####Can we make basic projections based on past trends?
# Forecast next 12 months using linear model (national)
model_data <- avg_trend %>%
  mutate(month_num = 1:n())  # create a time index

# First, make sure model_data$YearMonth is also a Date
model_data <- avg_trend %>%
  mutate(
    month_num = 1:n(),
    YearMonth = as.Date(paste0(YearMonth, "-01"))  # convert to Date
  )

combined <- bind_rows(
  model_data %>% mutate(source = "Actual"),
  future_months %>% select(mean_price = predicted_price, YearMonth) %>% mutate(source = "Forecast")
)

ggplot(combined, aes(x = YearMonth, y = mean_price, color = source)) +
  geom_line(size = 1) +
  labs(title = "Average Price Forecast (Next 12 Months)",
       x = "Date", y = "Mean Price (£)",
       color = "Data Type") +
  theme_minimal()

###regional price trends over time
house_prices %>%
  group_by(YearMonth, Tier) %>%
  summarise(avg_price = mean(AveragePrice, na.rm = TRUE)) %>%
  ggplot(aes(x = as.Date(paste0(YearMonth, "-01")), y = avg_price, color = Tier)) +
  geom_line(size = 1) +
  labs(title = "Average House Price Trends by Tier", x = "Date", y = "Average Price (£)") +
  theme_minimal()

###Cash vs Mortgage Trends Over Time (National or by Tier)
house_prices %>%
  filter(!is.na(CashSalesVolume), !is.na(MortgageSalesVolume)) %>%
  group_by(YearMonth) %>%
  summarise(
    cash_total = sum(CashSalesVolume, na.rm = TRUE),
    mortgage_total = sum(MortgageSalesVolume, na.rm = TRUE)
  ) %>%
  mutate(total = cash_total + mortgage_total,
         cash_pct = cash_total / total,
         mortgage_pct = mortgage_total / total) %>%
  pivot_longer(cols = c(cash_pct, mortgage_pct), names_to = "type", values_to = "percentage") %>%
  ggplot(aes(x = as.Date(paste0(YearMonth, "-01")), y = percentage, fill = type)) +
  geom_area(alpha = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Cash vs Mortgage Sales Proportion Over Time", y = "Percentage of Total Sales") +
  theme_minimal()

###
house_prices %>%
  group_by(Month) %>%
  summarise(avg_sales = mean(SalesVolume, na.rm = TRUE)) %>%
  ggplot(aes(x = Month, y = avg_sales)) +
  geom_col(fill = "skyblue") +
  labs(title = "Average Monthly Sales Volume (All Years)", x = "Month", y = "Avg Sales Volume") +
  theme_minimal()






# Fit model
model <- lm(mean_price ~ month_num, data = model_data)

# Forecast future 12 months
future_months <- data.frame(month_num = (max(model_data$month_num) + 1):(max(model_data$month_num) + 12))
future_months$predicted_price <- predict(model, newdata = future_months)
future_months$YearMonth <- seq(as.Date(paste0(tail(avg_trend$YearMonth, 1), "-01")) + months(1), by = "1 month", length.out = 12)

# Combine for plot
combined <- bind_rows(
  model_data %>% mutate(source = "Actual"),
  future_months %>% select(mean_price = predicted_price, YearMonth) %>% mutate(source = "Forecast")
)

# Plot actual vs forecast
ggplot(combined, aes(x = as.Date(YearMonth), y = mean_price, color = source)) +
  geom_line(size = 1.2) +
  labs(title = "Average Price Forecast (Next 12 Months)",
       x = "Date", y = "Price (£)") +
  scale_color_manual(values = c("Actual" = "dodgerblue", "Forecast" = "orange")) +
  theme_minimal()


  
  




































#to view all
print(df, n = Inf)

#view missing values visually
install.packages("naniar")
library(naniar)
gg_miss_var(house_prices)  # Bar plot of NA counts per column


#find entire columns with missing values for semidetached
house_prices %>%
  group_by(RegionName) %>%
  summarise(missing_count = sum(is.na(SemiDetachedPrice))) %>%
  filter(missing_count > 0)

#
#find entire columns with missing values for detached
df %>%
  group_by(RegionName) %>%
  summarise(missing_count = sum(is.na(DetachedPrice))) %>%
  filter(missing_count > 0)

#find entire columns with missing values for terraced
df %>%
  group_by(RegionName) %>%
  summarise(missing_count = sum(is.na(TerracedPrice))) %>%
  filter(missing_count > 0)

#find entire columns with missing values for flatprice
df %>%
  group_by(RegionName) %>%
  summarise(missing_count = sum(is.na(FlatPrice))) %>%
  filter(missing_count > 0)


# Count NA per region
df %>%
  group_by(RegionName) %>%
  summarise(across(where(is.numeric), ~sum(is.na(.)))) %>%
  arrange(desc(SalesVolume))

#######step 3

pct_change <- house_prices%>%
  arrange(RegionName, Date) %>%
  group_by(RegionName) %>%
  mutate(PctChange12M = (AveragePrice / lag(AveragePrice, 12) - 1) * 100) %>%
  ungroup()

# Find the maximum % change
pct_change %>%
  filter(!is.na(PctChange12M)) %>%
  arrange(desc(PctChange12M)) %>%
  slice(1)

###
# Visualize percentage change over time
ggplot(pct_change, aes(x = Date, y = PctChange12M, color = RegionName)) +
  geom_line() +
  labs(title = "12-Month Percentage Change in Average House Price by Region")



