#Read in packages
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

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

#Load packages
lapply(packages, library, character.only = T)

file_path <-"C:\\Users\\Dera\\Downloads\\Senior Analyst Interview Task.xlsx"

# Get all sheet names except the instruction sheet
sheets <- excel_sheets(file_path)
sheets <- sheets[!sheets %in% c("The task")]  # exclude by name


# Read and combine
all_data <- map_df(sheets, ~ {
  df <- read_excel(file_path, sheet = .x)
  df <- df %>%
    rename_with(~ tolower(gsub(" ", "_", .))) %>%  # clean column names
    mutate(year = as.integer(.x))                  # add year from sheet name
})
glimpse(all_data)

# remove duplicates within a year, based on PSR
all_data <- all_data %>% distinct(psr, year, .keep_all = TRUE)

#check for missing data
summary(all_data)

#Convert numeric columns to numeric type
all_data <- all_data %>%
  mutate(
    memberships = as.numeric(memberships),
    assets = as.numeric(assets)
  )

#Standardise text fields 
all_data <- all_data %>%
  mutate(scheme_name = tolower(scheme_name))

#count number of unique schemes each year
schemes_per_year <- all_data %>% 
  group_by(year) %>% 
  summarise(num_schemes = n_distinct(psr))

#line graph for schemes per year
library(ggplot2)
ggplot(schemes_per_year, aes(x = year, y = num_schemes)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Number of Distinct Pension Schemes per Year",
    x = "Year",
    y = "Number of Schemes"
  ) +
  theme_minimal()

#bar plot
ggplot(schemes_per_year, aes(x = factor(year), y = num_schemes)) +
  geom_col(fill = "skyblue") +
  labs(
    title = "Number of Distinct Pension Schemes per Year",
    x = "Year",
    y = "Number of Schemes"
  ) +
  theme_minimal()

####membership trends per year
members_per_year <- all_data %>% 
  group_by(year) %>% 
  summarise(total_members = sum(memberships, na.rm = TRUE))

#line plot for trends in membership
ggplot(members_per_year, aes(x = year, y = total_members)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Number of membership numbers per Year",
    x = "Year",
    y = "Number of memberships"
  ) +
  theme_minimal()

#bar plot for membership per year
ggplot(members_per_year, aes(x = factor(year), y = total_members)) +
  geom_col(fill = "skyblue") +
  labs(
    title = "Number of membership per Year",
    x = "Year",
    y = "Number of Memberships"
  ) +
  theme_minimal

###assets per year
assets_per_year <- all_data %>% 
  group_by(year) %>% 
  summarise(total_assets = sum(assets, na.rm = TRUE))

#bar plot for assets per year
ggplot(assets_per_year, aes(x = factor(year), y = total_assets)) +
  geom_col(fill = "red") +
  labs(
    title = "Number of assets per Year",
    x = "Year",
    y = "Number of Assets"
  ) +
  theme_minimal()

#line plot for asset
ggplot(assets_per_year, aes(x = year, y = total_assets)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Number of assets numbers per Year",
    x = "Year",
    y = "Number of assets"
  ) +
  theme_minimal()

###average memberships per year
avg_memberships_per_year <- all_data %>%
  group_by(year) %>%
  summarise(
    total_memberships = sum(memberships, na.rm = TRUE),
    num_schemes = n_distinct(psr),
    avg_memberships = total_memberships / num_schemes
  )

#average membership line plot
ggplot(avg_memberships_per_year, aes(x = year, y = avg_memberships)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Average Memberships per Pension Scheme per Year",
    x = "Year",
    y = "Average Memberships"
  ) +
  theme_minimal()




#future prediction for trust based schemes
# Fit the model
model <- lm(num_schemes ~ year, data = schemes_per_year)

# Predictions for future years
future_years <- data.frame(year = 2026:2030)
future_years$predicted_schemes <- predict(model, newdata = future_years)

# Combine historical and forecast data
combined_data <- rbind(
  schemes_per_year %>% mutate(type = "Actual"),
  future_years %>% rename(num_schemes = predicted_schemes) %>% mutate(type = "Forecast")
)

# Plot
ggplot(combined_data, aes(x = year, y = num_schemes, color = type)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(
    title = "Forecast of Trust-Based Pension Schemes",
    x = "Year",
    y = "Number of Schemes"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
# set the x-axis breaks to integers(sets years on x axis to whole numbers)
scale_x_continuous(breaks = scales::breaks_pretty())

#future prediction for assets
library(forecast)

# Fit linear model
lm_fit <- lm(total_assets ~ year, data = assets_per_year)

# Predict for next 5 years
future_yearss <- data.frame(year = 2025:2030)
lm_pred <- predict(lm_fit, newdata = future_yearss, interval = "confidence")

# Combine for plotting
lm_forecast <- data.frame(
  year = future_yearss$year,
  forecast = lm_pred[, "fit"],
  lower = lm_pred[, "lwr"],
  upper = lm_pred[, "upr"]
)
##plot
 ggplot() +
    geom_line(data = assets_per_year, aes(x = year, y = total_assets), color = "black", size = 1) +
    geom_point(data = assets_per_year, aes(x = year, y = total_assets), color = "black") +
    geom_line(data = lm_forecast, aes(x = year, y = forecast), color = "blue", size = 1, linetype = "dashed") +
    geom_ribbon(data = lm_forecast, aes(x = year, ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
    labs(
      title = "Forecast of Pension Assets Using Linear Model",
      x = "Year",
      y = "Total Assets (£)"
    ) +
    theme_minimal(base_size = 14) +
    scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
    # set the x-axis breaks to integers(sets years on x axis to whole numbers)
    scale_x_continuous(breaks = scales::breaks_pretty())

#forcast for membership
# Fit the model
model <- lm(total_members ~ year, data = memebers_per_year)

# Predictions for future years
futuremembers_years <- data.frame(year = 2026:2030)
future_years$predicted_schemes <- predict(model, newdata = future_years)

# Combine historical and forecast data
combined_data <- rbind(# Plot
 schemes_per_year %>% mutate(type = "Actual"),
  future_years %>% rename(num_schemes = predicted_schemes) %>% mutate(type = "Forecast")
)

# Plot
ggplot(combined_data, aes(x = year, y = num_schemes, color = type)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(
    title = "Forecast of Trust-Based Pension Schemes",
    x = "Year",
    y = "Number of Schemes"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  # set the x-axis breaks to integers(sets years on x axis to whole numbers)
  scale_x_continuous(breaks = scales::breaks_pretty())





