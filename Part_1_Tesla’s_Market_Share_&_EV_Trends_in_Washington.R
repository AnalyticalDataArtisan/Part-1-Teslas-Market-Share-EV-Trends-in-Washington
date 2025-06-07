# LOAD NECESSARY PACKAGES

# dplyr: for data manipulation
# tidyr: for reshaping and tidying data
# ggplot2: for data visualization
# scales: for formatting plot labels
# stringr: for string manipulation
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)


# READ IN DATASET

# Load dataset containing variables for EV Population
df <- read.csv("C:/Users/tonyh/OneDrive/Documents/Analytical Data Artisan/Analyst Builder/R/Practice Datasets/Tesla/Electric_Vehicle_Population_Data_04_17_2025.csv")

# Data Cleaning
# Rename columns by replacing . with _ so it doesn't confuse R and take Base.MSRP from character structure to numeric
clean_df <- df |>
  rename_with(~ gsub("\\.","_", .)) |>
  mutate(
    Base_MSRP = as.numeric(Base_MSRP),
    Tesla = ifelse(Make == "TESLA", "TESLA", "OTHER")
  )


# What percentage of EVs in Washington State are Teslas? - tesla/all vehicles *100
total_vehicles <- as.numeric(nrow(clean_df))
tesla_vehicles <- as.numeric(nrow(filter(clean_df, Tesla == "TESLA")))

tesla_market_share <- round(tesla_vehicles/total_vehicles * 100, 1)

# Top Tesla Models Selling in Washington State?
make_model_sold <- clean_df |> 
  filter(Tesla == "TESLA") |>
  group_by(Make,Model) |>
  summarise(count_sold = n(), .groups = "drop") |>
  arrange(desc(count_sold))

# MSRP by year
msrp_by_year <- clean_df |>
  filter(Base_MSRP > 0) |>
  filter(Tesla == "TESLA") |>
  group_by(Model_Year, Make, Model) |>
  summarise(Min_MSRP = min(Base_MSRP, na.rm = TRUE)) |>
  arrange(Model_Year, Make, Model)

# There weren't good base_msrp data in this dataset. Supplementing with online data and will default to lowest base price.

# READ IN SUPPLEMENTAL DATASET

# Load dataset containing variables for EV Population
df_tesla_prices <- read.csv("C:/Users/tonyh/OneDrive/Documents/Analytical Data Artisan/Analyst Builder/R/Practice Datasets/Tesla/Tesla_Current_Base_Prices.csv")

# Transform model names to uppercase to ensure matching works during the join
df_tesla_prices <- df_tesla_prices |>
  mutate(
    Model = toupper(Model)
  )

# Joined data frames and perform aggregation
top_tesla_model_sales <- make_model_sold |>
  left_join(df_tesla_prices, by = "Model") |>
  mutate(
    # Ensure both columns are numeric before multiplying to avoid coercion issues
    Estimated_Revenue = as.numeric(count_sold) * as.numeric(Base_Price_USD)
  )


# Filter to default lowest base price per model to provide a final estimate of lifetime sales as of April 2025
top_tesla_models_priced <- top_tesla_model_sales |>
  filter(!is.na(Estimated_Revenue)) |>
  group_by(Model) |>
  slice_min(Base_Price_USD) |>
  ungroup()


# Tesla vs competitors - Average Range
# MSRP data wasn't 100% provided in the dataset and may skew results
avg_range_for_evs <- clean_df |>
  filter(Electric_Vehicle_Type == "Battery Electric Vehicle (BEV)",
         Electric_Range != 0,
         Base_MSRP != 0) |>
  group_by(Tesla) |>
  summarise(AVG_Range = mean(Electric_Range),
            AVG_MSRP = mean(Base_MSRP))


# Tesla vs competitors - Median Range
# MSRP data wasn't 100% provided in the dataset and may skew results
median_range_for_evs <- clean_df |>
  filter(Electric_Vehicle_Type == "Battery Electric Vehicle (BEV)",
         Electric_Range != 0,
         Base_MSRP != 0) |>
  group_by(Tesla) |>
  summarise(Median_Range = median(Electric_Range),
            Median_MSRP = median(Base_MSRP))


# Plug-in Hybrid Electric (PHEV) and Battery Electric Vehicle (BEV) Trends
phev_vs_bev <- clean_df |>
  group_by(Model_Year, Electric_Vehicle_Type) |>
  summarise(Count = n(), .groups = "drop")


# Electric Utilities in Washington State for Tesla
utility_count_for_teslas <- clean_df |>
  filter(Tesla == "TESLA") |>
  group_by(Electric_Utility) |>
  summarise(Count = n(), .groups = "drop") |>
  arrange(desc(Count))


# Top 5 Electric Utilities in Washington State for Tesla
top_5_utility_count_for_teslas <- clean_df |>
  filter(Tesla == "TESLA") |>
  group_by(Electric_Utility) |>
  summarise(Count = n(), .groups = "drop") |>
  arrange(desc(Count)) |>
  head(5)