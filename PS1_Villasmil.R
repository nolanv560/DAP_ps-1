# Data Skills 2 - R
## Fall Quarter 2024

## Homework 1
## Due: October 12, 2024 before midnight on Gradescope

# Question 1 (40%): The two datasets included in the assignment repo are downloaded directly from the BEA.  The file labeled "total" has the total employment per state for the years 2000 and 2017.  The file labeled "by industry" has employment per industry in each of 10 industries per state for the same years.

# Load and merge the data into a panel dataframe, with the columns: "state", "year", and one for each of the 10 industries.  Every state-year combination should uniquely identify a row.  No more and no less than 12 columns should remain.  Do any necessary cleaning for the data to be easily usable.

# The values should be given as the share of the total employment in that place and time, e.g. if total employment in a place and time was 100, and the employment in one industry was 10, then the value shown for that state-year industry should be 0.1.  The "total" values should not be a part of the final dataframe.  

# Output this dataframe to a csv document named "data.csv" and sync it to your homework repo with your code.

## Load library
library(tidyverse)

## Set working directory
setwd("C:/Users/nolan/Documents/GitHub/problem-set-1-nolanv560/")

## Total employment dataset
total_employment <- read.csv("SAEMP25N total.csv", skip = 4)

## Industry share of employment dataset
industry_employment <- read.csv("SAEMP25N by industry.csv", skip = 4)

## Transforming industry share of employment dataset
industry_employment <- industry_employment |> pivot_wider(
  id_cols = "GeoName",
  names_from = "Description",
  values_from = c("X2000", "X2017")
) |>
  select("GeoName", starts_with(c("X2000_ ", "X2017_ "))) |>
  rename(state = GeoName) |>
  pivot_longer(
    cols = starts_with(c("X2000", "X2017")),
    names_to = "year"
  ) |>
  separate(year, c("year", "industry"), sep = "_ ") |>
  pivot_wider(
    id_cols = c("state", "year"),
    names_from = "industry",
    values_from = "value"
  ) |>
  mutate(
    year = case_when(
      year == "X2000" ~ 2000,
      year == "X2017" ~ 2017
    )) |>
  unnest(c(3:12))

## Transforming total employment dataset
total_employment <- total_employment |> rename(state = GeoName) |>
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year"
  ) |>
  select("state", "year", "value") |>
  mutate(
    year = case_when(
      year == "X2000" ~ 2000,
      year == "X2017" ~ 2017
    )
  )

## Merging both datasets
merged_employment <- left_join(total_employment, industry_employment, by = c("state", "year")) |>
  unnest(c(4:13)) |>
  mutate(across(c(4:13), ~ as.numeric(unlist(.))),
         across(c(4:13), ~ . / value),
         across(c(4:13), ~ round(., 3))) |>
  select(!value) |>
  filter(state != "")

## Deleting blank spaces from industry names
names(merged_employment) <- trimws(names(merged_employment))

# Question 2 (30%): 

# Write a function that finds the top states with the highest state-specific industry shares for an industry in a given year. This function should take as input 3 parameters: the number of states reported in the output, the industry, and the year. It should return a vector or list of states.

find_top_states <- function(n_states, industry_name, year) {
  df = merged_employment |>
    filter(year == year) |>
    select(state, industry_name) |>
    arrange(desc(!!sym(industry_name))) |>
    distinct(state, .keep_all = TRUE) |>
    slice(1:n_states) |>
    pull(state)
  
  return(df)
}

# 2a. Use this function to find the 5 states with the largest within-state manufacturing shares (i.e., manufacturing employment / total employment in that state) in 2000.

## Saving top 5 states by largest manufacturing share in 2000
top_five_states_manufacturing <- find_top_states(5, "Manufacturing", 2000)
print(top_five_states_manufacturing)

# 2b. For each of the 5 states that you just identified, find the state's manufacturing share in each year from 2000 and 2017.

find_employment_shares <- function(state_name, industry_name, year) {
  df = merged_employment |>
    filter(state %in% state_name) |>
    select(state, year, industry_name)
}

## Saving top 5 states by largest manufacturing share in 2000 and 2017
top_five_states_manufacturing_share <- find_employment_shares(top_five_states_manufacturing, "Manufacturing", c(2000, 2017))
print(top_five_states_manufacturing_share)

# 2c. Plot how each of those states' manufacturing employment share changed from 2000 and 2017. 

## Creating function
plot_employment_share <- function(state_name, industry_name) {
  p = merged_employment |>
    select(state, year, industry_name) |>
    filter(state %in% state_name) |>
    ggplot(aes(state, !!sym(industry_name), fill = as.character(year))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste(industry_name, "employment share, 2000-2017"),
         x = "State",
         y = "Share of total employment",
         fill = "Year") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

## Plot
print(plot_employment_share(top_five_states_manufacturing, "Manufacturing"))

# 2d. Place the code for steps 2a-2c within a single run of a for-loop.
# Then, modify the for-loop so that it also repeats steps 2a-2c for the 10 states
# with the largest within-state farming share in 2000, and for the 15 states with
# the largest within-state information share in 2017.

## Setting parameters of function
number_top_states <- c(5, 10, 15)
industries <- c("Manufacturing", "Farm employment", "Information")
years <- c(2000, 2000, 2017)

## Creating function
for(i in seq_along(industries)) {
  industry <- industries[i]
  n <- number_top_states[i]
  y <- years[i]
  
  top_states <- find_top_states(n, industry, y)
  top_employment_shares <- find_employment_shares(top_states, industry)
  plot_top_employment <- plot_employment_share(top_states, industry)
  
  print(top_states) # question 2a
  print(top_employment_shares) # question 2b
  print(plot_top_employment) # question 2c
}

# Question 3 (30%): Write a function that plots where a given state is in the
# distribution of employment shares for a given industry in 2000 and 2017, as 
# well as the distribution of the change in employment shares from 2000 to 2017.
# This function should take as input 2 parameters: the state and the industry.
# For each year as well as for the difference, it should plot a histogram of
# employment shares for that industry, and then include a vertical line for the
# mean of the overall distribution and a vertical line for the value for the 
# given state. It should be clearly denoted which is the mean and which is that
# state's value. Note that since the shares here are already in percent terms, 
# the change is just calculated as the difference in shares.

## Transforming merged employment dataset to create 'difference' variable
merged_employment_rev <- merged_employment |>
  pivot_longer(c(3:12),
               names_to = "industry",
               values_to = "employment_share")

## Calculating difference of shares in industry employment
merged_employment_rev <- merged_employment_rev |>
  group_by(state, industry) |>
  arrange(industry) |>
  mutate(difference = employment_share - lag(employment_share)) |>
  ungroup()

## Helper function to obtain industry employment share for a state in a given year
get_employment_share <- function(state_name, industry_name, year_name) {
  df = merged_employment_rev |>
    filter(state == state_name, industry == industry_name, year == year_name) |>
    pull(employment_share)
}

## Testing helper function
print(get_employment_share("Illinois", "Manufacturing", 2000))

## Helper function to obtain industry employment share plots for each year
get_year_plot <- function(state_name, industry_name, year_name) {
  by_year <- merged_employment_rev |>
    filter(year == year_name, industry %in% industry_name)
  
  mean_industry <- mean(by_year$employment_share, na.rm = TRUE)
  emp_share <- get_employment_share(state_name, industry_name, year_name)
  
  plot_year_name = merged_employment_rev |>
    filter(year == year_name, industry %in% industry_name) |>
    group_by(industry) |>
    ggplot() +
    geom_histogram(aes(employment_share), bins = 20, color = "black") +
    geom_vline(aes(xintercept = emp_share, color = state_name), lwd = 1.5) +
    geom_vline(aes(xintercept = mean_industry, color = "Mean"), lwd = 1.5) +
    labs(title = paste(industry_name, "employment shares in states for", year_name),
         x = "Share of total employment",
         y = "Frequency",
         color = "Legend")
}

## Testing helper function
print(get_year_plot("Illinois", "Manufacturing", 2000))

## Helper function to obtain plot of differences in shares
get_difference_plot <- function(state_name, industry_name) {
  by_industry <- merged_employment_rev |>
    filter(industry %in% industry_name)
  
  mean_diff <- mean(by_industry$difference, na.rm = TRUE)
  state_diff <- merged_employment_rev |>
    filter(year == 2017, industry %in% industry_name, state %in% state_name)|>
    pull(difference)
  
  plot_difference <- ggplot(by_industry) +
    geom_histogram(aes(difference), bins = 20, color = "black") +
    geom_vline(aes(xintercept = state_diff, color = state_name), lwd = 1.5) +
    geom_vline(aes(xintercept = mean_diff, color = "Mean"), lwd = 1.5) +
    labs(title = paste("Change in", industry_name, "employment shares in states, 2000-2017"),
         x = "Difference in employment shares",
         y = "Frequency",
         color = "Legend")
}

## Creating function for obtaining all required employment plots (shares by year and differences in year)
get_employment_plots <- function(state_name, industry_name){
  print(get_year_plot(state_name, industry_name, 2000))
  print(get_year_plot(state_name, industry_name, 2017))
  print(get_difference_plot(state_name, industry_name))
}

## Example
get_employment_plots("Illinois","Manufacturing")