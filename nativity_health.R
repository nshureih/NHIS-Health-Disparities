# Load necessary packages
library(tidyverse)   # For data cleaning and visualization
library(janitor)     # For cleaning variable names
library(survey)      # For weighted analysis
library(gtsummary)   # For professional tables

# Load the data
nhis_data <- read_csv("Project/adult23.csv") %>% 
  clean_names() %>%  # Makes all column names consistent
  # Select and rename key variables:
  select(
    id = hhx,
    weight = wtfa_a,
    sex = sex_a,
    age = agep_a,
    citizenship = citznstp_a,
    nativity = natusborn_a,
    years_in_us = yrsinus_a,
    health_insurance = cover_a,
    depression = depev_a,
    anxiety = anxev_a
  )

nhis_clean <- nhis_data %>%
  mutate(
    # Create clear population groups
    population_group = case_when(
      nativity == 1 ~ "U.S.-born",
      citizenship == 2 ~ "Naturalized citizen",
      citizenship == 3 ~ "Non-citizen",
      TRUE ~ "Other"
    ),
    
    # Simplify insurance status
    uninsured = ifelse(health_insurance == 4, 1, 0),
    
    # Create mental health indicator
    mental_health_issue = ifelse(depression == 1 | anxiety == 1, 1, 0),
    
    # Categorize time in U.S. for foreign-born
    years_in_us_group = case_when(
      nativity == 1 ~ "U.S.-born",
      years_in_us < 5 ~ "0-4 years",
      years_in_us < 10 ~ "5-9 years",
      years_in_us >= 10 ~ "10+ years",
      TRUE ~ "Missing"
    )
  ) %>%
  # Remove any missing values
  drop_na(population_group, weight)

#Calculate weighted percentages
insurance_rates <- nhis_clean %>%
  group_by(population_group) %>%
  summarize(
    insured_rate = 1 - weighted.mean(uninsured, weight, na.rm = TRUE),
    .groups = "drop"
  )

# Create visualization
ggplot(insurance_rates, 
       aes(x = population_group, y = insured_rate, fill = population_group)) +
  geom_col() +
  geom_text(aes(label = scales::percent(insured_rate, accuracy = 0.1)), 
            vjust = -0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Health Insurance Coverage Varies by Citizenship Status",
    x = "",
    y = "Percentage Insured",
    caption = "Data: 2023 NHIS"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Filter to foreign-born only
foreign_born <- nhis_clean %>%
  filter(nativity == 2 & years_in_us_group != "Missing")

# Calculate rates
mental_health_rates <- foreign_born %>%
  group_by(years_in_us_group) %>%
  summarize(
    rate = weighted.mean(mental_health_issue, weight),
    .groups = "drop"
  )

# Create plot
ggplot(mental_health_rates, aes(x = years_in_us_group, y = rate)) +
  geom_col(fill = "#4E79A7") +
  geom_text(aes(label = scales::percent(rate, accuracy = 0.1)), vjust = -0.5) +
  labs(
    title = "Mental Health Issues Increase with Time in U.S.",
    subtitle = "Among foreign-born residents",
    x = "Years in United States",
    y = "Percentage with Depression/Anxiety"
  ) +
  theme_minimal()

