# ==============================
# 1. LIBRARY UPLOAD
# ==============================

library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)

# ==============================
# 2. DATA LOADING
# ==============================

Mock_dataset <- read_csv("C:/Users/01444607/Desktop/Mock dataset.csv")
View(Mock_dataset)

# rename dataset
dataset <- `Mock_dataset`

# check if output data exists
if(!dir.exists("outputs")) dir.create("outputs")

# ==============================
# 3. DATA CLEANING / STANDARDISATION
# ==============================

# Convert characters into numeric
numeric_cols <- c("age_of_respondent", 
                  "number_of_hours_worked_last_week",
                  "number_of_hours_usually_work_a_week")

dataset <- dataset %>%
  mutate(across(all_of(numeric_cols), ~ as.numeric(.)))  # non-numeric -> NA

# Identify non-numeric values before coercion
non_numeric_flags <- dataset %>%
  select(all_of(numeric_cols)) %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(cols = -row_id,
               names_to = "column",
               values_to = "value") %>%
  filter(!is.na(value) & !str_detect(value, "^\\d+(\\.\\d+)?$"))

write_csv(non_numeric_flags, "outputs/non_numeric_values_flagged.csv")

dataset <- dataset %>%
  mutate(across(all_of(numeric_cols), ~ as.numeric(.)))  # non-numeric -> NA

# Standardise categorical columns (Gender)
if("respondents_sex" %in% colnames(dataset)) {
  dataset <- dataset %>%
    mutate(respondents_sex = str_to_title(trimws(respondents_sex))) %>%
    mutate(respondents_sex = case_when(
      respondents_sex %in% c("M", "Male") ~ "Male",
      respondents_sex %in% c("F", "Female") ~ "Female",
      respondents_sex %in% c("Other") ~ "Other",
      TRUE ~ NA_character_
    ))
}

# Standardise categorical columns (marital_status)
if("marital_status" %in% colnames(dataset))

# Standardise dates
if("survey_date" %in% colnames(dataset)) {
  dataset <- dataset %>%
    mutate(survey_date = as.Date(survey_date, format = "%Y-%m-%d"))
}

# Standardise text fields
text_cols <- c("occupation_code", "specific_denomination")  # add more if needed
dataset <- dataset %>%
  mutate(across(all_of(text_cols), ~ str_trim(.))) %>%
  mutate(across(all_of(text_cols), ~ str_replace_all(., "[^a-zA-Z0-9 ]", "")))

# Standardise ID fields
if("respondent_id_number" %in% colnames(dataset)) {
  dataset <- dataset %>%
    mutate(respondent_id_number = str_trim(as.character(respondent_id_number)))
}

# ==============================
# 4. DATA QUALITY CHECKS
# ==============================

# Check missing values per column
missing_summary <- dataset %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
  mutate(percent_missing = round((n_missing / nrow(dataset)) * 100, 2))
write_csv(missing_summary, "outputs/missing_summary.csv")

# Check rows with any missing values
missing_rows <- dataset %>% filter(if_any(everything(), is.na))
write_csv(missing_rows, "outputs/missing_rows.csv")

# Check duplicates (using participant ID)
duplicate_rows <- dataset %>%
  group_by(respondent_id_number) %>%
  filter(n() > 1) %>%
  ungroup()
write_csv(duplicate_rows, "outputs/duplicate_rows.csv")

# Check for Impossible / inconsistent values
inconsistent_rows <- dataset %>%
  filter(
    age_of_respondent < 0 | age_of_respondent > 120 |
      number_of_hours_worked_last_week < 0 | number_of_hours_worked_last_week > 168 |
      number_of_hours_usually_work_a_week < 0 | number_of_hours_usually_work_a_week > 168
  )
write_csv(inconsistent_rows, "outputs/inconsistent_rows.csv")

# Check date mismatched
if("survey_date" %in% colnames(dataset)) {
  date_issues <- dataset %>%
    filter(is.na(survey_date) | survey_date > Sys.Date())
  write_csv(date_issues, "outputs/date_issues.csv")
}

# Data Completeness Scores
# Check column completeness
column_completeness <- dataset %>%
  summarise(across(everything(), ~ mean(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "completeness_percent") %>%
  mutate(completeness_percent = round(completeness_percent * 100, 2))
write_csv(column_completeness, "outputs/column_completeness.csv")

# Check row completeness
row_completeness <- dataset %>%
  mutate(completeness_percent = round(rowMeans(!is.na(.)) * 100, 2)) %>%
  select(respondent_id_number, completeness_percent)
write_csv(row_completeness, "outputs/row_completeness.csv")

# Low completeness (<80%)
low_completeness <- row_completeness %>% filter(completeness_percent < 80)
write_csv(low_completeness, "outputs/low_completeness_rows.csv")

# Combine all issues into one data quality dashboard
all_issues <- bind_rows(
  missing_rows %>% mutate(issue = "Missing value"),
  inconsistent_rows %>% mutate(issue = "Inconsistent/Impossible value"),
  duplicate_rows %>% mutate(issue = "Duplicate"),
  if(exists("date_issues")) date_issues %>% mutate(issue = "Date issue") else NULL,
  low_completeness %>% mutate(issue = "Low completeness")
)

write_csv(all_issues, "outputs/data_quality_dashboard.csv")
cat("Data quality dashboard saved to 'outputs/data_quality_dashboard.csv'\n")

# Save final standardised dataset
write_csv(dataset, "outputs/Mock_dataset_standardised.csv")
cat("Final standardised dataset saved to 'outputs/Mock_dataset_standardised.csv'\n")

# Preview in R
head(dataset)
head(all_issues)
head(column_completeness)
head(row_completeness)

# ==============================
# 5. DATA VISUALISATION
# ==============================

# --- 5.1 Column Completeness (%) ---
column_completeness <- dataset %>%
  summarise(across(everything(), ~ mean(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "completeness") %>%
  mutate(completeness = completeness * 100)

ggplot(column_completeness, aes(x = reorder(column, completeness), y = completeness)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Column Completeness (%)", x = "Columns", y = "Completeness (%)") +
  theme_minimal()

# --- 5.2 Row Completeness Distribution ---
row_completeness <- dataset %>%
  mutate(completeness_percent = rowMeans(!is.na(.)) * 100)

ggplot(row_completeness, aes(x = completeness_percent)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Row Completeness", x = "Completeness (%)", y = "Number of Respondents") +
  theme_minimal()

# --- 5.3 Numeric Variable Distributions ---
numeric_cols <- dataset %>% select(where(is.numeric)) %>% colnames()

for(col in numeric_cols){
  p <- ggplot(dataset, aes_string(x = col)) +
    geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
    labs(title = paste("Distribution of", col), x = col, y = "Count") +
    theme_minimal()
  print(p)
}

# --- 5.4 Categorical Variable Frequencies ---
categorical_cols <- dataset %>% select(where(~ !is.numeric(.))) %>% colnames()

for(col in categorical_cols){
  freq_df <- dataset %>%
    group_by(across(all_of(col))) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(!is.na(.data[[col]]))
  
  p <- ggplot(freq_df, aes_string(x = col, y = "count")) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Frequency of", col), x = col, y = "Count") +
    theme_minimal()
  
  print(p)
}

# --- 5.5 Optional: Save Plots to Outputs Folder ---
if(!dir.exists("outputs")) dir.create("outputs")

# Save column completeness
ggsave("outputs/column_completeness.png", width = 8, height = 6)

# Save row completeness
ggsave("outputs/row_completeness_distribution.png", width = 8, height = 6)

# Save numeric distributions
for(col in numeric_cols){
  p <- ggplot(dataset, aes_string(x = col)) +
    geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
    labs(title = paste("Distribution of", col), x = col, y = "Count") +
    theme_minimal()
  ggsave(paste0("outputs/distribution_", col, ".png"), plot = p, width = 8, height = 6)
}

# Save categorical distributions
for(col in categorical_cols){
  freq_df <- dataset %>%
    group_by(across(all_of(col))) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(!is.na(.data[[col]]))
  
  p <- ggplot(freq_df, aes_string(x = col, y = "count")) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Frequency of", col), x = col, y = "Count") +
    theme_minimal()
  
  ggsave(paste0("outputs/frequency_", col, ".png"), plot = p, width = 8, height = 6)
}

cat("All visualizations generated and saved in 'outputs/' folder.\n")

