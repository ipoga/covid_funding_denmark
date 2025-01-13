# 1. Data
source("settings.R")

data <- read_delim("data/founder_data_combind_cutoff_save_181122.csv", delim = ",")

data_cleaned <- read_delim("data/grants_data_final.csv", delim = ",")

# 2. Cleaning data set for coding

# 2.1 Keep data within scope

data_in_scope <- data %>%
  mutate(across(c(amount_applied, amount_received), ~ str_remove_all(.x, "\\."))) %>% 
  mutate(across(c(amount_applied, amount_received), ~ as.numeric(str_replace_all(.x, ",", ".")))) %>% 
  filter(amount_received >= 500000) %>%  # Ensure projects have budgets equal to or over 500.000 DKK and were funded (8,385 -> 1,034)
  filter(grant_canceled %in% c(NA, 0)) %>% # Exclude canceled grants (1,034 -> 1,033)
  filter(str_detect(grant_accepted, "Withdraw") %in% c(NA, FALSE))  # Exclude withdrawn grants (1,033 -> 1,017)


# 3. Data for combining in database

project_table <- data_cleaned %>%
  mutate(research_area_specific = NA,
         amount_applied = NA, 
         grant_canceled = NA,
         education = NA,
         date_of_call = NA,
         application_deadline = NA,
         grant_accepted = NA,
         institute = NA,
         age = NA,
         award_date = NA,
         data_source = "Data cleaned by Emil and Christine") %>% 
  select(id, data_source, pi_id, pi, co_pi, email, email_clean, pi_affiliation, pi_affiliation_clean, institute, gender, gender_category, age, education, career_stage, career_stage_clean, career_stage_category, country_of_origin, funder_id, funder, grant_programme,
         title, summary, award_application_date, start_date, end_date, date_of_call, application_deadline, grant_accepted, award_date, research_area, research_area_specific, project_link_0 = `project_link/0`, project_link_1 = `project_link/1`, amount_awarded, amount_applied, covid_related)

data_in_scope <- data_in_scope %>%
  rename(amount_awarded = amount_received) %>% 
  mutate(data_source = "Combined filed from webscrabing and foundation contacts")

project_table <- bind_rows(project_table, data_in_scope) %>% 
  mutate(across(where(is.character), ~ str_replace_all(.x, "[\r\n]", " "))) %>% 
  select(-`...1`)


pi_table <- project_table %>% 
  select(1:18)

project_table_no_pi_info <- project_table %>% 
  select(1:3, 19:39)

# 4. Export

export(project_table, "data/database/project_table_full_info.tsv", format = "\t")
export(project_table_no_pi_info, "data/database/project_table.tsv", format = "\t")
export(pi_table, "data/database/pi_table.tsv", format = "\t")

covid_relation <- project_table %>% select(id, data_source, covid_related)

export(covid_relation, "data/database/covid_table.tsv", format = "\t")