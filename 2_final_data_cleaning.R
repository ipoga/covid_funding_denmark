#-----------------------------------#
# COVID-Funding                     #
# Final data cleaning               #
# 20. sep. 2023                     #
#-----------------------------------#

source("settings.R")

# 1. Data ----

data <- read_tsv("data/covid_funding_cleaned_data_sept2023.tsv")

data_2005_2017 <- import("data/grant_dk_2005.csv", encoding = "UTF-8")

# 2. Categorising -----

## 2.1 Gender ----

data <- data %>% 
  mutate(gender_category = case_when(gender == "m" ~ "Men",
                                     gender == "f" ~ "Women",
                                     TRUE ~ "Other"))

## 2.2 Titles ----

missing <- data %>% filter(is.na(career_stage_clean))

data <- data %>% 
  mutate(career_stage_clean = case_when(# Missing data
    grant_id == 60 ~ "assistant professor",
    grant_id == 690 ~ "senior researcher",
    grant_id == 695 ~ "phd student",
    grant_id == 925 ~ "phd student",
    grant_id == 1245 ~ "assistant professor",
    grant_id == 1509 ~ "senior researcher",
    grant_id == 1761 ~ "development consultant",
    grant_id == 1763 ~ "head of secretariat",
    grant_id == 1764 ~ "project manager",
    grant_id == 1844 ~ "senior researcher",
    grant_id == 1857 ~ "senior researcher",
    grant_id == 1875 ~ "head of research",
    TRUE ~ career_stage_clean),
    career_stage_category = case_when(career_stage_clean == "postdoc" ~ "Postdoc",
                                      career_stage_clean %in% c("associate professor", "clinical associate professor") ~ "Associate Professor",
                                      career_stage_clean == "senior researcher" ~ "Associate Professor",
                                      career_stage_clean %in% c("assistant professor", "researcher", "clinical researcher") ~ "Assistant Professor",
                                      career_stage_clean %in% c("emeritus professor", "professor emeritus", 
                                                                "professor mso", "professor", "clinical professor", "head doctor") ~ "Professor",
                                      career_stage_clean == "phd student" ~ "PhD Student",
                                      career_stage_clean %in% c("adjunct researcher", "clinical psychologist with research responsibilities",
                                                                "engineer", "part-time lecturer", "permanent academic staff",
                                                                "physicist") ~ "Other academic personnel",
                                      career_stage_clean %in% c("senior advisor", "senior consultant",  "editor", "project manager",
                                                                "staff specialist", "development consultant") ~ "Technical/administrative staff",
                                      career_stage_clean %in% c("management", "dean", "provost", "head of department", 
                                                                "head of research", "chief operating officer", "chief consultant", "head of secretariat",
                                                                "research leader", "project leader", "research co-ordinator") ~ "Management",
                                      career_stage_clean %in% c("medical doctor", "md", "md, phd") ~ "Medical Doctor",
                                      TRUE ~ career_stage_clean
    ),
    career_stage_dst = case_when(career_stage_category %in% c("Assistant Professor", "Postdoc") ~ "Assistant Professor/Postdoc",
                                 career_stage_category %in% c("Management", "Technical/administrative staff") ~ "Non-academic personnel",
                                 career_stage_category %in% c("Medical Doctor", "Other academic personnel") ~ "Other academic personnel",
                                 TRUE ~ career_stage_category)
  )



# 3. Data for analysis ----

export(data, "data/grants_data_final_2023.csv")

# 4. Historical data ----

data_2008_2016 <- data_2005_2017 %>% 
  filter(amount_granted >= 500000 & between(init_year, 2008, 2016)) %>%
  select(id, person_id, title, init_year, full_name, last_name, first_name, institution_name, funder_name, type, gender_new, amount_sought, amount_granted) %>% 
  mutate(gender_category = ifelse(gender_new == 1, "Men", "Women"),
         year_cat = case_when(between(init_year, 2008, 2010) ~ "2008-2010",
                              between(init_year, 2011, 2013) ~ "2011-2013",
                              between(init_year, 2014, 2016) ~ "2014-2016"))

export(data_2008_2016, "data/grants_data_2008_2016.csv")
