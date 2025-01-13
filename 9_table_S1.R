## 1.1 Gender differences ----

# Table of descriptives, table S1

source("3_figure_init.R")

table <- grants %>% 
  group_by(gender_category) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded, na.rm = TRUE),
            median_award = median(amount_awarded, na.rm = TRUE),
            sum_award = sum(amount_awarded, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_award = sum_award/sum(sum_award))

total <- grants %>% 
  summarise(gender_category = "Total",
            n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded, na.rm = TRUE),
            median_award = median(amount_awarded, na.rm = TRUE),
            sum_award = sum(amount_awarded, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_award = sum_award/sum(sum_award))


table <- rbind(table, total)

table <- table %>% 
  mutate(pct_grants = paste0(round(pct_grants, 4) * 100, " %"),
         pct_award = paste0(round(pct_award, 4) * 100, " %")
  ) 

export(table, "tables/table_S1.xlsx")