source("3_figure_init.R")

# 1. Permutation functions -----

## 1.1 Diff. between men and women ----

# Grant level
perm_test <- function(data, n_under, n_over, permutations){
  
  # Empty data frame to hold permutations
  results <- data.frame(mean_over = rep(NA, permutations),
                        mean_under = rep(NA, permutations))
  
  # Draw sample for each permutation for an under- and overrepresented group
  for (i in 1:nrow(results)) {
    
    over_rep <- sample(x = data$amount_awarded, size = n_over)
    under_rep <- sample(x = data$amount_awarded, size = n_under)
    
    # Compute prob. of being funded for each group
    results$mean_over[i] <- mean(over_rep, na.rm = TRUE)
    results$mean_under[i] <- mean(under_rep, na.rm = TRUE)
    results$median_over[i] <- median(over_rep, na.rm = TRUE)
    results$median_under[i] <- median(under_rep, na.rm = TRUE)
    
  }
  
  
  return(results)
  
}


# 2. Results ------

## 2.1 Diff. between men and women ----

data <- grants %>% 
  filter(funder %in% c("carlsbergfondet", "danish cancer society", 
                       "danish national research foundation", "lundbeckfonden", 
                       "velux fonden", "danmarks frie forskningsfond", "novo nordisk", "villum fonden")) %>% 
  group_by(gender_category) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded),
            sum_award = sum(amount_awarded)) %>% 
  ungroup() %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_pi = n_pi/sum(n_pi),
         pct_award = sum_award/sum(sum_award),
         year_cat = "2020-2021",
         group = "Total")

data_old <- grants_2008_2016 %>% 
  group_by(gender_category, year_cat) %>% 
  filter(funder_name %in% c("carlsbergfondet", "kræftens bekæmpelse", 
                            "danmarks grundforskningsfond", "lundbeck fonden", 
                            "velux fonden", "det frie forskningsråd", "novo nordisk", 
                            "villum kann rasmussen fonden", "velux fonden & villum kann rasmussen fonden")) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded),
            sum_award = sum(amount_awarded)) %>% 
  group_by(year_cat) %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_pi = n_pi/sum(n_pi),
         pct_award = sum_award/sum(sum_award),
         group = "Total")

grants_fte_gender <- bind_rows(data_old, data) %>% 
  left_join(population_gender, by = c("gender_category", "year_cat", "group"))

set.seed(666)

df <- perm_test(data = grants, n_under = 587, n_over = 1080, permutations = 10000)

real_diff <- grants %>% 
  filter(gender_category %in% c("Men", "Women")) %>% 
  group_by(gender_category) %>% 
  summarise(mean = mean(amount_awarded),
            median = median(amount_awarded)) %>% 
  arrange(mean) %>% 
  ungroup() %>% 
  summarise(diff_mean = diff(mean),
            diff_median = diff(median)) %>% 
  pivot_longer(everything(), names_to = "measure", values_to = "value")

df <- df %>% 
  mutate(diff_mean = mean_over - mean_under,
         diff_median = median_over - median_under,
         id = 1:n()) %>% 
  pivot_longer(-id, names_to = "measure", values_to = "value") %>% 
  mutate(comparison = "Grant level")

# Coverage

1 - mean(abs(df$value[df$measure == "diff_mean"]) >= abs(real_diff[[1, 2]])) # 0.0057 (two-sided)
1 - mean(df$value[df$measure == "diff_mean"] >= real_diff[[1, 2]]) # 0.0013 (one-sided)

1 - mean(abs(df$value[df$measure == "diff_median"]) >= abs(real_diff[[2, 2]])) # 0.000 (two-sided)
1 - mean(df$value[df$measure == "diff_median"] >= real_diff[[2, 2]]) # 0.000 (one-sided)

fig_2_a <- df %>% 
  filter(measure %in% c("diff_median")) %>%
  ggplot(aes(x = value, y = measure)) +
  stat_histinterval(.width = 0, 
                    point_size = 0,
                    breaks = 30,
                    outline_bars = TRUE,
                    slab_colour = "white",
                    slab_size = 0.2) +
  geom_vline(xintercept = real_diff[[2, 2]], 
             colour = colour_scale[2]) +
  geom_vline(xintercept = 0) +
  annotate(geom = "text", 
           x = real_diff[[2, 2]] - 100000, 
           y = 1.2, 
           label = ">100 %",
           size = 7 * text_scaler,
           colour = colour_scale[2]) +
  
  annotate(geom = "text", 
           x = real_diff[[2, 2]] + 100000, 
           y = 1.2, 
           label = real_diff[[2, 2]],
           size = 7 * text_scaler,
           colour = colour_scale[2]) +
  scale_x_continuous(limits = c(-800000, 800000),
                     breaks = seq(-750000, 750000, 250000),
                     expand = c(0,0),
                     labels = c("-0.75M", "-0.5M", "-0.25M", "0", "0.25M", "0.5M", "0.75M")) +
  scale_y_discrete(labels = NULL,
                   expand = c(0, 0)) +
  labs(x = "Difference in funding amount\n[Men - Women]",
       y = "",
       title = "Grant level") +
  theme_ebm_hbar(base_size = 7) +
  theme(axis.text.y = element_text(hjust = 0.5),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "lines"),
        legend.position = "none")

# Means

fig_S2_a <- df %>% 
  filter(measure %in% c("diff_mean")) %>%
  ggplot(aes(x = value, y = measure)) +
  stat_histinterval(.width = 0, 
                    point_size = 0,
                    breaks = 30,
                    outline_bars = TRUE,
                    slab_colour = "white",
                    slab_size = 0.2) +
  geom_vline(xintercept = real_diff[[1, 2]], 
             colour = colour_scale[2]) +
  geom_vline(xintercept = 0) +
  annotate(geom = "text", 
           x = real_diff[[1, 2]] - 250000, 
           y = 1.5, 
           label = ">99.87 %",
           size = 6 * text_scaler,
           colour = colour_scale[2]) +
  annotate(geom = "text", 
           x = real_diff[[1, 2]] + 250000, 
           y = 1.5, 
           label = round(real_diff[[1, 2]], 1),
           size = 6 * text_scaler,
           colour = colour_scale[2]) +
  scale_x_continuous(limits = c(-2000000, 2000000),
                     breaks = seq(-2000000, 2000000, 500000),
                     expand = c(0,0),
                     labels = c("-2M", "-1.5M", "-1M", "-0.5M", "0", "0.5M", "1M", "1.5M", "2M")) +
  scale_y_discrete(labels = NULL,
                   expand = c(0, 0)) +
  labs(x = "Difference in funding amount\n[Men - Women]",
       y = "",
       title = "Grant level") +
  theme_ebm_hbar(base_size = 8) +
  theme(axis.text.y = element_text(hjust = 0.5),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "lines"))


## 2.2 PI-level gender differences ----

set.seed(666)

data_pi <- grants %>% 
  group_by(pi_id, gender_category) %>% 
  summarise(amount_awarded = sum(amount_awarded)) %>% 
  ungroup()

df <- perm_test(data = data_pi, n_under = 514, n_over = 894, permutations = 10000)

real_diff <- data_pi %>% 
  filter(gender_category %in% c("Men", "Women")) %>% 
  group_by(gender_category) %>% 
  summarise(mean = mean(amount_awarded),
            median = median(amount_awarded)) %>% 
  arrange(mean) %>% 
  ungroup() %>% 
  summarise(diff_mean = diff(mean),
            diff_median = diff(median)) %>% 
  pivot_longer(everything(), names_to = "measure", values_to = "value")

df <- df %>% 
  mutate(diff_mean = mean_over - mean_under,
         diff_median = median_over - median_under,
         id = 1:n()) %>% 
  pivot_longer(-id, names_to = "measure", values_to = "value") %>% 
  mutate(comparison = "Principle investigator level")

# Coverage
1 - mean(abs(df$value[df$measure == "diff_mean"]) >= abs(real_diff[[1, 2]])) # 0.0014 (two-sided)
1 - mean(df$value[df$measure == "diff_mean"] >= real_diff[[1, 2]]) # 0.0004 (one-sided)

1 - mean(abs(df$value[df$measure == "diff_median"]) >= abs(real_diff[[2, 2]])) # 0.000 (two-sided)
1 - mean(df$value[df$measure == "diff_median"] >= real_diff[[2, 2]]) # 0.000 (one-sided)

fig_2_b <- df %>% 
  filter(measure %in% c("diff_median")) %>%
  ggplot(aes(x = value, y = measure)) +
  stat_histinterval(.width = 0, 
                    point_size = 0,
                    breaks = 30,
                    outline_bars = TRUE,
                    slab_colour = "white",
                    slab_size = 0.2) +
  geom_vline(xintercept = real_diff[[2, 2]], 
             colour = colour_scale[2]) +
  geom_vline(xintercept = 0) +
  annotate(geom = "text", 
           x = real_diff[[2, 2]] - 100000, 
           y = 1.2, 
           label = ">100 %",
           size = 7 * text_scaler,
           colour = colour_scale[2]) +
  
  annotate(geom = "text", 
           x = real_diff[[2, 2]] + 100000, 
           y = 1.2, 
           label = real_diff[[2, 2]],
           size = 7 * text_scaler,
           colour = colour_scale[2]) +
  scale_x_continuous(limits = c(-800000, 800000),
                     breaks = seq(-750000, 750000, 250000),
                     expand = c(0,0),
                     labels = c("-0.75M", "-0.5M", "-0.25M", "0", "0.25M", "0.5M", "0.75M")) +
  scale_y_discrete(labels = NULL,
                   expand = c(0, 0)) +
  labs(x = "Difference in cumulative funding amount\n[Men - Women]",
       y = "",
       title = "Principal investigator level") +
  theme_ebm_hbar(base_size = 7) +
  theme(axis.text.y = element_blank(),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "lines"))

# Means

fig_S2_b <- df %>% 
  filter(measure %in% c("diff_mean")) %>%
  ggplot(aes(x = value, y = measure)) +
  stat_histinterval(.width = 0, 
                    point_size = 0,
                    breaks = 30,
                    outline_bars = TRUE,
                    slab_colour = "white",
                    slab_size = 0.2) +
  geom_vline(xintercept = real_diff[[1, 2]], 
             colour = colour_scale[2]) +
  geom_vline(xintercept = 0) +
  annotate(geom = "text", 
           x = real_diff[[1, 2]] - 350000, 
           y = 1.5, 
           label = ">99.87 %",
           size = 6 * text_scaler,
           colour = colour_scale[2]) +
  annotate(geom = "text", 
           x = real_diff[[1, 2]] + 350000, 
           y = 1.5, 
           label = round(real_diff[[1, 2]], 1),
           size = 6 * text_scaler,
           colour = colour_scale[2]) +
  scale_x_continuous(limits = c(-2000000, 3000000),
                     breaks = seq(-2000000, 3000000, 500000),
                     expand = c(0,0),
                     labels = c("-2M", "-1.5M", "-1M", "-0.5M", "0", "0.5M", "1M", "1.5M", "2M", "2.5M", "3M")) +
  scale_y_discrete(labels = NULL,
                   expand = c(0, 0)) +
  labs(x = "Difference in cumulative funding amount\n[Men - Women]",
       y = "",
       title = "Principal investigator level") +
  theme_ebm_hbar(base_size = 8) +
  theme(axis.text.y = element_blank(),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "lines"))

fig_2_c <- grants_fte_gender %>% 
  select(gender_category, year_cat, group, contains("pct")) %>% 
  pivot_longer(cols = pct_grants:fte_pct, names_to = "measure", values_to = "pct") %>% 
  mutate(measure = case_when(measure == "pct_grants" ~ "No. of grants",
                             measure == "pct_pi" ~ "No. of grantees",
                             measure == "fte_pct" ~ "Full-time equivalents",
                             TRUE ~ "Grant amounts")) %>% 
  mutate(measure = factor(measure, levels = c("Grant amounts", "No. of grants", "No. of grantees", "Full-time equivalents")),
         gender_category = factor(gender_category, levels = c("Other", "Men", "Women")),
         year_group = case_when(year_cat == "2020-2021" ~ 1, .default = 0),
         year_cat = factor(year_cat, levels = c("2008-2010","2011-2013","2014-2016","2020-2021"), labels = c("'08-'10","'11-'13","'14-'16","'20-'21"))
  ) %>% 
  ggplot(aes(x = year_cat, y = pct, fill = gender_category)) + 
  geom_col(colour = "white",
           linewidth = 1, aes(alpha = as.factor(year_group))) +
  facet_wrap(~ measure, ncol = 4) +
  geom_text(aes(label = ifelse(gender_category %in% c("Men", "Women"), paste0(round(pct, 3)*100, "%"), "")), 
            position = position_stack(vjust = 0.5),
            size = 7 * text_scaler) +
  labs(x = "", y = "", fill = "") +
  #caption = "Proportion of men, women, or grantees identifying otherwise across the total grant amount, number of grants, number of\ngrantees, and the full-time equivalent number of researchers at Danish institutions of higher education and hospitals") +
  scale_fill_manual(values = c(color_other, color_men, color_women)) +
  scale_x_discrete() +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent_format()) +
  scale_alpha_manual(values = c("0" = .5, "1" = 1), guide = "none") +
  theme_ebm_bar(base_size = 8) +
  theme(axis.text.x = element_text(face = c("plain","plain","plain", "bold")),
        legend.position = "bottom", 
        legend.justification = "left",
        legend.text = element_text(size = 7))

# Compose figures
layout <- "
AABB
CCCC
CCCC
"

fig_2 <- fig_2_a + fig_2_b + fig_2_c + plot_layout(design = layout) + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold", size = 10))

ggsave("figures/fig_2.tiff", fig_2, width = 7, height = 6, units = "in", compression = "lzw", dpi = 300)
ggsave("figures/fig_2.pdf", fig_2, width = 7, height = 6, units = "in", device = cairo_pdf)

fig_S2 <- fig_S2_a + fig_S2_b + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold"))

ggsave("figures/fig_S2.tiff", fig_S2, width = 8, height = 3, units = "in", compression = "lzw", dpi = 300)
ggsave("figures/fig_S2.pdf", fig_S2, width = 8, height = 3, units = "in", device = cairo_pdf)


# 3. Group gender differences -----

## 3.1 Grant level ----

set.seed(666)

grants_nested <- grants %>% 
  mutate(women_dummy = ifelse(gender_category == "Women", 1, 0),
         men_dummy = ifelse(gender_category == "Men", 1, 0)) %>% 
  group_by(career_stage_dst) %>% 
  nest() %>% 
  mutate(n = map_int(data, nrow),
         n_women = map_dbl(.x = data, ~ sum(.x$women_dummy)),
         n_men = map_dbl(.x = data, ~ sum(.x$men_dummy)),
         permutations = map(.x = data, ~ perm_test(data = .x,
                                                   n_under = n_women,
                                                   n_over = n_men,
                                                   permutations = 10000))) %>% 
  select(career_stage_dst, permutations) %>% 
  unnest(permutations) %>% 
  mutate(diff_mean = mean_over - mean_under,
         diff_median = median_over - median_under,
         id = 1:n()) %>% 
  pivot_longer(mean_over:diff_median, names_to = "measure", values_to = "value")


real_diff <- grants %>% 
  filter(gender_category %in% c("Men", "Women")) %>% 
  group_by(gender_category, career_stage_dst) %>% 
  summarise(mean = mean(amount_awarded),
            median = median(amount_awarded)) %>% 
  arrange(career_stage_dst, desc(gender_category)) %>% 
  group_by(career_stage_dst) %>% 
  summarise(diff_mean = diff(mean),
            diff_median = diff(median)) %>% 
  mutate(career_stage_dst = factor(career_stage_dst, levels = c("Non-academic personnel", "PhD Student", 
                                                                "Other academic personnel", "Assistant Professor/Postdoc",
                                                                "Associate Professor", "Professor"))) %>% 
  pivot_longer(- career_stage_dst, names_to = "measure", values_to = "value")


# Coverage

grants_nested <- grants_nested %>% 
  filter(measure %in% c("diff_mean", "diff_median")) %>% 
  mutate(career_stage_dst = factor(career_stage_dst, levels = c("Non-academic personnel", "PhD Student", 
                                                                "Other academic personnel", "Assistant Professor/Postdoc",
                                                                "Associate Professor", "Professor"))) %>% 
  left_join(real_diff, by = c("career_stage_dst", "measure")) %>% 
  rename(value_perm = value.x, value_real = value.y) %>% 
  mutate(larger = ifelse(value_real >= value_perm, TRUE, FALSE),
         smaller = ifelse(value_real < value_perm, TRUE, FALSE),
         absolute = ifelse(abs(value_real) >= abs(value_perm), TRUE, FALSE))


coverage <- grants_nested %>% 
  group_by(career_stage_dst, measure) %>% 
  summarise(larger = mean(larger),
            smaller = mean(smaller),
            absolute = mean(absolute))

# Plot

label <- c(`diff_mean` = "Mean difference",
           `diff_median` = "Median difference")

fig_3_a <- grants_nested %>% 
  mutate(career_stage_dst = factor(career_stage_dst, levels = c("Non-academic personnel", "PhD Student", 
                                                                "Other academic personnel", "Assistant Professor/Postdoc",
                                                                "Associate Professor", "Professor"))) %>% 
  filter(measure %in% c("diff_mean", "diff_median")) %>%
  sample_n(200) %>% 
  ggplot(aes(x = value_perm, y = career_stage_dst)) +
  geom_vline(xintercept = 0) +
  geom_quasirandom(groupOnX = FALSE,
                   dodge.width = 0.9, 
                   varwidth = TRUE,
                   alpha = 0.3,
                   size = 1) + 
  geom_point(data = real_diff %>% filter(measure %in% c("diff_mean", "diff_median")),
             aes(x = value, colour = measure), 
             position = position_dodge(width = 0.9),
             shape = "|", 
             size = 6) +
  geom_text(data = coverage, 
            aes(x = 2500000,
                label = paste0(round(larger * 100, 3), " %"),
                colour = measure),
            size = 8 * text_scaler) +
  facet_wrap(~ measure,
             labeller = as_labeller(label)) +
  scale_x_continuous(#limits = c(-3000000, 3000000),
    breaks = seq(-3000000, 3000000, 1000000),
    expand = c(0,0),
    labels = c("-3M", "-2M", "-1M", "0","1M", "2M", "3M")) +
  coord_cartesian(xlim = c(-3000000, 3000000)) +
  scale_colour_manual(values = c(colour_scale[2],colour_scale[2])) +
  labs(x = "Difference in funding amount\n[Men - Women]",
       y = "",
       title = "Grant level") +
  theme_ebm_hbar(base_size = 8) +
  theme(legend.position = "none",
        panel.spacing.x = unit(2, "lines"))

## 3.2 PI level ----

set.seed(666)

grants_nested <- grants %>% 
  group_by(pi_id, gender_category, career_stage_dst) %>% 
  summarise(amount_awarded = sum(amount_awarded)) %>% # Cumulative amounts
  mutate(women_dummy = ifelse(gender_category == "Women", 1, 0),
         men_dummy = ifelse(gender_category == "Men", 1, 0)) %>% 
  group_by(career_stage_dst) %>% 
  nest() %>% 
  mutate(n = map_int(data, nrow),
         n_women = map_dbl(.x = data, ~ sum(.x$women_dummy)),
         n_men = map_dbl(.x = data, ~ sum(.x$men_dummy)),
         permutations = map(.x = data, ~ perm_test(data = .x,
                                                   n_under = n_women,
                                                   n_over = n_men,
                                                   permutations = 10000))) %>% 
  select(career_stage_dst, permutations) %>% 
  unnest(permutations) %>% 
  mutate(diff_mean = mean_over - mean_under,
         diff_median = median_over - median_under,
         id = 1:n()) %>% 
  pivot_longer(mean_over:diff_median, names_to = "measure", values_to = "value")

real_diff <- grants %>% 
  group_by(pi_id, gender_category, career_stage_dst) %>% 
  summarise(amount_awarded = sum(amount_awarded)) %>%
  filter(gender_category %in% c("Men", "Women")) %>% 
  group_by(gender_category, career_stage_dst) %>% 
  summarise(mean = mean(amount_awarded),
            median = median(amount_awarded)) %>% 
  arrange(career_stage_dst, desc(gender_category)) %>% 
  group_by(career_stage_dst) %>% 
  summarise(diff_mean = diff(mean),
            diff_median = diff(median)) %>% 
  pivot_longer(-career_stage_dst, names_to = "measure", values_to = "value") %>% 
  mutate(career_stage_dst = factor(career_stage_dst, levels = c("Non-academic personnel", "PhD Student", 
                                                                "Other academic personnel", "Assistant Professor/Postdoc",
                                                                "Associate Professor", "Professor")))

# Coverage

grants_nested <- grants_nested %>% 
  filter(measure %in% c("diff_mean", "diff_median")) %>% 
  mutate(career_stage_dst = factor(career_stage_dst, levels = c("Non-academic personnel", "PhD Student", 
                                                                "Other academic personnel", "Assistant Professor/Postdoc",
                                                                "Associate Professor", "Professor"))) %>% 
  left_join(real_diff, by = c("career_stage_dst", "measure")) %>% 
  rename(value_perm = value.x, value_real = value.y) %>% 
  mutate(larger = ifelse(value_real >= value_perm, TRUE, FALSE),
         smaller = ifelse(value_real < value_perm, TRUE, FALSE),
         absolute = ifelse(abs(value_real) >= abs(value_perm), TRUE, FALSE))


coverage <- grants_nested %>% 
  group_by(career_stage_dst, measure) %>% 
  summarise(larger = mean(larger),
            smaller = mean(smaller),
            absolute = mean(absolute))

# Plot

label <- c(`diff_mean` = "Mean difference",
           `diff_median` = "Median difference")

fig_3_b <- grants_nested %>% 
  mutate(career_stage_dst = factor(career_stage_dst, levels = c("Non-academic personnel", "PhD Student", 
                                                                "Other academic personnel", "Assistant Professor/Postdoc",
                                                                "Associate Professor", "Professor"))) %>% 
  filter(measure %in% c("diff_mean", "diff_median")) %>%
  sample_n(200) %>% 
  ggplot(aes(x = value_perm, y = career_stage_dst)) +
  geom_vline(xintercept = 0) +
  geom_quasirandom(groupOnX = FALSE,
                   dodge.width = 0.9, 
                   varwidth = TRUE,
                   alpha = 0.3,
                   size = 1) + 
  geom_point(data = real_diff %>% filter(measure %in% c("diff_mean", "diff_median")),
             aes(x = value, colour = measure), 
             position = position_dodge(width = 0.9),
             shape = "|", 
             size = 6) +
  geom_text(data = coverage, 
            aes(x = 2500000,
                label = paste0(round(larger * 100, 3), " %"),
                colour = measure),
            size = 8 * text_scaler) +
  facet_wrap(~ measure,
             labeller = as_labeller(label)) +
  scale_x_continuous(#limits = c(-3000000, 3000000),
    breaks = seq(-3000000, 3000000, 1000000),
    expand = c(0,0),
    labels = c("-3M", "-2M", "-1M", "0","1M", "2M", "3M")) +
  scale_colour_manual(values = c(colour_scale[2],colour_scale[2])) +
  coord_cartesian(xlim = c(-3000000, 3000000)) +
  labs(x = "Difference in funding amount\n[Men - Women]",
       y = "",
       title = "Principal investigator level") +
  theme_ebm_hbar(base_size = 8) +
  theme(legend.position = "none",
        panel.spacing.x = unit(2, "lines"))


fig_3 <- fig_3_a / fig_3_b + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold"))

ggsave("figures/fig_3.tiff", fig_3, width = 8, height = 8, units = "in", compression = "lzw", dpi = 300)
ggsave("figures/fig_3.pdf", fig_3, width = 8, height = 8, units = "in", device = cairo_pdf)

# 4. Gender differences among professors -----

prof_men <- grants %>% 
  filter(career_stage_dst == "Professor" & gender_category =="Men") %>% 
  group_by(gender_category) %>% 
  arrange(desc(amount_awarded)) %>% 
  slice_head(n = 181) # Equal to number of women professors

prof <- grants %>% 
  filter(career_stage_dst == "Professor" & gender_category =="Women") %>% 
  bind_rows(prof_men)

## 4.1 Grant level difference ----

set.seed(666)

df_grant <- perm_test(data = prof, n_under = 181, n_over = 181, permutations = 10000)

real_diff_grant <- prof %>% 
  group_by(gender_category) %>% 
  summarise(mean = mean(amount_awarded),
            median = median(amount_awarded)) %>% 
  arrange(mean) %>% 
  ungroup() %>% 
  summarise(diff_mean = diff(mean),
            diff_median = diff(median)) %>% 
  pivot_longer(everything(), names_to = "measure", values_to = "value") %>% 
  mutate(comparison = "Grant level")

df_grant <- df_grant %>% 
  mutate(diff_mean = mean_over - mean_under,
         diff_median = median_over - median_under,
         id = 1:n()) %>% 
  pivot_longer(-id, names_to = "measure", values_to = "value") %>% 
  mutate(comparison = "Grant level")

# Coverage

1 - mean(abs(df_grant$value[df_grant$measure == "diff_mean"]) >= abs(real_diff_grant[[1, 2]])) # 0.000 (two-sided)
1 - mean(df_grant$value[df_grant$measure == "diff_mean"] >= real_diff_grant[[1, 2]]) # 0.000 (one-sided)

1 - mean(abs(df_grant$value[df_grant$measure == "diff_median"]) >= abs(real_diff_grant[[2, 2]])) # 0.000 (two-sided)
1 - mean(df_grant$value[df_grant$measure == "diff_median"] >= real_diff_grant[[2, 2]]) # 0.000 (one-sided)

## 4.2 PI level difference ----

prof_men_pi <- grants %>% 
  filter(career_stage_dst == "Professor" & gender_category =="Men") %>%
  group_by(pi_id, gender_category) %>% 
  summarise(amount_awarded = sum(amount_awarded)) %>% 
  arrange(desc(amount_awarded)) %>% 
  slice_head(n = 181)

prof_pi <- grants %>% 
  filter(career_stage_dst == "Professor" & gender_category =="Women") %>%
  group_by(pi_id, gender_category) %>% 
  summarise(amount_awarded = sum(amount_awarded)) %>%
  bind_rows(prof_men_pi)

set.seed(666)

df_pi <- perm_test(data = prof_pi, n_under = 181, n_over = 181, permutations = 10000)

real_diff_pi <- prof_pi %>% 
  group_by(gender_category) %>% 
  summarise(mean = mean(amount_awarded),
            median = median(amount_awarded)) %>% 
  arrange(mean) %>% 
  ungroup() %>% 
  summarise(diff_mean = diff(mean),
            diff_median = diff(median)) %>% 
  pivot_longer(everything(), names_to = "measure", values_to = "value") %>% 
  mutate(comparison = "Principal investigator level")

df_pi <- df_pi %>% 
  mutate(diff_mean = mean_over - mean_under,
         diff_median = median_over - median_under,
         id = 1:n()) %>% 
  pivot_longer(-id, names_to = "measure", values_to = "value") %>% 
  mutate(comparison = "Principal investigator level")

# Coverage

1 - mean(abs(df_pi$value[df_pi$measure == "diff_mean"]) >= abs(real_diff_pi[[1, 2]])) # 0.8528 (two-sided)
1 - mean(df_pi$value[df_pi$measure == "diff_mean"] >= real_diff_pi[[1, 2]]) # 0.4313 (one-sided)

1 - mean(abs(df_pi$value[df_pi$measure == "diff_median"]) >= abs(real_diff_pi[[2, 2]])) # 0.4582 (two-sided)
1 - mean(df_pi$value[df_pi$measure == "diff_median"] >= real_diff_pi[[2, 2]]) # 0.222 (one-sided)


## 4.3 Figure ----

df <- bind_rows(df_grant, df_pi)

real_diff <- bind_rows(real_diff_grant, real_diff_pi)

fig_S3 <- df %>% 
  filter(measure %in% c("diff_median")) %>%
  #sample_n(200) %>% 
  ggplot(aes(x = value, y = comparison)) +
  
  stat_histinterval(.width = 0, 
                    point_size = 0,
                    breaks = 30,
                    outline_bars = TRUE,
                    slab_colour = "white",
                    slab_size = 0.2,
                    normalize = "groups",
                    position = "dodge") +
  geom_vline(xintercept = 0) +
  geom_point(data = real_diff %>% filter(measure == "diff_median"), shape = "|", size = 6, colour = colour_scale[2]) +
  annotate(geom = "text", 
           x = real_diff[[4, 2]] + 1200000, 
           y = 2.2, 
           label = ">77.8 %",
           size = 8 * text_scaler,
           colour = colour_scale[2]) +
  annotate(geom = "text", 
           x = real_diff[[2, 2]], 
           y = 1.2, 
           label = ">100 %",
           size = 8 * text_scaler,
           colour = colour_scale[2]) +
  annotate(geom = "text", 
           x = real_diff[[4, 2]] + 1200000, 
           y = 1.8, 
           label = real_diff[[4, 2]],
           size = 8 * text_scaler,
           colour = colour_scale[2]) +
  annotate(geom = "text", 
           x = real_diff[[2, 2]], 
           y = 0.8, 
           label = real_diff[[2, 2]],
           size = 8 * text_scaler,
           colour = colour_scale[2]) +
  scale_x_continuous(limits = c(-3000000, 8000000),
                     breaks = seq(-3000000, 8000000, 1000000),
                     expand = c(0,0),
                     labels = c("-3M", "-2M", "-1M", "0", "1M", "2M", "3M", "4M", "5M", "6M", "7M", "8M")) +
  #scale_y_discrete(labels = c("Median\ndifference")) +
  labs(x = "Difference in funding amount\n[Men - Women]",
       y = "",
       title = "Top-funded male professors vs. all female professors") +
  theme_ebm_hbar(base_size = 8) +
  theme(axis.text.y = element_text(hjust = 0.5),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "lines"))


ggsave("figures/fig_S3.tiff", fig_S3, width = 5.5, height = 3.5, units = "in", compression = "lzw", dpi = 300)
ggsave("figures/fig_S3.pdf", fig_S3, width = 5.5, height = 3.5, units = "in", device = cairo_pdf)
