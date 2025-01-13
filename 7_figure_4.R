# 4. Gender and career differences across covid and non-covid funding ----

## 4.1 Gender differences -----

fig_4_a <- grants %>%
  mutate(covid_related = ifelse(covid_related > 0, "Covid related", "Not Covid related")) %>%
  group_by(gender_category, covid_related) %>%   
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded),
            sum_award = sum(amount_awarded)) %>% 
  group_by(covid_related) %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_pi = n_pi/sum(n_pi),
         pct_award = sum_award/sum(sum_award)) %>% 
  select(gender_category, covid_related, pct_grants, pct_pi, pct_award) %>% 
  pivot_longer(cols = pct_grants:pct_award, names_to = "measure", values_to = "pct") %>% 
  mutate(measure = case_when(measure == "pct_grants" ~ "No. of grants",
                             measure == "pct_pi" ~ "No. of grantees",
                             TRUE ~ "Grant amounts")) %>% 
  mutate(measure = factor(measure, levels = c("Grant amounts", "No. of grants", "No. of grantees")),
         gender_category = factor(gender_category, levels = c("Other", "Men", "Women"))) %>% 
  
  ggplot(aes(x = pct, y = measure, fill = gender_category)) +
  geom_col(colour = "white",
           linewidth = 1,
           width = 0.75) +
  facet_wrap(~ covid_related) +
  geom_text(aes(label = ifelse(gender_category %in% c("Women", "Men"), paste0(round(pct, 3)*100, "%"), "")), 
            position = position_stack(vjust = 0.5),
            size = 7 * text_scaler) +
  geom_text(aes(label = ifelse(gender_category == "Other", paste0(round(pct, 3)*100, "%"), ""),
                x = 1.075), 
            #position = position_stack(vjust = 0),
            size = 7 * text_scaler,
            fontface = "bold",
            colour = colour_scale_desat[1]) +
  labs(x = "", y = "", fill = "") +
  scale_fill_manual(values = c(color_other, color_men, color_women)) +
  scale_colour_manual(values = c(color_other, color_men, color_women)) +
  scale_x_continuous(expand = c(0, 0), labels = scales::percent_format()) +
  coord_cartesian(clip = "off", xlim = c(0, 1)) +
  theme_ebm_hbar(base_size = 8) +
  theme(legend.position = "bottom",
        #plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(vjust = 5, margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"))
  )

## 4.2 Career stage differences ----

ann_arrow <- tibble(measure = factor("No. of grantees"), 
                    career_stage_category = factor("Assistant Professor"),
                    label = "Covid\nrelated")

fig_4_b <- grants %>% 
  mutate(covid_related = ifelse(covid_related > 0, "Covid related", "Not Covid related")) %>%
  group_by(career_display, covid_related) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded),
            sum_award = sum(amount_awarded)) %>% 
  group_by(covid_related) %>% 
  
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_pi = n_pi/sum(n_pi),
         pct_award = sum_award/sum(sum_award),
         ratio = sum_award/n_pi) %>% 
  select(career_display, pct_grants, pct_pi, pct_award, ratio) %>% 
  pivot_longer(cols = - c(career_display, covid_related), names_to = "measure", values_to = "pct") %>% 
  mutate(measure = case_when(measure == "pct_grants" ~ "No. of grants",
                             measure == "pct_pi" ~ "No. of grantees",
                             measure == "ratio" ~ "Avg. grant amounts",
                             TRUE ~ "Grant amounts")) %>% 
  mutate(measure = factor(measure, levels = c("No. of grantees", "No. of grants", 
                                              "Grant amounts", "Avg. grant amounts")),
         covid_related = factor(covid_related)
  ) %>% 
  ungroup() %>%
  complete(career_display, covid_related, measure, fill = list(pct = 0)) %>%
  filter(!is.na(measure)) %>%
  ggplot(aes(x = pct, y = career_display)) +
  geom_col(position = "dodge", aes(fill = covid_related)) + 
  facet_wrap_custom(~ measure, ncol = 4, scales = "free_x",
                    scale_overrides = list(
                      scale_override(4, scale_x_continuous(expand = c(0, 0), 
                                                           limits = c(0, 21000000),
                                                           breaks = seq(0, 20000000, 5000000),
                                                           labels = c("0", "5M", "10M", "15M", "20M"))
                      )
                    )
  ) + 
  geom_text(data = . %>% filter(measure != "Avg. grant amounts" & covid_related == "Covid related"),
            aes(label = paste0(round(pct, 3)*100, "%")),
            size = 7 * text_scaler, 
            hjust = -0.2,
            vjust = 1.6) +
  geom_text(data = . %>% filter(measure != "Avg. grant amounts" & covid_related != "Covid related"),
            aes(label = paste0(round(pct, 3)*100, "%")),
            hjust = -0.2,
            size = 7 * text_scaler,
            vjust = -.6) +
  geom_text(data = . %>% filter(measure == "Avg. grant amounts" & covid_related == "Covid related"),
            aes(label = paste0(round(pct/1000000, 1), "M")),
            hjust = -0.2,
            size = 7 * text_scaler,
            vjust = 1.6) +
  geom_text(data = . %>% filter(measure == "Avg. grant amounts" & covid_related != "Covid related"),
            aes(label = paste0(round(pct/1000000, 1), "M")),
            hjust = -0.2,
            size = 7 * text_scaler,
            vjust = -.6) +
  labs(x = "", y = "", fill = "") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.25),
                     labels = scales::percent_format()) +
  scale_fill_manual(values = c(color_covid, color_noncovid)) +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 8),
        panel.spacing = unit(2, "lines"),
        axis.ticks.y = element_blank())

layout <- "
AAAA
BBBB
BBBB
"


fig_4 <- fig_4_a +  fig_4_b + plot_annotation(tag_levels = "A") + plot_layout(design = layout) & 
  theme(plot.tag = element_text(face = "bold", size = 10),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "lines"))

ggsave("figures/fig_4.tiff", plot = fig_3, width = 7, height = 8, units = "in",  compression = "lzw", dpi = 300)
ggsave("figures/fig_4.pdf", plot = fig_3, width = 7, height = 8, units = "in", device = cairo_pdf)
