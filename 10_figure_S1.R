source("3_figure_init.R")

## Figure S1

survey <- read_tsv("data/funding_survey.txt")
survey_wide <- survey %>% 
  pivot_wider(names_from = "question", values_from = "response") %>%
  mutate(current_gender = case_when(
    gender_current_woman == "yes" ~ "Women",
    gender_current_man == "yes" ~ "Men",
    .default = "Other"
  )) %>% 
  mutate(career_stage_clean = case_when(
    career_stage == "scitap" ~ "other",
    career_stage == "Assistant Prof ved udenlandsk universitet" ~ "assistprof",
    career_stage == "Assistant professor, group leader" ~ "assistprof",
    career_stage == "institutleder" ~ "leadership",
    career_stage == "Bac. scient, projektleder" ~ "other",
    career_stage == "Projektleder for udviklingsprojekt med tilkoblet forskningsprojekt (inkl. PhD studerende)" ~ "other",
    career_stage == "postdoc (phd) med deltids lektorat ved siden af klinisk fuldtidsstilling" ~ "assistprof",
    career_stage == "postdoc" ~ "assistprof",
    .default = career_stage
  ))

ac <- expand_grid(
  current_gender = unique(survey_wide$current_gender),
  career_stage_clean = unique(survey_wide$career_stage_clean)
)

n_total <- nrow(survey_wide)

fig_S1 <- survey_wide %>%
  group_by(current_gender, career_stage_clean) %>%
  summarize(n = n()) %>%
  right_join(ac, by = join_by(current_gender, career_stage_clean)) %>%
  mutate(n = replace_na(n, 0)) %>%
  mutate(career_stage_clean = 
           factor(career_stage_clean, 
                  levels = rev(c("other",
                                 "leadership",
                                 "phdstudent",
                                 "assistprof",
                                 "assocprof",
                                 "prof")),
                  labels = rev(c("Other",
                                 "Leadership",
                                 "PhD Student",
                                 "Assistant professor",
                                 "Associate professor",
                                 "Full professor")))) %>%
  mutate(p = n / n_total) %>%
  ggplot(aes(x = career_stage_clean, y = p, fill = current_gender)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual("Reported current gender", values = c(color_men, color_other, color_women)) +
  scale_x_discrete("Career stage") + 
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, .27),
                     breaks = seq(0, .25, 0.05),
                     labels = scales::percent_format()) +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 8),
        panel.spacing = unit(2, "lines"),
        axis.ticks.y = element_blank())

ggsave("figures/fig_S1.tiff", fig_S1, width = 5, height = 4, units = "in", compression = "lzw", dpi = 300)
ggsave("figures/fig_S1.pdf", fig_S1, width = 5, height = 4, units = "in", device = cairo_pdf)  
