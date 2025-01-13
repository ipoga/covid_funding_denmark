source("3_figure_init.R")

# 1. Figure 1 : Gender and career stage differences in funding ----

fig_1_a <- grants %>% 
  group_by(gender_category) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded),
            sum_award = sum(amount_awarded)) %>% 
  ungroup() %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_pi = n_pi/sum(n_pi),
         pct_award = sum_award/sum(sum_award)) %>% 
  select(gender_category, pct_grants, pct_pi, pct_award) %>% 
  pivot_longer(cols = -gender_category, names_to = "measure", values_to = "pct") %>% 
  mutate(measure = case_when(measure == "pct_grants" ~ "No. of grants",
                             measure == "pct_pi" ~ "No. of grantees",
                             TRUE ~ "Grant amounts")) %>% 
  mutate(measure = factor(measure, levels = c("Grant amounts", "No. of grants", "No. of grantees")),
         gender_category = factor(gender_category, levels = c("Other", "Men", "Women"))) %>% 
  ggplot(aes(x = pct, y = measure, fill = gender_category)) +
  
  geom_col(colour = "white",
           linewidth = 1,
           width = 0.75) +
  geom_text(aes(label = ifelse(gender_category %in% c("Women", "Men"), paste0(round(pct, 3)*100, "%"), "")), 
            position = position_stack(vjust = 0.5),
            size = 7 * text_scaler) +
  geom_text(aes(label = ifelse(gender_category == "Other", paste0(round(pct, 3)*100, "%"), ""),
                x = 1.05), 
            #position = position_stack(vjust = 0),
            size = 7 * text_scaler,
            fontface = "bold",
            colour = colour_scale_desat[1]) +
  geom_text(data = . %>% filter(measure == "No. of grantees"), 
            aes(label = gender_category,
                y = 3.55,
                colour = gender_category), 
            fontface = "bold", 
            size = 7 * text_scaler,
            position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "", fill = "") +
  scale_fill_manual(values = c(color_other, color_men, color_women)) +
  scale_colour_manual(values = c(color_other, color_men, color_women)) +
  scale_x_continuous(expand = c(0, 0), labels = scales::percent_format()) +
  coord_cartesian(clip = "off", xlim = c(0, 1)) +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "none")

## 1.2 Career stage differences ----

sorting <- c("Technical/admin. staff", "Management", "Medical Doctor", 
             "PhD Student", "Other academic personnel", "Postdoc", 
             "Assistant Professor", "Associate Professor", "Professor")

df <- grants %>% 
  group_by(career_display) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded),
            sum_award = sum(amount_awarded)) %>% 
  ungroup() %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_pi = n_pi/sum(n_pi),
         pct_award = sum_award/sum(sum_award),
         ratio = sum_award/n_pi
  ) %>% 
  select(career_display, pct_grants, pct_pi, pct_award, ratio) %>% 
  pivot_longer(cols = -career_display, names_to = "measure", values_to = "pct") %>% 
  mutate(measure = case_when(measure == "pct_grants" ~ "No. of grants",
                             measure == "pct_pi" ~ "No. of grantees",
                             measure == "ratio" ~ "Avg. grant amount",
                             TRUE ~ "Grant amounts")) %>% 
  mutate(measure = factor(measure, levels = c("No. of grantees", "No. of grants", 
                                              "Grant amounts", "Avg. grant amount")
  )
  )

fig_1_b <- df %>% 
  
  ggplot(aes(x = pct, y = career_display)) +
  geom_col(fill = "gray40") +
  geom_text(data = filter(df, measure != "Avg. grant amount"),
            aes(label = paste0(round(pct, 3)*100, "%")), hjust = -0.1,
            size = 7 * text_scaler) +
  geom_text(data = filter(df, measure == "Avg. grant amount"),
            aes(label = paste0(round(pct/1000000, 1), "m")), hjust = -0.1,
            size = 7 * text_scaler) +
  labs(x = "", y = "") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 0.8),
                     breaks = seq(0, 0.75, 0.25),
                     labels = scales::percent_format()) +
  facet_wrap_custom(~ measure, ncol = 4, scales = "free_x",
                    scale_overrides = list(
                      scale_override(4, scale_x_continuous(expand = c(0, 0), 
                                                           limits = c(0, 20000000),
                                                           breaks = seq(0, 20000000, 5000000),
                                                           labels = c("0", "5m", "10m", "15m", "20m"))
                      )
                    )
  )  +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"),
        axis.ticks.y = element_blank())


## 1.3 Gender x career stage differences ----


fig_1_c <- grants %>% 
  group_by(gender_category, career_display) %>% 
  summarise(n = n_distinct(pi_id),
            n_grants = n_distinct(grant_id),
            amount = sum(amount_awarded)) %>% 
  group_by(career_display) %>% 
  mutate(pct_pi = (n/sum(n)) * 100,
         pct_grants = (n_grants/sum(n_grants)) * 100,
         pct_amount = (amount/sum(amount)) *100
  ) %>% 
  filter(gender_category == "Women") %>%
  select(gender_category, career_display, pct_pi:pct_amount) %>%
  pivot_longer(cols = - c("gender_category", "career_display"),
               names_to = "measure",
               values_to = "pct") %>% 
  mutate(measure = case_when(measure == "pct_grants" ~ "No. of\ngrants",
                             measure == "pct_pi" ~ "No. of\ngrantees",
                             TRUE ~ "Grant\namounts")) %>% 
  mutate(measure = factor(measure, levels = c("Grant\namounts", "No. of\ngrants", "No. of\ngrantees"))) %>% 
  ggplot(aes(x = career_display, y = pct)) +
  geom_bar(stat = "identity") + 
  coord_flip() + 
  scale_y_continuous("", limits = c(0,100)) +
  scale_x_discrete("") + 
  facet_wrap(~ measure, ncol = 3, scales = "free_x")  +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")), hjust = -0.1,
    size = 7 * text_scaler) +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"),
        axis.ticks.y = element_blank())

layout <- "
AABBBB
AABBBB
CCCCCC
CCCCCC
CCCCCC
"

fig_1 <- fig_1_a + fig_1_c + fig_1_b + 
  plot_layout(design = layout) + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(face = "bold", size = 10),
        plot.margin = margin(t = 0.15, r = 0.15, b = 0.15, l = 0.15, unit = "cm"))

ggsave("figures/fig_1.tiff", plot = fig_1, width = 7, height = 8, units = "in",  compression = "lzw", dpi = 300)
ggsave("figures/fig_1.pdf", plot = fig_1, width = 7, height = 8, units = "in", device = cairo_pdf)

