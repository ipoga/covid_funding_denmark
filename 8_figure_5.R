source("3_figure_init.R")

rates <- import("data/success_rates.xlsx") %>% 
  mutate(success_rate = case_when(funder == "danmarks frie forskningsfond" ~ 13,
                                  TRUE ~ success_rate))

# 1. Funding composition by funder -----

df_total <- grants %>% 
  group_by(funder) %>% 
  summarise(total_grants = n(),
            pct_pi_women = mean(gender == "f"))

df <- grants %>% 
  group_by(funder, gender_category) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            sum_award = sum(amount_awarded)) %>% 
  ungroup() %>% 
  group_by(funder) %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_pi = n_pi/sum(n_pi),
         pct_award = sum_award/sum(sum_award)) %>% 
  pivot_longer(cols = - c(funder, gender_category), names_to = "measure", values_to = "pct") %>% 
  pivot_wider(id_cols = c(funder, measure), names_from = gender_category, values_from = pct, values_fill = 0) %>% 
  filter(measure %in% c("pct_grants", "pct_pi", "pct_award")) %>% 
  left_join(df_total, by = "funder") %>% 
  mutate(funder = case_when(funder == "novo nordisk" ~ "Novo Nordisk Foundation",
                            funder == "gigtforeningen" ~ "Danish Rheumatism Association",
                            funder == "region midt" ~ "Central Denmark Region",
                            funder == "region hovedstaden" ~ "Capital Region of Denmark",
                            funder == "velux fonden" ~ "Velux Foundation",
                            funder == "danish cancer society" ~ "Danish Cancer Society",
                            funder == "helsefonden" ~ "The Health Foundation",
                            funder == "hjerteforeningen" ~ "Danish Heart Association",
                            funder == "carlsbergfondet" ~ "Carlsberg Foundation",
                            funder == "lundbeckfonden" ~ "Lundbeck Foundation",
                            funder == "danmarks frie forskningsfond" ~ "Independent Research Fund Denmark",
                            funder == "villum fonden" ~ "Villum Foundation",
                            funder == "danish national research foundation" ~ "Danish National Research Foundation",
                            funder == "poul due jensen foundation" ~ "Poul Due Jensen Foundation"))


df2 <- df %>% 
  select(-Men, -Other) %>%
  pivot_wider(names_from = measure, values_from = Women) %>%
  mutate(funder = reorder(funder, pct_pi_women))

fig_5_d <- df2 %>%
  ggplot(aes(x = total_grants, y = reorder(funder, pct_pi_women))) + 
  geom_col(fill = "gray40") + 
  geom_text(aes(label = paste0(total_grants)),
            hjust = -0.1,
            size = 7 * text_scaler) +
  labs(x = "Total grants", y = "") +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(0,0,0,0)) +
  scale_x_continuous(limits = c(0, 550), 
                     expand = c(0,0),)

fig_5_e <- df2 %>% 
  ggplot(aes(x = pct_pi, y = reorder(funder, pct_pi_women))) + 
  geom_col(fill = color_women) + 
  geom_text(aes(label = paste0(round(pct_pi * 100, 1), "%")),
            hjust = -0.1,
            size = 7 * text_scaler) +
  labs(x = "No. of grantees", y = "") +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(0,0,0,0),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(0, 1), 
                     expand = c(0,0), 
                     labels = scales::percent_format())

fig_5_f <- df2 %>% 
  ggplot(aes(x = pct_grants, y = reorder(funder, pct_pi_women))) + 
  geom_col(fill = color_women) + 
  geom_text(aes(label = paste0(round(pct_grants * 100, 1), "%")),
            hjust = -0.1,
            size = 7 * text_scaler) +
  labs(x = "No. of grants", y = "") +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(0,0,0,0),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(0, 1), 
                     expand = c(0,0), 
                     labels = scales::percent_format())

fig_5_g <- df2 %>% 
  ggplot(aes(x = pct_award, y = reorder(funder, pct_pi_women))) + 
  geom_col(fill = color_women) + 
  geom_text(aes(label = paste0(round(pct_award * 100, 1), "%")),
            hjust = -0.1,
            size = 7 * text_scaler) +
  labs(x = "Grant amounts", y = "") +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(0,5,0,0),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(0, 1), 
                     expand = c(0,0), 
                     labels = scales::percent_format())

# 2. Success rates

rates <- rates %>% 
  mutate(funder = case_when(funder == "novo nordisk" ~ "Novo Nordisk Foundation",
                            funder == "gigtforeningen" ~ "Danish Rheumatism Association",
                            funder == "region midt" ~ "Central Denmark Region",
                            funder == "region hovedstaden" ~ "Capital Region of Denmark",
                            funder == "velux fonden" ~ "Velux Foundation",
                            funder == "danish cancer society" ~ "Danish Cancer Society",
                            funder == "helsefonden" ~ "The Health Foundation",
                            funder == "hjerteforeningen" ~ "Danish Heart Association",
                            funder == "carlsbergfondet" ~ "Carlsberg Foundation",
                            funder == "lundbeckfonden" ~ "Lundbeck Foundation",
                            funder == "danmarks frie forskningsfond" ~ "Independent Research Fund Denmark",
                            funder == "villum fonden" ~ "Villum Foundation",
                            funder == "danish national research foundation" ~ "Danish National Research Foundation",
                            funder == "innovation fund denmark: explorer" ~ "Innovation Fund Denmark: Explorer grants",
                            funder == "innovation fund denmark: overall" ~ "Innovation Fund Denmark: Overall",
                            funder == "danish ministry for science and technology" ~ "Danish Ministry for Science and Technology", 
                            TRUE ~ funder),
         grant_data = case_when(funder %in% df$funder ~ "Present in grant data",
                                TRUE ~ "Not present in grant data"),
         shader = case_when(applications < 100 ~ 1, .default = 0))

rates <- rates %>%
  mutate(women_likelihood = (success_rate_women - success_rate_men) / success_rate_men * 100,
         funder = reorder(funder, women_likelihood))

rates <- rates %>%
  mutate(
    success_rate = success_rate / 100,
    women_likelihood = women_likelihood / 100
  )

fig_5_a <- rates %>% 
  ggplot(aes(x = success_rate, y = funder)) +
  geom_col(aes(fill = grant_data)) +
  geom_text(aes(label = paste0(round(success_rate * 100, 1), "%")),
            hjust = -0.1,
            size = 7 * text_scaler) +
  scale_fill_manual(values = c("gray80", "gray40")) +
  labs(x = "Overall success rate", y = "") +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.margin = margin(0,5,5,0)) +
  scale_x_continuous(limits = c(0, 1), 
                     expand = c(0,0), 
                     labels = scales::percent_format())

fig_5_b <- rates %>% 
  ggplot(aes(x = applications, y = funder)) +
  geom_col(aes(fill = grant_data)) +
  geom_text(aes(label = applications),
            hjust = -0.1,
            size = 7 * text_scaler) +
  scale_fill_manual(values = c("gray80", "gray40")) +
  labs(x = "N applications", y = "") +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "aliceblue", color = "white"),
        plot.margin = margin(0,5,5,0)) + 
  scale_x_continuous(limits = c(0, 5000), 
                     expand = c(0,0))

fig_5_c <- rates %>%
  mutate(label_position = ifelse(women_likelihood <= 0, 0, women_likelihood)) %>% # Precompute label positions
  ggplot(aes(x = women_likelihood, y = funder)) +
  geom_col(aes(fill = grant_data)) +
  geom_text(aes(x = label_position, # Use the precomputed position
                label = paste0(round(women_likelihood * 100, 1), "%")),
            hjust = -0.1, # Adjust alignment (still consistent)
            size = 7 * text_scaler) +
  scale_fill_manual(values = c("gray80", "gray40")) +
  labs(x = "Differential success likelihood", y = "") +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0,5,5,0)) +
  scale_x_continuous(limits = c(-1, 1), 
                     expand = c(0,0), 
                     labels = scales::percent_format())


layout <- "
AAAABBBCCCCCC
DDDDEEEFFFGGG
"

fig_5 <- fig_5_a + fig_5_b + fig_5_c + fig_5_d + fig_5_e + fig_5_f + fig_5_g + plot_layout(design = layout) + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold", size = 10))

ggsave("figures/fig_5.tiff", plot = fig_5, width = 8, height = 7, units = "in",  compression = "lzw", dpi = 300)
ggsave("figures/fig_5.pdf", plot = fig_5, width = 8, height = 7, units = "in", device = cairo_pdf)



