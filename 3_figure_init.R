source("settings.R")

## IF file does not exist, run 1_new_data_cleaning.R and 2_final_data_cleaning.R
grants <- import("data/grants_data_final_2023.csv")

grants <- grants %>% 
  filter(funder != "poul due jensen foundation") %>%
  mutate(career_clustered = case_when(career_stage_category == "Professor" ~ "Late-career",
                                      career_stage_category == "Associate Professor" ~ "Mid-career",
                                      career_stage_category %in% c("Assistant Professor", 
                                                                   "Postdoc", 
                                                                   "PhD Student", 
                                                                   "Medical Doctor") ~ "Early-career",
                                      TRUE ~ "Other")) %>%
  mutate(career_display = factor(career_stage_category, 
                                 levels = c("Technical/administrative staff","Management","Medical Doctor","PhD Student","Other academic personnel","Postdoc","Assistant Professor","Associate Professor","Professor"), 
                                 labels = c("Technical/\nadmin. staff","Management","Medical\nDoctor","Phd Student","Other academic\npersonnel","Postdoc","Assistant\nProfessor","Associate\nProfessor","Professor")))


population_gender <- import("data/dst_gender_fte.csv")

population_career <- import("data/dst_career_fte.csv")

grants_2008_2016 <- import("data/grants_data_2008_2016.csv", encoding = "UTF-8")

grants_2008_2016 <- grants_2008_2016 %>% 
  rename(pi_id = person_id, amount_awarded = amount_granted) %>% 
  filter(institution_name != "foreign")

font_fam <- "Fira Sans" # Set preferred font

ggplot2::update_geom_defaults(geom = "text", list(family = font_fam))
ggplot2::update_geom_defaults(geom = "label", list(family = font_fam))


# Themes for different plot types
theme_ebm <- function(base_size) {
  ggthemes::theme_base(base_size = base_size, base_family = font_fam) %+replace% ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_line(lineend = "square"), 
    axis.ticks.length = ggplot2::unit(0.3, "lines"), 
    axis.text = ggplot2::element_text(size = base_size),
    strip.text = element_text(face = "bold", margin = margin(t = 1, r = 1, b = 2, l = 1, unit = "pt"))
  )
}

theme_ebm_grid <- function(base_size) {
  ggthemes::theme_base(base_size = base_size, base_family = font_fam) %+replace% ggplot2::theme(
    plot.background = ggplot2::element_blank(), 
    axis.ticks = ggplot2::element_line(lineend = "square"), 
    axis.ticks.length = ggplot2::unit(0.3, "lines"), 
    axis.text = ggplot2::element_text(size = base_size),
    panel.grid.major = element_line(colour = "grey85", linetype = "dotted", size = 0.3),
    strip.text = element_text(face = "bold", margin = margin(t = 1, r = 1, b = 2, l = 1, unit = "pt"))
  )
}

theme_ebm_bar <- function(base_size) {
  ggthemes::theme_base(base_size = base_size, base_family = font_fam) %+replace% ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_line(lineend = "square"),
    axis.ticks.x = element_blank(),
    axis.ticks.length = ggplot2::unit(0.3, "lines"), 
    axis.text = ggplot2::element_text(size = base_size),
    axis.line.y = ggplot2::element_line(),
    panel.grid.major.y = element_line(colour = "grey85", linetype = "dotted", size = 0.3),
    #plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),
    strip.text = element_text(face = "bold", margin = margin(t = 1, r = 1, b = 2, l = 1, unit = "pt"))
  )
}

theme_ebm_hbar <- function(base_size) {
  ggthemes::theme_base(base_size = base_size, base_family = font_fam) %+replace% ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_line(lineend = "square"),
    axis.ticks.y = element_blank(), 
    axis.ticks.length = ggplot2::unit(0.3, "lines"), 
    axis.text = ggplot2::element_text(size = base_size),
    axis.line.x = ggplot2::element_line(),
    panel.grid.major.x = element_line(colour = "grey85", linetype = "dotted", size = 0.3),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
    strip.text = element_text(face = "bold", margin = margin(t = 1, r = 1, b = 2, l = 1, unit = "pt"))
  )
}

theme_ebm_minimal <- function(base_size) {
  ggthemes::theme_base(base_size = base_size, base_family = font_fam) %+replace% ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = base_size),
    panel.grid.major.x = element_line(colour = "grey85", linetype = "dotted", size = 0.3),
    panel.grid.major.y = element_line(colour = "grey85", linetype = "dotted", size = 0.3),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
    strip.text = element_text(face = "bold", margin = margin(t = 1, r = 1, b = 2, l = 1, unit = "pt"))
  )
}

# Scaling of geom_text to match base_size

text_scaler <- 0.352777778 # Matches font sizes if multiplied

# Colours

colour_scale <- MetBrewer::met.brewer(name = "Java")
colour_scale_light <- colorspace::lighten(colour_scale, 0.25)

color_scale_okito <- ggokabeito::palette_okabe_ito()

color_scale_brewer <- RColorBrewer::brewer.pal(11, "Spectral")

colour_scale_desat <- colorspace::desaturate(colour_scale_light, 0.25)

color_men <- "#5BAAA4"
color_women <- "#D9A464"
color_other <- "#87658F"

color_covid <- "#5674B9"
color_noncovid <- "#A6D5F7"


