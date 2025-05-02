#--just getting a ghetto example to put into a one-pager

library(tidyverse)
library(readxl)
library(patchwork)
library(PESTO)
library(PesticideLoadIndex4Dummies) #--need to test this more...

# A. qualitative data -----------------------------------------------------

a <- read_excel("data/elicitations/ABC_survey_fairy dust lettuce.xlsx")

d_6cat <- MakePestoDF(f_dat = a)
d_all <- SummarizePestoDF(f_dat2 = d_6cat)
d_util <- CalcPestoUtil(f_dat3 = d_all)

# B. pesticide data data -----------------------------------------------------

b1 <- read_excel("data/elicitations/ABC_peticides_fairy dust lettuce.xlsx")

b2 <- 
  b1 %>% 
  mutate_at(c("ai_conc_in_prod", "amt_prod_app"), as.numeric) %>% 
  fill(title) %>% 
  fill(package_title)

#--it is my job to fix the units
b3 <- 
  b2 %>% 
  mutate(ai_conc_in_prod2 = 
           case_when(
             ai_conc_units == "g/100g" ~ ai_conc_in_prod * 10,
             ai_conc_units == "g/kg" ~ ai_conc_in_prod,
             ai_conc_units == "g/L" ~ ai_conc_in_prod,
             TRUE ~ 999))

#--amount ai applied
b4 <- 
  b3 %>% 
  mutate(ai_applied = ai_conc_in_prod2 * amt_prod_app) %>% 
  select(title, package_title, ai_name, ai_applied)
  
#--get plis
b5 <- 
  b4 %>% 
  left_join(pli_ppdb3plis %>% rename(ai_name = substance)) %>% 
  select(-id) %>% 
  replace(is.na(.), 0)

#--mean of the three components?
b6 <- 
  b5 %>% 
  select(title, package_title, ai_name, ai_applied, 
         env_load2, hh_load2, eco_load2) %>% 
  pivot_longer(env_load2:eco_load2) %>% 
  group_by(title, package_title, ai_name, ai_applied) %>% 
  summarise(mean_pli = mean(value))

#--multiply by app rate
b7 <- 
  b6 %>% 
  mutate(pli_ha = round(mean_pli * ai_applied))

#--sum by package
d_pli <- 
  b7 %>% 
  group_by(title, package_title) %>% 
  summarise(pli_ha = sum(pli_ha))


# visualize ---------------------------------------------------------------

#--all six cats plot data
p1 <- 
  d_6cat %>%
  dplyr::left_join(pesto_assessmentkey) %>%
  dplyr::mutate(desc = paste0(performance_metric, " (", weight, "%)")) 

p2 <- 
  p1 %>% 
  mutate(assessment_category_order = case_when(
    assessment_category == "crop value losses" ~ 1,
    assessment_category == "direct costs" ~ 2,
    assessment_category == "time and management" ~ 3,
    assessment_category == "coordination requirements" ~ 4,
    assessment_category == "environment" ~ 5,
    assessment_category == "health and safety" ~ 6,
  )) %>% 
  arrange(package_title, assessment_category_order) %>%
  mutate(rating_textF = as.factor(rating_text),
                value_binF = as.factor(value_bin),
                descF = forcats::fct_inorder(desc),
                package_title = str_to_sentence(package_title))

t.title <- p2$title %>% unique()

f1 <- 
  p2 %>%
  filter(package_title != "Troll manure") %>% 
  mutate(package_title = ifelse(package_title == "Frog warts", 
                                "Package #1", 
                                "Package #2")) %>% 
  ggplot(aes(value_bin, score)) +
  geom_col(aes(fill = value_binF), linewidth = 1, color = "black", show.legend = F) +
  scale_fill_manual(values = c(pesto_pal[1],
                               pesto_pal[2],
                               pesto_pal[3],
                               pesto_pal[4],
                               pesto_pal[5])) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = c(0, 25, 50, 75, 100),
                     position = "right") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("Very low", "Low", "Medium", "High", "Very high")) +
  labs(y = NULL,
       x = "Performance") +
  pesto_th1 +
  theme(strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = rel(1))) +
  facet_grid(descF ~ package_title, 
             labeller = label_wrap_gen(width = 20), switch = "y")

f1

p3 <- 
  d_all %>% 
  left_join(d_util) %>% 
  left_join(d_pli) %>% 
  filter(package_title != "troll manure") 

p4 <-
  p3 %>%
  dplyr::mutate(desc = "Weighted combination of all categories") %>%
  dplyr::mutate(value_binF = as.factor(value_bin),
                descF = forcats::fct_inorder(desc),
                package_title = str_to_sentence(package_title),
                util_label = paste0("Utility = ", util, "%"),
                pli_label = paste0("Pesticide load = ", pli_ha))

f2 <- 
  p4 %>% 
  mutate(package_title = ifelse(package_title == "Frog warts", 
                                "Package #1", 
                                "Package #2")) %>% 
  ggplot(aes(value_bin, score)) +
  geom_col(aes(fill = value_binF), linewidth = 1, color = "black", show.legend = F) +
  geom_text(aes(x = 3, y = 95, label = util_label,
                fontface = "italic"),
            hjust = 0.5, check_overlap = T) +
  # geom_text(aes(x = 3, y = 90, label = pli_label,
  #               fontface = "italic"),
  #           hjust = 0.5, check_overlap = T) +
  scale_fill_manual(values = c(pesto_pal[1],
                               pesto_pal[2],
                               pesto_pal[3],
                               pesto_pal[4],
                               pesto_pal[5])) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = c(0, 25, 50, 75, 100),
                     position = "right") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("Very low", "Low", "Medium", "High", "Very high")) +
  labs(y = NULL,
       x = "Performance") +
  pesto_th1 +
  theme(strip.text.y = element_text(size = rel(1.5))) +
  facet_grid(descF ~ package_title, labeller = label_wrap_gen(width = 20), switch = "y")

f2

ggsave("figs/fig_example-2-packages-all.png", width = 6, height = 4)

f1 + f2 + 
  plot_layout(widths = c(1, 2)) +
  plot_annotation(
  #title = "Denmark, lettuce",
  #subtitle = "Comparing the use of frog warts, fairy dust, and troll manure for weed control",
  theme = theme(plot.title = element_text(size = rel(2)),
                plot.subtitle = element_text(size = rel(1.5)))
)

ggsave("figs/fig_example-2-packages.png", width = 10, height = 6)


