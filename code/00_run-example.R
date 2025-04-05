#--just getting a ghetto example to put into a one-pager

library(tidyverse)
library(readxl)
library(PESTO)
library(PesticideLoadIndex4Dummies) #--need to test this more...



# A. qualitative data -----------------------------------------------------

a <- read_excel("data/elicitations/ABC_survey_fairy dust lettuce.xlsx")

a1 <- MakePestoDF(f_dat = a)
a2 <- SummarizePestoDF(f_dat2 = a1)
a3 <- CalcPestoUtil(f_dat3 = a2)

# B. pesticide data data -----------------------------------------------------

b <- read_excel("data/elicitations/ABC_peticides_fairy dust lettuce.xlsx")

#--it is my job to fix the units



dat.part.c <-
  dat.part.a %>%
  dplyr::left_join(internal_impactcatsinfo) %>%
  dplyr::bind_rows(dat.part.b %>%
                     dplyr::mutate(weight = 100)) %>%
  dplyr::mutate(desc = paste0(value_metric, " (", weight, "%)"))

t.title <- a_dat1$title %>% unique() %>% stringr::str_to_sentence()


p1 <-
  dat.part.c %>%
  dplyr::filter(short != "all") %>%
  dplyr::arrange(scenario, -weight) %>%
  dplyr::mutate(value_binF = as.factor(value_bin),
                value_metric = stringr::str_to_sentence(value_metric),
                descF = forcats::fct_inorder(desc),
                scenarioF = factor(scenario, levels = c("Package #1",
                                                        "Package #2"))) %>%
  ggplot(aes(value_bin, score)) +
  geom_col(aes(fill = value_binF), linewidth = 1, color = "black", show.legend = F) +
  scale_fill_manual(values = c(av1, av2, av3, av4, av5)) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = c(0, 25, 50, 75, 100),
                     position = "right") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("Very low", "Low", "Medium", "High", "Very high")) +
  labs(y = NULL,
       x = "Performance") +
  th1 +
  facet_grid(descF ~ scenarioF, labeller = label_wrap_gen(width = 20), switch = "y")



dat.part.c.util <-
  dat.part.c %>%
  dplyr::filter(short == "all") %>%
  dplyr::arrange(scenario, -weight) %>%
  dplyr::select(scenario, score, value_bin) %>%
  dplyr::mutate(value = dplyr::case_when(
    value_bin == 5 ~ 100,
    value_bin == 4 ~ 75,
    value_bin == 3 ~ 50,
    value_bin == 2 ~ 25,
    value_bin == 1 ~ 0,
    TRUE ~ 9999
  )) %>%
  dplyr::group_by(scenario) %>%
  dplyr::summarise(util = round(weighted.mean(x = value, w = score), 0)) %>%
  dplyr::mutate(scenarioF = factor(scenario, levels = c("Package #1",
                                                        "Package #2")),
                util_label = paste0("Utility = ", util, "%"))

p2 <-
  dat.part.c %>%
  dplyr::filter(short == "all") %>%
  dplyr::mutate(desc = "Weighted combination of all categories") %>%
  dplyr::arrange(scenario, -weight) %>%
  dplyr::mutate(value_binF = as.factor(value_bin),
                value_metric = stringr::str_to_sentence(value_metric),
                descF = forcats::fct_inorder(desc),
                scenarioF = factor(scenario, levels = c("Package #1",
                                                        "Package #2"))) %>%
  ggplot(aes(value_bin, score)) +
  geom_col(aes(fill = value_binF), linewidth = 1, color = "black", show.legend = F) +
  geom_text(data = dat.part.c.util,
            aes(x = 3, y = 90, label = util_label,
                fontface = "italic"),
            hjust = 0.5) +
  scale_fill_manual(values = c(av1, av2, av3, av4, av5)) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = c(0, 25, 50, 75, 100),
                     position = "right") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("Very low", "Low", "Medium", "High", "Very high")) +
  labs(y = NULL,
       x = "Performance") +
  th1 +
  facet_grid(descF ~ scenarioF, labeller = label_wrap_gen(width = 20), switch = "y")


layout <- "
AABB
AA##
AA##"

theme_border <-
  ggplot2::theme_gray() +
  ggplot2::theme(plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3),
                 plot.title = element_text(size = 20, hjust = 0.5))

t.title <- a_dat1$title %>% unique()


p_all <-
  p1 +
  p2 +
  patchwork::plot_layout(design = layout) +
  patchwork::plot_annotation(title =   stringr::str_wrap(t.title, 40),
                             theme = theme_border)



ggsave("figs/fig_example-fairy-dust.png", width = 9, height = 9)
