#--example impact of change in confidence

library(tidyverse)
library(readxl)
library(patchwork)
library(PESTO)
library(PesticideLoadIndex4Dummies) #--need to test this more...


# A. betas binned -----------------------------------------------------

#--note the 'rating' is the 'badness', which is the opposite of the survey
d1 <- 
  read_excel("data/byhand_distribution-cheat-sheet3.xlsx", skip = 5)

#--process into tidy data
d2 <-
  d1 %>%
  dplyr::select(-tot) %>%
  tidyr::fill(confidence) %>%
  janitor::clean_names() %>%
  tidyr::pivot_longer(x1:x5) %>%
  dplyr::mutate(name = readr::parse_number(name),
                value_bin = dplyr::case_when(
                  name == 1 ~ 5,
                  name == 2 ~ 4,
                  name == 3 ~ 3,
                  name == 4 ~ 2,
                  name == 5 ~ 1,
                  TRUE ~ 9999
                )) %>%
  dplyr::rename(score = value) %>%
  dplyr::select(-name) %>%
  dplyr::mutate_if(is.character, stringr::str_to_lower)



# Make example ------------------------------------------------------------

d2 %>% 
  filter(rating_text == "very low") %>% 
  mutate(confidence = case_when(
    confidence == "l" ~ "Low confidence",
    confidence == "m" ~ "Medium confidence",
    confidence == "h" ~ "High confidence",
    confidence == "vh" ~ "Very high confidence",
    TRUE ~ "xx"
  ),
  rating_text = "Highly acceptable rating") %>% 
  mutate(rating_textF = fct_inorder(rating_text),
         confidenceF = fct_inorder(confidence),
         confidenceF2 = fct_rev(confidenceF),
         value_binF = as.factor(value_bin)) %>%
  ggplot(ggplot2::aes(value_bin, score)) +
  geom_col(aes(fill = value_binF), show.legend = F) +
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
    #labels = c("Not acceptable", "Low", "Medium", "High", "Very high")
    ) +
  labs(y = NULL,
       x = "Performance") +
  pesto_th1 +
  facet_grid(rating_textF ~confidenceF2,
             labeller = label_wrap_gen(width = 10), switch = "y") +
  theme(strip.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(angle = 0, hjust = 0.5))



# acceptable, very high confidence ---------------------------------

d2 %>% 
  filter(rating_text == "low") %>% 
  mutate(confidence = case_when(
    confidence == "l" ~ "Low confidence",
    confidence == "m" ~ "Medium confidence",
    confidence == "h" ~ "High confidence",
    confidence == "vh" ~ "Very high confidence",
    TRUE ~ "xx"
  ),
  rating_text = "Acceptable rating") %>% 
  filter(confidence == "Very high confidence") %>% 
  mutate(rating_textF = fct_inorder(rating_text),
         confidenceF = fct_inorder(confidence),
         confidenceF2 = fct_rev(confidenceF),
         value_binF = as.factor(value_bin)) %>%
  ggplot(ggplot2::aes(value_bin, score)) +
  geom_col(aes(fill = value_binF), color = "black", show.legend = F, size = 1.2) +
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
    #labels = c("Not acceptable", "Low", "Medium", "High", "Very high")
  ) +
  labs(y = NULL,
       x = "Performance") +
  pesto_th1 +
  facet_grid(rating_textF ~confidenceF2,
             labeller = label_wrap_gen(width = 10), switch = "y") +
  theme(strip.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave("figs/fig_example-acceptable-vh.png")


# acceptable, low conf ---------------------------------

d2 %>% 
  filter(rating_text == "low") %>% 
  mutate(confidence = case_when(
    confidence == "l" ~ "Low confidence",
    confidence == "m" ~ "Medium confidence",
    confidence == "h" ~ "High confidence",
    confidence == "vh" ~ "Very high confidence",
    TRUE ~ "xx"
  ),
  rating_text = "Acceptable rating") %>% 
  filter(confidence == "Low confidence") %>% 
  mutate(rating_textF = fct_inorder(rating_text),
         confidenceF = fct_inorder(confidence),
         confidenceF2 = fct_rev(confidenceF),
         value_binF = as.factor(value_bin)) %>%
  ggplot(ggplot2::aes(value_bin, score)) +
  geom_col(aes(fill = value_binF), color = "black", show.legend = F, size = 1.2) +
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
    #labels = c("Not acceptable", "Low", "Medium", "High", "Very high")
  ) +
  labs(y = NULL,
       x = "Performance") +
  pesto_th1 +
  facet_grid(rating_textF ~confidenceF2,
             labeller = label_wrap_gen(width = 10), switch = "y") +
  theme(strip.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave("figs/fig_example-acceptable-l.png")

# not acceptable, very high confidence ---------------------------------

d2 %>% 
  filter(rating_text == "very high") %>% 
  mutate(confidence = case_when(
    confidence == "l" ~ "Low confidence",
    confidence == "m" ~ "Medium confidence",
    confidence == "h" ~ "High confidence",
    confidence == "vh" ~ "Very high confidence",
    TRUE ~ "xx"
  ),
  rating_text = "Not acceptable rating") %>% 
  filter(confidence == "Very high confidence") %>% 
  mutate(rating_textF = fct_inorder(rating_text),
         confidenceF = fct_inorder(confidence),
         confidenceF2 = fct_rev(confidenceF),
         value_binF = as.factor(value_bin)) %>%
  ggplot(ggplot2::aes(value_bin, score)) +
  geom_col(aes(fill = value_binF), color = "black", show.legend = F, size = 1.2) +
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
    #labels = c("Not acceptable", "Low", "Medium", "High", "Very high")
  ) +
  labs(y = NULL,
       x = "Performance") +
  pesto_th1 +
  facet_grid(rating_textF ~confidenceF2,
             labeller = label_wrap_gen(width = 10), switch = "y") +
  theme(strip.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave("figs/fig_example-notacceptable-vh.png")


