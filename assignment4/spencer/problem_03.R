
# Meta --------------------------------------------------------------------

## HW 04, problem 3
#
#  Description:
#  Clean and tidy data for analysis of global tuberculosis trends over time


# Load libraries ----------------------------------------------------------

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)


# Load external functions -------------------------------------------------

source("problem_02.R")


# Load data ---------------------------------------------------------------

df_tb_raw <- tidyr::who

df_pop_raw <- readr::read_csv(
  "API_SP.POP.TOTL_DS2_en_csv_v2_1637443.csv",
  skip = 4
)


# Clean data --------------------------------------------------------------

df_tb <- df_tb_raw %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = "key",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "var", "sexage")) %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1)

df_pop <- df_pop_raw %>%
  pivot_longer(
    cols = `1960`:`2020`,
    names_to = "year",
    values_to = "population",
    names_transform = list(year = as.integer),
    values_drop_na = TRUE
  ) %>%
  select(country = `Country Name`, year, population)


# Analyze incidence -------------------------------------------------------

df_incidence <- df_tb %>%
  inner_join(df_pop, by = c("country", "year")) %>%
  mutate(incidence_ratio = cases / population)

df_incidence_coef <- regress_group_data(
  df_incidence,
  group_id = "country",
  obs = "incidence_ratio",
  cov = "year"
)

plot_incidence_coef <-
  ggplot(df_incidence_coef, aes(x = reorder(country, -year), y = year)) +
  geom_col() +
  theme_light() +
  labs(
    title = "Tuberculosis Indcidence Ratio Relationship with Year",
    subtitle = "Modeled by country (incidence_ratio ~ 1 + year)",
    x = "Country",
    y = "coefficient"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_incidence_coef
