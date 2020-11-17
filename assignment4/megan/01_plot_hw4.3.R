#############################################################
## Name: Megan Knight                                      ##
## Purpose: HMS520 Learning Activity 4 -- data prep,       ##
## regression anlaysis, and plot for problem 3             ##
#############################################################
## load libraries / functions 
source("/ihme/homes/mknight4/repos/class/HMS520A_Autumn2020_Megan_Knight/homework/01_functions_hw4.2.R")

## read datasets
df_tb <- as.data.frame(tidyr::who)
df_pop <- read.csv('~/class/hms520/data/API_SP.POP.TOTL_DS2_en_csv_v2_1637443.csv')

## data cleaning 
df_tb <- df_tb %>%
  pivot_longer(cols = new_sp_m014:newrel_f65, 
               names_to = "key", 
               values_to = "cases", 
               values_drop_na = TRUE) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

df_pop <- df_pop %>%
  pivot_longer(cols = X1960:X2020, 
               names_to = "year", 
               values_to = "pop", 
               values_drop_na = TRUE) %>%
  mutate(year = as.integer(stringr::str_remove(year, "X"))) %>%
  select(-Country.Code, -Indicator.Code, -Indicator.Name)

## join data 
df <- df_pop %>% 
  inner_join(df_tb, by = c("Country.Name" = "country", "year" = "year")) %>%
  mutate(incident_ratio = cases/pop)

## run regression using this formula: incidence_ratio ~ 1 + year
model_results <- regress_group_data(data = df, 
                                    group_id = 'Country.Name',
                                    obs = 'incident_ratio', 
                                    covs = c('1', 'year'), 
                                    include_intercept = FALSE)


## supress scientific notation 
options(scipen=10000)

## plot regression results
plot <- ggplot(data=model_results, aes(x=Country.Name, y=-sort(year))) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Country", y = "Year")