
# library
library(tidyverse)

# read data
dat <- read_csv("data/data-raw_tokachi-si.csv") %>%
  drop_na(d13C|d15N) %>%
  filter(taxon %in% c("noguchi", "kohoso", "grasshopper") ) %>% 
  mutate(site = "tottabetsu", 
         species = case_when(taxon == "noguchi" ~ "L_noguchii",
                             taxon == "kohoso" ~ "B_stenoderus",
                             TRUE ~ taxon) ) %>%
  select(site, date, species, d13C, d15N) %>% 
  write_csv("data/sample_data.csv")

