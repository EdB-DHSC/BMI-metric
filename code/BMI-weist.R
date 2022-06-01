install.packages("unpivotr")
library(unpivotr)
install.packages("tidyxl")
library(tidyxl)
install.packages("tidyverse")
library(tidyverse)

hse_waist <- xlsx_cells("data/HSE19-Overweight-obesity-tab-waist.xlsx")

hse_waist_formats <- xlsx_formats("data/HSE19-Overweight-obesity-tab-waist.xlsx")

tidy <- hse_waist %>% filter(row != 1,
                     row != 2,
                     row != 5,
                     col != 2,
                     !is_blank) %>% 
  behead('up-left',header1)%>% 
  behead('up',year) %>% 
  behead_if(hse_waist_formats$local$font$bold[local_format_id] == TRUE,
            direction = "left-up",
            name = "BMI_cat") %>%
  behead('left','waist') %>% 
  select(numeric,year,BMI_cat,waist) %>% 
  filter(!is.na(waist))
