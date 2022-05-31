install.packages("unpivotr")
library(unpivotr)
install.packages("tidyxl")
library(tidyxl)
install.packages("tidyverse")
library(tidyverse)

hse_waist <- xlsx_cells("data/HSE19-Overweight-obesity-tab.xlsx",
                        sheets = 'Table 11')

hse_waist %>% filter(row != 1,
                     row != 2,
                     row != 5,
                     col != 2) %>% 
  behead('up',header1)
