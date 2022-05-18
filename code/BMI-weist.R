install.packages("unpivotr")
library(unpivotr)
install.packages("tidyxl")
library(tidyxl)
install.packages("tidyverse")
library(tidyverse)

HSE2017 <- read_sav("C:/Users/EBeake/OneDrive - Department of Health and Social Care/Data/HSfE/2017/UKDA-8488-spss/spss/spss24/hse17i_eul_v1.sav") %>% 
  filter(BMIok == 1) %>% 
  select(BMI,hipval,Wstval) %>% 
  filter(Wstval > 1,
         hipval > 1) %>% 
  pivot_longer(cols = c('hipval','Wstval'),
               names_to = 'HW') %>% 
  mutate(BMI_cat = case_when(BMI <= 18.5 ~ 'underweight',
                             BMI >= 18.5 & BMI < 25 ~ 'healthy weight',
                             BMI >= 25 & BMI < 30 ~ 'overweight',
                             BMI >= 30 & BMI < 40 ~ 'obese',
                             BMI >= 40 ~ 'sever obese'))

HSE2017 %>% ggplot(aes(BMI,value,col = HW)) +
  geom_line() +
  facet_wrap(~BMI_cat,scales = 'free')
