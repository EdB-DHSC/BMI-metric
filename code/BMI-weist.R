install.packages("unpivotr")
library(unpivotr)
install.packages("tidyxl")
library(tidyxl)
install.packages("tidyverse")
library(tidyverse)
library(haven)

HSE2017 <- read_sav("C:/Users/EBeake/OneDrive - Department of Health and Social Care/Data/HSfE/2017/UKDA-8488-spss/spss/spss24/hse17i_eul_v1.sav") %>% 
  filter(BMIok == 1,
         Age16g5 >= 2,
         Sex >= 1) %>% 
  select(BMI,Age16g5,Sex,qimd) %>% 
  mutate(BMI_cat = case_when(BMI <= 18.5 ~ 'underweight',
                             BMI >= 18.5 & BMI < 25 ~ 'healthy weight',
                             BMI >= 25 & BMI < 30 ~ 'overweight',
                             BMI >= 30 & BMI < 40 ~ 'obese',
                             BMI >= 40 ~ 'sever obese'),
         BMI_rounded = round(BMI,0))

HSE2016 <- read_sav("C:/Users/EBeake/OneDrive - Department of Health and Social Care/Data/HSfE/2016/UKDA-8334-spss/spss/spss19/hse2016_eul.sav") %>% 
  filter(BMIok == 1,
         Age16g5 >= 2,
         Sex >= 1) %>% 
  select(BMI,Age16g5,Sex,qimd) %>% 
  mutate(BMI_cat = case_when(BMI <= 18.5 ~ 'underweight',
                             BMI >= 18.5 & BMI < 25 ~ 'healthy weight',
                             BMI >= 25 & BMI < 30 ~ 'overweight',
                             BMI >= 30 & BMI < 40 ~ 'obese',
                             BMI >= 40 ~ 'sever obese'),
         BMI_rounded = round(BMI,0))

HSE2015 <- read_sav("C:/Users/EBeake/OneDrive - Department of Health and Social Care/Data/HSfE/2015/UKDA-8280-spss/spss/spss19/hse2015ai.sav") %>% 
  filter(BMIok == 1,
         Age16g5 >= 2,
         Sex >= 1) %>% 
  select(BMI,Age16g5,Sex,qimd) %>% 
  mutate(BMI_cat = case_when(BMI <= 18.5 ~ 'underweight',
                             BMI >= 18.5 & BMI < 25 ~ 'healthy weight',
                             BMI >= 25 & BMI < 30 ~ 'overweight',
                             BMI >= 30 & BMI < 40 ~ 'obese',
                             BMI >= 40 ~ 'sever obese'),
         BMI_rounded = round(BMI,0))

HSE <- bind_rows(HSE2017,
                 HSE2016,
                 HSE2015)

HSE %>% filter(BMI <= 50) %>% 
  group_by(BMI_rounded) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(BMI_rounded,count)) +
  geom_line() +
  geom_vline(xintercept = 18.5)+
  geom_vline(xintercept = 25)+
  geom_vline(xintercept = 30)+
  geom_vline(xintercept = 40)+
  theme_bw()+
labs(title = 'BMI Profile of Adults in England',
     subtitle = 'BMI of over 18"s in HSE 15-17',
     y = '',
     x = 'BMI')
