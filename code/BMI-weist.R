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
  select(BMI,Age16g5,Sex,qimd,wt_int) %>% 
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
  select(BMI,Age16g5,Sex,qimd,wt_int) %>% 
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
  select(BMI,Age16g5,Sex,qimd,wt_int) %>% 
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
  summarise(count = n(),wt = mean(wt_int)) %>% 
  mutate(count = count * wt) %>% 
  ggplot(aes(BMI_rounded,count)) +
  geom_line() +
  geom_vline(xintercept = 18.5, linetype = 2)+
  geom_vline(xintercept = 25, linetype = 2)+
  geom_vline(xintercept = 30, linetype = 2)+
  geom_vline(xintercept = 40, linetype = 2)+
  theme_bw()+
labs(title = 'BMI Profile of Adults in England',
     subtitle = 'BMI of over 18"s in HSE 15-17',
     y = '',
     x = 'BMI')

HSE %>% mutate(qimd = as.numeric(qimd),
               Sex = case_when(Sex == 1 ~ 'Male',
                               Sex == 2 ~ 'Female')) %>% 
  filter(BMI <= 50,
               qimd == 1 |
                 qimd == 5) %>% 
  mutate(Deprevation = case_when(qimd == 1 ~ 'Least Deprived',
                                   qimd == 5 ~ 'Most Deprived')) %>% 
  group_by(BMI_rounded,Deprevation,Sex) %>% 
  summarise(count = n(),wt = mean(wt_int)) %>% 
  mutate(count = count * wt,
         count = case_when(Deprevation == 'Least Deprived' & Sex == 'Female' ~ count * (2000/2004),
                           Deprevation == 'Least Deprived' & Sex == 'Male' ~ count * (2000/1841),
                           Deprevation == 'Most Deprived' & Sex == 'Female' ~ count * (2000/1852),
                           Deprevation == 'Most Deprived' & Sex == 'Male' ~ count * (2000/1827))) %>% 
  ggplot(aes(BMI_rounded,count,col=Deprevation)) +
  geom_line() +
  geom_vline(xintercept = 18.5, linetype = 2)+
  geom_vline(xintercept = 25, linetype = 2)+
  geom_vline(xintercept = 30, linetype = 2)+
  geom_vline(xintercept = 40, linetype = 2)+
  theme_bw()+
  facet_wrap(~Sex) +
  labs(title = 'BMI Profile of Adults in England by Depravation and Gender',
       subtitle = 'BMI of over 18"s in HSE 15-17',
       y = '',
       x = 'BMI')
