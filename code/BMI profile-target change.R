install.packages("tidyverse")
library(tidyverse)
library(haven)

HSE2017 <- read_sav("C:/Users/EBeake/OneDrive - Department of Health and Social Care/Data/HSfE/2017/UKDA-8488-spss/spss/spss24/hse17i_eul_v1.sav") %>% 
  filter(BMIok == 1,
         Age16g5 >= 2,
         Sex >= 1) %>% 
  select(BMI,Age16g5,Sex,qimd,wt_int,whval) %>% 
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
  select(BMI,Age16g5,Sex,qimd,wt_int,whval) %>% 
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
  select(BMI,Age16g5,Sex,qimd,wt_int,whval) %>% 
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
  mutate(y2035 = BMI_rounded + 3.5,
         third = BMI_rounded - 6,
         fith = BMI_rounded - 3.5,
         quarter = BMI_rounded - 4.5,
         BMI_cat = case_when(BMI_rounded <= 18.5 ~ 'underweight',
                             BMI_rounded >= 18.5 & BMI_rounded < 25 ~ 'healthy weight',
                             BMI_rounded >= 25 & BMI_rounded < 30 ~ 'overweight',
                             BMI_rounded >= 30 & BMI_rounded < 40 ~ 'obese',
                             BMI_rounded >= 40 ~ 'sever obese')) %>% 
  pivot_longer(cols = c('y2035','BMI_rounded','third','fith','quarter'),
               names_to = 'target',
               values_to = 'BMI_rounded') %>% 
  group_by(BMI_rounded,target) %>% 
  summarise(count = n(),wt = mean(wt_int)) %>% 
  mutate(count = count * wt,
         target = str_replace(target,'third','Reduce adult obesity rates by a third by 2035'),
         target = str_replace(target,'BMI_rounded','Current'),
         target = str_replace(target,'y2035','BMI profile in 2035 if no further action is taken'),
         target = str_replace(target,'fith','Reduce adult obesity rates by a fith by 2035'),
         target = str_replace(target,'quarter','Reduce adult obesity rates by a quarter by 2035')) %>%
  ggplot(aes(BMI_rounded,count,col = target)) +
  geom_line() +
  geom_vline(xintercept = 18.5, linetype = 2)+
  geom_vline(xintercept = 25, linetype = 2)+
  geom_vline(xintercept = 30, linetype = 2)+
  geom_vline(xintercept = 40, linetype = 2)+
  theme_bw()+
labs(title = 'BMI Profile of Adults in England',
     y = '',
     x = 'BMI') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.direction = 'vertical',
        legend.position = 'bottom') +
  scale_color_manual(values=c('grey',
                              "black",
                              "#B39CE1",
                              "#F188F5",
                              "red"))
