library(tidyverse)
library(haven)
library(rlang)
library(foreign)
library(dplyr)
library(jtools)
library(stargazer)
library(AER)
library(schoolmath)

#=============== CREACIÓN DE LAS BASES DE DATOS ==================

#Lectura de la base principal de donde vamos a sacar los datos

balance <- read.csv("balance_aae.csv")
df0 <- data.frame(balance)
write.dta(df0,"balance.dta")


#====== Creción de la primera base: baseline

completa <- df0 %>% 
  mutate(happy = case_when(happiness_today==3 | happiness_today==4 ~ 1,
                           TRUE ~ 0),
         energy = case_when(ds_g8_feel_energetic>=0 & ds_g8_feel_energetic<=2 ~ 1,
                            TRUE ~ 0),
         stress = case_when(ds_g13c_stressed>=2 & ds_g13c_stressed<=4 ~ 1,
                            TRUE ~ 0),
         T_nap = case_when(nap_treatment==0 ~ 1,
                           TRUE ~ 0)) %>%
  select(-happiness_today,-no_sleepaids_chosen,-finished_study, -ds_g8_feel_energetic,
         -ds_g13c_stressed, -awake_per_hour_report, -awake_per_hour, -nap_treatment,
         -nap_group) 

write.csv(completa,"completa.csv",row.names = F)
write.dta(completa,"completa.dta")

baseline <- completa %>% filter(day_in_study>=1 & day_in_study<=7) %>% 
  group_by(pid) %>% 
  summarise(across(everything(),mean,na.rm = T)) %>% select(-day_in_study)

# Creamos la variable de atricion 

median_base_earnings <- median(baseline$earnings)
atricion <- baseline %>%
  mutate(drop_indicator = if_else((female_ == 1 & (age_ == 29 | age_ == 30) & T_nap == 1),1,
                                  if_else(age_ < 45 & education_ > 5 & earnings < median_base_earnings  & T_nap == 0,1,0))) %>%
  select(pid,drop_indicator)

# Pegamos la variable de atricion

baseline <- baseline %>% left_join(atricion,by = "pid", keep = F)


#====== Creación de la base: postline

postline <- completa %>% filter(day_in_study>=22 & day_in_study<=28) %>% 
  group_by(pid) %>% 
  summarise(across(everything(),mean,na.rm = T)) %>% 
  select(-day_in_study) %>%
  left_join(atricion,by = "pid", keep = F)

#Eliminamos las observaciones que no tienen valores para productivty o las variables de la pregunta 2d

#ids_to_eliminate = c(5118,5146,5152,5154,5155,5162,5165,5185,5205,5214,5236,5247,5270,5291,5303,5322,5343,5357,5362,5373,5378,5381,5385,5402,5404,5448,5451,5457,5459,5466,5468,5475,5480,5481,5503,5563,5575,5581)
ids_to_eliminate <- postline %>% filter(is.na(nap_time_mins) == TRUE |  is.na(productivity) == TRUE| is.na(corsi_measure) == TRUE| is.na(hf_measure)== TRUE | is.na(pvt_measure)== TRUE)
ids_to_eliminate <- ids_to_eliminate$pid

baseline<-baseline[!( baseline$pid  %in%  ids_to_eliminate == TRUE),]
postline<-postline[!( postline$pid  %in%  ids_to_eliminate == TRUE),]

#Generamos archivos .dta y .csv que contengan la base creada baseline

write.csv(baseline,"baseline.csv", row.names = F)
write.dta(baseline,"baseline.dta")

#Generamos archivos .dta y .csv que contengan la base creada postline

write.csv(postline, "endline.csv", row.names = F)
write.dta(postline,"endline.dta")









#=============== CÓDIGO ==================


#Cargamos las bases de datos

base_7 <- read.csv("baseline_7.csv")
post_7 <- read.csv("postline_7.csv")

base_8 <- read.csv("baseline_8.csv")
post_8 <- read.csv("postline_8.csv")

#======Pregunta 1: Tablas de balance

#TABLA DE BALANCE 1: Tabla de balance Pregunta 1 - Primeros 7 días

# Creamos un dataframe con las variables de interes
p_0_0 <- base_7%>%select(nap_group,female_, age_, no_of_children_, education_, unemployed, sleep_eff, productivity, typing_time_hr, tot_earnings, happiness_today)

# Calculamos las medias de cada variable
meds_0_0 <- p_0_0%>%group_by(nap_group)%>%summarise(across(female_:happiness_today,~ mean(.x, na.rm = T)))

# Calculamos la diferencia de medias
diff_0_0 <- c()
tstat_0_0 <- c()
pval_0_0 <- c()
for (i in 2:11){
  diff_0_0 <- c(diff_0_0,round(summ(lm(p_0_0[,i]~p_0_0[,1]),robust = 'HC1')$coeftable[2,1],4))
  tstat_0_0 <- c(tstat_0_0,round(summ(lm(p_0_0[,i]~p_0_0[,1]),robust = 'HC1')$coeftable[2,3],4))
  pval_0_0 <- c(pval_0_0,round(summ(lm(p_0_0[,i]~p_0_0[,1]),robust = 'HC1')$coeftable[2,4],4))
}
t_bal_0_0 <- data.frame(nap = t(meds_0_0[2,2:11]),no_nap = t(meds_0_0[1,2:11]),diff = diff_0_0,t = tstat_0_0,pvalue = pval_0_0)
stargazer(t_bal_0_0,summary = F)


#TABLA DE BALANCE 2: Tabla de balance Pregunta 1 - Primeros 8 días

# Creamos un dataframe con las variables de interes
p_0_1 <- base_8%>%select(nap_group,female_, age_, no_of_children_, education_, unemployed, sleep_eff, productivity, typing_time_hr, tot_earnings, happiness_today)

# Calculamos las medias de cada variable
meds_0_1 <- p_0_1%>%group_by(nap_group)%>%summarise(across(female_:happiness_today,~ mean(.x, na.rm = T)))

# Calculamos la diferencia de medias
diff_0_1 <- c()
tstat_0_1 <- c()
pval_0_1 <- c()
for (i in 2:11){
  diff_0_1 <- c(diff_0_1,round(summ(lm(p_0_1[,i]~p_0_1[,1]),robust = 'HC1')$coeftable[2,1],4))
  tstat_0_1 <- c(tstat_0_1,round(summ(lm(p_0_1[,i]~p_0_1[,1]),robust = 'HC1')$coeftable[2,3],4))
  pval_0_1 <- c(pval_0_1,round(summ(lm(p_0_1[,i]~p_0_1[,1]),robust = 'HC1')$coeftable[2,4],4))
}
t_bal_0_1 <- data.frame(nap = t(meds_0_1[2,2:11]),no_nap = t(meds_0_1[1,2:11]),diff = diff_0_1,t = tstat_0_1,pvalue = pval_0_1)
stargazer(t_bal_0_1,summary = F)



#TABLA DE BALANCE 3: Tabla de balance Pregunta 1 -  Últimos 7 días

# Creamos un dataframe con las variables de interes
p_1_0 <- post_7%>%select(nap_group,female_, age_, no_of_children_, education_, unemployed, sleep_eff, productivity, typing_time_hr, tot_earnings, happiness_today)

# Calculamos las medias de cada variable
meds_1_0 <- p_1_0%>%group_by(nap_group)%>%summarise(across(female_:happiness_today,~ mean(.x, na.rm = T)))

# Calculamos la diferencia de medias
diff_1_0 <- c()
tstat_1_0 <- c()
pval_1_0 <- c()
for (i in 2:11){
  diff_1_0 <- c(diff_1_0,round(summ(lm(p_1_0[,i]~p_1_0[,1]),robust = 'HC1')$coeftable[2,1],4))
  tstat_1_0 <- c(tstat_1_0,round(summ(lm(p_1_0[,i]~p_1_0[,1]),robust = 'HC1')$coeftable[2,3],4))
  pval_1_0 <- c(pval_1_0,round(summ(lm(p_1_0[,i]~p_1_0[,1]),robust = 'HC1')$coeftable[2,4],4))
}
t_bal_1_0 <- data.frame(nap = t(meds_1_0[2,2:11]),no_nap = t(meds_1_0[1,2:11]),diff = diff_1_0,t = tstat_1_0,pvalue = pval_1_0)
stargazer(t_bal_1_0,summary = F)


#TABLA DE BALANCE 4: Tabla de balance Pregunta 1 -  Últimos 8 días

# Creamos un dataframe con las variables de interes
p_1_1 <- post_8%>%select(nap_group,female_, age_, no_of_children_, education_, unemployed, sleep_eff, productivity, typing_time_hr, tot_earnings, happiness_today)

# Calculamos las medias de cada variable
meds_1_1 <- p_1_1%>%group_by(nap_group)%>%summarise(across(female_:happiness_today,~ mean(.x, na.rm = T)))

# Calculamos la diferencia de medias
diff_1_1 <- c()
tstat_1_1 <- c()
pval_1_1 <- c()
for (i in 2:11){
  diff_1_1 <- c(diff_1_1,round(summ(lm(p_1_1[,i]~p_1_1[,1]),robust = 'HC1')$coeftable[2,1],4))
  tstat_1_1 <- c(tstat_1_1,round(summ(lm(p_1_1[,i]~p_1_1[,1]),robust = 'HC1')$coeftable[2,3],4))
  pval_1_1 <- c(pval_1_1,round(summ(lm(p_1_1[,i]~p_1_1[,1]),robust = 'HC1')$coeftable[2,4],4))
}
t_bal_1_1 <- data.frame(nap = t(meds_1_1[2,2:11]),no_nap = t(meds_1_1[1,2:11]),diff = diff_1_1,t = tstat_1_1,pvalue = pval_1_1)
stargazer(t_bal_1_1,summary = F)




#TABLA DE BALANCE 5: Control contra tratamiento en los primeros siete días del experimento

base_without_drops <- base[base$drop_indicator == 0, ]

# Creamos un dataframe con las variables de interes
p_1 <- base_without_drops%>%select(nap_group,female_, age_, no_of_children_, education_, unemployed, sleep_eff, productivity, typing_time_hr, tot_earnings, happiness_today)

# Calculamos las medias de cada variable
meds_1 <- p_1%>%group_by(nap_group)%>%summarise(across(female_:happiness_today,~ mean(.x, na.rm = T)))

# Calculamos la diferencia de medias
diff_1 <- c()
tstat_1 <- c()
pval_1 <- c()
for (i in 2:11){
  diff_1 <- c(diff_1,round(summ(lm(p_1[,i]~p_1[,1]),robust = 'HC1')$coeftable[2,1],4))
  tstat_1 <- c(tstat_1,round(summ(lm(p_1[,i]~p_1[,1]),robust = 'HC1')$coeftable[2,3],4))
  pval_1 <- c(pval_1,round(summ(lm(p_1[,i]~p_1[,1]),robust = 'HC1')$coeftable[2,4],4))
}
t_bal_1 <- data.frame(nap = t(meds_1[2,2:11]),no_nap = t(meds_1[1,2:11]),diff = diff_1,t = tstat_1,pvalue = pval_1)
stargazer(t_bal_1,summary = F)

#TABLA DE BALANCE 6: Control contra tratamiento en los últimos siete días del experimento

post_without_drops <- post[post$drop_indicator == 0, ]

# Creamos un dataframe con las variables de interes
p_2 <- post_without_drops%>%select(nap_group,female_, age_, no_of_children_, education_, unemployed, sleep_eff, productivity, typing_time_hr, tot_earnings, happiness_today)

# Calculamos las medias de cada variable
meds_2 <- p_2%>%group_by(nap_group)%>%summarise(across(female_:happiness_today,~ mean(.x, na.rm = T)))

# Calculamos la diferencia de medias
diff_2 <- c()
tstat_2 <- c()
pval_2 <- c()
for (i in 2:11){
  diff_2 <- c(diff_2,round(summ(lm(p_2[,i]~p_2[,1]),robust = 'HC1')$coeftable[2,1],4))
  tstat_2 <- c(tstat_2,round(summ(lm(p_2[,i]~p_2[,1]),robust = 'HC1')$coeftable[2,3],4))
  pval_2 <- c(pval_2,round(summ(lm(p_2[,i]~p_2[,1]),robust = 'HC1')$coeftable[2,4],4))
}
t_bal_2 <- data.frame(nap = t(meds_2[2,2:11]),no_nap = t(meds_2[1,2:11]),diff = diff_2,t = tstat_2,pvalue = pval_2)
stargazer(t_bal_2,summary = F)

#TABLA DE BALANCE 7: Control contra tratamiento durante los primeros 7 días considerando a los participantes que concluyeron el estudio como tratamiento y como control a aquellos que no concluyeron

# Creamos un dataframe con las variables de interes
p_3 <- base%>%select(drop_indicator,female_, age_, no_of_children_, education_, unemployed, sleep_eff, productivity, typing_time_hr, tot_earnings, happiness_today)

# Calculamos las medias de cada variable
meds_3 <- p_3%>%group_by(drop_indicator)%>%summarise(across(female_:happiness_today,~ mean(.x, na.rm = T)))

# Calculamos la diferencia de medias
diff_3 <- c()
tstat_3 <- c()
pval_3 <- c()
for (i in 2:11){
  diff_3 <- c(diff_3,round(summ(lm(p_3[,i]~p_3[,1]),robust = 'HC1')$coeftable[2,1],4))
  tstat_3 <- c(tstat_3,round(summ(lm(p_3[,i]~p_3[,1]),robust = 'HC1')$coeftable[2,3],4))
  pval_3 <- c(pval_3,round(summ(lm(p_3[,i]~p_3[,1]),robust = 'HC1')$coeftable[2,4],4))
}
t_bal_3 <- data.frame(stay = t(meds_3[2,2:11]),drop = t(meds_3[1,2:11]),diff = diff_3,t = tstat_3,pvalue = pval_3)
stargazer(t_bal_3,summary = F)

#TABLA DE BALANCE 8: Control contra tratamiento durante los últimos 7 días considerando a los participantes que concluyeron el estudio como tratamiento y como control a aquellos que no concluyeron

# Creamos un dataframe con las variables de interes
p_4 <- post%>%select(drop_indicator,female_, age_, no_of_children_, education_, unemployed, sleep_eff, productivity, typing_time_hr, tot_earnings, happiness_today)

# Calculamos las medias de cada variable
meds_4 <- p_4%>%group_by(drop_indicator)%>%summarise(across(female_:happiness_today,~ mean(.x, na.rm = T)))

# Calculamos la diferencia de medias
diff_4 <- c()
tstat_4 <- c()
pval_4 <- c()
for (i in 2:11){
  diff_4 <- c(diff_4,round(summ(lm(p_4[,i]~p_4[,1]),robust = 'HC1')$coeftable[2,1],4))
  tstat_4 <- c(tstat_4,round(summ(lm(p_4[,i]~p_4[,1]),robust = 'HC1')$coeftable[2,3],4))
  pval_4 <- c(pval_4,round(summ(lm(p_4[,i]~p_4[,1]),robust = 'HC1')$coeftable[2,4],4))
}
t_bal_4 <- data.frame(stay = t(meds_4[2,2:11]),drop = t(meds_4[1,2:11]),diff = diff_4,t = tstat_4,pvalue = pval_4)
stargazer(t_bal_4,summary = F)

