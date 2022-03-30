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

balance <- read.csv("balance.csv")
df0 <- data.frame(balance)

#BASELINE

pid<-c()
for (i in 1:nrow(df0)){
  if (df0[i,1] %in% pid == FALSE){
    pid<-c(pid,df0[i,1])
  }
}


for (i in 2:ncol(df0)){
  prom<-c()
  suma<-c()
  col_count_1=1
  col_count_2=8
  for(l in 1:length(pid)){
    suma<-c()
    for (j in col_count_1:col_count_2){
      suma<-c(suma,df0[j,i])
    }
    col_count_1=col_count_1+28
    col_count_2=col_count_2+28
    prom<-c(prom,mean(suma,na.rm = TRUE))
  }
  assign(names(df0)[i],prom)
}

baseline<-data.frame(pid,day_in_study,time_in_office,absences_baseline,age_,female_,education_,treatment_group,no_sleepaids_chosen,sleep_night,nap_time_mins,no_of_children_,finished_study,nap_treatment,act_inbed,an_12_number_of_awakenings,an_13_average_awakening_length,typing_time_hr,earnings,productivity,tot_earnings,deposit_today_amount,withdraw_today_amount,pay_hf,pay_corsi,pay_pvt,b1,c27,c28_h,c28_m,d1,prior_savings,go_to_bed,ds_a3_report_wakeup,awake_self_report,out_of_bed,happiness_today,ds_g1_satisfaction,ds_g8_feel_energetic,ds_g13c_stressed,sleep_eff,sleep_report,awake_per_hour_report,awake_per_hour,nap_group,daily_savings,corsi_measure,hf_measure,pvt_measure,treat_s,treat_s_i,health_bsl)
view(baseline)
baseline$an_12_number_of_awakenings <- round(baseline$an_12_number_of_awakenings, 0)
baseline$ds_a3_report_wakeup <- round(baseline$ds_a3_report_wakeup, 0)
baseline$happiness_today <- round(baseline$happiness_today, 0)
write.csv(baseline,"/Users/migueln/Desktop/Maestría/baseline.csv", row.names = TRUE)
write.dta(baseline,"/Users/migueln/Desktop/Maestría/baseline.dta")

##POSTLINE

for (i in 2:ncol(df0)){
  prom<-c()
  suma<-c()
  col_count_1=21
  col_count_2=28
  for(l in 1:length(pid)){
    suma<-c()
    for (j in col_count_1:col_count_2){
      suma<-c(suma,df0[j,i])
    }
    col_count_1=col_count_1+28
    col_count_2=col_count_2+28
    prom<-c(prom,mean(suma,na.rm = TRUE))
  }
  assign(names(df0)[i],prom)
}

postline<-data.frame(pid,day_in_study,time_in_office,absences_baseline,age_,female_,education_,treatment_group,no_sleepaids_chosen,sleep_night,nap_time_mins,no_of_children_,finished_study,nap_treatment,act_inbed,an_12_number_of_awakenings,an_13_average_awakening_length,typing_time_hr,earnings,productivity,tot_earnings,deposit_today_amount,withdraw_today_amount,pay_hf,pay_corsi,pay_pvt,b1,c27,c28_h,c28_m,d1,prior_savings,go_to_bed,ds_a3_report_wakeup,awake_self_report,out_of_bed,happiness_today,ds_g1_satisfaction,ds_g8_feel_energetic,ds_g13c_stressed,sleep_eff,sleep_report,awake_per_hour_report,awake_per_hour,nap_group,daily_savings,corsi_measure,hf_measure,pvt_measure,treat_s,treat_s_i,health_bsl)
view(postline)
postline$an_12_number_of_awakenings <- round(postline$an_12_number_of_awakenings, 0)
postline$ds_a3_report_wakeup <- round(postline$ds_a3_report_wakeup, 0)
postline$happiness_today <- round(postline$happiness_today, 0)

write.csv(postline,"/Users/migueln/Desktop/Maestría/postline.csv", row.names = TRUE)
write.dta(postline,"/Users/migueln/Desktop/Maestría/postline.dta")









#=============== CÓDIGO ==================

#Cargamos las bases de datos

base <- read.csv("baseline.csv")
post <- read.csv("postline.csv")

#Creamos una nueva variable que indique la pérdida de observaciones en ambas bases

median_base_earnings <- median(base$earnings)
base<-base %>%
  mutate(drop_indicator = if_else(((female_ == 1 & (age_ == 29 | age_ == 30) & nap_treatment == 0) | 
                                     (earnings < median_base_earnings & age_ < 45 & ( no_of_children_>=0 & no_of_children_<=2 ) & education_ > 5 & (nap_treatment == 1 | nap_treatment == 2))),1,0))

data_new <- base[base$drop_indicator == 1, ]
pids <- c(data_new$pid)

post<-post %>%
  mutate(drop_indicator = if_else(pid %in% pids == TRUE,1,0))


#TABLA DE BALANCE 1

base_without_drops <- base[base$drop_indicator == 0, ]

# Creamos un dataframe con las variables de interes
p_1 <- base_without_drops%>%select(nap_group,female_, no_of_children_, sleep_eff, productivity, education_, age_, typing_time_hr, tot_earnings, happiness_today, b1)

# Calculamos las medias de cada variable
meds_1 <- p_1%>%group_by(nap_group)%>%summarise(across(female_:b1,~ mean(.x, na.rm = T)))

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

#TABLA DE BALANCE 2

post_without_drops <- post[post$drop_indicator == 0, ]

# Creamos un dataframe con las variables de interes
p_2 <- post_without_drops%>%select(nap_group,female_, no_of_children_, sleep_eff, productivity, education_, age_, typing_time_hr, tot_earnings, happiness_today, b1)

# Calculamos las medias de cada variable
meds_2 <- p_2%>%group_by(nap_group)%>%summarise(across(female_:b1,~ mean(.x, na.rm = T)))

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

#TABLA DE BALANCE 3.1

# Creamos un dataframe con las variables de interes
p_3 <- base%>%select(drop_indicator,female_, no_of_children_, sleep_eff, productivity, education_, age_, typing_time_hr, tot_earnings, happiness_today, b1)

# Calculamos las medias de cada variable
meds_3 <- p_3%>%group_by(drop_indicator)%>%summarise(across(female_:b1,~ mean(.x, na.rm = T)))

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

#TABLA DE BALANCE 3.2

# Creamos un dataframe con las variables de interes
p_4 <- post%>%select(drop_indicator,female_, no_of_children_, sleep_eff, productivity, education_, age_, typing_time_hr, tot_earnings, happiness_today, b1)

# Calculamos las medias de cada variable
meds_4 <- p_4%>%group_by(drop_indicator)%>%summarise(across(female_:b1,~ mean(.x, na.rm = T)))

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
