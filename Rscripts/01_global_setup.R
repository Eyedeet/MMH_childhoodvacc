#---general set up for libraries, paths and repeated functions

###R library set up
library(dplyr)
library(data.table)
library(lubridate)
library(janitor)
library(haven)
library(cli)
library(arrow)
library(ggplot2)
library(hash)
library(RColorBrewer)
library(cowplot)
library(patchwork)
library(ggalluvial)
library(tidyverse)
library(eha)
library(survival)
library(sjPlot)
library(lme4)

###Setting up the standard file paths used in this project
project <- 
codelists <- 
Rscripts <- 
graphfiles <- 
datafiles <- 
results <- 
raw_data_mothers <- 
raw_data_children <-
mothers_parquet <- 
children_parquet <- 
linked_data <-

################################################################################
###recurrent functions



#extract medcodes from parquet files
#make sure that path for list.files is set correctly
extractor_med <- function(list.files, codelist){
  
  df <- as.data.table(read_parquet(list.files[[1]]))
  df$medcodeid <- as.character(df$medcodeid)
  print(paste0(length(unique(df$patid)), " in original"))
  df <- df[medcodeid %chin% codelist[, medcodeid]]
  print(paste0(length(unique(df$patid)), " subjects with code extracted"))
  df <- merge(df, codelist, by ="medcodeid")
  
  for(i in 2:length(list.files)){
    df_temp <- as.data.table(read_parquet(list.files[[i]]))
    df_temp$medcodeid <- as.character(df_temp$medcodeid)
    print(paste0(length(unique(df_temp$patid)), " in original"))
    df_temp <- df_temp[medcodeid %chin% codelist[, medcodeid]]
    print(paste0(length(unique(df_temp$patid)), " subjects with code extracted"))
    df_temp <- merge(df_temp, codelist, by = "medcodeid")
    
    df <- rbind(df, df_temp)
  }
  
  return(df)
}

#extract prodcodes from parquet files
extractor_prod <- function(list.files, codelist){
  
  df <- as.data.table(read_parquet(list.files[[1]]))
  df$prodcodeid <- as.character(df$prodcodeid)
  print(paste0(length(unique(df$patid)), " in original"))
  df <- df[prodcodeid %chin% codelist[, prodcodeid]]
  print(paste0(length(unique(df$patid)), " subjects with code extracted"))
  df <- merge(df, codelist, by ="prodcodeid")
  
  for(i in 2:length(list.files)){
    df_temp <- as.data.table(read_parquet(list.files[[i]]))
    df_temp$prodcodeid <- as.character(df_temp$prodcodeid)
    print(paste0(length(unique(df_temp$patid)), " in original"))
    df_temp <- df_temp[prodcodeid %chin% codelist[, prodcodeid]]
    print(paste0(length(unique(df_temp$patid)), " subjects with code extracted"))
    df_temp <- merge(df_temp, codelist, by = "prodcodeid")
    
    df <- rbind(df, df_temp)
  }
  
  return(df)
}


#rounding the results
rounding <- function(X){
  n <- round(X, digits = 2)
  return(n)
}

#genearting max with missing values
max_NA <- function(x){
  return(sum(x, na.rm=T))
}

#vaccine data cleaning
#first dose given from 347 days
#second dose given from 530 days
# age difference between 1&2 is 26 days
length(unique(tmp1$patid)) #480562
nrow(tmp1) #883867 vaccine records

#utilty functions
#clean out date before minimum date
clean_min_vac_date <- function(df, dose, min_age){
  df[n_dose == dose, keep := age >=min_age]
  per <- round((nrow(df[keep ==F])/nrow(df)*100), digits = 2)
  print(paste0(nrow(df[keep ==F]), " vaccine records dropped (",
               per, "%)")) 
  df <- df[keep == T |is.na(keep)]
  df[, keep := NULL]
  print(paste0(nrow(df), " entries remaining in df")) #3334178
  #renumbering the vaccines
  df[MMR_vacc == "vaccinated", n_dose := seq_len(.N), by = "patid"]
  df[MMR_vacc == "vaccinated", n_dose := rowid(patid)]
  df[MMR_vacc == "vaccinated", by = .(patid, MMR_vacc),
     MMR_all_doses :=  .N]
  return(df)
}

#checking time diff between dose a and b
#depent on max_NA function from above
#checking now for the time difference between the two doses
check_min_diff <- function(df, dose_a, dose_b, min_diff){
  
  df[n_dose == dose_a, age1 := age]
  df[n_dose == dose_b, age2 := age]
  df[MMR_all_doses > dose_a, age_1 := lapply(.SD, max_NA), .SDcols = c("age1"), by="patid"]
  df[MMR_all_doses > dose_a, age_2 := lapply(.SD, max_NA), .SDcols = c("age2"), by="patid"]
  df[MMR_all_doses > dose_a, age_diff_12 := age_2-age_1]
  df[!is.na(age_diff_12), keep := age_diff_12 >=min_diff]
  per <- round((nrow(df[keep ==F])/nrow(df)*100), digits = 2)
  print(paste0(nrow(df[keep ==F]), " vaccine records dropped (",
               per, "%)")) 
  df <- df[keep == T |is.na(keep)]
  df[, keep := NULL]
  df[, age1:= NULL]
  df[, age2:= NULL]
  df[, age_1:= NULL]
  df[, age_2:= NULL]
  df[, age_diff_12:= NULL] 
  print(paste0(nrow(df), " entries remaining in df")) 
  
  #renumbering the vaccines
  df[MMR_vacc == "vaccinated", n_dose := seq_len(.N), by = "patid"]
  df[MMR_vacc == "vaccinated", n_dose := rowid(patid)]
  df[MMR_vacc == "vaccinated", by = .(patid, MMR_vacc),
     MMR_all_doses :=  .N]
  return(df)
}


#accination coverage at differenet ages plus 95%-CI
vacc_age <- function(data, vac, age_vac, start, end, vac_dose){
  
  n <- nrow(data[vaccine == vac &
                   age <= age_vac &
                   age_startfu_baby <= start &
                   age_endfu_baby >= end &
                   n_dose == vac_dose])
  return(n)
}

pop_vacc <- function(data_p, start, end){
  
  pop <- data_p[age_startfu_baby <= start & age_endfu_baby >= end]
  n <- length(unique(pop$babypatid))
  return(n)
}


coverage_1y <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 365, start = 40, end = 365, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 365)
  
  return((a/b)*100)
}
coverage_1y_lb <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 365, start = 40, end = 365, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 365)
  p_hat <- a/b
  res <- p_hat -1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res*100)
}
coverage_1y_ub <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 365, start = 40, end = 365, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 365)
  p_hat <- a/b
  res <- p_hat +1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res*100)
}



coverage_2y <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 731, start = 40, end = 731, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 731)
  
  return((a/b)*100)
}
coverage_2y_lb <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 731, start = 40, end = 731, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 731)
  p_hat <- a/b
  res <- p_hat -1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res*100)
}
coverage_2y_ub <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 731, start = 40, end = 731, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 731)
  p_hat <- a/b
  res <- p_hat +1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res*100)
}

coverage_3y <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 1096, start = 40, end = 1096, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 1096)
  
  return((a/b)*100)
}

coverage_3y_lb <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 1096, start = 40, end = 1096, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 1096)
  p_hat <- a/b
  res <- p_hat -1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res*100)
}
coverage_3y_ub <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 1096, start = 40, end = 1096, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 1096)
  p_hat <- a/b
  res <- p_hat +1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res*100)
}


coverage_4y <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 1461, start = 40, end = 1461, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 1461)
  
  return((a/b)*100)
}



coverage_4y_lb <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 1461, start = 40, end = 1461, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 1461)
  p_hat <- a/b
  res <- p_hat -1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res*100)
}
coverage_4y_ub <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 1461, start = 40, end = 1461, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 1461)
  p_hat <- a/b
  res <- p_hat +1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res*100)
}



coverage_5y <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 1826, start = 40, end = 1826, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 1826)
  
  return((a/b)*100)
}
coverage_5y_lb <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 1826, start = 40, end = 1826, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 1826)
  p_hat <- a/b
  res <- p_hat -1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res*100)
}
coverage_5y_ub <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 1826, start = 40, end = 1826, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 1826)
  p_hat <- a/b
  res <- p_hat +1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res*100)
}


#returning results in nice table with CI for different types of models
#GLM table
model_table <- function(model, model_name){
  
  model_values <- summary(model)
  result_table <- as.data.frame(model_values$coefficients)
  result_table$lci <- exp(result_table$Estimate - 1.96*result_table$`Std. Error`)
  result_table$uci <- exp(result_table$Estimate + 1.96*result_table$`Std. Error`)
  result_table$Estimate <- exp(result_table$Estimate)
  result_table$terms <- rownames(result_table)
  
  R2 <- pscl::pR2(model)["McFadden"]
  
  result_table<- result_table %>%  
    mutate(aic = result_table$aic) %>% 
    mutate(model = model_name,
           pseudo_r2 = R2)  %>% relocate(model, terms, Estimate,lci,uci) %>%
    mutate(Estimate = round(Estimate, digits = 2),
           lci = round(lci, digits = 2),
           uci = round(uci, digits = 2))
  
  return(result_table)
}

#AFT table
model_table_aft <- function(model, model_name){
  
  model_values <- summary(model)
  result_table <- as.data.frame(model_values$table)
  result_table$lci <- exp(result_table$Value - 1.96*result_table$`Std. Error`)
  result_table$uci <- exp(result_table$Value + 1.96*result_table$`Std. Error`)
  result_table$Estimate <- exp(result_table$Value)
  result_table$terms <- rownames(result_table)
  
  result_table<- result_table %>%  
    mutate(aic = result_table$aic) %>% 
    mutate(model = model_name) %>%
    relocate(model, terms, Estimate,lci,uci) %>%
    mutate(Estimate = round(Estimate, digits = 2),
           lci = round(lci, digits = 2),
           uci = round(uci, digits = 2))
  
  return(result_table)
}

#Mixed effetcs morel
model_table_mixed <- function(model, model_name){
  
  model_values <- summary(model)
  result_table <- as.data.table(model_values$coefficients)
  result_table$lci <- exp(result_table$Estimate - 1.96*result_table$`Std. Error`)
  result_table$uci <- exp(result_table$Estimate + 1.96*result_table$`Std. Error`)
  result_table$Estimate <- exp(result_table$Estimate)
  result_table$terms <- rownames(model_values$coefficients)
  
  
  result_table<- result_table %>%  
    mutate(aic = model_values$AICtab[1],
           bic = model_values$AICtab[2]) %>% 
    mutate(model = model_name)%>% 
    relocate(model, terms, Estimate,lci,uci) %>%
    mutate(Estimate = round(Estimate, digits = 2),
           lci = round(lci, digits = 2),
           uci = round(uci, digits = 2))
  
  return(result_table)
}

#GEE table
model_table_mixed_gee <-  function(model, model_name){
  
  model_values <- summary(model)
  result_table <- as.data.table(model_values$coefficients)
  result_table$lci <- exp(result_table$Estimate - 1.96*result_table$`Robust S.E.`)
  result_table$uci <- exp(result_table$Estimate + 1.96*result_table$`Robust S.E.`)
  result_table$Estimate <- exp(result_table$Estimate)
  result_table$terms <- rownames(model_values$coefficients)
  
  qic <- QIC.gee(model)
  
  result_table<- result_table %>% 
    mutate(model = model_name)%>% 
    relocate(model, terms, Estimate,lci,uci) %>%
    mutate(Estimate = round(Estimate, digits = 2),
           lci = round(lci, digits = 2),
           uci = round(uci, digits = 2))
  
  return(result_table)
}


