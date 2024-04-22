#### SURIVIVAL ANALYSIS
library(survival)
library(eha)
library(lme4)
library(glmmTMB)
library(lmtest)

####Logistic Regression 
options(scipen = 999)

#################################################################################
#preparing the datasets
df <- read_parquet(paste0(datafiles, "final_studypop_sensitivity_anal.parquet"))

df[, MHI_vacc1:= factor(MHI_vacc1, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                            ordered = F)]
df[, MHI_vacc2 := factor(MHI_vacc2, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                            ordered = F)]
df[, MHI_vacc3 := factor(MHI_vacc3, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                            ordered = F)]
df[, MHI_diag1:= factor(MHI_diag1, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                        ordered = F)]
df[, MHI_diag2 := factor(MHI_diag2, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                         ordered = F)]
df[, MHI_diag3 := factor(MHI_diag3, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                         ordered = F)]
df[, MHI_pres1:= factor(MHI_pres1, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                        ordered = F)]
df[, MHI_pres2 := factor(MHI_pres2, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                         ordered = F)]
df[, MHI_pres3 := factor(MHI_pres3, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                         ordered = F)]
df[, mat_age := relevel(mat_age, ref = "20-29")]
df[, birth_order_cat := relevel(birth_order_cat, ref = "1")]
df[, imd:= factor(imd, ordered = F)]
df[is.na(flurisk), flurisk := 0]
df[, flurisk := factor(flurisk)]



####turning data set into long format
df1 <- df[vaccine == "DTP" & n_dose == 1]
df1[, age_DTP1 := age]
df1[, obs_DTP1 := obsdate]
df1 <- df1[, list(babypatid, patid, age_DTP1, obs_DTP1)]


df2 <- df[vaccine == "PCV" & n_dose == 3]
df2[, age_Pneu3 := age]
df2[, obs_Pneu3 := obsdate]
df2 <-df2[, list(babypatid, patid,  age_Pneu3, obs_Pneu3)]

df3 <- df[vaccine == "MMR" & n_dose == 2]
df3[, age_MMR2 := age]
df3[, obs_MMR2 := obsdate]
df3 <- df3[, list(babypatid,patid, age_MMR2, obs_MMR2)]

tmp <- df[, c( "babypatid", "deldate", "patid", "pracid", "n_children", 
               "region" ,  "regstartdate",
               "start_fu", "end_fu" , "fu_time" , "ethnicity_5",
                "age_startfu", "age_endfu" , "gender", "imd",             
              "pregstart",  "pregend", "flurisk" , "birth_order_cat", "age_startfu_baby",
              "age_endfu_baby", "start_fu_baby", "end_fu_baby", "obsdate" ,
              "MHI_vacc1" ,"MHI_vacc2" , "MHI_vacc3" , 
              "MHI_diag1", "MHI_diag2", "MHI_diag3", 
              "MHI_pres1", "MHI_pres2", "MHI_pres3",
              "age_birth", 
              "gender_child", "age_birth")]
tmp <- distinct(tmp)
length(unique(tmp$patid)) #330199
length(unique(tmp$babypatid)) #397519

#re-joining into wide data set
tmp <- merge(tmp, df1, by = c("patid", "babypatid"), all.x = T)
length(unique(tmp$patid))
tmp <- merge(tmp, df2, by = c("patid", "babypatid"), all.x = T)
tmp <- merge(tmp, df3, by = c("patid", "babypatid"), all.x = T)
rm(df1)
rm(df2)
rm(df3)


#defining the the survival times by outcome
tmp[, DTP1 := ifelse(!is.na(age_DTP1), 1, 0)]
tmp[, fu_time_DTP1 := ifelse(!is.na(age_DTP1), age_DTP1, age_endfu_baby)]

tmp[, PCV3 := ifelse(!is.na(age_Pneu3), 1, 0)]
tmp[, fu_time_PCV3 := ifelse(!is.na(age_Pneu3), age_Pneu3, age_endfu_baby)]

tmp[, MMR2 := ifelse(!is.na(age_MMR2), 1, 0)]
tmp[, fu_time_MMR2 := ifelse(!is.na(age_MMR2), age_MMR2, age_endfu_baby)]

#################################
#checks
length(unique(tmp$patid)) #330199
length(unique(tmp$babypatid)) #397519

nrow(tmp) #1,856,420
#quality check
tmp <-tmp[age_startfu_baby <= age_endfu_baby] #one baby removed
nrow(tmp[is.na(age_startfu_baby)|is.na(age_endfu_baby)]) #0

#defining the the survival times by outcome
tmp[, DTP1 := ifelse(!is.na(age_DTP1), 1, 0)]
tmp[, fu_time_DTP1 := ifelse(!is.na(age_DTP1), age_DTP1, age_endfu_baby)]

tmp[, PCV3 := ifelse(!is.na(age_Pneu3), 1, 0)]
tmp[, fu_time_PCV3 := ifelse(!is.na(age_Pneu3), age_Pneu3, age_endfu_baby)]

tmp[, MMR2 := ifelse(!is.na(age_MMR2), 1, 0)]
tmp[, fu_time_MMR2 := ifelse(!is.na(age_MMR2), age_MMR2, age_endfu_baby)]

#################################
#checks
length(unique(tmp$patid)) #330199
length(unique(tmp$babypatid)) #397519

nrow(tmp) #1,856,420
#quality check
tmp <-tmp[age_startfu_baby <= age_endfu_baby] #one baby removed
nrow(tmp[is.na(age_startfu_baby)|is.na(age_endfu_baby)]) #0

write_parquet(tmp, paste0(datafiles, "survival_dataset_sens.parquet"))

################################################################################
tmp <- read_parquet(paste0(datafiles, "survival_dataset.parquet"))



#child data
length(unique(tmp$patid)) #330198
length(unique(tmp$babypatid)) #397518

all(tmp$fu_time_DTP1>0)
all(tmp$fu_time_PCV3>0)
all(tmp$fu_time_MMR2>0)

tmp <- unique(tmp, by = "babypatid")
nrow(tmp) #397518


################################################################################
#KM plots
km1 <- survfit(Surv(fu_time_DTP1, DTP1)~1, data = tmp, conf.type = "plain", conf.int = 0.5)
plot(km1, conf.int = TRUE)

km2 <- survfit(Surv(fu_time_PCV3, PCV3)~1, data = tmp, conf.type = "plain", conf.int = 0.5)
plot(km2, conf.int = TRUE)

km3 <- survfit(Surv(fu_time_MMR2, MMR2)~1, data = tmp, conf.type = "plain", conf.int = 0.5)
plot(km3, conf.int = TRUE)


################################################################################
#Stepwise model fitting for DTP
crude_DTP <- survival::survreg(Surv(time = fu_time_DTP1, event = DTP1) ~ MHI_vacc1,
                               data = tmp, 
                               dist = "loglogistic")

Model1_DTP <- survival::survreg(Surv(time = fu_time_DTP1, event = DTP1) ~ MHI_vacc1 + 
                                  region + ethnicity_5 + imd,
                                data = tmp, 
                                dist = "loglogistic")

Model2_DTP <- survival::survreg(Surv(time = fu_time_DTP1, event = DTP1) ~ MHI_vacc1 + 
                                  region + ethnicity_5 + imd +
                                  mat_age + birth_order_cat+ flurisk,
                                data = tmp, 
                                dist = "loglogistic")

mod1 <- model_table_aft(crude_DTP, "DTP crude")
mod2 <- model_table_aft(Model1_DTP, "DTP Model 1") 
mod3 <- model_table_aft(Model2_DTP, "DTP Model 2") 

crude_DTP <- survival::survreg(Surv(time = fu_time_DTP1, event = DTP1) ~ MHI_diag1,
                               data = tmp, 
                               dist = "loglogistic")

Model1_DTP <- survival::survreg(Surv(time = fu_time_DTP1, event = DTP1) ~ MHI_diag1 + 
                                  region + ethnicity_5 + imd,
                                data = tmp, 
                                dist = "loglogistic")

Model2_DTP <- survival::survreg(Surv(time = fu_time_DTP1, event = DTP1) ~ MHI_diag1 + 
                                  region + ethnicity_5 + imd +
                                  mat_age + birth_order_cat+ flurisk,
                                data = tmp, 
                                dist = "loglogistic")

mod4 <- model_table_aft(crude_DTP, "DTP crude - diag only")
mod5 <- model_table_aft(Model1_DTP, "DTP Model 1 - diag only") 
mod6 <- model_table_aft(Model2_DTP, "DTP Model 2 - diag only") 



crude_DTP <- survival::survreg(Surv(time = fu_time_DTP1, event = DTP1) ~ MHI_pres1,
                               data = tmp, 
                               dist = "loglogistic")

Model1_DTP <- survival::survreg(Surv(time = fu_time_DTP1, event = DTP1) ~ MHI_pres2 + 
                                  region + ethnicity_5 + imd,
                                data = tmp, 
                                dist = "loglogistic")

Model2_DTP <- survival::survreg(Surv(time = fu_time_DTP1, event = DTP1) ~ MHI_pres3 + 
                                  region + ethnicity_5 + imd +
                                  mat_age + birth_order_cat+ flurisk,
                                data = tmp, 
                                dist = "loglogistic")

mod7 <- model_table_aft(crude_DTP, "DTP crude - prescription")
mod8 <- model_table_aft(Model1_DTP, "DTP Model 1 - prescription") 
mod9 <- model_table_aft(Model2_DTP, "DTP Model 2 - prescription") 






results_DTP <- data.table(rbind(mod1, mod2, mod3,
                     mod4, mod5, mod6,
                     mod7, mod8, mod9))

results_DTP[, TR:= paste0(round(1/Estimate, digits = 2), " (", round(1/uci, digits = 2), "-", round(1/lci, digits = 2), ")")]
write.table(results_DTP, paste0(results, "aft_results_DTP_sensitivity.csv"), sep = "\t", row.names = T,
            dec = ".")

############################################################################
#Stepwise model fitting for PCV3
crude_DTP <- survival::survreg(Surv(time = fu_time_PCV3, event = PCV3) ~ MHI_vacc1,
                               data = tmp, 
                               dist = "loglogistic")

Model1_DTP <- survival::survreg(Surv(time = fu_time_PCV3, event = PCV3) ~ MHI_vacc1 + 
                                  region + ethnicity_5 + imd,
                                data = tmp, 
                                dist = "loglogistic")

Model2_DTP <- survival::survreg(Surv(time = fu_time_PCV3, event = PCV3) ~ MHI_vacc1 + 
                                  region + ethnicity_5 + imd +
                                  mat_age + birth_order_cat+ flurisk,
                                data = tmp, 
                                dist = "loglogistic")

mod1 <- model_table_aft(crude_DTP, "PCV3 crude")
mod2 <- model_table_aft(Model1_DTP, "PCV3 Model 1") 
mod3 <- model_table_aft(Model2_DTP, "PCV3 Model 2") 

crude_DTP <- survival::survreg(Surv(time = fu_time_PCV3, event = PCV3) ~ MHI_diag1,
                               data = tmp, 
                               dist = "loglogistic")

Model1_DTP <- survival::survreg(Surv(time = fu_time_PCV3, event = PCV3) ~ MHI_diag1 + 
                                  region + ethnicity_5 + imd,
                                data = tmp, 
                                dist = "loglogistic")

Model2_DTP <- survival::survreg(Surv(time = fu_time_PCV3, event = PCV3) ~ MHI_diag1 + 
                                  region + ethnicity_5 + imd +
                                  mat_age + birth_order_cat+ flurisk,
                                data = tmp, 
                                dist = "loglogistic")

mod4 <- model_table_aft(crude_DTP, "PCV3 crude - diag only")
mod5 <- model_table_aft(Model1_DTP, "PCV3 Model 1 - diag only") 
mod6 <- model_table_aft(Model2_DTP, "PCV3 Model 2 - diag only") 



crude_DTP <- survival::survreg(Surv(time = fu_time_PCV3, event = PCV3) ~ MHI_pres1,
                               data = tmp, 
                               dist = "loglogistic")

Model1_DTP <- survival::survreg(Surv(time = fu_time_PCV3, event = PCV3) ~ MHI_pres2 + 
                                  region + ethnicity_5 + imd,
                                data = tmp, 
                                dist = "loglogistic")

Model2_DTP <- survival::survreg(Surv(time = fu_time_PCV3, event = PCV3) ~ MHI_pres3 + 
                                  region + ethnicity_5 + imd +
                                  mat_age + birth_order_cat+ flurisk,
                                data = tmp, 
                                dist = "loglogistic")

mod7 <- model_table_aft(crude_DTP, "PCV3 crude - prescription")
mod8 <- model_table_aft(Model1_DTP, "PCV3 Model 1 - prescription") 
mod9 <- model_table_aft(Model2_DTP, "PCV3 Model 2 - prescription") 






results_DTP <- data.table(rbind(mod1, mod2, mod3,
                                mod4, mod5, mod6,
                                mod7, mod8, mod9))

results_DTP[, TR:= paste0(round(1/Estimate, digits = 2), " (", round(1/uci, digits = 2), "-", round(1/lci, digits = 2), ")")]
write.table(results_DTP, paste0(results, "aft_results_PCV3_sensitivity.csv"), sep = "\t", row.names = T,
            dec = ".")

#######################################################################

#Stepwise model fitting for DTP
crude_DTP <- survival::survreg(Surv(time = fu_time_MMR2, event = MMR2) ~ MHI_vacc1,
                               data = tmp, 
                               dist = "loglogistic")

Model1_DTP <- survival::survreg(Surv(time = fu_time_MMR2, event = MMR2) ~ MHI_vacc1 + 
                                  region + ethnicity_5 + imd,
                                data = tmp, 
                                dist = "loglogistic")

Model2_DTP <- survival::survreg(Surv(time = fu_time_MMR2, event = MMR2) ~ MHI_vacc1 + 
                                  region + ethnicity_5 + imd +
                                  mat_age + birth_order_cat+ flurisk,
                                data = tmp, 
                                dist = "loglogistic")

mod1 <- model_table_aft(crude_DTP, "MMR2 crude")
mod2 <- model_table_aft(Model1_DTP, "MMR2 Model 1") 
mod3 <- model_table_aft(Model2_DTP, "MMR2 Model 2") 

crude_DTP <- survival::survreg(Surv(time = fu_time_MMR2, event = MMR2) ~ MHI_diag1,
                               data = tmp, 
                               dist = "loglogistic")

Model1_DTP <- survival::survreg(Surv(time = fu_time_MMR2, event = MMR2) ~ MHI_diag1 + 
                                  region + ethnicity_5 + imd,
                                data = tmp, 
                                dist = "loglogistic")

Model2_DTP <- survival::survreg(Surv(time = fu_time_MMR2, event = MMR2) ~ MHI_diag1 + 
                                  region + ethnicity_5 + imd +
                                  mat_age + birth_order_cat+ flurisk,
                                data = tmp, 
                                dist = "loglogistic")

mod4 <- model_table_aft(crude_DTP, "MMR2 crude - diag only")
mod5 <- model_table_aft(Model1_DTP, "MMR2 Model 1 - diag only") 
mod6 <- model_table_aft(Model2_DTP, "MMR2 Model 2 - diag only") 



crude_DTP <- survival::survreg(Surv(time = fu_time_MMR2, event = MMR2) ~ MHI_pres1,
                               data = tmp, 
                               dist = "loglogistic")

Model1_DTP <- survival::survreg(Surv(time = fu_time_MMR2, event = MMR2) ~ MHI_pres2 + 
                                  region + ethnicity_5 + imd,
                                data = tmp, 
                                dist = "loglogistic")

Model2_DTP <- survival::survreg(Surv(time = fu_time_MMR2, event = MMR2) ~ MHI_pres3 + 
                                  region + ethnicity_5 + imd +
                                  mat_age + birth_order_cat+ flurisk,
                                data = tmp, 
                                dist = "loglogistic")

mod7 <- model_table_aft(crude_DTP, "MMR2 crude - prescription")
mod8 <- model_table_aft(Model1_DTP, "MMR2 Model 1 - prescription") 
mod9 <- model_table_aft(Model2_DTP, "MMR2 Model 2 - prescription") 




results_DTP <- data.table(rbind(mod1, mod2, mod3,
                                mod4, mod5, mod6,
                                mod7, mod8, mod9))

results_DTP[, TR:= paste0(round(1/Estimate, digits = 2), " (", round(1/uci, digits = 2), "-", round(1/lci, digits = 2), ")")]
write.table(results_DTP, paste0(results, "aft_results_MMR2_sensitivity.csv"), sep = "\t", row.names = T,
            dec = ".")

