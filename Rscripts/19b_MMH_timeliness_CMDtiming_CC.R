library(fitdistrplus)
library(flexsurv)
library(survival)
library(MASS)
library(eha)
library(nnet)
library(broom)
library(ciTools)
library(muhaz)
library(survminer)
options(scipen=999)

#preparation of the dataset
df <- read_parquet(paste0(datafiles, "final_studypop_small_FU_MHItiming.parquet"))
df <- df[, list(babypatid, CMD_timing_vac1,
                CMD_timing_vac2, CMD_timing_vac3 )]

tmp <- read_parquet(paste0(datafiles, "survival_dataset.parquet"))
tmp <- unique(tmp, by = "babypatid")
tmp <- merge(tmp, df, by = c("babypatid"), all.x = T)
tmp <- unique(tmp, by = "babypatid")


#defining the the survival times by outcome
tmp[, DTP1 := ifelse(!is.na(age_DTP1), 1, 0)]
tmp[, fu_time_DTP1 := ifelse(!is.na(age_DTP1), age_DTP1, age_endfu_baby)]

tmp[, PCV3 := ifelse(!is.na(age_Pneu3), 1, 0)]
tmp[, fu_time_PCV3 := ifelse(!is.na(age_Pneu3), age_Pneu3, age_endfu_baby)]

tmp[, MMR2 := ifelse(!is.na(age_MMR2), 1, 0)]
tmp[, fu_time_MMR2 := ifelse(!is.na(age_MMR2), age_MMR2, age_endfu_baby)]

#################################
#checks
length(unique(tmp$patid)) #330198
length(unique(tmp$babypatid)) #397518

nrow(tmp) #1,856,420
#quality check
tmp <-tmp[age_startfu_baby <= age_endfu_baby] #one baby removed
nrow(tmp[is.na(age_startfu_baby)|is.na(age_endfu_baby)]) #0

tmp[, mat_age := relevel(mat_age, ref = "20-29")]
tmp[, birth_order_cat := relevel(birth_order_cat, ref = "1")]

tmp[, CMD_timing_vac1 := factor(CMD_timing_vac1, levels = c("None","History of CMD",
                                                             "prenatal CMD", "postnatal CMD")
                                                            ,
                                labels = c("No CMD","History of CMD",
                                           "prenatal CMD", "postnatal CMD"))]
tmp[, CMD_timing_vac2 := factor(CMD_timing_vac2, levels = c("None","History of CMD",
                                                            "prenatal CMD", "postnatal CMD")
                                ,
                                labels = c("No CMD","History of CMD",
                                           "prenatal CMD", "postnatal CMD"))]
tmp[, CMD_timing_vac3 := factor(CMD_timing_vac3, levels = c("None","History of CMD",
                                                            "prenatal CMD", "postnatal CMD",
                                                            "childhood CMD")
                                ,
                                labels = c("No CMD","History of CMD",
                                           "prenatal CMD", "postnatal CMD", 
                                           "CMD after 1y"))]


######################################
#Describing the data
fit1 <- survfit(Surv(fu_time_DTP1, DTP1) ~ CMD_timing_vac1,
                data = tmp)
fit2 <- survfit(Surv(fu_time_PCV3, PCV3) ~ CMD_timing_vac2,
                data = tmp)
fit3 <- survfit(Surv(fu_time_MMR2, MMR2) ~ CMD_timing_vac3,
                data = tmp)


km1 <- ggsurvplot(fit1, data = tmp, risk.table = TRUE, linetype = 1,
                  palette = c("red","green", "blue", "yellow"),
                  legend.labs=c("No CMD","History of CMD",
                                "prenatal CMD", "postnatal CMD"),
                  title = "First dose of DTP vaccine",
                  xlab="Age in days",
                  ylab = "Probability of being unvaccinated")

km2 <- ggsurvplot(fit2, data = tmp, risk.table = TRUE, linetype = 1,
                  palette = c("red","green", "blue", "yellow" ),
                  legend.labs=c( "No CMD","History of CMD",
                                 "prenatal CMD", "postnatal CMD"),
                  title = "Third dose of PCV vaccine",           
                  xlab="Age in days",
                  ylab = "Probability of being unvaccinated")

km3 <- ggsurvplot(fit3, data = tmp, risk.table = TRUE, linetype = 1,
                  palette = c("red","green", "blue", "yellow" , "grey"),
                  legend.labs=c("No CMD","History of CMD",
                                "prenatal CMD", "postnatal CMD", 
                                "CMD after 1y"),
                  title = "Second dose of MMR vaccine",   
                  xlab="Age in days",
                  ylab = "Probability of being unvaccinated")


km_DTP <- ggsave_workaround(km1)
km_PCV <- ggsave_workaround(km2)
km_MMR <- ggsave_workaround(km3)

ggsave("KM_DTP1_timing.pdf",
       km_DTP,
       width = 10,
       height = 9,
       bg = "white",
       path= graphfiles)
ggsave("KM_PCV3_timing.pdf",
       km_PCV,
       width = 10,
       height = 9,
       bg = "white",
       path= graphfiles)
ggsave("KM_MMR2_timing.pdf",
       km_MMR,
       width = 10,
       height = 9,
       bg = "white",
       path= graphfiles)


###############################################################################
#DTP vaccine
#model choice for crude model
#PH models
fit_exp_PH <- weibreg(formula = Surv(time = fu_time_DTP1, event = DTP1) ~ CMD_timing_vac1,
                      data = tmp, shape = 1)
fit_weib_PH <-weibreg(formula = Surv(time = fu_time_DTP1, event = DTP1) ~ CMD_timing_vac1,
                      data = tmp)
fit_gompertz_PH <- flexsurvreg(Surv(fu_time_DTP1, DTP1) ~ CMD_timing_vac1, 
                               data = tmp, dist = "gompertz") 
#AFT models
fit_weib_aft <- survreg(Surv(time = fu_time_DTP1, event = DTP1)~  CMD_timing_vac1, 
                        data=tmp, dist="weibull")
fit_loglog_aft <- survival::survreg(Surv(time = fu_time_DTP1, event = DTP1) ~ CMD_timing_vac1,
                                    data = tmp, 
                                    dist = "loglogistic")
fit_log_norm_aft <- survival::survreg(Surv(time = fu_time_DTP1, event = DTP1) ~ CMD_timing_vac1, 
                                      data = tmp, 
                                      dist = "lognormal")

#comparing the fits
###comparing the fits
extractAIC(fit_exp_PH) #33114080
extractAIC(fit_weib_PH) #32744525
AIC(fit_gompertz_PH) # 31469176

extractAIC(fit_weib_aft) #32744525
extractAIC(fit_loglog_aft) #29239655
extractAIC(fit_log_norm_aft) #30577154



#Stepwise model fitting for DTP
crude_DTP <- survival::survreg(Surv(time = fu_time_DTP1, event = DTP1) ~ CMD_timing_vac1,
                               data = tmp, 
                               dist = "loglogistic")

Model1_DTP <- survival::survreg(Surv(time = fu_time_DTP1, event = DTP1) ~ CMD_timing_vac1 + 
                                  region + ethnicity_5 + imd,
                                data = tmp, 
                                dist = "loglogistic")

Model2_DTP <- survival::survreg(Surv(time = fu_time_DTP1, event = DTP1) ~ CMD_timing_vac1 + 
                                  region + ethnicity_5 + imd +
                                  mat_age + birth_order_cat+ flurisk,
                                data = tmp, 
                                dist = "loglogistic")

mod1 <- model_table_aft(crude_DTP, "DTP crude")
mod2 <- model_table_aft(Model1_DTP, "DTP Model 1") 
mod3 <- model_table_aft(Model2_DTP, "DTP Model 2") 

results_DTP <- rbind(mod1, mod2, mod3)

results_DTP[, TR:= paste0(round(1/Value, digits = 2), " (", round(1/uci, digits = 2), "-", round(1/lci, digits = 2), ")")]
write.table(results_DTP, paste0(results, "aft_results_DTP_timingCMD.csv"), sep = "\t", row.names = T,
            dec = ".")


###############################################################################
#fitting the PCV results
#model choice for crude model
#PH models
fit_exp_PH <- weibreg(formula = Surv(time = fu_time_PCV3, event = PCV3) ~ CMD_timing_vac2,
                      data = tmp, shape = 1)
fit_weib_PH <-weibreg(formula = Surv(time = fu_time_PCV3, event = PCV3) ~ CMD_timing_vac2,
                      data = tmp)
fit_gompertz_PH <- flexsurvreg(Surv(fu_time_PCV3, PCV3) ~ CMD_timing_vac2, 
                               data = tmp, dist = "gompertz") 
#AFT models
fit_weib_aft <- survreg(Surv(time = fu_time_PCV3, event = PCV3)~  CMD_timing_vac2, 
                        data=tmp, dist="weibull")
fit_loglog_aft <- survival::survreg(Surv(time = fu_time_PCV3, event = PCV3) ~ CMD_timing_vac2,
                                    data = tmp, 
                                    dist = "loglogistic")
fit_log_norm_aft <- survival::survreg(Surv(time = fu_time_PCV3, event = PCV3) ~ CMD_timing_vac2, 
                                      data = tmp, 
                                      dist = "lognormal")

#comparing the fits
###comparing the fits
extractAIC(fit_exp_PH) #36447761
extractAIC(fit_weib_PH) #35238778
AIC(fit_gompertz_PH) # 36333282

extractAIC(fit_weib_aft) #35238778
extractAIC(fit_loglog_aft) #31072120
extractAIC(fit_log_norm_aft) #32839112


#Stepwise model fitting for PCV3
crude_PCV <- survival::survreg(Surv(time = fu_time_PCV3, event = PCV3) ~ CMD_timing_vac2,
                               data = tmp, 
                               dist = "loglogistic")

Model1_PCV <- survival::survreg(Surv(time = fu_time_PCV3, event = PCV3) ~ CMD_timing_vac2 + 
                                  region + ethnicity_5 + imd,
                                data = tmp, 
                                dist = "loglogistic")

Model2_PCV <- survival::survreg(Surv(time = fu_time_PCV3, event = PCV3) ~ CMD_timing_vac2 + 
                                  region + ethnicity_5 + imd +
                                  mat_age + birth_order_cat+ flurisk,
                                data = tmp, 
                                dist = "loglogistic")

mod1 <- model_table_aft(crude_PCV, "PCV crude")
mod2 <- model_table_aft(Model1_PCV, "PCV Model 1") #
mod3 <- model_table_aft(Model2_PCV, "PCV Model 2") #

results_PCV <- rbind(mod1, mod2, mod3)
results_PCV[, TR:= paste0(round(1/Value, digits = 2), " (", round(1/uci, digits = 2), "-", round(1/lci, digits = 2), ")")]

write.table(results_PCV, paste0(results, "aft_results_PCV_timingCMD.csv"), sep = "\t", row.names = T,
            dec = ".")


###############################################################################
#fitting timing for the MMR vaccine

#model choice for crude model
#PH models
fit_exp_PH <- weibreg(formula = Surv(time = fu_time_MMR2, event = MMR2) ~ CMD_timing_vac3,
                      data = tmp, shape = 1)
fit_weib_PH <-weibreg(formula = Surv(time = fu_time_MMR2, event = MMR2) ~ CMD_timing_vac3,
                      data = tmp)
fit_gompertz_PH <- flexsurvreg(Surv(fu_time_MMR2, MMR2) ~ CMD_timing_vac3, 
                               data = tmp, dist = "gompertz") 
#AFT models
fit_weib_aft <- survreg(Surv(time = fu_time_MMR2, event = MMR2)~  CMD_timing_vac3, 
                        data=tmp, dist="weibull")
fit_loglog_aft <- survival::survreg(Surv(time = fu_time_MMR2, event = MMR2) ~ CMD_timing_vac3,
                                    data = tmp, 
                                    dist = "loglogistic")
fit_log_norm_aft <- survival::survreg(Surv(time = fu_time_MMR2, event = MMR2) ~ CMD_timing_vac3, 
                                      data = tmp, 
                                      dist = "lognormal")

#comparing the fits
###comparing the fits
extractAIC(fit_exp_PH) #4894623
extractAIC(fit_weib_PH) #4142226
AIC(fit_gompertz_PH) # 4220875

extractAIC(fit_weib_aft) #4142226
extractAIC(fit_loglog_aft) #3963361
extractAIC(fit_log_norm_aft) #4085225


#Stepwise model fitting for MMR2
crude_MMR <- survival::survreg(Surv(time = fu_time_MMR2, event = MMR2) ~ CMD_timing_vac3,
                               data = tmp, 
                               dist = "loglogistic")

Model1_MMR <- survival::survreg(Surv(time = fu_time_MMR2, event = MMR2) ~ CMD_timing_vac3 + 
                                  region + ethnicity_5 + imd,
                                data = tmp, 
                                dist = "loglogistic")

Model2_MMR <- survival::survreg(Surv(time = fu_time_MMR2, event = MMR2) ~ CMD_timing_vac3 + 
                                  region + ethnicity_5 + imd +
                                  mat_age + birth_order_cat+ flurisk,
                                data = tmp, 
                                dist = "loglogistic")

mod1 <- model_table_aft(crude_MMR, "MMR crude")
mod2 <- model_table_aft(Model1_MMR, "MMR Model 1") #
mod3 <- model_table_aft(Model2_MMR, "MMR Model 2") #

results_MMR <- rbind(mod1, mod2, mod3)
results_MMR[, TR:= paste0(round(1/Value, digits = 2), " (", round(1/uci, digits = 2), "-", round(1/lci, digits = 2), ")")]

write.table(results_MMR, paste0(results, "aft_results_MMR_timingCMD.csv"), sep = "\t", row.names = T,
            dec = ".")


