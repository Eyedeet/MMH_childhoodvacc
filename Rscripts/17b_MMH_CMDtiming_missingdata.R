####Logistic Regression 
options(scipen = 999)

#preparing the dataset
df <- read_parquet(paste0(datafiles, "final_studypop_small_FU_MHItiming.parquet"))

df[, CMD_timing_vac1 := factor(CMD_timing_vac1, levels = c("None", "History of CMD", 
                                                        "prenatal CMD", "postnatal CMD"),
                            ordered = F)]
df[, CMD_timing_vac2 := factor(CMD_timing_vac2, levels = c("None", "History of CMD", 
                                                        "prenatal CMD", "postnatal CMD"),
                            ordered = F)]
df[, CMD_timing_vac3 := factor(CMD_timing_vac3, levels = c("None", "History of CMD", 
                                                        "prenatal CMD", "postnatal CMD",
                                                        "childhood CMD"),
                            ordered = F)]


df[, imd := factor(imd,  ordered = F)]
df[is.na(flurisk), flurisk := 0]
df[, mat_age := relevel(mat_age, ref = "20-29")]
df[, birth_order_cat := relevel(birth_order_cat, ref = "1")]



#preparing the outcome
#vaccinated with one DTP dose at the age of 1
length(unique(df$babypatid)) #397519
df[age_endfu_baby >= 365, vaccinated_1y := 0]
df[age_endfu_baby >= 365 &
     vaccine == "DTP" & n_dose == 1 & age > 365, vaccinated_1y := 0]
df[age_endfu_baby >= 365 &
     vaccine == "DTP" & n_dose == 1 & age <= 365, vaccinated_1y := 1]
df[, vaccinated_1y := max(vaccinated_1y), by = "babypatid"]
df[age_endfu_baby < 365, vaccinated_1y := NA]

test <- unique(df[, list(babypatid, vaccinated_1y)])
tabyl(test$vaccinated_1y)

#vaccinated with third PCV vaccine at the age of 2
df[age_endfu_baby >= 730, vaccinated_2y := 0]
df[age_endfu_baby >= 730 &
     vaccine == "PCV" & n_dose == 3 & age > 730, vaccinated_2y := 0]
df[age_endfu_baby >= 730 &
     vaccine == "PCV" & n_dose == 3 & age <= 730, vaccinated_2y := 1]
df[, vaccinated_2y := max(vaccinated_2y), by = "babypatid"]
df[age_endfu_baby < 730, vaccinated_2y := NA]

#vaccinated with the 2nd MMR vaccine at the age of 5
df[age_endfu_baby >= 1825, vaccinated_5y := 0]
df[age_endfu_baby >= 1825 &
     vaccine == "MMR" & n_dose == 2 & age > 1825, vaccinated_5y := 0]
df[age_endfu_baby >= 1825 &
     vaccine == "MMR" & n_dose == 2 & age <= 1825, vaccinated_5y := 1]
df[, vaccinated_5y := max(vaccinated_5y), by = "babypatid"]
df[age_endfu_baby < 1825, vaccinated_5y := NA]

################################################################################
df1 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby,  age_endfu_baby, gender_child,
                        CMD_timing_vac1 , vaccinated_1y)])

df2 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby,  age_endfu_baby, gender_child,
                       CMD_timing_vac2, vaccinated_2y)])

df3 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby, 
                        age_endfu_baby, gender_child,
                        CMD_timing_vac3,  vaccinated_5y)])

#tabulating the CMD exposure
tabyl(df1$CMD_timing_vac1)
tabyl(df2$CMD_timing_vac2)
tabyl(df3$CMD_timing_vac3)

#checking the coverage by MHI group
sjPlot::tab_xtab(var.row = df1$CMD_timing_vac1, var.col = df1$vaccinated_1y, title = "DTP coverage at age 1", show.row.prc = TRUE)

sjPlot::tab_xtab(var.row = df2$CMD_timing_vac2, var.col = df2$vaccinated_2y, title = "PCV coverage at age 2", show.row.prc = TRUE)

sjPlot::tab_xtab(var.row = df3$CMD_timing_vac3, var.col = df3$vaccinated_5y, title = "MMR coverage at age 5", show.row.prc = TRUE)


#################################################################################################################
#LOGISTIC REGRESSION COVERAGE AGE 1 for DPT1

model1_crude_a <- glm(vaccinated_1y ~ CMD_timing_vac1, data = df1, family = "binomial")

model1_crude_a_print <- model_table(model = model1_crude_a, model_name = "Crude DTP1 - cat")



####first adjusted model adjusting for maternal ethnicity, region
model1_DTP_a <- glm(vaccinated_1y ~ CMD_timing_vac1 + ethnicity_5 + region, data = df1, family = "binomial")
model1_DTP_a_print <- model_table(model = model1_DTP_a, model_name = "Model1  DTP1 - cat")



###second adjusted model adjusting for maternal ethnicity, region, maternal imd,
#maternal age at birth, birthorder
model2_DTP_a <- glm(vaccinated_1y ~ CMD_timing_vac1 + ethnicity_5 + region + imd + 
                      mat_age + birth_order_cat + flurisk + gender_child, data = df1, family = "binomial")

model2_DTP_print_a <- model_table(model = model2_DTP_a, model_name = "Model2  DTP1 - cat")



#################################################################################################################
#LOGISTIC REGRESSION COVERAGE AGE 2 for PCV3

###crude vaccinated_1y
model_crude_PCV_a <- glm(vaccinated_2y ~ CMD_timing_vac2, data = df2, family = "binomial")

model_crude_PCV_a_print <- model_table(model = model_crude_PCV_a, model_name = "Crude PCV3 - cat")



####first adjusted model adjusting for maternal ethnicity, region, maternal imd
model1_PCV_a <- glm(vaccinated_2y ~ CMD_timing_vac2 + ethnicity_5 + region, data = df2, family = "binomial")



model1_PCV_print_1 <- model_table(model = model1_PCV_a, model_name = "Model1  PCV3 - cat")


###second adjusted model adjusting for maternal ethnicity, region, maternal imd,
#maternal age at birth, birthorder
model2_PCV_a <- glm(vaccinated_2y ~ CMD_timing_vac2 + ethnicity_5 + region + imd + 
                      mat_age + birth_order_cat + flurisk + gender_child, data = df2, family = "binomial")
model2_PCV_print_a <- model_table(model = model2_PCV_a, model_name = "Model2   PCV3 - cat")



#################################################################################################################
#LOGISTIC REGRESSION COVERAGE AGE 5 for MMR2

###crude vaccinated_5y
model_crude_MMR_a <- glm(vaccinated_5y ~ CMD_timing_vac3, data = df3, family = "binomial")

model_crude_MMR_a_print <- model_table(model = model_crude_MMR_a, model_name = "Crude MMR2 - cat")


#adjusting for region and ethnicity
model1_MMR_a <- glm(vaccinated_5y ~ CMD_timing_vac3 + ethnicity_5 + region, data = df3, family = "binomial")

model1_MMR_print_a <- model_table(model = model1_MMR_a, model_name = "Model1  MMR2 - cat")


#fully adjusted model
model2_MMR_a <- glm(vaccinated_5y ~ CMD_timing_vac3 + ethnicity_5 + region + imd + 
                      mat_age + birth_order_cat + flurisk+ gender_child, data = df3, family = "binomial")


model2_MMR_print_a <- model_table(model = model2_MMR_a, model_name = "Model2   MMR2 - cat")



#####creating a big output file

outputs <- as.data.table(rbind(model1_crude_a_print, 
                 model_crude_PCV_a_print, 
                 model_crude_MMR_a_print, 
                 model1_DTP_a_print, 
                 model1_PCV_print_1, 
                 model1_MMR_print_a, 
                 model2_DTP_print_a, 
                 model2_PCV_print_a, 
                 model2_MMR_print_a))

outputs[, OR_CI := paste0(Estimate, " (", lci, "-", uci, ")")]

write.table(outputs, paste0(results, "log_regression_coverage_timing.csv"), sep = "\t", row.names = T,
            dec = ".")

##############################################################################
#SENSITIVITY ANALYSIS of different outcome definition
options(scipen = 999)

#preparing the dataset
df <- read_parquet(paste0(datafiles, "final_studypop_sensitivity_anal_plustiming.parquet"))

df[, CMD_timing_vac1_diag := factor(CMD_timing_vac1_diag, levels = c("None", "History of CMD", 
                                                           "prenatal CMD", "postnatal CMD"),
                               ordered = F)]
df[, CMD_timing_vac2_diag := factor(CMD_timing_vac2_diag, levels = c("None", "History of CMD", 
                                                           "prenatal CMD", "postnatal CMD"),
                               ordered = F)]
df[, CMD_timing_vac3_diag := factor(CMD_timing_vac3_diag, levels = c("None", "History of CMD", 
                                                           "prenatal CMD", "postnatal CMD",
                                                           "childhood CMD"),
                               ordered = F)]
df[, CMD_timing_vac1_pres := factor(CMD_timing_vac1_pres, levels = c("None", "History of CMD", 
                                                                     "prenatal CMD", "postnatal CMD"),
                                    ordered = F)]
df[, CMD_timing_vac2_pres := factor(CMD_timing_vac2_pres, levels = c("None", "History of CMD", 
                                                                     "prenatal CMD", "postnatal CMD"),
                                    ordered = F)]
df[, CMD_timing_vac3_pres := factor(CMD_timing_vac3_pres, levels = c("None", "History of CMD", 
                                                                     "prenatal CMD", "postnatal CMD",
                                                                     "childhood CMD"),
                                    ordered = F)]


df[, imd := factor(imd,  ordered = F)]
df[is.na(flurisk), flurisk := 0]
df[, mat_age := relevel(mat_age, ref = "20-29")]
df[, birth_order_cat := relevel(birth_order_cat, ref = "1")]


#preparing the outcome
#vaccinated with one DTP dose at the age of 1
length(unique(df$babypatid)) #397519
df[age_endfu_baby >= 365, vaccinated_1y := 0]
df[age_endfu_baby >= 365 &
     vaccine == "DTP" & n_dose == 1 & age > 365, vaccinated_1y := 0]
df[age_endfu_baby >= 365 &
     vaccine == "DTP" & n_dose == 1 & age <= 365, vaccinated_1y := 1]
df[, vaccinated_1y := max(vaccinated_1y), by = "babypatid"]
df[age_endfu_baby < 365, vaccinated_1y := NA]

test <- unique(df[, list(babypatid, vaccinated_1y)])
tabyl(test$vaccinated_1y)

#vaccinated with third PCV vaccine at the age of 2
df[age_endfu_baby >= 730, vaccinated_2y := 0]
df[age_endfu_baby >= 730 &
     vaccine == "PCV" & n_dose == 3 & age > 730, vaccinated_2y := 0]
df[age_endfu_baby >= 730 &
     vaccine == "PCV" & n_dose == 3 & age <= 730, vaccinated_2y := 1]
df[, vaccinated_2y := max(vaccinated_2y), by = "babypatid"]
df[age_endfu_baby < 730, vaccinated_2y := NA]

#vaccinated with the 2nd MMR vaccine at the age of 5
df[age_endfu_baby >= 1825, vaccinated_5y := 0]
df[age_endfu_baby >= 1825 &
     vaccine == "MMR" & n_dose == 2 & age > 1825, vaccinated_5y := 0]
df[age_endfu_baby >= 1825 &
     vaccine == "MMR" & n_dose == 2 & age <= 1825, vaccinated_5y := 1]
df[, vaccinated_5y := max(vaccinated_5y), by = "babypatid"]
df[age_endfu_baby < 1825, vaccinated_5y := NA]

################################################################################
df1 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby,  age_endfu_baby, gender_child,
                        CMD_timing_vac1_diag,CMD_timing_vac1_pres, vaccinated_1y)])

df2 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby,  age_endfu_baby, gender_child,
                        CMD_timing_vac2_diag, CMD_timing_vac2_pres,  vaccinated_2y)])

df3 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby, 
                        age_endfu_baby, gender_child,
                        CMD_timing_vac3_diag, CMD_timing_vac3_pres, vaccinated_5y)])

###############################################################################
#LOGISTIC REGRESSION COVERAGE AGE 1 for DPT1


###second adjusted model adjusting for maternal ethnicity, region, maternal imd,
#maternal age at birth, birthorder
model2_DTP_diag <- glm(vaccinated_1y ~ CMD_timing_vac1_diag + ethnicity_5 + region + imd + 
                      mat_age + birth_order_cat + flurisk + gender_child, data = df1, family = "binomial")
model2_DTP_pres <- glm(vaccinated_1y ~ CMD_timing_vac1_pres + ethnicity_5 + region + imd + 
                         mat_age + birth_order_cat + flurisk + gender_child, data = df1, family = "binomial")

model2_DTP_diag_print <- model_table(model = model2_DTP_diag, model_name = "Model2  DTP1 - diagnosis")
model2_DTP_pres_print <- model_table(model = model2_DTP_pres, model_name = "Model2  DTP1 - presciption")

#################################################################################################################
#LOGISTIC REGRESSION COVERAGE AGE 2 for PCV3

###second adjusted model adjusting for maternal ethnicity, region, maternal imd,
#maternal age at birth, birthorder
model2_PCV_diag <- glm(vaccinated_2y ~ CMD_timing_vac2_diag + ethnicity_5 + region + imd + 
                      mat_age + birth_order_cat + flurisk + gender_child, data = df2, family = "binomial")
model2_PCV_pres <- glm(vaccinated_2y ~ CMD_timing_vac2_pres + ethnicity_5 + region + imd + 
                         mat_age + birth_order_cat + flurisk + gender_child, data = df2, family = "binomial")

model2_PCV_diag_print <- model_table(model = model2_PCV_diag, model_name = "Model2  PCV3 - diagnosis")
model2_PCV_pres_print <- model_table(model = model2_PCV_pres, model_name = "Model2  PCV3 - presciption")
#################################################################################################################
#LOGISTIC REGRESSION COVERAGE AGE 5 for MMR2


#fully adjusted model
model2_MMR_diag <- glm(vaccinated_5y ~ CMD_timing_vac3_diag + ethnicity_5 + region + imd + 
                      mat_age + birth_order_cat + flurisk+ gender_child, data = df3, family = "binomial")
model2_MMR_pres <- glm(vaccinated_5y ~ CMD_timing_vac3_pres + ethnicity_5 + region + imd + 
                         mat_age + birth_order_cat + flurisk+ gender_child, data = df3, family = "binomial")

model2_MMR_diag_print <- model_table(model = model2_MMR_diag, model_name = "Model2  MMR2 - diagnosis")
model2_MMR_pres_print <- model_table(model = model2_MMR_pres, model_name = "Model2  MMR2 - presciption")


#####creating a big output file

outputs <- as.data.table(rbind(model2_DTP_diag_print, 
                               model2_DTP_pres_print, 
                               model2_PCV_diag_print, 
                               model2_PCV_pres_print, 
                               model2_MMR_diag_print, 
                               model2_MMR_pres_print))

outputs[, OR_CI := paste0(Estimate, " (", lci, "-", uci, ")")]

write.table(outputs, paste0(results, "log_regression_coverage_timing_sensitivity_diag.csv"), sep = "\t", row.names = T,
            dec = ".")




###############################################################################
#sensitivty analysis timing
#MIXED EFFECTS MODELS
library(lme4)

#sibling
DTP1_sib <- lme4::glmer(vaccinated_1y ~ CMD_timing_vac1 + ethnicity_5 + region + imd + 
                          mat_age + birth_order_cat + flurisk+ gender_child + (1|patid), 
                        data = df1, family = "binomial", nAGQ=0)
PCV3_sib <- lme4::glmer(vaccinated_2y ~ CMD_timing_vac2 + ethnicity_5 + region + imd + 
                          mat_age + birth_order_cat + flurisk+ gender_child + (1|patid),
                        data = df2, family = "binomial", nAGQ=0)
MMR2_sib <- lme4::glmer(vaccinated_5y ~ CMD_timing_vac3 + ethnicity_5 + region + imd + 
                          mat_age + birth_order_cat + flurisk+ gender_child + (1|patid),
                        data = df3, family = "binomial", nAGQ=0)

#GP
DTP1_gp <- lme4::glmer(vaccinated_1y ~ CMD_timing_vac1 + ethnicity_5 + region + imd + 
                         mat_age + birth_order_cat + flurisk+ gender_child + (1|pracid), 
                       data = df1, family = "binomial", nAGQ=0)
PCV3_gp <- lme4::glmer(vaccinated_2y ~ CMD_timing_vac2 + ethnicity_5 + region + imd + 
                         mat_age + birth_order_cat + flurisk+ gender_child + (1|pracid),
                       data = df2, family = "binomial", nAGQ=0)
MMR2_gp <- lme4::glmer(vaccinated_5y ~ CMD_timing_vac3 + ethnicity_5 + region + imd + 
                         mat_age + birth_order_cat + flurisk+ gender_child + (1|pracid),
                       data = df3, family = "binomial", nAGQ=0)


#both
DTP1_rand <- lme4::glmer(vaccinated_1y ~ CMD_timing_vac1 + ethnicity_5 + region + imd + 
                           mat_age + birth_order_cat + flurisk+ gender_child + 
                           (1|patid) + (1|pracid), 
                         data = df1, family = "binomial", nAGQ=0)
PCV3_rand <- lme4::glmer(vaccinated_2y ~ CMD_timing_vac2 + ethnicity_5 + region + imd + 
                           mat_age + birth_order_cat + flurisk+ gender_child +
                           (1|patid) + (1|pracid),
                         data = df2, family = "binomial", nAGQ=0)
MMR2_rand <- lme4::glmer(vaccinated_5y ~ CMD_timing_vac3 + ethnicity_5 + region + imd + 
                           mat_age + birth_order_cat + flurisk+ gender_child + 
                           (1|patid) + (1|pracid),
                         data = df3, family = "binomial", nAGQ=0)

mod1 <- model_table_mixed(DTP1_sib, "DTP sib")
mod2 <- model_table_mixed(PCV3_sib, "PCV sib")
mod3 <- model_table_mixed(MMR2_sib, "MMR sib")

mod4 <- model_table_mixed(DTP1_gp, "DTP GP")
mod5 <- model_table_mixed(PCV3_gp, "PCV GP")
mod6 <- model_table_mixed(MMR2_gp, "MMR GP")

mod7 <- model_table_mixed(DTP1_rand, "DTP both")
mod8 <- model_table_mixed(PCV3_rand, "PCV both")
mod9 <- model_table_mixed(MMR2_rand, "MMR both")


#saving all the outputs in one file
outputs <- as.data.table(rbind(mod1,mod2, mod3,
                               mod4, mod5, mod6,
                               mod7, mod8, mod9))

outputs[, OR_CI := paste0(Estimate, " (", lci, "-", uci, ")")]

write.table(outputs, paste0(results, "log_regression_coverage_glmm_timing.csv"), sep = "\t", row.names = T,
            dec = ".")