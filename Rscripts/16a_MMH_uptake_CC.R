####Logistic Regression 
options(scipen = 999)

#preparing the dataset
df <- read_parquet(paste0(datafiles, "final_studypop_all_ind.parquet"))

df[, MHI_vacc1:= factor(MHI_vacc1, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                        ordered = F)]
df[, MHI_vacc2 := factor(MHI_vacc2, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                         ordered = F)]
df[, MHI_vacc3 := factor(MHI_vacc3, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                         ordered = F)]
df[, MHI_vacc1 := relevel(MHI_vacc1, ref = "None")]
df[, MHI_vacc2 := relevel(MHI_vacc2, ref = "None")]
df[, MHI_vacc3 := relevel(MHI_vacc3, ref = "None")]
df[, mat_age := relevel(mat_age, ref = "20-29")]
df[, birth_order_cat := relevel(birth_order_cat, ref = "1")]
df[, imd:= factor(imd, ordered = F)]
df[is.na(flurisk), flurisk := 0]
df[, flurisk := factor(flurisk)]


tabyl(df$gender_child)
tabyl(df$ethnicity_5)

length(unique(df$babypatid)) #397518
length(unique(df$patid)) #330198
df <- df[gender_child != 3] #two children + mums
df <- df[ethnicity_5 != "Unknown"]

length(unique(df$babypatid)) #387338
length(unique(df$patid)) #321731


#preparing the outcome
#vaccinated with one DTP dose at the age of 1
length(unique(df$babypatid)) #387338
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

nrow(df[age_endfu<end_fu_baby])

################################################################################
df1 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby,  age_endfu_baby, gender_child,
                        MHI_vacc1, vaccinated_1y)])

df2 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby,  age_endfu_baby, gender_child,
                        MHI_vacc2,vaccinated_2y)])

df3 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby, 
                        age_endfu_baby,gender_child,
                        MHI_vacc3, vaccinated_5y)])

#################################################################################

#checking the coverage by MHI group
sjPlot::tab_xtab(var.row = df1$MHI_vacc1, var.col = df1$vaccinated_1y, title = "DTP coverage at age 1", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = df2$MHI_vacc2, var.col = df2$vaccinated_2y, title = "PCV coverage at age 2", show.row.prc = TRUE)
sjPlot::tab_xtab(var.row = df3$MHI_vacc3, var.col = df3$vaccinated_5y, title = "MMR coverage at age 5", show.row.prc = TRUE)


########confounders
sjPlot::tab_xtab(var.row = df1$MHI_vacc1, var.col = df1$ethnicity_5, title = "DTP coverage at age 1", show.col.prc = TRUE)
sjPlot::tab_xtab(var.row = df2$MHI_vacc2, var.col = df2$ethnicity_5, title = "DTP coverage at age 1", show.col.prc = TRUE)
sjPlot::tab_xtab(var.row = df3$MHI_vacc3, var.col = df3$ethnicity_5, title = "DTP coverage at age 1", show.col.prc = TRUE)

sjPlot::tab_xtab(var.row = df1$MHI_vacc1, var.col = df1$imd, title = "DTP coverage at age 1", show.col.prc = TRUE)
sjPlot::tab_xtab(var.row = df2$MHI_vacc2, var.col = df2$imd, title = "DTP coverage at age 1", show.col.prc = TRUE)
sjPlot::tab_xtab(var.row = df3$MHI_vacc3, var.col = df3$imd, title = "DTP coverage at age 1", show.col.prc = TRUE)

sjPlot::tab_xtab(var.row = df1$MHI_vacc1, var.col = df1$region, title = "DTP coverage at age 1", show.col.prc = TRUE)
sjPlot::tab_xtab(var.row = df2$MHI_vacc2, var.col = df2$region, title = "DTP coverage at age 1", show.col.prc = TRUE)
sjPlot::tab_xtab(var.row = df3$MHI_vacc3, var.col = df3$region, title = "DTP coverage at age 1", show.col.prc = TRUE)

sjPlot::tab_xtab(var.row = df1$MHI_vacc1, var.col = df1$age_birth, title = "DTP coverage at age 1", show.col.prc = TRUE)
sjPlot::tab_xtab(var.row = df2$MHI_vacc2, var.col = df2$age_birth, title = "DTP coverage at age 1", show.col.prc = TRUE)
sjPlot::tab_xtab(var.row = df3$MHI_vacc3, var.col = df3$age_birth, title = "DTP coverage at age 1", show.col.prc = TRUE)

sjPlot::tab_xtab(var.row = df1$MHI_vacc1, var.col = df1$birth_order_cat, title = "DTP coverage at age 1", show.col.prc = TRUE)
sjPlot::tab_xtab(var.row = df2$MHI_vacc2, var.col = df2$birth_order_cat, title = "DTP coverage at age 1", show.col.prc = TRUE)
sjPlot::tab_xtab(var.row = df3$MHI_vacc3, var.col = df3$birth_order_cat, title = "DTP coverage at age 1", show.col.prc = TRUE)

sjPlot::tab_xtab(var.row = df1$MHI_vacc1, var.col = df1$gender_child, title = "DTP coverage at age 1", show.col.prc = TRUE)
sjPlot::tab_xtab(var.row = df2$MHI_vacc2, var.col = df2$gender_child, title = "DTP coverage at age 1", show.col.prc = TRUE)
sjPlot::tab_xtab(var.row = df3$MHI_vacc3, var.col = df3$gender_child, title = "DTP coverage at age 1", show.col.prc = TRUE)

sjPlot::tab_xtab(var.row = df3$MHI_vacc3, var.col = df3$flurisk, title = "DTP coverage at age 1", show.col.prc = TRUE)
#################################################################################################################
#LOGISTIC REGRESSION COVERAGE AGE 1 for DPT1
model1_crude <- glm(vaccinated_1y ~ MHI_vacc1, data = df1, family = "binomial")
model1_crude_print <- model_table(model = model1_crude, model_name = "Crude DTP1")

####first adjusted model adjusting for maternal ethnicity, region
model1_DTP <- glm(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region, data = df1, family = "binomial")
model1_DTP_print <- model_table(model = model1_DTP, model_name = "Model1  DTP1")


###second adjusted model adjusting for maternal ethnicity, region, maternal imd,
#maternal age at birth, birthorder
model2_DTP <- glm(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region + imd + 
                    mat_age + birth_order_cat + flurisk + gender_child, data = df1, family = "binomial")
model2_DTP_print<- model_table(model = model2_DTP, model_name = "Model2  DTP1")

#################################################################################################################
#LOGISTIC REGRESSION COVERAGE AGE 2 for PCV3

###crude vaccinated_1y
model_crude_PCV <- glm(vaccinated_2y ~ MHI_vacc2, data = df2, family = "binomial")
model_crude_PCV_print <- model_table(model = model_crude_PCV, model_name = "Crude PCV3")


####first adjusted model adjusting for maternal ethnicity, region, maternal imd
model1_PCV<- glm(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region, data = df2, family = "binomial")
model1_PCV_print <- model_table(model = model1_PCV, model_name = "Model1  PCV3")


###second adjusted model adjusting for maternal ethnicity, region, maternal imd,
#maternal age at birth, birthorder
model2_PCV<- glm(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region + imd + 
                   mat_age + birth_order_cat + flurisk + gender_child, data = df2, family = "binomial")
model2_PCV_print <- model_table(model = model2_PCV, model_name = "Model2   PCV3")


#################################################################################################################
#LOGISTIC REGRESSION COVERAGE AGE 5 for MMR2

###crude vaccinated_5y
model_crude_MMR <- glm(vaccinated_5y ~ MHI_vacc3, data = df3, family = "binomial")
model_crude_MMR_print <- model_table(model = model_crude_MMR, model_name = "Crude MMR2")


#adjusting for region and ethnicity
model1_MMR <- glm(vaccinated_5y ~ MHI_vacc3 + ethnicity_5 + region, data = df3, family = "binomial")
model1_MMR_print <- model_table(model = model1_MMR, model_name = "Model1  MMR2")

#fully adjusted model
model2_MMR <- glm(vaccinated_5y ~ MHI_vacc3 + ethnicity_5 + region + imd + 
                    mat_age + birth_order_cat + flurisk+ gender_child, data = df3, family = "binomial")
model2_MMR_print <- model_table(model = model2_MMR, model_name = "Model2   MMR2")



#####creating a big output file
outputs <- as.data.table(rbind(model1_crude_print, model1_DTP_print, model2_DTP_print,
                               model_crude_PCV_print,model1_PCV_print, model2_PCV_print,
                               model_crude_MMR_print, model1_MMR_print,model2_MMR_print ))


outputs[, OR_CI := paste0(Estimate, " (", lci, "-", uci, ")")] 

write.table(outputs, paste0(results, "log_regression_coverage_CC.csv"), sep = "\t", row.names = T,
            dec = ".")
################################################################################
#Sensitivity analysis based on diagnoses only
df <- read_parquet(paste0(datafiles, "final_studypop_diagnoses_only.parquet"))
df[, MHI_vacc1:= factor(MHI_vacc1, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                        ordered = F)]
df[, MHI_vacc2 := factor(MHI_vacc2, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                         ordered = F)]
df[, MHI_vacc3 := factor(MHI_vacc3, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                         ordered = F)]
df[, MHI_vacc1 := relevel(MHI_vacc1, ref = "None")]
df[, MHI_vacc2 := relevel(MHI_vacc2, ref = "None")]
df[, MHI_vacc3 := relevel(MHI_vacc3, ref = "None")]
df[, mat_age := relevel(mat_age, ref = "20-29")]
df[, birth_order_cat := relevel(birth_order_cat, ref = "1")]
df[, imd:= factor(imd, ordered = F)]
df <- df[gender_child != 3] #two children + mums
df <- df[ethnicity_5 != "Unknown"]


#preparing the outcome
#vaccinated with one DTP dose at the age of 1
length(unique(df$babypatid)) #387338
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


######
df1 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby,  age_endfu_baby, gender_child,
                        MHI_vacc1, vaccinated_1y)])

df2 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby,  age_endfu_baby, gender_child,
                        MHI_vacc2,vaccinated_2y)])

df3 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby, 
                        age_endfu_baby,gender_child,
                        MHI_vacc3, vaccinated_5y)])

########################################
#LOGISTIC REGRESSION COVERAGE AGE 1 for DPT1
model1_crude <- glm(vaccinated_1y ~ MHI_vacc1, data = df1, family = "binomial")
model1_crude_print <- model_table(model = model1_crude, model_name = "Crude DTP1")

####first adjusted model adjusting for maternal ethnicity, region
model1_DTP <- glm(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region, data = df1, family = "binomial")
model1_DTP_print <- model_table(model = model1_DTP, model_name = "Model1  DTP1")


###second adjusted model adjusting for maternal ethnicity, region, maternal imd,
#maternal age at birth, birthorder
model2_DTP <- glm(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region + imd + 
                    mat_age + birth_order_cat + flurisk + gender_child, data = df1, family = "binomial")
model2_DTP_print<- model_table(model = model2_DTP, model_name = "Model2  DTP1")

#################################################################################################################
#LOGISTIC REGRESSION COVERAGE AGE 2 for PCV3

###crude vaccinated_1y
model_crude_PCV <- glm(vaccinated_2y ~ MHI_vacc2, data = df2, family = "binomial")
model_crude_PCV_print <- model_table(model = model_crude_PCV, model_name = "Crude PCV3")


####first adjusted model adjusting for maternal ethnicity, region, maternal imd
model1_PCV<- glm(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region, data = df2, family = "binomial")
model1_PCV_print <- model_table(model = model1_PCV, model_name = "Model1  PCV3")


###second adjusted model adjusting for maternal ethnicity, region, maternal imd,
#maternal age at birth, birthorder
model2_PCV<- glm(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region + imd + 
                   mat_age + birth_order_cat + flurisk + gender_child, data = df2, family = "binomial")
model2_PCV_print <- model_table(model = model2_PCV, model_name = "Model2   PCV3")


#################################################################################################################
#LOGISTIC REGRESSION COVERAGE AGE 5 for MMR2

###crude vaccinated_5y
model_crude_MMR <- glm(vaccinated_5y ~ MHI_vacc3, data = df3, family = "binomial")
model_crude_MMR_print <- model_table(model = model_crude_MMR, model_name = "Crude MMR2")


#adjusting for region and ethnicity
model1_MMR <- glm(vaccinated_5y ~ MHI_vacc3 + ethnicity_5 + region, data = df3, family = "binomial")
model1_MMR_print <- model_table(model = model1_MMR, model_name = "Model1  MMR2")

#fully adjusted model
model2_MMR <- glm(vaccinated_5y ~ MHI_vacc3 + ethnicity_5 + region + imd + 
                    mat_age + birth_order_cat + flurisk+ gender_child, data = df3, family = "binomial")
model2_MMR_print <- model_table(model = model2_MMR, model_name = "Model2   MMR2")



#####creating a big output file
outputs <- as.data.table(rbind(model1_crude_print, model1_DTP_print, model2_DTP_print,
                               model_crude_PCV_print,model1_PCV_print, model2_PCV_print,
                               model_crude_MMR_print, model1_MMR_print,model2_MMR_print ))

outputs[, OR_CI := paste0(Estimate, " (", lci, "-", uci, ")")] 

write.table(outputs, paste0(results, "log_regression_coverage_diagnoses_CC.csv"), sep = "\t", row.names = T,
            dec = ".")



################################################################################
#sensitivity analysis using prescription data
df <- read_parquet(paste0(datafiles, "final_studypop_all_pluspres.parquet"))

df[, MHI_vacc1:= factor(MHI_vacc1, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                        ordered = F)]
df[, MHI_vacc2 := factor(MHI_vacc2, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                         ordered = F)]
df[, MHI_vacc3 := factor(MHI_vacc3, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"),
                         ordered = F)]
df[, MHI_vacc1 := relevel(MHI_vacc1, ref = "None")]
df[, MHI_vacc2 := relevel(MHI_vacc2, ref = "None")]
df[, MHI_vacc3 := relevel(MHI_vacc3, ref = "None")]
df[, mat_age := relevel(mat_age, ref = "20-29")]
df[, birth_order_cat := relevel(birth_order_cat, ref = "1")]
df[, imd:= factor(imd, ordered = F)]
df <- df[gender_child != 3] #two children + mums
df <- df[ethnicity_5 != "Unknown"]

#preparing the outcome
#vaccinated with one DTP dose at the age of 1
length(unique(df$babypatid)) #387338
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


######
df1 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby,  age_endfu_baby, gender_child,
                        MHI_vacc1, vaccinated_1y)])

df2 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby,  age_endfu_baby, gender_child,
                        MHI_vacc2,vaccinated_2y)])

df3 <- unique(df[, list(babypatid, patid, deldate, pracid, n_children, birth_order_cat,
                        mat_age, region, ethnicity_5, gender, imd , flurisk,
                        age_startfu_baby, 
                        age_endfu_baby,gender_child,
                        MHI_vacc3, vaccinated_5y)])

########################################
#LOGISTIC REGRESSION COVERAGE AGE 1 for DPT1
model1_crude <- glm(vaccinated_1y ~ MHI_vacc1, data = df1, family = "binomial")
model1_crude_print <- model_table(model = model1_crude, model_name = "Crude DTP1")

####first adjusted model adjusting for maternal ethnicity, region
model1_DTP <- glm(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region, data = df1, family = "binomial")
model1_DTP_print <- model_table(model = model1_DTP, model_name = "Model1  DTP1")


###second adjusted model adjusting for maternal ethnicity, region, maternal imd,
#maternal age at birth, birthorder
model2_DTP <- glm(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region + imd + 
                    mat_age + birth_order_cat + flurisk + gender_child, data = df1, family = "binomial")
model2_DTP_print<- model_table(model = model2_DTP, model_name = "Model2  DTP1")

#################################################################################################################
#LOGISTIC REGRESSION COVERAGE AGE 2 for PCV3

###crude vaccinated_1y
model_crude_PCV <- glm(vaccinated_2y ~ MHI_vacc2, data = df2, family = "binomial")
model_crude_PCV_print <- model_table(model = model_crude_PCV, model_name = "Crude PCV3")


####first adjusted model adjusting for maternal ethnicity, region, maternal imd
model1_PCV<- glm(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region, data = df2, family = "binomial")
model1_PCV_print <- model_table(model = model1_PCV, model_name = "Model1  PCV3")


###second adjusted model adjusting for maternal ethnicity, region, maternal imd,
#maternal age at birth, birthorder
model2_PCV<- glm(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region + imd + 
                   mat_age + birth_order_cat + flurisk + gender_child, data = df2, family = "binomial")
model2_PCV_print <- model_table(model = model2_PCV, model_name = "Model2   PCV3")


#################################################################################################################
#LOGISTIC REGRESSION COVERAGE AGE 5 for MMR2

###crude vaccinated_5y
model_crude_MMR <- glm(vaccinated_5y ~ MHI_vacc3, data = df3, family = "binomial")
model_crude_MMR_print <- model_table(model = model_crude_MMR, model_name = "Crude MMR2")


#adjusting for region and ethnicity
model1_MMR <- glm(vaccinated_5y ~ MHI_vacc3 + ethnicity_5 + region, data = df3, family = "binomial")
model1_MMR_print <- model_table(model = model1_MMR, model_name = "Model1  MMR2")

#fully adjusted model
model2_MMR <- glm(vaccinated_5y ~ MHI_vacc3 + ethnicity_5 + region + imd + 
                    mat_age + birth_order_cat + flurisk+ gender_child, data = df3, family = "binomial")
model2_MMR_print <- model_table(model = model2_MMR, model_name = "Model2   MMR2")



#####creating a big output file
outputs <- as.data.table(rbind(model1_crude_print, model1_DTP_print, model2_DTP_print,
                               model_crude_PCV_print,model1_PCV_print, model2_PCV_print,
                               model_crude_MMR_print, model1_MMR_print,model2_MMR_print ))


outputs[, OR_CI := paste0(Estimate, " (", lci, "-", uci, ")")] 

write.table(outputs, paste0(results, "log_regression_coverage_prescriptions_CC.csv"), sep = "\t", row.names = T,
            dec = ".")

#################################################################################
#Sensitivity analysis by calendar year, just for fully adjusted models
#born 2006-2010
earlydf1 <- df1[year(deldate) <= 2010]
earlydf2 <- df2[year(deldate) <= 2010]
earlydf3 <- df3[year(deldate) <= 2010]

#born 2011-2015
latedf1 <- df1[year(deldate) > 2010]
latedf2 <- df2[year(deldate) > 2010]
latedf3 <- df3[year(deldate) > 2010]

##early year estimates
#fully adjusted model
DTP1_early <- glm(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region + imd + 
                    mat_age + birth_order_cat + flurisk+ gender_child, data = earlydf1, family = "binomial")
PCV3_early <- glm(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region + imd + 
                    mat_age + birth_order_cat + flurisk+ gender_child, data = earlydf2, family = "binomial")
MMR2_early <- glm(vaccinated_5y ~ MHI_vacc3 + ethnicity_5 + region + imd + 
                    mat_age + birth_order_cat + flurisk+ gender_child, data = earlydf3, family = "binomial")

#late estimates
DTP1_late <- glm(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region + imd + 
                   mat_age + birth_order_cat + flurisk+ gender_child, data = latedf1, family = "binomial")
PCV3_late <- glm(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region + imd + 
                   mat_age + birth_order_cat + flurisk+ gender_child, data = latedf2, family = "binomial")
MMR2_late <- glm(vaccinated_5y ~ MHI_vacc3 + ethnicity_5 + region + imd + 
                   mat_age + birth_order_cat + flurisk+ gender_child, data = latedf3, family = "binomial")

#calendar year as a predictor
df1[, cal_year := year(deldate) + 1]
df2[, cal_year := year(deldate) + 2]
df3[, cal_year := year(deldate) + 5]

DTP1_year <- glm(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region + imd + 
                   mat_age + birth_order_cat + flurisk+ gender_child + cal_year, data = df1, family = "binomial")
PCV3_year <- glm(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region + imd + 
                   mat_age + birth_order_cat + flurisk+ gender_child + cal_year, data = df2, family = "binomial")
MMR2_year <- glm(vaccinated_5y ~ MHI_vacc3 + ethnicity_5 + region + imd + 
                   mat_age + birth_order_cat + flurisk+ gender_child + cal_year, data = df3, family = "binomial")


print1 <- model_table(model = DTP1_early, model_name = "DTP early")
print2 <- model_table(model = PCV3_early, model_name = "PCV early")
print3 <- model_table(model = MMR2_early, model_name = "MMR early")

print4 <- model_table(model = DTP1_late, model_name = "DTP late")
print5 <- model_table(model = PCV3_late, model_name = "PCV late")
print6 <- model_table(model = MMR2_late, model_name = "MMR late")

print7 <- model_table(model = DTP1_year, model_name = "DTP year")
print8 <- model_table(model = PCV3_year, model_name = "PCV year")
print9 <- model_table(model = MMR2_year, model_name = "MMR year")

output <- as.data.table(rbind(print1, print2, print3, 
                              print4, print5, print6,
                              print7, print8, print9))
output <- output[, OR_CI := paste0(Estimate, " (", lci, "-", uci, ")")] 

write.table(output, paste0(results, "log_regression_coverage_calendar_year_CC.csv"), sep = "\t", row.names = T,
            dec = ".")

################################################################################
#MIXED EFFECTS MODELS
library(lme4)

#sibling
DTP1_sib <- lme4::glmer(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region + imd + 
                          mat_age + birth_order_cat + flurisk+ gender_child + (1|patid), 
                        data = df1, family = "binomial", nAGQ=0)
PCV3_sib <- lme4::glmer(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region + imd + 
                          mat_age + birth_order_cat + flurisk+ gender_child + (1|patid),
                        data = df2, family = "binomial", nAGQ=0)
MMR2_sib <- lme4::glmer(vaccinated_5y ~ MHI_vacc3 + ethnicity_5 + region + imd + 
                          mat_age + birth_order_cat + flurisk+ gender_child + (1|patid),
                        data = df3, family = "binomial", nAGQ=0)

#GP
DTP1_gp <- lme4::glmer(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region + imd + 
                         mat_age + birth_order_cat + flurisk+ gender_child + (1|pracid), 
                       data = df1, family = "binomial", nAGQ=0)
PCV3_gp <- lme4::glmer(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region + imd + 
                         mat_age + birth_order_cat + flurisk+ gender_child + (1|pracid),
                       data = df2, family = "binomial", nAGQ=0)
MMR2_gp <- lme4::glmer(vaccinated_5y ~ MHI_vacc3+ ethnicity_5 + region + imd + 
                         mat_age + birth_order_cat + flurisk+ gender_child + (1|pracid),
                       data = df3, family = "binomial", nAGQ=0)


#both
DTP1_rand <- lme4::glmer(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region + imd + 
                           mat_age + birth_order_cat + flurisk+ gender_child + 
                           (1|patid) + (1|pracid), 
                         data = df1, family = "binomial", nAGQ=0)
PCV3_rand <- lme4::glmer(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region + imd + 
                           mat_age + birth_order_cat + flurisk+ gender_child +
                           (1|patid) + (1|pracid),
                         data = df2, family = "binomial", nAGQ=0)
MMR2_rand <- lme4::glmer(vaccinated_5y ~ MHI_vacc3 + ethnicity_5 + region + imd + 
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

write.table(outputs, paste0(results, "log_regression_coverage_glm_CC.csv"), sep = "\t", row.names = T,
            dec = ".")

###################only with simpler model
#sibling
DTP1_sib <- lme4::glmer(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region + imd + 
                          (1|patid), 
                        data = df1, family = "binomial")
PCV3_sib <- lme4::glmer(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region + imd + 
                          (1|patid),
                        data = df2, family = "binomial")
MMR2_sib <- lme4::glmer(vaccinated_5y ~ MHI_vacc3 + ethnicity_5 + region + imd + 
                          (1|patid),
                        data = df3, family = "binomial")

#GP
DTP1_gp <- lme4::glmer(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region + imd + 
                         (1|pracid), 
                       data = df1, family = "binomial")
PCV3_gp <- lme4::glmer(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region + imd + 
                         (1|pracid),
                       data = df2, family = "binomial")
MMR2_gp <- lme4::glmer(vaccinated_5y ~ MHI_vacc3 + ethnicity_5 + region + imd + 
                         (1|pracid),
                       data = df3, family = "binomial")


#both
DTP1_rand <- lme4::glmer(vaccinated_1y ~ MHI_vacc1 + ethnicity_5 + region + imd + 
                           mat_age + birth_order_cat + flurisk+ gender_child + 
                           (1|patid) + (1|pracid), 
                         data = df1, family = "binomial")
PCV3_rand <- lme4::glmer(vaccinated_2y ~ MHI_vacc2 + ethnicity_5 + region + imd + 
                           mat_age + birth_order_cat + flurisk+ gender_child +
                           (1|patid) + (1|pracid),
                         data = df2, family = "binomial")
MMR2_rand <- lme4::glmer(vaccinated_5y ~ MHI_vacc3 + ethnicity_5 + region + imd + 
                           
                           (1|patid) + (1|pracid),
                         data = df3, family = "binomial")









#crude models
model_crude_DTP<- lme4::glmer(vaccinated_1y ~ MHI_vacc1 + (1|patid) + (1|pracid),
                              data = df1, family = "binomial", nAGQ=0) #236042.5  124466.5 
model_crude_PCV<- lme4::glmer(vaccinated_2y ~ MHI_vacc2 + (1|patid) + (1|pracid),
                              data = df2, family = "binomial", nAGQ=0) #199980.8 188022.2 
model_crude_MMR<- lme4::glmer(vaccinated_5y ~ MHI_vacc3 + (1|patid) + (1|pracid),
                              data = df3, family = "binomial", nAGQ=0) #165057.7 150665.7 

model_crude_DTP_print <- model_table_mixed(model = model_crude_DTP, model_name = "Crude model DTP1")
model_crude_PCV_print <- model_table_mixed(model = model_crude_PCV, model_name = "Crude model PCV3")
model_crude_MMR_print <- model_table_mixed(model = model_crude_MMR, model_name = "Crude model MMR2")

#half adjusted models
model1_DTP<- lme4::glmer(vaccinated_1y ~ MHI_vacc1 + region + ethnicity_5 + 
                           imd +(1|patid)+ (1|pracid),
                         data = df1, family = "binomial", nAGQ=0)
model1_PCV<- lme4::glmer(vaccinated_2y ~ MHI_vacc2 +  region + ethnicity_5 + 
                           imd +(1|patid)+ (1|pracid),
                         data = df2, family = "binomial", nAGQ=0)
model1_MMR<- lme4::glmer(vaccinated_5y ~ MHI_vacc3 +  region + ethnicity_5 + 
                           imd + (1|patid)+ (1|pracid),
                         data = df3, family = "binomial", nAGQ=0)

model1_DTP_print <- model_table_mixed(model = model1_DTP, model_name = "Model1 DTP1")
model1_PCV_print <- model_table_mixed(model = model1_PCV, model_name = "Model1 PCV3")
model1_MMR_print <- model_table_mixed(model = model1_MMR, model_name = "Model1 MMR2")



#saving all the outputs in one file
outputs <- rbind(model_crude_DTP_print, 
                 model_crude_PCV_print, 
                 model_crude_MMR_print,
                 model1_DTP_print,
                 model1_PCV_print,
                 model1_MMR_print)

outputs[, OR_CI := paste0(Estimate, " (", lci, "-", uci, ")")]

write.table(outputs, paste0(results, "log_regression_coverage_glm_CC.csv"), sep = "\t", row.names = T,
            dec = ".")


#fully adjusted model
model2_DTP<- lme4::glmer(vaccinated_1y ~ MHI_vacc1 + region + ethnicity_5 + 
                           imd + mat_age + birth_order_cat + gender_child +
                           flurisk +  (1|patid)+ (1|pracid),
                         data = df1, family = "binomial", nAGQ=0)
model2_PCV<- lme4::glmer(vaccinated_2y ~ MHI_vacc2 +  region + ethnicity_5 + 
                           imd + mat_age + birth_order_cat + gender_child +
                           flurisk + (1|patid)+ (1|pracid),
                         data = df2, family = "binomial", nAGQ=0)
model2_MMR<- lme4::glmer(vaccinated_5y ~ MHI_vacc3 +  region + ethnicity_5 + 
                           imd + mat_age + birth_order_cat + gender_child +
                           flurisk + (1|patid)+ (1|pracid),
                         data = df3, family = "binomial", nAGQ=0)

model2_DTP_print <- model_table_mixed(model = model2_DTP, model_name = "Model2 DTP1")
model2_PCV_print <- model_table_mixed(model = model2_PCV, model_name = "Model2 PCV3")
model2_MMR_print <- model_table_mixed(model = model2_MMR, model_name = "Model2 MMR2")

#saving all the outputs in one file
outputs <- rbind(model2_DTP_print,
                 model2_PCV_print,
                 model2_MMR_print)

outputs[, OR_CI := paste0(Estimate, " (", lci, "-", uci, ")")]

write.table(outputs, paste0(results, "log_regression_coverage_glm_appendix_CC.csv"), sep = "\t", row.names = T,
            dec = ".")

