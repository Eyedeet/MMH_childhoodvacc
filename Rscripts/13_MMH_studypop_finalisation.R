#Study population finalisation
pop <- read_parquet(paste0(datafiles, "studypop_all_exposures_cat.parquet"))
babies <- read_parquet(paste0(datafiles, "children_studypop.parquet"))

length(unique(pop$patid)) #421750
length(unique(babies$patid)) #535689


#adding the follow-up time of the children
babies <- babies[, list(patid, deldate, age_startfu, age_endfu, start_fu, end_fu)]
babies[,babypatid := patid]
babies[, patid := NULL]
babies[,age_startfu_baby := age_startfu]
babies[, age_startfu := NULL]
babies[,age_endfu_baby := age_endfu]
babies[, age_endfu := NULL]
babies[,start_fu_baby := start_fu]
babies[, start_fu := NULL]
babies[,end_fu_baby := end_fu]
babies[, end_fu := NULL]

tmp <- merge(pop, babies, by = c("babypatid", "deldate"), all.x = T)

length(unique(tmp$patid)) #421750
length(unique(tmp$babypatid)) #535904

####----excluding the children with unrealistic vaccination recording
#MMR
MMR <- read_parquet(paste0(datafiles, "MMR_outcome.parquet"))

MMR[, babypatid := patid]
MMR[, patid := NULL]


tmp1 <- merge(tmp, MMR, by = c("babypatid", "deldate"), all.x = T)
length(unique(tmp1$patid)) #421750
length(unique(tmp1$babypatid)) #535904

#DTP
DTP <- read_parquet(paste0(datafiles, "DTP_outcome.parquet"))
DTP[, babypatid := patid]
DTP[, patid := NULL]

tmp2 <- merge(tmp, DTP, by = c("babypatid", "deldate"), all.x = T)
length(unique(tmp2$patid)) #421750
length(unique(tmp2$babypatid)) #535904

#PCV
PCV <- read_parquet(paste0(datafiles, "PCV_outcome.parquet"))
PCV[, babypatid := patid]
PCV[, patid := NULL]

tmp3 <- merge(tmp, PCV, by = c("babypatid", "deldate"), all.x = T)
length(unique(tmp3$patid)) #421750
length(unique(tmp3$babypatid)) #535904

###adding dataframes and excluding the children with incorrect date of birth
df <- rbind(tmp1, tmp2, tmp3)

ex1<- read_parquet(paste0(datafiles, "children_ex_MMR.parquet"))
ex2<- read_parquet(paste0(datafiles, "children_ex_DTP.parquet"))
ex3<- read_parquet(paste0(datafiles, "children_ex_PCV.parquet"))
ex <- unique(c(ex1$patid, ex2$patid, ex3$patid))
length(ex) #1212 children will be removed

length(unique(df$babypatid)) #535904
df <- df[!babypatid %chin% ex]
length(unique(df$babypatid))#534692

rm(ex1)
rm(ex2)
rm(ex3)
rm(tmp1)
rm(tmp2)
rm(tmp3)


####---adding flu risk groups for SMI
csv_files <- list.files(paste0(codelists, "mental_health/"))
for(i in 1:length(csv_files)){
  csv_files[[i]] <- paste(c(codelists, "mental_health/", csv_files[[i]]),
                          collapse="")
}

SMI_codelist <- as.data.table(read.csv(csv_files[[2]]))
SMI_codelist[, medcodeid := MedCodeId]
SMI_codelist[, MedCodeId := NULL ]
SMI_codelist$medcodeid <- as.character(SMI_codelist$medcodeid)
SMI_codelist <- unique(SMI_codelist)

setwd(mothers_parquet)
obs.files<- list.files(path = mothers_parquet, pattern = "\\Observation")
df_SMI <- extractor_med(list.files = obs.files, codelist = SMI_codelist)
df_SMI <- df_SMI[!is.na(medcodeid)]
length(unique(df_SMI$patid)) #1204
df_SMI[, obsdate := as.Date(obsdate, format = "%d/%m/%Y")]
df_SMI[, patid := as.character(patid)]
#find out whether SMI diagnosis came before start of pregnancy
tmp <- unique(df[, list(patid, babypatid, pregstart)])
tmp <- merge(df_SMI, tmp, by = c("patid"),  all.x = T)
tmp <- tmp[obsdate <= pregstart]
length(unique(tmp$patid))

df[babypatid %chin% tmp$babypatid, flurisk := 1]
rm(tmp)

write_parquet(df, paste0(datafiles, "final_studypop_small.parquet"))


#adding the gender of the children
child_info <- data.table(read_parquet(paste0(children_parquet, "children_Extract_Patient_001.parquet")))
child_info[, babypatid := as.character(patid)]
child_info[, gender_child := gender]
child_info <- child_info[, list(babypatid, gender_child)]

df<- merge(df, child_info, by = "babypatid", all.x = T)
tabyl(df$gender_child)

#old child data
pat.files <-  list.files(path = children_parquet_old, pattern = "\\Patient")
child_info <- data.table()
for(i in 1:length(pat.files)){
  
  tmp <- data.table(read_parquet(paste0(children_parquet_old, pat.files[[i]])))
  child_info <- rbind(child_info, tmp)
}
child_info[, babypatid := as.character(patid)]
child_info[, gender_child := gender]
child_info <- child_info[, list(babypatid, gender_child)]

df1 <- df[is.na(gender_child)]
df2 <- df[!is.na(gender_child)]

df1[, gender_child := NULL]
df1 <- merge(df1, child_info, by ="babypatid", all.x=T)
df <- rbind(df1, df2)
tabyl(df$gender_child)
# df$gender_child       n      percent
# 1 1878883 5.116540e-01
# 2 1793269 4.883397e-01
# 3      23 6.263318e-06
write_parquet(df, paste0(datafiles, "final_studypop_small.parquet"))

#ensuring min follow up of the children from age of under 40 days to 
#make sure enough gollow up for vaccine uptake
df <- df[age_startfu_baby <= 40]
length(unique(df$babypatid)) #397519
length(unique(df$patid)) #330199
 
#proper categories of the variables
df[, age_birth := year(deldate)-yob]
 df[is.na(ethnicity_5), ethnicity_5 := 5]
df[, ethnicity_5 := factor(ethnicity_5, levels = c(0, 1, 2, 3, 4, 5),
                            labels = c("White", "South Asian", "Black",
                                       "Other", "Mixed", "Unknown"))]
 
df[, imd := factor(imd, levels = c( 1, 2, 3, 4, 5),
                    labels = c("1 (Least deprived)", "2", "3",
                               "4", "5 (Most deprived)"),  ordered = T)]
 
df[, region := factor(region, levels = c( 1, 2, 3, 4, 5, 6, 7, 8, 9),
                       labels = c("North East", 
                                  "North West", 
                                  "Yorkshire & The Humber", 
                                  "East Midland", 
                                  "West Midlands", 
                                  "East of England", 
                                  "London", 
                                  "South East",
                                  "South West"))]

write_parquet(df, paste0(datafiles, "final_studypop_small_FU_req.parquet"))


################################################################################
####Creating the study population for Mental health exposure
#all types of observations but no prescriptions
df <- read_parquet(paste0(datafiles, "final_studypop_small_FU_req.parquet"))
nrow(df) #2,789,841
length(unique(df$patid))#330199
length(unique(df$babypatid))#397519


df<- df[age_startfu_baby <= age_endfu_baby]
nrow(df) #2789838
length(unique(df$babypatid))#397518

df[age_birth < 20, mat_age := "<20"]
df[age_birth >= 20 & age_birth <30, mat_age := "20-29"]
df[age_birth >= 30 & age_birth <40, mat_age := "30-39"]
df[age_birth >=40, mat_age := ">40"]
df[, mat_age:= as.factor(mat_age)]
df[birthorder == 1, birth_order_cat := "1"]
df[birthorder == 2, birth_order_cat := "2"]
df[birthorder == 3, birth_order_cat := "3"]
df[birthorder > 3, birth_order_cat := ">3r"]
df[, birth_order_cat := factor(birth_order_cat)]
df[, mat_age := relevel(mat_age, ref = "20-29")]
df[, birth_order_cat := relevel(birth_order_cat, ref = "1")]

df[, MHI_vacc1_ex := NULL]
df[, MHI_vacc2_ex := NULL]
df[, MHI_vacc3_ex := NULL]
df[, MHI_vacc1_any := NULL]
df[, MHI_vacc2_any := NULL]
df[, MHI_vacc3_any := NULL]
df[, MHI_vacc1 := NULL]
df[, MHI_vacc2 := NULL]
df[, MHI_vacc3 := NULL]

tmp <- read_parquet(paste0(datafiles, "studypop_MHI_all_ind.parquet"))
tmp <- tmp[, list(patid, babypatid, MHI_vacc1, MHI_vacc2, MHI_vacc3)]
df1 <- merge(df, tmp, by = c("patid", "babypatid"), all.x = T)
nrow(df1)
write_parquet(df1, paste0(datafiles, "final_studypop_all_ind.parquet"))
rm(df1)

#only diagnoses
tmp <- read_parquet(paste0(datafiles, "studypop_MHI_diagnosis_only.parquet"))
tmp <- tmp[, list(patid, babypatid, MHI_vacc1, MHI_vacc2, MHI_vacc3)]
df2 <- merge(df, tmp, by = c("patid", "babypatid"), all.x = T)
nrow(df2)
write_parquet(df2, paste0(datafiles, "final_studypop_diagnoses_only.parquet"))
rm(df2)

#plus prescription
tmp <- read_parquet(paste0(datafiles, "studypop_MHI_all_pluspres.parquet"))
tmp <- tmp[, list(patid, babypatid, MHI_vacc1, MHI_vacc2, MHI_vacc3)]
df3 <- merge(df, tmp, by = c("patid", "babypatid"), all.x = T)
nrow(df3)
write_parquet(df3, paste0(datafiles, "final_studypop_all_pluspres.parquet"))
rm(df3)


###############################################################################
#Creating one united dataset
df <- read_parquet(paste0(datafiles, "final_studypop_small_FU_req.parquet"))
nrow(df) #2,789,841
length(unique(df$patid))#330199
length(unique(df$babypatid))#397519


df<- df[age_startfu_baby <= age_endfu_baby]
nrow(df) #2789838
length(unique(df$babypatid))#397518

df[age_birth < 20, mat_age := "<20"]
df[age_birth >= 20 & age_birth <30, mat_age := "20-29"]
df[age_birth >= 30 & age_birth <40, mat_age := "30-39"]
df[age_birth >=40, mat_age := ">40"]
df[, mat_age:= as.factor(mat_age)]
df[birthorder == 1, birth_order_cat := "1"]
df[birthorder == 2, birth_order_cat := "2"]
df[birthorder == 3, birth_order_cat := "3"]
df[birthorder > 3, birth_order_cat := ">3r"]
df[, birth_order_cat := factor(birth_order_cat)]
df[, mat_age := relevel(mat_age, ref = "20-29")]
df[, birth_order_cat := relevel(birth_order_cat, ref = "1")]

df[, MHI_vacc1_ex := NULL]
df[, MHI_vacc2_ex := NULL]
df[, MHI_vacc3_ex := NULL]
df[, MHI_vacc1_any := NULL]
df[, MHI_vacc2_any := NULL]
df[, MHI_vacc3_any := NULL]
df[, MHI_vacc1 := NULL]
df[, MHI_vacc2 := NULL]
df[, MHI_vacc3 := NULL]

tmp <- read_parquet(paste0(datafiles, "studypop_MHI_all_ind.parquet"))
tmp <- tmp[, list(patid, babypatid, MHI_vacc1, MHI_vacc2, MHI_vacc3)]
df1 <- merge(df, tmp, by = c("patid", "babypatid"), all.x = T)

tmp <- read_parquet(paste0(datafiles, "studypop_MHI_diagnosis_only.parquet"))
tmp[, MHI_diag1 := MHI_vacc1]
tmp[, MHI_diag2 := MHI_vacc2]
tmp[, MHI_diag3 := MHI_vacc3]
tmp <- tmp[, list(patid, babypatid, MHI_diag1, MHI_diag2, MHI_diag3)]
df2 <- merge(df1, tmp, by = c("patid", "babypatid"), all.x = T)

tmp <- read_parquet(paste0(datafiles, "studypop_MHI_all_pluspres.parquet"))
tmp[, MHI_pres1 := MHI_vacc1]
tmp[, MHI_pres2 := MHI_vacc2]
tmp[, MHI_pres3 := MHI_vacc3]
tmp <- tmp[, list(patid, babypatid, MHI_pres1, MHI_pres2, MHI_pres3)]
df3 <- merge(df2, tmp, by = c("patid", "babypatid"), all.x = T)
head(df3)

write_parquet(df3, paste0(datafiles, "final_studypop_sensitivity_anal.parquet"))
