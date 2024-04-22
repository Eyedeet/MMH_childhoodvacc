###Script to generate maternal mental health outcomes
#--- reading in the the code lists for MMH 
csv_files <- list.files(paste0(codelists, "mental_health_sens/"))
for(i in 1:length(csv_files)){
  csv_files[[i]] <- paste(c(codelists, "mental_health_sens/", csv_files[[i]]),
                          collapse="")
}


#reading in the code lists
CMD_codelist <-as.data.table(read.csv(csv_files[[1]]))
CMD_codelist$medcodeid <- as.character(CMD_codelist$medcodeid)

SMI_codelist <- as.data.table(read.csv(csv_files[[2]]))
SMI_codelist[, medcodeid := MedCodeId]
SMI_codelist[, MedCodeId := NULL ]
SMI_codelist$medcodeid <- as.character(SMI_codelist$medcodeid)

SUD_codelist <- as.data.table(read.csv(csv_files[[3]]))
SUD_codelist[, medcodeid := MedCodeId]
SUD_codelist[, MedCodeId := NULL ]
SUD_codelist$medcodeid <- as.character(SUD_codelist$medcodeid)

#removing duplicates
#removing the duokicaated in the code lists
nrow(CMD_codelist) #426
CMD_codelist <- unique(CMD_codelist, by = "medcodeid")
nrow(CMD_codelist) #414

nrow(SUD_codelist) #622
SUD_codelist <- unique(SUD_codelist,  by = "medcodeid")
nrow(SUD_codelist) #622

nrow(SMI_codelist) #375
SMI_codelist <- unique(SMI_codelist,  by = "medcodeid")
nrow(SMI_codelist) #367


####filtering the different diagnosis
setwd(mothers_parquet)
obs.files<- list.files(path = mothers_parquet, pattern = "\\Observation")

#CMD
df_CMD <- extractor_med(list.files = obs.files, codelist = CMD_codelist)
df_CMD <- df_CMD[!is.na(medcodeid)]
length(unique(df_CMD$patid)) #268720

df_SUD <- extractor_med(list.files = obs.files, codelist = SUD_codelist)
df_SUD <- df_SUD[!is.na(medcodeid)]
length(unique(df_SUD$patid)) #28514

df_SMI <- extractor_med(list.files = obs.files, codelist = SMI_codelist)
df_SMI <- df_SMI[!is.na(medcodeid)]
length(unique(df_SMI$patid))#1204

#reformatting
df_CMD$patid <- as.character(df_CMD$patid)
df_SUD$patid <- as.character(df_SUD$patid)
df_SMI$patid <- as.character(df_SMI$patid)
df_CMD$pracid <- as.character(df_CMD$pracid)
df_SUD$pracid <- as.character(df_SUD$pracid)
df_SMI$pracid <- as.character(df_SMI$pracid)

df_CMD$obsdate <- as.Date(df_CMD$obsdate, format = "%d/%m/%Y")
df_SUD$obsdate <- as.Date(df_SUD$obsdate, format = "%d/%m/%Y")
df_SMI$obsdate <- as.Date(df_SMI$obsdate, format = "%d/%m/%Y")


df_CMD <- df_CMD[, list(patid, pracid, obsdate, diagnostic, symptom, ref_th, other)]
df_CMD <- df_CMD[, CMD:= 1]
df_CMD <- unique(df_CMD, by = c("patid", "pracid", "obsdate"))
df_SUD <- df_SUD[, list(patid, pracid, obsdate, diagnostic, symptom, ref_th, other)]
df_SUD <- df_SUD[, SUD:= 1]
df_SUD <- unique(df_SUD, by = c("patid", "pracid", "obsdate"))
df_SMI <- df_SMI[, list(patid, pracid, obsdate, diagnostic, symptom, ref_th, other)]
df_SMI <- df_SMI[, SUD:= 1]
df_SMI <- unique(df_SMI, by = c("patid", "pracid", "obsdate"))

#merging to maternal data
###establishing whether mothers had any MHI before FU time of outcome
pop <- read_parquet(paste0(datafiles, "mothers_studypop_all_cov.parquet"))


CMD<- merge(pop, df_CMD, by = c("patid", "pracid"), all.x = T)
SMI<- merge(pop, df_SMI, by = c("patid", "pracid"), all.x = T)
SUD<- merge(pop, df_SUD, by = c("patid", "pracid"), all.x = T)

#check whether obsdate is sensible
length(unique(CMD$patid)) #421750
CMD<- CMD[year(obsdate)>=yob| is.na(obsdate)]
length(unique(CMD$patid)) #421749

length(unique(SMI$patid)) #421750
SMI <- SMI[year(obsdate)>=yob| is.na(obsdate)]
length(unique(SMI$patid)) #421750

length(unique(SUD$patid)) #421750
SUD <- SUD[year(obsdate)>=yob| is.na(obsdate)]
length(unique(SUD$patid)) #421750


#FU for first vaccine, before age 8 weeks/ days
CMD[, age_vacc1 := deldate+dweeks(8)]
SMI[, age_vacc1 := deldate+dweeks(8)]
SUD[, age_vacc1 := deldate+dweeks(8)]
#FU for second vaccine, before age 1 year
CMD[, age_vacc2 := deldate+ddays(365)]
SMI[, age_vacc2 := deldate+ddays(365)]
SUD[, age_vacc2 := deldate+ddays(365)]
#FU for third vaccine, before age 18 months
CMD[, age_vacc3 := deldate+dmonths(18)]
SMI[, age_vacc3 := deldate+dmonths(18)]
SUD[, age_vacc3 := deldate+dmonths(18)]

#checking whether any diagnosis before vacc due date
CMD[!is.na(obsdate), CMD_vacc1 := obsdate <age_vacc1]
CMD[!is.na(obsdate), CMD_vacc2 := obsdate <age_vacc2]
CMD[!is.na(obsdate), CMD_vacc3 := obsdate <age_vacc3]
SMI[!is.na(obsdate), SMI_vacc1 := obsdate <age_vacc1]
SMI[!is.na(obsdate), SMI_vacc2 := obsdate <age_vacc2]
SMI[!is.na(obsdate), SMI_vacc3 := obsdate <age_vacc3]
SUD[!is.na(obsdate), SUD_vacc1 := obsdate <age_vacc1]
SUD[!is.na(obsdate), SUD_vacc2 := obsdate <age_vacc2]
SUD[!is.na(obsdate), SUD_vacc3 := obsdate <age_vacc3]


CMD[diagnostic == 1, code_cat := "diag"]
CMD[symptom == 1, code_cat := "symptom"]
CMD[ref_th == 1, code_cat := "ref/therapy"]
CMD[other == 1, code_cat := "other"]

SMI[diagnostic == 1, code_cat := "diag"]
SMI[symptom == 1, code_cat := "symptom"]
SMI[ref_th == 1, code_cat := "ref/therapy"]
SMI[other == 1, code_cat := "other"]


SUD[diagnostic == 1, code_cat := "diag"]
SUD[symptom == 1, code_cat := "symptom"]
SUD[ref_th == 1, code_cat := "ref/therapy"]
SUD[other == 1, code_cat := "other"]


write_parquet(CMD, paste0(datafiles, "CMD_exp.parquet"))
write_parquet(SUD, paste0(datafiles, "SUD_exp.parquet"))
write_parquet(SMI, paste0(datafiles, "SMI_exp.parquet"))


#count the number of record types per conditions
tab1 <- CMD %>%
  filter(CMD_vacc1 == T) %>%
  tabyl(code_cat)%>%
  mutate(percent = round((percent*100), digits = 2))
tab2 <- CMD %>%
  filter(CMD_vacc2 == T) %>%
  tabyl(code_cat)%>%
  mutate(percent = round((percent*100), digits = 2))
tab3 <- CMD %>%
  filter(CMD_vacc3 == T) %>%
  tabyl(code_cat)%>%
  mutate(percent = round((percent*100), digits = 2))

tab_CMD <- cbind(tab1, tab2, tab3)
write.table(tab_CMD, paste0(results, "CMD_obs_table.csv"), sep = "\t", row.names = T,
            dec = ".")

tab1 <- SMI %>%
  filter(SMI_vacc1 == T) %>%
  tabyl(code_cat)%>%
  mutate(percent = round((percent*100), digits = 2))
tab2 <- SMI %>%
  filter(SMI_vacc2 == T) %>%
  tabyl(code_cat)%>%
  mutate(percent = round((percent*100), digits = 2))
tab3 <- SMI %>%
  filter(SMI_vacc3 == T) %>%
  tabyl(code_cat)%>%
  mutate(percent = round((percent*100), digits = 2))

tab_SMI <- cbind(tab1, tab2, tab3)
write.table(tab_SMI, paste0(results, "SMI_obs_table.csv"), sep = "\t", row.names = T,
            dec = ".")

tab1 <- SUD %>%
  filter(SUD_vacc1 == T) %>%
  tabyl(code_cat)%>%
  mutate(percent = round((percent*100), digits = 2))
tab2 <- SUD %>%
  filter(SUD_vacc2 == T) %>%
  tabyl(code_cat)%>%
  mutate(percent = round((percent*100), digits = 2))
tab3 <- SUD %>%
  filter(SUD == T) %>%
  tabyl(code_cat)%>%
  mutate(percent = round((percent*100), digits = 2))

tab_SUD <- cbind(tab1, tab2, tab3)
write.table(tab_SUD, paste0(results, "SUD_obs_table.csv"), sep = "\t", row.names = T,
            dec = ".")


write_parquet(CMD, paste0(datafiles, "CMD_exp.parquet"))
write_parquet(SUD, paste0(datafiles, "SUD_exp.parquet"))
write_parquet(SMI, paste0(datafiles, "SMI_exp.parquet"))




######################################################################
CMD <- read_parquet(paste0(datafiles, "CMD_exp.parquet"))
SUD <- read_parquet(paste0(datafiles, "SUD_exp.parquet"))
SMI <- read_parquet(paste0(datafiles, "SMI_exp.parquet"))



#OUTCOME number one - only diagnoses count
CMD <- CMD[diagnostic == 1]
SMI <- SMI[diagnostic == 1]
SUD <- SUD[diagnostic == 1]

##check whether there was any diagnosis/symptom before vaccination date
CMD[, vacc1_CMD := any(CMD_vacc1 == T), by = c("patid", "babypatid")]
CMD[, vacc2_CMD := any(CMD_vacc2 == T), by = c("patid", "babypatid")]
CMD[, vacc3_CMD := any(CMD_vacc3 == T), by = c("patid", "babypatid")]

SMI[, vacc1_SMI := any(SMI_vacc1 == T), by = c("patid", "babypatid")]
SMI[, vacc2_SMI := any(SMI_vacc2 == T), by = c("patid", "babypatid")]
SMI[, vacc3_SMI := any(SMI_vacc3 == T), by = c("patid", "babypatid")]

SUD[, vacc1_SUD := any(SUD_vacc1 == T), by = c("patid", "babypatid")]
SUD[, vacc2_SUD := any(SUD_vacc2 == T), by = c("patid", "babypatid")]
SUD[, vacc3_SUD := any(SUD_vacc3 == T), by = c("patid", "babypatid")]

CMD <- unique(CMD[, list(patid, babypatid, vacc1_CMD, vacc2_CMD, vacc3_CMD)])
SMI <- unique(SMI[, list(patid, babypatid, vacc1_SMI, vacc2_SMI, vacc3_SMI)])
SUD <- unique(SUD[, list(patid, babypatid, vacc1_SUD, vacc2_SUD, vacc3_SUD)])

###merging this with pop data
df <- merge(pop, CMD, by = c("patid", "babypatid"), all.x = T)
df <- merge(df, SMI, by = c("patid", "babypatid"), all.x = T )
df <- merge(df, SUD, by = c("patid", "babypatid"), all.x = T )

#categorising the MH exposure in the different combinations
#first vaccination age
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "None"]
df[(vacc1_CMD == T) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "CMD"]
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == T )&
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "Any SMI"]
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == T), MHI_vacc1 := "SUD"]
df[(vacc1_CMD == T) &
     (vacc1_SMI == T) &
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "Any SMI"]
df[(vacc1_CMD == T) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == T), MHI_vacc1 := "CMD & SUD"]
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == T )&
     (vacc1_SUD == T), MHI_vacc1 := "Any SMI"]
df[(vacc1_CMD == T ) &
     (vacc1_SMI == T )&
     (vacc1_SUD == T), MHI_vacc1 := "Any SMI"]

#second vaccination age
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "None"]
df[(vacc2_CMD == T) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "CMD"]
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == T )&
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "Any SMI"]
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == T), MHI_vacc2 := "SUD"]
df[(vacc2_CMD == T) &
     (vacc2_SMI == T) &
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "Any SMI"]
df[(vacc2_CMD == T) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == T), MHI_vacc2 := "CMD & SUD"]
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == T )&
     (vacc2_SUD == T), MHI_vacc2 := "Any SMI"]
df[(vacc2_CMD == T ) &
     (vacc2_SMI == T )&
     (vacc2_SUD == T), MHI_vacc2 := "Any SMI"]

#third vaccination age
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "None"]
df[(vacc3_CMD == T) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "CMD"]
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == T )&
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "Any SMI"]
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == T), MHI_vacc3 := "SUD"]
df[(vacc3_CMD == T) &
     (vacc3_SMI == T) &
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "Any SMI"]
df[(vacc3_CMD == T) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == T), MHI_vacc3 := "CMD & SUD"]
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == T )&
     (vacc3_SUD == T), MHI_vacc3 := "Any SMI"]
df[(vacc3_CMD == T ) &
     (vacc3_SMI == T )&
     (vacc3_SUD == T), MHI_vacc3 := "Any SMI"]


df[,vacc1_CMD := NULL ]
df[,vacc2_CMD := NULL ]
df[,vacc3_CMD := NULL ]
df[,vacc1_SMI := NULL ]
df[,vacc2_SMI := NULL ]
df[,vacc3_SMI := NULL ]
df[,vacc1_SUD := NULL ]
df[,vacc2_SUD := NULL ]
df[,vacc3_SUD := NULL ]

write_parquet(df, paste0(datafiles, "studypop_MHI_diagnosis_only.parquet"))


##############################################################################
#using diagnosis, symptoms, referrals but no prescription for defining MHI
CMD <- read_parquet(paste0(datafiles, "CMD_exp.parquet"))
SUD <- read_parquet(paste0(datafiles, "SUD_exp.parquet"))
SMI <- read_parquet(paste0(datafiles, "SMI_exp.parquet"))


##check whether there was any diagnosis/symptom before vaccination date
CMD[, vacc1_CMD := any(CMD_vacc1 == T), by = c("patid", "babypatid")]
CMD[, vacc2_CMD := any(CMD_vacc2 == T), by = c("patid", "babypatid")]
CMD[, vacc3_CMD := any(CMD_vacc3 == T), by = c("patid", "babypatid")]

SMI[, vacc1_SMI := any(SMI_vacc1 == T), by = c("patid", "babypatid")]
SMI[, vacc2_SMI := any(SMI_vacc2 == T), by = c("patid", "babypatid")]
SMI[, vacc3_SMI := any(SMI_vacc3 == T), by = c("patid", "babypatid")]

SUD[, vacc1_SUD := any(SUD_vacc1 == T), by = c("patid", "babypatid")]
SUD[, vacc2_SUD := any(SUD_vacc2 == T), by = c("patid", "babypatid")]
SUD[, vacc3_SUD := any(SUD_vacc3 == T), by = c("patid", "babypatid")]

CMD <- unique(CMD[, list(patid, babypatid, vacc1_CMD, vacc2_CMD, vacc3_CMD)])
SMI <- unique(SMI[, list(patid, babypatid, vacc1_SMI, vacc2_SMI, vacc3_SMI)])
SUD <- unique(SUD[, list(patid, babypatid, vacc1_SUD, vacc2_SUD, vacc3_SUD)])

###merging this with pop data
df <- merge(pop, CMD, by = c("patid", "babypatid"), all.x = T)
df <- merge(df, SMI, by = c("patid", "babypatid"), all.x = T )
df <- merge(df, SUD, by = c("patid", "babypatid"), all.x = T )

#categorising the MH exposure in the different combinations
#first vaccination age
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "None"]
df[(vacc1_CMD == T) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "CMD"]
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == T )&
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "Any SMI"]
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == T), MHI_vacc1 := "SUD"]
df[(vacc1_CMD == T) &
     (vacc1_SMI == T) &
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "Any SMI"]
df[(vacc1_CMD == T) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == T), MHI_vacc1 := "CMD & SUD"]
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == T )&
     (vacc1_SUD == T), MHI_vacc1 := "Any SMI"]
df[(vacc1_CMD == T ) &
     (vacc1_SMI == T )&
     (vacc1_SUD == T), MHI_vacc1 := "Any SMI"]

#second vaccination age
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "None"]
df[(vacc2_CMD == T) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "CMD"]
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == T )&
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "Any SMI"]
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == T), MHI_vacc2 := "SUD"]
df[(vacc2_CMD == T) &
     (vacc2_SMI == T) &
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "Any SMI"]
df[(vacc2_CMD == T) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == T), MHI_vacc2 := "CMD & SUD"]
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == T )&
     (vacc2_SUD == T), MHI_vacc2 := "Any SMI"]
df[(vacc2_CMD == T ) &
     (vacc2_SMI == T )&
     (vacc2_SUD == T), MHI_vacc2 := "Any SMI"]

#third vaccination age
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "None"]
df[(vacc3_CMD == T) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "CMD"]
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == T )&
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "Any SMI"]
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == T), MHI_vacc3 := "SUD"]
df[(vacc3_CMD == T) &
     (vacc3_SMI == T) &
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "Any SMI"]
df[(vacc3_CMD == T) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == T), MHI_vacc3 := "CMD & SUD"]
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == T )&
     (vacc3_SUD == T), MHI_vacc3 := "Any SMI"]
df[(vacc3_CMD == T ) &
     (vacc3_SMI == T )&
     (vacc3_SUD == T), MHI_vacc3 := "Any SMI"]


df[,vacc1_CMD := NULL ]
df[,vacc2_CMD := NULL ]
df[,vacc3_CMD := NULL ]
df[,vacc1_SMI := NULL ]
df[,vacc2_SMI := NULL ]
df[,vacc3_SMI := NULL ]
df[,vacc1_SUD := NULL ]
df[,vacc2_SUD := NULL ]
df[,vacc3_SUD := NULL ]

write_parquet(df, paste0(datafiles, "studypop_MHI_all_ind.parquet"))

# tabyl(df$MHI_vacc1)
# df$MHI_vacc1      n     percent
# Any SMI    571 0.001065489
# CMD  96826 0.180677883
# CMD & SUD   1766 0.003295366
# None 435525 0.812692199
# SUD   1216 0.002269063

###############################################################################
#Using prescription data in order to use symptom codes
 

#extrating the medication for anxiety and depression
setwd(mothers_parquet)
drug.files<- list.files(path = mothers_parquet, pattern = "\\DrugIssue")
drug_anx <- as.data.table(read_dta("cr_codelist_anxiety_drugs_aurum.dta"))
drug_dep <- as.data.table(read_dta("cr_codelist_depression_drugs_aurum.dta"))

drug_anx$prodcodeid <- as.character(drug_anx$prodcodeid)
drug_dep$prodcodeid <- as.character(drug_dep$prodcodeid)
anx_drugs <- extractor_prod(drug.files, drug_anx)
dep_drugs <- extractor_prod(drug.files, drug_dep)

length(unique(dep_drugs$patid)) #257908
length(unique(anx_drugs$patid)) #251598

write_parquet(dep_drugs, paste0(datafiles, "drug_prescriptions_dep.parquet"))
write_parquet(anx_drugs, paste0(datafiles, "drug_prescriptions_anx.parquet"))
dep_drugs <- read_parquet(paste0(datafiles, "drug_prescriptions_dep.parquet"))
anx_drugs <- read_parquet(paste0(datafiles, "drug_prescriptions_anx.parquet"))



anx_drugs <- anx_drugs[, list(patid, pracid, issuedate)]
dep_drugs <- dep_drugs[, list(patid, pracid, issuedate)]

CMD_drugs <- rbind(anx_drugs, dep_drugs)
CMD_drugs$patid <- as.character(CMD_drugs$patid)
CMD_drugs$pracid <- as.character(CMD_drugs$pracid)
CMD_drugs$issuedate <- as.Date(CMD_drugs$issuedate, format = "%d/%m/%Y")

CMD_drugs <- merge(pop, CMD_drugs, by = c("patid", "pracid"))

#FU for first vaccine, before age 8 weeks/ days
CMD_drugs[, age_vacc1 := deldate+dweeks(8)]
#FU for second vaccine, before age 1 year
CMD_drugs[, age_vacc2 := deldate+ddays(365)]
#FU for third vaccine, before age 18 months
CMD_drugs[, age_vacc3 := deldate+dmonths(18)]

#checking whether any diagnosis before vacc due date
CMD_drugs[!is.na(issuedate), CMD_vacc1 := issuedate <age_vacc1]
CMD_drugs[!is.na(issuedate), CMD_vacc2 := issuedate <age_vacc2]
CMD_drugs[!is.na(issuedate), CMD_vacc3 := issuedate <age_vacc3]

#keep only with at least one relevant prescriptions
CMD_drugs <- CMD_drugs[CMD_vacc1 ==T | CMD_vacc2 == T | CMD_vacc3 == T]

CMD_symp <- CMD[symptom == 1]
CMD_symp <-  CMD_symp[CMD_vacc1 ==T | CMD_vacc2 == T | CMD_vacc3 == T]
CMD_symp <- CMD_symp[, list(patid, pracid, babypatid, CMD_vacc1, CMD_vacc2, CMD_vacc3, obsdate)]
CMD_drugs <- CMD_drugs[, list(patid, pracid, babypatid, CMD_vacc1, CMD_vacc2, CMD_vacc3, issuedate)]

pats_drugs <- unique(CMD_drugs$patid)
pats_symp <- unique(CMD_symp$patid)

patids <- intersect(pats_drugs, pats_symp)
CMD_drugs <- CMD_drugs[patid %chin% patids]
CMD_drugs[, match := NA]
CMD_symp <- CMD_symp[patid %chin% patids]

nrow(CMD_symp)
length(unique(CMD_symp$patid)) #58774
nrow(CMD_drugs) #1956021

tmp <- merge(CMD_symp, CMD_drugs, by = c("patid", "babypatid"), all.x = T, 
             allow.cartesian = T)

tmp[, diff:= interval(obsdate, issuedate) %/% months(1)]
tmp[, match:= diff <= 3 & diff >= -3]

tmp <- tmp[match==T]
tmp[, CMD_vacc1 := CMD_vacc1.x & CMD_vacc1.y]
tmp[, CMD_vacc2 := CMD_vacc2.x & CMD_vacc2.y]
tmp[, CMD_vacc3 := CMD_vacc3.x & CMD_vacc3.y]

tmp <- tmp[, list(patid, babypatid, pracid.x, obsdate,  CMD_vacc1, CMD_vacc2, CMD_vacc3)]
tmp[, pracid := pracid.x]
tmp[, pracid.x := NULL]

CMD <- CMD[, list(patid, babypatid, pracid, obsdate, CMD_vacc1, CMD_vacc2, CMD_vacc3, code_cat)]
CMD <- CMD[code_cat  != "symptom"]
CMD[, code_cat := NULL]

CMD <- rbind(CMD, tmp)

#any true entry by woman 
CMD[, vacc1_CMD := any(CMD_vacc1 == T), by = c("patid", "babypatid")]
CMD[, vacc2_CMD := any(CMD_vacc2 == T), by = c("patid", "babypatid")]
CMD[, vacc3_CMD := any(CMD_vacc3 == T), by = c("patid", "babypatid")]


#other mental health issues to be added
SMI[, vacc1_SMI := any(SMI_vacc1 == T), by = c("patid", "babypatid")]
SMI[, vacc2_SMI := any(SMI_vacc2 == T), by = c("patid", "babypatid")]
SMI[, vacc3_SMI := any(SMI_vacc3 == T), by = c("patid", "babypatid")]

SUD[, vacc1_SUD := any(SUD_vacc1 == T), by = c("patid", "babypatid")]
SUD[, vacc2_SUD := any(SUD_vacc2 == T), by = c("patid", "babypatid")]
SUD[, vacc3_SUD := any(SUD_vacc3 == T), by = c("patid", "babypatid")]

CMD <- unique(CMD[, list(patid, babypatid, vacc1_CMD, vacc2_CMD, vacc3_CMD)])
SMI <- unique(SMI[, list(patid, babypatid, vacc1_SMI, vacc2_SMI, vacc3_SMI)])
SUD <- unique(SUD[, list(patid, babypatid, vacc1_SUD, vacc2_SUD, vacc3_SUD)])


###merging this with pop data
df <- merge(pop, CMD, by = c("patid", "babypatid"), all.x = T)
df <- merge(df, SMI, by = c("patid", "babypatid"), all.x = T )
df <- merge(df, SUD, by = c("patid", "babypatid"), all.x = T )

#categorising the MH exposure in the different combinations
#first vaccination age
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "None"]
df[(vacc1_CMD == T) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "CMD"]
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == T )&
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "Any SMI"]
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == T), MHI_vacc1 := "SUD"]
df[(vacc1_CMD == T) &
     (vacc1_SMI == T) &
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "Any SMI"]
df[(vacc1_CMD == T) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == T), MHI_vacc1 := "CMD & SUD"]
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == T )&
     (vacc1_SUD == T), MHI_vacc1 := "Any SMI"]
df[(vacc1_CMD == T ) &
     (vacc1_SMI == T )&
     (vacc1_SUD == T), MHI_vacc1 := "Any SMI"]

#second vaccination age
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "None"]
df[(vacc2_CMD == T) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "CMD"]
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == T )&
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "Any SMI"]
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == T), MHI_vacc2 := "SUD"]
df[(vacc2_CMD == T) &
     (vacc2_SMI == T) &
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "Any SMI"]
df[(vacc2_CMD == T) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == T), MHI_vacc2 := "CMD & SUD"]
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == T )&
     (vacc2_SUD == T), MHI_vacc2 := "Any SMI"]
df[(vacc2_CMD == T ) &
     (vacc2_SMI == T )&
     (vacc2_SUD == T), MHI_vacc2 := "Any SMI"]

#third vaccination age
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "None"]
df[(vacc3_CMD == T) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "CMD"]
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == T )&
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "Any SMI"]
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == T), MHI_vacc3 := "SUD"]
df[(vacc3_CMD == T) &
     (vacc3_SMI == T) &
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "Any SMI"]
df[(vacc3_CMD == T) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == T), MHI_vacc3 := "CMD & SUD"]
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == T )&
     (vacc3_SUD == T), MHI_vacc3 := "Any SMI"]
df[(vacc3_CMD == T ) &
     (vacc3_SMI == T )&
     (vacc3_SUD == T), MHI_vacc3 := "Any SMI"]


df[,vacc1_CMD := NULL ]
df[,vacc2_CMD := NULL ]
df[,vacc3_CMD := NULL ]
df[,vacc1_SMI := NULL ]
df[,vacc2_SMI := NULL ]
df[,vacc3_SMI := NULL ]
df[,vacc1_SUD := NULL ]
df[,vacc2_SUD := NULL ]
df[,vacc3_SUD := NULL ]

write_parquet(df, paste0(datafiles, "studypop_MHI_all_pluspres.parquet"))

################################################################################
#ADDING THE TIMING OF THE MENTAL HEALTH ISSUE
CMD <- read_parquet(paste0(datafiles, "CMD_exp.parquet"))

#OUTCOME number one - only diagnoses count
CMD <- CMD[diagnostic == 1]

##check whether there was any diagnosis/symptom before vaccination date
CMD[, vacc1_CMD := any(CMD_vacc1 == T), by = c("patid", "babypatid")]
CMD[, vacc2_CMD := any(CMD_vacc2 == T), by = c("patid", "babypatid")]
CMD[, vacc3_CMD := any(CMD_vacc3 == T), by = c("patid", "babypatid")]

CMD <- unique(CMD[, list(patid, babypatid, obsdate, deldate, pregstart,
                         vacc1_CMD, vacc2_CMD, vacc3_CMD,
                         CMD_vacc1, CMD_vacc2, CMD_vacc3)])

#first vaccine
CMD1 <- unique(CMD[CMD_vacc1 == T])
CMD1[, obsdate_CMD := max(obsdate), by = c("patid", "babypatid")]
CMD1 <- unique(CMD1[, list(patid, babypatid, obsdate_CMD, pregstart, deldate)])
#second
CMD2 <- unique(CMD[CMD_vacc2 == T])
CMD2[, obsdate_CMD := max(obsdate), by = c("patid", "babypatid")]
CMD2 <- unique(CMD2[, list(patid, babypatid, obsdate_CMD, pregstart, deldate)])
#third vaccine
CMD3 <- unique(CMD[CMD_vacc3 == T])
CMD3[, obsdate_CMD := max(obsdate), by = c("patid", "babypatid")]
CMD3 <- unique(CMD3[, list(patid, babypatid, obsdate_CMD, pregstart, deldate)])


#categorising the date
#defining timing variable
#vaccine 1
CMD1[is.na(obsdate_CMD), CMD_timing_vac1_diag := "None"]
CMD1[obsdate_CMD < pregstart, CMD_timing_vac1_diag := "History of CMD" ]
CMD1[obsdate_CMD  >=pregstart & obsdate_CMD <deldate, CMD_timing_vac1_diag := "prenatal CMD" ]
CMD1[obsdate_CMD  >= deldate & obsdate_CMD < (deldate+dweeks(8)), CMD_timing_vac1_diag := "postnatal CMD" ]
CMD1[obsdate_CMD >= (deldate+dweeks(8)), CMD_timing_vac1_diag := "None"]

#vaccine 2
CMD2[is.na(obsdate_CMD), CMD_timing_vac2_diag := "None"]
CMD2[obsdate_CMD < pregstart, CMD_timing_vac2_diag := "History of CMD" ]
CMD2[obsdate_CMD  >=pregstart & obsdate_CMD <deldate, CMD_timing_vac2_diag := "prenatal CMD" ]
CMD2[obsdate_CMD  >= deldate & obsdate_CMD < (deldate+ddays(365)), CMD_timing_vac2_diag := "postnatal CMD" ]
CMD2[obsdate_CMD >= (deldate+ddays(365)), CMD_timing_vac2_diag := "None"]

#vaccine 3
CMD3[is.na(obsdate_CMD), CMD_timing_vac3_diag := "None"]
CMD3[obsdate_CMD < pregstart, CMD_timing_vac3_diag := "History of CMD" ]
CMD3[obsdate_CMD  >=pregstart & obsdate_CMD <deldate, CMD_timing_vac3_diag := "prenatal CMD" ]
CMD3[obsdate_CMD  >= deldate & obsdate_CMD < (deldate + dmonths(12)), CMD_timing_vac3_diag := "postnatal CMD" ]
CMD3[obsdate_CMD >= (deldate + dmonths(12)) & obsdate_CMD < (deldate + dmonths(18)) , CMD_timing_vac3_diag := "childhood CMD" ]
CMD3[obsdate_CMD >= (deldate+dmonths(18)), CMD_timing_vac3_diag := "None"]

####
CMD1 <- CMD1[, list(patid, babypatid, CMD_timing_vac1_diag)]
CMD2 <- CMD2[, list(patid, babypatid, CMD_timing_vac2_diag)]
CMD3 <- CMD3[, list(patid, babypatid, CMD_timing_vac3_diag)]


df <- read_parquet(paste0(datafiles, "final_studypop_sensitivity_anal.parquet"))
df <- merge(df, CMD1, by = c("patid", "babypatid"), all.x = T)
df <- merge(df, CMD2, by = c("patid", "babypatid"), all.x = T)
df <- merge(df, CMD3, by = c("patid", "babypatid"), all.x = T)

df[is.na(CMD_timing_vac1_diag), CMD_timing_vac1_diag:="None"]
df[is.na(CMD_timing_vac2_diag), CMD_timing_vac2_diag:="None"]
df[is.na(CMD_timing_vac3_diag), CMD_timing_vac3_diag:="None"]



#sorting out mothers who don't get followed up until vaccination date
#FU for first vaccine, before age 8 weeks/ days
df[, age_vacc1 := deldate+dweeks(8)]
#FU for second vaccine, before age 1 year
df[, age_vacc2 := deldate+ddays(365)]
#FU for third vaccine, before age 18 months
df[, age_vacc3 := deldate+dmonths(18)]

#checking whether any diagnosis before vacc due date
df[regenddate < age_vacc1, CMD_timing_vac1 := NA]
df[regenddate < age_vacc2, CMD_timing_vac2 := NA]
df[regenddate < age_vacc3, CMD_timing_vac3 := NA]

tabyl(df$CMD_timing_vac1_diag)
tabyl(df$CMD_timing_vac2_diag)
tabyl(df$CMD_timing_vac3_diag)

###############################################
#adding presciption
CMD <- read_parquet(paste0(datafiles, "CMD_exp.parquet"))
dep_drugs <- read_parquet(paste0(datafiles, "drug_prescriptions_dep.parquet"))
anx_drugs <- read_parquet(paste0(datafiles, "drug_prescriptions_anx.parquet"))



anx_drugs <- anx_drugs[, list(patid, pracid, issuedate)]
dep_drugs <- dep_drugs[, list(patid, pracid, issuedate)]

CMD_drugs <- rbind(anx_drugs, dep_drugs)
CMD_drugs$patid <- as.character(CMD_drugs$patid)
CMD_drugs$pracid <- as.character(CMD_drugs$pracid)
CMD_drugs$issuedate <- as.Date(CMD_drugs$issuedate, format = "%d/%m/%Y")

CMD_drugs <- merge(pop, CMD_drugs, by = c("patid", "pracid"))

#FU for first vaccine, before age 8 weeks/ days
CMD_drugs[, age_vacc1 := deldate+dweeks(8)]
#FU for second vaccine, before age 1 year
CMD_drugs[, age_vacc2 := deldate+ddays(365)]
#FU for third vaccine, before age 18 months
CMD_drugs[, age_vacc3 := deldate+dmonths(18)]

#checking whether any diagnosis before vacc due date
CMD_drugs[!is.na(issuedate), CMD_vacc1 := issuedate <age_vacc1]
CMD_drugs[!is.na(issuedate), CMD_vacc2 := issuedate <age_vacc2]
CMD_drugs[!is.na(issuedate), CMD_vacc3 := issuedate <age_vacc3]

#keep only with at least one relevant prescriptions
CMD_drugs <- CMD_drugs[CMD_vacc1 ==T | CMD_vacc2 == T | CMD_vacc3 == T]

CMD_symp <- CMD[symptom == 1]
CMD_symp <-  CMD_symp[CMD_vacc1 ==T | CMD_vacc2 == T | CMD_vacc3 == T]
CMD_symp <- CMD_symp[, list(patid, pracid, babypatid, CMD_vacc1, CMD_vacc2, CMD_vacc3, obsdate)]
CMD_drugs <- CMD_drugs[, list(patid, pracid, babypatid, CMD_vacc1, CMD_vacc2, CMD_vacc3, issuedate)]

pats_drugs <- unique(CMD_drugs$patid)
pats_symp <- unique(CMD_symp$patid)

patids <- intersect(pats_drugs, pats_symp)
CMD_drugs <- CMD_drugs[patid %chin% patids]
CMD_drugs[, match := NA]
CMD_symp <- CMD_symp[patid %chin% patids]

nrow(CMD_symp)
length(unique(CMD_symp$patid)) #58774
nrow(CMD_drugs) #1956021

tmp <- merge(CMD_symp, CMD_drugs, by = c("patid", "babypatid"), all.x = T, 
             allow.cartesian = T)

tmp[, diff:= interval(obsdate, issuedate) %/% months(1)]
tmp[, match:= diff <= 3 & diff >= -3]

tmp <- tmp[match==T]
tmp[, CMD_vacc1 := CMD_vacc1.x & CMD_vacc1.y]
tmp[, CMD_vacc2 := CMD_vacc2.x & CMD_vacc2.y]
tmp[, CMD_vacc3 := CMD_vacc3.x & CMD_vacc3.y]
tmp[, pracid := pracid.x]
tmp <- tmp[, list(patid, babypatid, obsdate, issuedate,  CMD_vacc1, CMD_vacc2, CMD_vacc3)]
tmp[, obsdate := max(obsdate, issuedate)]
tmp[, issuedate := NULL]
tmp[, code_cat := "symptom"]
tmp <- merge(pop, tmp, by = c("patid", "babypatid"))

tmp<- tmp[,list(patid, babypatid, pracid, obsdate, CMD_vacc1, CMD_vacc2, CMD_vacc3, code_cat,
                   deldate, pregstart)]
CMD <- CMD[, list(patid, babypatid, pracid, obsdate, CMD_vacc1, CMD_vacc2, CMD_vacc3, code_cat,
                  deldate, pregstart)]
CMD <- CMD[code_cat  != "symptom"]

CMD <- rbind(CMD, tmp)

#any true entry by woman 
CMD[, vacc1_CMD := any(CMD_vacc1 == T), by = c("patid", "babypatid")]
CMD[, vacc2_CMD := any(CMD_vacc2 == T), by = c("patid", "babypatid")]
CMD[, vacc3_CMD := any(CMD_vacc3 == T), by = c("patid", "babypatid")]


CMD <- unique(CMD[, list(patid, babypatid, obsdate, deldate, pregstart,
                         vacc1_CMD, vacc2_CMD, vacc3_CMD,
                         CMD_vacc1, CMD_vacc2, CMD_vacc3)])

#first vaccine
CMD1 <- unique(CMD[CMD_vacc1 == T])
CMD1[, obsdate_CMD := max(obsdate), by = c("patid", "babypatid")]
CMD1 <- unique(CMD1[, list(patid, babypatid, obsdate_CMD, pregstart, deldate)])
#second
CMD2 <- unique(CMD[CMD_vacc2 == T])
CMD2[, obsdate_CMD := max(obsdate), by = c("patid", "babypatid")]
CMD2 <- unique(CMD2[, list(patid, babypatid, obsdate_CMD, pregstart, deldate)])
#third vaccine
CMD3 <- unique(CMD[CMD_vacc3 == T])
CMD3[, obsdate_CMD := max(obsdate), by = c("patid", "babypatid")]
CMD3 <- unique(CMD3[, list(patid, babypatid, obsdate_CMD, pregstart, deldate)])


#categorising the date
#defining timing variable
#vaccine 1
CMD1[is.na(obsdate_CMD), CMD_timing_vac1_pres := "None"]
CMD1[obsdate_CMD < pregstart, CMD_timing_vac1_pres := "History of CMD" ]
CMD1[obsdate_CMD  >=pregstart & obsdate_CMD <deldate, CMD_timing_vac1_pres := "prenatal CMD" ]
CMD1[obsdate_CMD  >= deldate & obsdate_CMD < (deldate+dweeks(8)), CMD_timing_vac1_pres := "postnatal CMD" ]
CMD1[obsdate_CMD >= (deldate+dweeks(8)), CMD_timing_vac1_pres := "None"]

#vaccine 2
CMD2[is.na(obsdate_CMD), CMD_timing_vac2_pres := "None"]
CMD2[obsdate_CMD < pregstart, CMD_timing_vac2_pres := "History of CMD" ]
CMD2[obsdate_CMD  >=pregstart & obsdate_CMD <deldate, CMD_timing_vac2_pres := "prenatal CMD" ]
CMD2[obsdate_CMD  >= deldate & obsdate_CMD < (deldate+ddays(365)), CMD_timing_vac2_pres := "postnatal CMD" ]
CMD2[obsdate_CMD >= (deldate+ddays(365)), CMD_timing_vac2_pres := "None"]

#vaccine 3
CMD3[is.na(obsdate_CMD), CMD_timing_vac3_pres := "None"]
CMD3[obsdate_CMD < pregstart, CMD_timing_vac3_pres := "History of CMD" ]
CMD3[obsdate_CMD  >=pregstart & obsdate_CMD <deldate, CMD_timing_vac3_pres := "prenatal CMD" ]
CMD3[obsdate_CMD  >= deldate & obsdate_CMD < (deldate + dmonths(12)), CMD_timing_vac3_pres := "postnatal CMD" ]
CMD3[obsdate_CMD >= (deldate + dmonths(12)) & obsdate_CMD < (deldate + dmonths(18)) , CMD_timing_vac3_pres := "childhood CMD" ]
CMD3[obsdate_CMD >= (deldate+dmonths(18)), CMD_timing_vac3_pres := "None"]

####
CMD1 <- CMD1[, list(patid, babypatid, CMD_timing_vac1_pres)]
CMD2 <- CMD2[, list(patid, babypatid, CMD_timing_vac2_pres)]
CMD3 <- CMD3[, list(patid, babypatid, CMD_timing_vac3_pres)]

#adding it to the existing dataset
df <- merge(df, CMD1, by = c("patid", "babypatid"), all.x = T)
df <- merge(df, CMD2, by = c("patid", "babypatid"), all.x = T)
df <- merge(df, CMD3, by = c("patid", "babypatid"), all.x = T)

df[is.na(CMD_timing_vac1_pres), CMD_timing_vac1_pres:="None"]
df[is.na(CMD_timing_vac2_pres), CMD_timing_vac2_pres:="None"]
df[is.na(CMD_timing_vac3_pres), CMD_timing_vac3_pres:="None"]



#sorting out mothers who don't get followed up until vaccination date
#FU for first vaccine, before age 8 weeks/ days
df[, age_vacc1 := deldate+dweeks(8)]
#FU for second vaccine, before age 1 year
df[, age_vacc2 := deldate+ddays(365)]
#FU for third vaccine, before age 18 months
df[, age_vacc3 := deldate+dmonths(18)]

#checking whether any diagnosis before vacc due date
df[regenddate < age_vacc1, CMD_timing_vac1_pres := NA]
df[regenddate < age_vacc2, CMD_timing_vac2_pres := NA]
df[regenddate < age_vacc3, CMD_timing_vac3_pres := NA]

tabyl(df$CMD_timing_vac1_pres)
tabyl(df$CMD_timing_vac2_pres)
tabyl(df$CMD_timing_vac3_pres)


write_parquet(df, paste0(datafiles, "final_studypop_sensitivity_anal_plustiming.parquet"))
