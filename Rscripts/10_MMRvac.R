###Script to generate a variable for the MMR vaccine as an outcome

#--- reading in the the code lists for MMR vaccine
csv_files <- list.files(paste0(codelists, "vaccines/MMR/"))
vacc_codes <- list(rep(data.table(), times = length(csv_files)))

for(i in 1:length(csv_files)){
  csv_files[[i]] <- paste(c(codelists, "vaccines/MMR/", csv_files[[i]]), collapse="")
  vacc_codes[[i]]<- as.data.table(read.csv(csv_files[[i]]))
}

#prepare the formatting of the code lists
#first using the medcode based ones
medcode_list <- list (vacc_codes[[1]], vacc_codes[[3]], vacc_codes[[4]])
for(i in 1:length(medcode_list)){
  
  medcode_list[[i]]$medcodeid <- as.character(medcode_list[[i]]$MedCodeId)  
}

#the product code based one
vacc_codes[[2]]$prodcodeid <- as.character(vacc_codes[[2]]$ProdCodeId)

#attaching the right names to the codelists
MMR_codes <- medcode_list
MMR_codes[[4]] <- vacc_codes[[2]]
names(MMR_codes) <- c("MMR_history", "MMR_terms", "MMR_terms_declinded", 
                      "MMR_products")

MMR_codes$MMR_history <- unique(MMR_codes$MMR_history)
MMR_codes$MMR_terms <- unique(MMR_codes$MMR_terms)
MMR_codes$MMR_terms_declinded <- unique(MMR_codes$MMR_terms_declinded)
MMR_codes$MMR_products <- unique(MMR_codes$MMR_products)

#---extracting the different codes
obs.files<- list.files(path = children_parquet_old, pattern = "\\Observation")
prod.files <- list.files(path =children_parquet_old,  pattern = "\\DrugIssue")

setwd(children_parquet_old)
df_terms <- extractor_med(list.files = obs.files, codelist = MMR_codes$MMR_terms)
length(unique(df_terms$patid)) #1,761,072 babies with vaccine record
df_terms[given ==1, vac_status := "given"]
df_terms[is.na(given), vac_status := "neutral"]

df_declined <- extractor_med(list.files = obs.files, codelist = MMR_codes$MMR_terms_declinded)
length(unique(df_declined$patid)) # 25,870 babies with declined vaccine record
df_declined[DNA ==1, vac_status := "declined"]
df_declined[contraindicated ==1, vac_status := "declined"]
df_declined[not_immunised ==1, vac_status := "declined"]

df_products <- extractor_prod(list.files = prod.files, codelist = MMR_codes$MMR_products)
length(unique(df_products$patid)) #22187
df_products <- df_products[, vac_status := "product"]
df_products <- df_products[, obsdate := issuedate]
df_products <- df_products[, issuedate := NULL] 
df_products[, Term := Term.from.EMIS]

df_terms <- df_terms[, list(patid, pracid, medcodeid, obsdate, enterdate, Term, vac_status, given, 
                            measles)]
df_declined <- df_declined[, list(patid, pracid, medcodeid, obsdate, enterdate, Term,  vac_status, measles)]
df_products <- df_products[, list(patid, pracid, prodcodeid, obsdate, enterdate, Term, vac_status, measles)]

df_terms$patid <- as.character(df_terms$patid)
df_declined$patid <- as.character(df_declined$patid)
df_products$patid <- as.character(df_products$patid)
df_terms$pracid <- as.character(df_terms$pracid)
df_declined$pracid <- as.character(df_declined$pracid)
df_products$pracid <- as.character(df_products$pracid)



pop <- read_parquet(paste0(datafiles, "children_studypop.parquet"))

pop[, patid := as.character(patid)] #535918
pop[, pracid := as.character(pracid)]

length(unique(df_terms$patid)) #1,761,072
length(unique(df_declined$patid)) #25870
length(unique(df_products$patid)) #21917


#merging declined and term file
pop1 <- merge(pop, df_terms, by = c("patid", "pracid"), all.x = T)
pop1[, prodcodeid := 0] 
nrow(pop1[!is.na(medcodeid)]) #1266733


pop2 <-  merge(pop, df_declined, by = c("patid", "pracid"), all.x = T)
pop2[, given := 0]
pop2[, prodcodeid := 0 ]
nrow(pop2[!is.na(medcodeid)]) #10105

pop3 <- merge(pop, df_products, by = c("patid", "pracid"), all.x = T)
pop3[, given := 0]
pop3[, medcodeid := 0]
nrow(pop3[!is.na(prodcodeid)]) #10609

df <- rbind(pop1, pop2, pop3)
length(unique(df$patid)) #535689
length(unique(df[!is.na(vac_status)]$patid)) #481921

#cleaning memory
rm(df_declined)
rm(df_products)
rm(df_terms)

####saving prelim_files 
write_parquet(df, paste0(children_parquet, "MMR_outcome_uncleaned.parquet"))
df <- read_parquet(paste0(children_parquet, "MMR_outcome_uncleaned.parquet"))

df[, obsdate := as.Date(obsdate, format = "%d/%m/%Y")]
df[, enterdate := as.Date(enterdate, format = "%d/%m/%Y")]

###---Vaccine algorithm
length(unique(df$patid)) #535689


#only using the measles antigen containing vaccines
tabyl(df$measles)
df <- df[measles == 1]
length(unique(df$patid)) # 484114 in the cohort


#double checking the follow-up period of the study
colnames(df)
df <- df[obsdate <= end_fu &
           obsdate >= start_fu]

length(unique(df$patid)) # 480727 babies remaining

################################################################################
###Step 1: finding out whether a vaccine was given
#flag vaccine as given if there is a product code on a day

tmp <- df[, .(patid, obsdate, enterdate, vac_status, Term)]


tmp <- tmp[is.na(vac_status), vac_status := "declined"]
janitor::tabyl(tmp$vac_status)
tmp$vac_status      n     percent
# declined   7448 0.008137143
# given 497668 0.543715838
# neutral 399839 0.436834992
# product  10354 0.011312027

length(unique(tmp$patid)) #480727
length(unique(tmp$obsdate)) #4226

tmp_wide <- dcast(tmp, patid + obsdate + enterdate+ Term ~ vac_status, fun.aggregate = length,
                  value.var = "vac_status")
tmp_wide[, .(sum_declined = sum(declined),
             sum_given = sum(given), 
             sum_neutral = sum(neutral),
             sum_prod = sum(product))]

# sum_declined sum_given sum_neutral sum_prod
# 1:         7448    497668      399839    10354

#summing up the same codes per day 
tmp_filtered <- tmp_wide[, by=c("patid", "obsdate"), lapply(.SD, sum),
                         .SDcols = c("declined", "given", "neutral", "product")]
#default - all codes count as vaccinated
tmp_filtered[, MMR_vacc := "vaccinated"]
#declined when only declined codes or together with neutral
tmp_filtered[neutral > 0 & declined >0, MMR_vacc:= "declined"]
tmp_filtered[declined >0 & product == 0 & given == 0 & neutral == 0,
             MMR_vacc:= "declined"]
#conflict flag when declined and given or declined and product
tmp_filtered[declined >0 & given > 0, MMR_vacc := "conflict"]
tmp_filtered[declined >0 & product > 0, MMR_vacc := "conflict"]

#adding for the table
tmp_filtered[declined == 0 & product == 0 & given > 0 & neutral == 0,
             MMR_vacc:= "vaccinated"]
tmp_filtered[declined == 0 & product >0 & given == 0 & neutral == 0,
             MMR_vacc:= "vaccinated"]
tmp_filtered[declined == 0 & product ==0 & given == 0 & neutral > 0,
             MMR_vacc:= "vaccinated"]
tmp_filtered[neutral > 0 & given >0, MMR_vacc:= "vaccinated"]
tmp_filtered[neutral > 0 & product >0, MMR_vacc:= "vaccinated"]
tmp_filtered[given > 0 & product >0, MMR_vacc:= "vaccinated"]

janitor::tabyl(tmp_filtered$MMR_vacc)

# tmp_filtered$MMR_vacc      n      percent
# conflict     95 0.0001074334
# declined   6605 0.0074694465
# vaccinated 877569 0.9924231201


tmp<- tmp_filtered[MMR_vacc != "conflict"] # drop the  95 conflicted ones 
rm(tmp_filtered)


###Step 2: counting the number of doses
tmp[MMR_vacc == "vaccinated", by = .(patid, MMR_vacc),
    MMR_all_doses :=  .N]
tmp_df <- unique(tmp[, .(patid, MMR_all_doses)])
janitor::tabyl(tmp_df$MMR_all_doses)
# tmp_df$MMR_all_doses      n      percent valid_percent
# 1  83659 1.729721e-01  1.748688e-01
# 2 390701 8.078076e-01  8.166656e-01
# 3   3717 7.685214e-03  7.769486e-03
# 4    312 6.450866e-04  6.521603e-04
# 5     18 3.721653e-05  3.762463e-05

#and just by events
janitor::tabyl(tmp$MMR_all_doses)
# tmp$MMR_all_doses      n      percent valid_percent
# 1  83659 9.461825e-02  9.533040e-02
# 2 781402 8.837650e-01  8.904166e-01
# 3  11151 1.261177e-02  1.270669e-02
# 4   1248 1.411487e-03  1.422110e-03
# 5     90 1.017899e-04  1.025560e-04



#checking the distribution over years
hist(year(tmp$obsdate))
tmp <- tmp[order(patid, obsdate)] #increasing towards 2011, then decreasing again

#counting the number of doses 
tmp[MMR_vacc == "vaccinated", n_dose := seq_len(.N), by = "patid"]
tmp[MMR_vacc == "vaccinated", n_dose := rowid(patid)]

#now dob has be added as information again
dob <- unique(df[, list(patid, deldate)])
tmp<- merge(tmp, dob, by = "patid")
tmp[, age := interval(deldate, obsdate)%/%days(1)]

summary(tmp$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -180.0   400.0   512.0   813.5  1259.0  1826.0 


###---quality checks
length(unique(tmp$patid)) #480713

### Step 1: Excluding all the children who got their first dose of vaccine
###before they were born
q1 <- tmp[n_dose == 1 & age < 16]
tabyl(q1$MMR_all_doses)
length(unique(q1$patid)) #151
dubious_q1 <- q1$patid


#backdating as a cause?
q1vac <- tmp_wide[patid %chin% dubious_q1]
q1vac[, n_dose := seq_len(.N), by = "patid"] #approx for qual check 
q1vac[, n_dose := rowid(patid)]
q1vac[n_dose ==1,late_entry := enterdate >= (obsdate+ years(1))]
tabyl(q1vac$late_entry)
# q1vac$late_entry   n     percent valid_percent
# FALSE 148 0.482084691    0.98013245



#checking their date of birth
population <- read_parquet(paste0(datafiles, "children_studypop.parquet"))
pop_dub <- population[patid %chin% dubious_q1]
hist(pop_dub$yob) ## mainly children born in 2006 excluded

write_parquet(pop_dub, paste0(datafiles, "children_ex_MMR.parquet"))

#remove trash
rm(population)
rm(pop_dub)
rm(dob)
rm(q1)

#drop children with unrealistic vaccine record from data frame
tmp1 <- tmp[!patid %chin% dubious_q1]
tabyl(tmp1$MMR_all_doses)
length(unique(tmp1$patid)) #480562

###exploring the data
#at what age was the first MMR dose given
summary(tmp1[n_dose ==1, list(age)])
# Min.   :  23.0  
# 1st Qu.: 383.0  
# Median : 404.0  
# Mean   : 442.9  
# 3rd Qu.: 436.0  
# Max.   :1826.0   


#at what age was the second MMR dose given
summary(tmp1[n_dose ==2, list(age)])
# Min.   :  70  
# 1st Qu.:1232  
# Median :1265  
# Mean   :1257  
# 3rd Qu.:1310  
# Max.   :1826  


####final data clean following those thresholds


####cleaning the data set
#min age dose one is 347 days
#min age for second dose is 530 days
#time difference is 26 days between two doses
###---DOSE 1
#define if first dose in time, if not remove
tmp1 <- clean_min_vac_date(df = tmp1, dose = 1, min_age = 347)
[1] "1632 vaccine records dropped (0.18%)"
[1] "882235 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 1, min_age = 347)
[1] "36 vaccine records dropped (0%)"
[1] "882199 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 1, min_age = 347)
[1] "19 vaccine records dropped (0%)"
[1] "882180 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 1, min_age = 347)
[1] "0 vaccine records dropped (0%)"
[1] "882180 entries remaining in df"


#minimum age for the second dose
tmp1 <- clean_min_vac_date(df = tmp1, dose = 2, min_age = 530)
[1] "8982 vaccine records dropped (1.02%)"
[1] "873198 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 2, min_age = 530)
[1] "50 vaccine records dropped (0.01%)"
[1] "873148 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 2, min_age = 530)
[1] "1 vaccine records dropped (0%)"
[1] "873147 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 2, min_age = 530)
[1] "1 vaccine records dropped (0%)"
[1] "873146 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 2, min_age = 530)
[1] "0 vaccine records dropped (0%)"
[1] "873146 entries remaining in df"

#min difference between doses
tmp1 <- check_min_diff(df = tmp1, dose_a = 1, dose_b = 2, min_diff = 26)
[1] "144 vaccine records dropped (0.02%)"
[1] "873002 entries remaining in df"
tmp1 <- check_min_diff(df = tmp1, dose_a = 1, dose_b = 2, min_diff = 26)
[1] "0 vaccine records dropped (0%)"
[1] "873002 entries remaining in df"



tabyl(tmp1$MMR_all_doses)
# tmp1$MMR_all_doses      n      percent valid_percent
# 1  91974 1.053537e-01  1.061569e-01
# 2 767602 8.792672e-01  8.859703e-01
# 3   6000 6.872836e-03  6.925232e-03
# 4    800 9.163782e-04  9.233642e-04


####remove all recordings of a third dose and higher from the data set
###before they were born
tmp2 <- tmp1[n_dose <=2 | is.na(n_dose)]
tabyl(tmp2$n_dose)
# tmp2$n_dose      n     percent valid_percent
# 1 477979 0.549029450     0.5532267
# 2 386005 0.443383732     0.4467733
# NA   6605 0.007586818            NA


tmp2[, vaccine := "MMR"]
tmp2[, vac_status :=  MMR_vacc]
tmp2[, n_all_doses := MMR_all_doses]
tmp2[, MMR_vacc := NULL]
tmp2[, MMR_all_doses := NULL]

####---save the cleaned data file
colnames(tmp2)
MMR <- tmp2[, list(patid, deldate, obsdate, vaccine, vac_status, n_all_doses, n_dose, age)]
write_parquet(MMR, paste0(datafiles, "MMR_outcome.parquet"))
