###Script to generate a variable for the DTP vaccine as an outcome

#--- reading in the the code lists for DTP vaccine
csv_files <- list.files(paste0(codelists, "vaccines/Pneu/"))
vacc_codes <- list(rep(data.table(), times = length(csv_files)))

for(i in 1:length(csv_files)){
  csv_files[[i]] <- paste(c(codelists, "vaccines/Pneu/", csv_files[[i]]), collapse="")
  vacc_codes[[i]]<- as.data.table(read.csv2(csv_files[[i]]))
}

#prepare the formatting of the code lists
#first using the medcode based ones
medcode_list <- list (vacc_codes[[1]], vacc_codes[[3]], vacc_codes[[4]])
for(i in 1:length(medcode_list)){
  medcode_list[[i]]$medcodeid <- as.character(medcode_list[[i]]$medcodeid)
}

#the product code based one
vacc_codes[[2]]$prodcodeid <- as.character(vacc_codes[[2]]$prodcodeid)
#attaching the right names to the codelists
DTP_codes <- medcode_list
DTP_codes[[4]] <- vacc_codes[[2]]
names(DTP_codes) <- c("DTP_history", "DTP_terms", "DTP_terms_declinded", 
                      "DTP_products")
DTP_codes$DTP_terms <- unique(DTP_codes$DTP_terms)
DTP_codes$DTP_history <- unique(DTP_codes$DTP_history)
DTP_codes$DTP_terms_declinded <- unique(DTP_codes$DTP_terms_declinded)
DTP_codes$DTP_products <- unique(DTP_codes$DTP_products)


#---extracting the different codes
obs.files<- list.files(path = children_parquet_old, pattern = "\\Observation")
prod.files <- list.files(path = children_parquet_old, pattern = "\\DrugIssue")

setwd(children_parquet_old)
df_terms <- extractor_med(list.files = obs.files, codelist = DTP_codes$DTP_terms)

length(unique(df_terms$patid)) #1,872,814 babies with vaccine record
df_terms[given ==1, vac_status := "given"]
df_terms[is.na(given), vac_status := "neutral"]

df_declined <- extractor_med(list.files = obs.files, codelist = DTP_codes$DTP_terms_declinded)
length(unique(df_declined$patid)) #10870 babies with declined vaccine record
df_declined[no_consent ==1, vac_status := "declined"]
df_declined[DNA ==1, vac_status := "declined"]
df_declined[contraindictaed ==1, vac_status := "declined"]
df_declined[not.immunised ==1, vac_status := "declined"]

df_products <- extractor_prod(list.files = prod.files, codelist = DTP_codes$DTP_products)
df_products <- df_products[, vac_status := "product"]
df_products <- df_products[, obsdate := issuedate]
df_products <- df_products[, issuedate := NULL]
length(unique(df_products$patid)) #30419

#preparing data sets for the merge
df_terms <- df_terms[, list(patid, pracid, medcodeid, obsdate, enterdate, term, vac_status, given, 
                            pertussis)]
df_declined <- df_declined[, list(patid, pracid, medcodeid, obsdate, enterdate, term,  vac_status, pertussis)]
df_products <- df_products[, list(patid, pracid, prodcodeid, obsdate, enterdate, term, vac_status, pertussis)]

df_terms$patid <- as.character(df_terms$patid)
df_declined$patid <- as.character(df_declined$patid)
df_products$patid <- as.character(df_products$patid)
df_terms$pracid <- as.character(df_terms$pracid)
df_declined$pracid <- as.character(df_declined$pracid)
df_products$pracid <- as.character(df_products$pracid)

#study pop
pop <- read_parquet(paste0(datafiles, "children_studypop.parquet"))

pop[, patid := as.character(patid)] #535918
pop[, pracid := as.character(pracid)]
length(unique(df_terms$patid)) #1872814
length(unique(df_declined$patid)) #10870
length(unique(df_products$patid)) #30419


#merging declined and term file
pop1 <- merge(pop, df_terms, by = c("patid", "pracid"), all.x = T)
pop1[, prodcodeid := 0] 
nrow(pop1[!is.na(medcodeid)]) #1511469


pop2 <-  merge(pop, df_declined, by = c("patid", "pracid"), all.x = T)
pop2[, given := 0]
pop2[, prodcodeid := 0 ]
nrow(pop2[!is.na(medcodeid)]) #2141

pop3 <- merge(pop, df_products, by = c("patid", "pracid"), all.x = T)
pop3[, given := 0]
pop3[, medcodeid := 0]
nrow(pop3[!is.na(prodcodeid)]) #16527

df <- rbind(pop1, pop2, pop3)
length(unique(df$patid)) #535689
length(unique(df[!is.na(vac_status)]$patid)) #524816

#cleaning memory
rm(df_declined)
rm(df_products)
rm(df_terms)

####saving prelim_files 
write_parquet(df, paste0(children_parquet, "PCV_outcome_uncleaned.parquet"))
df <- read_parquet(paste0(children_parquet, "PCV_outcome_uncleaned.parquet"))

df[, obsdate := as.Date(obsdate, format = "%d/%m/%Y")]
df[, enterdate := as.Date(enterdate, format = "%d/%m/%Y")]

###---Vaccine algorithm
length(unique(df$patid)) #535689


#only looking at PCV vaccine
df <- df[is.na(PPV)]
length(unique(df$patid)) # 535689 in the cohort


#double checking the follow-up period of the study
colnames(df)
df <- df[obsdate <= end_fu &
           obsdate >= start_fu]

length(unique(df$patid)) # 519269 babies remaining

################################################################################
###Step 1: finding out whether a vaccine was given
#flag vaccine as given if there is a product code on a day

tmp <- df[, .(patid, obsdate, enterdate, vac_status, term)]


tmp <- tmp[is.na(vac_status), vac_status := "declined"]
# janitor::tabyl(tmp$vac_status)
# tmp$vac_status       n     percent
# declined    1848 0.001245386
# given   26135 0.017612646
# neutral 1439983 0.970419381
# product   15911 0.010722587


length(unique(tmp$patid)) #519269
length(unique(tmp$obsdate)) #3758

tmp_wide <- dcast(tmp, patid + obsdate + enterdate+ term ~ vac_status, fun.aggregate = length,
                  value.var = "vac_status")
tmp_wide[, .(sum_declined = sum(declined),
             sum_given = sum(given), 
             sum_neutral = sum(neutral),
             sum_prod = sum(product))]

# sum_declined sum_given sum_neutral sum_prod
# 1:         1848     26135     1439983    15911


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

tmp<- tmp_filtered[MMR_vacc != "conflict"] # drop the  1 conflicted one
rm(tmp_filtered)


###Step 2: counting the number of doses
tmp[MMR_vacc == "vaccinated", by = .(patid, MMR_vacc),
    MMR_all_doses :=  .N]
tmp_df <- unique(tmp[, .(patid, MMR_all_doses)])
janitor::tabyl(tmp_df$MMR_all_doses)
# tmp_df$MMR_all_doses      n      percent valid_percent
# 1  23212 4.460269e-02  4.473093e-02
# 2  65597 1.260470e-01  1.264094e-01
# 3 426866 8.202384e-01  8.225967e-01
# 4   3084 5.926017e-03  5.943055e-03
# 5    146 2.805443e-04  2.813509e-04

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
# -81.0    66.0   126.0   206.6   381.0  1826.0


###---quality checks
length(unique(tmp$patid)) #519269

### Step 1: Excluding all the children who got their first dose of vaccine
###before they were born
q1 <- tmp[n_dose == 1 & age < 16]
tabyl(q1$MMR_all_doses)
length(unique(q1$patid)) #472
dubious_q1 <- q1$patid


#backdating as a cause?
q1vac <- tmp_wide[patid %chin% dubious_q1]
q1vac[, n_dose := seq_len(.N), by = "patid"] #approx for qual check 
q1vac[, n_dose := rowid(patid)]
q1vac[n_dose ==1,late_entry := enterdate >= (obsdate+ years(1))]
tabyl(q1vac$late_entry)


#checking their date of birth
population <- read_parquet(paste0(datafiles, "children_studypop.parquet"))
pop_dub <- population[patid %chin% dubious_q1]
hist(pop_dub$yob) ## mainly children born in 2006 excluded

write_parquet(pop_dub, paste0(datafiles, "children_ex_PCV.parquet"))

#remove trash
rm(population)
rm(pop_dub)
rm(dob)
rm(q1)

#drop children with unrealistic vaccine record from data frame
tmp1 <- tmp[!patid %chin% dubious_q1]
tabyl(tmp1$MMR_all_doses)
length(unique(tmp1$patid)) #518797

###exploring the data
#at what age was the first PCV dose given
summary(tmp1[n_dose ==1, list(age)])
# Min.   :  16.00  
# 1st Qu.:  56.00  
# Median :  60.00  
# Mean   :  74.21  
# 3rd Qu.:  69.00  
# Max.   :1777.00  

#at what age was the second DTP dose given
summary(tmp1[n_dose ==2, list(age)])
# Min.   :  39.0  
# 1st Qu.: 117.0  
# Median : 126.0  
# Mean   : 153.4  
# 3rd Qu.: 147.0  
# Max.   :1823.0 

#at what age was the third DTP dose given
summary(tmp1[n_dose ==3, list(age)])
# Min.   :  90.0  
# 1st Qu.: 382.0  
# Median : 401.0  
# Mean   : 421.7  
# 3rd Qu.: 431.0  
# Max.   :1823.0 




########################################################################

####final data clean following those thresholds
#first dose given from 38 days
#second dose given from 96 days
#third dose given from 347 days
# age difference between 1&2 ans 2&3 is 26 days



####cleaning the data set
###---DOSE 1
#define if first dose in time, if not remove
tmp1 <- clean_min_vac_date(df = tmp1, dose = 1, min_age = 38)
[1] "1345 vaccine records dropped (0.09%)"
[1] "1447190 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 1, min_age = 38)
[1] "0 vaccine records dropped (0%)"
[1] "1447190 entries remaining in df"

###=---DOSE 2
tmp1 <- clean_min_vac_date(df = tmp1, dose = 2, min_age = 96)
[1] "1139 vaccine records dropped (0.08%)"
[1] "1446051 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 2, min_age = 96)
[1] "1 vaccine records dropped (0%)"
[1] "1446050 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 2, min_age = 96)
[1] "0 vaccine records dropped (0%)"
[1] "1446050 entries remaining in df"


#min age gap between doses
tmp1 <- check_min_diff(df = tmp1, dose_a = 1, dose_b = 2, min_diff = 26)
[1] "299 vaccine records dropped (0.02%)"
[1] "1445751 entries remaining in df"
tmp1 <- check_min_diff(df = tmp1, dose_a = 1, dose_b = 2, min_diff = 26)
[1] "0 vaccine records dropped (0%)"
[1] "1445751 entries remaining in df"

###---DOSE 3
tmp1 <- clean_min_vac_date(df = tmp1, dose = 3, min_age = 347)
[1] "1374 vaccine records dropped (0.1%)"
[1] "1444377 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 3, min_age = 347)
[1] "8 vaccine records dropped (0%)"
[1] "1444369 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 3, min_age = 347)
[1] "0 vaccine records dropped (0%)"
[1] "1444369 entries remaining in df"


#min age gap between dosesa
tmp1 <- check_min_diff(df = tmp1, dose_a = 2, dose_b = 3, min_diff = 26)
[1] "185 vaccine records dropped (0.01%)"
[1] "1444184 entries remaining in df"
tmp1 <- check_min_diff(df = tmp1, dose_a = 2, dose_b = 3, min_diff = 26)
[1] "0 vaccine records dropped (0%)"
[1] "1444184 entries remaining in df"


tabyl(tmp1$MMR_all_doses)
# tmp1$MMR_all_doses       n      percent valid_percent
# 1   23366 1.617938e-02  1.619857e-02
# 2  135212 9.362519e-02  9.373624e-02
# 3 1276269 8.837302e-01  8.847784e-01
# 4    7204 4.988284e-03  4.994201e-03
# 5     325 2.250406e-04  2.253075e-04
# 6      90 6.231893e-05  6.239285e-05


####remove all recordings of a third dose and higher from the data set
###before they were born
tmp2 <- tmp1[n_dose <=3 | is.na(n_dose)]
tabyl(tmp2$n_dose)
# tmp2$n_dose      n     percent valid_percent
# 1 518277 0.359364556     0.3597914
# 2 494911 0.343162964     0.3435706
# 3 427305 0.296286101     0.2966380
# NA   1711 0.001186379            NA


tmp2[, vaccine := "PCV"]
tmp2[, vac_status :=  MMR_vacc]
tmp2[, n_all_doses := MMR_all_doses]
tmp2[, MMR_vacc := NULL]
tmp2[, MMR_all_doses := NULL]

####---save the cleaned data file
colnames(tmp2)
MMR <- tmp2[, list(patid, deldate, obsdate, vaccine, vac_status, n_all_doses, n_dose, age)]
write_parquet(MMR, paste0(datafiles, "PCV_outcome.parquet"))
