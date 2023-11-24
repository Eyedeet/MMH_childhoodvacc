###Script to generate a variable for the DTP vaccine as an outcome

#--- reading in the the code lists for DTP vaccine
csv_files <- list.files(paste0(codelists, "vaccines/DTP/"))
vacc_codes <- list(rep(data.table(), times = length(csv_files)))

for(i in 1:length(csv_files)){
  csv_files[[i]] <- paste(c(codelists, "vaccines/DTP/", csv_files[[i]]), collapse="")
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
length(unique(df_declined$patid)) #6890 babies with declined vaccine record
df_declined[no_consent ==1, vac_status := "declined"]
df_declined[DNA ==1, vac_status := "declined"]
df_declined[contraindictaed ==1, vac_status := "declined"]
df_declined[not.immunised ==1, vac_status := "declined"]
df_declined[, PPV:= NA]

df_products <- extractor_prod(list.files = prod.files, codelist = DTP_codes$DTP_products)
df_products <- df_products[, vac_status := "product"]
df_products <- df_products[, obsdate := issuedate]
df_products <- df_products[, issuedate := NULL]
df_products[, PPV:= NA]
length(unique(df_products$patid)) #23124

#preparing data sets for the merge
df_terms <- df_terms[, list(patid, pracid, medcodeid, obsdate, enterdate, term, vac_status, given, 
                            PPV)]
df_declined <- df_declined[, list(patid, pracid, medcodeid, obsdate, enterdate, term,  vac_status, PPV)]
df_products <- df_products[, list(patid, pracid, prodcodeid, obsdate, enterdate, term, vac_status, PPV)]

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
nrow(pop1[!is.na(medcodeid)]) #2059428


pop2 <-  merge(pop, df_declined, by = c("patid", "pracid"), all.x = T)
pop2[, given := 0]
pop2[, prodcodeid := 0 ]
nrow(pop2[!is.na(medcodeid)]) #3876

pop3 <- merge(pop, df_products, by = c("patid", "pracid"), all.x = T)
pop3[, given := 0]
pop3[, medcodeid := 0]
nrow(pop3[!is.na(prodcodeid)]) #23645

df <- rbind(pop1, pop2, pop3)
length(unique(df$patid)) #535689
length(unique(df[!is.na(vac_status)]$patid)) #509687

#cleaning memory
rm(df_declined)
rm(df_products)
rm(df_terms)

####saving prelim_files 
write_parquet(df, paste0(children_parquet, "DTP_outcome_uncleaned.parquet"))
df <- read_parquet(paste0(children_parquet, "DTP_outcome_uncleaned.parquet"))

df[, obsdate := as.Date(obsdate, format = "%d/%m/%Y")]
df[, enterdate := as.Date(enterdate, format = "%d/%m/%Y")]

###---Vaccine algorithm
length(unique(df$patid)) #535689


#only using the measles antigen containing vaccines
tabyl(df$pertussis)
df <- df[pertussis == 1]
length(unique(df$patid)) # 509130 in the cohort


#double checking the follow-up period of the study
colnames(df)
df <- df[obsdate <= end_fu &
           obsdate >= start_fu]

length(unique(df$patid)) # 503167 babies remaining

################################################################################
###Step 1: finding out whether a vaccine was given
#flag vaccine as given if there is a product code on a day

tmp <- df[, .(patid, obsdate, enterdate, vac_status, term)]


tmp <- tmp[is.na(vac_status), vac_status := "declined"]
janitor::tabyl(tmp$vac_status)
# tmp$vac_status       n    percent
# declined    3341 0.00256851
# given  259583 0.19956348
# neutral 1015613 0.78078791
# product   22217 0.01708009

length(unique(tmp$patid)) #503167
length(unique(tmp$obsdate)) #4408

tmp_wide <- dcast(tmp, patid + obsdate + enterdate+ term ~ vac_status, fun.aggregate = length,
                  value.var = "vac_status")
tmp_wide[, .(sum_declined = sum(declined),
             sum_given = sum(given), 
             sum_neutral = sum(neutral),
             sum_prod = sum(product))]

# sum_declined sum_given sum_neutral sum_prod
# 1:         3341    259583     1015613    22217

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

tmp<- tmp_filtered[MMR_vacc != "conflict"] # drop the  11 conflicted ones 
rm(tmp_filtered)


###Step 2: counting the number of doses
tmp[MMR_vacc == "vaccinated", by = .(patid, MMR_vacc),
    MMR_all_doses :=  .N]
tmp_df <- unique(tmp[, .(patid, MMR_all_doses)])
janitor::tabyl(tmp_df$MMR_all_doses)
# tmp_df$MMR_all_doses      n      percent valid_percent
# 1 159606 3.158388e-01  3.174060e-01
# 2  92853 1.837436e-01  1.846553e-01
# 3  77326 1.530178e-01  1.537770e-01
# 4 172192 3.407448e-01  3.424355e-01
# 5    825 1.632564e-03  1.640665e-03
# 6     36 7.123917e-05  7.159264e-05



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
# -187.0    84.0   107.0   385.4   236.0  1826.0

###---quality checks
length(unique(tmp$patid)) #503167

### Step 1: Excluding all the children who got their first dose of vaccine
###before they were born
q1 <- tmp[n_dose == 1 & age < 16]
tabyl(q1$MMR_all_doses)
length(unique(q1$patid)) #986
dubious_q1 <- q1$patid


#backdating as a cause?
q1vac <- tmp_wide[patid %chin% dubious_q1]
q1vac[, n_dose := seq_len(.N), by = "patid"] #approx for qual check 
q1vac[, n_dose := rowid(patid)]
q1vac[n_dose ==1,late_entry := enterdate >= (obsdate+ years(1))]
tabyl(q1vac$late_entry)
# q1vac$late_entry    n      percent valid_percent
# FALSE  985 0.2617592347   0.998985801
# TRUE    1 0.0002657454   0.001014199
# NA 2777 0.7379750199            NA

#checking their date of birth
population <- read_parquet(paste0(datafiles, "children_studypop.parquet"))
pop_dub <- population[patid %chin% dubious_q1]
hist(pop_dub$yob) ## mainly children born in 2006 excluded

write_parquet(pop_dub, paste0(datafiles, "children_ex_DTP.parquet"))

#remove trash
rm(population)
rm(pop_dub)
rm(dob)
rm(q1)

#drop children with unrealistic vaccine record from data frame
tmp1 <- tmp[!patid %chin% dubious_q1]
tabyl(tmp1$MMR_all_doses)
length(unique(tmp1$patid)) #502181

###exploring the data
#at what age was the first DTP dose given
summary(tmp1[n_dose ==1, list(age)])
# Min.   :  16.0  
# 1st Qu.:  59.0  
# Median :  79.0  
# Mean   : 158.2  
# 3rd Qu.:  96.0  
# Max.   :1826.0  

#at what age was the second DTP dose given
summary(tmp1[n_dose ==2, list(age)])
# age        
# Min.   :  41.0  
# 1st Qu.:  88.0  
# Median :  99.0  
# Mean   : 396.1  
# 3rd Qu.: 398.0  
# Max.   :1826.0 

#at what age was the third DTP dose given
summary(tmp1[n_dose ==3, list(age)])
# age        
# Min.   :  70.0  
# 1st Qu.: 117.0  
# Median : 126.0  
# Mean   : 191.8  
# 3rd Qu.: 145.0  
# Max.   :1825.0  


#at what age was the third DTP dose given
summary(tmp1[n_dose ==4, list(age)])
# Min.   :  84  
# 1st Qu.:1239  
# Median :1273  
# Mean   :1299  
# 3rd Qu.:1323  
# Max.   :1826  


########################################################################

####final data clean following those thresholds
#first dose given from 38 days
#second dose given from 66 days
#third dose given from 96 days
#fourth dose given from 1,077 days
# age difference between 1&2 ans 2&3 is 26 days
#age diff for 3rd and 4th dose is 363 days
length(unique(tmp1$patid)) #502181
nrow(tmp1) #1269944 vaccine records

####cleaning the data set
###---DOSE 1
#define if first dose in time, if not remove
tmp1 <- clean_min_vac_date(df = tmp1, dose = 1, min_age = 38)
[1] "1060 vaccine records dropped (0.08%)"
[1] "1268884 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 1, min_age = 38)
[1] "0 vaccine records dropped (0%)"
[1] "1268884 entries remaining in df"

###=---DOSE 2
tmp1 <- clean_min_vac_date(df = tmp1, dose = 2, min_age = 66)
[1] "88 vaccine records dropped (0.01%)"
[1] "1268796 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 2, min_age = 66)
[1] "0 vaccine records dropped (0%)"
[1] "1268796 entries remaining in df"


#min age gap between dosesa
tmp1 <- check_min_diff(df = tmp1, dose_a = 1, dose_b = 2, min_diff = 26)
[1] "6829 vaccine records dropped (0.54%)"
[1] "1261967 entries remaining in df"
tmp1 <- check_min_diff(df = tmp1, dose_a = 1, dose_b = 2, min_diff = 26)
[1] "0 vaccine records dropped (0%)"
[1] "1261967 entries remaining in df"

###---DOSE 3
tmp1 <- clean_min_vac_date(df = tmp1, dose = 3, min_age = 96)
[1] "122 vaccine records dropped (0.01%)"
[1] "1261845 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 3, min_age = 96)
[1] "0 vaccine records dropped (0%)"
[1] "1261845 entries remaining in df"


#min age gap between dosesa
tmp1 <- check_min_diff(df = tmp1, dose_a = 2, dose_b = 3, min_diff = 26)
[1] "5915 vaccine records dropped (0.47%)"
[1] "1255930 entries remaining in df"
tmp1 <- check_min_diff(df = tmp1, dose_a = 2, dose_b = 3, min_diff = 26)
[1] "0 vaccine records dropped (0%)"
[1] "1255930 entries remaining in df"

###---DOSE 4
tmp1 <- clean_min_vac_date(df = tmp1, dose = 4, min_age = 1077)
[1] "721 vaccine records dropped (0.06%)"
[1] "1255209 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 4, min_age = 1077)
[1] "18 vaccine records dropped (0%)"
[1] "1255191 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 4, min_age = 1077)
[1] "4 vaccine records dropped (0%)"
[1] "1255187 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 4, min_age = 1077)
[1] "1 vaccine records dropped (0%)"
[1] "1255186 entries remaining in df"
tmp1 <- clean_min_vac_date(df = tmp1, dose = 4, min_age = 1077)
[1] "0 vaccine records dropped (0%)"
[1] "1255186 entries remaining in df"

#min gap
tmp1 <- check_min_diff(df = tmp1, dose_a = 3, dose_b = 4, min_diff = 363)
[1] "402 vaccine records dropped (0.03%)"
[1] "1254784 entries remaining in df"
tmp1 <- check_min_diff(df = tmp1, dose_a = 3, dose_b = 4, min_diff = 363)
[1] "0 vaccine records dropped (0%)"
[1] "1254784 entries remaining in df"



tabyl(tmp1$MMR_all_doses)
# tmp1$MMR_all_doses      n      percent valid_percent
# 1 159584 0.1271804550  1.275048e-01
# 2 185714 0.1480047562  1.483822e-01
# 3 231036 0.1841241202  1.845937e-01
# 4 674024 0.5371633684  5.385333e-01
# 5   1190 0.0009483704  9.507891e-04
# 6     30 0.0000239085  2.396947e-05


####remove all recordings of a third dose and higher from the data set
###before they were born
tmp2 <- tmp1[n_dose <=4 | is.na(n_dose)]
tabyl(tmp2$n_dose)
# tmp2$n_dose      n     percent valid_percent
# 1 498204 0.397124023     0.3981370
# 2 338620 0.269917818     0.2706063
# 3 245763 0.195900457     0.1964002
# 4 168751 0.134513324     0.1348564
# NA   3192 0.002544379            NA

tmp2[, vaccine := "DTP"]
tmp2[, vac_status :=  MMR_vacc]
tmp2[, n_all_doses := MMR_all_doses]
tmp2[, MMR_vacc := NULL]
tmp2[, MMR_all_doses := NULL]

####---save the cleaned data file
colnames(tmp2)
MMR <- tmp2[, list(patid, deldate, obsdate, vaccine, vac_status, n_all_doses, n_dose, age)]
write_parquet(MMR, paste0(datafiles, "DTP_outcome.parquet"))
