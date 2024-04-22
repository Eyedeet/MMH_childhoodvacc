##### DESCRIPTIVE TABLES
df <- read_parquet(paste0(datafiles, "final_studypop_all_ind.parquet"))
pop <- unique(df[, list(patid, babypatid, pracid, age_startfu_baby, age_endfu_baby, age_birth,
                        birthorder, n_children, ethnicity_5, region, imd, gender_child,
                        MHI_vacc1, MHI_vacc2, MHI_vacc3, birth_order_cat, mat_age,
                        flurisk)])
pop0 <- pop
pop1 <- pop[age_endfu_baby >= 365]
pop2 <- pop[age_endfu_baby >= 730]
pop3 <- pop[age_endfu_baby >= 1825]

df[, fu_time_baby := age_endfu_baby - age_startfu_baby]
summary(df$fu_time)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.00   10.00   15.00   17.14   22.00   59.00
summary(df$fu_time_baby)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0    1790    1805    1619    1815    1826 


length(unique(pop0$babypatid)) #397518
length(unique(pop1$babypatid)) #373107
length(unique(pop2$babypatid)) #349013
length(unique(pop3$babypatid)) #299438

#mothers per group
length(unique(pop0$patid)) #330198
length(unique(pop1$patid)) #309114
length(unique(pop2$patid)) #288224
length(unique(pop3$patid)) #245003


#children per group 
pop_all <- pop0 %>%
  tabyl(mat_age)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_1 <- pop1 %>%
  tabyl(mat_age)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_2 <- pop2 %>%
  tabyl(mat_age)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_5 <- pop3 %>%
  tabyl(mat_age)%>%
  mutate(percent = round((percent*100), digits = 2))

summary_table <- cbind(pop_all, pop_1, pop_2, pop_5)
rownames(summary_table) <- c("20-29", "<20", ">40", "30-39")
colnames(summary_table) <- c(  "item", "N_all", "per_all", "item_1", "N_1", "per_1",
                               "item_2", "N_2", "per_2", "item_5", "N_5", "per_5")



#maternal age
summary(pop0$age_birth) #31 (27-35)
summary(pop1$age_birth) #31 (27-35)
summary(pop2$age_birth) #31 (27-35)
summary(pop3$age_birth) #31 (27-35)

#number of siblings
summary(pop0$n_children) #2 (1-2)
summary(pop1$n_children) #2 (1-2)
summary(pop2$n_children) #2 (1-2)
summary(pop3$n_children) #2 (1-2)

#birth order
pop_all <- pop0 %>%
  tabyl(birth_order_cat)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_1 <- pop1 %>%
  tabyl(birth_order_cat)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_2 <- pop2 %>%
  tabyl(birth_order_cat)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_5 <- pop3 %>%
  tabyl(birth_order_cat)%>%
  mutate(percent = round((percent*100), digits = 2))

tmp <- cbind(pop_all, pop_1, pop_2, pop_5)
rownames(tmp) <- c("1", ">3", "2", "3")
colnames(tmp) <- c(  "item", "N_all", "per_all", "item_1", "N_1", "per_1",
                     "item_2", "N_2", "per_2", "item_5", "N_5", "per_5")
summary_table <- rbind(summary_table, tmp)


summary(pop0$birthorder)  #1 (1-2)
summary(pop1$birthorder)  #1 (1-2)
summary(pop2$birthorder)  #1 (1-2)
summary(pop3$birthorder)  #1 (1-2)

#gender of the children
tabyl(pop0$gender_child)
tabyl(pop1$gender_child)
tabyl(pop2$gender_child)
tabyl(pop3$gender_child)

#flu risk groups
tabyl(pop0$flurisk) #71138  (17.90%)
tabyl(pop1$flurisk) #66838  (17.91%)
tabyl(pop2$flurisk) #62740  (18.00%)
tabyl(pop3$flurisk) #54189  (18.10%)

#ethnicity
pop_all <- pop0 %>%
  tabyl(ethnicity_5)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_1 <- pop1 %>%
  tabyl(ethnicity_5)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_2 <- pop2 %>%
  tabyl(ethnicity_5)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_5 <- pop3 %>%
  tabyl(ethnicity_5)%>%
  mutate(percent = round((percent*100), digits = 2))

tmp <- cbind(pop_all, pop_1, pop_2, pop_5)
rownames(tmp) <- c("white", "Asian/British Asian", "Black/Black British",
                          "Mixed", "Other", "Unknown/missing")
colnames(tmp) <- c(  "item", "N_all", "per_all", "item_1", "N_1", "per_1",
                     "item_2", "N_2", "per_2", "item_5", "N_5", "per_5")
summary_table <- rbind(summary_table, tmp)

# IMD
pop_all <- pop0 %>%
  tabyl(imd)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_1 <- pop1 %>%
  tabyl(imd)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_2 <- pop2 %>%
  tabyl(imd)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_5 <- pop3 %>%
  tabyl(imd)%>%
  mutate(percent = round((percent*100), digits = 2))

tmp <- cbind(pop_all, pop_1, pop_2, pop_5)
rownames(tmp) <- c("1 (Least deprived)", "2", "3",
                   "4", "5 (Most deprived)")
colnames(tmp) <- c(  "item", "N_all", "per_all", "item_1", "N_1", "per_1",
                     "item_2", "N_2", "per_2", "item_5", "N_5", "per_5")
summary_table <- rbind(summary_table, tmp)

#region
pop_all <- pop0 %>%
  tabyl(region)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_1 <- pop1 %>%
  tabyl(region)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_2 <- pop2 %>%
  tabyl(region)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_5 <- pop3 %>%
  tabyl(region)%>%
  mutate(percent = round((percent*100), digits = 2))

tmp <- cbind(pop_all, pop_1, pop_2, pop_5)
rownames(tmp) <- c("North East", 
                   "North West", 
                   "Yorkshire & The Humber", 
                   "East Midland", 
                   "West Midlands", 
                   "East of England", 
                   "London", 
                   "South East",
                   "South West")
colnames(tmp) <- c(  "item", "N_all", "per_all", "item_1", "N_1", "per_1",
                     "item_2", "N_2", "per_2", "item_5", "N_5", "per_5")
summary_table <- rbind(summary_table, tmp)

#Mental health issue before 8 weeks
pop_all <- pop0 %>%
  tabyl(MHI_vacc1)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_1 <- pop1 %>%
  tabyl(MHI_vacc1)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_2 <- pop2 %>%
  tabyl(MHI_vacc1)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_5 <- pop3 %>%
  tabyl(MHI_vacc1)%>%
  mutate(percent = round((percent*100), digits = 2))

tmp <- cbind(pop_all, pop_1, pop_2, pop_5)
rownames(tmp) <- c("Any SMI", "CMD", "CMD & SUD", "None", "SUD")
colnames(tmp) <- c(  "item", "N_all", "per_all", "item_1", "N_1", "per_1",
                     "item_2", "N_2", "per_2", "item_5", "N_5", "per_5")
summary_table <- rbind(summary_table, tmp)


#MHI before one year
pop_all <- pop0 %>%
  tabyl(MHI_vacc2)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_1 <- pop1 %>%
  tabyl(MHI_vacc2)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_2 <- pop2 %>%
  tabyl(MHI_vacc2)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_5 <- pop3 %>%
  tabyl(MHI_vacc2)%>%
  mutate(percent = round((percent*100), digits = 2))

tmp <- cbind(pop_all, pop_1, pop_2, pop_5)
rownames(tmp) <- c("Any SMI", "CMD", "CMD & SUD", "None", "SUD")
colnames(tmp) <- c(  "item", "N_all", "per_all", "item_1", "N_1", "per_1",
                     "item_2", "N_2", "per_2", "item_5", "N_5", "per_5")
summary_table <- rbind(summary_table, tmp)

#before 18 months
pop_all <- pop0 %>%
  tabyl(MHI_vacc3)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_1 <- pop1 %>%
  tabyl(MHI_vacc3)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_2 <- pop2 %>%
  tabyl(MHI_vacc3)%>%
  mutate(percent = round((percent*100), digits = 2))

pop_5 <- pop3 %>%
  tabyl(MHI_vacc3)%>%
  mutate(percent = round((percent*100), digits = 2))

tmp <- cbind(pop_all, pop_1, pop_2, pop_5)
rownames(tmp) <- c("Any SMI", "CMD", "CMD & SUD", "None", "SUD")
colnames(tmp) <- c(  "item", "N_all", "per_all", "item_1", "N_1", "per_1",
                     "item_2", "N_2", "per_2", "item_5", "N_5", "per_5")
summary_table <- rbind(summary_table, tmp)

#finishing formatting
summary_table <- as.data.table(summary_table)
summary_table[, all := paste0(N_all, " ", "(", per_all, ")")]
summary_table[, group1 := paste0(N_1, " ", "(", per_1, ")")]
summary_table[, group2 := paste0(N_2, " ", "(", per_2, ")")]
summary_table[, group5 := paste0(N_5, " ", "(", per_5, ")")]
summary_table[, N_all := NULL]
summary_table[, per_all := NULL]
summary_table[, N_1 := NULL]
summary_table[, per_1 := NULL]
summary_table[, item_1 := NULL]
summary_table[, N_2 := NULL]
summary_table[, per_2 := NULL]
summary_table[, item_2 := NULL]
summary_table[, N_5 := NULL]
summary_table[, per_5 := NULL]
summary_table[, item_5 := NULL]


colnames (summary_table)
write.table(summary_table, paste0(results, "summary_table.csv"), sep = "\t", row.names = T,
            dec = ".")

################################################################################
#alternative exposure definitions
#only diagnosis
df1 <- read_parquet(paste0(datafiles, "final_studypop_diagnoses_only.parquet"))
diag_pop <- unique(df1[, list(patid, babypatid, pracid, age_startfu_baby, age_endfu_baby, age_birth,
                        birthorder, n_children, ethnicity_5, region, imd, gender_child,
                        MHI_vacc1, MHI_vacc2, MHI_vacc3,
                        flurisk)], by = "babypatid")
diag_pop1 <- diag_pop[age_endfu_baby >= 365]
diag_pop2 <- diag_pop[age_endfu_baby >= 730]
diag_pop3 <- diag_pop[age_endfu_baby >= 1825]

#with prescription
df2 <- read_parquet(paste0(datafiles, "final_studypop_all_pluspres.parquet"))
pres_pop <- unique(df2[, list(patid, babypatid, pracid, age_startfu_baby, age_endfu_baby, age_birth,
                              birthorder, n_children, ethnicity_5, region, imd, gender_child,
                              MHI_vacc1, MHI_vacc2, MHI_vacc3,
                              flurisk)], by = "babypatid")
pres_pop1 <- pres_pop[age_endfu_baby >= 365]
pres_pop2 <- pres_pop[age_endfu_baby >= 730]
pres_pop3 <- pres_pop[age_endfu_baby >= 1825]


#before age 8 weeks
pop_1_all <- pop1 %>%
  tabyl(MHI_vacc1)%>%
  mutate(percent = round((percent*100), digits = 2))
pop_1_diag <- diag_pop1 %>%
  tabyl(MHI_vacc1)%>%
  mutate(percent = round((percent*100), digits = 2))
pres_1_diag <- pres_pop1 %>%
  tabyl(MHI_vacc1)%>%
  mutate(percent = round((percent*100), digits = 2))

summary_table <- cbind(pop_1_all, pop_1_diag, pres_1_diag)
rownames(summary_table) <- c("Any SMI", "CMD", "CMD & SUD", "None", "SUD")
colnames(summary_table) <- c("general", "N_1", "per_1",
                     "diag only", "N_2", "per_2", "prescription", "N_5", "per_5")

#before age one year
pop_2_all <- pop2 %>%
  tabyl(MHI_vacc2)%>%
  mutate(percent = round((percent*100), digits = 2))
pop_2_diag <- diag_pop2 %>%
  tabyl(MHI_vacc2)%>%
  mutate(percent = round((percent*100), digits = 2))
pres_2_diag <- pres_pop2 %>%
  tabyl(MHI_vacc2)%>%
  mutate(percent = round((percent*100), digits = 2))

tmp <- cbind(pop_2_all, pop_2_diag, pres_2_diag)
rownames(tmp) <- c("Any SMI", "CMD", "CMD & SUD", "None", "SUD")
colnames(tmp) <- c("general", "N_1", "per_1",
                   "diag only", "N_2", "per_2", "prescription", "N_5", "per_5")
summary_table <- rbind(summary_table, tmp)

#before age of 18 months
pop_5_all <- pop3 %>%
  tabyl(MHI_vacc3)%>%
  mutate(percent = round((percent*100), digits = 2))
pop_5_diag <- diag_pop3 %>%
  tabyl(MHI_vacc3)%>%
  mutate(percent = round((percent*100), digits = 2))
pres_5_diag <- pres_pop3 %>%
  tabyl(MHI_vacc3)%>%
  mutate(percent = round((percent*100), digits = 2))

tmp <- cbind(pop_5_all, pop_5_diag, pres_5_diag)
rownames(tmp) <- c("Any SMI", "CMD", "CMD & SUD", "None", "SUD")
colnames(tmp) <- c("general", "N_1", "per_1",
                   "diag only", "N_2", "per_2", "prescription", "N_5", "per_5")
summary_table <- rbind(summary_table, tmp)
colnames(summary_table)

#finishing formatting
summary_table <- as.data.table(summary_table)
summary_table[, all := paste0(N_1, " ", "(", per_1, ")")]
summary_table[, diagnosos := paste0(N_2, " ", "(", per_2, ")")]
summary_table[, prescription := paste0(N_5, " ", "(", per_5, ")")]
summary_table[, N_1 := NULL]
summary_table[, per_1 := NULL]
summary_table[, N_2 := NULL]
summary_table[, per_2 := NULL]
summary_table[, N_5 := NULL]
summary_table[, per_5 := NULL]
summary_table[, `diag only`:= NULL]

write.table(summary_table, paste0(results, "summary_MHI_sensitivity.csv"), sep = "\t", row.names = T,
            dec = ".")
