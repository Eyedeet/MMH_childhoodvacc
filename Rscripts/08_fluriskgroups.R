####identifying mothers with flu risk group condition
pop <- data.table(read_parquet(paste0(datafiles, "mothers_studypop_eth_dep_preg.parquet")))
pop <- pop[, patid := as.character(patid)]
pop <- pop[, pracid := as.character(pracid)]
colnames(pop)


setwd(mothers_parquet)
obs.files<- list.files(path = mothers_parquet, pattern = "\\Observation")


creating_dataframe_pop <- function(sheet_name){
  
  condition <- as.data.table(readxl::read_xlsx(path = paste0(codelists, "Influenza_at_risk_conditions.xlsx"), sheet = sheet_name))
  print(sheet_name)
  tmp <- extractor_med(obs.files, codelist = condition)
  print(length(unique(tmp$patid)))
  tmp[, flurisk := 1]
  tmp[, obsdate := as.Date(obsdate, format = "%d/%m/%Y")]
  tmp[, patid := as.character(patid)]
  tmp[, pracid := as.character(pracid)]
  tmp <- tmp[, list(patid, pracid, obsdate, flurisk)]
  tmp <- merge(tmp, pop, by = c("patid", "pracid"), all.x = T)
  tmp <- tmp[flurisk == 1, keep := obsdate<= pregstart] #drop these with record after pregnancy of interest
  tmp <- tmp [keep == T]
  tmp[, keep := NULL]
  print(length(unique(tmp$patid)))
  return(tmp)
}


#all sheet names 
sheets <- c(
  
  "Aurum_chronic_heart", "Aurum_chronic_resp", 
  "Aurum_asthma", 
  "Aurum_chronic_kid",
  "Aurum_chronic_liver", "Aurum_haem_cancer", "Aurum_diabetes", "Aurum_HIV",
  "Aurum_asplenia", "Aurum_chronic_neur",
  "Aurum_perm_immune", "Aurum_organ",  
  "Aurum_learning")


####running code over all sheets 

for(i in 1:length(sheets)){
  cli::cli_progress_bar("loop flu risk groups")
  df <- creating_dataframe_pop(sheet_name = sheets[i])
  write_parquet(df, paste0(datafiles, "_", sheets[i], "_flurisk.parquet"))
  rm(df)
}
cli::cli_progress_done()


#adding asthma
asthma <- as.data.table(readxl::read_xlsx(path = paste0(codelists, "Influenza_at_risk_conditions.xlsx"), sheet = "Aurum_asthma"))
any(duplicated(asthma$medcodeid))
tmp <- extractor_med(obs.files, codelist = asthma)
tmp[, flurisk := 1]
tmp[, obsdate := as.Date(obsdate, format = "%d/%m/%Y")]
tmp[, patid := as.character(patid)]
tmp[, pracid := as.character(pracid)]
tmp <- tmp[, list(patid, pracid, obsdate, flurisk)]
tmp <- unique(tmp)
tmp <- merge(tmp, pop, by = c("patid", "pracid"), all.x = T)
tmp <- tmp[flurisk == 1, keep := obsdate<= pregstart] #drop these with record after pregnancy of interest
tmp <- tmp [keep == T]
tmp[, keep := NULL]
write_parquet(tmp, paste0(datafiles, "_", "Aurum_asthma", "_flurisk.parquet"))


###--- adding all flu conditions together
#permanent flu conditions
flu.files<- list.files(path = datafiles, pattern = "flurisk")

df <- read_parquet(paste0(datafiles, flu.files[[1]]))
for(i in 2:length(flu.files)){
  
  tmp <- read_parquet(paste0(datafiles, flu.files[[i]]))
  df <- rbind(df, tmp)
  
}

#quality checks
all(df$obsdate <= df$pregstart)
all(as.numeric(year(df$obsdate)) >= df$yob)

nrow(df) #528,436
length(unique(df$patid)) #72,991
df <- df[as.numeric(year(obsdate)) >= yob]
nrow(df) #528,133
length(unique(df$patid)) #72,984

#deduplicating women with several entries
df <- unique(df[, list(patid, pracid, babypatid, flurisk)])
nrow(df)#92,937
length(unique(df$patid)) #72,984

pop_new <- merge(pop, df, by = c("patid", "pracid", "babypatid"), all.x = T)

write_parquet(pop_new, paste0(datafiles,"mothers_studypop_all_cov.parquet" ))
