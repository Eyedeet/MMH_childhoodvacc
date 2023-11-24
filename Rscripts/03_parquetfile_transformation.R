###saving the maternal files as parquet files

#---maternal data
#list all observation files
observation <- list.files(path = raw_data_mothers, pattern = "\\Observation")
#list all practice files
practice <- list.files(path = raw_data_mothers, pattern = "\\Practice")
#list all patient files
patient <- list.files(path = raw_data_mothers, pattern = "\\Patient")
#list all drug issues 
drugs <- list.files(path = raw_data_mothers, pattern = "\\DrugIssue")

###function to transform all the files for each category into parquett files
parquett_transform <- function(files){
  filenames <- tools::file_path_sans_ext(basename(files))
  
  for (i in cli::cli_progress_along(files)) {
    write_parquet(data.table(read.table(files[i], header = TRUE, sep = "\t", na.strings = "")), 
                  paste0(mothers_parquet, filenames[i], ".parquet"))
  }
}




#writing parquett files
setwd(raw_data_mothers)
parquett_transform(files = patient)
parquett_transform(files = practice)
parquett_transform(files = drugs)
parquett_transform(files = observation)


#---children's data
#list all observation files
observation <- list.files(path = raw_data_children, pattern = "\\Observation")
#list all practice files
practice <- list.files(path = raw_data_children, pattern = "\\Practice")
#list all patient files
patient <- list.files(path = raw_data_children, pattern = "\\Patient")
#list all drug issues 
drugs <- list.files(path = raw_data_children, pattern = "\\DrugIssue")

###function to transform all the files for each category into parquett files
parquett_transform <- function(files){
  filenames <- tools::file_path_sans_ext(basename(files))
  
  for (i in cli::cli_progress_along(files)) {
    write_parquet(data.table(read.table(files[i], header = TRUE, sep = "\t", na.strings = "")), 
                  paste0(children_parquet, filenames[i], ".parquet"))
  }
}



parquett_transform <- function(files){
  filenames <- tools::file_path_sans_ext(basename(files))
  
  for (i in cli::cli_progress_along(files)) {
    write_parquet(data.table(read.table(files[i], header = TRUE, sep = "\t", na.strings = "")), 
                  paste0(children_parquet, filenames[i], ".parquet"))
  }
}


setwd(raw_data_children)


#writing parquet files
parquett_transform(files = patient)
parquett_transform(files = practice)
parquett_transform(files = drugs)
parquett_transform(files = observation)
