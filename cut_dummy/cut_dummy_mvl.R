# developed for 1-yr cycle dummy data of formatted stata .dta and sas .sas7bdat files
# RENAME STATA AND SAS FILES TO INCLUDE THE CAPITLIZED AGE GROUP (EITHER "ADULT" OR "TEEN" OR "CHILD") 
# AND THE YEAR OF THE DATA IN 20## FORMAT
# Ex: ADULT_2021

library(tidyverse)
library(purrr)

# enter folder location of all downloaded Stata and SAS dummy files
filepath_dummyfiles_unzipped = "..\\data-chis-dummy\\unzippeddatafiles\\"

# enter location and name of master variable list excel sheet updated with desired variables
filepath_mvl = "..\\MasterVariableList.xlsx"

# enter output data folder location
filepath_save = "..\\data-chis-dummy\\output\\"

# enter either ".dta" for Stata or ".sas7bdat" for SAS for the format of downloaded dummy files
mydatatype = ".dta"



read_dta_add_filename <- function(filename, datatype=".dta"){
  require(dplyr)
  require(stringr)
  require(haven)
  
  # files_dta = filepath_dummyfiles_unzipped %>%
  #   list.files(path = .,full.names = T,recursive = TRUE) %>%
  #   purrr::keep(str_detect(string=.,pattern="format"))
  # filename=files_dta[[16]]
  
  atc_file_0 = as.character(str_match(toupper(filename),"ADULT|TEEN|CHILD"))
  
  # manually dling files some older zip files named the same
  # leaves (1) artifact, so use [0-9]{2,} to get 2+ YYYY digits
  
  year_file_0 = as.character(str_match(filename,'[0-9]{2,}'))
  
  if(datatype==".dta") {
    
    dat = haven::read_dta(filename)
    
  }
  
  if(datatype==".sas7bdat") {
    
    dat = haven::read_sas(filename)
    
  }
  
  dat2 = dat %>% 
    mutate(filename = filename) %>% 
    mutate(atc_file=atc_file_0) %>% 
    
    # some files only have last 2 YY digits missing the prefix 20
    mutate(year_file=ifelse(nchar(year_file_0)==2,
                            paste0("20",year_file_0),
                            year_file_0)) %>%
    rename_with(toupper)
  
  # dat2 %>% select(FILENAME,YEAR_FILE,ATC_FILE)
  
  return(dat2)
  
}



# throw out not (yet) approved variables of your MVL
# keep the X approved variables only

keep_approved_vars = function(filepath_mvl,
                              df_dta,
                              toupper=TRUE,
                              datatype=".dta",
                              filepath_save){
  
  require(dplyr)
  require(readxl)
  require(haven)
  
  # df_dta = list_dta[[1]]
  sheet = df_dta %>% distinct(ATC_FILE) %>% unlist
  
  year_file = unlist(df_dta[1,'YEAR_FILE'],use.names = F)
  
  mvl = readxl::read_xlsx(filepath_mvl,
                          sheet=sheet)
  
  # var approved marked with "X"
  df_app = mvl %>% 
    # mvl[-1,] %>% 
    filter(!grepl(`VARIABLE NAME`,pattern="SECTION [A-Z]+")) %>% 
    
    # focus to specific year file
    select(`VARIABLE NAME`, paste0("CHIS ",year_file)) %>% 
    
    # select(`VARIABLE NAME`,starts_with("CHIS ")) %>% 
    # select(where(~!all(is.na(.x)))) %>% 
    filter(if_any(.cols=everything(),
                  .fns = ~ grepl(x=.x,
                                 pattern="^[xX]$")))
  
  # names(df_dta)
  
  df_dta_cut = df_dta %>% select((df_app[["VARIABLE NAME"]]))
  # data will not have our explicitly created FILENAME,YEAR_FILE,ATC_FILE
  
  if(datatype == ".dta") {
    
    haven::write_dta(df_dta_cut,paste0(filepath_save,sheet,"_",year_file,".dta"))
    
  }
  
  if(datatype == ".sas7bdat") {
    
    haven::write_sas(df_dta_cut,paste0(filepath_save,sheet,"_",year_file,".sas7bdat"))
    
  }
  
}



# pipe together statements ------------------------------------------------

# unzipped stata and sas files in 'filepath_dummyfiles_unzipped'

# read files and cut according to your 'filepath_mvl'
# saved in 'filepath_save'

filepath_dummyfiles_unzipped %>% 
  
  list.files(path = .,full.names = T,recursive = TRUE) %>% 
  purrr::keep(str_detect(string=.,pattern=mydatatype)) %>% 
  
  purrr::map(read_dta_add_filename,
             datatype = mydatatype) %>% 
  
  purrr::walk(keep_approved_vars,
              filepath_mvl = filepath_mvl,
              filepath_save = filepath_save,
              datatype = mydatatype)

message(filepath_save)