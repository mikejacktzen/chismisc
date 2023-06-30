
# developed for 1-yr cycle dummy data files

library(tidyverse)
library(purrr)

directory_dummyfiles_unzipped = "..\\data-chis-dummy\\unzipped"

filepath_mvl="..\\MasterVariableList.xlsx"

file_path_save = "..\\data-chis-dummy\\mvl-approved\\"



read_dta_add_filename <- function(filename){
  # filename=files_dta[[21]]
  dat = haven::read_dta(filename)
  
  dat = dat %>% 
    mutate(filename = filename) %>% 
    
    # manually dling files some older zip files named the same
    # leaves (1) artifact, so use [0-9]{2,} to get 2 digits
    
    mutate(year_file=str_match(filename,'[0-9]{2,}')) %>% 
    mutate(atc_file=str_match(toupper(filename),"ADULT|TEEN|CHILD")) %>% 
    rename_with(toupper)
  
  # dat %>% select(FILENAME,YEAR_FILE,ATC_FILE)
  
  return(dat)
}



# throw out not (yet) approved variables of your MVL
# keep the X approved variables only


keep_approved_vars = function(filepath_mvl,
                              df_dta,
                              toupper=TRUE,
                              file_path_save){
  
  
  # df_dta = list_dta[[1]]
  sheet = df_dta %>% distinct(ATC_FILE) %>% unlist
  
  year_file = unlist(df_dta[1,'YEAR_FILE'],use.names = F)
  if(nchar(year_file)==2){
    # front pad '20' for edge case years with only '15'
    year_file = paste0("20",year_file)
  }
  
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
  
  df_dta_cut = df_dta %>% select(contains(df_app[["VARIABLE NAME"]]))
  # data will not have our explicitly created FILENAME,YEAR_FILE,ATC_FILE
  
  haven::write_dta(df_dta_cut,paste0(file_path_save,sheet,"_",year_file,".dta"))
  
}



# pipe together statements ------------------------------------------------

# unzipped files in 'directory_dummyfiles_unzipped'
# get opened and cut according to your 'filepath_mvl'
# saved in 'file_path_save'

directory_dummyfiles_unzipped %>% 
  
  list.files(path = .,full.names = T,recursive = TRUE) %>% 
  purrr::keep(str_detect(string=.,pattern="format")) %>% 
  
  purrr::map(read_dta_add_filename) %>% 
  
  purrr::walk(keep_approved_vars,
              filepath_mvl=filepath_mvl,
              file_path_save=file_path_save)

message(file_path_save)


