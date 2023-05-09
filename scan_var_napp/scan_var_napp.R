#' The 'scan_var_napp()' function to scan user supplied code file for variable names not approved on the dac project variable list 
#'
#' @param fp_mvl a character string of the file path of the users approved '.xlsx' master variable list
#' @param sheet a character string of the "ADULT" "CHILD" or "TEEN" tab in the MVL
#' @param code_client a character vector whose elements are each line of the users code. Result of ?readLines()
#' @param lgl_code_tolower a logical TRUE/FALSE whether the variable names in the user's code are lowercase or not
#'
#' @return a 2 column dataframe with column 'var_notapp' for the not approved variables and the associated column 'lines' for the line number(s) of the user supplied code where the variable was found
#' @export
#'
#' @examples
#' 
#' # file path to client code to inspect
#' # program files .do .sas or .r are all text files with lines of code
#' 
#' getwd()
#' 
#' fp_code_client = paste0(getwd(),"/your_ex_stata.do")
#' # fp_code_client = '/.../ex_fake_stata.do'
#' 
#' code_client = readLines(con = file(fp_code_client))
#' head(code_client)
#' tail(code_client)
#' str(code_client)
#' 
#' # # NOT RUN:
#' # # when read in, you have a character vector
#' # # where each element is a line of text code
#' # code_client0 = c('fake line1',
#' #                  'fake line2',
#' #                  'regress ADLTCNT CHLDCNT HH_SIZE FAMCNT',
#' #                  "",
#' #                  '/* this is a comment with mention of var FAMCNT not approved in mvl */')
#' # 
#' # # The above is the r internal representation of
#' # # the corresponding external '.do .sas. or .r' file 
#' # # that looks like 
#' # 
#' # writeLines(code_client0)
#' 
#' 
#' # file path mvl with selected / approved variables
#' 
#' fp_mvl = paste0(getwd(),"/Copy of MasterVariableList.xlsx")
#' 
#' # toy example MVL
#' # 'ADLTCNT' 'CHLDCNT' are approved with "X" in the cell
#' # 'HH_SIZE' 'FAMCNT' not approved
#' 
#' scan_var_napp(code_client,
#'               lgl_code_tolower = TRUE,
#'               fp_mvl=fp_mvl,
#'               sheet="ADULT")
#' 
#' scan_var_napp(code_client,
#'               lgl_code_tolower = FALSE,
#'               fp_mvl=fp_mvl,
#'               sheet="ADULT"
#' )
#' 
#' scan_var_napp(code_client,
#'               lgl_code_tolower = FALSE,
#'               fp_mvl=fp_mvl,
#'               sheet="TEEN")
#' 
#' scan_var_napp(code_client,
#'               lgl_code_tolower = FALSE,
#'               fp_mvl=fp_mvl,
#'               sheet="CHILD")

scan_var_napp <- function(fp_mvl,
                          sheet="ADULT",
                          code_client,
                          lgl_code_tolower=TRUE
) {
  
  require(readxl)
  require(dplyr)
  
  message('This may flag comments or unrelated partial matches.\nIt is up to the user to review and distinguish.\n')
  if(isTRUE(lgl_code_tolower)){
    message(paste0("This function expected you used lowercase variable names in your code. If not the case, switch the 'lgl_code_tolower' argument to FALSE\n"))
  }else{
    message(paste0("This function expected you used UPPERCASE variable names in your code. If not the case, switch the 'lgl_code_tolower' argument to TRUE\n"))
  }
  
  # supplied by user 
  mvl = readxl::read_xlsx(fp_mvl,
                          sheet=sheet)
  
  # var approved marked with "X"
  df_app = mvl %>% 
    # mvl[-1,] %>% 
    filter(!grepl(`VARIABLE NAME`,pattern="SECTION [A-Z]+")) %>% 
    
    select(`VARIABLE NAME`,starts_with("CHIS ")) %>% 
    # select(where(~!all(is.na(.x)))) %>% 
    filter(if_any(.cols=everything(),
                  .fns = ~ grepl(x=.x,
                                 pattern="^[xX]$")))
  
  # var not approved == everything else
  vars_notapp = setdiff(mvl[-1,]$`VARIABLE NAME`,
                        df_app$`VARIABLE NAME`)
  
  # lgl_tolower=FALSE
  if(isTRUE(lgl_code_tolower)){
    vars_notapp = tolower(vars_notapp)
  }
  
  
  # "HH_SIZE" %in% vars_notapp
  # "FAMCNT" %in% vars_notapp
  
  # improve regex with word boundaries \bfoo\b
  # but using boundary unwanted skips for
  # i.foo
  # as.factor(foo)
  # so maybe just look for foo
  
  lst_vna_lines = lapply(vars_notapp,
                         FUN=function(var1){
                           
                           # var1 = "HH_SIZE"
                           # grep(pattern=paste0("\\b",var1,"\\b"),
                           #      x=code_client)
                           
                           # 'instype' %in% vars_notapp
                           # var1 = "instype"
                           ind_linecode = grep(pattern=paste0(var1),
                                               x=code_client)
                           
                           return(paste0(ind_linecode,collapse=','))
                         })
  
  # head(lst_vna_lines)
  
  names(lst_vna_lines) = vars_notapp
  ind_lst_vna = lapply(lst_vna_lines,FUN=function(xx){nchar(xx)>0})
  lst_vna_lines_only = lst_vna_lines[which(unlist(ind_lst_vna))]
  
  df_vna_lines = data.frame(var_notapp=names(lst_vna_lines_only),
                            lines=unlist(lst_vna_lines_only),
                            row.names = NULL)
  return(df_vna_lines)
  
}

