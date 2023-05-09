#' pool_cycles() r function to pool 2+ CHIS data sets that you stored in a ?list
#'
#' @param list_dat_raw list whose elements are data frames, each a CHIS data set
#' @param vec_fctr_prop_yr vector whose numeric elements are proportions of total years that each data set represents
#'
#' @return data frame with pooled weights that have been adjusted. Columns are weights and Rows are observations.
#' 
#' This returns a single data set of pooled weights. Other survey variables not attached by design.
#' Leaving it to the end-user to 'harmonize' variables across data sets.
#' 
#' @export
#'
#' @examples
#' 
#' # setwd("/.../")
#' 
#' library(haven)
#' dat_raw1 = haven::read_sas('raw/ADULT_SAS_PUF_2021/SAS/adult.sas7bdat')
#' dat_raw2 = haven::read_sas('raw/adult_2020_sas/adult_2020_sas.zip/adult.sas7bdat')
#' 
#' list_dat_raw = list(dat_raw1, dat_raw2)  # list of data sets
#' # ?lapply; ?list.files; ?haven::read_sas
#' 
#' # assume user has / will harmonize variable names across the 2+ datasets
#' # setdiff(names(dat_raw1),names(dat_raw2))
#' 
#' pooled_wgts_only = pool_cycles(list_dat_raw,
#'                                vec_fctr_prop_yr = c(1/2, 1/2))
#'                                
#' 
#' dim(pooled_wgts_only)
#' 
#' # expect number of rows in df to be == to total number of rows in list of raw
#' sum(unlist(lapply(list_dat_raw, nrow)))
#' 
#' # expect number of columns to be = 1 + 80 x num of datasets pooled
#' (1 + 80 * length(list_dat_raw))
#'
#' # not run:
#' # save output                                
#' # write.csv(out,'/.../')
#' # haven::write_sas(out,'/.../')
#' # haven::write_dta(out,'/.../')
#' #
#' # example of 3 files: 1 year, 1 year, and 2 year data sets                               
#' # list_dat_raw = list(dat_raw1,  # 1 year file
#' #                     dat_raw2,  # 1 year file
#' #                     dat_raw3)  # 2 year file
#' #
#' # vec_fctr_prop_yr = c(1 / 4,
#' #                      1 / 4,
#' #                      2 / 4)
#' #
#' # pooled_wgts_only(list_dat_raw,vec_fctr_prop_yr)
#'                                

pool_cycles = function(list_dat_raw, vec_fctr_prop_yr) {
  stopifnot(length(vec_fctr_prop_yr) == length(list_dat_raw))
  
  # assumes exists 0 value design weights
  # assumes no negative weights
  
  # select-weights-focus -----------------------------------------------------------
  
  # subset to RAKED weights only
  # RAKEDW0 == base
  # RAKEDW[1-80] == replicate
  
  # # dplyr version
  # # library(dplyr)
  # # base weights W0
  # list_wbase = lapply(list_dat_raw,dplyr::select,contains("RAKEDW0"))
  # # rep weights W[1-9]
  # list_wrep = lapply(list_dat_raw,dplyr::select,matches("^RAKEDW[1-9]"))
  
  ## base r lapply grep [] version
  list_wbase_raw = lapply(
    list_dat_raw,
    FUN = function(x) {
      # x=list_dat_raw[[1]]
      x[grep(names(x), pattern = "^RAKEDW0", value = TRUE)]
    }
  )
  
  list_wrep_raw = lapply(
    list_dat_raw,
    FUN = function(x) {
      # x=list_dat_raw[[1]]
      x[grep(names(x), pattern = "^RAKEDW[1-9]", value = TRUE)]
    }
  )
  
  
  # weight-scale-prop-year ---------------------------------------------------------------
  
  # apply any weight scaling here to previous raw weights
  #
  # scale := proportion of years represented by each dataset
  #
  # num years the i-th data set represents
  # divided by
  # num years pooled data sets represents
  #
  # your supplied factor adjust weights
  # vec_fctr_prop_yr
  #
  # if your scaling factor is the same across all datasets
  # eg (1/2) for DS1 and (1/2) for DS2
  #
  # vec_fctr_prop_yr = c(1/2,1/2)
  #
  # if your weight scaling factor is different for some datasets
  # eg (1/3) for DS1 and (2/3) for DS2
  # pooling one 1-year dataset and one 2-year dataset
  # vec_fctr_prop_yr = c(1/3 , 2/3)
  
  
  # easier for loop
  list_wbase = list(length = length(list_wbase_raw))
  for (i in seq_along(list_wbase_raw)) {
    list_wbase[[i]] = vec_fctr_prop_yr[[i]] * list_wbase_raw[[i]]
  }
  
  list_wrep = list(length = length(list_wrep_raw))
  for (i in seq_along(list_wbase_raw)) {
    list_wrep[[i]] = vec_fctr_prop_yr[[i]] * list_wrep_raw[[i]]
  }
  
  # append-base -------------------------------------------------------------
  
  # row wise stack base weights into vector
  
  col_wbase_stacked = unlist(list_wbase, use.names = FALSE)
  # length(col_wbase_stacked)
  
  # block-diag-rep --------------------------------------------------------------
  
  # form block diagonal replicate weights
  
  library(Matrix)
  
  # use -99 placeholder for actual 0 value rep weights
  # to distinguish from later convenience 0 for off block diag
  
  list_wrep_99 = lapply(
    list_wrep,
    FUN = function(x) {
      xx = as.matrix(x)
      
      xx[xx == 0] <- -99
      return(xx)
    }
  )
  
  # rep weights as block diag
  # bdiag() will have convenience 0s in off diagonals
  
  mat_bdiag = Matrix::bdiag(list_wrep_99)
  
  # any(mat_bdiag == -99);  any(mat_bdiag == 0)
  # nrow(mat_bdiag);  length(col_wbase_stacked)
  
  # row wise replace each convenience 0 with base weight
  
  # get matrix index of off diag convenience 0s
  # and use as row index of wbase for lookup and replacement
  
  ind_offdiag = which(mat_bdiag == 0, arr.ind = TRUE)
  mat_bdiag[ind_offdiag] <- col_wbase_stacked[ind_offdiag[, 1]]
  
  # replace -99 placeholder back to original 0
  ind_99 = which(mat_bdiag == -99, arr.ind = TRUE)
  mat_bdiag[ind_99] <- 0
  
  # any(mat_bdiag < 0);  sum(mat_bdiag == 0);  ncol(mat_bdiag)
  
  # final col names of output, match sas/stata example name "fnwgt"
  colnames(mat_bdiag) = paste0("FNWGT", 1:ncol(mat_bdiag))
  
  out_weight = cbind(data.frame(FNWGT0 = col_wbase_stacked),
                     data.frame(as.matrix(mat_bdiag)))
  
  # # expect number of columns to be = 1 + 80 x num of datasets pooled
  # ncol(out_weight) == (1 + 80*length(list_dat_raw))
  
  return(out_weight)
}


