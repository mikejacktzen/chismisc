

# classic 2-way FE --------------------------------------------------------

# the textbook '2-way fixed effects' is a regression 
# using an interaction of indicators

# See "Introductory Econometrics Fifth Edition" (Woolridge 2012)
# Chapter 13.2 Policy Analysis with Pooled Cross Sections

# use CHIS survey weights in regression


library(survey)

# standard setup to set the survey design
# see line 30 of web examples
# https://github.com/mikejacktzen/chismisc/blob/main/web-examples/examples-chis-web.R#L30

chis_design <- dat_raw %>%
  svrepdesign( 
    data = . , 
    weights = ~ RAKEDW0 , 
    repweights = "RAKEDW[1-80]" , 
    type = "other" , 
    scale = 1 , 
    rscales = 1 , 
    mse = TRUE)

# below uses survey weighted regression via survey::svyglm()

?survey::svyglm

# outcome   variable for the outcome of interest
# ind_BA    1/0 for Before vs After 'roll out period'
# ind_TC    1/0 for Treatment vs Control
# x1        adjustment variable

survey::svyglm(outcome ~ ind_BA + ind_TC + ind_BA:ind_TC + x1,
               design=chis_design)



