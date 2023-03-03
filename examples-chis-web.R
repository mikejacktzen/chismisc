library(haven)

# ?read_sas

setwd("")

dat_raw = read_sas('raw/ADULT_SAS_PUF_2021/SAS/adult.sas7bdat')
# dat_raw = read_sas('raw/ADULT_SAS_DUMMY_2021/SAS/dummy_adult.sas7bdat')

dat_raw = read_dta('raw/adult_2018_stata/ADULT.dta')


dim(dat_raw)
names(dat_raw)

library(survey)

?svrepdesign 

dim(dat_raw)
str(dat_raw)
names(dat_raw)


library(dplyr)

dat_raw %>% distinct(AB22V2)
# 1=yes, 2=no

chis_design <- dat_raw %>% 
  mutate(AB22V2_01 = AB22V2-1) %>% # 0/1
  
  svrepdesign( 
    # data = dat_raw ,
    data = . , 
    weights = ~ RAKEDW0 , 
    repweights = "RAKEDW[1-80]" , 
    type = "other" , 
    scale = 1 , 
    rscales = 1 , 
    mse = TRUE 
  )

str(chis_design)

# DOCTOR EVER TOLD HAVE DIABETES
svymean(~AB22V2_01,chis_design)
# .8898 vs .8889


# mean --------------------------------------------------------------------

# svy: mean bmi_p, over(racehpr2)
# svy: mean bmi_p, over(srsex racehpr2)

names(dat_raw)
grep(names(dat_raw),pattern='bmi',value=TRUE,ignore.case = TRUE)
grep(names(dat_raw),pattern='race',value=TRUE,ignore.case = TRUE)
grep(names(dat_raw),pattern='srsex',value=TRUE,ignore.case = TRUE)

# ?svyby

chis_design |> 
  svyby( formula = ~ BMI_P ,
         by = ~ RACEHP2_P1,
         FUN=svymean,
         deff = TRUE)

chis_design |> 
  svyby( formula = ~ BMI_P ,
         by = ~ RACEHP2_P1+SRSEX,
         FUN=svymean,
         deff = TRUE)

# svyr
library(srvyr)

chis_design %>% 
  srvyr::as_survey() %>% 
  group_by(RACEHP2_P1,SRSEX) %>%
  summarize(mean_bmi = survey_mean(BMI_P))

# freq --------------------------------------------------------------------

# svy: tabulate astcur racehpr2, col se ci
# svy, subpop (if srsex==1): tab astcur racehpr2, col se ci
# svy, subpop (if srsex==2): tab astcur racehpr2, col se ci

# ?subset.svyrep.design

grep(names(dat_raw),pattern='astcur',value=TRUE,ignore.case = TRUE)

chis_design |> subset(SRSEX=="1") |> 
  svytable(formula=~ASTCUR+RACEHP2_P1) |>
  prop.table() 

chis_design |> subset(SRSEX=="1") |> 
  svytable(formula=~ASTCUR+RACEHP2_P1) |>
  # prop.table() |> 
  summary()

chis_design |> subset(SRSEX=="1") |> 
  svytotal(x=~interaction(ASTCUR, RACEHP2_P1))

chis_design |> subset(SRSEX=="1") |> 
  svymean(x=~interaction(ASTCUR, RACEHP2_P1))



# https://github.com/gergness/srvyr/issues/51
# not convinced srvyr does subset / filtering correctly
# so instead of subset filtering by v1
# use group_by(v1,interact(v2,v3))
# for a level of v1, using interact() for joint denoms,
# results in props that sum to 1 for that level of v1

# # # svyr
# library(srvyr)
# chis_design %>%
#   srvyr::as_survey() %>%
#   group_by(SRSEX,  # acts like filtering to subpop
#            srvyr::interact(ASTCUR, RACEHP2_P1)) %>%
#   summarize(prop = survey_prop())
# 
# # svyr takes longer

# ols regression --------------------------------------------------------------

# xi: svy: regress bmi_p i.srsex i.race srage_p

grep(names(dat_raw),pattern='SRAGE_P',value=TRUE,ignore.case = TRUE)

chis_design |> 
  svyglm(formula = BMI_P ~ as.factor(SRSEX) + 
           as.factor(RACEHP2_P1) + 
           SRAGE_P1,
         family=stats::gaussian(),
         rescale=TRUE)


# logistic regression -----------------------------------------------------

# recode astcur (2=0) (1=1) (-9=.), gen (ast)
# xi: svy: logit ast srage_p i.race i.srsex


grep(names(dat_raw),pattern='ast',value=TRUE,ignore.case = TRUE)

# update to create / modify variables of survey design
?update.survey.design

summary(chis_design$variables$ASTCUR)

chis_design |> 
  stats::update(AST = ifelse(ASTCUR==2,0,1)) |>
  svyglm(formula = AST ~ SRAGE_P1 + 
           as.factor(RACEHP2_P1) + 
           as.factor(SRSEX),
         family=quasibinomial(),
         rescale=TRUE)

  
# srvyr ezier for harder data manipulations via dplyr syntax

library(srvyr)

chis_design %>% 
  srvyr::as_survey() %>% 
  dplyr::mutate(AST=case_when(ASTCUR==2 ~ 0,
                              ASTCUR==1 ~ 1,
                              TRUE ~ NA)) %>% 
  svyglm(formula = AST ~ SRAGE_P1 + 
           as.factor(RACEHP2_P1) + 
           as.factor(SRSEX),
         family=quasibinomial(),
         rescale=TRUE)
