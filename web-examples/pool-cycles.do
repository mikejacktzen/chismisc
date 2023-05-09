* stata
* https://healthpolicy.ucla.edu/chis/analyze/Pages/sample-code-pooling.aspx

log using "folder location\data_step.log", replace
***CHIS 2017 Adult data****
use "your folder location\CHIS 2017 data"

gen year=2017

gen fnwgt0=rakedw0/2

for new fnwgt1-fnwgt160: gen X=0

foreach i of numlist 1/80{
            local j=`i'-0
            replace fnwgt`i'=rakedw`j'/2

}

foreach i of numlist 81/160{
            replace fnwgt`i'=rakedw0/2

}

save adult17 , replace

***CHIS 2018 Adult data****
use "folder location\CHIS 2018 data"

gen year=2018

gen fnwgt0=rakedw0/2

for new fnwgt1-fnwgt160: gen X=0

foreach i of numlist 1/80{           
            replace fnwgt`i'=rakedw0/2

}

foreach i of numlist 81/160{
            local j=`i'-80
            replace fnwgt`i'=rakedw`j'/2
}

append using adult17 /*this step concatenates the data files*/
save "folder location\combined.dta", replace

svyset [pw=fnwgt0], jkrw(fnwgt1-fnwgt160, multiplier(1)) vce(jack) mse
