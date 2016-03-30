//Neil Davies 07/01/16
//This imputes the effect of treatment on abstinence
ssc install ICE, replace 
cd z:\
set more off
use "5_effectabstinence\data\effectabstinence.dta",clear
mi set mlong
order cov_bmi cov_imd
ice Rx_eventdate-cov_hyp_anxioly_ever cov_bmi o.cov_imd, saving(5_effectabstinence\data\effectabstinence_imputed_1, replace) m(20) seed(12345) 

//Open the imputed dataset
use "5_effectabstinence\data\effectabstinence_imputed_1.dta",clear

//Reset all the mi data
mi unset
mi import ice, automatic clear 
mi register regular patid-cov_hyp_anxioly_ever
mi describe

save "5_effectabstinence\data\effectabstinence_imputed.dta",replace

/*
//The original data are stored in the variables indicated by _mi_m==0
//The distributions of BMI and IMD in the original data are:

mean cov_bmi if _mi_m==0
mean cov_imd if _mi_m==0

//The distributions of BMI and IMD in the imputed data are:
mi estimate:mean cov_bmi cov_imd

//We can run regressions using:

mi estimate : reg out_quit_730 dr_varenicline 
mi estimate : reg out_quit_730 dr_varenicline  cov_bmi cov_imd 
*/
//All of the other post-estimation commands such as est store and regsave should work following regressions using imputed data.
