clear
cd Z:\
log using "Z:\5_effectabstinence\logs\cr_1_effectabstinence", replace
///Paper five - Taylor et al. The effectiveness of varenicline versus nicotine replacement therapy on long-term smoking cessation in primary care

//UPDATE 7/12/2015//Neil Davies 18/06/11// revised ND & GT 16/10/16
//This .do file merges relevant individual level datasets - eligible nrt&varenicline cohorts with smoking status data

set max_memory .
cd z:\ 
set more off

//use first eligible smoking cessation prescription data
use "tempdata\first_eligible_smoking_cessation_Rx.dta", clear
codebook patid

//merge in individual level smoking status
joinby patid using "tempdata\individual_level_smoke.dta", unmatched(both)
codebook patid
tab _m
drop _m

//merge in individual level covariate data
joinby patid using "z:\tempdata\indiv_cov_all.dta", unmatched (both)
codebook patid
tab _m
drop _m

//merge in survival analysis outcomes
joinby patid using "tempdata\individual_level_survival.dta", unmatched (both)
compress
codebook patid
tab _merge
drop _merge


//merge together uncommon covariates (autism, dementia, eating disorder, hyperkinetic disorder, learning disorder, other behavioral disorder, personality disorder, schizophrenia) //
	//and uncommon medication prescriptions (CNS medication and dementia medication)
gen cov_rare_mental_ever=0
replace cov_rare_mental_ever=1 if (cov_autism_ever==1 | cov_dementia_ever==1 | cov_eatingdis_ever==1 | cov_hyperkineticdis_ever==1 | cov_learningdis_ever==1 |  ///
cov_otherbehavdis_ever==1 | cov_persondis_ever==1 | cov_schizop_ever==1)
label variable cov_rare_mental_ever "occurence of rare mental health conditions"

gen cov_rare_med_ever=0
replace cov_rare_med_ever=1 if (cov_cns_stim_ever==1 | cov_dementiameds_ever==1)
label variable cov_rare_med_ever "occurence of rare psychotropic medications"

drop cov_autism_ever cov_dementia_ever cov_eatingdis_ever cov_hyperkineticdis_ever cov_learningdis_ever cov_otherbehavdis_ever cov_persondis_ever cov_schizop_ever  ///
cov_cns_stim_ever cov_dementiameds_ever

compress
notes _dta: cohort of people prescribed NRT & varenicline, with their smoking status at follow-up points - all protocol restrictions applied. Davies et al, 2015, BMJOpen.
save "Z:\5_effectabstinence\data\effectabstinence.dta", replace

sum patid prodcode staffid Rx_eventdate dr_varenicline follow_up history exclude n_physician out_quit_1460 out_quit_730 out_quit_365 out_quit_270 out_quit_180 out_quit_90 sens_quit_730 cov_bmi cov_imd 
log close

