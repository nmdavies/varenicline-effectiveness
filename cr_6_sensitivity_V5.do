//Gemma Taylor 28-4-16

log using "Z:\5_effectabstinence\logs\cr_3_statistics_sensitivity V5", replace
set more off

//SENSITIVITY ANALYSIS
	//Comparison of primary outcome model (at 2 years/730 days) whereby patients with missing data are assumed to be smokers (out_quit_730), compared to sensitivity outcome model... //
		//...whereby patients with missing smoking data are assumed to be quitters (sens_quit_730)
			
use "5_effectabstinence/data/effectabstinence.dta", clear

//determine how many smokers had missing follow-up data
gen sens_miss_data=1 if (out_quit_730-sens_quit_730)!=0
tab sens_miss_data if sens_miss_data==1
tab out_quit_730
tab sens_quit_730

//Does baseline data for those missing outcome data, differ by exposure group (eTable 2.15)
	//By treatment arm - means
	foreach j in cov_age cov_gp_visit cov_bmi history { 
		ttest `j' if sens_miss_data==1 , by (dr_varenicline)
			}
				
	//By treatment arm - proportions  (eTable 2.15)
	foreach j in cov_sex cov_charlson_ever cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever  ///
	cov_depression_ever  cov_neuroticdis_ever  cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever  ///
	cov_hyp_anxioly_ever  cov_rare_med_ever cov_rare_mental_ever {
		tab dr_varenicline `j' if sens_miss_data==1, row 
		}
			
		//By treatment arm - medians   (eTable 2.15)
	 foreach j in cov_Rx_year cov_imd {
		 tabstat `j' if sens_miss_data==1, statistics (median ) by (dr_varenicline)
		}
		

	//Baseline data for imputed dataset, across treatment groups  (eTable 2.15)
	use "5_effectabstinence/data/effectabstinence_imputed.dta",clear
	set more off
	compress
	gen sens_miss_data=1 if (out_quit_730-sens_quit_730)!=0
	mi estimate: mean cov_bmi if sens_miss_data==1, over (dr_varenicline) 
	tabstat cov_imd if _mi_m!=0 & sens_miss_data==1, stats(median)
	tabstat cov_imd if _mi_m!=0 & sens_miss_data==1, stats(median) by (dr_varenicline)


//Traditional logistic regression models - Odds ratios - partial and fully adjusted models in imputed data (eTable 2.16)
	foreach i in 730 {
	//primary outcome model
	mi estimate, or: logistic out_quit_`i' dr_varenicline cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10, ro cluster (staffid)
	mi estimate, or: logistic out_quit_`i' dr_varenicline history cov_imd cov_gp_visit cov_bmi cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 ///
		cov_charlson_ever cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever   ///
		cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever cov_rare_mental_ever, ro cluster (staffid)
	
	//sensitivity outcome model
	mi estimate, or: logistic sens_quit_`i' dr_varenicline cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10, ro cluster (staffid)
	mi estimate, or: logistic sens_quit_`i' dr_varenicline history cov_imd cov_gp_visit cov_bmi cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 ///
		cov_charlson_ever cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever   ///
		cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever cov_rare_mental_ever, ro cluster (staffid)
		}

 
//Instrumental variable models - Risk difference - regular and IV linear regression models in imputed data (eTable 2.16)
	use "5_effectabstinence/data/effectabstinence.dta",clear
	set more off
	compress
	//Generate the instrument
	set seed 12345 
	gen rand=uniform()
	bys staffid (Rx_eventdate rand):gen IV_varenicline=dr_varenicline[_n-1]
			
	//Derive IV estimates and estimates from partial adjusted regressions	
	foreach i in 730 {
		//primary outcome model
		reg out_quit_`i' dr_varenicline cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 if IV_varenicline!=. ,ro cluster (staffid)
		ivreg2 out_quit_`i' (dr_varenicline=IV_varenicline) cov_Rx_year_1-cov_Rx_year_10, ro cluster (staffid) endog(dr_varenicline)

		//sensitivity outcome model
		reg sens_quit_`i' dr_varenicline cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 if IV_varenicline!=. ,ro cluster (staffid)
		ivreg2 sens_quit_`i' (dr_varenicline=IV_varenicline) cov_Rx_year_1-cov_Rx_year_10, ro cluster (staffid) endog(dr_varenicline)
		}
//
	
log close
