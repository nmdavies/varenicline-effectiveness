//Open log
clear
log using "Z:\5_effectabstinence\logs\cr_3_statistics_Q1 V4", replace

///create blank .dta files for regsave tables 
generate var1 = . 
save "Z:\5_effectabstinence\tables\cox models", replace
clear
generate var1 = . 
save "Z:\5_effectabstinence\tables\logistic models",replace
clear
generate var1 = . 
save "Z:\5_effectabstinence\tables\iv models", replace
clear





//This do file runs the statistical analyses for paper five
//Gemma Taylor 7-12-2015 to 11-1-16
//Use "dofiles_create\cr_1_add_covariates.do" to add in covaraites before using this .do cr_2_statistics

//The original data are stored in the variables indicated by mi_m1==0
//The distributions of BMI and IMD in the original data are:
/*mean cov_bmi if mi_m1==0
mean cov_imd if mi_m1==0*/

//list of covariates 
/*history cov_sex cov_age cov_imd cov_gp_visit cov_bmi cov_Rx_year_* cov_charlson_ever  cov_bipolar_ever  cov_depression_ever  ///
cov_alcohol_misuse_ever cov_drug_misuse_ever cov_neuroticdis_ever  cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever  /// 
cov_rare_med_ever cov_rare_mental_ever */

//Update programs 
/*ssc install ivreg2, replace
ssc install psmatch2, replace
ssc install outreg, replace
*/

//Prepare Stata
set max_memory .
cd z:/

//Open the data
use "5_effectabstinence/data/effectabstinence.dta",clear
set more off

//Missing data report
mdesc


//BASELINE CHARACTERISTICS & DESCRIPTIVES
	//In whole sample - means
	summarize cov_age cov_gp_visit  cov_bmi history
	
	//In whole sample - proportions
	foreach j in cov_sex cov_charlson_ever cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever  ///
		cov_neuroticdis_ever cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever  ///
		cov_rare_mental_ever 	{
		tab `j' 
		}
	
	//In whole sample - medians 
	 foreach j in cov_Rx_year cov_imd {
		 tabstat `j', statistics (median ) 
		}
		
	//By treatment arm - means
	foreach j in cov_age cov_gp_visit  cov_bmi history { 
		ttest `j', by (dr_varenicline)
			}
				
	//By treatment arm - proportions
	foreach j in cov_sex cov_charlson_ever cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever  ///
	cov_depression_ever  cov_neuroticdis_ever  cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever  ///
	cov_hyp_anxioly_ever  cov_rare_med_ever cov_rare_mental_ever {
		tab dr_varenicline `j', row 
		}
			
		//By treatment arm - medians 
	 foreach j in cov_Rx_year cov_imd {
		 tabstat `j', statistics (median ) by (dr_varenicline)
		 		 mlogit dr_varenicline `j'
		}
		
	///How many people without missing data by treatment arm
	tab dr_varenicline if cov_imd!=.
	tab dr_varenicline if cov_bmi!=.
	
	//Outcome descriptives - how many people quit at each follow-up?
	foreach i in 90 180 270 365 730 1460 {
			tab   dr_varenicline out_quit_`i', row
		}

	//Descriptives for imputed data, whole sample and across treatment groups.
	use "5_effectabstinence/data/effectabstinence_imputed.dta",clear
	set more off
	compress
	
	mi estimate: mean cov_bmi
	mi estimate: mean cov_bmi, over (dr_varenicline) 
	
	tabstat cov_imd if _mi_m!=0, stats(median)
	tabstat cov_imd if _mi_m!=0, stats(median) by (dr_varenicline)
	

//RESEARCH AIM 1 - WHAT IS THE LONG TERM EFFECT OF VARENICLINE ON SMOKING STATUS?

//SUPPLEMENTARY ANALYSIS: COX PROPORTIONAL HAZARDS MODEL

	///Cox regressions: partial and fully adjusted models
	/*mi unregister first_smoke_date out_first_smoke*/
	set more off
	mi stset first_smoke_date, failure(out_first_smoke) ex(follow_up)
		
		//Partial adjusted
		mi estimate, esampvaryok errorok hr: stcox dr_varenicline cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 , ro cluster (staffid)
		regsave dr_varenicline using "Z:\5_effectabstinence\tables\cox models",  pval ci replace  addlabel(model, partial adjusted Cox) 
		
		//Fully adjusted
		mi estimate, esampvaryok hr: stcox dr_varenicline cov_imd cov_gp_visit cov_bmi cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10  cov_charlson_ever ///
			cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever  cov_neuroticdis_ever cov_selfharm_ever cov_antidepres_ever  ///
			cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever cov_rare_mental_ever, ro cluster (staffid)
		regsave dr_varenicline using "Z:\5_effectabstinence\tables\cox models",  pval ci append  addlabel(model, fully adjusted Cox) 

	//Graph survival plots & obtain events per/100000, and phtest
		use "z:/5_effectabstinence/data/effectabstinence.dta",clear
		set more off
		compress
				
		//Derive estimates to plot curves for people with 4 years or less follow-up
		stset first_smoke_date, failure(out_first_smoke) /*  ex(follow_up)*/
		stcox dr_varenicline history /*cov_imd */ cov_gp_visit /*cov_bmi */ cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 cov_charlson_ever ///
			cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever cov_selfharm_ever ///
			cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever cov_rare_mental_ever, ro cluster (staffid) 
		stcurve , hazard  at1(dr_varenicline=1 ) at2(dr_varenicline=0) range (179 1460) 
		stcurve , survival  at1(dr_varenicline=1 ) at2(dr_varenicline=0) range (179 1460) 

		stcox dr_varenicline history cov_imd cov_gp_visit cov_bmi cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 cov_charlson_ever ///
			cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever cov_selfharm_ever  ///
			cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever cov_rare_mental_ever , ro cluster (staffid)
		estat phtest, detail
		stset first_smoke_date , fail( out_first_smoke ) id( patid ) scale(365.25)
		stptime, by(dr_varenicline) 
		graph save Graph "Z:\5_effectabstinence\images\Cox_survival_plot.gph", replace

		
		
		
//ANALYSIS 1: LOGISTIC REGRESSION MODELS-ODDS RATIOS
use "5_effectabstinence/data/effectabstinence_imputed.dta", clear

//Odds ratio analysis - traditional logistic regressions - Odds ratios - partial and fully adjusted
		foreach i in 90 180 270 365 730 1460 {
		mi estimate, or: logistic out_quit_`i' dr_varenicline cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10, ro cluster (staffid)
			regsave dr_varenicline using "Z:\5_effectabstinence\tables\logistic models",  pval ci append  addlabel(model, partial adjusted OR `i') 
			}
		foreach i in 90 180 270 365 730 1460 {
		mi estimate, or: logistic out_quit_`i' dr_varenicline history cov_imd cov_gp_visit cov_bmi cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 cov_charlson_ever ///
			cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever  cov_selfharm_ever cov_antidepres_ever ///
			cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever cov_rare_mental_ever, ro cluster (staffid)
			regsave dr_varenicline using "Z:\5_effectabstinence\tables\logistic models",  pval ci append  addlabel(model, fully adjusted OR `i') 
			}


//	
		
	
//ANALYSIS 2: PROPENSITY SCORE MATCHING
	set more off
	
	//Ensure observations are randomly sorted before calling PSMATCH2
	set seed 12345
	tempvar sortorder
	gen `sortorder' = runiform()
	sort `sortorder'
	compress 
	
	//Create propensity score matched sample within multiple imputation sets
	foreach j in pscore treated support weight id n1 nn pdif{
		gen `j'=.
		}
	set more off
	forvalues i=1(1)20{
		psmatch2 dr_varenicline history cov_sex cov_age cov_bmi cov_imd cov_gp_visit cov_Rx_year_1-cov_Rx_year_10 cov_charlson_ever  ///
			cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever cov_selfharm_ever  ///
			cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever cov_rare_mental_ever if _mi_m==`i' ,  outcome ///
			(out_quit_90) common noreplace logit odds neighbor(1)
		foreach j in pscore treated support weight id n1 nn pdif {
		replace `j'=_`j' if `j'==.
		compress
		}
		}
	
	//Average propensity score model 
	mi estimate, esampvaryok errorok or: logistic dr_varenicline history cov_sex cov_age cov_bmi cov_imd cov_gp_visit cov_Rx_year_1-cov_Rx_year_10 ///
		cov_charlson_ever  cov_drug_misuse_ever cov_alcohol_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever ///
		cov_rare_mental_ever cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever 
	
	/*regsave history cov_sex cov_age cov_bmi cov_imd cov_gp_visit cov_Rx_year_1-cov_Rx_year_10 cov_charlson_ever cov_drug_misuse_ever  ///
	cov_alcohol_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever cov_rare_mental_ever cov_selfharm_ever  ///
	cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever using "Z:\5_effectabstinence\tables\s_table_2-3", ///
	replace detail (all) addlabel (model, average propensity score model) */

	
	//PSM model adequacy check - Covariate balance
	set more off
	compress
	pstest history cov_sex cov_age cov_bmi cov_imd cov_gp_visit cov_Rx_year, both 
	pstest cov_charlson_ever cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever, both 
	pstest cov_depression_ever cov_neuroticdis_ever cov_selfharm_ever cov_rare_mental_ever, both
	pstest cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever, both 

	
	//PSM model adequacy check - Kernal density plots of treatment groups' propensity score distributions before and after matching 
		//Before matching
		twoway (kdensity pscore if dr_varenicline==1) (kdensity pscore if dr_varenicline==0), legend(label(1 "Varenicline") label(2 "NRT" ) subtitle ///
		("Treatment groups' propensity score distributions before matching"))
		graph save Graph "Z:\5_effectabstinence\images\ps_distr_before.gph", replace
		
		//After matching
		twoway (kdensity pscore if  treated==1 & weight==1)(kdensity pscore if treated==0 & weight==1), legend( label(1 "Varenicline") label( 2 "NRT") ///
		subtitle  ("Treatment groups' propensity score distributions after matching") )
		graph save Graph "Z:\5_effectabstinence\images\ps_distr_after.gph", replace
	
	//Save PSM dataset for future use
	compress
	save "5_effectabstinence/data/Q1_effectabstinence_psm_imputed.dta",replace
    set more off
		
		
	//PROPENSITY SCORE MATCHED - LOGISTIC REGRESSION ANALYSIS
		use "5_effectabstinence/data/Q1_effectabstinence_psm_imputed.dta",clear 
		compress

		//How many people quit at two year follow-up in each arm within each MI set?
		set more off
		forvalues i=1(1)20 {
			tabulate dr_varenicline out_quit_730 if _mi_m==`i' & weight==1 & support==1, row
			}
	
		
		//Calculate ORs
		foreach i in 90 180 270 365 730 1460 {
			mi estimate, esampvaryok errorok or: logistic out_quit_`i' dr_varenicline pscore if weight==1 , ro cluster (staffid)
			regsave dr_varenicline using "Z:\5_effectabstinence\tables\logistic models",  pval ci append  addlabel(model, propensity OR `i') 
			}
			
		//How many patients and events/treatment group were matched within each imputation set?
		forvalues i=1(1)20 {
			tab treated out_quit_90 if _mi_m==`i' & weight==1 & support==1 , row
			}
	
//	

		
	
//ANALYSIS 3: INSTRUMENTAL VARIABLE ANALYSES
	
	//IV model
	use "5_effectabstinence/data/effectabstinence.dta",clear
	set more off
	compress
	
	//Generate the instrument
	set seed 12345 
	gen rand=uniform()
	bys staffid (Rx_eventdate rand):gen IV_varenicline=dr_varenicline[_n-1]
	
	//How many quitters were there by instrumental variable condition
	foreach i in 90 180 270 365 730 1460 {
		tab IV_varenicline out_quit_`i' if IV_varenicline!=., row
		}
		
	//Derive IV estimates and estimates from partial adjusted regressions	
	foreach i in 90 180 270 365 730 1460 {
		reg out_quit_`i' dr_varenicline cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 if IV_varenicline!=. ,ro cluster (staffid)
			regsave dr_varenicline using "Z:\5_effectabstinence\tables\iv models",  pval ci append  addlabel (model, normal linear regression `i') 
		ivreg2 out_quit_`i' (dr_varenicline=IV_varenicline) cov_Rx_year_1-cov_Rx_year_10, ro cluster (staffid) endog(dr_varenicline)
			regsave dr_varenicline using "Z:\5_effectabstinence\tables\iv models",  pval ci append  addlabel(model, iv regression `i') 
		}
			

	//Association of confounders with the instrument (IV), and with the exposure
	
	logistic IV_varenicline history cov_sex cov_age cov_imd cov_gp_visit cov_bmi cov_Rx_year_1-cov_Rx_year_10 cov_charlson_ever ///
		cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever cov_rare_mental_ever  ///
		cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever  if IV_varenicline!=., ro cluster (staffid)
	regsave history cov_sex cov_age cov_imd cov_gp_visit cov_bmi cov_Rx_year_2-cov_Rx_year_9 cov_charlson_ever ///
		cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever cov_selfharm_ever ///
		cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever cov_rare_mental_ever  ///
		using "Z:\5_effectabstinence\tables\s_table 2-11",  pval ci replace  addlabel (model, confounders assoc. with IV) 

	logistic dr_varenicline history cov_sex cov_age cov_imd cov_gp_visit cov_bmi cov_Rx_year_1-cov_Rx_year_10 cov_charlson_ever ///
		cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever  cov_depression_ever cov_neuroticdis_ever cov_rare_mental_ever  ///
		cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever  if IV_varenicline!=., ro cluster (staffid)
	regsave history cov_sex cov_age cov_imd cov_gp_visit cov_bmi cov_Rx_year_2-cov_Rx_year_9 cov_charlson_ever ///
		cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever cov_selfharm_ever ///
		cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever cov_rare_mental_ever  ///
		using "Z:\5_effectabstinence\tables\s_table 2-12",  pval ci replace  addlabel (model, confounders assoc. with exposure) 
		

		
//SENSITIVTY ANALYSIS
	//comparison of primary outcome model where patients with missing data are assumed to be smokers (out_quit_730), compared to sensitivity outcome model... //
	//...where patients with missing smoking data are assumed to be quitters (sens_quit_730)
			
use "5_effectabstinence/data/effectabstinence.dta", clear

//determine how many smokers had missing follow-up data
gen sens_miss_data=1 if (out_quit_730-sens_quit_730)!=0
tab sens_miss_data if sens_miss_data==1
tab out_quit_730
tab sens_quit_730

//Does baseline data for those missing outcome data, differ by exposure group
	//By treatment arm - means
	foreach j in cov_age cov_gp_visit  cov_bmi history { 
		ttest `j' if sens_miss_data==1 , by (dr_varenicline)
			}
				
	//By treatment arm - proportions
	foreach j in cov_sex cov_charlson_ever cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever  ///
	cov_depression_ever  cov_neuroticdis_ever  cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever  ///
	cov_hyp_anxioly_ever  cov_rare_med_ever cov_rare_mental_ever {
		tab dr_varenicline `j' if sens_miss_data==1, row 
		}
			
		//By treatment arm - medians 
	 foreach j in cov_Rx_year cov_imd {
		 tabstat `j' if sens_miss_data==1, statistics (median ) by (dr_varenicline)
		 		 
		}
		

	//Baseline data for imputed dataset, across treatment groups.
	use "5_effectabstinence/data/effectabstinence_imputed.dta",clear
	set more off
	compress
	
	mi estimate: mean cov_bmi
	mi estimate: mean cov_bmi, over (dr_varenicline) 
	
	tabstat cov_imd if _mi_m!=0, stats(median)
	tabstat cov_imd if _mi_m!=0, stats(median) by (dr_varenicline)


//Traditional logistic regression models - Odds ratios - partial and fully adjusted models in imputed data- 
	
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


//Instrumental variable models - Risk difference - regular and IV linear regression models in imputed data
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

	

		log close		

		


	






