
//Open log
clear
log using "Z:\5_effectabstinence\logs\cr_3_statistics_Q1 V5 final doublecheck", replace

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
ssc install ivreg2, replace
ssc install psmatch2, replace

//Prepare Stata
set max_memory .
cd z:/

//Open the data
use "5_effectabstinence/data/effectabstinence.dta",clear
set more off

//Missing data report
mdesc


//RUN BASELINE CHARACTERISTICS & DESCRIPTIVES (Table 1)
	//In whole sample - means
	summarize cov_age cov_gp_visit cov_bmi history
	
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
	
	//Outcome descriptives - how many people quit at each follow-up? (eTable 2.4)
	foreach i in 90 180 270 365 730 1460 {
			tab   dr_varenicline out_quit_`i', row
		}

	//Run descriptives for imputed data, whole sample and across treatment groups (eTable 2.1)
	use "5_effectabstinence/data/effectabstinence_imputed.dta",clear
	set more off
	compress
	
	mi estimate: mean cov_bmi
	mi estimate: mean cov_bmi, over (dr_varenicline) 
	
	tabstat cov_imd if _mi_m!=0, stats(median)
	tabstat cov_imd if _mi_m!=0, stats(median) by (dr_varenicline)
	

//RESEARCH AIM 1 - WHAT IS THE EFFECT OF VARENICLINE ON SMOKING STATUS?

//SUPPLEMENTARY ANALYSIS: COX PROPORTIONAL HAZARDS MODEL

	///Cox regressions: partial and fully adjusted models (eTable 2.3)
	/*mi unregister first_smoke_date out_first_smoke*/
	set more off
	mi stset first_smoke_date, failure(out_first_smoke) ex(follow_up)
		
		//Partial adjusted
		mi estimate, esampvaryok errorok hr: stcox dr_varenicline cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 , ro cluster (staffid)
		
		//Fully adjusted
		mi estimate, esampvaryok hr: stcox dr_varenicline cov_imd cov_gp_visit cov_bmi cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10  cov_charlson_ever ///
			cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever  cov_neuroticdis_ever cov_selfharm_ever cov_antidepres_ever  ///
			cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever cov_rare_mental_ever, ro cluster (staffid)

	//Graph survival plots & obtain events per/100000, and phtest
		use "z:/5_effectabstinence/data/effectabstinence.dta",clear
		set more off
		compress
				
		//Derive estimates to plot curves for people with 4 years or less follow-up (eFigure 2)
		stset first_smoke_date, failure(out_first_smoke) /*  ex(follow_up)*/
		stcox dr_varenicline history /*cov_imd */ cov_gp_visit /*cov_bmi */ cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 cov_charlson_ever ///
			cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever cov_selfharm_ever ///
			cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever cov_rare_mental_ever, ro cluster (staffid) 
		stcurve , hazard  at1(dr_varenicline=1 ) at2(dr_varenicline=0) range (179 1460) 
		stcurve , survival  at1(dr_varenicline=1 ) at2(dr_varenicline=0) range (179 1460) 
		
		//Test of proportional hazards assumptions (eTable 2.2)
		stcox dr_varenicline history cov_imd cov_gp_visit cov_bmi cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 cov_charlson_ever ///
			cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever cov_selfharm_ever  ///
			cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever cov_rare_mental_ever , ro cluster (staffid)
		estat phtest, detail
		stset first_smoke_date , fail( out_first_smoke ) id( patid ) scale(365.25)
		stptime, by(dr_varenicline) 
		graph save Graph "Z:\5_effectabstinence\images\eFig2_cox.gph", replace
	
		
		
//ANALYSIS 1: LOGISTIC REGRESSION MODELS ODDS-RATIOS
use "5_effectabstinence/data/effectabstinence_imputed.dta", clear

//Odds ratio analysis - traditional logistic regressions - Odds ratios - partial and fully adjusted (Table 2, Figure 1)
		foreach i in 90 180 270 365 730 1460 {
		mi estimate, or: logistic out_quit_`i' dr_varenicline cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10, ro cluster (staffid)
			}
		foreach i in 90 180 270 365 730 1460 {
		mi estimate, or: logistic out_quit_`i' dr_varenicline history cov_imd cov_gp_visit cov_bmi cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 cov_charlson_ever ///
			cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever  cov_selfharm_ever cov_antidepres_ever ///
			cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever cov_rare_mental_ever, ro cluster (staffid)
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
	
	//Average propensity score model  (eTable 2.6)
	mi estimate, esampvaryok errorok or: logistic dr_varenicline history cov_sex cov_age cov_bmi cov_imd cov_gp_visit cov_Rx_year_1-cov_Rx_year_10 ///
		cov_charlson_ever  cov_drug_misuse_ever cov_alcohol_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever ///
		cov_rare_mental_ever cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever 
		
	//PSM model adequacy check - Covariate balance (eFigure 4, eTable 2.7)
	set more off
	compress
	pstest history cov_sex cov_age cov_bmi cov_imd cov_gp_visit cov_Rx_year, both 
	pstest cov_charlson_ever cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever, both 
	pstest cov_depression_ever cov_neuroticdis_ever cov_selfharm_ever cov_rare_mental_ever, both
	pstest cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever cov_rare_med_ever, both 
	
	//PSM model adequacy check - Kernal density plots of treatment groups' propensity score distributions before and after matching 
		//Before matching (eFigure 5)
		twoway (kdensity pscore if dr_varenicline==1) (kdensity pscore if dr_varenicline==0), legend(label(1 "Varenicline") label(2 "NRT" ) subtitle ///
		("Treatment groups' propensity score distributions before matching"))
		graph save Graph "Z:\5_effectabstinence\images\eFig5_ps.gph", replace
		
		//After matching (eFigure 6)
		twoway (kdensity pscore if  treated==1 & weight==1)(kdensity pscore if treated==0 & weight==1), legend( label(1 "Varenicline") label( 2 "NRT") ///
		subtitle  ("Treatment groups' propensity score distributions after matching") )
		graph save Graph "Z:\5_effectabstinence\images\eFig6_ps.gph", replace
	
	//Save PSM dataset for future use
	compress
	save "5_effectabstinence/data/Q1_effectabstinence_psm_imputed.dta",replace
    set more off
		
		
	//PROPENSITY SCORE MATCHED - LOGISTIC REGRESSION ANALYSIS (eTable 2.5)
		use "5_effectabstinence/data/Q1_effectabstinence_psm_imputed.dta",clear 
		compress
		
		//Calculate ORs
		foreach i in 90 180 270 365 730 1460 {
			mi estimate, esampvaryok errorok or: logistic out_quit_`i' dr_varenicline pscore if weight==1 , ro cluster (staffid)
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
	
	//How many quitters were there by instrumental variable condition (eTable 2.8)
	foreach i in 90 180 270 365 730 1460 {
		tab IV_varenicline out_quit_`i' if IV_varenicline!=., row
		}
		
	//Derive IV estimates and estimates from partial adjusted regressions(eTable 2.9)
	foreach i in 90 180 270 365 730 1460 {
		reg out_quit_`i' dr_varenicline cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 if IV_varenicline!=. ,ro cluster (staffid)
		ivreg2 out_quit_`i' (dr_varenicline=IV_varenicline) cov_Rx_year_1-cov_Rx_year_10, ro cluster (staffid) endog(dr_varenicline)
		}
//			

		log close		

		


	






