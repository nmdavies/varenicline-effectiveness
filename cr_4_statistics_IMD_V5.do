clear
//Open log
log using "Z:\5_effectabstinence\logs\cr_4_statistics_IMD_V5", replace

//This do file runs the statistical analyses for paper five
//Gemma Taylor 7-12-2015 to 11-1-16
//Use "dofiles_create\cr_1_add_covariates.do" to add in covaraites before using this .do cr_2_statistics

//The original data are stored in the variables indicated by mi_m1==0
//The distributions of BMI and IMD in the original data are:

//list of covariates 
/*history cov_sex cov_age cov_imd cov_gp_visit cov_bmi cov_Rx_year_* cov_charlson_ever cov_alcohol_misuse_ever cov_bipolar_ever //
  cov_depression_ever cov_drug_misuse_ever cov_neuroticdis_ever cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever 
  cov_rare_med_ever cov_rare_mental_ever */

//Install/update programs
ssc install ivreg2, replace
ssc install psmatch2, replace


//Prepare Stata
clear
set max_memory .
set more off
cd z:/




//RESEARCH QUESTION 2 - DO THE EFFECTS OF VARENICLINE DIFFER BY SES GROUP?
use "5_effectabstinence/data/effectabstinence.dta",clear
set more off
compress

//Descriptive data - the proportion of smokers who attend their GP for smoking cessation treatment by IMD (eTable 2.10)
	///Are there any differences in smoking cessation medication prescriptions between areas of high and low deprivation 
	///Generate categorical variable
	gen cov_imd_categ =.
	replace cov_imd_categ=3 if cov_imd<=20 & cov_imd>=16 
	replace cov_imd_categ=2 if cov_imd<=15 & cov_imd>=11
	replace cov_imd_categ=1 if cov_imd<=10 & cov_imd>=6
	replace cov_imd_categ=0 if cov_imd<=5 & cov_imd>=0
	label define cov_imd_categ 0 "cov_imd<=5 & >=0" 1 "cov_imd<=10 & cov_imd>6" 2 "cov_imd<=15 & cove_imd>=11" 3 "cov_imd<=20 & cov_imd>=16"
	label variable cov_imd_categ "0=least deprived 3=most deprived"
	label values cov_imd_categ cov_imd_categ

	///Decriptives
	proportion cov_imd_categ,  level(95)
	tab cov_imd_categ
	proportion dr_varenicline, over (cov_imd_categ) level(95)
	tabulate cov_imd_categ dr_varenicline , row
	
	///Basic logistic regression - is IMD associated with smoking medication prescriptions, adjusted for age and sex.
	logistic dr_varenicline cov_imd cov_age cov_sex

//Generate terms for interactions between index of multiple deprivation (IMD) scores and all covariate terms
	set more off
	foreach i in dr_varenicline history cov_sex cov_age cov_imd cov_gp_visit cov_bmi cov_Rx_year_1 cov_Rx_year_2 cov_Rx_year_3 cov_Rx_year_4 /// 
		cov_Rx_year_5  cov_Rx_year_6 cov_Rx_year_7 cov_Rx_year_8 cov_Rx_year_9 cov_Rx_year_10 cov_charlson_ever cov_alcohol_misuse_ever cov_drug_misuse_ever  ///
		cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever /// 
		cov_rare_med_ever cov_rare_mental_ever {
			gen I_imd_`i' = cov_imd*`i'
			}
	format  first_smoke_date %tc
	compress

///ANALYSIS 1: LOGISTIC REGRESSION MODELS  (eTable 2.11)

	//Logistic regression estimates
	foreach i in 90 180 270 365 730 1460 {
	 	logistic out_quit_`i' dr_varenicline cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 I_imd_dr_varenicline I_imd_cov_sex ///
			I_imd_cov_age I_imd_cov_Rx_year_1-I_imd_cov_Rx_year_10 if cov_bmi!=. & cov_imd!=. , ro cluster (staffid)  

		logistic out_quit_`i' dr_varenicline history cov_imd cov_gp_visit cov_bmi cov_age cov_sex cov_Rx_year_1-cov_Rx_year_10 ///
			cov_charlson_ever cov_alcohol_misuse_ever cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever ///
			cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever cov_hyp_anxioly_ever I_imd_dr_varenicline I_imd_history  ///
			I_imd_cov_sex I_imd_cov_age I_imd_cov_gp_visit I_imd_cov_bmi I_imd_cov_Rx_year_1-I_imd_cov_Rx_year_10 I_imd_cov_charlson_ever  ///
			I_imd_cov_alcohol_misuse_ever I_imd_cov_drug_misuse_ever  I_imd_cov_bipolar_ever I_imd_cov_depression_ever ///
			I_imd_cov_neuroticdis_ever  I_imd_cov_selfharm_ever I_imd_cov_antidepres_ever I_imd_cov_antipsyc_ever I_imd_cov_hyp_anxioly_ever  ///
			I_imd_cov_rare_med_ever I_imd_cov_rare_mental_ever 	if cov_bmi!=. & cov_imd!=., ro cluster (staffid) 

		}
///

		

//ANALYSIS 2: PROPENSITY SCORE MATCHING
	
	//Propensity score matching - with complete data only, to ensure accuracy of interaction with IMD
	set more off
	
	//Ensure observations are randomly sorted before calling PSMATCH2
	set seed 12345
	tempvar sortorder
	gen `sortorder' = runiform()
	sort `sortorder'
	compress 
	
	//How many patients were available for PSM?
	tabulate dr_varenicline out_quit_90 if  cov_bmi!=. & cov_imd!=.
	
	//Create propensity score matched sample 
	foreach j in pscore treated support weight id n1 nn pdif {
		gen `j'=.
		}
		set more off
		psmatch2 dr_varenicline history cov_sex cov_age cov_bmi cov_imd cov_gp_visit cov_Rx_year_1-cov_Rx_year_10 cov_charlson_ever cov_alcohol_misuse_ever  ///
			cov_drug_misuse_ever cov_bipolar_ever cov_depression_ever cov_neuroticdis_ever cov_selfharm_ever cov_antidepres_ever cov_antipsyc_ever   ///
			cov_hyp_anxioly_ever cov_rare_med_ever cov_rare_mental_ever if cov_bmi!=. & cov_imd!=., outcome (out_quit_90) common noreplace logit odds neighbor(1)
		foreach j in pscore treated support weight id n1 nn pdif {
		replace `j'=_`j' if `j'==.
		compress
			}
		//

				
	///Generate interaction terms
	gen I_imd_pscore = pscore*cov_imd
	gen I_imd_treated = treated*cov_imd
		
	//Derive Odds-Ratios (eTable 2.12)
	set more off
	foreach i in 90 180 270 365 730 1460 {
		logistic out_quit_`i' treated pscore cov_imd I_imd_pscore I_imd_treated if cov_bmi!=. & cov_imd!=. & weight==1, ro cluster (staffid)
		}
	//
	
	
///ANALYSIS 3: INSTRUMENTAL VARAIBLE ANALYSIS
set more off
compress
	
	//Generate the instrument
	set seed 12345 
	gen rand=uniform()
	bys staffid (Rx_eventdate rand):gen IV_varenicline=dr_varenicline[_n-1]

	//Generate interaction terms
  	gen I_imd_IV_varenicline = cov_imd*IV_varenicline
	compress

	//IV regressions with adjustment for interaction of IMD score (eTable 2.13)
		//IV regressions
		foreach i in 90 180 270 365 730 1460 {
			ivreg2 out_quit_`i' (dr_varenicline I_imd_dr_varenicline=IV_varenicline I_imd_IV_varenicline) cov_Rx_year_1-cov_Rx_year_10  ///
				I_imd_cov_Rx_year_1-I_imd_cov_Rx_year_10  if cov_bmi!=. & cov_imd!=., ro cluster (staffid) endog(dr_varenicline)
				}
			
	//How many people were missing data
	tab dr_varenicline if cov_imd==. 
	tab dr_varenicline if cov_imd==. & I_imd_IV_varenicline==.
	tab dr_varenicline if cov_imd==. & I_imd_IV_varenicline==. & cov_bmi==.
	
	//How many people were in each treatment group	
	tab dr_varenicline if cov_imd!=. & I_imd_IV_varenicline!=. & cov_bmi!=.

//Save dataset for future use
	compress
	save "5_effectabstinence/data/Q2_effectabstinence_IMD.dta",replace

	clear
	log close
