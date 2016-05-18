//Neil Davies 11/02/16
//This produces the bias plots

set more off
use "Z:\5_effectabstinence\data\effectabstinence.dta", clear
set seed 12345 
gen rand=uniform()
bys staffid (Rx_eventdate rand):gen IV_varenicline=dr_varenicline[_n-1]

cap prog drop hetero_test
prog def hetero_test

args outcome exposure iv 

macro shift 3
local covar="`*'"

cap drop _const
cap gen _const  = 1

di "outcome=`outcome'"
di "exposure=`exposure'"
di "instrumen=`iv'"
di "covariates=`covar'"

gmm (`outcome' - {xb1:`exposure' `covar' _const})  ///
	(`outcome' - {xb2:`exposure' `covar' _const}), ///
	instruments(1:`exposure' `covar') ///
	instruments(2:`iv' `covar') ///
	winit(unadjusted,independent) onestep  ///
	vce(cluster staffid) ///
	deriv(1/xb1 = -1) ///
	deriv(2/xb2 = -1)
drop _const

local outcome2=substr("`outcome'",1,16)
est sto results_`outcome2'

lincom _b[xb1:`exposure']-_b[xb2:`exposure']
local het_p=2*(1-normal(abs(r(estimate)/r(se))))

regsave `exposure' using "Z:\5_effectabstinence\images\bias_plots_basic_adjusted_`outcome'", detail(all) pval ci replace addvar(het_p,`het_p')

end

//Running each of the tests:

hetero_test cov_sex dr_varenicline IV_varenicline cov_Rx_year_1 cov_Rx_year_2 cov_Rx_year_3 cov_Rx_year_4 cov_Rx_year_5 cov_Rx_year_6 ///
	cov_Rx_year_7 cov_Rx_year_8 cov_Rx_year_9
hetero_test cov_age dr_varenicline IV_varenicline cov_Rx_year_1 cov_Rx_year_2 cov_Rx_year_3 cov_Rx_year_4 cov_Rx_year_5 cov_Rx_year_6 ///
	cov_Rx_year_7 cov_Rx_year_8 cov_Rx_year_9 cov_sex

ds cov_bmi cov_gp_visit cov_charlson_ever cov_alcohol_misuse_ever cov_bipolar_ever cov_depression_ever  ///
 cov_neuroticdis_ever cov_drug_misuse_ever  cov_selfharm_ever cov_rare_mental_ever cov_antidepres_ever cov_antipsyc_ever  ///
 cov_hyp_anxioly_ever cov_rare_med_ever cov_imd history
	
foreach i in `r(varlist)'{
	hetero_test `i' dr_varenicline IV_varenicline cov_Rx_year_1 cov_Rx_year_2 cov_Rx_year_3 cov_Rx_year_4 cov_Rx_year_5 cov_Rx_year_6 ///
	cov_Rx_year_7 cov_Rx_year_8 cov_Rx_year_9 cov_sex cov_age
	}

	
//Plot the regression results for gender and year of first prescription:
//Label the variables for inclusion in the figures

label variable cov_sex "Female sex"
label variable cov_bmi "BMI (kg/m2)"
label variable cov_age "Age (years)"
label variable cov_gp_visit "Number of GP visits in prior year"
label variable cov_charlson_ever "Chronic disease (Charlson)"
label variable cov_alcohol_misuse_ever "Misuses alcohol"
label variable cov_bipolar_ever "Bipolar"
label variable cov_depression_ever "Depression"
label variable cov_neuroticdis_ever "Neurotic disorder"
label variable cov_drug_misuse_ever  "Misuses drugs"
label variable cov_selfharm_ever "Self-harm"
label variable cov_rare_mental_ever "Rare mental disorder"
label variable cov_antidepres_ever "Anti-depressants"
label variable cov_antipsyc_ever "Anti-psychotics"
label variable cov_hyp_anxioly_ever "Anxiolytics/Hypnotics"
label variable cov_rare_med_ever "Rare psychotropic medication"
label variable cov_imd "Index of Multiple Deprivation"
label variable cov_Rx_year "Year of first prescription"
label variable history "Days in history"

foreach i in cov_sex cov_charlson_ever cov_alcohol_misuse_ever cov_bipolar_ever cov_depression_ever  ///
 cov_neuroticdis_ever cov_drug_misuse_ever  cov_selfharm_ever cov_rare_mental_ever cov_antidepres_ever cov_antipsyc_ever  ///
 cov_hyp_anxioly_ever cov_rare_med_ever  {
	
	local var="`i'"
	local name_var: variable label `var'  /* <- save variable label in local `lab' */
	
	di "`name_var'"

	local outcome2=substr("`i'",1,16)
	di "`outcome2'"
	local n=`n'+1
	local reg=`"(results_`outcome2' ,keep(xb1:dr_varenicline) ms(S) mc(gs5) ciopts(lc(gs5)) offset(0.05) rename(xb1:dr_varenicline="`name_var'"))"'
	local ivreg=`"(results_`outcome2',keep(xb2:dr_varenicline) ms(T) mc(gs10) ciopts(lc(gs10)) offset(-0.05) rename(xb2:dr_varenicline="`name_var'"))"'
	
	local combined=`"`combined'"'+" "+`"`reg'"'+" "+`"`ivreg'"'
	di `"`combined'"'
	
	}
di `"`combined'"'
coefplot `"`combined'"' , legend(off) xline(0) byopts(yrescale)  xtitle("Difference in absolute risk of outcome") graphregion(color(white)) 
graph save "Z:\5_effectabstinence\images\graph_2_bias_plot_binary",replace 
graph use  "Z:\5_effectabstinence\images\graph_2_bias_plot_binary"
graph export "Z:\5_effectabstinence\images\figure_1a_ps_dist_before.eps", as(eps) replace
local combined=""
foreach i in cov_bmi cov_age cov_gp_visit cov_imd /*cov_Rx_year */history {
	
	local var="`i'"
	local name_var: variable label `var'  /* <- save variable label in local `lab' */
	
	di "`name_var'"

	local outcome2=substr("`i'",1,16)
	di "`outcome2'"
	local n=`n'+1
	local reg=`"(results_`outcome2' ,keep(xb1:dr_varenicline) ms(S) mc(gs5) ciopts(lc(gs5)) offset(0.05) rename(xb1:dr_varenicline="`name_var'"))"'
	local ivreg=`"(results_`outcome2',keep(xb2:dr_varenicline) ms(T) mc(gs10) ciopts(lc(gs10)) offset(-0.05) rename(xb2:dr_varenicline="`name_var'"))"'
	
	local combined=`"`combined'"'+" "+`"`reg'"'+" "+`"`ivreg'"'
	di `"`combined'"'
	}

coefplot `combined' , legend(off) xline(0) byopts(yrescale)  xtitle("Mean differences in outcome") graphregion(color(white)) 
graph save "Z:\5_effectabstinence\images\graph_2_bias_plot_continious",replace 
graph use "Z:\5_effectabstinence\images\graph_2_bias_plot_continious"
graph export "Z:\5_effectabstinence\images\figure_1b_ps_dist_before.eps", as(eps) replace
 
 
//Cleaning and saving the bias plot tables 
//binary outcomes:

cap prog drop bias_results_bin
prog def bias_results_bin
use "Z:\5_effectabstinence\images\bias_plots_basic_adjusted_`1'",  clear
gen results_ols=string(coef[1]*100, "%9.2f")+" ("+string(ci_lower[1]*100, "%9.2f")+" to "+string(ci_upper[1]*100, "%9.2f") + ")"
gen results_iv=string(coef[2]*100, "%9.2f")+" ("+string(ci_lower[2]*100, "%9.2f")+" to "+string(ci_upper[2]*100, "%9.2f") + ")"
gen results_phet=coef[3]
gen variable="`1'"
keep variable N results*
keep in 1
save "Z:\5_effectabstinence\temp_data\cleaned_`1'_bin",replace
end

foreach i in cov_sex cov_charlson_ever cov_alcohol_misuse_ever cov_bipolar_ever cov_depression_ever  ///
 cov_neuroticdis_ever cov_drug_misuse_ever  cov_selfharm_ever cov_rare_mental_ever cov_antidepres_ever cov_antipsyc_ever  ///
 cov_hyp_anxioly_ever cov_rare_med_ever  {
	bias_results_bin `i'
	}
 
use  "Z:\5_effectabstinence\temp_data\cleaned_cov_sex",clear
foreach i in cov_sex cov_charlson_ever cov_alcohol_misuse_ever cov_bipolar_ever cov_depression_ever  ///
 cov_neuroticdis_ever cov_drug_misuse_ever  cov_selfharm_ever cov_rare_mental_ever cov_antidepres_ever cov_antipsyc_ever  ///
 cov_hyp_anxioly_ever cov_rare_med_ever  {
	append using "Z:\5_effectabstinence\temp_data\cleaned_`i'_bin",
	}
 order variable
 
 
 
 
 ////continous outcomes:

cap prog drop bias_results_cont
prog def bias_results_cont
use "Z:\5_effectabstinence\images\bias_plots_basic_adjusted_`1'",  clear
gen results_ols=string(coef[1]*100, "%9.2f")+" ("+string(ci_lower[1]*100, "%9.2f")+" to "+string(ci_upper[1]*100, "%9.2f") + ")"
gen results_iv=string(coef[2]*100, "%9.2f")+" ("+string(ci_lower[2]*100, "%9.2f")+" to "+string(ci_upper[2]*100, "%9.2f") + ")"
gen results_phet=coef[3]
gen variable="`1'"
keep variable N results*
keep in 1
save "Z:\5_effectabstinence\temp_data\cleaned_`1'_cont",replace
end

foreach i in cov_bmi cov_age cov_gp_visit cov_imd /*cov_Rx_year*/ history {
	bias_results_cont `i'
	}
 
use  "Z:\5_effectabstinence\temp_data\cleaned_cov_bmi",clear
foreach i in cov_age cov_gp_visit cov_imd /*cov_Rx_year */ history {
	append using "Z:\5_effectabstinence\temp_data\cleaned_`i'_cont",
	}
 order variable

