//Gemma Taylor 
//this meta-analyses the effect estimates from trials of varenicline versus NRT, and our IV results.
ssc install metan, update
clear
import excel "Z:\5_effectabstinence\Paper\meta-analysisdiscussion meta-analysis.xlsx", sheet("meta-analysis data") cellrange(A1:H5) firstrow
set more off

//label data
label define author_year 1 "Cahill et al. 2013" 2 "Baker et al. 2016" 3 "Taylor et al. 2016" 4 "Anthenelli et al. 2016"

label values studyid author_year
label define study_type 1 "Randomised controlled trial" 2 "Instrumental variable analysis"
label values study_type study_type

//subgroup
metan risk_diff lower_ci upper_ci, random by( study_type) label (namevar= author_year) texts(140) effect ("Risk difference")  graphregion (color(white)) xtitle("Varenicline vs. NRT")  nowarning  nosubgroup
graph save Graph "Z:\5_effectabstinence\images\Fig3_metan.gph", replace
graph export "Z:\5_effectabstinence\images\Fig3_metan.eps", as(eps) preview(on) replace





//**change aspect ratio to .5 on graph edit.//

