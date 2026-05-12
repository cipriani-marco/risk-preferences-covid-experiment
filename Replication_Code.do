////////////////////////////////////////////////////////////
/// Risk Preferences at the Time of COVID-19:            ///
/// an Experiment with Professional Traders and Students ///
/// REPLICATION CODE                                     ///
////////////////////////////////////////////////////////////

sysdir set PLUS "C:\Program Files\Stata18\ado\plus\"
cd "SPECIFY YOUR DIRECTORY PATH"
set scheme s1color

////////////////////////////////////////////////////////////////////////////////

use "analysis_data.dta", clear

////////////////////////////////////////////////////////////////////////////////

//////////////////////////
/// ATTRITION ANALYSIS ///
//////////////////////////

ranksum bret if pro==1 & wave==1, by(panel)
ranksum bret if pro==0 & wave==1, by(panel)

////////////////////////////////////////////////////////////////////////////////

/// Keeping only subjects in both waves ///
keep if panel==2

/// Drop students from CGH1 experiment ///
drop if exp=="CGH1"

/// Compute Change in BRET ///
xtset subject wave
gen d_bret = d.bret
e
////////////////////////////////////////////////////////////////////////////////

///////////////
/// TABLE 1 ///
///////////////

/// Traders ///
bys wave: sum bret if pro==1, d
ttest d_bret == 0 if pro==1

preserve
keep if pro==1
gen usvar1 = bret if wave==1
bys subject: egen out1 = max(usvar1) 
gen usvar2 = bret if wave==2
bys subject: egen out2 = max(usvar2)
bys subject: keep if _n==1
signrank out1 = out2
restore

robvar bret if pro==1, by(wave)

/// Students ///
bys wave: sum bret if pro==0, d
ttest d_bret == 0 if pro==0 

preserve
keep if pro==0
gen usvar1 = bret if wave==1
bys subject: egen out1 = max(usvar1) 
gen usvar2 = bret if wave==2
bys subject: egen out2 = max(usvar2)
bys subject: keep if _n==1
signrank out1 = out2
restore

robvar bret if pro==0, by(wave)

/// TESTING DIFFERENCES in BRET BETWEEN GROUPS ///
ttest bret if wave==1, by(pro) unequal
ranksum bret if wave==1, by(pro)
 
ttest bret if wave==2, by(pro) unequal
ranksum bret if wave==2, by(pro)


////////////////
/// FIGURE 1 ///
////////////////
cap program drop cumulplot
	program define cumulplot

	cumul `1' if wave == 1, gen(F_1) equal
	cumul `1' if wave == 2, gen(F_2) equal
	
	label variable F_1 "Pre-COVID"
	label variable F_2 "COVID"
	
	_pctile `1', p(1 60 99)
	
	ksmirnov `1', by(wave)
	scalar a = r(p)
	local pvalue: di %4.2f scalar(a)
	
	twoway (line F_1 `1' if wave == 1, sort lcolor(red) lwidth(medium) lpattern(solid) connect(stairstep)) ///
		(line F_2 `1' if wave == 2, sort lcolor(black) lwidth(medium) lpattern(dash) connect(stairstep)),  ///	
		ylabel(, nogrid) graphregion(fcolor(white)) xtitle("Observed range", size(small))                  ///
		xlabel(minmax,format(%9.1fc)) title("`2'", size(medium)) legend(ring(0) position(5) rows(2))       ///
		note("Kolmogorov-Smirnov test for equality of distribution: p-value= `pvalue'", size(medsmall))    ///
		subtitle("(`3')") name(`5', replace)                   
	
	drop F_*
end

/// Students ///
preserve
keep if pro==0
cumulplot bret    "Risk Preference Over Time" "Students" "_students" "bret_students"
restore

/// Traders ///
preserve
keep if pro==1
cumulplot bret    "Risk Preference Over Time" "Traders" "_traders" "bret_traders"
restore

graph combine bret_traders bret_students, rows(2) ///
	note("K-S Test is the Kolmogorov-Smirnov test for equality of distribution", size(small))

////////////////
/// FIGURE 2 ///
////////////////
xtset subject wave

tab d_bret if pro==1
tab d_bret if pro==0
tab d_bret

histogram d_bret if pro==0, width(10) start(-55) fraction fcolor(ebg) lcolor(black) ///
	xtitle("") title("Students", size(medlarge)) name(g1, replace)                  
histogram d_bret if pro==1, width(10) start(-55) fraction fcolor(ebg) lcolor(black) ///
	xtitle("") title("Traders", size(medlarge)) name(g2, replace)
graph combine g2 g1, title("Histogram of Change in Risk Preference")                ///
	subtitle("(bin width = 10)") ycommon rows(2)
graph drop _all

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////
/// POWER ANALYSIS EXERCISE: TABLE 3 ///
////////////////////////////////////////

/// Traders ///
matrix define A = J(5,1,.)
forval i = 1/5 {
	ttest d_bret == -`i' if pro==1
	matrix A[`i',1] = r(p_u)
}
matrix li A

power pairedmeans, corr(0.50) altdiff(-1 -2 -3 -4 -5) n(50) sd(12) onesided
power pairedmeans, corr(0.35) altdiff(-1 -2 -3 -4 -5) n(50) sd(12) onesided

/// Students ///
matrix define A = J(5,1,.)
	forval i = 1/5 {
	ttest d_bret == -`i' if pro==0
matrix A[`i',1] = r(p_u)
}
matrix li A

power pairedmeans, corr(0.50) altdiff(-1 -2 -3 -4 -5) n(60) sd(15) onesided
power pairedmeans, corr(0.20) altdiff(-1 -2 -3 -4 -5 -6) n(60) sd(15) onesided

////////////////////////////////////////////////////////////////////////////////

////////////////
/// FIGURE 3 ///
////////////////

catplot covid_infected1, percent                                                           ///
	bar(1, fcolor(midblue) lcolor(midblue))                                                ///
	blabel(bar, format(%3.1f) pos(outside) size(medsmall)) ylabel(0(10)75)                 ///
	l1title("") b1title("") legend(off) var1opts(label(labsize(small))) name(g1, replace)  ///
	title("Have you or a member of your household been infected with Coronavirus?", size(medlarge))
catplot covid_infected2, percent                                                           ///
	bar(1, fcolor(midblue) lcolor(midblue))                                                ///
	blabel(bar, format(%3.1f) pos(outside) size(medsmall)) ylabel(0(10)75)                 ///
	l1title("") b1title("") legend(off) var1opts(label(labsize(small))) name(g2, replace)  ///
	title("Has any relative or close friend been infected with Coronavirus?", size(medlarge))
graph combine g1 g2, rows(2) 
graph drop _all

////////////////
/// FIGURE 4 ///
////////////////

catplot fin_impact_current, percent asyvars stack ylabel(0(50)100, gmax) plotregion(margin(zero)) ///
	bar(1, blcolor(green) bfcolor(green)) bar(2, blcolor(yellow) bfcolor(yellow))                 ///
	bar(3, blcolor(orange_red) bfcolor(orange_red)) bar(4, blcolor(cranberry) bfcolor(cranberry)) ///
	bar(5, blcolor(purple) bfcolor(purple)) blabel(bar, format(%3.1f) pos(center) size(small))    ///
	title("How much has your financial situation" "been negatively affected?", size(medium))      ///
	l1title("")  b1title("") legend(off) name(g1, replace)
catplot fin_impact_future, percent asyvars stack ylabel(0(50)100, gmax) plotregion(margin(zero))  ///
	bar(1, blcolor(green) bfcolor(green)) bar(2, blcolor(yellow) bfcolor(yellow))                 ///
	bar(3, blcolor(orange_red) bfcolor(orange_red)) bar(4, blcolor(cranberry) bfcolor(cranberry)) ///
	bar(5, blcolor(purple) bfcolor(purple)) blabel(bar, format(%3.1f) pos(center) size(small))    ///
	title("How much do you think your financial" "situation will be negatively affected?", size(medium)) ///
	l1title("")  b1title("") legend(off) name(g2, replace)
catplot activity_disruption, percent asyvars stack ylabel(0(50)100, gmax) plotregion(margin(zero)) ///
	bar(1, blcolor(green) bfcolor(green)) bar(2, blcolor(yellow) bfcolor(yellow))                  ///
	bar(3, blcolor(orange_red) bfcolor(orange_red)) bar(4, blcolor(cranberry) bfcolor(cranberry))  ///
	bar(5, blcolor(purple) bfcolor(purple)) blabel(bar, format(%3.1f) pos(center) size(small))     ///
	title("How much changes in daily activites" "have impacted quality of life?", size(medium))    ///
	l1title("")  b1title("") legend(off) name(g3, replace)
catplot pandemic_worry, percent asyvars stack ylabel(0(50)100, gmax) plotregion(margin(zero))      ///
	bar(1, blcolor(green) bfcolor(green)) bar(2, blcolor(yellow) bfcolor(yellow))                  ///
	bar(3, blcolor(orange_red) bfcolor(orange_red)) bar(4, blcolor(cranberry) bfcolor(cranberry))  ///
	bar(5, blcolor(purple) bfcolor(purple)) blabel(bar, format(%3.1f) pos(center) size(small))     ///
	title("How worried are you about" "the Coronavirus pandemic?", size(medium))                   ///
	l1title("")  b1title("") legend(rows(2) size(small)) name(g4, replace)
grc1leg g1 g2 g3 g4, rows(2) legendfrom(g4) 
graph drop _all

////////////////////////////////////////////////////////////////////////////////

/// Differences between Traders and Students ///
/// Footnotes 16, 17, and 18                 /// 

gen dummy = (covid_infected1==1) if !missing(covid_infected1)
ttest dummy if wave==2, by(pro) unequal
drop dummy 

gen dummy = (covid_infected2==1) if !missing(covid_infected2)
ttest dummy if wave==2, by(pro) unequal
drop dummy 

ttest fin_impact_current2_d if wave==2, by(pro) unequal
ttest fin_impact_future2_d if wave==2, by(pro) unequal

ttest activity_disruption2_d if wave==2, by(pro) unequal
ttest pandemic_worry2_d if wave==2, by(pro) unequal

////////////////////////////////////////////////////////////////////////////////

/// INFECTION INDICATOR: ONLY INFECTION OF CLOSE CIRCLE ///
gen infected = (covid_infected22_d==2)

/// IMPACT INDEX BASED ON IMPACTS ON CURRENT FINANCIAL SITUATION AND DAILY LIFE: BELOW/ABOVE MEDIAN /// 
gen impact_score = fin_impact_current2 + activity_disruption2
label var impact_score "Alt Composite Impact Index" 
xtile impact_q = impact_score, n(2)

////////////////////////////////////////////////////////////////////////////////

////////////////
/// FIGURE 5 ///
////////////////

preserve

keep if pro==1 

*** BRET ***
reg d_bret ibn.impact_q, noconstant

clear
set obs 2
gen impact_q = 1 if _n==1
replace impact_q = 2 if _n==2

gen coeff = .
gen coeff_lo = .
gen coeff_up = .
replace coeff = _b[1.impact_q] if impact_q==1
replace coeff_lo = _b[1.impact_q] - invt(e(df_r),0.975)*_se[1.impact_q] if impact_q==1
replace coeff_up = _b[1.impact_q] + invt(e(df_r),0.975)*_se[1.impact_q] if impact_q==1
replace coeff = _b[2.impact_q] if impact_q==2
replace coeff_lo = _b[2.impact_q] - invt(e(df_r),0.975)*_se[2.impact_q] if impact_q==2
replace coeff_up = _b[2.impact_q] + invt(e(df_r),0.975)*_se[2.impact_q] if impact_q==2

graph twoway (bar coeff impact_q if impact_q==1, fcolor(dkgreen) lcolor(dkgreen) lwidth(medium)) ///
	(bar coeff impact_q if impact_q==2, fcolor(cranberry) lcolor(cranberry) lwidth(medium))      ///
	(rcap coeff_up coeff_lo impact_q, lcolor(black) lpattern(dash)),                             ///
	title("by Pandemic Impact Index", size(medium)) ytitle("") ylabel(#3) xtitle("") xlabel(none) name(g1, replace)  ///
	legend(order(1 "Low Impact (below median)" 2 "High Impact (above median)") rows(2) size(small))
restore

////////////////////////////////////////////////////////////////////////////////

preserve

keep if pro==1

*** BRET ***
reg d_bret ibn.infected, noconstant

clear
set obs 2
gen c_no = _n
gen infected = 0 if _n==1
replace infected = 1 if _n==2

gen coeff = .
gen coeff_lo = .
gen coeff_up = .
replace coeff = _b[0.infected] if infected==0
replace coeff_lo = _b[0.infected] - invt(e(df_r),0.975)*_se[0.infected] if infected==0
replace coeff_up = _b[0.infected] + invt(e(df_r),0.975)*_se[0.infected] if infected==0
replace coeff = _b[1.infected] if infected==1
replace coeff_lo = _b[1.infected] - invt(e(df_r),0.975)*_se[1.infected] if infected==1
replace coeff_up = _b[1.infected] + invt(e(df_r),0.975)*_se[1.infected] if infected==1

graph twoway (bar coeff infected if infected==0, fcolor(dkgreen) lcolor(dkgreen) lwidth(medium)) ///
	(bar coeff infected if infected==1, fcolor(cranberry) lcolor(cranberry) lwidth(medium))      ///
	(rcap coeff_up coeff_lo infected, lcolor(black) lpattern(dash)),                             ///
	title("by Experience of Infection", size(medium)) ytitle("") ylabel(#3) xtitle("") xlabel(none) name(g2, replace)  ///
	legend(order(1 "No Infection" 2 "Infection") rows(2) size(small))
restore

graph combine g1 g2

////////////////////////////////////////////////////////////////////////////////

////////////////
/// FIGURE 6 ///
////////////////

preserve

keep if pro==0 

*** BRET ***
reg d_bret ibn.impact_q, noconstant

clear
set obs 2
gen impact_q = 1 if _n==1
replace impact_q = 2 if _n==2

gen coeff = .
gen coeff_lo = .
gen coeff_up = .
replace coeff = _b[1.impact_q] if impact_q==1
replace coeff_lo = _b[1.impact_q] - invt(e(df_r),0.975)*_se[1.impact_q] if impact_q==1
replace coeff_up = _b[1.impact_q] + invt(e(df_r),0.975)*_se[1.impact_q] if impact_q==1
replace coeff = _b[2.impact_q] if impact_q==2
replace coeff_lo = _b[2.impact_q] - invt(e(df_r),0.975)*_se[2.impact_q] if impact_q==2
replace coeff_up = _b[2.impact_q] + invt(e(df_r),0.975)*_se[2.impact_q] if impact_q==2

graph twoway (bar coeff impact_q if impact_q==1, fcolor(dkgreen) lcolor(dkgreen) lwidth(medium)) ///
	(bar coeff impact_q if impact_q==2, fcolor(cranberry) lcolor(cranberry) lwidth(medium))      ///
	(rcap coeff_up coeff_lo impact_q, lcolor(black) lpattern(dash)),                             ///
	title("by Pandemic Impact Index", size(medium)) ytitle("") ylabel(#3) xtitle("") xlabel(none) name(g1, replace)  ///
	legend(order(1 "Low Impact (below median)" 2 "High Impact (above median)") rows(2) size(small))
restore

////////////////////////////////////////////////////////////////////////////////

preserve

keep if pro==0

*** BRET ***
reg d_bret ibn.infected, noconstant

clear
set obs 2
gen c_no = _n
gen infected = 0 if _n==1
replace infected = 1 if _n==2

gen coeff = .
gen coeff_lo = .
gen coeff_up = .
replace coeff = _b[0.infected] if infected==0
replace coeff_lo = _b[0.infected] - invt(e(df_r),0.975)*_se[0.infected] if infected==0
replace coeff_up = _b[0.infected] + invt(e(df_r),0.975)*_se[0.infected] if infected==0
replace coeff = _b[1.infected] if infected==1
replace coeff_lo = _b[1.infected] - invt(e(df_r),0.975)*_se[1.infected] if infected==1
replace coeff_up = _b[1.infected] + invt(e(df_r),0.975)*_se[1.infected] if infected==1

graph twoway (bar coeff infected if infected==0, fcolor(dkgreen) lcolor(dkgreen) lwidth(medium)) ///
	(bar coeff infected if infected==1, fcolor(cranberry) lcolor(cranberry) lwidth(medium))      ///
	(rcap coeff_up coeff_lo infected, lcolor(black) lpattern(dash)),                             ///
	title("by Experience of Infection", size(medium)) ytitle("") ylabel(#3) xtitle("") xlabel(none) name(g2, replace)  ///
	legend(order(1 "No Infection" 2 "Infection") rows(2) size(small))
restore

graph combine g1 g2

////////////////////////////////////////////////////////////////////////////////

////////////////////////////
/// ADDITIONAL MATERIALS ///
////////////////////////////

use "analysis_data.dta", clear

/// Keeping only subjects in both waves ///
keep if panel==2

/// Drop students from CGH1 experiment ///
drop if exp=="CGH1"

/// Compute Change in BRET ///
xtset subject wave
gen d_bret = d.bret

////////////////////////////////////////////////////////////////////////////////

///////////////////
/// Footnote 12 ///
///////////////////

/// Comparison between BRET in wave 2 and predicted BRET in wave 2 ///
preserve
reg bret age if wave==1 & pro==0, r
gen age_old = age
replace age = f.age if e(sample)
predict usvar if e(sample)
bys subject: egen bret_hat = max(usvar) 
sum bret bret_hat if wave==2 & !mi(bret_hat) & pro==0
gen d_bret_hat = bret_hat - bret if wave==2 & !mi(bret_hat) & pro==0
ttest d_bret_hat == 0 if wave==2 & !mi(bret_hat) & pro==0
restore

reg bret age i.wave if pro==0, cluster(subject)

////////////////////////////////////////////////////////////////////////////////

///////////////////////////
/// APPENDIX FIGURE A.1 ///
///////////////////////////

local var = "bret"
qui: reg `var' age if wave==1 & pro==1, r
scalar a = _b[age]
local beta: di %4.3f scalar(a)
scalar b = _se[age]
local se: di %4.3f scalar(b)
twoway (scatter `var' age, sort mcolor(gs9) msymbol(circle))         ///
	(lfit `var' age) if wave==1 & pro==1, ytitle("BRET") legend(off) ///
	title("Traders") note("Slope (s.e.) = `beta' (`se')") name(g1, replace)
qui: reg `var' age if wave==1 & pro==0, r
scalar a = _b[age]
local beta: di %4.3f scalar(a)
scalar b = _se[age]
local se: di %4.3f scalar(b)
twoway (scatter `var' age, sort mcolor(gs9) msymbol(circle))         ///
	(lfit `var' age) if wave==1 & pro==0, ytitle("BRET") legend(off) ///
	title("Students") note("Slope (s.e.) = `beta' (`se')") name(g2, replace)
qui: reg `var' age if wave==1, r
scalar a = _b[age]
local beta: di %4.3f scalar(a)
scalar b = _se[age]
local se: di %4.3f scalar(b)
twoway (scatter `var' age, sort mcolor(gs9) msymbol(circle)) ///
	(lfit `var' age) if wave==1, ytitle("BRET") legend(off)  ///
	title("All") note("Slope (s.e.) = `beta' (`se')") name(g3, replace) fxsize(80) graphregion(margin(l 35 r 35))
graph combine g1 g2, ycommon
graph drop _all

////////////////////////////////////////////////////////////////////////////////

///////////////////////////
/// APPENDIX FIGURE A.2 ///
///////////////////////////

preserve
keep if pro==1

capture drop impact_score
gen impact_score = fin_impact_current2 + activity_disruption2

reg d_bret impact_score
scalar a1 = _b[impact_score]
local b1: di %4.2f scalar(a1)
scalar num = sqrt(e(V)[1,1])
scalar a2 = num
local b2: di %4.2f scalar(a2)

/// SCATTER PLOT AND FITTED LINE ///
twoway (scatter d_bret impact_score, mcolor(purple%50) msymbol(circle)) ///
	(lfit d_bret impact_score, lcolor(black) lwidth(medthick)),         ///
	xtitle("Impact Index") title("Traders") legend(off)                 ///
	note("estimated coefficient (standard error) = `b1' (`b2')")
restore

preserve
keep if pro==0

capture drop impact_score
gen impact_score = fin_impact_current2 + activity_disruption2

reg d_bret impact_score
scalar a1 = _b[impact_score]
local b1: di %4.2f scalar(a1)
scalar num = sqrt(e(V)[1,1])
scalar a2 = num
local b2: di %4.2f scalar(a2)

/// SCATTER PLOT AND FITTED LINE ///
twoway (scatter d_bret impact_score, mcolor(purple%50) msymbol(circle)) ///
	(lfit d_bret impact_score, lcolor(black) lwidth(medthick)),         ///
	xtitle("Impact Index") title("Students") legend(off)                ///
	note("estimated coefficient (standard error) = `b1' (`b2')")
restore

////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////
/// APPENDIX FIGURE A.3 and A.4 ///
///////////////////////////////////

use "analysis_data.dta", clear

////////////////////////////////////////////////////////////////////////////////

/// Keeping only subjects in both waves ///
keep if panel==2

/// Drop students from CGH1 experiment ///
drop if exp=="CGH1"

/// Compute Change in BRET ///
xtset subject wave
gen d_bret = d.bret

/// INFECTION INDICATOR: OWN AND CLOSE CIRCLE INFECTION ///
gen infected = (covid_infected12_d==2 | covid_infected22_d==2)

/// IMPACT INDEX BASED ON ALL AVAILABLE MEASURES: BELOW/ABOVE MEDIAN (excluding infection) ///
gen impact_score = fin_impact_current2 + fin_impact_future2 + activity_disruption2 + pandemic_worry2
label var impact_score "Composite Impact Index" 
xtile impact_q = impact_score, n(2)

preserve

keep if pro==1

*** BRET ***
reg d_bret ibn.impact_q, noconstant

clear
set obs 2
gen impact_q = 1 if _n==1
replace impact_q = 2 if _n==2

gen coeff = .
gen coeff_lo = .
gen coeff_up = .
replace coeff = _b[1.impact_q] if impact_q==1
replace coeff_lo = _b[1.impact_q] - invt(e(df_r),0.975)*_se[1.impact_q] if impact_q==1
replace coeff_up = _b[1.impact_q] + invt(e(df_r),0.975)*_se[1.impact_q] if impact_q==1
replace coeff = _b[2.impact_q] if impact_q==2
replace coeff_lo = _b[2.impact_q] - invt(e(df_r),0.975)*_se[2.impact_q] if impact_q==2
replace coeff_up = _b[2.impact_q] + invt(e(df_r),0.975)*_se[2.impact_q] if impact_q==2

graph twoway (bar coeff impact_q if impact_q==1, fcolor(dkgreen) lcolor(dkgreen) lwidth(medium)) ///
	(bar coeff impact_q if impact_q==2, fcolor(cranberry) lcolor(cranberry) lwidth(medium))      ///
	(rcap coeff_up coeff_lo impact_q, lcolor(black) lpattern(dash)),                             ///
	title("by Pandemic Impact Index", size(medium)) ytitle("") ylabel(#3) xtitle("") xlabel(none) name(g1, replace)  ///
	legend(order(1 "Low Impact (below median)" 2 "High Impact (above median)") rows(2) size(small))
restore

preserve 

keep if pro==1

*** BRET ***
reg d_bret ibn.infected, noconstant

clear
set obs 2
gen c_no = _n
gen infected = 0 if _n==1
replace infected = 1 if _n==2

gen coeff = .
gen coeff_lo = .
gen coeff_up = .
replace coeff = _b[0.infected] if infected==0
replace coeff_lo = _b[0.infected] - invt(e(df_r),0.975)*_se[0.infected] if infected==0
replace coeff_up = _b[0.infected] + invt(e(df_r),0.975)*_se[0.infected] if infected==0
replace coeff = _b[1.infected] if infected==1
replace coeff_lo = _b[1.infected] - invt(e(df_r),0.975)*_se[1.infected] if infected==1
replace coeff_up = _b[1.infected] + invt(e(df_r),0.975)*_se[1.infected] if infected==1

graph twoway (bar coeff infected if infected==0, fcolor(dkgreen) lcolor(dkgreen) lwidth(medium)) ///
	(bar coeff infected if infected==1, fcolor(cranberry) lcolor(cranberry) lwidth(medium))      ///
	(rcap coeff_up coeff_lo infected, lcolor(black) lpattern(dash)),                             ///
	title("by Experience of Infection", size(medium)) ytitle("") ylabel(#3) xtitle("") xlabel(none) name(g2, replace)  ///
	legend(order(1 "No Infection" 2 "Infection") rows(2) size(small))
restore

graph combine g1 g2

////////////////////////////////////////////////////////////////////////////////

preserve

keep if pro==0

*** BRET ***
reg d_bret ibn.impact_q, noconstant

clear
set obs 2
gen impact_q = 1 if _n==1
replace impact_q = 2 if _n==2

gen coeff = .
gen coeff_lo = .
gen coeff_up = .
replace coeff = _b[1.impact_q] if impact_q==1
replace coeff_lo = _b[1.impact_q] - invt(e(df_r),0.975)*_se[1.impact_q] if impact_q==1
replace coeff_up = _b[1.impact_q] + invt(e(df_r),0.975)*_se[1.impact_q] if impact_q==1
replace coeff = _b[2.impact_q] if impact_q==2
replace coeff_lo = _b[2.impact_q] - invt(e(df_r),0.975)*_se[2.impact_q] if impact_q==2
replace coeff_up = _b[2.impact_q] + invt(e(df_r),0.975)*_se[2.impact_q] if impact_q==2

graph twoway (bar coeff impact_q if impact_q==1, fcolor(dkgreen) lcolor(dkgreen) lwidth(medium)) ///
	(bar coeff impact_q if impact_q==2, fcolor(cranberry) lcolor(cranberry) lwidth(medium))      ///
	(rcap coeff_up coeff_lo impact_q, lcolor(black) lpattern(dash)),                             ///
	title("by Pandemic Impact Index", size(medium)) ytitle("") ylabel(#3) xtitle("") xlabel(none) name(g1, replace)  ///
	legend(order(1 "Low Impact (below median)" 2 "High Impact (above median)") rows(2) size(small))
restore

preserve 

keep if pro==0

*** BRET ***
reg d_bret ibn.infected, noconstant

clear
set obs 2
gen c_no = _n
gen infected = 0 if _n==1
replace infected = 1 if _n==2

gen coeff = .
gen coeff_lo = .
gen coeff_up = .
replace coeff = _b[0.infected] if infected==0
replace coeff_lo = _b[0.infected] - invt(e(df_r),0.975)*_se[0.infected] if infected==0
replace coeff_up = _b[0.infected] + invt(e(df_r),0.975)*_se[0.infected] if infected==0
replace coeff = _b[1.infected] if infected==1
replace coeff_lo = _b[1.infected] - invt(e(df_r),0.975)*_se[1.infected] if infected==1
replace coeff_up = _b[1.infected] + invt(e(df_r),0.975)*_se[1.infected] if infected==1

graph twoway (bar coeff infected if infected==0, fcolor(dkgreen) lcolor(dkgreen) lwidth(medium)) ///
	(bar coeff infected if infected==1, fcolor(cranberry) lcolor(cranberry) lwidth(medium))      ///
	(rcap coeff_up coeff_lo infected, lcolor(black) lpattern(dash)),                             ///
	title("by Experience of Infection", size(medium)) ytitle("") ylabel(#3) xtitle("") xlabel(none) name(g2, replace)  ///
	legend(order(1 "No Infection" 2 "Infection") rows(2) size(small))
restore

graph combine g1 g2

////////////////////////////////////////////////////////////////////////////////