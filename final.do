///Econometrics Final Exam 
//First set the working directory which should contain the three data files
cd "/Users/ibethrivero/Desktop/final_eco_B" //Your path goes here
//Create log file
log using final.smcl
// I'll replicate the summary of statistics 
use seedanalysis_011204_080404.dta, clear 
// Generate a control group variable
gen control = 1 if group == "C" 
replace control = 0 if control ==.
//Generate dormant clients variable
gen dormant_new = 1- active
drop dormant
rename dormant_new dormant
//Divide total balances by 100
replace totbal = totbal/100
replace newtotbal = newtotbal/100
//Generate Barangay’s distance to branch variable
gen dist_GB = dbutuan if butuan ==1
replace dist_GB = dampayon if ampayon == 1
//Generate Population of barangay variable
destring pop, ignore(",") replace
// Generate Bank’s penetration in barangay variable
gen brgy_penetration = no_clients /pop
// Generate Standard deviation of balances in barangay (hundreds) variable
bysort brgy: egen sd_totbal = sd(totbal)
// Generate Mean savings balance in barangay (hundreds) variable
bysort brgy: egen mean_totbal = mean(totbal)

//Relabel variables
label variable totbal "Client savings balance (hundreds)"
label variable active "Active account"
label variable dist_GB "Barangay’s distance to branch"
label variable brgy_penetration "Bank’s penetration in barangay"
label variable sd_totbal "Standard deviation of balances in barangay (hundreds)"
label variable mean_totbal "Mean savings balance in barangay (hundreds)"
label variable pop "Population of barangay (thousands)"
label variable yearsed "Education"
label variable female "Female"
label variable age "Age"
label variable impatient_mon01 "Impatient (now versus one month)"
label variable  hyper_mon_new2 "Hyperbolic"

//Generate the tables and save it in the .tex format

estpost tabstat totbal active dist_GB brgy_penetration sd_totbal mean_totbal pop  if call==1, by(group)  stats(mean sd) columns(stats)  listwise
esttab using "summary1.tex",  main(mean) aux(sd) unstack noobs nonote nomtitle nonumber label 
estpost tabstat yearsed female age impatient_mon01 hyper_mon_new2 if call==1, by(group) stats(mean sd) columns(stats) listwise
esttab using "summary2.tex",  main(mean) aux(sd) unstack noobs nonote nomtitle nonumber label 

//I'll replicate table 6 of the paper
// Second column of the table
//OLS Regression, Six Months After treatment 
//Import data file 
use seedanalysis_011204.dta, clear
// Relabel variables
label variable balchange "Change in total savings"
//Regress Changes in Balance on treatment and marketing
reg balchange treatment marketing, robust
//Store Results 
eststo
//Third column of the table
//Same regression but for Commitment and marketing groups only
reg balchange treatment if (treatment == 1 | marketing == 1), robust
//Store Results 
eststo
//Generate table 
esttab using olssixmonths.tex, se ar2 label 
eststo clear
// Fourth column of the table
//OLS Regression, Twelve Months After treatment 
use seedanalysis_011204_080404.dta, clear
//Relabel variables
label variable balchange "Change in total savings"
//Regress Changes in Balance on treatment and marketing
reg balchange treatment marketing, robust
//Store Results 
eststo
//Fifth column of the table 
// Same regression but for Commitment and marketing groups only
reg balchange treatment if (treatment == 1 | marketing == 1), robust
//Store Results 
eststo
//Generate table 
esttab using olstwelvemonths.tex, se ar2 label 
eststo clear
//Sixth column of the table 
use seedanalysis_011204_080404.dta, clear
//Relabel variables 
label variable frac_change_00 "Change in balance over 0 pct"
label variable frac_change_20 "Change in balance over 20 pct"
//All probit regressions are Twelve Months After treatment
//Authors only report marginal effects
// Probit regression, Change in balance > 0% on treatment and marketing 
probit frac_change_00 treatment marketing, robust
estpost margins, dydx(*)
//Store Results 
eststo
//Seventh column of the table
//  Same regression but for Commitment and marketing groups only
probit frac_change_00 treatment if (treatment == 1 | marketing == 1), robust
estpost margins, dydx(*)
//Store Results 
eststo
//Generate table 
esttab using probitzero.tex, se ar2 label 
eststo clear
//Eighth column of the table
// Probit regression, Change in balance > 20% on treatment and marketing 
probit frac_change_20 treatment marketing, robust
estpost margins, dydx(*)
//Store Results 
eststo
//Ninth column of the table
//  Same regression but for Commitment and marketing groups only
probit frac_change_20 treatment if (treatment == 1 | marketing == 1), robust
estpost margins, dydx(*)
//Store Results 
eststo
//Generate table 
esttab using probittwenty.tex, se ar2 label 
eststo clear
//Local Average Treatment Effect 
//IV Regression, Six Months After treatment 
use seedanalysis_011204.dta, clear
//relabel variables 
label variable balchange "Change in total savings"
//Regress Changes in Balance on take up variable and marketing, using assignment to  
//group as instrument
ivreg balchange marketing (seedtakeup = treatment), robust 
//Store Results 
eststo
//  Same regression but for Commitment and marketing groups only
ivreg  balchange (seedtakeup = treatment) if (treatment == 1 | marketing == 1), robust
//Store Results 
eststo
//Generate table 
esttab using latesix.tex, se ar2 label 
eststo clear
//IV Regression, Twelve Months After treatment 
use seedanalysis_011204_080404.dta, clear
//relabel variables 
label variable balchange "Change in total savings"
//Regress Changes in Balance on take up variable and marketing, using assignment to  
//group as instrument
ivreg balchange marketing (seedtakeup = treatment), robust 
//Store Results 
eststo
//  Same regression but for Commitment and marketing groups only
ivreg  balchange (seedtakeup = treatment) if (treatment == 1 | marketing == 1), robust
//Store Results 
eststo
//Generate table 
esttab using latetwelve.tex, se ar2 label 
eststo clear
//Now I'll replicate table 8 of the paper (accounts for heterogeneity)
//OLS Regression, twelve Months After treatment 
use seedanalysis_011204_080404.dta, clear
//Relabel variables
label variable balchange "Change in balance"
label variable treatment "Commitment-treatment"
label variable marketing "Marketing-treatment"
label variable female "Female"
label variable female_treat "Female (commitment)"
label variable active "Active"
label variable active_treat "Active (commitment)"
label variable edhi "College"
label variable edhi_treat "College (commitment)"
label variable hi_hh_inc "High Income"
label variable hi_hh_inc_treat "High Income (commitment)"
label variable hyper_mon_new2 "Hyperbolic"
label variable hyper_mon_new2_treat "Hyperbolic (commitment)"
label variable silly_mon_new2 "Inconsistent"
label variable silly_mon_new2_treat "Inconsistent (commitment)"
// Regress Changes in Balance on treatment , marketing, female and 
//interaction between female and commitment treatment
reg balchange treatment marketing female female_treat, robust 
//Store Results 
eststo
// Regress Changes in Balance on treatment , marketing, active and 
//interaction between active and commitment treatment
reg balchange treatment marketing active active_treat, robust
//Store Results 
eststo
// Regress Changes in Balance on treatment , marketing, college education and 
//interaction between college education and commitment treatment
reg balchange treatment marketing edhi edhi_treat, robust
//Store Results 
eststo
// Regress Changes in Balance on treatment , marketing, high income  and 
//interaction between high income and commitment treatment
reg balchange treatment marketing hi_hh_inc hi_hh_inc_treat, robust 
//Store Results 
eststo
// Regress Changes in Balance on treatment , marketing, time inconsistent  and 
//interaction between time inconsistent and commitment treatment
reg balchange treatment marketing hyper_mon_new2 hyper_mon_new2_treat, robust
//Store Results 
eststo
// Regress Changes in Balance on treatment , marketing, patient now/not patient 
//future inconsistent  and  interaction between patient now/not patient future 
//and commitment treatment
reg balchange treatment marketing silly_mon_new2 silly_mon_new2_treat, robust 
//Store Results 
eststo
//Generate table 
esttab using heterogeneity.tex, se ar2 label 
eststo clear


//Manski Bounds
// Since Manski's original program to compute Bounds is no longer available, 
// I will attempt to compute those bounds in stata
//Import data (after six months)
use seedanalysis_011204_080404.dta, clear
//Relabel variables 
label variable balchange "Change in total savings"
// First compute the Average Treatment Effect 
ttest balchange, by(seedtakeup) // P value indicates that this difference in 
//means is statistically significant 
//Another approach (included in the final report)
reg balchange seedtakeup, robust
//Store Results 
eststo
esttab using ate.tex, se ar2  label 
eststo clear
//Now we'll bound the ATE 
//First we'll calculate expected value, probabilities and minimum and maximum 
// Balance changes 
quietly summarize balchange 
scalar minbalance=r(min) // Minimum Balance Change
scalar maxbalance=r(max) // Maximum Balance Change
quietly summarize seedtakeup if(seedtakeup==1 & treatment==1)
scalar pd1z1=r(N)/3153
quietly summarize seedtakeup if(seedtakeup==0 & treatment==1)
scalar pd0z1=r(N)/3153
quietly summarize seedtakeup if(seedtakeup==1 & treatment==0)
scalar pd1z0=r(N)/3153
quietly summarize seedtakeup if(seedtakeup==0 & treatment==0)
scalar pd0z0=r(N)/3153 
quietly summarize balchange if (seedtakeup==1 & treatment==1)
scalar eyd1z1=r(mean)
quietly summarize balchange if(seedtakeup==0 & treatment==1)
scalar eyd0z1=r(mean)
quietly summarize balchange if(seedtakeup==1 & treatment==0)
scalar eyd1z0=0
quietly summarize balchange if(seedtakeup==0 & treatment==0)
scalar eyd0z0=r(mean)
quietly summarize balchange if(seedtakeup==1)
scalar eyd1=r(mean)
quietly summarize balchange if(seedtakeup==0)
scalar eyd0=r(mean)
quietly summarize balchange if(treatment==1)
scalar eyz1=r(mean)
quietly summarize balchange if(treatment==0)
scalar eyz0=r(mean)
quietly summarize seedtakeup if(seedtakeup==1)
scalar pd1=r(N)/3153
quietly summarize seedtakeup if(seedtakeup==0)
scalar pd0=r(N)/3153
quietly summarize treatment if(treatment==1)
scalar pz1=r(N)/3153
quietly summarize treatment if (treatment==0)
scalar pz0=r(N)/3153
scalar list 

// ATE BOUNDS 
// Bounded Outcome  (BO)
scalar lb_n=(eyd1-maxbalance)*pd1+(minbalance-eyd0)*pd0 //lower natural bound
scalar ub_n=(eyd1-minbalance)*pd1+(maxbalance-eyd0)*pd0 //upper natural bound
display "The natural bounds are [ " lb_n " , " ub_n " ]"
//Tightening bounds 
// Mean Independence of the Instrument (MI) + Bounded Outcome (BO)
scalar lb_mi=(eyd1z1*pd1z1+minbalance*pd0z1)-(eyd0z0*pd0z0+maxbalance*pd1z0)
scalar ub_mi=(eyd1z0*pd1z0+maxbalance*pd0z0)-(eyd0z1*pd0z1+minbalance*pd1z1)
display "The mean independence bounds are [ " lb_mi " , " ub_mi "]"
// Monotone Treatment Response  (MTR) + Bounded Outcome (BO)
scalar lb_mtr=0 
scalar ub_mtr=(eyd1-minbalance)*pd1+(maxbalance-eyd0)*pd0
display "The MTR bounds are [ " lb_mtr " , " ub_mtr "]"
// Mean Independence of the Instrument(MI)+Monotone Treatment Response(MTR)+ BO
scalar lb_mimtr=eyz1-eyz0
scalar ub_mimtr=eyd1z1*pd1z1-eyd0z0*pd0z0+maxbalance*pd0z1-minbalance*pd1z0
display "The MI+MTR bounds are [ " lb_mimtr " , " ub_mimtr " ]"
// Monotone Treatment Selection (MTS) + Bounded Outcome (BO)
scalar lb_mts=(eyd1-maxbalance)*pd1+(minbalance-eyd0)*pd0 
scalar ub_mts=eyd1-eyd0 
display "The MTS bounds are [ " lb_mts " , " ub_mts " ]"
//Monotone Treatment Selection (MTS)+ Monotone Treatment Response (MTR)
scalar lb_mtsmtr=0
scalar ub_mtsmtr=eyd1-eyd0 
display "The MTS+MTR bounds are [ " lb_mtsmtr " , " ub_mtsmtr " ]"
// Monotone Instrumental Variable 
scalar lb10=eyd1z0*pd1z0+minbalance*pd0z0 
scalar lb00=eyd0z0*pd0z0+minbalance*pd1z0 
scalar lb11=eyd1z1*pd1z1+minbalance*pd0z1 
scalar lb01=eyd0z1*pd0z1+minbalance*pd1z1 
scalar ub10=eyd1z0*pd1z0+maxbalance*pd0z0 
scalar ub00=eyd0z0*pd0z0+maxbalance*pd1z0
scalar ub11=eyd1z1*pd1z1+maxbalance*pd0z1 
scalar ub01=eyd0z1*pd0z1+maxbalance*pd1z1 
scalar lb1=pz0*lb10+pz1*max(lb10,lb11)
scalar lb0=pz0*lb00+pz1*max(lb00,lb01)
scalar ub1=pz0*min(ub10,ub11)+pz1*ub11
scalar ub0=pz0*min(ub01,ub00)+pz1*ub01

scalar lb_miv=lb1-ub0
scalar ub_miv=ub1-lb0

display "The MIV bounds are [ " lb_miv  " , " ub_miv " ]"

// MTR+MTS+MIV 
scalar fub1=pz0*min(eyd1z0,eyd1z1)+pz1*eyd1z1 
scalar flb0=pz0*eyd0z0+pz1*max(eyd0z1,eyd0z0)

scalar lb_mtrmtsmiv=0
scalar ub_mtrmtsmiv=fub1-flb0

display "The MTR+MTS+MIV bounds are [ " lb_mtrmtsmiv  " , " ub_mtrmtsmiv " ]"
clear 
// Graph 

input lower upper tbound 
-9385.6831 106747.39 1
-411.2185 33884.534 2
0 106747.39 3
349.71968 23169.101 4
-9385.6831 912.80347 5
0 912.80347 6
-2268.2438 13057.879 7
0 160.47635 8
end 

twoway  rcap lower upper tbound,xlabel(1 "BO" 2 "MI+BO" 3 "MTR+BO" 4 "MI+MTR+BO" 5 "MTS+BO" 6 "MTS+MTR" 7 "MIV" 8 "MIV+MTS+MTR",angle(vertical)) ytitle("Change in Savings Balance") ylabel(#3) xtitle("")

log close
translate final.smcl final.pdf
