* import data and keep and rename relevant variables
* import total nutrient day 1 2015
fdause DR1TOT_I.XPT, clear
quietly compress
gsort +seqn
generate carb_ratio = dr1tcarb*4 / dr1tkcal
* 0 for carb_ratio between 0.45 and 0.65
generate carb_level = 0
replace carb_level = 1 if carb_ratio < 0.45
replace carb_level = 2 if carb_ratio > 0.65
keep seqn carb_level
save DR1TOT_l.dta, replace

* import sleep disorder data
fdause SLQ_I.XPT, clear
quietly compress
gsort +seqn
keep seqn sld012 slq120
* drop 'dont know' response
drop if slq120 == 9
rename sld012 sleephr
rename slq120 sleepy
merge 1:1 seqn using DR1TOT_l.dta
drop _merge
save DRTOT_SLP.dta, replace

* import demographic data
fdause DEMO_I.XPT, clear
quietly compress
gsort +seqn
generate winter = 0
replace winter = 1 if ridexmon == 1
* 0 male 1 female
generate gender = riagendr - 1
keep seqn gender ridageyr winter
rename ridageyr age
merge 1:1 seqn using DRTOT_SLP.dta
keep if _merge == 3
drop _merge
label define sleepiness 0 "Never" 1 "Rarely" 2 "Sometimes" 3 "Often" 4 "Almost always"
label values sleepy sleepiness
save DRTOT_SLP_DEMO.dta, replace

* response: sleepy (five levels: 0-4)
* independent variables: carb_ratio
* control variables: gender, age, winter, sleephr

* ordered logistic regression (proportional odds assumption)
ologit sleepy i.carb_level sleephr gender age winter

* intall package for brant test to check assumption
net from http://www.indiana.edu/~jslsoc/stata/
net install spost13_ado
brant

* assumption for ologit violated so try generalized
ssc install gologit2

* compare between models
quietly gologit2 sleepy i.carb_level sleephr gender age winter, store(gologit)
* Partial Proportional Odds Model (constraints from brant test)
quietly gologit2 sleepy i.carb_level sleephr gender age winter, store(gologit2) pl(age i.carb_level gender)
quietly gologit2 sleepy i.carb_level sleephr gender age winter, store(ologit) pl
* ologit is too restrictive
lrtest ologit gologit
* if use significance level 0.001, partial proportional odds is not too restrictive
lrtest gologit gologit2

* model we chose
quietly gologit2 sleepy i.carb_level sleephr gender age winter, store(gologit2) pl(age i.carb_level gender)

* coefficient table
esttab gologit2, wide label title(Rgression Table for Partial Proportional Odds Model) addnote("Source: DRTOT_SLP_DEMO.dta")

* marginal effect for carb_level
mtable, dydx(carb_level)
