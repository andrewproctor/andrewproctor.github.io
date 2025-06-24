******************** 		Econ 651: Panel Seminar		********************
************ 			Stata Solutions - Andrew Proctor 		************

********** Preliminaries

	capture log close _all // Closes any log if open //
	set more off		   // In case any command creates a lot of outuput 

	* Set working dir
	cd "/Users/home/Dropbox/Teaching 2018/StataPanelSeminar/Exercise/"
	
	* Initialize log
	log using "panel_seminar_log", text replace
		
	* Import data
	use "NLSY97.dta", clear  // Note 'clear' options so you can rerun the do file.

********** Data Preparation

***** Generate and format indicator variables

* Female indicator variable
gen female = .						// Start with missing values
replace female = 0 if sex == 1		// Set female = 0 if sex indicates Male
replace female = 1 if sex == 2		// Set female = 1 if sex indicates Female
label variable female "Female"		// Label variable
drop sex							// Drop old sex variable

* Urban indicator variable
gen urban = .							// Start with missing values
replace urban = 0 if rural_urban == 0	// Set urban = 0 for rural areas
replace urban = 1 if rural_urban == 1	// Set urban = 1 for urban areas
label variable urban "Urban"			// Label variable
drop rural_urban						// Drop old rural_urban variable

* High school indicator variable
gen highschool = . 		// Same process for new completed HS completion indicator
replace highschool = 0 if highest_grade_compl < 12
replace highschool = 1 if highest_grade_compl >= 12 & ///
	!missing(highest_grade_compl)  
label variable highschool "Completed High School"

/* 
Note the !missing(highest_grade_compl) condition when coding the 'highschool' 
indicator variable.  Since 'highest_grade_compl' has missing values, I need 
the "not missing: condition or else all missing values would be coded as 
highschool = 1, since missing values evaluate to the largest possible values
in Stata.
*/


***** Generate continuous variables
gen age = year - birthyr 				// Created imputed age variable
gen logearnings = log(wage_salary_yr)	// Generate log of earnings variable
replace hs_gpa = hs_gpa / 100 	/* Note: the label for hs_gpa says it's a 4.0 
								scale, but the actual scale is 0-400.  So I 
								rescale it back to 0-4.						*/

								
								
********** Exploratory Analysis

set scheme sj   /* This sets the color scheme of the Stata graphs so that they 
					are grayscale.  I didn't ask you to do this, but I like 
					for the graphs to appear this way.						*/


*** Mean Earnings by Age
egen mean_earnings = mean(wage_salary_yr), by(age) // Gen mean earngings by age
graph twoway line  mean_earnings age, sort ///
	title("Mean Earnings by Age") ///
	xtitle("age in years") ///
	ytitle("Earnings (nominal USD)")

graph export "earningsbyage.tif", replace

*** Scatterplot of GPA and Earnings
graph twoway scatter logearnings hs_gpa if year == 2015 ///
 & census_region == 1 & age == 32, ///
	title("Log earnings and high school GPA") ///
	xtitle("High school GPA (4.0 Scale)") ///
	ytitle("Log of earnings")
graph export "earningsandGPA.tif", replace


********** Regression Analysis

xtset person_id year // Set Panel Structure

eststo clear	/* When storing regression output to create tables, it's best
					to start with eststo clear so that if you need to rerun the 
					code, it won't duplicate any table. 				*/

eststo: regress logearnings asvab_score hs_gpa highschool  ///
	yrs_at_job effort_at_work dependable female urban ///
	  i.age i.race_ethnicity i.census_region i.year, robust
	
eststo: xtreg logearnings asvab_score hs_gpa highschool  ///
	yrs_at_job effort_at_work dependable female urban ///
	  i.age i.race_ethnicity i.census_region i.year, re robust
	
eststo: xtreg logearnings asvab_score hs_gpa highschool  ///
	yrs_at_job effort_at_work dependable female urban ///
	  i.age i.race_ethnicity i.census_region i.year, fe robust

	predict earnings_resid, residuals // Save residuals

* Save regression output tables
esttab using "panel_seminar_exercise_regs", ///
	title("Regression of earnings on education, experience, and other determinants") ///
	mtitles("Pooled OLS" "Random Effects" "Fixed Effects") ///
	se label wrap noabbrev rtf  ///
	drop(16* 17* 18* 19* 20* 21* 22* 23* 24* 25* 26* 27*) ///
	star(* 0.10 ** 0.05 *** 0.01) b(%8.2g) ///
compress one replace
eststo clear
	
*** Graphs of Residuals
graph twoway scatter earnings_resid year if person_id == 2
graph twoway scatter earnings_resid year if person_id == 13
graph twoway scatter earnings_resid year if person_id == 21
graph twoway scatter earnings_resid year if person_id == 28
graph twoway scatter earnings_resid year if person_id == 35
graph twoway scatter earnings_resid year if person_id == 41
graph twoway scatter earnings_resid year if person_id == 49
graph twoway scatter earnings_resid year if person_id == 53


****************************************************************************
****						Interpretation								****
****************************************************************************
/*

1. Why are some variables omitted from the fixed effects regression?

The demographic variables (sex and race) as well as the ability measures
(ASVAB score and high school GPA) are omitted from the fixed effects 
regression because they are time-invariant.  When we think of the fixed effects
explanatory variable as the demeaned x_it - avg(x)_i, then it's clear when 
these variables do not vary over time, they will be collinear with the 
individual fixed effects.



2. How (and why) do the standard errors of estimates change between estimates?

The standard errors get larger going from pooled OLS, to random effects, to 
fixed effects regressions.  This makes sense given that OLS uses all of the 
within and between variation, fixed effects uses only within variation, and 
random effects uses something inbetween.

3.   Do the residuals look autocorrelated? 

Yes.  There are occasionally big jumps in the error, but for the most part 
the residual in one period looks like its position is related to the position of 
the residual in the previous period.

*/
****************************************************************************
****						General Notes								****
****************************************************************************
/*

1.  Note the "i." prefix for the census_region, age, and race_ethnicity 
controls.  In the exercise, we did not create indicator variables, but in the 
regression you can quickly control for indicators for each distinct value of 
these variables by using the "i." prefix. (Note, I have also done this for 
years).

Using the "i." prefix is great for anything where you need to generate a lot of
indicators quickly, which are not the main explanatory variables of analysis.

2.  Note that when I run the fixed effects regression, I still include 
indicators for each year by using i.year (which are of course, the year fixed 
effects).  It is tempting to think that fixed effects regression will 
control for these automatically, but it does not.  The fixed effect estimator
by default controls only for the individual effects (the equivalent of 
i.person_id).

Hence, you always need to control for time trends by creating time fixed effects 
manually.  You can also control for time fixed effects in pooled OLS and random 
effects regression, as the same time trend concerns apply there too.

3.  Note that one of the options for the `esttab` command is 
"drop(16* 17* 18* 19* 20* 21* 22* 23* 24* 25* 26* 27*)"

What does this do?  Compare the regression output vs the word table generated 
by esttab.  The fixed effects for each year (2000-2009) and age (16-27) are 
missing in the regression table that I exported.  Thats because I have omitted 
displaying any estimates for variables that start with these values (hence 
the year fixed effects are omitted because of the "20*" term in the drop.

These age and year fixed effects are not my primary explanatory variables, 
just necessary controls.  To make the table I present less cluttered, I 
therefore do not report the estimates for the variables.
*/
