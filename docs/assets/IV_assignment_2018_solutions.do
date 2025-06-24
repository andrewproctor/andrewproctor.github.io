******************** 		Econ 651: Assignment 3 		********************
************ 			Stata Solutions - Andrew Proctor 		************



********** Preliminaries
	capture log close _all // Closes any log if open //
	set more off
	set varabbrev off

	* Set working dir
	*cd "/Users/home/Dropbox/Assignment4/"
	cd "C:/Users/AN.4271/Dropbox/Teaching 2018/Assignment4"
	
	* Initialize log
	log using "Assignment4log", text replace
		
	* Import data
	use "assignments3to4data.dta", clear
	
********** Data Preparation

	keep if year == 2013
	rename hf_prights prop_rights
	rename ht_colonial colonizer
	
	* Region Dummie	
	summarize country if !missing(logGDP, prop_rights,ajr_settmort) ///
		& (colonizer != 0)
	tabulate colonizer if !missing(logGDP, prop_rights,ajr_settmort) ///
		& (colonizer != 0)

	** Exploratory analysis
	scatter logGDP prop_rights
	
	
********** Regression Analysis

*** Specification without Controls
	eststo clear
	* Structural Equation
	eststo: reg logGDP prop_rights, robust

	* Reduced Form
	eststo: reg logGDP ajr_settmort if ///
		!missing(logGDP, prop_rights, ajr_settmort), robust

	* First-Stage
	eststo: reg prop_rights ajr_settmort if ///
		!missing(logGDP, prop_rights, ajr_settmort), robust
	predict proprights_hat, xb
	label var proprights_hat "Predicted Property Rights from first stage"

	*IV Regression
	eststo: ivreg2 logGDP (prop_rights = ajr_settmort), robust
	
	* IV Manual Estimates
	eststo: reg logGDP proprights_hat if !missing(logGDP, prop_rights, ajr_settmort)

	* Create tables
	esttab using "Tables/iv_nocontrols", ///
		title("Table 1: IV Regression of GDP and Property Rights (no controls)") ///
		mtitles("Log GDP (OLS)" "Log GDP (Reduced Form)" ///
		"Property Rights (1st Stage)" "Log GDP (IV)" "Log GDP (Manual IV)") ///
		se label wrap noabbrev rtf  ///
		star(* 0.10 ** 0.05 *** 0.01) b(%9.2fc) compress one replace 
	eststo clear
	
drop proprights_hat
	
*** Specification with climate and disease environment controls
	eststo clear
	* OLS - Structural Equation
	eststo: reg logGDP prop_rights latitude malaria yellow, robust

	* Reduced Form
	eststo: reg logGDP ajr_settmort latitude malaria yellow if ///
		!missing(prop_rights,latitude,malaria, yellow), robust
	test latitude malaria yellow // F Test of controls
	
	* First-Stage
	eststo: reg prop_rights ajr_settmort latitude malaria yellow if ///
		!missing(logGDP), robust
	test latitude malaria yellow // F Test of controls
	predict proprights_hat, xb
	label var proprights_hat "Predicted Property Rights from first stage"
	
	*IV Regression
	eststo: ivreg2 logGDP (prop_rights = ajr_settmort) ///
		latitude malaria yellow, robust
	
	*IV Manual Estimates
	eststo: reg logGDP proprights_hat latitude malaria yellow if ///
		!missing(prop_rights, ajr_settmort), robust   

	* Create tables
	esttab using "Tables/iv_climatecontrols", ///
		title("Table 2: IV Regression of GDP and Property Rights (disease and climate controls)") ///
		mtitles("Log GDP (OLS)" "Log GDP (Reduced Form)" ///
		"Property Rights (1st Stage)" "Log GDP (IV)" "Log GDP (Manual IV)") ///
		se label wrap noabbrev rtf   ///
		star(* 0.10 ** 0.05 *** 0.01) b(%9.2fc) compress one replace 
	eststo clear

drop proprights_hat
	
*** Specification with fractionalization controls
	* Structural Equation
	eststo: reg logGDP prop_rights al_ethnic al_language al_religion, robust
	
	* Reduced Form
	eststo: reg logGDP ajr_settmort al_ethnic al_language al_religion if ///
		!missing(prop_rights), robust
	test al_ethnic al_language al_religion // F Test of controls
	
	* First-Stage
	eststo: reg prop_rights ajr_settmort al_ethnic al_language al_religion if ///
		!missing(logGDP), robust
	test al_ethnic al_language al_religion // F Test of controls
	predict proprights_hat, xb
	label var proprights_hat "Predicted Property Rights from first stage"
	
	*IV Regression
	eststo: ivreg2 logGDP (prop_rights = ajr_settmort) al_ethnic al_language ///
		al_religion, robust
	
	*IV Manual Estimates
	eststo: reg logGDP proprights_hat al_ethnic al_language al_religion if ///
		!missing(prop_rights, ajr_settmort), robust   

	* Create tables
	esttab using "Tables/iv_fractcontrols", ///
		title("Table 3: IV Regression of GDP and Property Rights (fractionalization controls)") ///
		mtitles("Log GDP (OLS)" "Log GDP (Reduced Form)" ///
		"Property Rights (1st Stage)" "Log GDP (IV)" "Log GDP (Manual IV)") ///
		se label wrap noabbrev rtf  ///
		star(* 0.10 ** 0.05 *** 0.01) b(%9.2fc) compress one replace 
	eststo clear




********** End do-file

log close _all
