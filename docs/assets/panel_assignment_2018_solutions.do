******************** 		Econ 651: Assignment 3 		********************
************ 			Stata Solutions - Andrew Proctor 		************



********** Preliminaries
	capture log close _all // Closes any log if open //
	set more off
	set varabbrev off

	* Set working dir
	cd "/home/andrew/Dropbox/Teaching 2018/Assignment3"
	
	* Initialize log
	log using "Assignment3log", text replace
		
	* Import data
	use "qog_bas_ts_jan18.dta", clear
	
	* Merge Acemoglu Data
	merge m:1 ccodealp using "colonialorigins.dta"

	tabulate cname if _merge == 1
	
********** Data Preparation

	/*	
	Note:  most potential controls are undersirable for potential endogeneity 
	with quality of government (and I don't want multiple proxy variables 
	for quality of government).
	
	There are a lot of variables that may influence quality of government 
	(fractionalization, colonial legacy, etc) but may have persistent effects as 
	well.  Others, like quality of human capital, infrastructure, etc may be 
	intermediate outcomes of quality of government.  Neither are desirable to 
	controls, although their omission may likewise be problematic.  We should 
	prefer an econometric approach which isolates the effect of quality of 
	government while avoiding bias from direct effects of determinants of 
	quality of government, as well as effects of intermediate outcomes of 
	quality of government.
	
	*/
						
		label variable ccode "Countries"
		label variable year "Year"
	
	* Rename Variables
		rename vdem_corr corruption
		rename cname country
		rename ht_region region
		rename ccodealp country_abbrev
	
	* Reformat Time-Invariant Controls
		rename lat_abst latitude
		label var latitude "Latitude of capital (absolute value)"
		replace latitude = latitude *100
		rename f_brit BritishColony
		label var BritishColony "Former British colony"
		rename f_french FrenchColony
		label var FrenchColony "Former French colony"
		rename malfal94 malaria
		label var malaria "Malaria index in 1994"
		label var africa "Africa indicator"
		label var yellow "Yellow fever present today"
		label var meantemp "Mean temperature"
		
	* Reformat Factor Input Controls: Oil & Natural Gas Production, Population
		rename ross_gas_value_2014 gas
		replace gas = gas / 10000000000
		label var gas "National gas production (in 2014 dollars, 10 billions)"
		rename ross_oil_value_2014 oil
		replace oil = oil / 10000000000
		label var oil "National oil production (in 2014 dollars, 10 billions)"
		rename imf_pop pop
		replace pop = pop / 100
		label variable pop "Population (Lagged, 100 millions)"
		
	* Transform GDP
	gen logGDP = log(wdi_gdpcappppcon2011)
	label variable logGDP "Log of GDP (2011 Constant Dollars PPP)"
	
	* Save Data for Assignment 4
	save "C:/Users/AN.4271/Dropbox/Teaching 2018/Assignment4/assignments3to4data.dta", replace
	*save "/home/andrew/Dropbox/Teaching 2018/Assignment4/assignments3to4data.dta", replace
	
	* Keep relevant variables for Assignment 4
	keep corruption logGDP gle_rgdpc wdi_gdpcappppcon2011 ccode ///
		year country pop country_abbrev region gas ///
		oil wdi_refori latitude BritishColony FrenchColony ///
		 malaria africa yellow meantemp
	
	* Create mean corruption  by year
	egen mean_corruption = mean(corruption), by(year)
	
	* Select only countries with at least 10 observations for all variables
	gen obs_nonmissing = .
	replace obs_nonmissing = 1 if !missing(logGDP, corruption, ///
		pop,oil,gas) & year >= 1991
	egen counter = total(obs_nonmissing), by(ccode)
	keep if counter >= 10
	
********** Data Exploration

	set scheme sj   // Set Color Scheme of Stata Graphs
	
	eststo clear
	* Create Summary Statistics Table
	estpost summarize year logGDP corruption pop oil gas ///
		latitude meantemp yellow malaria BritishColony FrenchColony ///
		 if inrange(year, 1991,2016)
	esttab using "tables/summarystats", ///
		cells("count(fmt(a2)) mean(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") ///
		title("Summary Statistics of Key Variables") ///
		label rtf replace 	
		
	* Set Panel
	xtset ccode year

	* Summary Statistics
	summarize logGDP corruption  if inrange(year,1991,2016)
	
	* Show countries with highest and lowest GDP values (start and end year)
		count if year == 2016 & !missing(logGDP, corruption)
		scalar obs_ranks = r(N)
		
		gen nGDP = logGDP * -1
		egen rk_GDP =  rank(nGDP) if year == 2016 & !missing(logGDP, corruption)
		egen rk_Corr =  rank(corruption) if year == 2016 & !missing(logGDP, corruption)

		sort rk_Corr
		list country rk_Corr rk_GDP region if year == 2016 & ///
			(rk_Corr <= 10 | rk_Corr >= (obs_ranks - 9)) & ///
			!missing(logGDP, corruption)	
		
	* Within- and Between- Variation
	xtsum corruption if inrange(year,1991,2016)
		
	* Graph of mean corruption  by year
	graph twoway line  mean_corruption year if inrange(year,1991,2016), sort ///
		title("Mean corruption  by Year") ///
		ytitle("Mean corruption ") ///
		xtitle("Year")
	graph export "scatterplot.emf", as(emf) replace
	
	codebook if inrange(year,1991,2016)
	
	***** Histograms
		* Histogram of GDP
		histogram logGDP if inrange(year,1991,2016),  ///
			xscale(range(5 12))  ///
			yscale(range(0 0.4)) ///
			title("Distribution of GDP") ///
			subtitle("(all years)") ///
			xtitle("Log GDP")
		graph export "histogram_gdp.emf", as(emf) replace

			* Histogram of GDP for 1991
			histogram logGDP if year == 1991,  ///
				xscale(range(5 12))  ///
				yscale(range(0 0.4)) ///
				title("Distribution of GDP") ///
				subtitle("in 1991") ///
				xtitle("Log GDP")
			graph export "histogram_gdp1991.emf", as(emf) replace

			* Histogram of GDP for 2016
			histogram logGDP if year == 2016,  ///
				xscale(range(5 12))  ///
				yscale(range(0 0.4)) ///
				title("Distribution of GDP") ///
				subtitle("in 2016") ///
				xtitle("Log GDP") 
			graph export "graphs/histogram_gdp2016.emf", as(emf) replace
		
		* Histogram of Corruption
		histogram corruption if inrange(year,1991,2016),  ///
			title("Distribution of Political corruption Index") ///
				subtitle("(all years)") ///
			xtitle("Political corruption  Index")
		graph export "graphs/histogram_corrupt.emf", as(emf) replace
		
			* Histogram of corruption for 1991
			histogram corruption if year == 1991,  ///
				title("Distribution of Political corruption Index") ///
				subtitle("in 1991") ///
				xtitle("Political corruption Index")
			graph export "graphs/histogram_corrupt1991.emf", as(emf) replace

			* Histogram of corruption for 2016
			histogram corruption if year == 2016,  ///
				title("Distribution of Political corruption Index") ///
				subtitle("in 2016") ///
				xtitle("Political corruption Index")
			graph export "graphs/histogram_corrupt2016.emf", as(emf) replace
			
			xtset ccode year
			histogram corruption if year == 2016 & !missing(L16.corruption),  ///
				title("Distribution of Political corruption Index") ///
				subtitle("in 2016") ///
				xtitle("Political corruption Index") 
			graph export "graphs/histogram_corrupt2016_sampleconsist.emf", as(emf) replace
	
	***** Scatterplots
		* 1991-2016
		graph twoway scatter logGDP corruption if inrange(year,1991,2016), ///
			title("Scatterplot of corruption and GDP") ///
				subtitle("(all years)") ///
			ytitle("Log GDP") ///
			xtitle("Political corruption Index")
		graph export "graphs/scatter_GDPcorrupt.emf", as(emf) replace

		* Scatterplot for 1991
		graph twoway scatter logGDP corruption if year == 1991, ///
			title("Scatterplot of corruption and GDP") ///
			subtitle("in 1991") ///
			ytitle("Log GDP") ///
			xtitle("Political corruption Index") ///
			mlabel(country_abbrev) m(i) mlabsize(vsmall)
		graph export "graphs/scatter_GDPcorrupt1991.emf", as(emf) replace
		
		* Scatterplot for 2016
		graph twoway scatter logGDP corruption if year == 2016, ///
			title("Scatterplot of corruption and GDP") ///
			subtitle("in 2016") ///
			ytitle("Log GDP") ///
			xtitle("Political corruption Index") ///
			mlabel(country_abbrev) m(i) mlabsize(vsmall)
		graph export "graphs/scatter_GDPcorrupt2016.emf", as(emf) replace
	
	* Missingness
	gen is_missing = .
	replace is_missing = 0 if !missing(logGDP,corruption)
	replace is_missing = 1 if missing(logGDP,corruption)
	label var is_missing "Missing data (=1)"
	
	gen corruption_missing = .
	replace corruption_missing = 0 if !missing(corruption)
	replace corruption_missing = 1 if missing(corruption)
	label var corruption_missing "Corruption data missing (=1)"
	
	gen GDP_missing = .
	replace GDP_missing = 0 if !missing(logGDP)
	replace GDP_missing = 1 if missing(logGDP)
	label var GDP_missing "GDP data missing (=1)"
	
	label define GDP_missing_lab 0 "GDP data missing" 1 "GDP data non-missing"
	label define corruption_missing_lab 0 "Corruption data missing" 1 "Corruption data non-missing"
	label values GDP_missing GDP_missing_lab
	label values corruption_missing corruption_missing_lab
	
	summarize is_missing
	summarize is_missing if !missing(logGDP)

	egen mean_corruption_country = mean(corruption) if inrange(year,1991,2016), by(ccode)
	label var mean_corruption_country "Average corruption index in country, 1991-2016"
	egen mean_GDP_country = mean(logGDP) if inrange(year,1991,2016), by(ccode)
	label var mean_GDP_country "Average GDP per capita in country, 1991-2016"

	eststo clear
	eststo: reg is_missing mean_corruption_country mean_GDP_country, robust
	eststo: reg corruption_missing logGDP i.year if inrange(year,1991,2016)
	eststo: xtreg GDP_missing corruption i.year if inrange(year,1991,2016), fe
		
	esttab using "tables/missingness", title("Regression of missingness by variable") ///
		drop(19* 20*) ///
		se label wrap noabbrev rtf star(* 0.10 ** 0.05 *** 0.01) ///
		b(%9.2fc) compress one replace 
	eststo clear

	eststo clear
	estpost  tabulate corruption_missing GDP_missing	
	esttab using "tables/missing_cross_tab", cell(b(fmt(%11.0g))  b(fmt(%11.0g) par keep(Total))) ///
		collabels(none) noabbrev unstack noobs nonumber nomtitle ///
	 	title("Cross-Tabulation Table of Missing GDP and Corruption Data") ///
		rtf replace 	
                   
********** Regression Analysis

**** Basic Pooled OLS
eststo clear

	* Set Panel
	xtset ccode year
	
	* Without controls
	eststo: regress logGDP corruption if inrange(year,1991,2016), robust
	
	* Insitutions and Climate/Disease Environment Controls
	eststo: regress logGDP corruption latitude meantemp yellow malaria /// 
		BritishColony FrenchColony, robust
	
	* Domestic controls
	eststo: regress logGDP corruption L5.pop oil gas, robust
	
	* Create tables
	esttab using "tables/pooled", ///
		title("Pooled OLS Regression of GDP and corruption ") ///
		se label wrap noabbrev rtf star(* 0.10 ** 0.05 *** 0.01) ///
		b(%9.2fc) compress one replace 

	**** Basic Pooled OLS
eststo clear

	* Set Panel
	xtset ccode year
	
	* Without controls
	eststo: regress logGDP corruption if inrange(year,1991,2016), cluster(ccode)
	
	* Insitutions and Climate/Disease Environment Controls
	eststo: regress logGDP corruption latitude meantemp yellow malaria /// 
		BritishColony FrenchColony ,  cluster(ccode)
	
	* Domestic controls
	eststo: regress logGDP corruption L5.pop oil gas, cluster(ccode)
	
	* Create tables
	esttab using "tables/pooled_andclustered", ///
		title("Pooled OLS Regression of GDP and corruption ") ///
		se label wrap noabbrev rtf star(* 0.10 ** 0.05 *** 0.01) ///
		b(%9.2fc) compress one replace 
		
		
**** Fixed Effects Regression

	* Without controls
	eststo: xtreg logGDP corruption i.year if inrange(year,1991,2016), fe robust

	* Factor input controls	
	eststo: xtreg logGDP corruption L5.pop oil gas i.year ///
		if inrange(year,1991,2016), fe robust
	
	* Create tables
	esttab using "tables/fixeff", ///
		title("Fixed Effects Regression of GDP and corruption") ///
		drop(19* 20*) se label wrap noabbrev rtf ///
		star(* 0.10 ** 0.05 *** 0.01) b(%8.2g) compress one replace 
	eststo clear
	
	/*
	Without lag:
		xtreg logGDP corruption pop oil gas i.year ///
			if inrange(year,1991,2016), fe robust
	*/
	
**** Random Effects Regression
eststo clear

	* Without controls
	eststo: xtreg logGDP corruption i.year if inrange(year,1991,2016), re theta robust

	* Insitutions and Climate/Disease Environment Controls
	eststo: xtreg logGDP corruption latitude meantemp yellow malaria /// 
		BritishColony FrenchColony  if inrange(year,1991,2016), re theta robust
	
	* Factor input controls	
	eststo: xtreg logGDP corruption L5.pop oil gas i.year ///
		if inrange(year,1991,2016), re theta robust
	
	* Create tables
	esttab using "tables/randomeff", ///
		title("Random Effects Regression of GDP and corruption") ///
		drop(19* 20*) se label wrap noabbrev rtf ///
		star(* 0.10 ** 0.05 *** 0.01) b(%9.2fc) compress one replace 
	eststo clear

	

*****************************************************************************
************* 				Extra Analysis				 		*************
*****************************************************************************

**** LDV Regression

eststo: reg logGDP L(1/3).logGDP corruption i.year, vce(cluster ccode)

	
/* Here, I use a pooled regression specification including lags of GDP.  These 
lags are useful in two ways:

First, output is often considered autoregressive, such that a primary determinant
 of GDP growth is growth in previous periods.  

Moreover, given that controlling for the lag of GDP is tantamount to controlling 
for the persistent effect of lagged determinants of the GDP, the lag variable may
partially control for OVBs in a manner somewhat similar to fixed effects.  
Lags should not be expected to capture time-invariant effects as well as FE, but 
FE by construction will not capture time-varying omitted variables, while lags 
can partially capture time-varying effects that are common between the lagged 
and current period.  

Since lagged corruption  is presumed to be a determinant of the lagged GDP, it is
worth noting that the explanatory variable becomes a measure of the
effect of the contemporaneous corruption conditional on lagged corruption .
*/


	/* 
	--Note 1--
	
	Originally, I ran:
		reg logGDP L(1/5)logGDP corruption pop i.year, vce(cluster ccode)
	But note insignificance of later lags.  Reducing it to 3 lags, I get 
	consistence significance and higher F-stats.  This may or may not be
	better. 
	*/

	/*
	--Note 2--

	I could also include lags of the corruption  measure, but under the 
	assumption of linear additivity of variables in logGDP, then this is 
	already at least partially captured. Running the following regression, 
	I get no significance for additional corruption  lags.
	
	reg logGDP L(1/3).logGDP L(0/3).corruption i.year, vce(cluster ccode)
	*/

/* As a reminder:  Should we trust any regression here with no real case for 
exogeneity?  No.  The regression below looks somewhat plausible, and would seem 
to yield a very different interpretation of the effects of corruption than the 
preceding LDV models.
	reg logGDP L(1/5).logGDP L(0/5).corruption i.year, vce(cluster ccode)
*/

*** Deep Lags as an instrument
	eststo: reg logGDP L(20).logGDP L(20).corruption i.year, vce(cluster ccode)
	/* Assumes 20-year lagged corruption  is only related to current GDP 
	through it's relationship with current corruption , and 20-year lagged 
	GDP is only related to current GDP through eg time-invariant 
	determinants of production (unrelated to current corruption 
	conditional on lagged corruption ). */

	/* 
	Alternately:
	reg logGDP L(20).corruption i.year, vce(cluster ccode)
		/* Instrument for current corruption  using a deep lag.  */
	reg logGDP L(20).logGDP corruption i.year, vce(cluster ccode)
		/* Control for time fixed effects through deep lag. */
	*/

*** Reverse Causality?
eststo: xtreg logGDP F(0/3).corruption i.year, fe robust

/*
We find that *future* corruption  values are estimated to affect current consumption,
which is probably not reasonable.  Hence, it appears likely there is simultaneity 
between GDP and the quality of government measure. 
*/

esttab using "tables/leadsandlags", ///
	title("Further Analysis: LDV Regression and Reverse Cauality") ///
		mtitles("LDV" "Deep Lags as Instruments" "Reverse Causality") ///
		drop(19* 20*) se label wrap noabbrev rtf ///
		star(* 0.10 ** 0.05 *** 0.01) compress one replace
eststo clear




********** End do-file

log close _all
