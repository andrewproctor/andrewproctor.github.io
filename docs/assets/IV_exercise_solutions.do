************* 	   Econ 651: Instrumental Variables Exercise	   *************
************ 			Stata Solutions - Andrew Proctor 		    ************

********** Preliminaries

	capture log close _all // Closes any log if open //
	set more off		   // In case any command creates a lot of outuput 

	* Set working dir
	cd "/Users/home/Dropbox/Teaching 2018/IV Seminar/Exercise"
	
	* Initialize log
	log using "iv_seminar_log", text replace
		
	* Import data
	use "angrist_evans_data.dta", clear // Note 'clear' options so you can rerun the do file.

	
********** Regression Analysis

		
***** Instrumental variables regression
	eststo clear
	
	***  OLS Regression 
	eststo my_ols: reg workedind_moth morekids boy1st boy2nd black_mother ///
		hisp_moth othrace_moth if Main == 1
		
	*** IV Regression (including both first stage and reduced form)
	eststo my_iv: ivreg2 workedind_moth boy1st boy2nd black_mother ///
		hisp_moth othrace_moth (morekids = samesex) if Main == 1, ///
		first savefirst savefprefix(first) saverf saverfprefix(reduced)
	
	esttab my_ols my_iv first* reduced* using "motherworked", ///
		title("IV Regression of Family Size and Probability of Mother Working") ///
		mtitles("Mother worked (=1)  OLS" "Mother worked (=1) IV" ///
			"More than 2 children (=1) 1st Stage" ///
			"Mother worked (=1) Reduced Form") ///
		se label wrap noabbrev rtf  ///
		star(* 0.10 ** 0.05 *** 0.01) b(%8.2g) ///
		compress one replace
		
	/* Note:
	Using eststo on ivreg2 is a little more complicated if you use it 
	to show you first stage or reduced form results.   When you do this, 
	the command shows more than 1 regression at a time (ie the IV 
	estimates of the structural equation, as well as the first stage or 
	reduced form.
	
	To get estout to save and display the right regressions, you need to 
	name each regression when stroing them, then list the name of each 
	regression you want reported in the estta.  
	
	To name OLS or structural IV regressions, just add a name immediately after 
	eststo.  I have given the name "my_ols" to the OLS regression, and "my_iv" 
	to the structural equation estimates from ivreg2, above.
	
	To save and name first stage and reduced form regressions, the easiest 
	thing to do is copy what I have written.  For the reduced form, use as 
	options: 
		saverf saverfprefix(reduced)
	And for saving the first stage, use as options:
		savefirst savefprefix(first)
	Then, when you are ready to create the table, list the names of 
	any regressions you wanted reported, where the names of the 
	first stage and reduced form are (respectivelY):
			first* reduced*
	
	Of course, a much easier option when it comes to saving the first 
	stage and reduced forms is to run these as OLS regressions 
	manually. This is what I suggest - and what I have done for the 
	next examples, where hours worked and income are the dependent variables.
	*/
	
	eststo clear

	**** Dependent variable: The numbers of hours worked by the mother
	
	***  OLS Regression 
	eststo: reg hourswked_moth morekids boy1st boy2nd black_mother ///
		hisp_moth othrace_moth if Main == 1
		
	*** IV Regression
	eststo: ivreg2 hourswked_moth boy1st boy2nd black_mother ///
		hisp_moth othrace_moth (morekids = samesex) if Main == 1
		
		*** First Stage (using regress)
		eststo: reg morekids samesex boy1st boy2nd black_mother ///
			hisp_moth othrace_moth if Main == 1
				
		*** Reduced Form (using regress)
		eststo: reg hourswked_moth samesex boy1st boy2nd black_mother ///
			hisp_moth othrace_moth if Main == 1
	
	esttab using "hourswkd", ///
		title("IV Regression of Family Size and Mother's Hours Worked") ///
		mtitles("Mother worked (=1)  OLS" "Mother worked (=1) IV" ///
			"More than 2 children (=1) 1st Stage" ///
			"Mother worked (=1) Reduced Form") ///
		se label wrap noabbrev rtf  ///
		star(* 0.10 ** 0.05 *** 0.01) b(%8.2g) ///
		compress one replace
	eststo clear
	
	**** Dependent variable: The total income of the mother 
	
	***  OLS Regression 
	eststo: reg totalinc_moth morekids boy1st boy2nd black_mother ///
		hisp_moth othrace_moth if Main == 1
		
	*** IV Regression
	eststo: ivreg2 totalinc_moth boy1st boy2nd black_mother ///
		hisp_moth othrace_moth  (morekids = samesex) if Main == 1
		
		*** First Stage (using regress)
		eststo: reg morekids samesex boy1st boy2nd black_mother ///
			hisp_moth othrace_moth if Main == 1
				
		*** Reduced Form (using regress)
		eststo: reg totalinc_moth samesex boy1st boy2nd black_mother ///
			hisp_moth othrace_moth if Main == 1
	
	esttab using "totalinc", ///
		title("IV Regression of Family Size and Mother's Income") ///
		mtitles("Mother worked (=1)  OLS" "Mother worked (=1) IV" ///
			"More than 2 children (=1) 1st Stage" ///
			"Mother worked (=1) Reduced Form") ///
		se label wrap noabbrev rtf  ///
		star(* 0.10 ** 0.05 *** 0.01) b(%8.2g) ///
		compress one replace
	eststo clear
	
****************************************************************************
****					Interpratation Questions  						****
****************************************************************************
/*

*Question:*
Why do you think that Angrist and Evans need an IV strategy? Why 
can’t they get causal estimates by just running OLS regression of number of 
children on parental labor supply? Could they control for any ommitted variable 
bias by adding controls?

*Answer:*
Angrist and Evans are interested in the effect of family size on labor supply,
such as whether or not a parent works, how many hours they worked, and how 
much they earned. But there are any number of reasons why family sizes differ 
between individuals, in ways that are likely to also affect labor supply.

Some examples of possible confounders include age, educational level (more 
educated people tend to have less children), personality (if someone does 
not get along well with other peoples, it can be harder to both have good 
jobs and find a partner), and attitude towards work (people that are more 
career driven may purposefully put off having children).  But there are 
many more possible confounders.  

Rather than trying to identify and control for each possible confounder, 
a more desirable strategy would be to find a way to isolate variation in the 
number of children a parent has that is as good as random.  

*Question:*
Why might the first two children being of the same sex serve as a valid 
instrument for having more than than 2 children? Consider both relevance and 
exogeneity.

*Answer:*
To try to identify exogenous (as good as random) variation in family size, 
Angrist and Evans use whether or not parents' first two children were of the 
same sex to predict whether or not they had a third child.  

The hypothesis is that both children being of the same sex is a relevant 
predictor of having one or more additional children because parents are often 
thought to have preferences for child gender or gender mix.  If parents want a 
girl and they have only had boys, they may be more likely to have an additional 
child. Or they might think that it is beneficial for boys to have sisters and 
girls to have brothers. To the extent that these preferences do exist, then 
having the first two children from the same sex should be a relevant predictor 
of having 3 or more children.

The argument for validity of the instrument is that birth gender itself is 
already as good as random, hence having two children of the same sex should 
also be as good as random.  Since birth gender is random, only a couple of 
possibilities would seem to pose a potential problem to instrument exogeniety.

One concern is that having two children of the same sex might tend to have 
a direct effect on labor supply other than through it's effect on number of 
children.  Using the controls for having a sex of the first child and the sex 
child  (boy1st and boy2nd) controls for the direct effects of gender of the first 
two children, so it is only a unique direct effect of having two children of the 
same gender that the researcher needs to worry about. This would be true for 
instance if having two children of the same sex allowed parents to leverage 
economies of scale (eg less time spent clothes shopping, taking both children to 
the same afterschool activities, etc).

A second concern is that although gender of children conceived is as good as 
random, parents may choose to selectively terminate pregnancies.  If selection 
is related to gender, then having two children of the same gender will no longer 
be random.

*Question:*
How do the estimates change between OLS and IV? And the standard errors?

*Answer:*
Both OLS and IV estimates of family size on labor supply find negative effects. 
However, the IV estimates of the effect of family size (measured by having 3 or 
more children) on mother's income is about 40% smaller than the OLS estimates. 
Similarly, the estimated effect on probability of employment is about 33% (or 4 
percentage points) smaller.  The effect on hours worked per week is smaller, with 
estimated effect of -4.2 hours vs -4.5 in OLS.  For  every variable, the standard 
error of the esimates is smaller in the OLS regression than in IV, as expected.
 
 
*Question:*
Evaluate the instrument relevance and weakness using the ivreg2 output. Can the 
test of overidentifying restrictions tell us anything about the instrument 
exogeneity in this case?

*Answer:*
The F-test of excluded instruments is equal to 346.96, indicating that the 
weak instrument is relevant and far above the F-statistic of 10 used as a rule 
of thumb to ensure the instrument is not weak.  The underidentification test 
indicates that the regression has very good size properties.  The test of 
overidentifying restrictions can not be used for falsification of instrument 
validity here, since the test requires more than 1 instrument and only 1 is used.

*/

***** IV Regression with more than 1 instrument

ivreg2 hourswked_moth boy1st black_mother ///
		hisp_moth othrace_moth (morekids = boys2 girls2) if Main == 1

/*

*Question:*
When using two instruments (whether the first two children were both 
boys (boys2) or both girls (girl2) for having 3 or more children, 
what can you conclude from the test of overidentifying restrictions?

*Answer:*
For the test of overidentifying restrictions, we fail to reject the null 
hypothesis that the instruments are both exogenous (p=0.5721).  This does not 
mean that we conclude the instruments are valid, just that we do not find 
evidence against that assumption from this test.

Note that when running the IV regression with boys2 and girls2 as instruments,
I had to drop either boys1st or boys2nd becuase of collinearity with boys2.  
If I had not, then boys2 would have been omitted and I would have again a
regression with only one instrument, whether both children were girls.
*/
