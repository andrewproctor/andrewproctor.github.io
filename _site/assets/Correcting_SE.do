*** Preliminaries 
 
 cd "\\VICOMTE.stu.hhs.se\Usr01$\65472\Documents\IV Seminar" // set working dir
 
 use "card.dta" , clear // load data
 set type double, permanently // Use high precision numbers
 
 	/* In the following code, I want to save the coefficients, degrees of freedom
	and standard deviation estimates for beta and the error term.
	
	To do this, I combine using:
	 - Using the command "scalar" to generate new scalar values instead of 
		variables.
	 - Different outputs produced by regression when running a regression.  To 
	 see what a regression information is stored, I can use the command:
	 
			ereturn list
	 - I could also see what is stored from a summary command using the similar 
	 command:
	 
			return list
			
	- A couple of the most important data found in the list are the estimates 
	of the coefficients and standard errors, stored as matrices.  I can view this 
	matrices by using:
		
	  matrix list e(b)
	  matrix list e(V) 
	*/
 
 ***** Regression by Hand
 
	*** First Stage
	reg educ nearc4
	predict educ_hat, xb // Save predicted values "xb"

	*** 2nd Stage Regression using fitted values
	reg wage educ_hat

	scalar beta0 =  _b[_cons]
	scalar beta1_educ = _b[educ_hat]
	scalar sigmau_2ndst = e(rmse)
	scalar sigma_beta1_2ndst = _se[educ_hat]
	scalar degrees_freedom = e(df_r)

*** Correct Standard Errors

	** Error assumed for structural Equation
	gen correct_error = wage - beta0 - beta1_educ * educ
	gen double error_sq = correct_error^2
	
	*** Save output from list
	summarize error_sq
	*return list
		scalar sampsize = r(N) // Need only for small-sample correction
		scalar sum_sqerror = r(sum)  // Sums the squared error variable
		scalar sigmasq_u = sum_sqerror / sampsize // Sigma^2 = (Sum of Squared Errors) / N

	/* To allow for a degrees of freedom adjustment 
		(Sum of Squared Err / (N - k)), 
	
		I could use either:

			scalar sigmasq_u = sum_sqerror / degrees_freedom 

		Or:
			* Assuming 1 x variable and the constant
			scalar sigmasq_u = sum_sqerror / (sampsize - 2)  

		Then I would compare against ivreg2 using the "small" option.
	*/

scalar sigma_u = sqrt(sigmasq_u)
scalar sigma_beta1 = sigma_beta1_2ndst * (sigma_u / sigmau_2ndst) 

ivreg2 wage (educ = nearc4)
di sigma_beta1
