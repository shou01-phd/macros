
/***********************************************************************/
/***********************TITLE: MACRO BOOT_DIFF**************************/
/*******************CREATED BY: SHERRY SHU-YEU HOU**********************/
/**********************LAST EDITED: 2023-10-12**************************/
/****PURPOSE: This macro prepares the output from the bootstrapped,*****/
/***multiply-imputed data from a logistical regression to obtain the****/
/**point estimate of prevalence difference and its confidence interval**/
/***********************************************************************/

%macro boot_diff(ds=);
/*input data: use the parameter output from regression
note: the input data should have all regression estimates for each
the MIs and bootstrap replicates.*/

	%macro dummy; %mend dummy;
	
	/*create a dummy variable level 0, important for later*/
	data &ds.;
		set &ds.;
		if classval0=. then classval0=0;
	run;
	
	/*extract variable and level into a macro variable*/
	data prep;
		set &ds.(where=(_imputation_=1 & replicate=1));
	run;

	proc sql noprint;
		select cats(variable,classval0) into :varlvls separated by ' ' from prep;
		select variable into :vars separated by ' ' from prep;
		select classval0 into :lvls separated by ' ' from prep;
	quit;
	%put &vars.;
	%put &lvls.;

	/*use the array macro to loop through each variable and level*/
	%array(beta,values=&vars.);
	%array(levels,values=&lvls.);

	data &ds.(drop=b:);
		set &ds.;
		by _imputation_ replicate;

		retain intercept;
		if variable="Intercept" then intercept=estimate;
		else intercept=intercept;
		
		/*loop through the variables and levels to create "beta" dummy variables*/
		%do b=2 %to &betan.;
				if variable="&&beta&b.." & classval0=&&levels&b.. then b&b.=1;
				else b&b.=0;
		%end;

		/*use the equation to transform estimates from a logistic regression to prevalence point estimates*/
		prob_pt=exp(intercept %do b=2 %to &betan.; +b&b.*estimate %end;)/(1+exp(intercept %do b=2 %to &betan.; +b&b.*estimate %end;));

		retain prob_int;
		if variable="Intercept" then prob_int=prob_pt;
		else prob_int=prob_int;
		
		/*prevalence difference*/
		prob_diff=prob_pt-prob_int;
	run;

	proc sql;
		/*create prevalence point estimate and SE by imputation*/
		create table &ds._ptprep as select
		_imputation_
		,variable
		,classval0
		,mean(prob_pt) as estimate
		,std(prob_pt) as stderr
		from &ds. group by _imputation_, variable, classval0;

		/*create prevalence difference estimate and SE by imputation*/
		create table &ds._pdprep as select
		_imputation_
		,variable
		,classval0
		,mean(prob_diff) as estimate
		,std(prob_diff) as stderr
		from &ds. group by _imputation_, variable, classval0;
	quit;

	%let versiondate=%sysfunc(today(), yymmddn8.);
	%put &versiondate.;
	*output results into pdf;
	ods pdf file="C:\Users\Sherry\Documents\share\results\msm_&versiondate..pdf";

	/*use the mianalyze step to correct for confidence interval for prevalence*/
	title "MSM: prevalence point estimate, weighted and corrected for MI";
	proc mianalyze parms(classvar=classval)=&ds._ptprep;
		class giv_help;
		modeleffects intercept giv_help;
		ods exclude modelinfo varianceinfo;
		ods output parameterestimates=mi_pt;
	run;

	/*use the mianalyze step to correct for confidence interval for prevalence difference*/
	title "MSM: prevalence difference, weighted and corrected for MI";
	proc mianalyze parms(classvar=classval)=&ds._pdprep;
		class giv_help;
		modeleffects giv_help;
		ods exclude modelinfo varianceinfo;
		ods output ParameterEstimates=mi_pd;
	run;
	title;
	ods pdf close;
%mend;
