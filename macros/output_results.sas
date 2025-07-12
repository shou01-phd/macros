%macro output_results(filepath=	/*output file path*/
					 ,filecode=	/*code to distinguish output file*/
					 ,input_ds= /*input data, include libname*/
					 ,yvar=		/*dependent variable: treatment for iptw, censor for ipcw*/
					 ,multinom= /*multinomial logistic (yvar has more than 2 categories)*/
					 ,tf_cat=  	/*time-fixed categorical vars*/
					 ,tf_ref=  	/*reference group for time-fixed categorical vars*/
					 ,tf_bc=	/*time-fixed binary or continuous vars*/
					 ,tv_cat=  	/*time-varying categorical vars*/
					 ,tv_ref=  	/*reference group for time-varying categorical vars*/
					 ,tv_bc=   	/*time-varying binary or continuous vars*/
					 ,weight_types=/*different weights*/);
	%macro dummy; %mend dummy;

	%let versiondate=%sysfunc(today(), yymmddn8.);
	%put &versiondate.;
	ods pdf file="&filepath.\&filecode._&versiondate..pdf";
	%array(tfc, values=&tf_cat.);
	%array(tfr, values=&tf_ref., delim=|);
	%array(tfbc, values=&tf_bc.);
	%array(tvc, values=&tv_cat.);
	%array(tvr, values=&tv_ref., delim=|);
	%array(tvbc, values=&tv_bc.);

	title "Numerator P(Ak)";
	proc logistic data=&input_ds.;
		ods exclude ClassLevelInfo modelanova Association FitStatistics GlobalTests;
		class wave (ref="2") 
			  /*time-fixed or baseline categorical*/
			  %do a=1 %to &tfcn.; &&tfc&a.. (ref="&&tfr&a..")	%end; / param=ref;
		model &yvar.=wave 
			  /*time-fixed or baseline categorical*/
			  %do a=1 %to &tfcn.; &&tfc&a.. %end;
			  /*time-fixed or basline continuous or binary*/
			  %do b=1 %to &tfbcn.; &&tfbc&b.. %end; 
			  /*switch between multinomial or binomial*/
			  %if &multinom.=1 %then %do; / link=glogit; %end;
			  %else %do; ; %end;
		output out=num1 (keep=mergeid wave _level_ num) p=num;
	run;

	title "Denominator P(Ak|Ak-1,Lk,Z0)";
	proc logistic data=&input_ds.; 
		ods exclude ClassLevelInfo modelanova Association FitStatistics GlobalTests;
		class wave (ref="2") 
			  /*time-fixed or baseline categorical*/
			  %do a=1 %to &tfcn.; &&tfc&a.. (ref="&&tfr&a..") %end;
			  /*time-varying categorical*/
			  %do c=1 %to &tvcn.; &&tvc&c.. (ref="&&tvr&c..") %end; / param=ref;
		model &yvar.=wave 
			  /*time-fixed or baseline categorical*/
			  %do a=1 %to &tfcn.; &&tfc&a.. %end;
			  /*time-fixed or basline continuous or binary*/
			  %do b=1 %to &tfbcn.; &&tfbc&b.. %end; 
			  /*time-varying categorical*/
			  %do c=1 %to &tvcn.; &&tvc&c.. %end; 
			  /*time-varying continuous or binary*/
			  %do d=1 %to &tvbcn.; &&tvbc&d.. %end; 

			  %if &multinom.=1 %then %do; / link=glogit; %end;
			  %else %do; ; %end;
		output out=denom1 (keep=mergeid wave _level_ denom) p=denom;
	run;

	title;

	*merged dataset that includes numerator and denominator to calculate weights;
	data &input_ds._m;
		merge &input_ds. (keep=mergeid wave &yvar. eurodcat
			  /*time-fixed or baseline categorical*/
			  %do a=1 %to &tfcn.; &&tfc&a.. %end;
			  /*time-fixed or basline continuous or binary*/
			  %do b=1 %to &tfbcn.; &&tfbc&b.. %end; 
			  /*time-varying categorical*/
			  %do c=1 %to &tvcn.; &&tvc&c.. %end; 
			  /*time-varying continuous or binary*/
			  %do d=1 %to &tvbcn.; &&tvbc&d.. %end; ) 
			  denom1 num1;
		by mergeid wave;

		/*if multinomial, match by level*/
		%if &multinom.=1 %then %do;
			if &yvar.=_level_ then output;
		%end;
	run;

	data &input_ds._m;
		set &input_ds._m;
		by mergeid wave;
		
		%if &multinom.=1 %then %do;
			retain ipw_n ipw_d;
			if first.mergeid & denom ne . then do;
				ipw_n=1;
				ipw_d=1;
			end;
			ipw_n=ipw_n*num;
			ipw_d=ipw_d*denom;

			uw=1/ipw_d;
			sw=ipw_n/ipw_d;

			*for std mean diff comparison, recode exp to binary;
			if &yvar.=0 then &yvar._02=0;
			else if &yvar.=2 then &yvar._02=1;
			if &yvar.=0 then &yvar._01=0;
			else if &yvar.=1 then &yvar._01=1;
		%end;

		%else %do;
			*Combining time and weight specification the same way as IPTW;
			retain ipcw_n ipcw_d;
			if first.mergeid then do;
				ipcw_n=1;
				ipcw_d=1;
			end;
			ipcw_n=ipcw_n*num;
			ipcw_d=ipcw_d*denom;

			if censor=0 then do;
				uw=1/ipcw_d;
				sw=ipcw_n/ipcw_d;
			end;
			if censor=1 then do;
				uw=1/(1-ipcw_d);
				sw=(1-ipcw_n)/(1-ipcw_d);
			end;
		%end;

		*compare job status;
		if prev_jobs=2 then prev_jobs_b=1;
		else prev_jobs_b=0;
	run;
		
	proc univariate data=&input_ds._m;
		var uw sw;
		output out=percentile1
		pctlpts=0.5 1 99 99.5 pctlpre=puw_ psw_;
	run;

	proc sql noprint;
		select puw_1 into :puw1 separated by ' ' from percentile1;
		select puw_99 into :puw99 separated by ' ' from percentile1;
		select psw_1 into :psw1 separated by ' ' from percentile1;
		select psw_99 into :psw99 separated by ' ' from percentile1;
	quit;
	%put puw1=&puw1. puw99=&puw99. psw1=&psw1. psw99=&psw99.;

	data &input_ds._m;
		set &input_ds._m;

		uw_t=uw; 
		if uw>&puw99. & uw ne . then uw_t=&puw99.;
		else if uw<&puw1. & uw ne . then uw_t=&puw1.;

		sw_t=sw;
		if sw>&psw99. & sw ne . then sw_t=&psw99.;
		else if sw<&psw1. & sw ne . then sw_t=&psw1.;
	run;

	%array(weighttype, values=&weight_types.);

	title "Distribution of weights";
	proc means data=&input_ds._m;
		var %do w=1 %to &weighttypen.; &&weighttype&w.. %end; ;
	run;

	*All OR estimates should be around 1;
	%do w=1 %to &weighttypen.;
		title "Logistic reg estimates with weights (&&weighttype&w..)";
		proc logistic data=&input_ds._m;
			ods exclude ClassLevelInfo modelanova Association FitStatistics GlobalTests;
			class wave (ref="2") 
				  /*time-fixed or baseline categorical*/
				  %do a=1 %to &tfcn.; &&tfc&a.. (ref="&&tfr&a..") %end;
				  /*time-varying categorical*/
				  %do c=1 %to &tvcn.; &&tvc&c.. (ref="&&tvr&c..") %end; / param=ref;
			model &yvar.=wave 
				  /*time-fixed or baseline categorical*/
				  %do a=1 %to &tfcn.; &&tfc&a.. %end;
				  /*time-fixed or basline continuous or binary*/
				  %do b=1 %to &tfbcn.; &&tfbc&b.. %end; 
				  /*time-varying categorical*/
				  %do c=1 %to &tvcn.; &&tvc&c.. %end; 
				  /*time-varying continuous or binary*/
				  %do d=1 %to &tvbcn.; &&tvbc&d.. %end; 

				  %if &multinom.=1 %then %do; / link=glogit; %end;
				  %else %do; ; %end;
			weight &&weighttype&w..; *loop through the 6 different weights (original covars);
		run;
	%end;

	title;
	ods pdf close;
%mend;
