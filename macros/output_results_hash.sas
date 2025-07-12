%macro output_results_hash(filepath=	/*output file path*/
					 ,filecode=	/*code to distinguish output file*/
					 ,diag=		/*whether to include reg output*/
					 ,input_ds= /*input data, include libname*/
					 ,output_ds=/*output data: with only individual levels and weights*/
					 ,desc=		/*"descending" for the outcome variable*/
					 ,yvar=		/*dependent variable: treatment for iptw, censor for ipcw*/
					 ,multinom= /*multinomial logistic (yvar has more than 2 categories)*/
					 ,tf_cat=  	/*time-fixed categorical vars*/
					 ,tf_ref=  	/*reference group for time-fixed categorical vars*/
					 ,tf_bc=	/*time-fixed binary or continuous vars*/
					 ,tv_cat=  	/*time-varying categorical vars*/
					 ,tv_ref=  	/*reference group for time-varying categorical vars*/
					 ,tv_bc=   	/*time-varying binary or continuous vars*/
					 ,groupvar= /*variable to group by (e.g., imputation)*/
					 ,testmode= /*test mode runs on 1 rep and has outputs*/
					 ,kill=		/*delete datasets in work folder*/);
	%macro dummy; %mend dummy;

	%let versiondate=%sysfunc(today(), yymmddn8.);
	%put &versiondate.;
	%if &diag.=1 %then %do; 
		ods pdf file="&filepath.\&filecode._&versiondate..pdf";
	%end;

	%array(tfc, values=&tf_cat.);
	%array(tfr, values=&tf_ref., delim=|);
	%array(tfbc, values=&tf_bc.);
	%array(tvc, values=&tv_cat.);
	%array(tvr, values=&tv_ref., delim=|);
	%array(tvbc, values=&tv_bc.);

	*merged dataset that includes numerator and denominator to calculate weights;
	data &input_ds._m;
		merge 		
		%if &testmode.=1 %then %do; test(keep=mergeid wave &yvar. &groupvar.) %end; 
		%else %do; &input_ds.(keep=mergeid wave &yvar. &groupvar.) %end;
		denom1 num1;

		by &groupvar. mergeid wave;

		/*if multinomial, match by level*/
		%if &multinom.=1 %then %do;
			if &yvar.=_level_ then output;
		%end;
	run;

	data &input_ds._m;
		set &input_ds._m;
		by &groupvar. mergeid wave;
		
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
	run;

	*output 1st and 99th percentile;
	proc univariate data=&input_ds._m;
		var uw sw;
		by &groupvar.;
		output out=percentile
		pctlpts=1 99 pctlpre=puw_ psw_;
	run;
	ods exclude none;

	/*merge the summary statistics (the 1 and 99th percentile)
	 with all observations and their respective weights*/
	data &output_ds.(keep=mergeid wave &yvar.: &groupvar. uw sw uw_t sw_t);
		drop rc;
		length psw_1 psw_99 puw_1 puw_99 8.;
		if _n_=1 then do;
			declare hash h(dataset:'work.percentile');
			h.definekey('_imputation_', 'replicate');
			h.definedata('psw_1', 'psw_99', 'puw_1', 'puw_99');
			h.definedone();
			call missing(psw_1, psw_99, puw_1, puw_99);
		end;
		set bootsample_m;
		rc=h.find();
		uw_t=uw;
		sw_t=sw;

	 /*truncate extreme weights by assigning the 1st and 99th %ile 
	weights to them*/
		if uw>puw_99 then uw_t=puw_99;
		if uw ne . & uw<puw_1 then uw_t=puw_1;
		if sw>psw_99 then sw_t=psw_99;
		if sw ne . & sw<psw_1 then sw_t=psw_1;
	run;

	%if &kill.=1 %then %do;
		proc datasets library=work nolist;
			delete num1 denom1 percentile &input_ds._m;
		quit;
	%end;

	%if &diag.=1 %then %do; 
		title "Distribution of weights";
		proc means data=&output_ds.;
			var uw uw_t sw sw_t;
		run;
	%end;
	title;
	ods pdf close;
%mend;
