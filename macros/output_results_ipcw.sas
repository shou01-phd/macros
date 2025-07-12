%macro output_results_ipcw(filecode=
						   ,tf_cat=
						   ,tf_ref=
						   ,tf_bc=
						   ,tv_cat=
						   ,tv_ref=
						   ,tv_bc=
						   ,weight_types=);
	%macro dummy; %mend dummy;

	%let versiondate=%sysfunc(today(), yymmddn8.);
	%put &versiondate.;
	filename reg_ipcw "C:\Users\Hoff\Documents\hou\mcgill\dissertation\results\ipcw_&filecode._&versiondate..pdf";
	ods pdf file = reg_ipcw;
	%array(tfc, values=&tf_cat.);
	%array(tfr, values=&tf_ref., delim=|);
	%array(tfbc, values=&tf_bc.);
	%array(tvc, values=&tv_cat.);
	%array(tvr, values=&tv_ref., delim=|);
	%array(tvbc, values=&tv_bc.);

	title "Numerator P(Ck=0|Ck-1)";
	proc logistic data=share1_ipcw;
		ods exclude ClassLevelInfo modelanova Association FitStatistics GlobalTests;
		class wave (ref="2") 
			  /*time-fixed or baseline categorical*/
			  %do a=1 %to &tfcn.; &&tfc&a.. (ref="&&tfr&a..")	%end; / param=ref;
		model censor=wave 
			  /*time-fixed or baseline categorical*/
			  %do a=1 %to &tfcn.; &&tfc&a.. %end;
			  /*time-fixed or basline continuous or binary*/
			  %do b=1 %to &tfbcn.; &&tfbc&b.. %end; ; 
		output out=num1c (keep=mergeid wave _level_ num) p=num;
	run;

	title "Denominator P(Ck=0|Ck-1=0,Ak,Ak-1,Lk,Z0)";
	proc logistic data=share1_ipcw; 
		ods exclude ClassLevelInfo modelanova Association FitStatistics GlobalTests;
		class wave (ref="2") 
			  /*time-fixed or baseline categorical*/
			  %do a=1 %to &tfcn.; &&tfc&a.. (ref="&&tfr&a..") %end;
			  /*time-varying categorical*/
			  %do c=1 %to &tvcn.; &&tvc&c.. (ref="&&tvr&c..") %end; / param=ref;
		model censor=wave 
			  /*time-fixed or baseline categorical*/
			  %do a=1 %to &tfcn.; &&tfc&a.. %end;
			  /*time-fixed or basline continuous or binary*/
			  %do b=1 %to &tfbcn.; &&tfbc&b.. %end; 
			  /*time-varying categorical*/
			  %do c=1 %to &tvcn.; &&tvc&c.. %end; 
			  /*time-varying continuous or binary*/
			  %do d=1 %to &tvbcn.; &&tvbc&d.. %end; ; 
		output out=denom1c (keep=mergeid wave _level_ denom) pred=denom;
	run;
	title;

	*merged dataset that includes numerator and denominator to calculate weights;
	data merged1c;
		merge share1_ipcw (keep=mergeid wave censor 
			  /*time-fixed or baseline categorical*/
			  %do a=1 %to &tfcn.; &&tfc&a.. %end;
			  /*time-fixed or basline continuous or binary*/
			  %do b=1 %to &tfbcn.; &&tfbc&b.. %end; 
			  /*time-varying categorical*/
			  %do c=1 %to &tvcn.; &&tvc&c.. %end; 
			  /*time-varying continuous or binary*/
			  %do d=1 %to &tvbcn.; &&tvbc&d.. %end; ) 
			  denom1c (keep=mergeid wave denom _level_) 
			  num1c (keep=mergeid wave num);
		by mergeid wave;

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
	run;
	
	title "Distribution of weights";
	proc means data=merged1c;
		var uw sw;
	run;

	proc univariate data=merged1c(where=(wave ne 8));
		var uw sw;
		output out=percentile1c
		pctlpts=0.5 1 99 99.5 pctlpre=puw_ psw_;
	run;

	proc sql noprint;
		select puw_1 into :puw1c separated by ' ' from percentile1c;
		select puw_99 into :puw99c separated by ' ' from percentile1c;
		select psw_1 into :psw1c separated by ' ' from percentile1c;
		select psw_99 into :psw99c separated by ' ' from percentile1c;
	quit;
	%put puw1=&puw1c. puw99=&puw99c. psw1=&psw1c. psw99=&psw99c.;

	data merged1c;
		set merged1c;

		uw_t=uw; 
		if uw>&puw99c. & uw ne . then uw_t=&puw99c.;
		else if uw<&puw1c. & uw ne . then uw_t=&puw1c.;

		sw_t=sw;
		if sw>&psw99c. & sw ne . then sw_t=&psw99c.;
		else if sw<&psw1c. & sw ne . then sw_t=&psw1c.;
	run;

	%array(weighttype, values=&weight_types.);

	*All OR estimates should be around 1;
	%do w=1 %to &weighttypen.;
		title "Logistic reg estimates with weights (&&weighttype&w..)";
		proc logistic data=merged1c;
			ods exclude ClassLevelInfo modelanova Association FitStatistics GlobalTests;
			class wave (ref="2") 
				  /*time-fixed or baseline categorical*/
				  %do a=1 %to &tfcn.; &&tfc&a.. (ref="&&tfr&a..") %end;
				  /*time-varying categorical*/
				  %do c=1 %to &tvcn.; &&tvc&c.. (ref="&&tvr&c..") %end; / param=ref;
			model censor=wave 
				  /*time-fixed or baseline categorical*/
				  %do a=1 %to &tfcn.; &&tfc&a.. %end;
				  /*time-fixed or basline continuous or binary*/
				  %do b=1 %to &tfbcn.; &&tfbc&b.. %end; 
				  /*time-varying categorical*/
				  %do c=1 %to &tvcn.; &&tvc&c.. %end; 
				  /*time-varying continuous or binary*/
				  %do d=1 %to &tvbcn.; &&tvbc&d.. %end; ; 
			weight &&weighttype&w..; *loop through the 6 different weights (original covars);
		run;
	%end;

	title;
	ods pdf close;
%mend;
