/*wave country age gender: mstat: cjs: adl: yedu: nchild:
rhfo: rhih: sphus: ghih: ghto: thinc: yreg: ypen: ydip:*/
%macro iptw(output_title=
		   ,data_in=
		   ,data_num=
		   ,data_denom=
		   ,var_tx=
		   ,var_time=
		   ,covars_class=
		   ,covars_cont=
		   ,var_num=
		   ,var_denom=);
	%macro dummy; %mend dummy;

	%let versiondate=%sysfunc(today(), yymmddn8.);
	%put &versiondate.;
	filename reg "C:\Users\Hoff\Documents\hou\mcgill\dissertation\results\iptw_&versiondate..pdf";
	
	ods pdf file = reg;
	proc logistic data=&data_in. descending;
		class &var_tx. &var_time. / param=ref;
		model &var_tx.=&var_time. / link=glogit;
		output out=&data_num. p=&var_num.;
	run;

	title "&output_title.";
	proc logistic data=&data_in. descending;
		class &var_tx. &var_time. &covars_class. / param=ref;
		model &var_tx.=&var_time. &covars_class. &covars_cont. / link=glogit;
		output out=&data_denom. pred=&var_denom.;
	run;
	ods pdf close;
%mend;
/*
%macro iptw_svyweight(output_title=
					 ,data_in=
		   			 ,data_num=
					 ,data_denom=
					 ,var_tx=
		   			 ,var_time=
		   			 ,covars_class=
		   			 ,covars_cont=
					 ,var_stratum=
					 ,var_weight=
		   			 ,var_num=
		   			 ,var_denom=);
	%macro dummy; %mend dummy;

	%let versiondate=%sysfunc(today(), yymmddn8.);
	%put &versiondate.;
	filename reg "C:\Users\Hoff\Documents\hou\mcgill\dissertation\results\iptw_svy_&versiondate..pdf";
	*ODS PDF FILE = 'C:\Users\Hoff\Documents\hou\mcgill\dissertation\results\iptw_svy_&versiondate..pdf';
	ods pdf file = reg;
	proc surveylogistic data=&data_in. descending;
		stratum &var_stratum.;
		class &var_tx. &var_time. / param=ref;
		model &var_tx.=&var_time. / link=glogit;
		weight &var_weight.;
		output out=&data_num. p=&var_num.;
	run;

	title "&output_title.";
	proc surveylogistic data=&data_in. descending;
		stratum &var_stratum.;
		class &var_tx. &var_time. &covars_class. / param=ref;
		model &var_tx.=&var_time. &covars_class. &covars_cont. / link=glogit;
		weight &var_weight.;
		output out=&data_denom. pred=&var_denom.;
	run;
	ods pdf close;
%mend;
*/
