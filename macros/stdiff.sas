* STDIFF macro created by Dr. Joseph M. Caswell, 2018;

* Calculate standardized differences (d) of means and/or
	proportions between two groups;

* Required entry fields;
* Dataset = dataset containing variables;
* Group = the group variable (i.e., treatment or control) which must
	be coded as 0/1;
* At least one (or both) of the following must be included;
* Mean = list of variables for calculating differences of means
	(variables must be entered in a list separated by spaces like this:
		mean=var1 var2 var3 var4);
* Proportion = list of variables for calculating differences of proportions
	(variables must be entered in a list separated by spaces like this:
		proportion=var1 var2 var3 var4)
	Variables must be coded as 0/1;
* Optional field:
* W = weight variable (i.e., to weight the sample)
* subtitle = 'subtitle to describe details';

%macro stdiff(dataset=, group=, mean=, proportion=, w=, subtitle=);
	%macro dummy; %mend dummy;
* Check for required parameters;
%if %length(&dataset.)=0 or %length(&group.)=0 %then %do;
	%put ERROR: DATASET and GROUP are required parameters;
	%abort cancel;
%end;
%if %length(&mean.)=0 and %length(&proportion.)=0 %then %do;
	%put ERROR: At least one of MEAN and/or PROPORTION must be specified;
	%abort cancel;
%end;

* Create base blank dataset(s);
%if %length(&mean.) NE 0 %then %do;
	data _stdiff_mean_fin;
		set _null_;
	run;
%end;
%if %length(&proportion.) NE 0 %then %do;
	data _stdiff_prop_fin;
		set _null_;
	run;
%end;

* Identify number of mean variables;
* Set macro loop specification for mean variables;
* Scan string for variable names;
* Calculate d for each variable;
%if %length(&mean.) NE 0 %then %do;

	%let mean_cnt=%sysfunc(countw(&mean.));

	%do var_mean=1 %to &mean_cnt.;

		%let v_m=%scan(&mean., &var_mean.);

		proc means data=&dataset. noprint;
			class &group.;
			var &v_m.;
			%if %length(&w.) NE 0 %then %do;
				weight &w.;
			%end;
			output out=_mean_&v_m.;
		run;

		data _null_;
			set _mean_&v_m.;
			if &group.=0 and _STAT_='MEAN' then call symput("_mean0", &v_m.);
			else if &group.=0 and _STAT_='STD' then call symput("_sd0", &v_m.);
			else if &group.=1 and _STAT_='MEAN' then call symput("_mean1", &v_m.);
			else if &group.=1 and _STAT_='STD' then call symput("_sd1", &v_m.);
		run;

		data _stdiff_mean_&v_m.;
			length _var $10;
			_var="&v_m.";
			_mean0=&_mean0.;
			_sd0=&_sd0.;
			_mean1=&_mean1.;
			_sd1=&_sd1.;
			_d=(&_mean1.-&_mean0.)/(sqrt((&_sd1.**2+&_sd0.**2)/2));
			_d_pct=100*_d;
			label _var='Variable'
					_d='Standardized Difference (d)'
					_d_pct='Standardized Difference (d) in Percent (%)';
			drop _sd0 _sd1 _mean0 _mean1;
		run;

		data _stdiff_mean_fin;
			set _stdiff_mean_fin _stdiff_mean_&v_m.;
		run;
	%end;
%end;

* Identify number of proportion variables;
* Set macro loop specification for proportion variables;
* Scan string for variable names;
* Calculate d for each variable;
%if %length(&proportion.) NE 0 %then %do;

	%let prop_cnt=%sysfunc(countw(&proportion.));

	%do var_prop=1 %to &prop_cnt.;

		%let v_p=%scan(&proportion., &var_prop.);

		proc freq data=&dataset. noprint;
			table &v_p.*&group. /
			out=_prop_&v_p. outpct;
			%if %length(&w.) NE 0 %then %do;
				weight &w.;
			%end;
		run;

		data _null_;
			set _prop_&v_p.;
			if &group.=0 and &v_p.=1 then call symput("_prop0", pct_col);
			else if &group.=1 and &v_p.=1 then call symput("_prop1", pct_col);
		run;

		data _stdiff_prop_&v_p.;
			length _var $10;
			_var="&v_p.";
			_prop0=(&_prop0.)/100;
			_prop1=(&_prop1.)/100;
			_d=(_prop1-_prop0)/sqrt((_prop1*(1-_prop1)+_prop0*(1-_prop0))/2);
			_d_pct=100*_d;
			label _var='Variable'
					_d='Standardized Difference (d)'
					_d_pct='Standardized Difference (d) in Percent (%)';
			drop _prop0 _prop1;
		run;

		data _stdiff_prop_fin;
			set _stdiff_prop_fin _stdiff_prop_&v_p.;
		run;
	%end;
%end;

* Print results;
%if %length(&mean.) NE 0 %then %do;
	title 'Standardized Differences of Means';
	title3 &subtitle.;
	%if %length(&w.) NE 0 %then %do;
		title2 'Weighted Sample';
		title3 &subtitle.;
	%end;
	proc print data=_stdiff_mean_fin noobs label;
		var _all_;
	run;
	title;
	%if %length(&w.) NE 0 %then %do;
		title2;
		title3 &subtitle.;
	%end;
%end;
%if %length(&proportion.) NE 0 %then %do;
	title 'Standardized Differences of Proportions';
	title3 &subtitle.;
	%if %length(&w.) NE 0 %then %do;
		title2 'Weighted Sample';
		title3 &subtitle.;
	%end;
	proc print data=_stdiff_prop_fin noobs label;
		var _all_;
	run;
	title;
	%if %length(&w.) NE 0 %then %do;
		title2;
		title3 &subtitle.;
	%end;
%end;

* Delete leftover datasets;
proc datasets library=work noprint;
	delete _stdiff: 
	%if %length(&mean.) NE 0 %then %do;
		_mean_:
	%end;
	%if %length(&proportion.) NE 0 %then %do;
		_prop_:
	%end;
	;
run; quit;

%mend;
