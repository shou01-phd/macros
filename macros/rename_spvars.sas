/*recode wave4 sp vars*/
%macro rename_spvars(lib=
					,dataname=);
	%macro dummy; %mend dummy;

	*use proc sql to save these variables in macro variables;
	proc sql noprint;
		*this pastes the variable names to create oldvar=newvar;
		SELECT NAME
		INTO :names_sn SEPARATED BY " "
		FROM dictionary.columns
		where libname=upcase("&lib.") and memname=upcase("&dataname.") and NAME like '%sn%';
		*this selects any variable that contains sn anywhere in the variable;
		*the % sign indicates wildcard;

		SELECT cats(NAME,"=",substr(NAME,1,length(NAME)-2))
		INTO :names_sp SEPARATED BY " "

		FROM dictionary.columns
		where libname=upcase("&lib.") and memname=upcase("&dataname.") and NAME like 'sp%sp';
		*this selects any variable that contains sp at the end of the variable;
	quit;

	%put &names_sn.;
	%put &names_sp.;

	%array(vars,values=&names_sp.);

	data &lib..&dataname.;
		set &lib..&dataname.;
		rename 
			%do v=1 %to &varsn.;
				&&vars&v..
			%end;;
	run;

	*recode the %sn variables: if sp019dxsn=1, then sp018=1;
	*if sp021dxsn=1 then sp020=1;
	data wave4_sp019 wave4_sp021;
		length mergeid $12.;
		length sp018_ sp018_r 8.;
		set &lib..&dataname.;
		array sp019sn {7} sp019d1sn sp019d2sn sp019d3sn sp019d4sn sp019d5sn sp019d6sn sp019d7sn;
		array sp021sn {7} sp021d1sn sp021d2sn sp021d3sn sp021d4sn sp021d5sn sp021d6sn sp021d7sn;
		array b_sp019 {7} _temporary_;
		array b_sp021 {7} _temporary_;
		
		sp018_r=sp018_;

		*to check if any variable in the array is 1;
		do i=1 to dim(sp019sn);
			b_sp019[i]=(sp019sn[i]=1);
		end;
		sp018_r=(sum(of b_sp019[*])>0);
		drop i;

		if sp018_r=1 and sp018_=0 then output wave4_sp019;

		do i=1 to dim(sp021sn);
			b_sp021[i]=(sp021sn[i]=1);
		end;
		sp020_r=(sum(of b_sp021[*])>0);
		drop i;

		if sp020_r=1 and sp020_=0 then output wave4_sp019; 
	run;

	data _NULL_;
		if 0 then set wave4_sp019 nobs=n;
		call symputx('nrows',n);
		stop;
	run;
	%put nobs=&nrows;
	*check code: this internal check needs work;
	/*%if &nobs > 0 %then %do;
	* code to process the data ;
	%end;
	%else %do;
	* code to run when input set is empty ;
	%end;*/
%mend;
