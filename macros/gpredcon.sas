/*This macro organizes and then predicts continuous variables*/

*run the data step to simulate logthinc1;
%macro gpredcon(waves=
				,tv_cont=
				,tv_bcat=
				,set=
				,setvalues=/*exposure variable value*/);
	%macro dummy; %mend dummy;
	%array(wave,values=&waves.);
	%array(tvcon,values=&tv_cont.);
	*%array(lvls,values=&setvalues.,delim=_);
	%array(tvbcat,values=&tv_bcat.);

	%if &set.=1 %then %do;
		%do l=1 %to &lvlsn.;
			%let lvl=&&lvls&l..;
			
		/*set exposure variable to value specified in "set="*/
			data t.giv_help_l&lvl.;
				set boot;

				giv_help0=&lvl.;
				giv_help1=&lvl.;
				%do w=2 %to &waven.;
					%let x = %sysevalf(&w.-1);
					if censor&&wave&x..=0 then giv_help&&wave&w..=&lvl.;
				%end;
			run;
		%end;
	%end;

	%do w=1 %to &waven.;
		%let x = %sysevalf(&w.-1);
		%let waveno=&&wave&w..;

		%do c=1 %to &tvconn.;
			%let tvc=&&tvcon&c..;
			
			%if &waveno.=1 %then %do;
				data p_&tvc.&waveno.;
					format dependent $9. parameter $13.;
					informat estimate 8.4;
					set p_&tvc.&waveno.;
					if parameter="Intercept" then int_flag=1;
					else int_flag=0;
					parlvl=compress(parameter, ,'s');

					*identify all the time-varying covariates;
					if scan(parameter,1,' ')="&tvc.0" 
					%do g=1 %to &tvbcatn.; 
						| scan(parameter,1,' ')="&&tvbcat&g..0" 
					%end;
					then tv_flag=1;
					else tv_flag=0;
				run;
			%end;

			%else %do;
				data p_&tvc.&waveno.;
					format dependent $9. parameter $13.;
					informat estimate 8.4;
					set p_&tvc.&waveno.;
					if parameter="Intercept" then int_flag=1;
					else int_flag=0;
					parlvl=compress(parameter, ,'s');

					*identify all the time-varying covariates;
					if scan(parameter,1,' ')="&tvc.&&wave&x.." 
					%do g=1 %to &tvbcatn.; 
						| scan(parameter,1,' ')="&&tvbcat&g..&&wave&x.." 
					%end; 
					then tv_flag=1;
					else tv_flag=0;
				run;
			%end;

			proc sql noprint;
				select parlvl into :tv_g separated by ' ' from p_&tvc.&waveno. where int_flag=0 & probt ne . & tv_flag=1;
				select parlvl into :tf_g separated by ' ' from p_&tvc.&waveno. where int_flag=0 & probt ne . & tv_flag=0;
				select estimate into :tvest_g separated by ' ' from p_&tvc.&waveno. where int_flag=0 & probt ne . & tv_flag=1;
				select estimate into :tfest_g separated by ' ' from p_&tvc.&waveno. where int_flag=0 & probt ne . & tv_flag=0;
				select estimate into :int separated by ' ' from p_&tvc.&waveno. where int_flag=1;
			quit;
			%array(tvs,values=&tv_g.); %array(tve,values=&tvest_g.,numlist=N); 
			%array(tfs,values=&tf_g.); %array(tfe,values=&tfest_g.,numlist=N);

			/*question for jee won: should i include tf vars in final causal estimand
			calculations or in this simulation step*/
			data t.&tvc.&waveno._nc(keep=mergeid &tvc.&waveno._nc i);
				set	%if &waveno.=1 %then %do; boot %end; %else %do; nc&x. %end;;

				if censor&x.=0 then 
				&tvc.&waveno._nc=rand("normal",intercept*&int.
					
					%do f=1 %to &tfsn.;
						%let fcovar=&&tfs&f..;
						+&fcovar.*&&tfe&f..
					%end;
					%do v=1 %to &tvsn.;
						%let vcovar=&&tvs&v..;
						+&vcovar.*&&tve&v..
					%end;,0.015);
			run;
		%end;
	%end;
%mend;
*example: %gpredcon(waves=1,tv_cont=logthinc,tv_bcat=giv_help rec_help,set=0,setvalues=0_2);
%gpredcon(waves=1 2,tv_cont=logthinc,tv_bcat=giv_help rec_help,set=0);

