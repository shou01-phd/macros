/*20241203 - this version output different exposure level
giv_help (or any other binary/categorical var) can be set to certain levels
this means the causal estimand of everything being exposed vs not can be obtained*/

%macro gstep3_int(input_ds= /*input data, include libname*/
				,waves=		/*list out waves AFTER baseline*/
				,set_var=	/*intervened variable*/
				,int_typ=	/*1-binary, 2-ordinal/categorical, 3-continuous*/
				,set_val=	/*setting the intervened var to a certain value, ex 0_1_2|0_1*/
				,yvar=		/*dependent variable: treatment for iptw, censor for ipcw*/
				,tf_cat=  	/*time-fixed categorical vars*/
				,tf_bc=		/*time-fixed binary or continuous vars*/
				,tv_cat=  	/*time-varying categorical vars*/
				,tv_lvl=	/*non-referent levels of tv_cat, ex 1_2|0_1*/
				,tv_bin=	/*time-varying binary vars*/
				,tv_con=  	/*time-varying continuous vars*/
				,all_wave=	/*whether exposed for all or some waves*/);

	%macro dummy; %mend dummy;

	%array(wave,values=&waves.);
	%array(tfc, values=&tf_cat.);
	%array(tfbc, values=&tf_bc.);
	/*will have to make it flexible: combine int_typ w/ corresponding var
	right now focus on giv_help which is categorical*/
	%array(tvm, values=&tv_cat. &set_var.);
	%array(tvi, values=&set_var., delim=|); /*intervened variable*/
	%array(tvv,	values=&set_val., delim=|); /*intervened levels*/
	%array(tvl, values=&tv_lvl. &set_val.);
	%array(tvb, values=&tv_bin.);
	%array(tvc, values=&tv_con.);
	%array(tvbcat, values=&tv_cat. &tv_bin.);
	%array(tv, values=&tv_cat. &tv_bin. &tv_con. &set_var.);

	%do i=1 %to &tvin.;
		%array(stl, values=&&tvv&i..,delim=_);
		%put intervened var = &&tvi&i..;
		%put intervened lvl = &&tvv&i..;
			
		%do s=1 %to &stln.;
			%put stln=&stln.;
			%put current level=&&stl&s..;
			%let int_lvl=&&stl&s..;
			
			/*loop through each wave*/
			%do w=1 %to &waven.;
				%let x = %sysevalf(&w.-1);
				%let waveno=&&wave&w..;
				
				%if &waveno.=1 %then %do;
					data &input_ds._s&int_lvl.;
						set &input_ds.;
						if &set_var.0 ne . then &set_var.0=&int_lvl.;
					run;

					%macro loop; %do m=1 %to &tvmn.; &&tvm&m..0 %end; %mend;
					%macro loop1; %do m=1 %to &tvln.; &&tvl&m.. %end; %mend; 
					%let loopvars=%loop;
					%let looplvls=%loop1;

					%makedummy(inds=&input_ds._s&int_lvl.,outds=&input_ds._s&int_lvl.
			  		,varnames=&loopvars.
			  		,levels=&looplvls.);
				%end;
				%else %do;
					data t.w&x._s&int_lvl.;
						set t.w&x._s&int_lvl.;
						if &set_var.&x. ne . then &set_var.&x.=&int_lvl.;
					run;
					%macro loop; %do m=1 %to &tvmn.; &&tvm&m..&x. %end; %mend;
					%macro loop1; %do m=1 %to &tvln.; &&tvl&m.. %end; %mend; 
					%let loopvars=%loop;
					%let looplvls=%loop1;

					%makedummy(inds=t.w&x._s&int_lvl.,outds=t.w&x._s&int_lvl.
			  		,varnames=&loopvars.
			  		,levels=&looplvls.);
				%end;


				/*loop through each tv continuous*/
				%do c=1 %to &tvcn.;
					%let con=&&tvc&c..;
					data p_&con.&waveno.;
						format dependent $9. parameter $13.;
						informat estimate 8.4;
						set p_&con.&waveno.;
						if parameter="Intercept" then int_flag=1;
						else int_flag=0;
						parlvl=compress(parameter, ,'s');

						*identify all the time-varying covariates;
						%if &waveno.=1 %then %do;
							if scan(parameter,1,' ')="&con.0" 
							%do g=1 %to &tvbcatn.; 
								| scan(parameter,1,' ')="&&tvbcat&g..0" 
							%end;
						%end;
						%else %do;
							if scan(parameter,1,' ')="&con.&&wave&x.." 
							%do g=1 %to &tvbcatn.; 
								| scan(parameter,1,' ')="&&tvbcat&g..&&wave&x.." 
							%end; 
						%end;
						then tv_flag=1;
						else tv_flag=0;
					run;

					proc sql noprint;
						select parlvl into :tv_g separated by ' ' from p_&con.&waveno. where int_flag=0 & probt ne . & tv_flag=1;
						select parlvl into :tf_g separated by ' ' from p_&con.&waveno. where int_flag=0 & probt ne . & tv_flag=0;
						select estimate into :tvest_g separated by ' ' from p_&con.&waveno. where int_flag=0 & probt ne . & tv_flag=1;
						select estimate into :tfest_g separated by ' ' from p_&con.&waveno. where int_flag=0 & probt ne . & tv_flag=0;
						select estimate into :int separated by ' ' from p_&con.&waveno. where int_flag=1;
					quit;
					%array(tvs,values=&tv_g.); %array(tve,values=&tvest_g.,numlist=N); 
					%array(tfs,values=&tf_g.); %array(tfe,values=&tfest_g.,numlist=N);

					data t.&con.&waveno._s&int_lvl.(keep=mergeid &con.&waveno. i);
						set	%if &waveno.=1 %then %do; &input_ds._s&int_lvl.(drop=&con.&waveno.) %end; %else %do; t.w&x._s&int_lvl. %end;;
						
						if censor&x.=0 then 
						&con.&waveno.=rand("normal",intercept*&int.
							
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

				/*loop through each tv categorical*/
				%do m=1 %to &tvmn.;
					%let cat=&&tvm&m..;
					%let l_cat=l_&cat.;
					%array(&l_cat., values=&&tvl&m.., delim=_);
					%let l_catn=&&&l_cat.n.;
					%do p=1 %to &l_catn.;
						%put level=&&&l_cat&p..;
					%end;
					
					proc logistic inmodel=om_&cat.&waveno.;
						score data= 
						%if &waveno.=1 %then %do; 
							&input_ds._s&int_lvl. 
						%end; 
						%else %do; 
							t.w&x._s&int_lvl. 
						%end;
						out=t.pom_&cat.&waveno._s&int_lvl.(keep=mergeid p_: censor: i);
					run;
					
					data t.&cat.&waveno._s&int_lvl.(keep=mergeid i &cat.&waveno.);
						set t.pom_&cat.&waveno._s&int_lvl.;
						call streaminit(886);
						if censor&x.=0 then &cat.&waveno.=rand('table'
																%do p=1 %to &l_catn.; 
																	,p_&&&l_cat&p..
																%end;);
						else &cat.&waveno.=.;
					run;
				%end;

				/*loop through each tv binary*/
				%do n=1 %to &tvbn.;
					%let bin=&&tvb&n..;
					proc logistic inmodel=om_&bin.&waveno.;
						score data=						
						%if &waveno.=1 %then %do; 
							&input_ds._s&int_lvl. 
						%end; 
						%else %do; 
							t.w&x._s&int_lvl. 
						%end;
 						out=t.pom_&bin.&waveno._s&int_lvl.(keep=mergeid p_: censor: i);
					run;
					
					data t.&bin.&waveno._s&int_lvl.(keep=mergeid i &bin.&waveno.);
						set t.pom_&bin.&waveno._s&int_lvl.;
						call streaminit(886);
						if censor&x.=0 then &bin.&waveno.=rand('table',p_0,p_1)-1;
						else &bin.&waveno.=.;
					run;
				%end;

				/*predict y*/
				proc logistic inmodel=om_&yvar.&waveno.;
					score data=
					%if &waveno.=1 %then %do; 
						&input_ds._s&int_lvl. 
					%end; 
					%else %do; 
						t.w&x._s&int_lvl. 
					%end;
					out=t.pom_&yvar.&waveno._s&int_lvl.(keep=mergeid p_: censor: i);
				run;
				data t.&yvar.&waveno._s&int_lvl.(keep=mergeid i &yvar.&waveno.);
					set t.pom_&yvar.&waveno._s&int_lvl.;
					call streaminit(886);
					if censor&x.=0 then &yvar.&waveno.=rand('table',p_0,p_1)-1;
					else &yvar.&waveno.=.;
				run;
				
				/*merge all the separate datasets: 
				- make keep vars flexible to include only tf covars*/
				data t.w&waveno._s&int_lvl.;
					merge 
						%if &waveno.=1 %then %do; &input_ds._s&int_lvl.(keep=mergeid i country: gender yedu nchild censor: intercept giv_help0) %end;
						%else %do; t.w&x._s&int_lvl.(keep=mergeid i country: gender yedu nchild censor: intercept) %end;
					%do q=1 %to &tvn.; t.&&tv&q..&waveno._s&int_lvl. %end;
					t.&yvar.&waveno._s&int_lvl.;
					by mergeid i;
					giv_help&waveno.=giv_help&waveno.-1;
				run;
			%end;
		%end;
	%end;
%mend;

%gstep3_int(input_ds=test 
		 ,waves=1 2 3
		 ,set_var=giv_help
		 ,set_val=0_1_2
		 ,yvar=eurodcat
		 ,tf_cat=country 
		 ,tf_bc=gender yedu nchild
		 ,tv_cat=jobs
		 ,tv_lvl=1_2_3_4
		 ,tv_bin=rec_help /*mstatb psych_med adlb*/
		 ,tv_con=logthinc age);

/*causal estimand is the mean value of eurodcat*/
proc means data=sharem min max mean; 
	title "observed data"; 
	var giv_help0 giv_help1 giv_help2 eurodcat2 logthinc1 logthinc2; 
run;
proc means data=t.nc2 min max mean; 
	title "natural course"; 
	var giv_help0 giv_help1 giv_help2 eurodcat2 logthinc1 logthinc2; 
run;
proc means data=t.w2_s2 min max mean;
	title "set giv_help=2 at t=2"; 
	var giv_help2 eurodcat2 logthinc2; 
run;
proc means data=t.w2_s0 min max mean;
	title "set giv_help=0 at t=2"; 
	var giv_help2 eurodcat2 logthinc2; 
run;


proc means data=t.w3_s2 min max mean;
	title "set giv_help=2 at t=3"; 
	var giv_help3 eurodcat3 logthinc3; 
run;
proc means data=t.w3_s1 min max mean;
	title "set giv_help=1 at t=3"; 
	var giv_help3 eurodcat3 logthinc3; 
run;
proc means data=t.w3_s0 min max mean;
	title "set giv_help=0 at t=3"; 
	var giv_help3 eurodcat3 logthinc3; 
run;

