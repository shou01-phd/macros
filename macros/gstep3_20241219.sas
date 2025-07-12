/*This version of gstep3 and gstep3_int outputs results that is used for 
preliminary analysis.
*/

%macro gstep3(input_ds= /*input data, include libname*/
			 ,waves=	/*list out waves AFTER baseline*/
			 ,yvar=		/*dependent variable: treatment for iptw, censor for ipcw*/
			 ,tf_cat=  	/*time-fixed categorical vars*/
			 ,tf_bc=	/*time-fixed binary or continuous vars*/
			 ,tv_cat=  	/*time-varying categorical vars*/
			 ,tv_lvl=	/*non-referent levels of tv_cat, ex 1_2|0_1*/
			 ,tv_bin=	/*time-varying binary vars*/
			 ,tv_con=  	/*time-varying continuous vars*/);

	%macro dummy; %mend dummy;

	%array(wave,values=&waves.);
	%array(tfc, values=&tf_cat.);
	%array(tfbc, values=&tf_bc.);
	%array(tvm, values=&tv_cat.);
	%array(tvl, values=&tv_lvl.);
	%array(tvb, values=&tv_bin.);
	%array(tvc, values=&tv_con.);
	%array(tvbcat, values=&tv_cat. &tv_bin.);
	%array(tv, values=&tv_cat. &tv_bin. &tv_con.);

	/*loop through each wave*/
	%do w=1 %to &waven.;
		%let x = %sysevalf(&w.-1);
		%let waveno=&&wave&w..;

		%if &waveno.=1 %then %do;
			/*use %loop and %loop1 to write %makedummy*/
			%macro loop; %do m=1 %to &tvmn.; &&tvm&m..0 %end; %mend;
			%macro loop1; %do m=1 %to &tvln.; &&tvl&m.. %end; %mend; 
			%let loopvars=%loop;
			%let looplvls=%loop1;

			%makedummy(inds=&input_ds.,outds=&input_ds.
	  		,varnames=&loopvars.
	  		,levels=&looplvls.);
		%end;
		%else %do;
			%macro loop; %do m=1 %to &tvmn.; &&tvm&m..&x. %end; %mend;
			%macro loop1; %do m=1 %to &tvln.; &&tvl&m.. %end; %mend; 
			%let loopvars=%loop;
			%let looplvls=%loop1;

			%makedummy(inds=t.w&x._snc,outds=t.w&x._snc
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

			data t.&con.&waveno._nc(keep=mergeid &con.&waveno. i);
				set	%if &waveno.=1 %then %do; &input_ds.(drop=&con.&waveno.) %end; %else %do; t.w&x._snc %end;;

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

			/*natural course*/
			proc logistic inmodel=om_&cat.&waveno.;
				score data=
				%if &waveno.=1 %then %do; &input_ds. %end; 
				%else %do; t.w&x._snc %end; 
				out=t.pom_&cat.&waveno._nc(keep=mergeid p_: censor: i);
			run;
			
			data t.&cat.&waveno._nc(keep=mergeid i &cat.&waveno.);
				set t.pom_&cat.&waveno._nc;
				call streaminit(886);
				if censor&x.=0 then &cat.&waveno.=rand('table'
														%do p=1 %to &l_catn.; 
															,p_&&&l_cat&p..
														%end;);
				else &cat.&waveno.=.;
			run;
		%end;

		/*use new simulated data to predict outcome:*/

		/*loop through each tv binary*/
		%do n=1 %to &tvbn.;
			%let bin=&&tvb&n..;

			proc logistic inmodel=om_&bin.&waveno.;
				score data=
				%if &waveno.=1 %then %do; &input_ds. %end; 
				%else %do; t.w&x._snc %end; 
				out=t.pom_&bin.&waveno._nc(keep=mergeid p_: censor: i);
			run;
			
			data t.&bin.&waveno._nc(keep=mergeid i &bin.&waveno.);
				set t.pom_&bin.&waveno._nc;
				call streaminit(886);
				if censor&x.=0 then &bin.&waveno.=rand('table',p_0,p_1)-1;
				else &bin.&waveno.=.;
			run;
		%end;

		/*simulate y*/
		data t.&yvar.&waveno._nc;
			set	%if &waveno.=1 %then %do; &input_ds. %end; %else %do; t.w&x._snc %end;;
		run;
		proc logistic inmodel=om_&yvar.&waveno.;
			score data=
			%if &waveno.=1 %then %do; &input_ds. %end; 
			%else %do; t.w&x._snc %end;
			out=t.pom_&yvar.&waveno._nc(keep=mergeid p_: censor: i);
		run;
		data t.&yvar.&waveno._nc(keep=mergeid i &yvar.&waveno.);
			set t.pom_&yvar.&waveno._nc;
			call streaminit(886);
			if censor&x.=0 then &yvar.&waveno.=rand('table',p_0,p_1)-1;
			else &yvar.&waveno.=.;
		run;

		/*merge all the separate datasets: 
		- make keep vars flexible to include only tf covars*/
		data t.w&waveno._snc;
			merge 
				%if &waveno.=1 %then %do; &input_ds.(keep=mergeid i country: gender yedu nchild censor: intercept eurodcat: giv_help0) %end;
				%else %do; t.w&x._snc(keep=mergeid i country: gender yedu nchild censor: intercept) %end;
			%do q=1 %to &tvn.; t.&&tv&q..&waveno._nc %end;
			t.&yvar.&waveno._nc;
			by mergeid i;
			giv_help&waveno.=giv_help&waveno.-1;
		run;
	%end;
%mend;
