/*20241219 - using the residuals outputted from ey to randomly generate predicted value
for continuous variables; will have to reflect on whether this is appropriate
20250103 - adding replicate for bootstrapping
20250127 - adding kill command and referencing input data in libname t
20250225 - update to include flexible libname*/

%macro gstep6(input_ds= /*input data with libname*/
			 ,output_ds=/*output data with libname*/
			 ,lib=		/*libname of output datasets*/
			 ,waves=	/*list out waves AFTER baseline*/
			 ,yvar=		/*dependent variable: treatment for iptw, censor for ipcw*/
			 ,tf_cat=  	/*time-fixed categorical vars*/
			 ,tf_bc=	/*time-fixed binary or continuous vars*/
			 ,tv_cat=  	/*time-varying categorical vars*/
			 ,tv_lvl=	/*non-referent levels of tv_cat, ex 1_2|0_1*/
			 ,tv_bin=	/*time-varying binary vars*/
			 ,tv_con=  	/*time-varying continuous vars*/
			 ,groupvar= /*variable to group by (e.g., imputation or replicate)*/
			 ,bootvar=  /*this should be replicate*/
			 ,deletefile=/*delete working files*/
			);

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
		%let waveno=&&wave&w..;
		%put time period=&waveno.;
		%let x = %sysevalf(&waveno.-1);
		%put prior wave = &x.;

		%if &w.=1 %then %do;
			/*use %loop and %loop1 to write %makedummy*/
			%macro loop; %do m=1 %to &tvmn.; &&tvm&m..&waveno. %end; %mend;
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

			%makedummy(inds=&lib..w&x._snc,outds=&lib..w&x._snc
	  		,varnames=&loopvars.
	  		,levels=&looplvls.);
		%end;

		/*loop through each tv continuous*/
		%do c=1 %to &tvcn.;
			%let con=&&tvc&c..;

			data &con.&waveno._nc(keep=mergeid &con.&waveno. i &groupvar. &bootvar.);
				merge t.p_&con.&waveno. &input_ds.(keep=mergeid i &groupvar. &bootvar.);
				by &groupvar. mergeid;

				p_resid=abs(p_resid);
				if censor&x.=0 then 
				&con.&waveno.=rand("normal",p_&con.&waveno.,p_resid);
			run;
		%end;

		/*loop through each tv categorical*/
		%do m=1 %to &tvmn.;
			%let cat=&&tvm&m..;
			%let l_cat=l_&cat.;
			%array(&l_cat., values=&&tvl&m.., delim=_);
			%let l_catn=&&&l_cat.n.;

			/*natural course*/
			proc logistic inmodel=t.om_&cat.&waveno.;
				score data=
				%if &waveno.=1 %then %do; &input_ds. %end; 
				%else %do; &lib..w&x._snc %end; 
				out=pom_&cat.&waveno._nc(keep=mergeid p_: censor: i &groupvar. &bootvar.);

				by &groupvar.;
			run;
			
			data &cat.&waveno._nc(keep=mergeid i &cat.&waveno. &groupvar. &bootvar.);
				set pom_&cat.&waveno._nc;
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

			proc logistic inmodel=t.om_&bin.&waveno.;
				score data=
				%if &waveno.=1 %then %do; &input_ds. %end; 
				%else %do; &lib..w&x._snc %end; 
				out=pom_&bin.&waveno._nc(keep=mergeid p_: censor: i &groupvar. &bootvar.);

				by &groupvar.;
			run;
			
			data &bin.&waveno._nc(keep=mergeid i &bin.&waveno. &groupvar. &bootvar.);
				set pom_&bin.&waveno._nc;
				call streaminit(886);
				if censor&x.=0 then &bin.&waveno.=rand('table',p_0,p_1)-1;
				else &bin.&waveno.=.;
			run;
		%end;

		/*simulate y*/
		proc logistic inmodel=t.om_&yvar.&waveno.;
			score data=
			%if &waveno.=1 %then %do; &input_ds. %end; 
			%else %do; &lib..w&x._snc %end;
			out=pom_&yvar.&waveno._nc(keep=mergeid p_: censor: i &groupvar. &bootvar.);
			by &groupvar.;
		run;
		data &yvar.&waveno._nc(keep=mergeid i &yvar.&waveno. &groupvar. &bootvar.);
			set pom_&yvar.&waveno._nc;
			call streaminit(886);
			if censor&x.=0 then &yvar.&waveno.=rand('table',p_0,p_1)-1;
			else &yvar.&waveno.=.;
		run;

		/*merge all the separate datasets: 
		- make keep vars flexible to include only tf covars*/
		data &lib..w&waveno._snc;
			merge 
				%if &waveno.=1 %then %do; 
					&input_ds.(keep=&groupvar. &bootvar. mergeid i country: gender yedu nchild censor: intercept eurodcat: giv_help0) 
				%end;
				%else %do; 
					&lib..w&x._snc(keep=&groupvar. &bootvar. mergeid i country: gender yedu nchild censor: intercept) 
				%end;
				%do q=1 %to &tvn.; 
					&&tv&q..&waveno._nc 
				%end;
				&yvar.&waveno._nc;

			by &groupvar. &bootvar. mergeid;
			giv_help&waveno.=giv_help&waveno.-1;
		run;
	%end;

	%if &deletefile.=1 %then %do;
		proc datasets nolist lib=work kill; quit;
	%end;

%mend;

