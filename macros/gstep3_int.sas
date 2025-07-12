/*20241203 - this version output different exposure level
giv_help (or any other binary/categorical var) can be set to certain levels
this means the causal estimand of everything being exposed vs not can be obtained
20241206 - this version should allow manipulation of income variable
20241219 - using the residuals outputted from ey to randomly generate predicted value
for continuous variables; will have to reflect on whether this is appropriate
20250103 - adding replicate for bootstrapping
20250225 - output working files into different libname for debugging and later contrast
			difficult to keep track otherwise*/

%macro gstep3_int(input_ds= /*input data with libname */
				,lib=		/*libname of output datasets*/
				,output_ds= /*output data name*/
				,waves=		/*list out waves AFTER baseline*/
				,set_var=	/*intervened binar/categorical variable*/
				,set_val=	/*setting the intervened var to a certain value, ex 0_1_2|0_1*/
				,set_inc=	/*intervening on income? 1/0*/
				,add_amt=	/*intervened by adding a set amount*/
				,logtrfm=
				,yvar=		/*dependent variable: treatment for iptw, censor for ipcw*/
				,tf_cat=  	/*time-fixed categorical vars*/
				,tf_bc=		/*time-fixed binary or continuous vars*/
				,tv_cat=  	/*time-varying categorical vars*/
				,tv_lvl=	/*non-referent levels of tv_cat, ex 1_2|0_1*/
				,tv_bin=	/*time-varying binary vars*/
				,tv_con=  	/*time-varying continuous vars*/
				,all_wave=	/*whether exposed for all or some waves*/
				,groupvar=  /*variable to group by (e.g., imputation or replicate)*/
				,deletefile=/*delete files saved in work to make room*/
				);

	%macro dummy; %mend dummy;

	%array(wave,values=&waves.);
	%array(tfc, values=&tf_cat.);
	%array(tfbc, values=&tf_bc.);
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
				
				/*prepare input dataset*/
				%if &waveno.=1 %then %do;
					data &input_ds._s&int_lvl.;
						set &input_ds.;
						if &set_var.0 ne . then &set_var.0=&int_lvl.;
						%if &set_inc.=1 %then %do;
							thinc0=thinc0+&add_amt.;
							logthinc0=log10(thinc0);
						%end;
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
					data &lib..w&x._s&int_lvl.;
						set &lib..w&x._s&int_lvl.;
						if &set_var.&x. ne . then &set_var.&x.=&int_lvl.;
						%if &set_inc.=1 %then %do;
							/*use this code when using log-transformed income var*/
							%if &logtrfm.=1 %then %do;
								thinc&x.=10**(logthinc&x.)+&add_amt.; 
								logthinc&x.=log10(thinc&x.+&add_amt.);
							%end;
							%else %do;
								thinc&x.=thinc&x.+&add_amt.;
							%end;
						%end;
					run;
					%macro loop; %do m=1 %to &tvmn.; &&tvm&m..&x. %end; %mend;
					%macro loop1; %do m=1 %to &tvln.; &&tvl&m.. %end; %mend; 
					%let loopvars=%loop;
					%let looplvls=%loop1;

					%makedummy(inds=&lib..w&x._s&int_lvl.,outds=&lib..w&x._s&int_lvl.
			  		,varnames=&loopvars.
			  		,levels=&looplvls.);
				%end;

				/*loop through each tv continuous*/
				%do c=1 %to &tvcn.;
					%let con=&&tvc&c..;

					data &con.&waveno._s&int_lvl.(keep=mergeid &con.&waveno. i &groupvar.);
						merge t.p_&con.&waveno. &input_ds.(keep=mergeid i &groupvar.);
						by &groupvar. mergeid i;

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
					%do p=1 %to &l_catn.;
						%put level=&&&l_cat&p..;
					%end;
					
					proc logistic inmodel=t.om_&cat.&waveno.;
						score data= 
						%if &waveno.=1 %then %do; 
							&input_ds._s&int_lvl. 
						%end; 
						%else %do; 
							&lib..w&x._s&int_lvl. 
						%end;
						out=pom_&cat.&waveno._s&int_lvl.(keep=mergeid p_: censor: i &groupvar.);

						by &groupvar.;
					run;
					
					data &cat.&waveno._s&int_lvl.(keep=mergeid i &cat.&waveno. &groupvar.);
						set pom_&cat.&waveno._s&int_lvl.;
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

					proc logistic inmodel=t.om_&bin.&waveno.;
						score data=						
						%if &waveno.=1 %then %do; 
							&input_ds._s&int_lvl. 
						%end; 
						%else %do; 
							&lib..w&x._s&int_lvl. 
						%end;
 						out=pom_&bin.&waveno._s&int_lvl.(keep=mergeid p_: censor: i &groupvar.);

						by &groupvar.;
					run;
					
					data &bin.&waveno._s&int_lvl.(keep=mergeid i &bin.&waveno. &groupvar.);
						set pom_&bin.&waveno._s&int_lvl.;
						call streaminit(886);
						if censor&x.=0 then &bin.&waveno.=rand('table',p_0,p_1)-1;
						else &bin.&waveno.=.;
					run;
				%end;

				/*predict y*/
				proc logistic inmodel=t.om_&yvar.&waveno.;
					score data=
					%if &waveno.=1 %then %do; 
						&input_ds._s&int_lvl. 
					%end; 
					%else %do; 
						&lib..w&x._s&int_lvl. 
					%end;
					out=pom_&yvar.&waveno._s&int_lvl.(keep=mergeid p_: censor: i &groupvar.);
					by &groupvar.;
				run;
				data &yvar.&waveno._s&int_lvl.(keep=mergeid i &yvar.&waveno. &groupvar.);
					set pom_&yvar.&waveno._s&int_lvl.;
					call streaminit(886);
					if censor&x.=0 then &yvar.&waveno.=rand('table',p_0,p_1)-1;
					else &yvar.&waveno.=.;
				run;
				
				/*merge all the separate datasets: 
				- make keep vars flexible to include only tf covars*/
				data &lib..w&waveno._s&int_lvl.;
					merge 
						%if &waveno.=1 %then %do; 
							&input_ds._s&int_lvl.(keep=&groupvar. mergeid i country: gender yedu nchild censor: intercept giv_help0) 
						%end;
						%else %do; 
							&lib..w&x._s&int_lvl.(keep=&groupvar. mergeid i country: gender yedu nchild censor: intercept) 
						%end;
						%do q=1 %to &tvn.; 
							&&tv&q..&waveno._s&int_lvl. 
						%end;
						&yvar.&waveno._s&int_lvl.;

					by &groupvar. mergeid i;
					giv_help&waveno.=giv_help&waveno.-1;
				run;

				%if &deletefile.=1 %then %do;
					proc datasets nolist lib=work kill; quit;
				%end;
			%end;
		%end;
	%end;
%mend;
