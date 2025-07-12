/***ey macro includes the first steps of g-formula 
	by predicting the outcome with all the covariates
	step 1: E(Y | A, C, L)
	step 2: E(L | A, C)

This version of gstep3 and gstep3_int outputs results that is used for 
preliminary analysis.
***/

%macro ey(filepath=	/*output file path*/
					 ,filecode=	/*code to distinguish output file*/
					 ,input_ds= /*input data, include libname*/
					 ,waves=	/*list out waves AFTER baseline*/
					 ,yvar=		/*dependent variable: treatment for iptw, censor for ipcw*/
					 ,tf_cat=  	/*time-fixed categorical vars*/
					 ,tf_ref=  	/*reference group for time-fixed categorical vars*/
					 ,tf_bc=	/*time-fixed binary or continuous vars*/
					 ,tv_cat=  	/*time-varying categorical vars*/
					 ,tv_ref=  	/*reference group for time-varying categorical vars*/
					 ,tv_dec=	/*whether each categorical variable should be descending*/
					 ,tv_bin=	/*time-varying binary vars*/
					 ,tv_con=  	/*time-varying continuous vars*/);
	%macro dummy; %mend dummy;

	%let versiondate=%sysfunc(today(), yymmddn8.);
	%put &versiondate.;

	%array(wave,values=&waves.);
	%array(tfc, values=&tf_cat.);
	%array(tfr, values=&tf_ref., delim=|);
	%array(tfbc, values=&tf_bc.);
	%array(tvm, values=&tv_cat.);
	%array(tvr, values=&tv_ref., delim=|);
	%array(tvd, values=&tv_dec., delim=|);
	%array(tvb, values=&tv_bin.);
	%array(tvc, values=&tv_con.);

	%do w=1 %to &waven.;
		%let x = %sysevalf(&w.-1);
		%let waveno=&&wave&w..;
		%put time period=&waveno.;

		title "E(Y|A, C, L), Time=&waveno.";
		proc logistic data=&input_ds. descending outmodel=om_&yvar.&waveno.;
			ods exclude all;
			class
			  	  /*time-fixed or baseline categorical*/
			  	  %do a=1 %to &tfcn.; &&tfc&a.. (ref="&&tfr&a..") %end;
			  	  /*time-varying categorical*/
			  	  %do c=1 %to &tvmn.; &&tvm&c..&x. (ref="&&tvr&c..") %end; / param=ref;
			model &yvar.&waveno.= 
				  /*time-fixed or baseline categorical*/
				  %do a=1 %to &tfcn.; &&tfc&a.. %end;
				  /*time-fixed or basline continuous or binary*/
				  %do b=1 %to &tfbcn.; &&tfbc&b.. %end; 
				  /*time-varying categorical*/
			  	  %do c=1 %to &tvmn.; &&tvm&c..&x. %end; 
			  	  /*time-varying binary*/
			  	  %do d=1 %to &tvbn.; &&tvb&d..&x. %end; 
				  /*time-varying continuous*/
				  %do e=1 %to &tvcn.; &&tvc&e..&x. %end;;
			*ods output ParameterEstimates=p_&yvar.&waveno.;
		run;
	
		/*proc reg*/
		%do l=1 %to &tvcn.;
			%let con=&&tvc&l..;
			%put y=&con.&waveno.;

			title "E(L|A,C), Time=&&wave&w.., L=&con.&waveno.";
			proc glm data=&input_ds.;
				class
					/*time-fixed or baseline categorical*/
					%do a=1 %to &tfcn.; &&tfc&a.. (ref="&&tfr&a..") %end;
					/*time-varying categorical*/
					%do c=1 %to &tvmn.; &&tvm&c..&x. (ref="&&tvr&c..") %end;;
				 
				model &con.&waveno.= 
					  /*time-fixed or baseline categorical*/
					  %do a=1 %to &tfcn.; &&tfc&a.. %end;
					  /*time-fixed or basline continuous or binary*/
					  %do b=1 %to &tfbcn.; &&tfbc&b.. %end; 
					  /*time-varying categorical*/
				  	  %do c=1 %to &tvmn.; &&tvm&c..&x. %end; 
					  /*time-varying binary*/
				  	  %do d=1 %to &tvbn.; &&tvb&d..&x. %end; 
					  /*time-varying continuous*/
					  %do e=1 %to &tvcn.; &&tvc&e..&x. %end;  / solution;
				*output out=p_&con.&waveno. p=p_&con.&waveno.;
				ods output ParameterEstimates=p_&con.&waveno.;
			run;

			/*data p_&con.&waveno.;
				set p_&con.&waveno.;
				variable=scan(parameter,1,' ');
				classval0=scan(parameter,2,' ');
			run;*/
		%end;

		/*proc logistic for multinomial*/
		%do m=1 %to &tvmn.;
			%let cat=&&tvm&m..;
			%put y=&cat.&waveno.;

			title "E(L|A,C), Time=&&wave&w.., L=&cat.&waveno.";
			proc logistic data=&input_ds. 
				%if &&tvd&m..=1 %then %do; 
					descending outmodel=om_&cat.&waveno.
				%end;
				%else %do;
					outmodel=om_&cat.&waveno.
				%end;;
				ods exclude all;
				class
				  	  /*time-fixed or baseline categorical*/
				  	  %do a=1 %to &tfcn.; &&tfc&a.. (ref="&&tfr&a..") %end;
				  	  /*time-varying categorical*/
				  	  %do e=1 %to &tvmn.; &&tvm&e..&x. (ref="&&tvr&e..") %end; / param=ref;
				model &cat.&waveno.= 
					  /*time-fixed or baseline categorical*/
					  %do a=1 %to &tfcn.; &&tfc&a.. %end;
					  /*time-fixed or basline continuous or binary*/
					  %do b=1 %to &tfbcn.; &&tfbc&b.. %end; 
					  /*time-varying categorical*/
				  	  %do c=1 %to &tvmn.; &&tvm&c..&x. %end; 
					  /*time-varying binary*/
				  	  %do d=1 %to &tvbn.; &&tvb&d..&x. %end; 
					  /*time-varying continuous*/
					  %do e=1 %to &tvcn.; &&tvc&e..&x. %end;
					  / link=glogit;
				*ods output ParameterEstimates=p_&cat.&waveno.;
			run;
		%end;
		
		/*proc logistic for binomial*/
		%do n=1 %to &tvbn.;
			%let bin=&&tvb&n..;
			%put y=&bin.&waveno.;

			title "E(L|A,C), Time=&waveno., L=&bin.&waveno.";
			proc logistic data=&input_ds. outmodel=om_&bin.&waveno. desc;
				ods exclude all;
				class
				  	  /*time-fixed or baseline categorical*/
				  	  %do a=1 %to &tfcn.; &&tfc&a.. (ref="&&tfr&a..") %end;
				  	  /*time-varying categorical*/
				  	  %do e=1 %to &tvmn.; &&tvm&e..&x. (ref="&&tvr&e..") %end; / param=ref;
				model &bin.&waveno.= 
					  /*time-fixed or baseline categorical*/
					  %do a=1 %to &tfcn.; &&tfc&a.. %end;
					  /*time-fixed or basline continuous or binary*/
					  %do b=1 %to &tfbcn.; &&tfbc&b.. %end; 
					  /*time-varying categorical*/
				  	  %do c=1 %to &tvmn.; &&tvm&c..&x. %end; 
					  /*time-varying binary*/
				  	  %do d=1 %to &tvbn.; &&tvb&d..&x. %end; 
					  /*time-varying continuous*/
					  %do e=1 %to &tvcn.; &&tvc&e..&x. %end;;
				*ods output ParameterEstimates=p_&bin.&waveno;
			run;
		%end;
	%end;
	title;
	ods exclude none;
%mend;

