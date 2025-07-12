%macro gstep3(input_ds= /*input data, include libname*/
			 ,waves=	/*list out waves AFTER baseline*/
			 ,nat_cor=	/*whether to run natural course*/
			 ,yvar=		/*dependent variable: treatment for iptw, censor for ipcw*/
			 ,tf_cat=  	/*time-fixed categorical vars*/
			 ,tf_bc=	/*time-fixed binary or continuous vars*/
			 ,tv_cat=  	/*time-varying categorical vars*/
			 ,tv_lvl=	/*non-referent levels of tv_cat, ex 1_2|0_1*/
			 ,tv_bin=	/*time-varying binary vars*/
			 ,tv_con=  	/*time-varying continuous vars*/
			 ,all_wave= /*whether exposed for all or some waves*/);

	%macro dummy; %mend dummy;

	%array(wave,values=&waves.);
	%array(tfc, values=&tf_cat.);
	%array(tfbc, values=&tf_bc.);
	%array(tvm, values=&tv_cat.);
	%array(tvl, values=&tv_lvl., delim=|);
	%array(tvb, values=&tv_bin.);
	%array(tvc, values=&tv_con.);
	%array(tv, values=&tv_cat. &tv_bin.);
	
	/*loop through each wave*/
	%do w=1 %to &waven.;
		%let x = %sysevalf(&w.-1);
		%let waveno=&&wave&w..;
		
		/*loop through each tv continuous*/

		/*loop through each tv categorical*/
		%do m=1 %to &tvmn.;
			%let cat=&&tvm&m..;
			%let l_cat=l_&cat.;
			%array(&l_cat., values=&&tvl&m.., delim=_);
			%let l_catn=&&&l_cat.n.;

			/*natural course*/
			%if &nat_cor.=1 %then %do;
				data t.&cat.&waveno._nc;
					set boot;
				run;

				proc logistic inmodel=om_&cat.&waveno.;
					score data=t.&cat.&waveno._nc out=t.pom_&cat.&waveno._nc(keep=mergeid p_: censor: i);
				run;
				
				data t.&cat.&waveno._nc(keep=mergeid i &cat.&waveno._nc);
					set t.pom_&cat.&waveno._nc;
					call streaminit(886);
					if censor&x.=0 then &cat.&waveno._nc=rand('table',p_0,p_1,p_2)-1;
					else &cat.&waveno._nc=.;
				run;
			%end;

			%else %do;
				%do l=1 %to &l_catn.;
					%let lvl=&&&l_cat.&l..;

					*if first wave, then use observed data to simulate covar;
					%if &w.=1 %then %do;
						data t.&cat.&waveno._l&lvl.;
							set boot;
							/*set giv_help0 to each level*/
							giv_help&waveno.=&lvl.;
						run;
					%end;

					/*if not first wave, then merge observed data with wave1 simulated data
					to simulate covars in subsequent wave*/
					%else %do;
						data t.&cat.&waveno._l&lvl.;
							merge boot t.&cat.&&wave&x.._l&lvl.(keep=mergeid &cat.&&wave&x..:);
							by mergeid;
							/*setting giv_help to each level*/
							giv_help&&wave&x..=&lvl.;
						run;
					%end;
					
					*use score to generate predicted probability of each level using 
					regression parameter generated from %ey;	
					proc logistic inmodel=om_&cat.&waveno.;
						score data=t.&cat.&waveno._l&lvl. out=t.pom_&cat.&waveno._l&lvl.(keep=mergeid p_: censor: i);
					run;
					
					*based on predicted probability, randomly simulate covariates in the next wave;
					data t.&cat.&waveno._l&lvl.(keep=mergeid i &cat.&waveno._l&lvl.);
						set t.pom_&cat.&waveno._l&lvl.;
						call streaminit(886);
						/*need to be flexible to accomodate different number of categories*/
						/*check definition of censor. Pretty sure censor1=1 means wave2 the person is dead*/
						if censor&waveno.=0 then &cat.&waveno._l&lvl.=rand('table',p_0,p_1,p_2)-1;
						else &cat.&waveno._&lvl.=.;
					run;
				%end;
			%end;
		%end;

		/*use new simulated data to predict outcome:*/

		/*loop through each tv binary*/
		/*natural course*/
		%do n=1 %to &tvbn.;
			%let bin=&&tvb&n..;

			%if &nat_cor.=1 %then %do;
				data t.&bin.&waveno._nc;
					set boot;
				run;

				proc logistic inmodel=om_&bin.&waveno.;
					score data=t.&bin.&waveno._nc out=t.pom_&bin.&waveno._nc(keep=mergeid p_: censor: i);
				run;
				
				data t.&bin.&waveno._nc(keep=mergeid i &bin.&waveno._nc);
					set t.pom_&bin.&waveno._nc;
					call streaminit(886);
					if censor&x.=0 then &bin.&waveno._nc=rand('table',p_0,p_1,p_2)-1;
					else &bin.&waveno._nc=.;
				run;
			%end;
		%end;
	%end;
%mend;
		 
%gstep3(input_ds=boot 
		 ,waves=1 2
		 ,nat_cor=1
		 ,yvar=eurodcat
		 ,tf_cat=country 
		 ,tf_bc=gender
		 ,tv_cat=giv_help
		 ,tv_lvl=0_1_2
		 ,tv_bin=rec_help
		 ,tv_con=logthinc);
