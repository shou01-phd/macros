proc surveyselect data=sharem out=boot(keep=numberhits mergeid gender age0 country eurodcat0 giv_help0 logthinc0 rec_help0)
				  method=urs seed=886 sampsize=1000; run;

data boot;
	set boot; 

	do i=1 to numberhits;
		intercept=1;
		output;
	end;
run;

%macro gstep3_score(input_ds= /*input data, include libname*/
					 ,waves=	/*list out waves AFTER baseline*/
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
	%array(tvl, values=&tv_lvl., delim=|);
	%array(tvb, values=&tv_bin.);
	%array(tvc, values=&tv_con.);
	%array(tv, values=&tv_cat. &tv_bin. &tv_con.);

	%do w=1 %to &waven.;
		%let x = %sysevalf(&w.-1);
		%let waveno=&&wave&w..;
		
		%do m=1 %to &tvmn.;
			%let cat=&&tvm&m..;
			%let l_cat=l_&cat.;
			%array(&l_cat., values=&&tvl&m.., delim=_);
			%let l_catn = &&&l_cat.n.;
			
			%do l=1 %to &l_catn.;
				data boot1;
					set &input_ds.(keep=mergeid %do a=1 %to &tfcn.; &&tfc&a.. %end; 
										  %do b=1 %to &tfbcn.; &&tfbc&b.. %end; 
										  %do c=1 %to &tvmn.; &&tvm&c..&x. %end;
										  %do d=1 %to &tvbn.; &&tvb&d..&x. %end; 
										  %do e=1 %to &tvcn.; &&tvc&e..&x. %end;);
					&cat.&x.=&&&l_cat.&l..;
				run;
				proc logistic inmodel=om_&cat.&waveno.;
					score data=boot1 out=p_&cat.&waveno._&&&l_cat.&l..(keep=mergeid i_giv_help1 p_:);
				run;
			%end;
		%end;
	%end;
%mend;

%gstep3_score(input_ds=boot 
		 ,waves=1 2	
		 ,tf_cat=country 
		 ,tf_bc=gender age0
		 ,tv_cat=giv_help
		 ,tv_lvl=0_1_2
		 ,tv_bin=rec_help
		 ,tv_con=logthinc);

		 proc freq data=p_giv_help1_2;
		 	table i_giv_help1;
		run;
