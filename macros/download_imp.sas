%macro download_imp(file_folder=
					,data_version=
					,filetypes=
					,xt=
					,wave=
					,delete_work=);
	%macro dummy; %mend dummy;
	%array(df_names,values=&filetypes.);
	%array(wave_no,values=&wave., delim=_);

	*download all waves and datafiles;
	%do a=1 %to &wave_non.;
		%do d=1 %to &df_namesn.;
			proc import datafile="&file_folder.&&wave_no&a..\sharew&&wave_no&a.._&data_version._&&df_names&d...dta"
				out=&&df_names&d..&&wave_no&a..
				dbms=dta
				replace;
			run;
			proc sort data=&&df_names&d..&&wave_no&a..;
				by mergeid;
			run;
		%end;
			
		*merge datafiles by wave;
		data m.wave&&wave_no&a.. (rename=(hhid&&wave_no&a..=hhid 
										  coupleid&&wave_no&a..=coupleid
										  mergeidp&&wave_no&a..=mergeidp
										  dw_w&&wave_no&a..=dw_w
										  cchw_w&&wave_no&a..=cchw_w
										  cciw_w&&wave_no&a..=cciw_w));
			merge %do d=1 %to &df_namesn.; &&df_names&d..&&wave_no&a.. %end;;
			by mergeid;
			wave=&&wave_no&a..;
		run;
	%end;
	
	*copy format to a permanent library;
	proc catalog catalog = work.formats;
		copy out = m.formats;
	run;
	quit;

	/*call rename_spvars macro: it renames all the social support variables in wave 4
	MUST INCLUDE THIS IF USING WAVE4*/
	%if &wave.=4 %then %do;
		%rename_spvars(lib=m,dataname=wave4);
	%end;

	*stack all the waves;
	data m.share_all;
		set %do a=1 %to &wave_non.;
			m.wave&&wave_no&a..
		%end;;
	run;

	*load xt file (death information);
	%if &xt.=1 %then %do;
		%do a=1 %to &wave_non.;
			proc import datafile="&file_folder.&&wave_no&a..\sharew&&wave_no&a.._&data_version._xt.dta"
				out=xt&&wave_no&a..
				dbms=dta
				replace;
			run;

			data xt&&wave_no&a.. (rename=(hhid&&wave_no&a..=hhid
								   		  language_xt=language
										  gender_xt=gender));
				set xt&&wave_no&a..;
				wave=&&wave_no&a..;
			run;
		%end;
		
		*stack xt files;
		data m.xt_all;
			set %do a=1 %to &wave_non.;
				xt&&wave_no&a..
			%end;;
			death=1;
		run;
	%end;

	*delete datasets in the work library;
	%if &delete_work.=1 %then %do;
		proc datasets library=WORK kill; 
		run; 
		quit;
	%end;

	options fmtsearch=(m);
%mend;
