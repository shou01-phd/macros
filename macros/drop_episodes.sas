/*****************************************************************************
This macro is called into the share_aim1_ipcw to do data cleaning:
create lag vars, recode vars values, etc. Most importantly, it creates
"time" and censor variables that determine which observations must be dropped. 
This version was last edited on 2023-07-06.***********************************/

%macro drop_episodes;
	%macro dummy; %mend dummy;
	%array(wave_no,values=1_2_4_5_6_7_8, delim=_);

	/*Censor those who have missed 1 wave*/
	/*M.SHARE_all should be considered the "base" dataset:
	all eligibility criteria and censoring have been applied*/
	/*Recode macro to allow for dataset loop*/
	%do i=1 %to 5;
		proc sort data=share&i.;
			by mergeid wave;
		run;

		data share&i. (drop=lag:);
			format mergeid $12. hhid $11. country wave giv_help gender 8.;
			set share&i.;
			by mergeid;
			
			/*participants that miss 1 wave are censored*/
			*recode waveno (because wave3 is not included);
			if wave>=4 then waveno=wave-1;
			else waveno=wave;
			
			*wavediff is the difference between current;
			if first.mergeid then wavediff=0;
			lagwaveno=lag(waveno);
			if mergeid=lag(mergeid) then wavediff=waveno-lagwaveno;

			*since it's possible to miss a wave and then return,
			I want to exclude those observations. An single participant can't contribute
			more than 1 episode.;
			retain episode;
			if first.mergeid then episode=1;
			*if wavediff>1, then the participant has missed one wave or more;
			if mergeid=lag(mergeid) & wavediff>1 then episode=episode+1;

			*create dummy variables for wave;
			%do a=1 %to &wave_non.;
				if wave=&&wave_no&a.. then  wave&&wave_no&a..=1;
				else wave&&wave_no&a..=0;
			%end;

			/**lag variables**/

			/*generate prior treatment*/
			if first.mergeid then prev_help=.;
			laggiv_help=lag(giv_help);
			if mergeid=lag(mergeid) then prev_help=laggiv_help;

			/*recode marital status: 1-married/partnered, 2-never married, 3-divorced/separated, 4-widowed*/
			if mstat=1 | mstat=2 | mstat=3 then m_stat=1;
			else if mstat=4 then m_stat=2;
			else if mstat=5 then m_stat=3;
			else if mstat=6 then m_stat=4;
			if first.mergeid then prev_mstat=.;
			lagm_stat=lag(m_stat);
			if mergeid=lag(mergeid) then prev_mstat=lagm_stat;
			
			*recode to different categories: 1-married, 0-not;
			if m_stat=1 then mstatb=1;
			else if m_stat>1 then mstatb=0;
			if first.mergeid then prev_mstatb=.;
			lagm_statb=lag(mstatb);
			if mergeid=lag(mergeid) then prev_mstatb=lagm_statb;

			
			/*recode receiving help: binary of whether the respondent has received 
			any help in or out of the household*/
			if rhfo>0 | rhih>0 then rec_help=1;
			else if rhfo=0 & rhih=-99 then rec_help=0;
			else if rhfo=0 & rhih=0 then rec_help=0;
			if first.mergeid then prev_rec_help=.;
			lagrec_help=lag(rec_help);
			if mergeid=lag(mergeid) then prev_rec_help=lagrec_help;

			/*recode medication*/
			if ph011d9=1 | ph011d10=1 then psych_med=1;
			else if ph011d9=0 | ph011d10=0 then psych_med=0;
			if first.mergeid then prev_psych_med=.;
			lagpsych_med=lag(psych_med);
			if mergeid=lag(mergeid) then prev_psych_med=lagpsych_med;

			/*recode employment status*/
			if cjs=-99 then cjs=97;
			if first.mergeid then prev_cjs=.;
			lagcjs=lag(cjs);
			if mergeid=lag(mergeid) then prev_cjs=lagcjs;
			*recode with different categories;
			if cjs=1 then jobs=1;
			else if cjs=2 then jobs=2;
			else if cjs=5 then jobs=3;
			else jobs=4;
			if cjs=2 then work=1;
			else work=0;
			if first.mergeid then prev_jobs=.;
			lagjobs=lag(jobs);
			if mergeid=lag(mergeid) then prev_jobs=lagjobs;

			/*create lag sphus*/
			if first.mergeid then prev_sphus=.;
			lagsphus=lag(sphus);
			if mergeid=lag(mergeid) then prev_sphus=lagsphus;

			/*create lag adl*/
			*recode adl;
			if adl=0 then adlb=0;
			else adlb=1;

			if first.mergeid then do;
				prev_adl=.;
				prev_adlb=.;
			end;
			lagadl=lag(adl);
			lagadlb=lag(adlb);
			if mergeid=lag(mergeid) then do;
				prev_adl=lagadl;
				prev_adlb=lagadlb;
			end;

			/*create lag and log10 transformed thinc*/
			if thinc<1 then thinct=1;
			else thinct=thinc;
			logthinc=log10(thinct);

			if first.mergeid then prev_thinc=.;
			lagthinc=lag(logthinc);
			if mergeid=lag(mergeid) then prev_thinc=lagthinc;

			/*create lag maxgrip*/
			if first.mergeid then prev_maxgrip=.;
			lagmaxgrip=lag(maxgrip);
			if mergeid=lag(mergeid) then prev_maxgrip=lagmaxgrip;

			/*create lag eurod and lag eurodcat*/
			if first.mergeid then prev_eurod=.;
			lageurod=lag(eurod);
			if mergeid=lag(mergeid) then prev_eurod=lageurod;

		 	if eurod>=4 then eurodcat=1;
			if eurod<4 then eurodcat=0;
			if first.mergeid then prev_eurodcat=.;
			lageurodcat=lag(eurodcat);
			if mergeid=lag(mergeid) then prev_eurodcat=lageurodcat;

			/*for measuring followup time: create firstwave*/
			retain firstwave firstintyear;
			if first.mergeid then do;
				firstwave=waveno;
				firstintyear=int_year;
			end;
			firstwave=firstwave;
			firstintyear=firstintyear;

			/*non-fatal diagnoses/event: 
			heart attack, hypertension, high cholesterol, stroke, and diabetes*/
			if ph006d1=1 | ph006d2=1 | ph006d3=1 | ph006d4=1 | ph006d5=1 then diag_ever=1;
			else diag_ever=0;
			
			if first.mergeid then diag_wave=diag_ever;
			lagdiag=lag(diag_ever);
			if mergeid=lag(mergeid) & diag_ever=1 & lagdiag=0 then diag_wave=1;
			else diag_wave=0;

			retain diag_count;
			if first.mergeid then diag_count=0;
			else if diag_wave=1 then diag_count=diag_count+1;

			if first.mergeid then do;
				prev_diag_wave=.; prev_diag_ever=.;
			end;
			lagdiagwave=lag(diag_wave);
			lagdiagever=lag(diag_ever);
			if mergeid=lag(mergeid) then do;
				prev_diag_wave=lagdiagwave; prev_diag_ever=lagdiagever;
			end;

			/*recode gender*/
			if gender=2 then gender=0;

			/*generate censor variable:
			1. missed visit (or episode>1)
			2. death
			3. obs before either of those occurences*/
			if episode>=2 then censor=1;
			else if death=1 then censor=1;
			else censor=0;
		run;
		
		proc sort data=share&i.;
			by mergeid censor descending wave;
		run;

		*The first observation before death or lost-to-followup will be censored;
		data share&i.;
			set share&i.;
			by mergeid censor;
			if first.censor & wave ne 8 then censor=1;
		run;
		proc sort data=share&i.;
			by mergeid descending censor wave;
		run;
		data share&i.;
			set share&i.;
			by mergeid;
			if first.mergeid then keepflag=1;
			else if censor=1 then keepflag=0;
			else if censor=0 then keepflag=1;

			if keepflag=0 then delete;
			if int_year=. then delete;
		run;
		proc sort data=share&i.;
			by mergeid descending wave;
		run;
		data share&i.;
			set share&i.;
			by mergeid;
			retain lastintyear;
			if first.mergeid then lastintyear=int_year;
			else lastintyear=lastintyear;
		run;
		proc sort data=share&i.;
			by mergeid wave;
		run;

		*create new time variable;
		data share&i.;
			set share&i.;
			by mergeid;

			retain time;
			if first.mergeid then time=1;
			else time=time+1;

			*for std mean diff comparison, recode exp to binary;
			if giv_help=0 then giv_help_02=0;
			else if giv_help=2 then giv_help_02=1;
			if giv_help=0 then giv_help_01=0;
			else if giv_help=1 then giv_help_01=1;
			if prev_help=0 then prev_help_02=0;
			else if prev_help=2 then prev_help_02=1;
			if prev_help=0 then prev_help_01=0;
			else if prev_help=1 then prev_help_01=1;
			
			*compare job status: 5-homemaker;
			if prev_jobs=2 then prev_jobsb=1;
			else prev_jobsb=0;
		run;
	%end;
%mend; 
