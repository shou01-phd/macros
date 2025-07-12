*download other variables to predict censor status;
data test1;
	set m.share_all
	(keep=mergeid hhid coupleid implicat wave fam_resp country age int_year
		  hhsize gender: mstat: cjs: adl: iadl: gali: isced: yedu: nchild:
		  rhfo: rhih: eurod: sphus: sharelife fdistress: ghih: ghto: 
		  thinc: yreg: ypen: ydip:
		  sp002_ sp003_: sp004d: sp005_: sp007_: sp008_ sp009_:
		  sp010d: sp011_: sp018_ sp019d: sp020_ sp021d: ph011d9 ph011d10
		  reading writing maxgrip);
	if age>=50 and country in (11, 12, 13, 14, 15, 16, 17, 18, 20, 23);
	if (wave=4|wave=5) & fam_resp=0 then delete; 
	if wave=7 & sharelife=1 then delete; *sharelife=1 means the participant is in sharelife;

	/*Exposure definition*/
	*exposure: no cg, some cg, daily cg;
	*1. no outside hh AND hhsize=1 OR 2. no outside hh AND no inside hh cg;
	if (sp008_=5 & hhsize=1) | (sp008_=5 & sp018_=5) then giv_help=0;

	*less than daily outside hh care;
	if sp011_1>1|sp011_2>1|sp011_3>1 then giv_help=1;
	
	*daily outside hh care or any inside hh care;
	if (sp011_1=1|sp011_2=1|sp011_3=1)|sp018_=1 then giv_help=2;

	if implicat=1 then output test1;
run;
proc sort data=test1;
	by mergeid wave;
run;

data test1;
	format mergeid hhid $12. country wave giv_help 8. m_stat m_stat.;
	set test1;
	by mergeid;
	
	/*participants that miss more than 2 waves are censored*/
	*recode waveno (because wave3 is not included);
	if wave>=4 then waveno=wave-1;
	else waveno=wave;
	
	*wavediff is the difference between current;
	if first.mergeid then wavediff=0;
	lwaveno=lag(waveno);
	if mergeid=lag(mergeid) then wavediff=waveno-lwaveno;

	*since it's possible to miss 2 waves and then return,
	I want to exclude those observations. An single participant can't contribute
	more than 1 episode.;
	retain episode;
	if first.mergeid then episode=1;
	*if wavediff>2, then the participant has missed more than 1 wave;
	if mergeid=lag(mergeid) & wavediff>2 then episode=episode+1;

	/*generate prior treatment*/
	if first.mergeid then prev_help=0;
	lgiv_help=lag(giv_help);
	if mergeid=lag(mergeid) then prev_help=lgiv_help;

	/*recode marital status*/
	*dn014_ wave>1 is only listed if there is a change in marital status
	from the previous wave, so I am using retain to code marital status,
	which only changes when respondent indicates change;
	if mstat=1|mstat=2|mstat=3 then m_stat=1;
	else if mstat=4 then m_stat=2;
	else if mstat=5 then m_stat=3;
	else if mstat=6 then m_stat=4;
	
	/*recode receiving help: binary of whether the respondent has received 
	any help in or out of the household*/
	if rhfo>0 | rhih>0 then rec_help=1;
	else if rhfo=0 & rhih=-99 then rec_help=0;
	else if rhfo=0 & rhih=0 then rec_help=0;

	/*recode medication*/
	if ph011d9=1|ph011d10=1 then psych_med=1;
	else if ph011d9=0|ph011d10=0 then psych_med=0;

	/*recode employment status*/
	if cjs=-99 then cjs=97;

	/*generate censor variable*/
	if episode=2 then censor=1;
	else censor=0;

	*this drops all person-waves of individuals who have missed 2 or more waves;
	if episode=1 then output;
run;

/*create censor indicator*/
data test;
	set test1;
 	by mergeid;
	if (last.mergeid & wave ne 8) then censor=1;
	else censor=0;
run;

filename ipcw1 "C:\Users\Hoff\Documents\hou\mcgill\dissertation\results\ipcw1_20230404.pdf";
ods pdf file = ipcw1;
title "Numerator P(Ck=0|Ck-1=0,Ak)";
proc logistic data=test;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	class giv_help (ref="0") wave (ref="1") / param=ref;
	model censor=giv_help wave;
	output out=num1c (keep=mergeid wave num) p=num;
run;
title "Numerator P(Ck=0|Ck-1=0,Ak-1) hernan-robins";
proc logistic data=test;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	class prev_help (ref="0") wave (ref="1") / param=ref;
	model censor=giv_help wave;
	output out=num1c_hr (keep=mergeid wave num_hr) p=num_hr;
run;
title "Denominator P(Ck=0|Ck-1=0,Ak,Ak-1,Lk,Z0) w same vars as iptw";
proc logistic data=test; 
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	class giv_help (ref="0") wave (ref="1") rec_help (ref="0") gender (ref="Male") country (ref="11")
		  m_stat (ref="Married/partnered") cjs (ref="Employed or self-employed") sphus (ref="Excellent") 
		  psych_med (ref="0") prev_help (ref="0") / param=ref;
	model censor=give_help wave rec_help gender country m_stat cjs sphus psych_med prev_help 
		  adl age nchild thinc yedu / link=glogit;
	output out=denom1c (keep=mergeid wave denom) p=denom;
run;
title "Denominator P(Ck=0|Ck-1=0,Ak,Lk,Z0) updated vars pred c";
proc logistic data=test; 
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	class giv_help (ref="0") gender (ref="Male") country (ref="11") sphus (ref="Excellent") 
		  reading (ref="Excellent") writing (ref="Excellent") wave (ref="1") / param=ref;
	model censor=giv_help gender country sphus reading writing wave
		  iadl age thinc maxgrip;
	output out=denom1c_uv (keep=mergeid wave denom_nv) p=denom_nv;
run;
ods pdf close;
title;

*merged dataset that includes numerator and denominator to calculate weights;
data merged1c;
	merge test  
		  num1c
		  num1c_hr
		  denom1c
		  denom1c_uv;
	by mergeid wave;
	uw=1/denom;
	sw=num/denom;
	sw_hr=num_hr/denom;
	uw_nv=1/denom_nv;
	sw_nv=num/denom_nv;
	sw_hr_nv=num_hr/denom_nv;
run;

proc univariate data=merged1c;
	var uw sw sw_hr uw_nv sw_nv sw_hr_nv;
	output out=percentile1c
	pctlpts=0.5 1 99 99.5 pctlpre=uw_ sw_ swhr_ uwnv_ swnv_ swhrnv_;
run;

proc logistic data=merged1c; 
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	class giv_help (ref="0") gender (ref="Male") country (ref="11") sphus (ref="Excellent") 
		  reading (ref="Excellent") writing (ref="Excellent") wave (ref="1") / param=ref;
	model censor=giv_help gender country sphus reading writing wave
		  iadl age thinc maxgrip;
	weight sw_nv;
run;


proc sql noprint;
	select puw_1 into :puw1c separated by ' ' from percentile1c;
	select puw_99 into :puw99c separated by ' ' from percentile1c;
	select psw_1 into :psw1c separated by ' ' from percentile1c;
	select psw_99 into :psw99c separated by ' ' from percentile1c;
quit;
%put puw1=&puw1c. puw99=&puw99c. psw1=&psw1c. psw99=&psw99c.;

data merged1c;
	set merged1c;

	uw_t=uw; 
	if uw>&puw99c. & uw ne . then uw_t=&puw99c.;
	else if uw<&puw1c. & uw ne . then uw_t=&puw1c.;

	sw_t=sw;
	if sw>&psw99c. & sw ne . then sw_t=&psw99c.;
	else if sw<&psw1c. & sw ne . then sw_t=&psw1c.;
run;

*All OR estimates should be around 1;
title "Logistic reg estimates with weights (uw: 1/P(Ck=0|Ck-1=0,Lk,Z0))";
proc logistic data=merged1c;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	class giv_help (ref="0") rec_help (ref="0") gender (ref="Male") country (ref="11")
		  m_stat (ref="Married/partnered") cjs (ref="Employed or self-employed") sphus (ref="Excellent") 
		  psych_med (ref="0") prev_help (ref="0") / param=ref;
	model censor=giv_help rec_help gender country m_stat cjs sphus psych_med prev_help 
		  adl age nchild thinc yedu;
	weight uw;
run;
title "Logistic reg estimates with weights (sw: P(Ck=0|Ck-1=0)/P(Ck=0|Ck-1=0,Lk,Z0))";
proc logistic data=merged1c;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	class giv_help (ref="0") rec_help (ref="0") gender (ref="Male") country (ref="11")
		  m_stat (ref="Married/partnered") cjs (ref="Employed or self-employed") sphus (ref="Excellent") 
		  psych_med (ref="0") prev_help (ref="0") / param=ref;
	model censor=giv_help rec_help gender country m_stat cjs sphus psych_med prev_help 
		  adl age nchild thinc yedu;
	weight sw;
run;
title "Logistic reg estimates with weights (truncated uw: 1/P(Ck=0|Ck-1=0,Lk,Z0))";
proc logistic data=merged1c;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	class giv_help (ref="0") rec_help (ref="0") gender (ref="Male") country (ref="11")
		  m_stat (ref="Married/partnered") cjs (ref="Employed or self-employed") sphus (ref="Excellent") 
		  psych_med (ref="0") prev_help (ref="0") / param=ref;
	model censor=giv_help rec_help gender country m_stat cjs sphus psych_med prev_help 
		  adl age nchild thinc yedu;
	weight uw_t;
run;
title "Logistic reg estimates with weights (truncated sw: P(Ck=0|Ck-1=0)/P(Ck=0|Ck-1=0,Lk,Z0))";
proc logistic data=merged1c;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	class giv_help (ref="0") rec_help (ref="0") gender (ref="Male") country (ref="11")
		  m_stat (ref="Married/partnered") cjs (ref="Employed or self-employed") sphus (ref="Excellent") 
		  psych_med (ref="0") prev_help (ref="0") / param=ref;
	model censor=giv_help rec_help gender country m_stat cjs sphus psych_med prev_help 
		  adl age nchild thinc yedu;
	weight sw_t;
run;
title;

/*Average number of waves per participant*/
data check;
	set test (keep=mergeid wave int_year);
	by mergeid;

	retain wavecount firstintyear;

	if first.mergeid then do;
		wavecount=0;
		*int_year=firstintyear;
		firstintyear=int_year;
	end;
	wavecount=wavecount+1;
	if last.mergeid then do;
		yearsinstudy=int_year-firstintyear;
		output;
	end;
run;

proc freq data=check;
	table wavecount;
run;
proc means data=check;
	var wavecount yearsinstudy;
run;
