%macro result_format(input_ds=,suffix=,t=,d=,r=);
	%macro dummy; %mend dummy;
	proc sql;
		create table mi_fmt_pt as select
		wave
		,set
		,estimate as pt format=percentn7.1 
		,put(lclmean,percentn7.1) as lcl_pt
		,put(uclmean,percentn7.1) as ucl_pt
		,strip(cats('[',calculated lcl_pt,",")) || " " || strip(cats(calculated ucl_pt,"]"))
			as pt_ci length=20
		from &input_ds._&suffix. where parm="pt";

		create table mi_fmt_pd as select
		wave
		,set
		,estimate*100 as pd format=8.1
		,put(lclmean*100,8.&d.) as lcl_pd
		,put(uclmean*100,8.&d.) as ucl_pd
		,strip(cats('[',calculated lcl_pd,",")) || " " || strip(cats(calculated ucl_pd,"]")) 
			as pd_ci length=20
		from &input_ds._&suffix. where parm="pd";

		create table mi_fmt_pr as select
		wave
		,set
		,estimate as pr format=8.1
		,put(lclmean,8.&r.) as lcl_pr
		,put(uclmean,8.&r.) as ucl_pr
		,strip(cats('[',calculated lcl_pr,",")) || " " || strip(cats(calculated ucl_pr,"]")) 
			as pr_ci length=20
		from &input_ds._&suffix. where parm="pr";
	quit;

	data &input_ds._fmt_&suffix.(drop=lcl_: ucl_:);
		merge mi_fmt_pt mi_fmt_pd mi_fmt_pr;
		by wave set;
	run;
%mend;
