/*Results of balance analysis*/
%macro balance_diag(filepath=
					,filecode=
					,input_ds=
					,con_variables=
					,bin_variables=
					,yvar=
					,weights=);
	%macro dummy; %mend dummy;

	%let versiondate=%sysfunc(today(), yymmddn8.);
	%put &versiondate.;
	ods pdf file="&filepath.\&filecode._&versiondate..pdf";
	%array(cont_vars, values=&con_variables.);
	%array(weighttype, values=&weights.);

	%do e=1 %to &cont_varsn.;
		title "Visual assessments: &&cont_vars&e..";
		proc sgplot data=&input_ds.;
			hbox &&cont_vars&e.. / category=&yvar.;
		run;
	%end;

	%do e=1 %to &cont_varsn.;
		%do w=1 %to &weighttypen.;
			title "Visual assessments: &&cont_vars&e.. (&&weighttype&w..)";
			proc sgplot data=&input_ds.;
				hbox &&cont_vars&e.. / category=&yvar. weight=&&weighttype&w..;
			run;
		%end;
	%end; 

	*standardized difference loop through all weights;
	%stdiff(dataset=&input_ds.
	,group=&yvar.
	,mean=&con_variables.
	,proportion=&bin_variables.
	,subtitle="No weights");

	%do w=1 %to &weighttypen.;
		%stdiff(dataset=&input_ds.
		,group=&yvar.
		,mean=&con_variables.
		,proportion=&bin_variables.
		,w=&&weighttype&w..
		,subtitle="&&weighttype&w.");
	%end;
	ods pdf close;
%mend;
