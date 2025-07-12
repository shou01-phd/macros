/*causal estimand is the mean value of eurodcat*/
%macro eurodcatmean (waves=, set=, lib=, groupvar=, ds_output=, suffix=, deletefile=);
	%macro dummy; %mend dummy;
	%array(wa,values=&waves.);
	%array(se,values=&set.);

	%do w=1 %to &wan.;
		%do s=1 %to &sen.;
			proc means data=&lib..w&&wa&w.._s&&se&s.. noprint;
				title "set giv_help=&&se&s.. at t=&&wa&w..";
				var eurodcat&&wa&w..;
				output out=w&&wa&w.._s&&se&s.. mean=eurodcat n=n_size;
				by &groupvar.;
			run;

			data w&&wa&w.._s&&se&s..;
				set w&&wa&w.._s&&se&s..;
				set="&&se&s..";
			run;

			%if &deletefile.=1 %then %do;
				proc delete data=&lib..w&&wa&w.._s&&se&s..; quit;
			%end;
		%end;

		data w&&wa&w..;
			set %do s=1 %to &sen.; w&&wa&w.._s&&se&s.. %end;;
			wave=&&wa&w..;
		run;
	%end;
	
	data &lib..&ds_output._&suffix.(drop=_type_);
		format _freq_ wave 8. set $2. n_size 8. eurodcat 8.5;
		set %do w=1 %to &wan.; w&&wa&w.. %end;;
	run;		
%mend;
