%macro makedummy(inds=, outds=, varnames=, levels=);
	%macro dummy; %mend dummy;

	%array(vns, values=&varnames.);
	%array(varls, values=&levels.);
	data &outds.;
		set &inds.;
		%do t=1 %to &vnsn.;
			%let l_var=l_&&vns&t..;
			%array(&l_var., values=&&varls&t.., delim=_);
			%let l_varn = &&&l_var.n.;
			%put variable = &&vns&t..;

			/*%put variable to be looped: array=&l_var., values=&&varls&t.., n=&&&l_var.n.;*/

			%do u=1 %to &l_varn.;
				%put the level being resolved = &&&l_var.&u..;
				if &&vns&t.. ne . then do;
					if &&vns&t..=&&&l_var.&u.. then &&vns&t..&&&l_var.&u..=1;
					else &&vns&t..&&&l_var.&u..=0;
				end;
				else &&vns&t..=.;
			%end;	
		%end;
	run;
%mend;

%macro makedummy1(varnames=, levels=);
	%macro dummy; %mend dummy;

	%array(vns, values=&varnames.,delim=|);
	%array(varls, values=&levels., delim=|);
	%do t=1 %to &vnsn.;
		%let l_var=l_&&vns&t..;
		%array(&l_var., values=&&varls&t.., delim=_);
		%let l_varn = &&&l_var.n.;

		/*%put variable to be looped: array=&l_var., values=&&varls&t.., n=&&&l_var.n.;*/

		%do u=1 %to &l_varn.;
			/*%put the level being resolved = &&&l_var.&u..;*/

			if &&vns&t..=&&&l_var.&u.. then &&vns&t..&&&l_var.&u..=1;
			else &&vns&t..&&&l_var.&u..=0;
		%end;	
	%end;
%mend;
