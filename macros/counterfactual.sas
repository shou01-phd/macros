**Data created from Hanley and Shapiro (1994 Journal of Statistical Education 2(1) );
data Flies;
input ID treatment thorax death40;
cards;
1 0 0.64 1
2 0 0.68 1
3 0 0.68 0
4 0 0.72 0
5 0 0.72 0
6 0 0.76 1
7 0 0.76 0
8 0 0.76 0
9 0 0.76 0
10 0 0.76 0
11 0 0.80 0
12 0 0.80 0
13 0 0.80 0
14 0 0.84 0
15 0 0.84 0
16 0 0.84 0
17 0 0.84 0
18 0 0.84 0
19 0 0.84 0
20 0 0.88 0
21 0 0.88 0
22 0 0.92 0
23 0 0.92 0
24 0 0.92 0
25 0 0.94 0
26 1 0.64 1
27 1 0.64 1
28 1 0.68 1
29 1 0.72 1
30 1 0.72 1
31 1 0.74 1
32 1 0.76 1
33 1 0.76 0
34 1 0.76 0
35 1 0.78 1
36 1 0.80 1
37 1 0.80 1
38 1 0.82 1
39 1 0.82 0
40 1 0.84 1
41 1 0.84 1
42 1 0.84 0
43 1 0.84 0
44 1 0.88 0
45 1 0.88 0
46 1 0.88 0
47 1 0.88 0
48 1 0.88 0
49 1 0.88 0
50 1 0.92 0
;

proc logistic data=flies descending; 
	ods exclude ClassLevelInfo modelanova Association FitStatistics GlobalTests;
	model death40=treatment thorax;
	ods output ParameterEstimates=ctest;
run;
****************************************************************************
* Note: Always remember to change data set name as well as variable names *
****************************************************************************;
%macro CounterFactual(data=, /*data set to be analyzed*/
yvar =, /*outcome variabe*/
trt =, /* Exposure or treatment variable */
xvar =, /* independent variables to be adjusted */
link =, /* link function used in PROC GENMOD: logit, probit, cloglog */
alpha=0.05 /* alpah level for confidence interval, default is 0.05*/
);
%macro dummy; %mend dummy;

	%if &link= probit %then %let dist='normal';
	%else %if &link=logit %then %let dist='logistic';
	%else %if &link=cloglog %then %let dist='extreme';

	proc genmod desc data= &data;
		model &yvar = &trt &xvar/covb dist=bin link= &link;
		ods output ParameterEstimates=est(keep=estimate)
		covB=cov(drop=rowname);

	proc iml;
		start CI4P(Pest, Var, n);
			%let z=probit(1-&alpha/2);
			*Wald;
			W_L = pest - &z*sqrt(var) ;
			W_U = pest + &z*sqrt(var) ;
			*log, usd by Flanders and Rhodes (1987 J Chron Dis 40: 697-704);
			lOG_L = pest*exp(- &z*sqrt(var)/(pest));
			log_U = pest*exp(+ &Z*sqrt(var)/(pest));
			*logit;
			varlgt = var/( pest*(1-pest))**2 ;
			l=log(pest/(1-pest)) - &z*sqrt(varlgt);
			lgt_L = exp(l)/(1+exp(l));
			u=log(pest/(1-pest)) + &z*sqrt(varlgt);
			lgt_U = exp(u)/(1+exp(u));
			*Wilson, based on Newcombe (2001 Am Stat 55: 200-202);
			ll=log(pest/(1-pest)) - 2*arsinh( &z/2 * sqrt(varlgt));
			Wln_L = exp(ll)/(1+exp(ll));
			uu=log(pest/(1-pest)) + 2*arsinh( &z/2 * sqrt(varlgt));
			Wln_U = exp(uu)/(1+exp(uu));
			return(pest||W_L||W_U||log_L||log_U||lgt_L||lgt_U||wln_L||wln_U);
	finish CI4P;

	start MOVER(p1, l1, u1, p2, l2, u2, corr);
		**Baded on Zou (2008 Am J Epidemiol 162: 212-224);
		point = p1-p2;
		L = p1- p2 - sqrt(max(0, (p1-l1)**2 -2*corr*(p1-l1)*(u2-p2) + (u2-p2)**2));
		U = p1- p2 + sqrt(max(0, (u1-p1)**2 -2*corr*(u1-p1)*(p2-l2) + (p2-l2)**2));
		return(point||L||U);
	finish MOVER;

	**Bring in data for prediction;
	use &data;
	read all var{&xvar} into X;
	n = nrow(X);
	m = ncol(X);
	X1 = J(n,2,1)||X;
	X0 = J(n,1,1)||J(n,1,0)||X;
	use cov;
	read all var _num_ into V;
	use est;
	read all var _num_ into beta;
	beta=beta[1:(m+2)];
	if &dist = 'extreme' then do;
		p_1 = 1-exp(-exp(X1 * beta));
		p_0 = 1-exp(-exp(X0 * beta));
		piece1 = exp(X1 * beta -exp(X1 * beta) )# X1;
		piece0 = exp(X0 * beta -exp(X0 * beta) )# X0;
	end;
	else do;
		p_1 = cdf(&dist, X1 * beta); **predicted prob, if exposed;
		p_0 = cdf(&dist, X0 * beta); **predicted prob, if unexposed;
		piece1 = pdf(&dist, X1 * beta)# X1;
		piece0 = pdf(&dist, X0 * beta)# X0;
	end;
	p1est = sum(p_1)/n;
	p0est = sum(p_0)/n;
	V1=0; V0=0; COV =0;
	do i =1 to n;
		do j=1 to n;
			Xi1 = X1[i,]; Xj1 = X1[j,];
			Xi0 = X0[i,]; Xj0 = X0[j,];
			if &dist = 'extreme' then do;
				V1 = V1 + 1/N**2 * exp(Xi1 * beta - exp(Xi1 * beta) )*
				exp(Xj1 * beta - exp(Xj1 * beta) )* (xi1*V*T(xj1));
				V0 = V0 + 1/N**2 * exp(Xi0 * beta - exp(Xi0 * beta) )*
				exp(Xj0 * beta - exp(Xj0 * beta) )* (xi0*V*T(xj0));
				COV = cov + 1/N**2 * exp(Xi1 * beta - exp(Xi1 * beta) )*
				exp(Xj0 * beta - exp(Xj0 * beta) )* (xi1*V*T(xj0));
			end;
			else do;
				V1 = V1 + 1/N**2*pdf(&dist, xi1* beta) *
				pdf(&dist, xj1* beta) * (xi1*V*T(xj1));
				V0 = V0 + 1/N**2*pdf(&dist, xi0* beta) *
				pdf(&dist, xj0* beta) * (xi0*V*T(xj0));
				COV = COV + 1/N**2* pdf(&dist, xi1* beta)*
				pdf(&dist, xj0* beta) * (xi1*V*T(xj0));
			end;
		end; *END J;
	end; *END I;
	ll = p1est-p0est - &z*sqrt(v1+v0-2*cov);
	uu = p1est-p0est + &z*sqrt(v1+v0-2*cov);
	group1 = CI4P(p1est, V1, n); **CI for exposed risk;
	group0 = CI4P(p0est, V0, n); **CI for unexposed risk;
	print "==== CI for risks ===";
	name = {estimate Wald_L wald_U log_L log_U lgt_L lgt_U Wlsn_L Wlsn_U};
	print group1[colname=name],
	group0[colname=name];

	create MyData var {group1, group0, rho}; /** create data set **/
	append;       /** write data in vectors **/
	close MyData; /** close the data set **/
	start MOVER(p1, l1, u1, p2, l2, u2, corr);

	**Confidence interval for difference;
	rho = COV/sqrt(V1*V0);
	dWald = mover(group1[1], group1[2], group1[3], group0[1], group0[2], group0[3], rho);
	dlgt = mover(group1[1], group1[6], group1[7], group0[1], group0[6], group0[7], rho);
	dWln = mover(group1[1], group1[8], group1[9], group0[1], group0[8], group0[9], rho);
	**The following is based on Zou and Donner (2004 Controlled Clin Trials 25: 3-12);
	vd = V1 + V0 - 2*cov;
	d = p1est - p0est;
	F_LL = log( (1+ d)/(1-d) ) - &z *2*sqrt(vd)/(1- d**2);
	F_L = (exp(F_LL) - 1)/(exp(F_LL) + 1);
	F_Uu = log( (1+ d)/(1-d) ) + &z *2* sqrt(vd)/(1-d**2);
	F_U = (exp(F_uu) - 1)/(exp(F_uu) + 1);
	Fisher = d||F_L||F_U;
	print "===CI for difference==";
	print dwald, dlgt, dwln, Fisher;

	** CI for Ratio;
	**log-delta;
	RRest = p1est/p0est;
	var = V1/p1est**2 + V0/p0est**2 - 2 * cov/(p1est*p0est);
	Lower = RRest*exp(-&z*sqrt(var));
	Upper = RRest*exp(+&z*sqrt(var));
	RlogDelta = RRest||lower||upper;
	* Zou and Donner (2008 Stat Med 27: 1693-1702);
	Rlgt = exp(mover(log(group1[1]), log(group1[6]), log(group1[7]),
	log(group0[1]), log(group0[6]), log(group0[7]), rho));
	RWln = exp(mover(log(group1[1]), log(group1[8]), log(group1[9]),
	log(group0[1]), log(group0[8]), log(group0[9]), rho));
	print "=== CI for RR ===";
	print RlogDelta, Rlgt, Rwln;
quit;
%mend CounterFactual;

%CounterFactual(data =Flies, /*data set to be analyzed*/
yvar = death40, /*outcome variabe*/
trt = treatment, /* Exposure or treatment variable */
xvar = thorax, /* independent variables to be adjusted */
link = logit, /* link function used by PROC GENMOD: logit, probit, cloglog */
alpha=0.05); /* alpha level for confidence interval, default is 0.05*/
