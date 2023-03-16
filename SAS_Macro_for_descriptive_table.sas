/*Part I: create frequency tables*/
/*for col-variables who have level = (0,1,...)*/
/*indata: name of input dataset;
outcome: name of col-variable;
nfreql: # of levels of col-variables;
outdata: name of output dataset*/
%macro output(indata,outcome,nfreql,outdata);
data &outdata.;
run;
%local count var cvar;
%let count = 1;
%do %while(%scan(&charlist,&count," ") ne %str());
	%let var = %scan(&charlist,&count," ");
	%let count = %eval(&count+1);
%compcrossfreq(&indata.,&var.,&nfreql.,&outcome.,&outdata.);
%end;
%mend;
/*indata: name of input dataset;
varlevel: name of row-variable;
nfreql: # of levels of col-variables;
varfreq: name of col-variables;
outdata: name of output dataset*/
%macro compcrossfreq(indata,varlevel,nfreql,varfreq,outdata);
data out;
run;
ods output CrossTabFreqs = freq chisq=chisq;
proc freq data = &indata.;
	table &varlevel.*&varfreq./chisq;
run;
data out;
	set chisq;
	where statistic = 'Chi-Square';
	pvalue&varfreq.=vvalue(prob);
	keep pvalue&varfreq.;
run;
data output0;
	set freq;
	retain characteristic overall;
	where &varlevel. ne . and &varfreq. = .;
	overall = catt(frequency,' (',round(percent,.01),')');
	length characteristic $40.;
	characteristic=vvalue(&varlevel.);
	keep overall characteristic;
run;
%do i = 1 %to &nfreql.;
%let j = &i.-1;
data output&i.;
	set freq;
	retain characteristic &varfreq.&i.;
	where &varlevel. ne . and &varfreq. = &j.;
	&varfreq.&i. = catt(frequency,' (',round(rowpercent,.01),')');
	length characteristic $40.;
	characteristic=vvalue(&varlevel.);
	keep &varfreq.&i. characteristic;
run;
%end;
data output;
	merge output0 output1-output&nfreql.;
run;
data output;
	set out output;
	if characteristic = '' then characteristic = vname(&varlevel.);
run;
data &outdata.;
	retain characteristic overall &varfreq.1-&varfreq.&nfreql. pvalue&varfreq.;
	set &outdata. output;
	keep characteristic overall &varfreq.1-&varfreq.&nfreql. pvalue&varfreq.;
	if characteristic = '' then delete;
run;
%mend;

/*Part II: create mean (std) tables*/
/*for col-variables who have level = (0,1,...)*/
/*input:
indata: name of input dataset;
varfreq: name of col-variable;
nfreql: # of levels of col-variables;
varnum: name of the numeric variable whose mean (std) we want;
outdata: name of output dataset*/
/*output:
0 column: name and levels of characteristics;
1st column: overall;
2nd column: level 0 (usually "No") of column variable;
3rd column: level 1 (usually "Yes") of column variable*/
%macro outputnum(indata,nfreql,varfreq,varnum,outdata);
data &outdata.;
run;
%local count var cvar;
%let count = 1;
%do %while(%scan(&charlist,&count," ") ne %str());
	%let var = %scan(&charlist,&count," ");
	%let count = %eval(&count+1);
%compcrossmean(&indata.,&var.,&nfreql.,&varfreq.,&varnum.,&outdata.);
%end;
%mend;
/*indata: name of input dataset;
varlevel: name of row-variable;
nfreql: # of levels of col-variables;
varfreq: name of col-variables;
varnum: name of the numeric variable whose mean (std) we want;
outdata: name of output dataset*/
%macro compcrossmean(indata,varlevel,nfreql,varfreq,varnum,outdata);
data out;
run;
ods output summary = tab1_0;
proc means data = &indata. mean std;var &varnum.;class &varlevel.;run;
data output0;
	retain characteristic msd;
	set tab1_0;
	characteristic = vvalue(&varlevel.);
	length msd $30.;
	msd = compress(put(&varnum._mean,6.2))||" ("||compress(put(&varnum._stddev,6.2))||")";
	keep characteristic msd;
run;
%do i = 1 %to &nfreql.;
%let j = &i.-1;
ods output summary = tab1_0;
proc means data = &indata.(where=(&varfreq.=&j.)) mean std;var &varnum.;class &varlevel.;run;
data output&i.;
	retain characteristic msd&i.;
	set tab1_0;
	characteristic = vvalue(&varlevel.);
	length msd $30.;
	msd&i. = compress(put(&varnum._mean,6.2))||" ("||compress(put(&varnum._stddev,6.2))||")";
	keep characteristic msd&i.;
run;
%end;
data output;
	merge output0 output1-output&nfreql.;
run;
data output;
	set out output;
	if characteristic = '' then characteristic = vname(&varlevel.);
run;
data &outdata.;
	set &outdata. output;
	if characteristic = '' then delete;
	keep characteristic msd msd1-msd&nfreql.;
run;
%mend;



/*Part III: create test tables*/
/*input:
indata: name of input dataset;
outcome: name of outcomes;
varlevel: name of subgroup variables;
nvarl: number of levels of subgroup variables;
outdata: name of output dataset*/
/*output:
OR/HR of outcome between predictors*/
%macro outputtest(indata,varpred,varfreq,outdata,model);
data &outdata.;
run;
%local count var cvar;
%let count = 1;
%do %while(%scan(&charlist,&count," ") ne %str());
	%let var = %scan(&charlist,&count," ");
	%let nlvl = %scan(&nlvllist,&count," ");
	%let count = %eval(&count+1);
	%if &model. = "logistic" %then %do;
		%logittest(&indata.,&var.,&nlvl.,&varpred.,&varfreq.,&outdata.);
	%end;
	%if &model. = "coxph" %then %do;
		%coxphtest(&indata.,&var.,&nlvl.,&varpred.,&varfreq.,&outdata.);
	%end;
	%if &model. = "genmod" %then %do;
		%genmodtest(&indata.,&var.,&nlvl.,&varpred.,&varfreq.,&outdata.);
	%end;
%end;
%mend;
/*indata: name of input dataset;
varlevel: name of row-variable;
varfreq: name of col-variables;
varnum: name of the numeric variable whose mean (std) we want;
outdata: name of output dataset*/
%macro genmodtest(indata,varlevel,nlvl,varpred,varfreq,outdata);
data out;
run;
%do i = 1 %to &nlvl.;
%let j = &i.-1;
ods output GEEEmpPEst=est;
proc genmod data=&indata.(where=(&varlevel.=&j.)) descending;
/*need to change reference if necessary*/
	class rfa_id &varpred.(ref="0")/PARAM=GLM;
	model &varfreq.= &varpred. / dist=binomial link=logit;
/*need to change type of covariance matrix if necessary*/
	repeated subject=rfa_id / type=ar(1) corrw covb;
run;
data output&i.;
	set est;
	keep est p;
/*need to change condition for "where" if necessary*/
	where parm = "wicd" and level1 = "1";
	length est $30.;
	if estimate=0 and lowerCL=0 and upperCL=0 then do;
		est="Ref";
	end;
	else do;
		est = compress(put(exp(estimate),6.3))||" ("||compress(put(exp(lowerCL),6.3))||","||compress(put(exp(upperCL),6.3))||")";
	end;
	length p $7.;
	if probz = . then p=""; else if probz<0.0001 then p = "<0.0001"; else p = round(probz,.0001);
run;
%end;
data output;
	set out output1-output&nlvl.;
run;
data &outdata.;
	set &outdata. output;
run;
%mend;

%macro coxphtest(indata,varlevel,nlvl,varpred,varfreq,outdata);
data out;
run;
%do i = 1 %to &nlvl.;
%let j = &i.-1;
ods output ParameterEstimates=est;
proc phreg data = &indata.(where=(&varlevel.=&j.));
/*need to change reference if necessary*/
	class &varpred.(ref="before COVID-19");
/*need to change model (adjusted variables if necessary*/
	model etime*&varfreq(0) = &varpred. yr/RISKLIMITS=BOTH;
run;
data output&i.;
	set est;
	keep est p;
/*need to change condition for "where" if necessary*/
	where Parameter = "postcovid";
	length est $30.;
	est = compress(put(HazardRatio,6.3))||" ("||compress(put(HRLowerCL,6.3))||","||compress(put(HRUpperCL,6.3))||")";
	length p $7.;
	if ProbChiSq = . then p=""; else if ProbChiSq<0.0001 then p = "<0.0001"; else p = round(ProbChiSq,.0001);
run;
%end;
data output;
	set out output1-output&nlvl.;
run;
data &outdata.;
	set &outdata. output;
run;
%mend;

%macro logittest(indata,varlevel,nlvl,varpred,varfreq,outdata);
data out;
run;
%do i = 1 %to &nlvl.;
%let j = &i.-1;
ods output ParameterEstimates=est;
proc logistic data = &indata.(where=(&varlevel.=&j.));
/*need to change reference if necessary*/
	class &varpred.(ref="before COVID-19")/PARAM=GLM;
/*need to change model (adjusted variables if necessary*/
	model &varfreq(event='1') = &varpred. yr/orpvalue;
run;
data output&i.;
	set est;
	keep est p;
/*need to change condition for "where" if necessary*/
	where variable = "postcovid" and ClassVal0 = "during COVID-19";
	length est $30.;
	est = compress(put(exp(estimate),6.3))||" ("||compress(put(exp(estimate-1.96*stderr),6.3))||","||compress(put(exp(estimate+1.96*stderr),6.3))||")";
	length p $7.;
	if ProbChiSq = . then p=""; else if ProbChiSq<0.0001 then p = "<0.0001"; else p = round(ProbChiSq,.0001);
run;
%end;
data output;
	set out output1-output&nlvl.;
run;
data &outdata.;
	set &outdata. output;
run;
%mend;
