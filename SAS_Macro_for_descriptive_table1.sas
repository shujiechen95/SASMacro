/*Part I: create frequency tables*/
/*for col-variables who have level = (0,1,...)!!!!!*/
/*indata: name of input dataset;
outcome: name of col-variable;
nfreql: # of levels of col-variables;
outdata: name of output dataset*/
%macro output(indata,outcome,nfreql,outdata);
ods output onewayfreqs = freq;
proc freq data = &indata.;table &outcome.;run;
data freq;
	set freq;
	tmpv = &outcome.+1;
	length &outcome.t $20.;
	&outcome.t = cats(vname(&outcome.),tmpv);
	drop &outcome.;
run;

/*get overall #*/
data freq;
	set freq;
	&outcome. = &outcome.t;
	length freq1 pct1 $40.;
	freq1 = compress(put(frequency,nlnum32.0));
	pct1 = compress(cats(put(percent,6.2),"%"));
run;
proc transpose data = freq(keep=freq1 pct1 &outcome.) out = freq1;
	id &outcome.;
	var freq1 pct1;
run;
data &outdata.;
	retain characteristic overall &outcome.1-&outcome.&nfreql.;
	set freq1;
	length characteristic overall $40.;
	characteristic = "";
	array c &outcome.1-&outcome.&nfreql.;
	overall0 = 0;
	%do i = 1 %to &nfreql.;
		overall0 = overall0 + input(c[&i.],comma10.);
	%end;
	overall = put(overall0,nlnum32.0);
/*	if at the line with percent, then let overall be empty. i.e. not need to report percent for overall*/
	if find(&outcome.1, "%", "i") ge 1 then overall = "";
	keep characteristic overall &outcome.1-&outcome.&nfreql.;
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
/*data out;*/
/*run;*/
ods output CrossTabFreqs = freq chisq=chisq;
proc freq data = &indata.;
	table &varlevel.*&varfreq./chisq;
run;
data out;
retain characteristic;
	set chisq;
	where statistic = 'Chi-Square';
	pvalue&varfreq.=vvalue(prob);
	length characteristic $40.;
	characteristic = vname(&varlevel.);
	keep characteristic pvalue&varfreq.;
run;
data output0;
	set freq;
	retain characteristic overall;
	length characteristic overall $40.;
	where &varlevel. ne . and &varfreq. = .;
	overall = put(frequency,nlnum32.0)||' ('||compress(put(percent,6.2))||')';
	characteristic=vvalue(&varlevel.);
	keep overall characteristic;
run;
%do i = 1 %to &nfreql.;
%let j = &i.-1;
data output&i.;
	set freq;
	retain characteristic &varfreq.&i.;
	length characteristic &varfreq.&i. $40.;
	where &varlevel. ne . and &varfreq. = &j.;
	&varfreq.&i. = put(frequency,nlnum32.0)||' ('||compress(put(colpercent,6.2))||')';
	characteristic=vvalue(&varlevel.);
	keep &varfreq.&i. characteristic;
run;
%end;
data output;
	merge output0 output1-output&nfreql.;
run;
data output;
	set out output;
/*	if characteristic = '' then characteristic = vname(&varlevel.);*/
run;
data &outdata.;
	retain characteristic overall &varfreq.1-&varfreq.&nfreql. pvalue&varfreq.;
	set &outdata. output;
	keep characteristic overall &varfreq.1-&varfreq.&nfreql. pvalue&varfreq.;
	if characteristic = '' and &varfreq.1 = '' then delete;
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




/*fill missing as 0 for binary variable*/
%macro nomiss(missdata);
%local count var;
%let count = 1;
%do %while(%scan(&misslist,&count," ") ne %str());
	%let var = %scan(&misslist,&count," ");
	%let count = %eval(&count+1);
	data &missdata.;
		set &missdata.;
		if &var. = . then &var. = 0;
	run;
%end;
%mend;
