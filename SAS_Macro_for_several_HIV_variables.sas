/*SAS programmer: Shujie Chen*/
/*Date:OCT2019-FEB2020*/
/*Task: macro for defining HIV related variavles; create tables; create figures*/



/*Part I: definition for ICDcode-related diseases/ retention in care/ percent days with ...*/
/*define macro: make and sort the reference ICD code dataset*/
PROC OPTIONS OPTION=MACRO; 
RUN;
/*create binary diseases related variables*/
/*diag: input data with sorted icd code
diagname: input reference icd cod list
disease_name: name of output data
colname: variable in the icd code list*/
%macro timedisease(diag,diagname,disease_name,colname);
/*delete the decimal point of the ICD code in the reference file(the ICD code in our original database has no decimal point)*/
data diagname;
	set &diagname.;
run;
proc sort data = diagname;by code diag;run;
data diag;
	set &diag.;
run;
proc sql;
	create table long&disease_name. as select *
	from diag inner join diagname 
	on diag.diag like trim(diagname.diag) and diag.code=diagname.code;
quit;
/*to get binary diseases related variables*/
proc sort data = long&disease_name. out = long&disease_name. nodupkey;by rfa_id time &colname.;run;/*longitudinal; sort by time (visit_id)*/
proc sort data = long&disease_name. out = uni&disease_name. nodupkey;by rfa_id &colname.;run;/*unique disease; keep the first time*/
proc transpose data = uni&disease_name. out = uni2&disease_name.;
	by rfa_id;
	id &colname.;
	var time;
run;
data &disease_name.;
	set uni2&disease_name.;
	array c{*} _numeric_;
	do i = 2 to dim(c);
    	if c{i} ne . then c{i} = 1;else c{i} = 0;
	end;
	drop i _name_;
run;
%mend;

%macro timediseasepart(diag,diagname,disease_name,colname);
/*delete the decimal point of the ICD code in the reference file(the ICD code in our original database has no decimal point)*/
data diagname;
	set &diagname.;
run;
proc sort data = diagname;by code diag;run;
data diag;
	set &diag.;
run;
proc sql;
	create table long&disease_name. as select *
	from diag inner join diagname 
	on diag.diag like trim(diagname.diag) and diag.code=diagname.code;
quit;
%mend;

%macro timedisease0(diag,diagname,disease_name,colname);
/*delete the decimal point of the ICD code in the reference file(the ICD code in our original database has no decimal point)*/
data diagname;
	set &diagname.;
run;
data diagname1;
	set diagname;
	where ICD10_code ne '';
run;
proc sort data = diagname1;by ICD10_code;run;
proc sql;
	create table diagnameicd9 as select count(ICD10_code) as n, ICD10_code from diagname1 group by ICD10_code;
run;
/*call macro: create the reference dataset for ICD9 code and ICD10 code */
%ref_ICD_code(ref_ICD9code,ICD9_Code,&colname.); /*for ICD9*/
%ref_ICD_code(ref_ICD10code,ICD10_Code,&colname.); /*for ICD10*/
data diag;
	set &diag.;
run;
proc sql;
	create table diagname9 as select *
	from diag inner join ref_ICD9code 
	on diag.diag like catt(ref_ICD9code.icd9_code,"%");
run;
proc sql;
	create table diagname10 as select *
	from diag inner join ref_ICD10code 
	on diag.diag like catt(ref_ICD10code.icd10_code,"%");
run;
data long&disease_name.;set diagname9 diagname10;run;
/*to get binary diseases related variables*/
proc sort data = long&disease_name. out = long&disease_name. nodupkey;by rfa_id time &colname.;run;/*longitudinal; sort by time (visit_id)*/
proc sort data = long&disease_name. out = uni&disease_name. nodupkey;by rfa_id &colname.;run;/*unique disease; keep the first time*/
proc transpose data = uni&disease_name. out = uni2&disease_name.;
	by rfa_id;
	id &colname.;
	var time;
run;
data &disease_name.;
	set uni2&disease_name.;
	array c{*} _numeric_;
	do i = 2 to dim(c);
    	if c{i} ne . then c{i} = 1;else c{i} = 0;
	end;
	drop i _name_;
run;
%mend;



/*retention in care by follow-up year*/
%macro incare(tests,out,endyr,endmth);
data labyr;
	set &tests.(in=a where=(test in ('CD4#', 'VL') and time_dxdate_testdate>0)) ;
	by rfa_id;
	month = input(put(input(catt(substr(TESTDATE,1,4),'1960'),monyy.),month.),2.);/*grasp month of test*/
	if input(substr(TESTDATE,5,2),8.)>22 then year = input(substr(TESTDATE,5,2),8.)-100;
		else year = input(substr(TESTDATE,5,2),8.);/*grasp year of test*/
	remonth = year*12+month;/*define remonth as the months after JAN01 2000*/
run;
/*select first visit date (linkage date) and last visit date for each patient*/
proc sql;
	create table range1 as select min(remonth) as minmth, max(remonth) as maxmth, RFA_ID 
		from labyr group by RFA_ID;
quit;
run;/*9721 patients have tests*/
data deathyr;
	merge range1(in=a keep = rfa_id minmth) cases(in=b keep=rfa_id death_hars);
	by rfa_id;
	if a;
	if death_hars ne '' then do;
		dmonth = input(substr(death_hars,5,2),8.);
		if input(substr(death_hars,3,2),8.)>22 then dyear = input(substr(death_hars,3,2),8.)-100;
			else dyear = input(substr(death_hars,3,2),8.);
	end;
	else do;
		dmonth = &endmth.;
		dyear = &endyr.;
	end;
	dremonth = dyear*12+dmonth;
	dgapmth = dremonth - minmth;
	longyr = ceil(dgapmth/12);
	keep rfa_id longyr;
run;
proc sql;
	create table maxlongyr as select max(longyr) as maxlongyr from deathyr;
run;
data maxlongyear;
	set maxlongyr;
	do yr = 1 to maxlongyr;
		output;
	end;
	drop maxlongyr;
run;
proc sql;
	create table deathyr1 as select * from deathyr right join maxlongyear 
	on deathyr.longyr>=maxlongyear.yr;
run;
/*merge 'range1' and 'labyr' to get gap month between linkage date and test date for these patients*/
data range2;
	merge labyr(in=a) range1(in=b);
	by rfa_id;
	gapmth = remonth - minmth;
	yr = ceil(gapmth/12);
	if yr = 0 then yr = 1;
run;
proc sql;
	create table range4 as select min(gapmth) as minmthyr, max(gapmth) as maxmthyr, yr, RFA_ID 
		from range2 group by yr, RFA_ID;
quit;
run;
/*define whether in care or not in each year*/
data incyr;
	set range4;
	if maxmthyr - minmthyr >= 3 then incare = 1;
	else incare = 0;
run;
proc sort data = incyr;by rfa_id;run;
data &out.;
	merge incyr(in=a) deathyr1(in=b);
	by rfa_id yr;
	if b;
	if incare = . then incare = 0;
	if yr = longyr and incare = 0 then delete;
run;
%mend;



/*calculate days with observed definition like suppression/with low CD4 count*/
%macro timewithobs(indata,outdata,id,obsvar,seq,time,endata,entime);
data sup2;
	set &indata.;
	&seq. = &seq.-1;
	l2 = &TIME.;
	keep &id. &seq. l2;
run;
data sup3;
	set &indata.;
	&seq. = &seq.+1;
	l0 = &TIME.;
	keep &id. &seq. l0;
run;
data lsup;
	merge &indata.(in=a) sup2(in=b) sup3(in=c);
	by &id. &seq.;
	if a;
run;
data lsup;
	merge lsup(in=a) &endata.(in=b);
	by &id.;
	if a;
	l1 = &TIME.;
	if l0 = . then l0 = 0;
	if l2 = . and l1 <= &entime. then l2 = &entime.;
		else if l2 = . and l1 > &entime. then l2 = l1;
run;
/*calculate days with suppression*/
data tlsup;
	set lsup;
	where &obsvar. = 1;
	tsup = (l2-l0)/2;
run;
proc sql;
	create table &outdata. as select sum(tsup) as &outdata., &id. from tlsup group by &id.;
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




