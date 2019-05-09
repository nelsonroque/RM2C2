/*check for outliers: cut offs 200ms and 10000, nixes 1%
proc univariate data=data.symbols;
var response_time;
where response_time>200 and response_time<10000;
run;*/

data symbols2;
set out.symbols;
/*get date and time separately*/
newdate=datepart(time);
newtime=timepart(time);
if user_response=correct_response then symcorr=1;
else if user_response ne correct_response then symcorr=0;
if id=. then delete;
if id>=12000 then cohort=1;/*new cohort*/
if id<12000 then cohort=0;/*old cohort*/
if id=10769 or id=9198 or id=10948 or id=10974 or id=10999 then flag_error=1;/*date/time errors during EMA*/
run;

*you will need to clean out duplicates. I found a bunch, and they were true duplicates with repeated identifical data. 
You will need code similar to the proc below for EACH of the cognitive tests.;

****burst1;
data startdates1;
set startdates1; *assumes startdates sas file already exists;
run;
*this merges the burst start dates with the color dot data;
proc sort data=startdates1 out=startdates1;by id;run;
proc sort data=symbols2 out=symbols2;by id session newdate newtime trial_num;run;

data symbols3;
merge symbols2 startdates1;
by id;
if newdate>=burststart_date then studyday=intck('day',burststart_date,newdate)+1; *Returns the number of days that lie between two dates;
if newdate<burststart_date then studyday=intck('day',burststart_date,newdate); *pre-study days will be given negative values*;
if 0<studyday<15 then flag_studyday=1;else flag_studyday=0;
weekday=weekday(newdate);
if newdate=. then delete;
if studyday>30 or studyday<-30 then delete; *delete cases from other bursts(beyound 1 month earlier or later);
run;

proc sort data=symbols3 out=b1clean_symbol_trial nodupkey;
by id session newtime trial_num symcorr response_time;
run;

data out.b1clean_symbol_trial_&sysdate.;
set b1clean_symbol_trial;
run; 

proc means data=b1clean_symbol_trial nway noprint;
class id session;
id newdate newtime;
var response_time studyday flag_studyday cohort weekday;
where response_time>200 and response_time<10000 and symcorr=1;output out=b1symrt mean(response_time)=symrt_mn median(response_time)=symrt_med
std(response_time)=symrt_sd
min(studyday)=studyday min(flag_studyday)=flag_studyday min(cohort)=cohort min(weekday)=weekday;
run;

proc means data=b1clean_symbol_trial nway noprint;
class id session;
var symcorr;
output out=b1symacc mean(symcorr)=symacc sum(symcorr)=symtot;
run;

data out.b1clean_symbols_session_&sysdate.;
merge b1symrt b1symacc;
by id session;
symbol_qual=_freq_=11;/*each session should include 11 trails*/
drop _type_ _freq_;
run;


****burst2;
data startdates2;
set startdates2; *assumes startdates sas file already exists;
run;
*this merges the burst start dates with the color dot data;
proc sort data=startdates2 out=startdates2;by id;run;
proc sort data=symbols2 out=symbols2;by id session newdate newtime trial_num;run;

data symbols3;
merge symbols2 startdates2;
by id;
if newdate>=burststart_date then studyday=intck('day',burststart_date,newdate)+1; *Returns the number of days that lie between two dates;
if newdate<burststart_date then studyday=intck('day',burststart_date,newdate); *pre-study days will be given negative values*;
if 0<studyday<15 then flag_studyday=1;else flag_studyday=0;
weekday=weekday(newdate);
if newdate=. then delete;
if studyday>30 or studyday<-30 then delete; *delete cases from other bursts(beyound 1 month earlier or later);
run;

proc sort data=symbols3 out=b2clean_symbol_trial nodupkey;
by id session newtime trial_num symcorr response_time;
run;

data out.b2clean_symbol_trial_&sysdate.;
set b2clean_symbol_trial;
run; 

proc means data=b2clean_symbol_trial nway noprint;
class id session;
id newdate newtime;
var response_time studyday flag_studyday cohort weekday;
where response_time>200 and response_time<10000 and symcorr=1;output out=b2symrt mean(response_time)=symrt_mn median(response_time)=symrt_med
std(response_time)=symrt_sd
min(studyday)=studyday min(flag_studyday)=flag_studyday min(cohort)=cohort min(weekday)=weekday;
run;

proc means data=b2clean_symbol_trial nway noprint;
class id session;
var symcorr;
output out=b2symacc mean(symcorr)=symacc sum(symcorr)=symtot;
run;

data out.b2clean_symbols_session_&sysdate.;
merge b2symrt b2symacc;
by id session;
symbol_qual=_freq_=11;/*each session should include 11 trails*/
drop _type_ _freq_;
run;
