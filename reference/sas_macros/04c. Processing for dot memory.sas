data dot_memory2;
set out.dot_memory;
r1=dot1+1;
r2=dot2+1;
r3=dot3+1;
t1=resp1+1;
t2=resp2+1;
t3=resp3+1;
run;

%macro coords(var=);
select (&var);
	when (1,6,11,16,21) &var.x=1;
	when (2,7,12,17,22) &var.x=2;
	when (3,8,13,18,23) &var.x=3;
	when (4,9,14,19,24) &var.x=4;
	when (5,10,15,20,25) &var.x=5;
	otherwise;
end;
select (&var);
	when (1,2,3,4,5) &var.y=1;
	when (6,7,8,9,10) &var.y=2;
	when (11,12,13,14,15) &var.y=3;
	when (16,17,18,19,20) &var.y=4;
	when (21,22,23,24,25) &var.y=5;
	otherwise;
end;
%mend coords;

*this macro calculates distances for the two input variables, based on
the distances computed by the above %coords macro;

%macro distance(r=,t=);
&r.&t.=sqrt( ((&r.x-&t.x)**2) + ((&r.y-&t.y)**2)   );
%mend distance;


data coords2;
set dot_memory2;
*these macros assign xy coords to the grids;
%coords(var=r1);%coords(var=r2);%coords(var=r3);
%coords(var=t1);%coords(var=t2);%coords(var=t3);
*ab1=sqrt( ((ax-bx)**2) + ((ay-by)**2)   ) ;*<--this is distance formula;
run;


data coords3;
set coords2;
*these macros compute distance between each response and each target;
%distance(r=r1,t=t1);
%distance(r=r1,t=t2);
%distance(r=r1,t=t3);

%distance(r=r2,t=t1);
%distance(r=r2,t=t2);
%distance(r=r2,t=t3);

%distance(r=r3,t=t1);
%distance(r=r3,t=t2);
%distance(r=r3,t=t3);

*this code removes a correctly selected target from subsequent distance calculations;
if r1t1=0 then do;
r1_d=0;r2t1=99;r3t1=99;
end;
if r1t2=0 then do;
r1_d=0;r2t2=99;r3t2=99;
end;
if r1t3=0 then do;
r1_d=0;r2t3=99;r3t3=99;
end;

if r2t1=0 then do;
r2_d=0;r1t1=99;r3t1=99;
end;
if r2t2=0 then do;
r2_d=0;r1t2=99;r3t2=99;
end;
if r2t3=0 then do;
r2_d=0;r1t3=99;r3t3=99;
end;

if r3t1=0 then do;
r3_d=0;r1t1=99;r2t1=99;
end;
if r3t2=0 then do;
r3_d=0;r1t2=99;r2t2=99;
end;
if r3t3=0 then do;
r3_d=0;r1t3=99;r2t3=99;
end;

*this code assigns a distance score for each response based on its minimum
 distance from any target not selected by another response;
r1_d=min(of r1t1,r1t2,r1t3,r1_d);
r2_d=min(of r2t1,r2t2,r2t3,r2_d);
r3_d=min(of r3t1,r3t2,r3t3,r3_d);
error=sum(of r1_d,r2_d,r3_d);
*error is the sum of the euclidean distances for all 3 responses. So a 0 means they 
got all 3 correct;
run;

data dot_memory3;
set coords3;
/*get date and time separately*/
newdate=datepart(time);
newtime=timepart(time);
if id=. then delete;
if id>=12000 then cohort=1;/*new cohort*/
if id<12000 then cohort=0;/*old cohort*/
if id=10769 or id=9198 or id=10948 or id=10974 or id=10999 then flag_error=1;/*date/time errors during EMA*/

run;

*you will need to clean out duplicates. I found a bunch, and they were true duplicates with repeated identifical data. 
You will need code similar to the proc below for EACH of the cognitive tests.;

****burst 1;

data startdates1;
set startdates1; *assumes startdates sas file already exists;
run;
*this merges the burst start dates with the color dot data;
proc sort data=startdates1 out=startdates1;by id;run;
proc sort data=dot_memory3 out=dot_memory3;by id session newdate newtime trial_num;run;

data dot_memory4;
merge dot_memory3 startdates1;
by id;
if newdate>=burststart_date then studyday=intck('day',burststart_date,newdate)+1; *Returns the number of days that lie between two dates;
if newdate<burststart_date then studyday=intck('day',burststart_date,newdate); *pre-study days will be given negative values*;
if 0<studyday<15 then flag_studyday=1;else flag_studyday=0;
weekday=weekday(newdate);
if newdate=. then delete;
if studyday>30 or studyday<-30 then delete; *delete cases from other bursts(beyound 1 month earlier or later);
run;

proc sort data=dot_memory4 out=b1clean_memory_trial nodupkey;
by id session newtime trial_num error;
run;

data out.b1clean_memory_trail_&sysdate.;
set b1clean_memory_trial;
run; 

proc means data=b1clean_memory_trial nway noprint;
class id session;
id newdate newtime;
var error studyday flag_studyday cohort weekday;
output out=b1clean_dot mean(error response_time)=spat_dmn rt_mn sum(error)=spat_dsum median(response_time)=rt_med std(response_time error)=rt_std spat_dsd
min(studyday)=studyday min(flag_studyday)=flag_studyday min(cohort)=cohort min(weekday)=weekday;
run;

data out.b1clean_memory_session_&sysdate.;
set b1clean_dot;
memory_qual=_freq_=2;/*each session should include 2 trails*/
drop _type_ _freq_;
run;


***burst 2;
data startdates2;
set startdates2; *assumes startdates sas file already exists;
run;
*this merges the burst start dates with the color dot data;
proc sort data=startdates2 out=startdates2;by id;run;
proc sort data=dot_memory3 out=dot_memory3;by id session newdate newtime trial_num;run;

data dot_memory4;
merge dot_memory3 startdates2;
by id;
if newdate>=burststart_date then studyday=intck('day',burststart_date,newdate)+1; *Returns the number of days that lie between two dates;
if newdate<burststart_date then studyday=intck('day',burststart_date,newdate); *pre-study days will be given negative values*;
if 0<studyday<15 then flag_studyday=1;else flag_studyday=0;
weekday=weekday(newdate);
if newdate=. then delete;
if studyday>30 or studyday<-30 then delete; *delete cases from other bursts(beyound 1 month earlier or later);
run;

proc sort data=dot_memory4 out=b2clean_memory_trial nodupkey;
by id session newtime trial_num error;
run;

data out.b2clean_memory_trail_&sysdate.;
set b2clean_memory_trial;
run; 

proc means data=b2clean_memory_trial nway noprint;
class id session;
id newdate newtime;
var error studyday flag_studyday cohort weekday;
output out=b2clean_dot mean(error response_time)=spat_dmn rt_mn sum(error)=spat_dsum median(response_time)=rt_med std(response_time error)=rt_std spat_dsd
min(studyday)=studyday min(flag_studyday)=flag_studyday min(cohort)=cohort min(weekday)=weekday;
run;

data out.b2clean_memory_session_&sysdate.;
set b2clean_dot;
memory_qual=_freq_=2;/*each session should include 2 trails*/
drop _type_ _freq_;
run;
