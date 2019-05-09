data colorshapes2;
set out.color_shapes;
newdate=datepart(time);
newtime=timepart(time);
if trial_type=button_pressed then CScorr=1;
else if trial_type ne button_pressed then CScorr=0;
if id=. then delete;
if id>=12000 then cohort=1;/*new cohort*/
if id<12000 then cohort=0;/*old cohort*/
if id=10769 or id=9198 or id=10948 or id=10974 or id=10999 then flag_error=1;/*date/time errors during EMA*/
run;

/*get cut offs for response time: cut off at 9000 loses 1.2% of data (could go up to 10000 for consistency with other tasks)

proc univariate data=colorshapes2;
var response_time;
where response_time<9000;
run;*/

*you will need to clean out duplicates. I found a bunch, and they were true duplicates with repeated identifical data. 
You will need code similar to the proc below for EACH of the cognitive tests.;

****burst 1;
data startdates1;
set startdates1; *assumes startdates sas file already exists;
run;
*this merges the burst start dates with the color dot data;
proc sort data=startdates1 out=startdates1;by id;run;
proc sort data=colorshapes2 out=colorshapes2;by id session newdate newtime trial_num;run;

data colorshapes3;
merge colorshapes2 startdates1;
by id;
if newdate>=burststart_date then studyday=intck('day',burststart_date,newdate)+1; *Returns the number of days that lie between two dates;
if newdate<burststart_date then studyday=intck('day',burststart_date,newdate); *pre-study days will be given negative values*;
if 0<studyday<15 then flag_studyday=1;else flag_studyday=0;
weekday=weekday(newdate);
if newdate=. then delete;
if studyday>30 or studyday<-30 then delete; *delete cases from other bursts(beyound 1 month earlier or later);
run;

proc sort data=colorshapes3 out=b1clean_shape_trial nodupkey;
by id session newtime trial_num CScorr;
run;

data out.b1clean_shape_trial_&sysdate.;
set b1clean_shape_trial;
run; 


****burst 2;
data startdates2;
set startdates2; *assumes startdates sas file already exists;
run;
*this merges the burst start dates with the color dot data;
proc sort data=startdates2 out=startdates2;by id;run;
proc sort data=colorshapes2 out=colorshapes2;by id session newdate newtime trial_num;run;

data colorshapes3;
merge colorshapes2 startdates2;
by id;
if newdate>=burststart_date then studyday=intck('day',burststart_date,newdate)+1; *Returns the number of days that lie between two dates;
if newdate<burststart_date then studyday=intck('day',burststart_date,newdate); *pre-study days will be given negative values*;
if 0<studyday<15 then flag_studyday=1;else flag_studyday=0;
weekday=weekday(newdate);
if newdate=. then delete;
if studyday>30 or studyday<-30 then delete; *delete cases from other bursts(beyound 1 month earlier or later);
run;

proc sort data=colorshapes3 out=b2clean_shape_trial nodupkey;
by id session newtime trial_num CScorr;
run;

data out.b2clean_shape_trial_&sysdate.;
set b2clean_shape_trial;
run; 




*****add the new color shape script from here;

***burst 1;

*rescore colorshapes;
data color_shapes;
set out.b1clean_shape_trial_&sysdate.;
if trial_type=1 then cs_hit=0;
if trial_type=0 then cs_fa=0;
if trial_type=1 then cs_hit=button_pressed=1;*this codes for hits;
if trial_type=0 then cs_fa=button_pressed=1;*this codes for false alarms;
if trial_type=1 then cs_miss=button_pressed=0;*this codes for misses;
if trial_type=0 then cs_crej=button_pressed=0;*this code for correct rejection;
CS_error=CScorr=0;*this code for error of any time--needed for a LISAS scoring method;
run;

proc sort data=color_shapes out=colorshapes3a nodupkey;
by id session trial_num;
run;

proc means data=color_shapes nway noprint;
class id session;
var CScorr CS_fa CS_hit CS_error;
output out=CSacc mean(CScorr)=CSper sum(CScorr)=CStot mean(CS_FA)=CS_FAper sum(CS_FA)=CS_FAtot mean(CS_hit)=CS_HITper 
std(CS_error)=CS_Err_std mean(CS_error)=CS_err_mn mean(cs_miss)=CS_missPer mean(cs_crej)=CS_cRejPer
sum(CS_hit)=CS_HITtot;
run;

proc sort data=csacc;by id session;run;

****************************************************NOTE: need to calculate CSrt_mn CSrt_sd;

data out.b1clean_shape_session_&sysdate.;
set CSacc;
by id session;
color_shape_qual=_freq_=5;/*each session should include 5 trails*/
drop _type_ ;
cs_pr=cs_hitper-cs_faper;
cs_br=cs_faPer/(1-cs_pr);
if cs_br=. and cs_faPer ne . and cs_pr ne . then cs_br=0;
cs_lisas=CSrt_mn+(CSrt_sd/CS_err_std)*CS_err_mn;
color_shapes_qual=_freq_=6;
if _freq_=5 then color_shapes_qual=1;
label cs_pr='cs_pr(H-FA)' cs_br='cs_br(FA/[1-pr])';
run;

proc means data=out.b1clean_shape_session_&sysdate. noprint nway;
class id;where color_shapes_qual=1;
var cs_pr cs_br cs_hitper cs_faper;
output out=out.b1clean_shape_person_&sysdate. mean(cs_pr)= mean(cs_br)=  mean(cs_hitper)= mean(cs_faper)=;
run;



**burst 2;

*rescore colorshapes;
data color_shapes;
set out.b2clean_shape_trial_&sysdate.;
if trial_type=1 then cs_hit=0;
if trial_type=0 then cs_fa=0;
if trial_type=1 then cs_hit=button_pressed=1;*this codes for hits;
if trial_type=0 then cs_fa=button_pressed=1;*this codes for false alarms;
if trial_type=1 then cs_miss=button_pressed=0;*this codes for misses;
if trial_type=0 then cs_crej=button_pressed=0;*this code for correct rejection;
CS_error=CScorr=0;*this code for error of any time--needed for a LISAS scoring method;
run;

proc sort data=color_shapes out=colorshapes3a nodupkey;
by id session trial_num;
run;

proc means data=color_shapes nway noprint;
class id session;
var CScorr CS_fa CS_hit CS_error;
output out=CSacc mean(CScorr)=CSper sum(CScorr)=CStot mean(CS_FA)=CS_FAper sum(CS_FA)=CS_FAtot mean(CS_hit)=CS_HITper 
std(CS_error)=CS_Err_std mean(CS_error)=CS_err_mn mean(cs_miss)=CS_missPer mean(cs_crej)=CS_cRejPer
sum(CS_hit)=CS_HITtot;
run;

proc sort data=csacc;by id session;run;

data out.b2clean_shape_session_&sysdate.;
set CSacc;
by id session;
color_shape_qual=_freq_=5;/*each session should include 5 trails*/
drop _type_ ;
cs_pr=cs_hitper-cs_faper;
cs_br=cs_faPer/(1-cs_pr);
if cs_br=. and cs_faPer ne . and cs_pr ne . then cs_br=0;
cs_lisas=CSrt_mn+(CSrt_sd/CS_err_std)*CS_err_mn;
color_shapes_qual=_freq_=6;
if _freq_=5 then color_shapes_qual=1;
label cs_pr='cs_pr(H-FA)' cs_br='cs_br(FA/[1-pr])';
run;

proc means data=out.b2clean_shape_session_&sysdate. noprint nway;
class id;where color_shapes_qual=1;
var cs_pr cs_br cs_hitper cs_faper;
output out=out.b2clean_shape_person_&sysdate. mean(cs_pr)= mean(cs_br)=  mean(cs_hitper)= mean(cs_faper)=;
run;


