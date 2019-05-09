libname out "C:\Users\ruixue\Box Sync\Ruixue\PSU Projects\EAS study\data\processed\EMA\processed_06Dec2018";

data x;
set out.color_dots;
newdate=datepart(time);
newtime=timepart(time);
if id=. then delete;
if id>=12000 then cohort=1;/*new cohort*/
if id<12000 then cohort=0;/*old cohort*/
if id=10769 or id=9198 or id=10948 or id=10974 or id=10999 then flag_error=1;/*date/time errors during EMA*/


loc1_X=(finallocationa-loc1a)**2;
loc1_Y=(finallocationb-loc1b)**2;
loc1dist=sqrt(loc1_x+loc1_y);

loc2_X=(finallocationa-loc2a)**2;
loc2_Y=(finallocationb-loc2b)**2;
loc2dist=sqrt(loc2_x+loc2_y);

loc3_X=(finallocationa-loc3a)**2;
loc3_Y=(finallocationb-loc3b)**2;
loc3dist=sqrt(loc3_x+loc3_y);

if probedcolor=col1 then corrdist=loc1dist;
else if probedcolor=col2 then corrdist=loc2dist;
else if probedcolor=col3 then corrdist=loc3dist;

if probedlocation=loc1 and colorchoice=col1 then colorcorr=1;
else if probedlocation=loc2 and colorchoice=col2 then colorcorr=1;
else if probedlocation=loc3 and colorchoice=col3 then colorcorr=1;
else if colorcorr=. then colorcorr=0;

if probedcolor=col1 then corrdist=loc1dist;
else if probedcolor=col2 then corrdist=loc2dist;
else if probedcolor=col3 then corrdist=loc3dist;

corrmin=min(of loc1dist loc2dist loc3dist);
if corrdist=corrmin then swap_error=0;
else swap_error=1;
if swap_error=0 then corrdist2=corrdist;
run;

*you will need to clean out duplicates. I found a bunch, and they were true duplicates with repeated identifical data. 
You will need code similar to the proc below for EACH of the cognitive tests.;

**burst 1;
data startdates1;
set startdates1; *assumes startdates sas file already exists;
run;
*this merges the burst start dates with the color dot data;
proc sort data=startdates1 out=startdates1;by id;run;
proc sort data=X out=X;by id session newdate newtime trial_num;run;

data b1x2;
merge x startdates1;
by id;

if newdate>=burststart_date then studyday=intck('day',burststart_date,newdate)+1; *Returns the number of days that lie between two dates;
if newdate<burststart_date then studyday=intck('day',burststart_date,newdate); *pre-study days will be given negative values*;
if 0<studyday<15 then flag_studyday=1;else flag_studyday=0;
weekday=weekday(newdate);
if newdate=. then delete;
if studyday>30 or studyday<-30 then delete; *delete cases from other bursts(beyound 1 month earlier or later);
run;

proc sort data=b1x2 out=b1clean_colordot_trial nodupkey;
by id session newdate newtime trial_num corrdist;
run;

data out.b1clean_colordot_trial_&sysdate.;
set b1clean_colordot_trial;
run;

**burst 2;
data startdates2;
set startdates2; *assumes startdates sas file already exists;
run;
*this merges the burst start dates with the color dot data;
proc sort data=startdates2 out=startdates2;by id;run;
proc sort data=X out=X;by id session newdate newtime trial_num;run;

data b2x2;
merge x startdates2;
by id;

if newdate>=burststart_date then studyday=intck('day',burststart_date,newdate)+1; *Returns the number of days that lie between two dates;
if newdate<burststart_date then studyday=intck('day',burststart_date,newdate); *pre-study days will be given negative values*;
if 0<studyday<15 then flag_studyday=1;else flag_studyday=0;
weekday=weekday(newdate);
if newdate=. then delete;
if studyday>30 or studyday<-30 then delete; *delete cases from other bursts(beyound 1 month earlier or later);
run;

proc sort data=b2x2 out=b2clean_colordot_trial nodupkey;
by id session newdate newtime trial_num corrdist;
run;

data out.b2clean_colordot_trial_&sysdate.;
set b2clean_colordot_trial;
run;

*Please save the cleaned trial level data (from the above sort statement) to a permanent SAS data file. This will allow us to 
look at trial level analysis. Do this for each cognitive test; 

**burst1;

proc means nway noprint data=b1clean_colordot_trial;
class id session;
var newdate newtime colorcorr corrdist corrdist2 swap_error studyday flag_studyday cohort weekday;
output out=b1color_colordot_session min(newtime)=newtime min(newdate)=newdate min(studyday)=studyday min(flag_studyday)=flag_studyday min(cohort)=cohort
min(weekday)=weekday
mean(colorcorr)=color_dot_acc mean(corrdist)=color_dot_dist mean(corrdist2)=color_dot_dist2 mean(swap_error)=color_dot_swap;
run;

*the above step creates a cleaned session level summary. we should have this for each of the cognitive tests.;

data b1color_colordot_session;
set b1color_colordot_session;
color_dot_qual=_freq_=4;/*each session should include 4 trails*/
label color_dot_qual="Quality code Color Dots"
color_dot_dist2="Color Dot precision" color_dot_swap="Color Dot Swap Errors" color_dot_acc="Color-Location Binding";
run;
*the above step LABELS each variable and should save to a permanent SAS data set for session level analysis. We should have something like 
this for each of the cognitive tests. ;

data out.b1clean_colordot_session_&sysdate.;
set b1color_colordot_session;
drop _type_ _freq_;
run;

**burst2;

proc means nway noprint data=b2clean_colordot_trial;
class id session;
var newdate newtime colorcorr corrdist corrdist2 swap_error studyday flag_studyday cohort weekday;
output out=b2color_colordot_session min(newtime)=newtime min(newdate)=newdate min(studyday)=studyday min(flag_studyday)=flag_studyday min(cohort)=cohort
min(weekday)=weekday
mean(colorcorr)=color_dot_acc mean(corrdist)=color_dot_dist mean(corrdist2)=color_dot_dist2 mean(swap_error)=color_dot_swap;
run;

*the above step creates a cleaned session level summary. we should have this for each of the cognitive tests.;

data b2color_colordot_session;
set b2color_colordot_session;
color_dot_qual=_freq_=4;/*each session should include 4 trails*/
label color_dot_qual="Quality code Color Dots"
color_dot_dist2="Color Dot precision" color_dot_swap="Color Dot Swap Errors" color_dot_acc="Color-Location Binding";
run;
*the above step LABELS each variable and should save to a permanent SAS data set for session level analysis. We should have something like 
this for each of the cognitive tests. ;

data out.b2clean_colordot_session_&sysdate.;
set b2color_colordot_session;
drop _type_ _freq_;
run;


/*
data summary_colordots;
merge color_rts loc_rts coldotscorr;
by id session;
drop _type_ _freq_;
run;

*ignore below;
/*
proc print data=color_dot_session;
where color_dot_qual=0;run;
where _freq_ ne 4;run;

proc print data=clean_dot_trial;
var id session time colorcorr corrdist;
where id=10159 and session=13;
run;

proc print data=clean_dot_trial;
var id session time colorcorr corrdist;
where id=10880 and session=84;
run;
*/
