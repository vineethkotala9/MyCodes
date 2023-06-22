/*1.1. Downloading the data */
libname HW2 "/home/u63116105/sasuser.v94/FIN557/HW2"; proc contents data=hw2.ownership;
run;

/*1.2 Creating new variables */
data own1;
set HW2.ownership;
SHROUT=SHROUT2*1000; PERCENT=(SHARES/SHROUT)*100;
keep FDATE TICKER SHARES SHROUT SHROUT2 PERCENT;
run;
proc print data=own1 (obs=10);
run;

/*1.3 Sorting the data */
proc sort data=own1 out=own2;
by TICKER descending PERCENT;
run;
proc print data=own2 (obs=10);
run;

/*1.4 total percentage of institutional ownership and the total number of institutional 
investors for each firm */
data own3; set own2;
by TICKER descending PERCENT; if first.TICKER=1 then SUM=0; SUM=SUM+PERCENT;
if first.TICKER=1 then N=0; 
N=N+1;
if last.TICKER=1 then output; 
run;
proc print data=own3; 
run;

/*1.5 Finding the top 10 investors */
data own4; set own2;
by TICKER descending PERCENT; if first.TICKER=1 then SUM=0; SUM=SUM+PERCENT;
if first.TICKER=1 then N=0; 
N=N+1;
if N<=10 then output; 
run;
proc print data=own4 (obs=10); 
run;

/*1.6 Finding large investors within 20% of ownership  */
data own5; set own2;
by TICKER descending PERCENT; if first.TICKER=1 then SUM=0; SUM=SUM+PERCENT;
if first.TICKER=1 then N=0; 
N=N+1;
if SUM<=20 then output; 
run;
proc print data=own5 (obs=10); 
run;

/* 2 Manipulating data with character functions*/
/*2.1 Removing characters from a string*/
proc contents data=HW2.location; 
run;
data location1;
set hw2.location; location1=compbl(cat(LOCATION)); id1=compress(ID," "); id2=compress(ID," -");
run;
proc print data=location1 (obs=10); 
run;

/*2.2 Extracting words from a string */
data location2;
	set HW2.location; 
	location=compbl(cat(LOCATION)); 
	city1=scan(LOCATION,1); 
	prefecture1=scan(LOCATION,2); 
	country1=scan(LOCATION,-1); 
	run;
proc print data=location2 (obs=10); 
run;

/*2.3 Correcting inconsistent character case */
data location3;
	set HW2.location; 
	location=compbl(cat(LOCATION)); 
	city2=propcase(scan(LOCATION,1, ","), ""); 
	run;
	
proc print data=location3 (obs=10); 
run;

/*2.4 Filtering the rows */
data location4;
	set HW2.location; 
	location=compbl(cat(LOCATION)); 
	prefecture2=scan(LOCATION,2, ",");
if find(prefecture2,"Tokyo") then output;