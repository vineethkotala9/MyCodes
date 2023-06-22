/*create a library */
libname hw4 "/home/u1566860/sasuser.v94/fin557/hw4";

/*hw4.ret contains firms with SICCD greater than or equal to 4000 and less than or equal to 5999*/

%let outpath=/home/u1566860/sasuser.v94/fin557/hw4;
ods pdf file="&outpath/hw4_output.pdf" startpage=yes style=sapphire;
/*2.1*/
proc sql;
create table ret1 as
select DATE, PERMNO, RET, SICCD, floor(SICCD/10) as SIC3
    from hw4.ret
	where year(DATE)=2018 and SICCD is not null;
quit;
/*include data in year 2018*/
/*you can also use int(SICCD/10) as SIC3*/

/*2.2*/
title "Question 2.2";
proc sql;
select count(distinct PERMNO) as FREQ
    from ret1;
quit;
/*sample includes 767 firms*/

/*2.3*/
title "Question 2.3";
proc sql outobs=10;
select count(PERMNO) as TRADAYS
    from ret1
	group by PERMNO
    order by TRADAYS desc;
quit;
/*year 2018 has 251 trading days*/

/*2.4*/
proc sql;
create table ret2 as
select DATE, PERMNO, RET, SICCD, SIC3, count(PERMNO) as TRADAYS
    from ret1
	group by PERMNO
	having TRADAYS=251; 
quit;
/*only include firms with 251 trading days in 2018*/

/*2.5*/
title "Question 2.5";
proc sql;
select count(distinct permno) as FREQ
    from ret2;
quit;
/*sample includes 703 firms*/

/*3.1*/
proc sql;
create table ret3 as
select DATE, PERMNO, RET, SICCD, SIC3, mean(RET) as RET_IND, count(RET) as RET_N
    from ret2
	group by SIC3, DATE
	having RET_N>30;
quit;
/*find the daily industry avergae for each SIC3 */
/*keep only groups with more than 30 observations */

title "Question 3.1";
proc print data=ret3 (obs=10);
run;

/*3.2*/
proc sql;
create table ret4 as
select DATE, PERMNO, RET, SIC3, RET_IND, RET-RET_IND as ABNRET
    from ret3;
quit;
/*define abnormal return */

title "Question 3.2";
proc print data=ret4 (obs=10);
run;

/*3.3*/
title "Question 3.3";
proc sql;
select month(DATE) as MONTH, sum(ABNRET>0.01) as ABNRET_N
    from ret4
	group by MONTH
	order by ABNRET_N desc;
quit;
/*count the number of ABNRET greater than 0.01 in each month*/


title;
ods pdf close;
