/*Homework 1*/

/*Question 1 */

proc contents data="/home/u63083620/sasuser.v94/fin557/hw1/ceo.sas7bdat";
run;

libname hw1 "/home/u63083620/sasuser.v94/fin557/hw1";

proc means data=hw1.ceo;
run;

/*Question 2.1 */

proc print data=hw1.ceo;
where AGE<40 and STATE in ("ND","SD","NE","KS","MO","IA","MN","WI","MI","IL","IN","OH");
var TICKER STATE EXECID CEOANN SALARY TDC1_PCT;
format SALARY dollar10. TDC1_PCT 8.1;
run;

/*Question 2.2 */

proc means data=hw1.ceo;
where GENDER="FEMALE" and BECAMECEO >= "01jan2010"d;
var SALARY BONUS TDC1;
run;

/*Question 2.3 */

%let SIC=2834;
%let GENDER=FEMALE;
%let SALARY=1000000;

proc print data=hw1.ceo;
	where SIC=&SIC and GENDER="&GENDER" and SALARY>&SALARY;
	var SIC SICDESC TICKER EXECID GENDER SALARY;
run;


proc means data=hw1.ceo;
	where SIC=&SIC and GENDER="&GENDER" and SALARY>&SALARY;
	var SALARY;
run;

/*Question 3.1 */

proc sort data=hw1.ceo out=ceo1 nodupkey;
	by YEAR TICKER;
run;

/*Question 3.2 */

proc sort data=hw1.ceo out=ceo2;
	by YEAR descending SALARY;
run;

proc print data=ceo2 (obs=10);
	VAR YEAR TICKER EXECID SALARY;
run;

/*Question 3.3 */

proc print data=ceo2 (obs=10);
	VAR TICKER EXECID YEAR  SALARY;
run;

proc sort data=ceo2 out=ceo3 nodupkey;
	by YEAR;
run;

proc print data=ceo3;
	VAR YEAR TICKER EXECID SALARY;
run;

