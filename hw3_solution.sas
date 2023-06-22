/*create a library */
libname hw3 "/home/u1566860/sasuser.v94/fin557/hw3";

%let outpath=/home/u1566860/sasuser.v94/fin557/hw3;
ods pdf file="&outpath/hw3_output.pdf" startpage=yes style=sapphire;

proc contents data=hw3.firm;
run;

****************************************************************;
*  2 Creating the sample                                       *;
****************************************************************;

/*2.1 Keep observations in the fiscal year of 2011*/
data list1;
set hw3.firm;
if FYEAR=2011;
keep FYEAR SIC TIC SALE;
run;

/*2.2 Sort the data */
proc sort data=list1;
by SIC descending SALE;
run;

/*2.3 Create an accumulating column*/
data list2;
set list1;
by SIC descending SALE;
if FIRST.SIC then n=0;
n+1;
run;

/*2.4 Keep only 10 firms in each industry*/
data list3;
set list2;
if n<=10;
keep TIC;
run;

/*2.5 Merge tables and include matching rows*/
proc sort data=hw3.firm;
by TIC;
run;

proc sort data=list3;
by TIC;
run;

data firm1;
merge hw3.firm list3(in=x);
by TIC;
if x=1;
run;
/* hw3.firm includes firms in list3 */

/* or */
data firm1;
merge hw3.firm(in=y) list3(in=x);
by TIC;
if y=1 and x=1;
run;


/*2.6 Clean the data */
proc sort data=firm1;
by TIC FYEAR descending CHE;
run;

data firm2;
set firm1;
by TIC FYEAR descending CHE;
if first.FYEAR;
run;
/*done! these are the 88 firms in our sample*/


****************************************************************;
*  3. Creating new variables                                   *;
****************************************************************;

/*3.1 Create common-size income statement items*/
data firm3;
set firm2;
SALE_P=divide(SALE,SALE);
COGS_P=divide(COGS,SALE);
EBIT_P=divide(EBIT,SALE);
GP_P=divide(GP,SALE);
NI_P=divide(NI,SALE);
label SALE_P="Sales" COGS_P="Cost of Goods Sold"  GP_P="Gross Profit"
      EBIT_P="Operating Profit"  NI_P="Net Income";
run;

/*3.2 Create common-size balance sheet items*/
data firm4;
set firm3;
CHE_P=divide(CHE,AT);
RECT_P=divide(RECT,AT);
INVT_P=divide(INVT,AT);
ACT_P=divide(ACT,AT);
AT_P=divide(AT,AT);
DLC_P=divide(DLC,AT);
LCT_P=divide(LCT,AT);
DLTT_P=divide(DLTT,AT);
TEQ_P=divide(TEQ,AT);
label CHE_P="Cash & Short-Term Investment" RECT_P="Net Receivables" INVT_P="Inventories"  
      ACT_P="Total Current Assets"  AT_P="Total Assets" DLC_P="Total Current Debt" 
	  LCT_P="Total Current Liabilities" DLTT_P="Long Term Debt"  TEQ_P="Stockholders Equity Total";
run;

/*3.3 Create financial ratios*/
data firm5;
set firm4;
CASH_RATIO=divide(CHE,AT);
ACID_TEST_RATIO=divide(CHE+RECT,LCT);
CURRENT_RATIO=divide(ACT,LCT);

DAY_RECEIVABLE=divide(365*RECT,SALE);
DAY_INVENTORY=divide(365*INVT,COGS);
ASSET_TURNOVER=divide(SALE,AT);

DEBT_ASSETS=divide(DLC+DLTT,AT);

GROSS_MARGIN=divide(GP,SALE);
RETURN_SALE=divide(NI,SALE);
RETURN_ASSET=divide(NI,AT);
RETURN_EQUITY=divide(NI,TEQ);
label CASH_RATIO="Current and Marketable Securities to Total Assets" ACID_TEST_RATIO="Acid Test Ratio" CURRENT_RATIO="Current Ratio"  
      DAY_RECEIVABLE="Day's Receivable"  DAY_INVENTORY="Day's Inventory" ASSET_TURNOVER="Asset Turnover" 
	  DEBT_ASSETS="Debt to Total Assets" 
      GROSS_MARGIN="Gross Margin Ratio"  RETURN_SALE="Return on Sales" 
      RETURN_ASSET="Return on Assets" RETURN_EQUITY="Return on Equity";
run;



****************************************************************;
*  4. Compare firm characteristics across industries           *;
****************************************************************;

/*4.1 Generate summary statistics*/
proc means data=firm5 mean;
class SIC;
var DAY_RECEIVABLE INVT_P GROSS_MARGIN;
run;


ods pdf close;
