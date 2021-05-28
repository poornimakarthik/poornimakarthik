/* FOR PROJECT*/

libname Homework "C:\Users\metro\SAS\SAS_Example\HomeWork";
%include "C:\Users\metro\SAS\SAS_Example\HomeWork\Poornima_Macro.sas";

Options yearcutoff=1990;
DATA Homework.WN;
 INFILE 'C:\Users\metro\SAS\New_Wireless_Fixed.txt' truncover;
 INPUT Acctno $ 1-13 Actdt @26 Deactdt  DeactReason $ 41-48 GoodCredit 53-54  RatePlan 62-63 DealerType $ 65-66 Age 74-75 Province $ 80-81 Sales DOLLAR11.2 ;
 informat Actdt Deactdt MMDDYY10.;
 format Actdt Deactdt MMDDYY10.;
 FORMAT Sales DOLLAR11.2;

RUN;
quit;
DATA Homework.WN_COPY_ORIGINAL;
set Homework.WN;
RUN;
DATA Homework.WN_ccheck;
 INFILE 'C:\Users\metro\SAS\New_Wireless_Fixed.txt' truncover;
 INPUT Acctno $ 1-13 Actdt @26 Deactdt  DeactReason $ 41-48 GoodCredit 53-54  RatePlan 62-63 DealerType $ 65-66 Age 74-75 Province $ 80-81 Sales DOLLAR11.2 ;
 informat Actdt Deactdt MMDDYY10.;
 format Actdt Deactdt MMDDYY10.;
 FORMAT Sales DOLLAR11.2;

RUN;
PROC PRINT DATA = Homework.WN(obs=20);
RUN;
PROC PRINT DATA = Homework.WN(obs=102255 firstobs=102235);
title 'CONTENTS OF DATASET WIRELESS NETWORK DATASET';
RUN;
PROC CONTENTS DATA = Homework.WN_ccheck;
title 'CONTENTS OF DATASET WIRELESS NETWORK DATASET';
run; /* No of Observation = 102255,10 columns,5 Char Variables ,3 Num Variales,2 Date variables */

/* 1.1 Explore and, is the describe the dataset briefly. For example acctno unique? What
is the number of accounts activated and deactivated? When is the earliest and
latest activation/deactivation dates available? And so on….
*/


OPTIONS MLOGIC;
*NUMERICAL CONTINUOUS VARIABLE: ;
%UNI_ANALYSIS_NUM(Homework.WN,Age)
OPTIONS NOMLOGIC;
/* Avg age 47 ,No such outliers ,seems Normally distributed ,but age has minimum value as 0 which is ambigous ,because there is age limit for telcom companies to provide contract like 18 */
/* Age  missing values are around 13% of total dataset,I have deleted those rows of Age mising */
DATA Homework.WN;
 SET Homework.WN;
 
 if Age = . then delete;
RUN; /*Current No of Rows are 94547 */
%UNI_ANALYSIS_NUM(Homework.WN,Sales)
/* Many values seems to be above the third quartile ,seems like few are outliers yet to investigate */
DATA Homework.WN;
 SET Homework.WN;
 
 if Sales = . then delete; /* Only 7468 OBS having missing values,thereofore deleting them*/
RUN; /*Current No of Rows 81620 */

*CATEGORICAL VARIABLES : ;
%UNI_ANALYSIS_CAT(Homework.WN,DealerType)
/* 4 types of Dealers A1--54.89% ,A2--11.01% ,B1--20.21% ,C1 --13.88% ..highest being A1 */
%UNI_ANALYSIS_CAT(Homework.WN,Province)
/* 5 Provinces ON with 44.11% Accounts,NS with 11.97%, QC with 10.38% , AB with 10.67% , BC with 22.88% ,ON having the most Accounts followed by BC,NS,AB,QC */

DATA Homework.WN;
 SET Homework.WN;
 if Province = '' then delete;/* 5459 Province data missing ,therefore deleteing them */
RUN; /*Current No of Rows 89088 */


*CATEGORICAL VARIABLES WITH FORMAT :  ;
%UNI_ANALYSIS_CAT_FORMAT(Homework.WN,GoodCredit,GCfmt.);
/* NO MISSING Accounts with 69.44% as GOODCREDIT, 30.56% with NOTGOODCREDIT */

%UNI_ANALYSIS_CAT_FORMAT(Homework.WN,RatePlan,RPfmt.); /* NO MISSING ,3 RatePlans ,RatePlan1 with 66.69% Accounts,RatePlan2 with 19.74% ,RatePlan3 with 13.57% */

/* Are acctno unique? */
 title 'UNIQUE ACCOUNT NUMBERS';

PROC SQL;
 SELECT COUNT(*) AS NOOBS,COUNT(DISTINCT Acctno) AS UniqueAccount
 FROM Homework.WN
 ;
 QUIT; /* BEFORE DELETION OF MISSING VALUES IN OTHER COLUMNS,equal No of ActiveaAccount = 102255, */
  title 'UNIQUE ACCOUNT NUMBERS';

PROC SQL;
 SELECT COUNT(*) AS NOOBS,COUNT(DISTINCT Acctno) AS UniqueAccount
 FROM Homework.WN
 ;
 QUIT;/*AFTER ALL Deletion equal No of ActiveaAccount = 81620 */
 /*What is the number of accounts activated and deactivated? */
 title 'No of ACTIVE and DEACTIVE accounts ';
 PROC SQL;
 SELECT COUNT(Actdt) AS ACTIVE_Accounts,COUNT(Deactdt) AS DEACTIVE_Accounts
 FROM Homework.WN
 ;
 QUIT;

/* When is the earliest and latest activation/deactivation dates available  */

proc sort data=Homework.WN out=Homework.earliestActdt;
  by Actdt ;
run;
proc sort data=Homework.WN out=Homework.latesetDeactdt;
  by descending Deactdt;
run;
/* When is the earliest and latest activation/deactivation dates available  */
 title 'Earliest ACTIVE accounts ';

PROC PRINT DATA = Homework.earliestActdt(obs=20);
FORMAT Sales DOLLAR11.2;
RUN; /* Earliest ACTIVE ACTIVATION DATE IS 01/20/1999 */
PROC PRINT DATA = Homework.earliestActdt(obs=81620 firstobs=81619);
FORMAT Sales DOLLAR11.2;
RUN;/* LATEST ACTIVE DATE IS 01/20/2001*/
title 'Earliest DEACTIVE accounts ';

PROC PRINT DATA = Homework.latesetDeactdt(obs=20);
FORMAT Sales DOLLAR11.2;
RUN; /* LATEST DEACTACTIVE DATE IS 01/20/2001 */
/*Active Account and Deactiv Account in Each Province */
PROC SQL;
CREATE TABLE Homework.PROVINCEGROUP as
SELECT  
	  Province,COUNT(Actdt) as ACTIVE_ACCT_IN_EACH_PROVINCE,COUNT(Deactdt) as INACTIVE_ACCT_IN_EACH_PROVINCE
FROM Homework.WN
GROUP BY Province
;
QUIT;
title 'ACTIVE and DEACTIVE Accounts For Province Group ';
PROC PRINT DATA = Homework.PROVINCEGROUP(obs=20);
RUN;
PROC sgplot DATA=PROVINCEGROUP;
  
      VBAR Province/ GROUP=ACTIVE_ACCT_IN_EACH_PROVINCE RESPONSE=ACTIVE_ACCT_IN_EACH_PROVINCE ;
	  title 'No of ACTIVE accounts for each Province';
RUN;
PROC sgplot DATA=PROVINCEGROUP;
  
      VBAR Province/ GROUP=INACTIVE_ACCT_IN_EACH_PROVINCE RESPONSE=INACTIVE_ACCT_IN_EACH_PROVINCE;
	  title 'No of DEACTIVE accounts for EACH Province';
RUN;

options fmtsearch=(Homework);
PROC FORMAT library=Homework;
value GCfmt
      0 = "NOTGOODCREDIT"
      1 = "GOODCREDIT" 
      OTHER = "";
run;
PROC FORMAT library=Homework;
value RPfmt
      1 = "RatePlan 1"
      2 = "RatePlan 2" 
	  3 = "RatePlan 3"
      other = "";
run;



%BI_ANALYSIS_NUMs_CAT (DSN =Homework.WN ,CLASS=Province , VAR=Sales);

/* Ananlysis Highest Sales by Province ON is the highest as ON being the province having more accounts*/

/*%BI_ANALYSIS_NUMs_CAT (DSN =SASHELP.HEART ,CLASS=Status , VAR=Height Weight);*/



/*H0 : Province and DealerType are Independent ,There is no statistically significant relationship between Province and DealerType*/
/*HA : Province and DealerType are not Independent,There is a statistically significant relationship between Province and DealerType*/
%CHSQUARE(DSN = Homework.WN , VAR1= Province , VAR2 =DealerType);/*The P value is 0.1715 ,i.e means we fail to reject the HO i.e is There is no correlation between the two variables as Pvalue is greater at significance level  0.05 */


/*
1.3 Segment the customers based on age, province and sales amount:
Sales segment: < $100, $100---500, $500-$800, $800 and above.
Age segments: < 20, 21-40, 41-60, 60 and above.
Create analysis report 
*/
DATA Homework.GROUP;
 SET Homework.WN  ;
 LENGTH Sales_GROUP  $ 20;

 IF Sales EQ . THEN Sales_GROUP = "SALESMISSING";
 ELSE IF Sales LE 100 THEN Sales_GROUP = "SALES<100";
 ELSE IF 100 <= Sales < 500  THEN Sales_GROUP = "SALES100TO500";
 ELSE IF 500 <= Sales < 800  THEN Sales_GROUP = "SALES500TO800";
 ELSE IF Sales GE 800 THEN Sales_GROUP = "SALES>800";
  ELSE Sales_GROUP = "NOT DEFINED";

LENGTH Age_GROUP  $ 20;

  
 IF Age EQ . THEN Age_GROUP = "AGEMISSING";
 ELSE IF Age LE 20 THEN Age_GROUP = "AGE<20";
 ELSE IF 21 <= Age <= 40  THEN Age_GROUP = "AGE21TO40";
 ELSE IF 41 <= Age < 60  THEN Age_GROUP = "AGE41TO60";
 ELSE IF Age GE 60 THEN Age_GROUP = "AGE>60";
  ELSE Age_GROUP = "NOT DEFINED";

 RUN;

PROC PRINT DATA = Homework.GROUP(obs=20);
RUN;

PROC SGPLOT DATA= Homework.GROUP;
VBAR Age_GROUP/GROUP=Sales_GROUP;
RUN;
QUIT; /*People AGED Betweeen 41 TO 60 Customers with more SALES,followed by people AGED Between 21 to 40 */




/*H0 : Sales_GROUP and Age_GROUP are Independent ,There is no statistically significant relationship between Sales_GROUP and Age_GROUP*/
/*HA : Sales_GROUP and Age_GROUP are not Independent,There is a statistically significant relationship between Sales_GROUP and Age_GROUP*/
%CHSQUARE(DSN = Homework.GROUP , VAR1= Age_GROUP , VAR2 =Sales_GROUP);
/*The P value is 0.2389 ,i.e means we fail to reject the HO i.e is There is no association between the two variables as Pvalue is greater at significance level  0.05 */


title;

PROC SGPLOT DATA= Homework.GROUP;
VBAR DealerType/GROUP=Age_GROUP;
RUN;
QUIT; /* A1 is the DealerType most used by Customer */

/* 1.4.Statistical Analysis:
1) Calculate the tenure in days for each account and give its simple statistics.
 */
title;
footnote;
title "Frequncy For Tenur";
DATA Homework.Age_Tenur;
SET Homework.WN ;
LENGTH TENURE_DAY 3.;
TENURE_DAY = INTCK("DAY",Actdt,Deactdt); 
IF TENURE_DAY EQ . THEN TENURE_DAY = 0;
RUN;
PROC PRINT DATA = Homework.Age_Tenur(obs=20);
RUN;


/*1.4.3 
Segment the account, first by account status “Active” and “Deactivated”, then by
Tenure: < 30 days, 31---60 days, 61 days--- one year, over one year. Report the
number of accounts of percent of all for each segment. */
/*INCLUDED ALL SEGMENTS TOGETHER AND ANALYSIED THEM TOGETHER */

title;
footnote;
Options yearcutoff=1984;
DATA Homework.ACTIVE_DEACTIVE;
 SET Homework.WN ;

 LENGTH ACTIVE_DEACTIVE  $ 20;

 IF Deactdt NE . THEN ACTIVE_DEACTIVE = "DEACTIVE";
 ELSE IF Actdt NE . THEN ACTIVE_DEACTIVE = "ACTIVE";
 ELSE ACTIVE_DEACTIVE = "NOT DEFINED";

 LENGTH CHURN  $ 20;

 IF Deactdt NE . THEN CHURN = "YES";
 ELSE IF Actdt NE . THEN CHURN = "NO";
 ELSE CHURN = "NOT DEFINED";
 
 IF Deactdt EQ . THEN Deactdt_New = "20JAN2001"D;/*latest deactivated date */
 ELSE Deactdt_New = Deactdt;
 informat Deactdt_New MMDDYY10.;
 format Deactdt_New MMDDYY10.;

 LENGTH TENURE_MONTH  3;
 TENURE_MONTH=INTCK("MONTH",Actdt ,Deactdt_New);

LENGTH CONTRACT $ 30;
IF TENURE_MONTH EQ . THEN CONTRACT = "MISSING";
ELSE IF  TENURE_MONTH LE 12 THEN CONTRACT = "MONTH-TOMONTH";
ELSE IF 12 <= TENURE_MONTH < 24  THEN CONTRACT = "ONE-YEAR";
ELSE IF TENURE_MONTH >= 24 THEN CONTRACT = "TWO-YEAR";
ELSE CONTRACT = "NOT DEFINED";


LENGTH TENURE_DAY 3.;
TENURE_DAY = INTCK("DAY",Actdt,Deactdt_New); 
IF TENURE_DAY EQ . THEN TENURE_DAY = 0;

 LENGTH DAYSCONTRACT $ 30;
 IF TENURE_DAY EQ . THEN DAYSCONTRACT = "MISSING";
 ELSE IF  TENURE_DAY LE 30 THEN DAYSCONTRACT = "LESSTHANMONTH";
 ELSE IF 31 <= TENURE_DAY <= 60  THEN DAYSCONTRACT = "LESSTHANTWOMONTHS";
 ELSE IF 61 <= TENURE_DAY <= 365  THEN DAYSCONTRACT = "LESSTHANYEAR";
 ELSE IF TENURE_DAY > 365 THEN DAYSCONTRACT = "ABOVEYEAR";
 ELSE DAYSCONTRACT = "NOT DEFINED";


   
 LENGTH Age_GROUP  $ 20; 
 IF Age EQ . THEN Age_GROUP = "AGEMISSING";
 ELSE IF Age LE 20 THEN Age_GROUP = "AGE<20";
 ELSE IF 21 <= Age <= 40  THEN Age_GROUP = "AGE21TO40";
 ELSE IF 41 <= Age < 60  THEN Age_GROUP = "AGE41TO60";
 ELSE IF Age GE 60 THEN Age_GROUP = "AGE>60";
 ELSE Age_GROUP = "NOT DEFINED";

 LENGTH Sales_GROUP  $ 20;

 IF Sales EQ . THEN Sales_GROUP = "SALESMISSING";
 ELSE IF Sales LE 100 THEN Sales_GROUP = "SALES<100";
 ELSE IF 100 <= Sales < 500  THEN Sales_GROUP = "SALES100TO500";
 ELSE IF 500 <= Sales < 800  THEN Sales_GROUP = "SALES500TO800";
 ELSE IF Sales GE 800 THEN Sales_GROUP = "SALES>800";
 ELSE Sales_GROUP = "NOT DEFINED";


 RUN;
PROC PRINT DATA = Homework.ACTIVE_DEACTIVE(obs=30);
RUN;

DATA Homework.WN_CHURN ;
 SET Homework.ACTIVE_DEACTIVE (keep = Actdt Deactdt CHURN GoodCredit RatePlan DealerType TENURE_MONTH CONTRACT TENURE_DAY DAYSCONTRACT Age_GROUP Sales_GROUP);
RUN;
PROC PRINT DATA = Homework.WN_CHURN(obs=30);
RUN;

 %UNI_ANALYSIS_CAT(Homework.ACTIVE_DEACTIVE,CHURN)

%UNI_ANALYSIS_NUM(Homework.ACTIVE_DEACTIVE,TENURE_DAY) /* Min 0 Lower Quartile 101.00 Median 265.00 Mean 282.57  Upper Quartile 426.00 Maximum 731.00 */


%UNI_ANALYSIS_CAT(Homework.ACTIVE_DEACTIVE,DAYSCONTRACT)

%UNI_ANALYSIS_NUM(Homework.ACTIVE_DEACTIVE,TENURE_MONTH) /* Min 0 Lower Quartile 3.00 Median 9.00 Mean 9.16  Upper Quartile 11.00 Maximum 24.00*/
%UNI_ANALYSIS_CAT(Homework.ACTIVE_DEACTIVE,CONTRACT)

/* STACKED BAR CHART for 2 Categorical Variables */
PROC SGPLOT DATA= Homework.ACTIVE_DEACTIVE;
TITLE " RELATION BETWEEN CHURN AND DAYSCONTRACT";
VBAR DAYSCONTRACT/GROUP=CHURN;
RUN;
QUIT; 
/*H0 : CHURN and DAYSCONTRACT are Independent ,There is no statistically significant relationship between CHURN and DAYSCONTRACT*/
/*HA : CHURN and DAYSCONTRACT are not Independent,There is a statistically significant relationship between CHURN and DAYSCONTRACT*/
%CHSQUARE(DSN = Homework.ACTIVE_DEACTIVE , VAR1= CHURN , VAR2 =DAYSCONTRACT);/*The P value is 0.0001 ,i.e means we reject the HO i.e is There is association between the two variables as Pvalue is lesser at significance level  0.05 */

PROC SGPLOT DATA= Homework.ACTIVE_DEACTIVE;
TITLE " RELATION BETWEEN CHURN AND CONTRACT";
VBAR CONTRACT/GROUP=CHURN;
RUN;
QUIT;
/*H0 : CHURN and CONTRACT are Independent ,There is no statistically significant relationship between CHURN and CONTRACT*/
/*HA : CHURN and CONTRACT are not Independent,There is a statistically significant relationship between CHURN and CONTRACT*/
%CHSQUARE(DSN = Homework.ACTIVE_DEACTIVE , VAR1= CHURN , VAR2 =CONTRACT);/*The P value is 0.0001 ,i.e means we reject the HO i.e is There is association between the two variables as Pvalue is lesser at significance level  0.05 */

PROC MEANS DATA = Homework.ACTIVE_DEACTIVE MAXDEC= 2 ;
TITLE " RELATION BETWEEN SALES AND GoodCredit";
CLASS CHURN  ;
FORMAT GoodCredit GCfmt.;
VAR TENURE_DAY ;
run;

%UNI_ANALYSIS_CAT(Homework.ACTIVE_DEACTIVE,CHURN) /*IMBALANCED DATA`*/

%UNI_ANALYSIS_CAT(Homework.ACTIVE_DEACTIVE,Age_GROUP)

%UNI_ANALYSIS_CAT(Homework.ACTIVE_DEACTIVE,Sales_GROUP)


*H0 : no assocaiton/correlation;
*HA : TENURE_MONTH and TENURE_DAY are associated with Sales;

TITLE "Conducting Pearson Correlation";
PROC CORR DATA = Homework.ACTIVE_DEACTIVE;/*  Pearson correlation between TENURE_MONTH,TENURE_DAY and Sales is -0.00230,-0.00250 ,so No linear Correlation */
 VAR TENURE_MONTH TENURE_DAY Sales;
RUN;


PROC MEANS DATA = Homework.ACTIVE_DEACTIVE MAXDEC= 2 ;
TITLE " RELATION BETWEEN SALES AND GoodCredit";
CLASS ACTIVE_DEACTIVE GoodCredit Age_GROUP ;
FORMAT GoodCredit GCfmt.;
VAR TENURE_MONTH ;
run;

/*H0 : CHURN and TENURE_MONTH are Independent ,There is no statistically significant relationship between CHURN and TENURE_MONTH*/
/*HA : CHURN and TENURE_MONTH are not Independent,There is a statistically significant relationship between CHURN and TENURE_MONTH*/
PROC TTEST data=Homework.ACTIVE_DEACTIVE;
CLASS CHURN;
VAR TENURE_MONTH;
RUN; /*The P value is 0.0001 ,i.e means we Reject the HO i.e is There is association between the two variables as Pvalue is lesser at significance level  0.05 */
/*HO is that there is no difference between treatment group(CHURN) means, while 
HA alternative hypothesis is that mean values differ between treatment groups(CHURN). */
TITLE "RELATIONSHIP BETWEEN BETWEEN TENURE_DAY AND ACTIVE_DEACTIVE";
PROC GLM data=Homework.ACTIVE_DEACTIVE;
CLASS CHURN;
MODEL TENURE_DAY = CHURN;
MEANS CHURN / hovtest=levene(type=abs) welch;/*mean of churn customers are differenet from mean of NOT churn customers at signifcance level 0.05 for TENURE_DAY  */
run;/*The P value is 0.0001 ,i.e means we Reject the HO i.e is There is association between the two variables as Pvalue is lesser at significance level  0.05,the Mean Values DIFFER BETWEEN 2 GROUPS OF CHURN I;E is NOTCHURNED and CHURNED */
/*Bivariant Analysis of Province and Active/Deactive( NOChurn/Churn) Accounts*/
PROC SGPLOT data=Homework.ACTIVE_DEACTIVE;
TITLE "Bivariant Analysis of Province and Active/Deactive( NOChurn/Churn) Accounts";
vbar Province /group=ACTIVE_DEACTIVE groupdisplay=cluster;
run;

/*H0 : ACTIVE_DEACTIVE and Province are Independent ,There is no statistically significant relationship between ACTIVE_DEACTIVE and Province*/
/*HA : ACTIVE_DEACTIVE and Province are not Independent,There is a statistically significant relationship between ACTIVE_DEACTIVE and Province*/
%CHSQUARE(DSN = Homework.ACTIVE_DEACTIVE , VAR1= Province , VAR2 =ACTIVE_DEACTIVE);/*THE PVALUE are higher so we FAIL TO REJECT HO;so there is no assciation between 2 variables */

/* Age Groups Segments for Active and Deactive Acoounts */
PROC SGPLOT DATA= Homework.ACTIVE_DEACTIVE;
VBAR Age_GROUP/GROUP=ACTIVE_DEACTIVE;
RUN;
QUIT; 
/*H0 : ACTIVE_DEACTIVE and Province are Independent ,There is no statistically significant relationship between ACTIVE_DEACTIVE and Province*/
/*HA : ACTIVE_DEACTIVE and Province are not Independent,There is a statistically significant relationship between ACTIVE_DEACTIVE and Province*/
%CHSQUARE(DSN = Homework.ACTIVE_DEACTIVE , VAR1= Age_GROUP , VAR2 =ACTIVE_DEACTIVE);/*THE PVALUE are higher so we FAIL TO REJECT HO;so there is no assciation between 2 variables */


PROC SGPLOT DATA =Homework.ACTIVE_DEACTIVE;
VBOX TENURE_MONTH/GROUP =CHURN;
RUN;
QUIT;

PROC SGPLOT DATA =Homework.ACTIVE_DEACTIVE;
VBOX TENURE_DAY/GROUP =CHURN;
RUN;
QUIT;
PROC SGPLOT DATA =Homework.ACTIVE_DEACTIVE;
VBOX Sales/GROUP =CHURN;
RUN;
QUIT;

/*. Report the number of accounts of percent of all for each segment. */
title;
PROC CONTENTS DATA = Homework.ACTIVE_DEACTIVE;
run;
PROC SQL;
SELECT  
	  Age_GROUP, COUNT(Actdt) as ACTIVE_ACC,COUNT(Deactdt) as DEACTIVE_ACC
FROM Homework.ACTIVE_DEACTIVE
GROUP BY Age_GROUP
;
QUIT;

PROC SQL;
SELECT  
	  TENURE_MONTH, COUNT(Actdt) as ACTIVE_ACC,COUNT(Deactdt) as DEACTIVE_ACC
FROM Homework.ACTIVE_DEACTIVE
GROUP BY TENURE_MONTH
;
QUIT;
/*2) Calculate the number of accounts deactivated for each month.*/
title 'No of deactivated accounts in each MONTH';
PROC SQL;
CREATE TABLE Homework.ACCTDEACTIVE  as
SELECT MONTH(Deactdt) as month  , count(*) as count
FROM Homework.WN_COPY_ORIGINAL
GROUP BY month
;
QUIT;
PROC PRINT DATA = Homework.ACCTDEACTIVE;
RUN;
/* Recreated table using Format the Month in PLOT */
PROC SQL;
     CREATE TABLE Homework.ACCTDEACTIVE  as
     SELECT Deactdt as month format=MONNAME. , count(*) as count
     FROM Homework.WN
     GROUP BY month
     ;
     QUIT;
PROC PRINT DATA = Homework.ACCTDEACTIVE;
RUN;


PROC sgplot DATA=Homework.;
  
      VBAR month/ GROUP=month RESPONSE=count  ;
	  FORMAT month MONNAME.;
	  title 'No of deactivated accounts in each MONTH';
RUN;

DATA Homework.ACCTDEACTIVE;
 SET Homework.ACCTDEACTIVE;
 
 if month = . then delete; /* Only 7468 OBS having missing values,thereofore deleting them*/
RUN;


/*6) Does Sales amount differ among different account status, GoodCredit, and
customer age segments?
*/
options fmtsearch=(Homework);

PROC TABULATE data=Homework.ACTIVE_DEACTIVE;
CLASS ACTIVE_DEACTIVE ;
VAR Sales;
TABLE Sales * (N Mean STD)
 Sales * MEAN * ACTIVE_DEACTIVE ;
RUN;
PROC MEANS DATA = Homework.ACTIVE_DEACTIVE MAXDEC= 2 ;
TITLE " RELATION BETWEEN SALES AND GoodCredit";
CLASS ACTIVE_DEACTIVE GoodCredit Age_GROUP ;
FORMAT GoodCredit GCfmt.;
VAR Sales ;
run; /* Need to apply Ttest */
/*6) Does Sales amount differ among different account status, GoodCredit, and customer age segments? */


/*4) Test the general association between the tenure segments and “Good Credit” “RatePlan ” and “DealerType.”*/
/*H0 : GoodCredit RatePlan DealerType and TENURE_MONTH are Independent ,There is no statistically significant relationship between GoodCredit RatePlan DealerType and TENURE_MONTH*/
/*HA : GoodCredit RatePlan DealerType and TENURE_MONTH are not Independent,There is a statistically significant relationship between GoodCredit RatePlan DealerType and TENURE_MONTH*/
/*ALSO can be stated */
/*HO is that there is no difference between treatment group(GoodCredit RatePlan DealerType) means with TENURE_MONTH, while 
HA alternative hypothesis is that mean values differ between treatment groups(GoodCredit RatePlan DealerType) with TENURE_MONTH. */

/*Test the general association between the tenure segments and “Good Credit” “RatePlan ” and “DealerType.” */

/*H0 : DAYSCONTRACT and DealerType are Independent ,There is no statistically significant relationship between DAYSCONTRACT and DealerType*/
/*HA : DAYSCONTRACT and DealerType are not Independent,There is a statistically significant relationship between DAYSCONTRACT and DealerType*/

%CHSQUARE(DSN = Homework.ACTIVE_DEACTIVE , VAR1= DAYSCONTRACT , VAR2 =DealerType)
/*THE PVALUE <=0.05 so we REJECT HO;so there is no association between 2 variables */

/*H0 : DAYSCONTRACT and RatePlan are Independent ,There is no statistically significant relationship between DAYSCONTRACT and RatePlan*/
/*HA : DAYSCONTRACT and RatePlan are not Independent,There is a statistically significant relationship between DAYSCONTRACT and RatePlan*/
%CHSQUARE(DSN = Homework.ACTIVE_DEACTIVE , VAR1= DAYSCONTRACT , VAR2 =RatePlan)
/*THE PVALUE <=0.05 so we REJECT HO;so there is no association between 2 variables */

/*H0 : DAYSCONTRACT and GoodCredit are Independent ,There is no statistically significant relationship between DAYSCONTRACT and GoodCredit*/
/*HA : DAYSCONTRACT and GoodCredit are not Independent,There is a statistically significant relationship between DAYSCONTRACT and GoodCredit*/
%CHSQUARE(DSN = Homework.ACTIVE_DEACTIVE , VAR1= DAYSCONTRACT , VAR2 =GoodCredit)
/*THE PVALUE <=0.05 so we REJECT HO;so there is no association between 2 variables */
