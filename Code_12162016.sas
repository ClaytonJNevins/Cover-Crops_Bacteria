/* Meeting 12/16/2016 */

PROC IMPORT OUT = data_wPrecTemp 
            DATAFILE = "W:\pu.data\Desktop\SCS project\data_wPrecTemp.xlsx" 
            DBMS = xlsx REPLACE;
  SHEET = "data_wPrecTemp"; 
  GETNAMES = YES;
RUN;

PROC SORT DATA=data_wPrecTemp;
BY Treatment;
RUN;

DATA data_wPrecTemp;
SET data_wPrecTemp;
log_SBG = LOG(SBG);
log_inorganicN = LOG(InorganicN);
RUN;

ODS PDF FILE="W:\pu.data\Desktop\SCS project\data_wPrecTemp_output0128.pdf";
PROC MIXED DATA=data_wPrecTemp PLOTS=(RESIDUALPANEL STUDENTPANEL); * PLOTS=ALL;
* BY Treatment; 
* WHERE (myDate=3 | myDate=4); /* a particular date */
CLASS Plot Rep Treatment Tillage Date;								*CLASS is used to show the categorical variables;
MODEL log_inorganicN = Treatment Tillage Treatment*Tillage			/*response variable and the fixed effects*/
      /* SBG */ Date Date*Treatment Date*Treatment*Tillage
            Moisture
			Moisture*Treatment 
            / OUTPRED=p_loginorganicN;								/*using the fixed effects, creates a predicted value*/
			* temp_fromlast;
RANDOM Rep;																	
REPEATED / SUBJECT=Plot TYPE=AR(1);
/* correlation: http://www2.sas.com/proceedings/sugi30/198-30.pdf */
LSMEANS Treatment / DIFF ADJUST=TUKEY;										/*multiple parewise comparisons*/
SLICE Treatment*Tillage / SLICEBY=Tillage DIFF ADJUST=TUKEY;
SLICE Date*Treatment / SLICEBY=Date DIFF ADJUST=TUKEY;
SLICE Date*Treatment / SLICEBY=Treatment DIFF ADJUST=TUKEY;
SLICE Date*Treatment*Tillage / SLICEBY=Date*Tillage DIFF ADJUST=TUKEY;
SLICE Date*Treatment*Tillage / SLICEBY=Treatment*Tillage DIFF ADJUST=TUKEY;
RUN;
ODS PDF CLOSE;
