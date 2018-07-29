#SAS Code for Statistical Analysis of Enzyme Activity

/* prepare plot-specific data */
PROC IMPORT OUT=mydata0
DATAFILE = "F:\M.Sc\Research and Data\SAS Code\BSData_11012016.xlsx"
DBMS = xlsx REPLACE;
SHEET = "Sheet1";
GETNAMES = YES;
RUN;

PROC SORT DATA=mydata0;
BY LabGroup;
RUN;

PROC MEANS DATA=mydata0 NOPRINT;
VAR Date1-Date7 Moisture1-Moisture7 Nitrate1-Nitrate7 Ammonium1-Ammonium7; 
BY LabGroup;
OUTPUT OUT=mydata1 MEAN=DateMean1-DateMean7 MMean1-MMean7 NMean1-NMean7 AMean1-AMean7;
RUN;

/* wide format */
DATA mydata1;
SET mydata1;
Plot = SCAN(LabGroup, 1, ";");
Rep = SCAN(LabGroup, 2, ";");
Treatment = SCAN(LabGroup, 3, ";");
Tillage = SCAN(LabGroup, 4, ";");
DROP _TYPE_ _FREQ_;
RUN;
/* long format */
DATA mydata2;
SET mydata1;
ARRAY DateList(7) DateMean1-DateMean7;
ARRAY MList(7) MMean1-MMean7;
ARRAY NList(7) NMean1-NMean7;
ARRAY AList(7) AMean1-AMean7;
DO Date = 1 to 7; 
  SBG = DateList(Date);
  Moisture = MList(Date);
  Nitrate = NList(Date);
  Ammonium = AList(Date);
  IF SBG NE " " THEN OUTPUT;
END;
DROP LabGroup DateMean1-DateMean7 MMean1-MMean7 NMean1-NMean7 AMean1-AMean7;
RUN;



/* Manipulate precipitation and temperature data */
PROC IMPORT OUT = prectemp
  DATAFILE = "F:\M.Sc\Research and Data\SAS Code\Precipitation_1206.xlsx" 
  DBMS = xlsx REPLACE;
  SHEET = "Sheet1"; 
  GETNAMES = YES;
RUN;

DATA prectemp_temp;
  SET prectemp;
  RETAIN prec_from0 temp_from0 ndays_from0 0;
  ndays_from0 = ndays_from0 + 1;
  prec_from0 = prec_from0 + prec;
  temp_from0 = temp_from0 + temp;
  IF sample_soil="y" THEN OUTPUT;
RUN;

DATA prectemp_summary;
  SET prectemp_temp;
  RETAIN prec_fromlast temp_fromlast ndays_fromlast 0;
  prec_fromlast = DIF(prec_from0);
  temp_fromlast = DIF(temp_from0);
  ndays_fromlast = DIF(ndays_from0);
  IF _N_=1 THEN DO;
    prec_fromlast = prec_from0;
	temp_fromlast = temp_from0;
	ndays_fromlast = ndays_from0;
  END;
  DROP prec temp Bare_Soil_Temperature__Ce_ sample_soil;
RUN;

PROC SORT DATA=mydata2 OUT=mydata3;
BY Date;
RUN;

DATA prectemp_tocombine;
SET prectemp_summary;
myDate = _N_;
DO i = 1 to 24;
  OUTPUT;
END;
DROP i;
RUN;

DATA data_wPrecTemp;
  SET mydata3;
  SET prectemp_tocombine;
  rowID = _N_;
  inorganicN = Nitrate + Ammonium;
RUN;

PROC EXPORT DATA = data_wPrecTemp 
            OUTFILE = "W:\pu.data\Desktop\SCS project\data_wPrecTemp.xlsx"
			DBMS = XLSX REPLACE;
RUN;


/* visualization */
/*
1. boxplot: continuous response, categorical predictor (maybe two)
2. scatter plot: two continuous variables (possibly with one categorical variable)
3. interaction plot: continuous response, two categorical predictors 
*/
/* 1. boxplot */
/* http://support.sas.com/kb/43/090.html */
PROC SGPLOT DATA=data_wPrecTemp;
  VBOX SBG / CATEGORY=Tillage GROUP=Treatment;
  XAXIS LABEL="X axis categories";
  KEYLEGEND / TITLE="Your Title Here";
  WHERE (myDate = 4 | myDate = 5);
RUN; 
/* 2. scatter plot with correlation */
PROC SGPLOT DATA=data_wPrecTemp;
  SCATTER X=Moisture Y=SBG / GROUP=Treatment DATALABEL=myDate
                             MARKERATTRS=(symbol=circlefilled);
*  WHERE (myDate = 4 | myDate = 5);
RUN;
PROC CORR DATA=data_wPrecTemp;
  VAR Moisture Nitrate Ammonium;
RUN;
/* 3. interaction plot */
/* https://www.ecu.edu/cs-dhs/bios/upload/EDMCSAS12.pdf */
PROC MEANS DATA=data_wPrecTemp NOPRINT;
 CLASS Treatment Tillage;
 VAR SBG;
 OUTPUT OUT=data_wPrec_m MEAN=SBG_m;
RUN;
PROC SGPLOT DATA=data_wPrec_m;
  VLINE Treatment / RESPONSE=SBG_m GROUP=Tillage MARKERS;
RUN;


/* fit the model */
PROC SORT DATA=data_wPrecTemp;
BY Plot;
RUN;
PROC MIXED DATA=data_wPrecTemp PLOTS=ALL;
* BY Date; 
* WHERE (myDate=3 | myDate=4); /* a particular date */
CLASS Plot Rep Treatment Tillage Date;
MODEL SBG = Treatment Tillage Treatment*Tillage
            Date
            Moisture inorganicN
			Moisture*Treatment inorganicN*Date
            temp_fromlast;
RANDOM Rep;
REPEATED / SUBJECT=Plot TYPE=AR(1);
/* correlation: http://www2.sas.com/proceedings/sugi30/198-30.pdf */
LSMEANS Treatment / DIFF ADJUST=TUKEY;
SLICE Treatment*Tillage / SLICEBY=Tillage DIFF ADJUST=TUKEY;
RUN;
