# voodoo
Validation and Verification of Database Tables

/*
%let pgm=oto_voodoo;
* use this to get locations of macros for easy editiong;
* once program is solid you may want to move macros to autocall library;
data _null_;
infile "c:\utl\oto_voodoo.sas" lrecl=300 recfm=v;
input;
if index(lowcase(_infile_),'%macro')>0 then do;
 macro=scan(left(_infile_ ),1,'(');
 put @5 _n_ @15 macro $32.;
end;
run;quit;


What this macro does

see the tail of this program for same usage
You should be able to run this as is.

The macro invocation is at the end of this file

1.   Dataset level summary -- ie number of obs, variable types, static data
2.   Cardinality page  (primary keys, codes/decodes  number of unique
     values for every variable - EG fails here)
3.   Complete frequency for all variables numeric and character with less
     than 200 levels
4.   For variables with over 200 levels top 100 most frequent and bottom
     100 least frequent
     Least frequent are the more interesting cases.
5.   Proc means on all numeric variables
6.   Proc univariate on all numeric variables
7.   Special datetime  analysis
9.   Histograms on all variables with less than 200 levels
10.  Proc contents
11.  Frequency of all numeric variables Missing, negative, zero and positive
12.  Duplicates on single or compound key. Output printed vertically for
     easy comparison
13.  Cross tabs of every variable with every other variable top 16 levels
     (if selectd)
14.  You can also select one variable to cross tab with all other variables
     max top 16 levels
16.  Maximum and minimum lengths to hold all numeric and character variables
     exactly (optimize)
17.  Correlation of all pairs of numeric variables sorted by largest
    correlation to lowest.
18.  Nice display of max and mins for numeric and character in one table
19.  List of identical columns ie date and date1 have equal values on all
     observations
19   One to Many, Many to One, One to Many and Many to Many
20   Cochran-Mantel-Haenszel Statistics
21   Finds missing patterns
22   Printout of first 20, middle 20 and last 20 observations.


for easy editing here are the locations macros
prefix area helps

also these macros do the analyses above

   54        %macro utlnopts
   90        %macro _vdo_macnam
   106       %macro utlfkil
   155       %macro nobs
   196       %macro nvar
   271       %macro _vdo_cdedec
   303       %macro _vv_annxtb
   415       %macro _vdo_basic
   3056      %macro _vdo_optlen
   3149      %macro _vdo_getmaxmin
   3174      %macro _vdo_getmaxmin001;
   3242      %macro _vdo_begmidend
   3336      %macro _vdo_clean
   3407      %macro _vdo_chartx
   3611      %macro _vdo_mispop
   3658      %macro _vdo_keyunq
   3741      %macro _vdo_dupcol
   3826      %macro _vdo_cor
   3889      %macro _vdo_mnymny
   3904      %macro _vdo_relhow
   4027      %macro _vdo_cmh
   4123      %macro _vdo_tabone
   4188      %macro _vdo_taball
   4268      %macro _vdo_unqtwo
   4375      %macro qcmprltb
   4401      %macro qblankta
   4430      %macro qblanktc
   4451      %macro qlastvar
   4481      %macro _vdo_mispat
   5031      %macro utl_getstm
   5044      %macro DirExist
   5056      %macro utlvdoc
*/
