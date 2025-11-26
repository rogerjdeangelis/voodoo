# voodoo

Validation and Verification of Database Tables. Profile any SAS dataset. Cardinality, outliers, top n frequencies, bottom n frequencies, one to many, many to many, associations between numerical and character variables and duplicates on key

There are two versions of VOODOO

  1 SAS version see oto_voodoo_20231031.sas (full output oto_voodoo_zipcode.txt)
  2 Altair SLC version slc_voodoo_20251126.sas (slc_voodoo_zipcode.txt I run with ultaedit))

What this macro does

see the tail of this program for usage
You should be able to run this as is.

The macro invocation is at the end of this file

    1. Dataset level summary -- ie number of obs, variable types, static data                                                                 
    2. Cardinality page (primary keys, codes/decodes number of unique values for every variable - EG fails here)                              
    3. Complete frequency for all variables numeric and character with less than 200 levels                                                   
    4. For variables with over 200 levels top 100 most frequent and bottom 100 least frequent Least frequent are the more interesting cases.  
    5. Proc means on all numeric variables                                                                                                    
    6. Proc univariate on all numeric variables                                                                                               
    7. Special datetime analysis                                                                                                              
    8. Histograms on all variables with less than 200 levels                                                                                  
    9. Proc contents                                                                                                                          
    10. Frequency of all numeric variables Missing, negative, zero and positive                                                               
    11. Duplicates on single or compound key. Output printed vertically for easy comparison                                                   
    12. Cross tabs of every variable with every other variable top 16 levels (if selectd)                                                     
    13. You can also select one variable to cross tab with all other variables max top 16 levels                                              
    14. Maximum and minimum lengths to hold all numeric and character variables exactly (optimize)                                            
    15. Correlation of all pairs of numeric variables sorted by largest correlation to lowest.                                                
    16. Nice display of max and mins for numeric and character in one table                                                                   
    17. List of identical columns ie date and date1 have equal values on all observations                                                     
    18. One to Many, Many to One, One to Many and Many to Many                                                                                
    19. Cochran-Mantel-Haenszel Statistics (Cramer relationship amoung catagorical variables)                                                 
    20. Finds missing patterns                                                                                                                
    21. Printout of first 20, middle 20 and last 20 observations.                                                                             
    22. Missing Pattern amalysis                                                                                                              
    23. Missing populated in a single table                                                                                                   
    24. Missing Pattern Analysis  

    SAMPLE RUN

    %inc "c:/oto/oto_voodoo.sas";

    %utlvdoc
    (
    libname        = sashelp         /* libname of input dataset */
    ,data          = zipcode      /* name of input dataset */
    ,key           = 0            /* 0 or variable */
    ,ExtrmVal      = 10           /* display top and bottom 30 frequencies */
    ,UniPlot       = 0            /* 0 or univariate plots    */
    ,UniVar        = 0            /* 0 or univariate analysis */
    ,chart         = 0            /* 0 or proc chart horizontal histograme */
    ,misspat       = 0            /* 0 or 1 missing patterns */
    ,taball        = 0            /* 0 crosstabs of all pairwise combinations of vriables */
    ,tabone        = 0            /* 0 or all pairwise cross tabs with limits */
    ,mispop        = 0            /* 0 0 negative positive or missing on each variable */
    ,mispoptbl     = 0            /* 0 missing populated table */
    ,dupcol        = 0            /* 0 do two columns have the same values in all rows */
    ,unqtwo        = 0            /* 0 only use to find primary key unique leveels of compund keys */
    ,vdocor        = 0            /* 0 or all pairwise parametric and non parametric collolations */
    ,oneone        = 0            /* 0 or 1:1  1:many many:many */
    ,cramer        = 1            /* 0 or cramer V variable crossed with all others */
    ,optlength     = 0            /* 0 optimum length for character and numeric variables */
    ,maxmin        = 0            /* 0 or max min for every varuiable */
    ,unichr        = 0            /* 0 univariate analysis of character variiables */
    ,outlier       = 0            /* 0 robust regression determination of outliers */
    ,printto       = c:\txt\vdo\&data..txt  /* save the voluminous output */
    ,Cleanup       = 0
    );



