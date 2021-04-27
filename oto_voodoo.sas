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

LINE LOCATION OF MACROS

154       %macro utlnopts
191       %macro cmpres
219       %macro _vdo_macnam
235       %macro utlfkil
284       %macro nobs
326       %macro nvar
401       %macro _vdo_cdedec
433       %macro _vv_annxtb
545       %macro _vdo_basic
3187      %macro qcmprltb
3213      %macro qblankta
3242      %macro qblanktc
3263      %macro qlastvar
3294      %macro _vdo_misspat
3665      %macro _vdo_optlen
3758      %macro _vdo_getmaxmin
3783      %macro _vdo_getmaxmin001;001;
3851      %macro _vdo_begmidend
3945      %macro _vdo_clean
4016      %macro _vdo_chartx
4220      %macro _vdo_mispop
4259      %macro _vdo_mispoptbl
4349      %macro _vdo_keyunq
4432      %macro _vdo_dupcol
4517      %macro _vdo_cor
4583      %macro _vdo_mnymny
4598      %macro _vdo_relhow
4721      %macro _vdo_cmh
4817      %macro _vdo_tabone
4882      %macro _vdo_taball
4962      %macro _vdo_unqtwo
5164      %macro utl_getstm
5177      %macro DirExist
5191      %MACRO _vdo_UNICHR
5855      %macro _vdo_outlyr
6036      %macro utlvdoc
6339      %macro offcall

*/

*   *   ***    ***   ****    ***    ***
*   *  *   *  *   *   *  *  *   *  *   *
*   *  *   *  *   *   *  *  *   *  *   *
*   *  *   *  *   *   *  *  *   *  *   *
*   *  *   *  *   *   *  *  *   *  *   *
 * *   *   *  *   *   *  *  *   *  *   *
  *     ***    ***   ****    ***    ***;



%macro utl_close;

  %utlnopts;

  /* https://communities.sas.com/t5/user/viewprofilepage/user-id/12151 */

  %local i rc;
  %do i=1 %to 1000;
    %let rc=%sysfunc(close(&i));
  %end;
  %utlopts;
%mend utl_close;

%utl_close;

%macro lowcase(string);
%*********************************************************************;
%*                                                                   *;
%*  MACRO: LOWCASE                                                   *;
%*                                                                   *;
%*  USAGE: 1) %lowcase(argument)                                     *;
%*                                                                   *;
%*  DESCRIPTION:                                                     *;
%*    This macro returns the argument passed to it unchanged         *;
%*    except that all upper-case alphabetic characters are changed   *;
%*    to their lower-case equivalents.                               *;
%*                                                                   *;
%*  E.g.:          %let macvar=%lowcase(SAS Institute Inc.);        %*;
%*  The variable macvar gets the value "sas institute inc."          *;
%*                                                                   *;
%*  NOTES:                                                           *;
%*    Although the argument to the %UPCASE macro function may        *;
%*    contain commas, the argument to %LOWCASE may not, unless       *;
%*    they are quoted.  Because %LOWCASE is a macro, not a function, *;
%*    it interprets a comma as the end of a parameter.               *;
%*                                                                   *;
%*********************************************************************;
%sysfunc(lowcase(%nrbquote(&string)))
%mend;

%macro qleft(text);
%*********************************************************************;
%*                                                                   *;
%*  MACRO: QLEFT                                                     *;
%*                                                                   *;
%*  USAGE: 1) %qleft(argument)                                       *;
%*                                                                   *;
%*  DESCRIPTION:                                                     *;
%*    This macro returns the argument passed to it without any       *;
%*    leading blanks in a quoted form. The syntax for its use        *;
%*    is similar to that of native macro functions.                  *;
%*                                                                   *;
%*    Eg. %let macvar=%qleft(&argtext)                               *;
%*                                                                   *;
%*  NOTES:                                                           *;
%*    The %VERIFY macro is used to determine the first non-blank     *;
%*    character position.                                            *;
%*                                                                   *;
%*********************************************************************;
%local i;
%if %length(&text)=0 %then %let text=%str( );
%let i=%verify(&text,%str( ));
%if &i %then %qsubstr(&text,&i);
%mend;

%macro utlnopts(note2err=nonote2err,nonotes=nonotes)
    / des = "rock solid code fast and clean";

OPTIONS
     &nonotes
     FIRSTOBS=1
     NONUMBER
     lrecl=384
     NOFMTERR     /* turn  Format Error off                           */
     NOMACROGEN   /* turn  MACROGENERATON off                         */
     NOSYMBOLGEN  /* turn  SYMBOLGENERATION off                       */
     &NONOTES     /* turn  NOTES off                                  */
     NOOVP        /* never overstike                                  */
     NOCMDMAC     /* turn  CMDMAC command macros on                   */
     NOSOURCE    /* turn  source off * are you sure?                 */
     NOSOURCE2    /* turn  SOURCE2   show gererated source off        */
     NOMLOGIC     /* turn  MLOGIC    macro logic off                  */
     NOMPRINT     /* turn  MPRINT    macro statements off             */
     NOCENTER     /* turn  NOCENTER  I do not like centering          */
     NOMTRACE     /* turn  MTRACE    macro tracing                    */
     NOSERROR     /* turn  SERROR    show unresolved macro refs       */
     NOMERROR     /* turn  MERROR    show macro errors                */
     OBS=MAX      /* turn  max obs on                                 */
     NOFULLSTIMER /* turn  FULLSTIMER  give me all space/time stats   */
     NODATE       /* turn  NODATE      suppress date                  */
     DSOPTIONS=&NOTE2ERR
     ERRORCHECK=STRICT /*  syntax-check mode when an error occurs in a LIBNAME or FILENAME statement */
     DKRICOND=ERROR    /*  variable is missing from input data during a DROP=, KEEP=, or RENAME=     */

     /* NO$SYNTAXCHECK  be careful with this one */
;

RUN;quit;

%MEND UTLNOPTS;


%macro cmpres(text);
%*********************************************************************;
%*                                                                   *;
%*  MACRO: CMPRES                                                    *;
%*                                                                   *;
%*  USAGE: 1) %cmpres(argument)                                      *;
%*                                                                   *;
%*  DESCRIPTION:                                                     *;
%*    This macro returns the argument passed to it in an unquoted    *;
%*    form with multiple blanks compressed to single blanks and also *;
%*    with leading and trailing blanks removed.                      *;
%*                                                                   *;
%*    Eg. %let macvar=%cmpres(&argtext)                              *;
%*                                                                   *;
%*  NOTES:                                                           *;
%*    The %LEFT and %TRIM macros in the autocall library are used    *;
%*    in this macro.                                                 *;
%*                                                                   *;
%*********************************************************************;
%local i;
%let i=%index(&text,%str(  ));
%do %while(&i ne 0);
  %let text=%qsubstr(&text,1,&i)%qleft(%qsubstr(&text,&i+1));
  %let i=%index(&text,%str(  ));
%end;
%left(%qtrim(&text))
%mend;

%macro _vdo_macnam(macnam);

 data _null_;
 file "delete.dat";
 put "D&macnam";
 run;

 filename ft15f001 "delete.dat";
 proc explode;
 run;

 filename ft15f001 clear;
 run;

%mend _vdo_macnam;

%macro utlfkil
    (
    utlfkil
    ) / des="delete an external file";


    /*-------------------------------------------------*\
    |                                                   |
    |  Delete an external file                          |
    |   From SAS macro guide
    |  Sample invocations                               |
    |                                                   |
    |  WIN95                                            |
    |  %utlfkil(c:\dat\utlfkil.sas);                    |
    |                                                   |
    |                                                   |
    |  Solaris 2.5                                      |
    |  %utlfkil(/home/deangel/delete.dat);              |
    |                                                   |
    |                                                   |
    |  Roger DeAngelis                                  |
    |                                                   |
    \*-------------------------------------------------*/

    %local urc;

    /*-------------------------------------------------*\
    | Open file   -- assign file reference              |
    \*-------------------------------------------------*/

    %let urc = %sysfunc(filename(fname,%quote(&utlfkil)));

    /*-------------------------------------------------*\
    | Delete file if it exits                           |
    \*-------------------------------------------------*/

    %if &urc = 0 and %sysfunc(fexist(&fname)) %then
        %let urc = %sysfunc(fdelete(&fname));

    /*-------------------------------------------------*\
    | Close file  -- deassign file reference            |
    \*-------------------------------------------------*/

    %let urc = %sysfunc(filename(fname,''));

  run;

%mend utlfkil;

%macro nobs( libname= , data= );

proc sql noprint;select count(*) into :nobs separated by ' ' from &libname..&data;quit;

%mend nobs;

%*---------------------------------------
%*  Program:        nvar.sasmac
%*  Author:         Karsten Self
%*  Date:           2/8/96
%*
%*  copyright (c) 1996 Karsten M. Self
%*
%*  Rights for free redistribution granted if copied in whole and copyright
%*  notice is maintained.
%*
%*  THIS CODE IS PROVIDED ON AN 'AS IS' BASIS WITH NO WARRANTEE WHATSOEVER.
%*
%*  Description:    returns &nvar as observations in a dataset.
%*
%*  Bugs + quirks:
%*                  - behavior with tape data libraries is unknown, probably
%*                   does not work.
%*
%* --------------------
%*  Revised by:     KMS
%*  Revision Date:  2/13/96
%*  Version:       0.2b
%*
%* --------------------
%*  Modification log:
%*
%*  date        pgmr    ver     notes
%* -------      ----    ----    -----
%*  2/8/96      kms     0.2a    Created (do not ask me how it got to be 0.2a)
%*
%*  2/13/96     kms     0.2b    Modified NOTE/WARNING/ERROR messages to refer to this macro;
%*  1/10/2010   rjd     0.2c    Fiixed robust regression outlier anaysys                   ;
%*
%*---------------------------------------
;

%macro nvar( libname= , data= );

    /*
      %let libname=work;
      %let data=tstdatchr;
    */

    %let nvar = ;
    %let exit = 0;

    %if %length( &libname ) gt 8 %then
        %do;
        %put ERROR{} (NVAR macro) The libname %upcase( &libname ) contains more than 8 characters.;
        %let exit = 1;
        %end;

    %if %length( &data ) gt 32 %then
        %do;
        %put ERROR{} (NVAR macro) The dataset name %upcase( &data ) contains more than 8 characters.;
        %let exit = 1;
        %end;

    %if &exit gt 0 %then
        %goto exit;

    options nonotes;
    data _vvnumchr;
        set _vvtable(
            where=(
                libname= %upcase( "&libname" ) and memname eq %upcase ( "&data" )
                )
            keep= nvar libname memname num_numeric num_character
            )
        ;

        format = 'F';
        if nvar gt 1 then
            width  = ceil( log( nvar ) );

        else
            width = 1;

        call symput( 'nvar', putn( nvar, format, width ));
        output;
        stop;
        run;quit;

        options notes;
        %if &nvar eq %then
            %do;
            %put WARNING: (NVAR macro) dataset %upcase( &libname ).%upcase( &data ) does not exist;
            %end;

        %exit:


%mend nvar;


 ***    ***   ****   *****         ****   *****   ***    ***   ****   *****
*   *  *   *   *  *  *              *  *  *      *   *  *   *   *  *  *
*      *   *   *  *  *              *  *  *      *      *   *   *  *  *
*      *   *   *  *  ****           *  *  ****   *      *   *   *  *  ****
*      *   *   *  *  *              *  *  *      *      *   *   *  *  *
*   *  *   *   *  *  *              *  *  *      *   *  *   *   *  *  *
 ***    ***   ****   *****         ****   *****   ***    ***   ****   *****;

****   *****  ****   ****   *****   ***     *    *****  *****  ****
 *  *  *      *   *  *   *  *      *   *   * *     *    *       *  *
 *  *  *      *   *  *   *  *      *      *   *    *    *       *  *
 *  *  ****   ****   ****   ****   *      *****    *    ****    *  *
 *  *  *      *      * *    *      *      *   *    *    *       *  *
 *  *  *      *      *  *   *      *   *  *   *    *    *       *  *
****   *****  *      *   *  *****   ***   *   *    *    *****  ****;

%macro _vdo_cdedec(dummy);
  /*
     data classchk;
       set sashelp.class(rename=(weight=sex_cd height=name_cd));
     run;

     %let libname=WORK;
     %let data=zip;
  */
     options nonotes;
     proc sql;
       create
         table _vv_two(where=(index(upcase(name),'_CD')=0)) as
       select
         name
        ,type
       from
         _vvcolumn
       where
         upcase(libname) = upcase("&libname") and
         upcase(memname) = upcase("&data")
       group
         by prxchange('s/_CD//',99,upcase(name))
       having
         count(prxchange('s/_CD//',99,upcase(name)))=2
     ;quit;
      options notes;

     proc format;
       value $best
     ;run;

     %macro _vv_annxtb(var);

          /* %let var=SEX; */

          /* code decode pairs have to have names like sex sex_cd */

          proc datasets nolist;
             delete _vv_nrm _vv_sec _vv_cut;
          run;quit;

          data _vv_nrm (keep=nam &var._cd &var rename=(&var._cd=cde &var=des ));
             length &var $80 &var._cd $32;
             set &libname..&data (
                  where=(s&var._cd ne '')
                  rename=(&var._cd=s&var._cd)
                  keep=&var &var._cd);
             array cd s&var._cd;
             nam   =substr(vname(cd[1]),2);;
             typ=vtype(cd[1]);
             if typ='N' then &var._cd=put(cd[1],best.);
             else &var._cd=s&var._cd;
          run;

          proc freq data=_vv_nrm order=freq noprint;
             tables nam*cde*des/missing list out=_vv_sec;
          run;

          %let nobs=%utl_nobs(_vv_sec);
          %let end=%eval(&nobs - 49);

          %put &nobs &end;

          %if &nobs > 0 %then %do;

              data _vv_cut (keep=nam grp cut cde des count percent cut);
                retain a b 0;
                %sysfunc(ifc ( (&nobs le 100),
                         %str(set _vv_sec;grp='ALL';),
                         %str(set _vv_sec(obs=50 in=a) _vv_sec(in=b firstobs=%eval(&end));grp='CUT';) ));
                select;
                  when (grp='CUT' and a ) cut='BEG';
                  when (grp='CUT' and b ) cut='END';
                  when (grp='ALL'       ) cut='ALL';
                end;
              run;

              proc append data=_vv_cut base=_vv_bascde;
              run;

          %end;

     %mend _vv_annxtb;

     proc datasets library=work nolist;
       delete _vv_bascde;
     run;

     %let fyl=%sysfunc(pathname(work))/cmd1.sas;

     data _null_;
       file "&fyl";
       set _vv_two;
       cmd=cats('%_vv_annxtb(',name,');');
       put cmd;
       putlog cmd;
       *call execute(cmd);
     run;

     %include "&fyl";

     proc format;
       value $top
      'ALL' = 'All'
      'BEG' = 'Top 30'
      'END' = 'Bottom 30'
     ;
     run;
     title;footnote;
     title "A given code should one and only one decode - one - one";
     proc report data=_vv_bascde nowd split='#';
     cols
      ( "Are Codes and Description All non-missing and one to one "
      nam
      grp
      cut
      cde
      des
      count
      percent
     )
     ;
     define nam     / order   order=data "Variable" style={just=left} width=10 flow;
     define grp     / order   order=data noprint;
     define cut     / order   order=data "Highest#Lowest 30#Frequency" format=$top. width=10;
     define cde     / display "Code"  width=16;
     define des     / display "Description"  width=55 flow;
     define count   / display order=data format=comma12. 'Count'  width=16;
     define percent / display order=data format=5.1 'Percent' width=16;
     run; quit;

%mend _vdo_cdedec;


****     *     ***   *****   ***
 *  *   * *   *   *    *    *   *
 *  *  *   *   *       *    *
 ***   *****    *      *    *
 *  *  *   *     *     *    *
 *  *  *   *  *   *    *    *   *
****   *   *   ***   *****   ***;


%macro _vdo_basic(dummy);

    * Date formats -- used for identifying date numeric variables later;
    %let dateFmt =
        %str(
        'DATE',
        'DAY',
        'DDMMYY',
        'DOWNAME',
        'JULDAY',
        'JULIAN',
        'MMDDYY',
        'MMYY',
        'MMYYC',
        'MMYYD',
        'MMYYN',
        'MMYYP',
        'MMYYS',
        'MONNAME',
        'MONTH',
        'MONYY',
        'NENGO',
        'QTR',
        'QTRR',
        'WEEKDATE',
        'WEEKDATEX',
        'WEEDKAY',
        'WORDDATE',
        'WORDDATEX',
        'YEAR',
        'YYMM',
        'YYMMC',
        'YYMMD',
        'YYMMN',
        'YYMMP',
        'YYMMS',
        'YYMMDD',
        'YYMON',
        'YYQ',
        'YYQR'
        );

    %let dttimFmt =
        %str(
        'DATETIME',
        'TOD'
        );

    %let timeFmt =
        %str(
        'HHMM',
        'HOUR',
        'MMSS',
        'MSEC',
        'PDTIME',
        'RMFDUR',
        'RMFSTAMP',
        'SMFSTAMP',
        'TIME',
        'TODSTAMP',
        'TU'
        );

    %let dtFmts=
        %str( &dateFmt., &dttimFmt., &timeFmt. );


    %*---------------
    %* Internal macros;


    *---------------
    * Get obs option setting for current session.  Save.  Reset to max for VV and restore on exit;
    proc sql noprint;
        create table work._vvRSOpt( label= "VV - SAS Options Reset Values" ) as
        select optname, setting
        from dictionary.options
        ;

        quit;

    options
        obs= max
        firstobs= 1
        compress= no
        pagesize= &PageSize.
        linesize= &LineSize.
        ;



    *----------------
    * Records and variables on requested dataset;

    /*
      %let libname=sashelp;
      %let data=class;
    */

    %let nobs = test;
    %nobs( Libname= &libname., Data= &data. );

    %let nvar = test;
    %nvar( Libname= &libname., Data= &data. );


    * No records? Quit;
    %if &nobs. lt 1 %then
            %do;
            %put ERROR: (VV macro) the dataset %upcase( &libname..&data. ) has zero records.;
            %put ERROR: (VV macro) Further processing halted.;
            %let exit = 1;
            %end;

    * No variables? Quit;
    %if &nvar. lt 1 %then
        %do;
        %put ERROR: (VV macro) the dataset %upcase( &libname..&data. ) has zero variables.;
        %put ERROR: (VV macro) Further processing halted.;
        %let exit = 1;
        %end;

    * Exit if flagged;
    %if &exit. gt 0 %then
        %goto exit;



    * ----------------
    * Parameter parsing;

    * ...FreqOrdr - change to default if illegal, with warning;

    %let FreqOrdr = %lowcase( &FreqOrdr. );

    %if &FreqOrdr. ne data       and
        &FreqOrdr. ne formatted  and
        &FreqOrdr. ne freq       and
        &FreqOrdr. ne internal      %then
        %do;
        %put WARNING: (VV macro)  invalid FreqOrdr &FreqOrdr. selected.  Using 'Freq' instead;
        %let FreqOrdr = freq;
        %end;


    * ...UniPlot - change to false if illegal, with warning;
/*
    %if &UniPlot. ne 0  %then
        %do;
        %put WARNING: (VV macro)  invalid UniPlot value &UniPlot. selected.  Using 'false' instead;
        %let UniPlot = false;
        %end;
*/
    * ...Cleanup - change to false if illegal, with warning;


    %let Cleanup = %lowcase( &Cleanup. );

    %if &Cleanup. ne 0  %then
        %do;
        %put WARNING: (VV macro)  invalid Cleanup value &Cleanup. selected.  Using 'false' instead;
        %let Cleanup = false;
        %end;



    * ...Title - trunc to 200 characters if too long;
    %if %length( &title. ) gt 200 %then
        %do;
        %let title = %substr( &title, 1, 200 );
        %end;



    * ----------------
    * Else, get serious;


    title;     * Clear titles;
    options nonotes;
    * Generate sql for query.  This is run immediately following data step;
    data _null_;

        length text $ 200;

        set _vvcolumn(
            where= ( libname = "%upcase( &libname. )" and memname = "%upcase( &data. )" ))
            end = last
            ;

        * First -- open sql statment;
        if _n_ = 1 then
            do;

            text = "proc sql noprint; " ;
            call execute( text );

            text = "create table work._vv1M( label= 'VV - Master - distinct values') as" ;
            call execute( text );

            text = "   select" ;
            call execute( text );

            text = "   count ( * ) as records, " ;
            call execute( text );

            end;


        * First and all but last -- generate query statment for variable counts;
        if not last then
            do;

            text = "count ( distinct " || name || ") as " || name || " , ";
            call execute( text );

            end;


        * Last -- close sql statement;
        else if last then
            do;
            text = "count ( distinct " || name || ") as " || name ;
            call execute( text );

            text = "from &libname..&data. " ;
            call execute( text );

            text = "; " ;
            call execute( text );

            text = "quit; " ;
            call execute( text );

            end;

        run;
    options notes;

    title "DATA VERIFICATION + VALIDATION FOR %upcase( &libname. ).%upcase( &data. )";

    %if %length( &title. ) gt 0 %then
        %do;
        title2 "&title.";
        %end;


    * Generate a dataset with variable names, labels, and counts;

    * ...transpose what you have ;
    proc transpose
        data= work._vv1M
        out= work._vv1M(
            label= "VV - SQL results - transposed"
            )
        prefix= col
        name= _name_
        ;
    run;

    options nonotes;
    * ...get more info.  Label, type, length, format;
    data _vv1D(
        label= "VV - Master - Dictionary "
        rename= ( name = variable )
        )
        ;

        keep name label type length format;

        set _vvcolumn (
            where= ( libname = "%upcase( &libname. )" and memname = "%upcase( &data. )" )
            );


        run;
    options notes;

    * join them in...;
    proc sql noprint;
        create table _vv1M(
            compress = no
            label= 'VV - Master - merge '
            ) as
        select
            M._name_ as variable label= "Variable Name",
            M.col1   as values   label= "# of Distinct Values" format= comma10. ,
            D.label  as label    label= "Label",
            D.type   as type     label= "Type",
            D.length as length   label= "Length",
            D.format as format   label= "Format"

        from
            work._vv1M as M,
            work._vv1D as D

        where
            M._name_ = D.variable
        order by D.variable
        ;
        quit;



    * More stuff to do;
    * ...identify character and numeric types for further analysis;
    *    character...
    *          ...if less than &ValCtOff values, proc freq in frequency order;
    *         ...if more than &ValCtOff values, 10 most frequent, 10 least frequent values;
    *    numeric
    *    ...if not a date....
    *            ...all - proc univariate
    *            ...less than &ValCtOff values, proc freq
    *    ...if a date
    *            proc freq on mm/yy format or something similar
    *    ...by number of values
    *       - none (null)
    *       - one (n_one) -- these guys need more looking at to see if there is just
    *         one value, or if sometimes there are missing values.  Keep them only if
    *         there is one and only one value, none missing
    ;




    * Get nobs count again, just to be sure;
    %nobs( libname= &libname., data= &data. );

    data
        _vv1M( label  = "VV - Master - classed" )

            _vvch1(
            label = "VV - Char <= &ValCtOff. values"
            drop=
                nNumA
                nNum
                nDate
                nNul
                nulls
                n_One
                n_One0
                n_One1
                 )

            _vvch2(
            label = "VV - Char  > &ValCtOff. values"
            drop=
                nNumA
                nNum
                nDate
                nNul
                nulls
                n_One
                n_One0
                n_One1
                )

        _vvNumA(
            label = "VV - Num - All quantity + date/time"
            drop=
                nChar
                nDate
                nNul
                nulls
                n_One
                n_One0
                n_One1
                )

            _vvNum1(
            label = "VV - Num <= &ValCtOff. values "
            drop=
                nChar
                nDate
                nNul
                nulls
                n_One
                n_One0
                n_One1
                )

            _vvNum2(
            label = "VV - Num  > &ValCtOff. values "
            drop=
                nChar
                nDate
                nNul
                nulls
                n_One
                n_One0
                n_One1
                )

            _vvDt(
            label  = "VV - Date/Time variables"
            drop=
                nChar
                nNumA
                nNum
                nNul
                nulls
                n_One
                n_One0
                n_One1
                )


        _vvNul(
            label  = "VV - Not evaluated"
            drop=
                nChar
                nNumA
                nNum
                nDate
                nulls
                n_One
                n_One0
                n_One1
                )

        _vvOne(
            label  = "VV - Ones vars"
            drop=
                nChar
                nNumA
                nNum
                nDate
                nNul
                n_One0
                n_One1
                )

        _vvErr(label  = "VV - Error output" )
        ;

        * Redundant and overkill, but what the hey;
        length
            nObs
            nVar
            nChar
            nNum
            nDate
            nNul
            n_One
                 8.
            label
                 $40
        ;


        retain

            /* Tabulation counters */

            nObs
            nVar
            nChar
            nNum
            nDate
            nNul
            n_One

            /* Null and One counts (values supplied in later data step) */

            nulls
            n_One0
            n_One1

            /* Initial value */

                0
            ;

        keep
            /* Info fields */
            variable
            label
            values
            format
            type
            length

            /* Dataset counts */
            _n
            nObs
            nVar

            /* Var counts */
            nChar
            nNumA
            nNum
            nDate
            nNul
            n_One

            /* Sepcial cases */

            nulls
            n_One0
            n_One1

            ;



        * Initial pass through data to pick up summary tabulations;
        if _n_ eq 1 then
            do;

            do point = 1 to _vars;

                set _vv1M
                    nobs= _vars
                    point= point
                    ;

                if _error_ then abort;

                nObs = &nobs.;
                nVar = _vars;

                select( type );
                    when( "char" )
                        nChar + 1;

                    when( "num" )
                        do;

                        nNumA + 1;

                        if upcase( compress( format, '0123456789. ' )) in( &dtFmts. ) then
                            nDate + 1;

                        else
                            nNum + 1;

                        end;

                    otherwise
                        put "ERROR: (VV macro) unexpected 'type' value:  " type= variable= _n_=;

                    end;   * select(type) processing;

                if values eq 0 then
                    nNul + 1;

                if values eq 1 then
                    n_One + 1;



                * While we are here, generate some width formatting variables;
                if point eq 1 then
                    do;
                    wnObs = max( 5, round( 1 + ( log10( nobs ) + floor( log10( nobs ) / 3 ))));
                    call symput( 'wnObs', put( wnObs, f2. ) );
                    end;


                end;   * do point= processing;
            end;   * if _n_ eq 1 processing;


        set _vv1M;



        * Output, continue process;
        if values eq 1 then
            do;
            cvvOne + 1;
            _n = cvvOne;
            output _vvOne;
            end;


        * Output all;
        cvv1M + 1;
        _n = cvv1M;
        output _vv1M;


        * Output;
        if values eq 0 then
            do;
            cvvNul + 1;
            _n = cvvNul;
            output _vvNul;
            end;


        select( type );

            when( "char" )
                do;
                if values le &ValCtOff. then
                    do;
                    cvvCh1 + 1;
                    _n = cvvCh1;
                    output _vvch1;
                    end;

                else
                    do;
                    cvvCh2 + 1;
                    _n = cvvCh2;
                    output _vvch2;
                    end;
                end;  * 'char' processing;

            when( "num" )
                do;
                cvvNumA + 1;
                _n = cvvNumA;
                output _vvNumA;

                * Date type data;
                if upcase( compress( format, '0123456789. ' )) in( &dtFmts. ) then
                    do;
                    cvvDt + 1;
                    _n = cvvDt;
                    output _vvDt;
                    end;

                else
                    do;
                    if values le &ValCtOff. then
                        do;
                        cvvNum1 + 1;
                        _n = cvvNum1;
                        output _vvnum1;
                        end;
                    else
                        do;
                        cvvNum2 + 1;
                        _n = cvvNum2;
                        output _vvnum2;
                        end;
                    end;  * numerics;

                end;

            otherwise
                do;

                error;
                put "ERROR: (VV macro) Unexpected data type " type " in %upcase( &libname. ).%upcase( &data. )";
                cvvErr + 1;
                _n = cvvErr;
                output _vverr;

                end;

            end;   * select(type);

        run;


    * Index numeric datasets by variable (to allow sequential processing later);

    proc datasets lib= work nolist;

        modify _vvNum1;
        index create variable;

        modify _vvNum2;
        index create variable;

        run;
        quit;



    *----------------------------------------
    *
    *        Beginning of 'Ones' variable processing
    *
    *----------------------------------------
    *
    *
    * If there are any 'Ones' variables, find out their null counts and the single value, use to
    *  update Master and Ones datasets.
    *
    * Because of CALL EXECUTE processing, this processing cannot be run if there are no 'Ones'
    *  variables -- much unhappiness results.
    *
    * I am breaking this up into several smaller chunks in order to avoid having a macro do loop
    *   spanning pages of code
    *
    *  Process:
    *
    *     - Find out if there are any 'Ones' records (by obs count)
    *     - Set test variable (doOnes)
    *     - Execute each step if true
    ;

    %nobs( libname= work, data= _vvOne );

    %let doOnes = false;

    %if &nObs. gt 0 %then
        %let doOnes= true;



    %* ----------------;
    %if &doOnes. eq true %then
        %do;

        * Generate SQL to find number of nulls for 'Ones' variables;

        data _null_;

            length text $ 200;

            set work._vvOne
                end = last
                ;

            * First -- open sql statment;
            if _n_ = 1 then
                do;

                text = "proc sql noprint; " ;
                call execute( text );

                text = "create table work._vvOneS1(label= 'VV - Ones vars - nulls') as" ;
                call execute( text );

                text = "   select" ;
                call execute( text );

                end;


            * First and all but last -- generate query statment for variable counts;
            if not last then
                do;

                text = "nmiss( " || variable || ") as " || variable || " , ";
                call execute( text );

                end;


            * Last -- close sql statement;
            else if last then
                do;
                text = "nmiss( " || variable || ") as " || variable ;
                call execute( text );

                text = "from &libname..&data. " ;
                call execute( text );

                text = "; " ;
                call execute( text );

                text = "quit; " ;
                call execute( text );

                end;

        run;

        %end;  * doOnes processing step;
        %* ----------------;




    %* ----------------;
    %if &doOnes. eq true %then
        %do;


        * Transpose to turn variables as variables to records with variable names;

        proc transpose
            data= work._vvOneS1
            out= work._vvOneS1( label= 'VV - Ones - nulls Transposed' );
        run;

        proc datasets lib= work
            nolist
            ;

            modify _vvOneS1;
            rename
                _name_= variable
                col1  = nulls
                ;
        run;quit;

        %end;  * doOnes processing step;
        %* ----------------;




    %* ----------------;
    %if &doOnes. eq true %then
        %do;


        * Get data value associated with each 'Ones' variable;
        * ...efficiently, even (maybe)

        * ...first want to get max width of 'value';
        options nonotes;
        proc sql noprint;
            select max( length )
                into :MaxLen
                from _vvcolumn
                where libname= "%upcase( &libname. )" and memname= "%upcase( &data. )"
                ;

            quit;
        options notes;

        * ...code-generating datastep;
        data _null_;

            length   text   $ 200;

            array PutFmt{ &nVar. }   $ 16 _temporary_;   * Formatting for each variable;
            array Alignmnt{ &nVar. } $ 5  _temporary_;   * Alignment for each variable;

            set work._vvOne
                end  = last
                nobs = _nvar
                ;

            * First -- open data step;
            if _n_ eq 1 then
                do;

                * Initializaitons;
                MaxLen = min( 30, &MaxLen. );   * Maximum data length (restricted to 30);
                retain MaxLen;


                text= "    data work._vvOneV1(label= 'VV - Ones values') ;" ;
                call execute( text );

                text= "    keep variable value;" ;
                call execute( text );

                * Added put statement to solve numeric-to-char conversion problem (kms 2/7/96);
                text= "    length value $ " || put( MaxLen, f4. ) || "; " ;
                call execute( text );

                text= "    if _n_ eq 1 then do; " ;
                call execute( text );

                end;  * _n_ eq  processing;


            * Accumulate non-null value of each variable - using multiple set statements with 'where'
            *  processing to eliminate nulls
            ;

            * ...set up options for char/num processing -- format, alignment;
            select( type );

                    * Character processing;
                when( "char" )
                    do;

                    * Used to align formatted value for output;
                    Alignmnt{ _n_ } = 'left';

                    * Display format;
                    if format ne ' ' then
                        PutFmt{ _n_ } = format;

                    else
                        PutFmt{ _n_ } = compress( '$F' || ( put( MaxLen, best. )) || '.' );

                    * Missing values test;
                    MissVal = "' '";

                    end;  * type(char) processing;


                * Numeric processing;
                when( "num" )
                    do;

                    * Several possibilities: date, time, datetime, or quantity.  Question is whether
                    * or not there is a format to use.  If there is, use it.
                    ;

                    * Used to align formatted value for output;
                    Alignmnt{ _n_ } = 'right';

                    * Display format;
                    if format ne ' ' then
                        PutFmt{ _n_ } = format;

                    else
                        PutFmt{ _n_ } = compress( 'best' || ( put( MaxLen, best.)) || '.' );


                    * Missing values test;
                    MissVal = ".";

                    end;  * type(num) processing;


                otherwise
                    do;

                    * Bad type variable;
                    error "ERROR: (VV macro) bad variable TYPE value: %upcase( &libname. ).%upcase( &data. )"
                        variable= type= ;
                    stop;

                    end;

                end;  * select(type) processing;


            text= "    do i = 1 to 2; ";
            call execute( text );

            text= "        set &libname..&data.(keep= " || variable ||
                " where=( " || variable || " gt " || MissVal || " )) ; "
                ;
            call execute( text );

            text= "        retain " || variable || "; " ;
            call execute( text );

            text= "        if " || variable || " gt " || MissVal || "  then leave; " ;
            call execute( text );

            text= "        end; ";
            call execute( text );



            * Now we have a bunch of variables in one record.  Put to multiple records for each
            *  of the 'singles' vars (sounds like a bad place to meet a desperate programmer);
            if last then
                do;

                text= "    end;  * if _n_ = 1 procesing;" ;
                call execute( text );

                do _i2 = 1 to _nvar;

                    set work._vvOne;

                    text= "    variable = '" || variable || "';" ;
                    call execute( text );

                    text= "    value = " || Alignmnt{ _i2 } ||
                        " ( trim( put( " || variable || ", " || PutFmt{ _i2 } || " )));"
                        ;
                    call execute( text );

                    text= "    output;";
                    call execute( text );


                    if _i2 eq ( _nvar ) then
                        do;

                        text= "    stop; " ;
                        call execute( text );

                        text= "    run; " ;
                        call execute( text );

                        end;  * if (_i2 ) processing;
                    end;  * do ( _i2 ) processing;
                end;  * if (last) processing;
            run;

        %end;  * doOnes processing step;
        %* ----------------;




    %* ----------------;
    %if &doOnes. eq true %then
        %do;


        *----------------
        * Update the Ones dataset
        ;

        * Sort.  Nodupkey is a 'just in case';

        proc sort data= work._vvOneS1;
            by variable;
        run;


        proc sort data= work._vvOneV1  nodupkey;
            by variable value;
        run;



        data work._vvOne(
            compress= no
            label= 'VV - Ones master w/ Nulls'
            )
            ;

            merge
                work._vvOne (
                    drop= nulls
                    in= o
                )
                work._vvOneS1 (in = n)
                work._vvOneV1 (in = v)
                ;

            by variable;
            if n;

            * number 1 and 0 stuff;
            if nulls eq 0 then
                do;
                _n0 + 1;
                end;

            else
                do;
                _n1 + 1;
                end;
        run;


        * Add labels;
        proc datasets lib= work nolist;
            modify _vvOne;
            label
                nulls = '# of Missing Values'
                value = 'Data Value'
                ;

        run;
        quit;

        %end;  * doOnes processing step;
        %* ----------------;




    %* ----------------;
    %if &doOnes. eq true %then
        %do;

        *---------------
        * Update the Master dataset.  This is a majorly backass way to do things.
        * ...populate:
        *    - nulls   -- null count (global) from _vvOneS1
        *    - n_One1  -- One w/o nulls count (global)
        *    - n_One0  -- One w/ nulls count (global)
        *
        ;

        * Merge with Ones SQL for nulls -- only need 'variable' (for merge),
        * 'nulls', and 'values' (for n_One1 and n_One0 processing)
        ;

        data work._vv1M(label=  "VV - Master - %upcase( &libname. ).%upcase( &data. )" );

            merge
                work._vv1M(
                in= m
                )

                work._vvOneS1(
                in= s
                )
                ;

            by variable;
            if m;

            run;


        *--------
        * Generate totals for n_One1 and n_One0 to carry with all records
        ;
        data work._vv1M(
            label= "VV - Master - %upcase( &libname. ).%upcase( &data. )"
            )
            ;

            set work._vv1M(
                drop=
                    n_One1
                    n_One0
                )
                ;

            retain
                n_One1
                n_One0
                    0
                ;

            drop point;


            if values eq 1 then
                do;

                * Count single value guys with and without nulls;
                if nulls eq 0 then
                    do;

                    * with nulls;
                    n_One1 + 1;
                    end;

                else
                    do;

                    * without nulls;
                    n_One0 + 1;
                    end;
                end;  * if (values) processing;


            * Point processing to generate totals to carry with all records of dataset
            * ...all we need to keep is 'values' -- we just generated 'nulls'
            * ...on last, re re-read the input dataset, keeping, say, variable, and;
            * create the output dataset
            ;

            if _n_ eq _vars then
                do;

                do point= 1 to _vars;

                    set _vv1M(
                        drop=
                            n_One1
                            n_One0
                        )
                        nobs= _vars
                        ;

                    output;
                    end;  * point= processing;
                end;  * last processing;


            label
                nulls = "# Missing obs for Var"
                n_One0= "# of Unique Vars w/missing"
                n_One1= "# of Unique non-Missing Vars"
                ;
        run;


        %end;  * doOnes processing;
        %* ----------------;

    *----------------------------------------
    *
    *        End of 'Ones' variable processing
    *
    *----------------------------------------
    ;




    *--------------------------------------------------------------------------------
    *
    *  Start of reports
    *
    *--------------------------------------------------------------------------------
    * Initial reports:
    * ...all variables + dataset summary
    *    breakout by
    *       character
    *       numeric
    *          - numeric
    *          - data + time
    *
    *       unevaluated
    *       single (nonmissing) value
    ;


    data _null_;

        file print
            linesleft= remain
            ;

        set _vv1M;

        * column variables;
        y_n_  = 1;
        yVar  = y_n_  + 7;
        yVal  = yVar  + 40;
        yLabl = yVal  + 20;
        yType = yLabl + 40;
        yLen  = yType + 8;
        yFmt  = yLen  + 8;



        if _n_ eq 1 then
            do;


            put
                "Dataset summary for %upcase( &libname. ).%upcase( &data. )"
                //
                @5  "Observations: " @%eval( 5 + 12 + 3 + &wnObs. ) nObs comma&wnObs..-r /
                @5  "Variables:    " @%eval( 5 + 12 + 3 + &wnObs. ) nVar comma&wnObs..-r /
                @5  40*'-' /
                /
                @5  "Variables by type:" /
                @5  19*'-' /
                @8  "Numeric:   " @20 nNumA comma5. /
                @10 "Quantity:  " @25 nNum comma5./
                @10 "Date/Time: " @25 nDate comma5./
                /
                @8  "Character: " @20 nChar comma5./
                @5  25*'=' /
                /
                /
                @5  "Missing or uniformly evaluated variables:" /
                @5  42*'-' /
                @7  "- missing for all observations: "    @40 nNul comma5. /
                @7  "- uniformly evaluated -- all: "      @40 n_One comma5. /
                @11  "with one or more missing values:"  @45 n_One0 comma5. /
                @11  "with no missing values:"           @45 n_One1 comma5. /
                @5  50*'=' /

                ;

            link title;

            end;


         put
            @y_n_  _n_          3.0
            @yVar  variable     $32.
            @yVal  values  comma12.
            @yLabl label       $40.
            @yType type         $4.
            @yLen  length        3.
            @yFmt  format      $10.
            ;

        if remain le 3 then
            do;
            put _page_;
            goto title;
            end;

        return;

        title:
            put /
            @y_n_  " #"
            @yVar  "Variable"
            @yVal  "Unique Values"
            @yLabl "Label"
            @yType "Type"
            @yLen  "Length"
            @yFmt  "Format"
            ;

        put
            @y_n_ "---"
            @yVar "--------"
            @yVal "-------------"
            @yLabl "-----"
            @yType "----"
            @yLen  "------"
            @yFmt  "------"
            /
            ;

        return;

        run;


    *----------------------------------------------------------------------------------------
    *    character...
    *          ...if less than &ValCtOff values, proc freq in frequency order
    ;

    * first get nvar again to do some formatting - width of 'obs' count in report.  Min 3;

    %nvar( libname= &libname, data= &data );

    data _null_;
        nvar= "&nvar";
        Width= length( compress( nvar ));
        Width= max( width, 3 );
        call symput( 'Width', put( Width, best. ));
        run;



    title3 "The following variables are missing or unevaluated for all occurances";

    proc report data= work._vvNul
        nowindows
        headskip
        spacing= 4
        ;

        column    _n variable label nNul;

        define   _n       / analysis width= &width '#' '--';
        define   variable / display 'Variable Name' '--';
        define   label    / display 'Label' '--';
        define   nNul     / analysis noprint;

        run;



    title3 "The following variables are uniformly evaluated with NO missing values";
    title4 "a single non-missing value is present for all observations";

    proc report data= work._vvOne(
        where= ( nulls eq 0 )
        )

        nowindows
        headskip
        spacing= 4
        ;

        column  _n0 variable value label;

        define   _n0     / analysis width= &width '#' '--';
        define variable  / display 'Variable' '--';
        define value     / display 'Value' '--';
        define label     / display 'Label' '--';

        run;



    title3 "The following variables are uniformly evaluated with SOME missing values";

    proc report data= work._vvOne(
        where=( nulls gt 0 )
        )

        nowindows
        headskip
        spacing= 4
        ;

        column  _n1 variable value nulls label;

        define   _n1     / analysis width= &width '#' '--';
        define variable  / display 'Variable' '--';
        define value     / display 'Value' '--';
        define nulls     / display '# of Missing Values' '--';
        define label     / display 'Label' '--';

        run;



    *----------------------------------------------------------------------------------------
    * Character variables -- frequencies
    ;

    title3 "Frequency tabulations of Character variables with <= &ValCtOff. discrete values";
    title4 "non-evaluated variables excluded";

    data _null_;
        file print
            linesleft= remain
            ;

        set _vvch1(where= ( values gt 1 ));


        if _n_ eq 1 then
            link title;

        if remain le 1 then
            do;
            put _page_;
            goto title;
            end;


        put
            @15 _n_ 3.0
            @20 20*'.'
            @19 variable
            @53 label
            @107 values comma6.
            ;

        file log;

        return;

        title:
            put /
                @15 " # "
                @20 "Variables listed:"
                @40 "Label"
                @85 "# of Values"
                /
                @15 3*'-'
                @20 18*'-'
                @40 40*'-'
                @85 15*'-'
                /
                ;
        return;


        run;


    data _null_;

        length text $ 200;
        set _vvch1(where= (values gt 1))
            end= last
            ;

        if _n_ eq 1 then
            do;

            text = "proc freq data= &libname..&data.";
            call execute( text );

            text = "order= &FreqOrdr.";
            call execute( text );

            text = ";";
            call execute( text );

            text = "tables ";
            call execute( text );

            end;

        call execute( variable );

        if last then
            do;

            text = "/ missing nocum ;";
            call execute( text );

            text = "run;";
            call execute( text );

            end;

        run;

    %_vdo_macnam(CHARaFREQ);


    *----------------------------------------------------------------------------------------
    *         ...if more than &ValCtOff values, 10 most frequent, 10 least frequent values;

    title3 "&ExtrmVal. most frequent + &ExtrmVal. least frequent values";
    title4 "For Character variables with > &ValCtOff. discrete values";


    data _null_;
        file print
            linesleft= remain
            ;

        set _vvch2(where= ( values gt 1 ));

        if _n_ eq 1 then
            link title;

        if remain le 1 then
            do;
            put _page_;
            goto title;
            end;


        put
            @15 _n_ 3.0
            @20 20*'.'
            @19 variable
            @53 label
            @88 values comma12.
            ;

        file log;

        return;

        title:
            put /
                @15 " # "
                @20 "Variables listed:"
                @40 "Label"
                @85 "# of Values"
                /
                @15 3*'-'
                @20 18*'-'
                @40 40*'-'
                @85 15*'-'
                /
                ;

        return;

        run;


    * Generate frequency data;
    data _null_;

        length text $ 200;

        set _vvch2(where= ( values gt 1 ));

        * If fewer than &MaxFreq results then use proc freq, else, sql;
        * &MaxFreq is the SAS system maximum number of levels for a value;
        * allowed by Proc Freq, = 32,767 as of 1/18/96 (emperically determined :-);
        * ...Order here MUST be freq -- not parameterized
        ;

        if values le &MaxFreq. then
            do;

            text = "proc freq data= &libname..&data.";
            call execute( text );

            text = "order= freq;";
            call execute( text );

            text = "tables ";
            call execute( text );

            call execute( variable );

            text = "/ missing noprint out= " || variable || "( label= 'VV - Char Extremes Freq - " || variable || "' );";
            call execute( text );

            text = "run;";
            call execute( text );

            end;

        else
            do;

            text = "proc sql noprint; " ;
            call execute( text );

            text = "create table " || variable || " as" ;
                call execute( text );

            text = "select " || variable || " as " || variable || ", count( * ) as count" ;
            call execute( text );

            text = "from &libname..&data." ;
            call execute( text );

            text = "group by " || variable ;
            call execute( text );

            text = " ; " ;
            call execute( text );

            call execute( text );
            text = "quit; " ;

            end;

        run;


    * Sort that stuff;
    data _null_;

        length text $ 200;

        set _vvch2(where= ( values gt 1 ));

        text = "proc sort data= " || variable || " nodupkey; by descending count " || variable || " ; run; ";
        call execute( text );

        run;

    * Print it;
    data _null_;

        length text $ 200;
        retain extreme;
        extreme = "&ExtrmVal.";

        set _vvch2(where= ( values gt 1 ));

        text = "data _null_ ; " ;
        call execute( text );

            text = "set " || variable || " nobs= recs ; " ;
            call execute( text );

            text = "file print;" ;
            call execute( text );

            * Report title and header;
            text = "if _n_ eq 1 then do; " ;
            call execute( text );

            text = "put // @10 ' " || extreme || " most frequent values of " || variable ||
                "   (" || trim( label ) || ") ' / ; " ;
            call execute( text );

            text = "put @20 recs comma9. ' distinct values in total' / ; " ;
            call execute ( text );

            text = "put @11 'Rank' @20 'Value' @ 100 'Frequency' ; ";
            call execute( text );

            text = "put @11 '----' @20 '-----' @ 100 '---------' //; ";
            call execute( text );

            text = "end;";
            call execute( text );


            * Report data;
            text = "if ( _n_ ge 1 and _n_ le &ExtrmVal. ) or ( _n_ le recs and _n_ ge ( recs - &ExtrmVal. )) then do; " ;
            call execute( text );

            text = "put @5 _n_  comma9. @18 " || variable ||  " @99 count comma9.; " ;
            call execute( text );

            text = "end; " ;
            call execute( text );


            text = "if _n_ = %eval(&ExtrmVal. + 1) then " ;  /* rjd */
            call execute( text );

            text = "    put  // @10 ' " || extreme || "  least frequent values' / ;" ;
            call execute( text );

            text= "run; " ;

        * Run Cleanup;

        if &Cleanup. ne 0 then
            do;
            text = "proc delete data= " || variable || " ; ";
            call execute( text );

            text = "run; " ;
            call execute( text );

            end;

        run;

    * where univariate uses to be title3 Univar;

    * end where univariate used to be;


    %_vdo_macnam(NUMaFREQ);

    *----------------------------------------------------------------------------------------
    *            ...less than &ValCtOff values, proc freq;
    title3 "Frequency tabulations of numeric variables with <= &ValCtOff. discrete values";
    title4 "non-evaluated variables excluded";


    data _null_;
        file print
            linesleft= remain
            ;

        set _vvnum1(where= ( values gt 1 ))
            _vvnum2(where= ( values gt 1 ))
       ;

        if _n_ eq 1 then
            link title
            ;

        if remain le 1 then
            do;
            put _page_;
            goto title;
            end;

        put
            @15 _n_ 3.0
            @20 20*'.'
            @19 variable
            @53 label
            @107 values  comma6.
            ;

        file log;

        return;

        title:
            put /
                @15 " # "
                @20 "Variables listed:"
                @40 "Label"
                @85 "# of Values"
                /
                @15 3*'-'
                @20 18*'-'
                @40 40*'-'
                @85 15*'-'
                /
                ;

        return;

        run;

    data _null_;


        length text $200;

        set
            _vvnum1(where= ( values gt 1 ))
            _vvnum2(where= ( values gt 1 ))
            end= last;

        if _n_ eq 1 then
            do;

            text = "proc freq data= &libname..&data.";
            call execute( text );

            text = "order= &FreqOrdr.";
            call execute( text );

            text = ";";
            call execute( text );

        end;

            text = "tables ";
            call execute( text );

            text = variable||"/ missing noprint out= " || variable || "( label= 'VV - Num  Extremes Freq - " || variable || "' );";
            call execute( text );

         if last then do;
            text = "run;quit;";
            call execute( text );
         end;

        run;


    * Sort that stuff;
    data _null_;

        length text $ 200;

        set _vvnum1(where= ( values gt 1 ))
          _vvnum2(where= ( values gt 1 ))
        ;

        text = "proc sort data= " || variable || " nodupkey; by descending count " || variable || " ; run; ";
        call execute( text );

        run;



    * Print it;
    data _null_;

        length text $ 200;
        retain extreme;
        extreme = "&ExtrmVal.";

        set _vvnum1(where= ( values gt 1 ))
            _vvnum2(where= ( values gt 1 ))
       ;

        text = "data _null_ ; " ;
        call execute( text );

            text = "set " || variable || " nobs= recs ; " ;
            call execute( text );

            text = "file print;" ;
            call execute( text );

            * Report title and header;
            text = "if _n_ eq 1 then do; " ;
            call execute( text );

            text = "put // @10 ' " || extreme || " most frequent values of " || variable ||
                "   (" || trim( label ) || ") ' / ; " ;
            call execute( text );

            text = "put @20 recs comma9. ' distinct values in total' / ; " ;
            call execute ( text );

            text = "put @11 'Rank' @20 'Value' @ 100 'Frequency' ; ";
            call execute( text );

            text = "put @11 '----' @20 '-----' @ 100 '---------' //; ";
            call execute( text );

            text = "end;";
            call execute( text );


            * Report data;
            text = "if ( _n_ ge 1 and _n_ le &ExtrmVal. ) or ( _n_ le recs and _n_ ge ( recs - &ExtrmVal. )) then do; " ;
            call execute( text );

            text = "put @5 _n_ comma9. @18 " || variable ||  " @99 count comma9.; " ;
            call execute( text );

            text = "end; " ;
            call execute( text );


            text = "if _n_ = %eval(&ExtrmVal. + 1) then " ;  /* rjd */
            call execute( text );

            text = "    put // @10 ' " || extreme || "  least frequent values' / ;" ;
            call execute( text );

            text= "run; " ;

        * Run Cleanup;

        if upcase("&Cleanup.") eq upcase("true") then
            do;
            text = "proc delete data= " || variable || " ; ";
            call execute( text );

            text = "run; " ;
            call execute( text );

            end;

        run;

    %_vdo_macnam(MEANS);

    data _null_;
        set
            _vvnum1(where= ( values gt 1 ))
            _vvnum2(where= ( values gt 1 ))   /* rjd 2/1/2015 */
            _vvDt(where= ( values gt 1 ))
            end= last
            ;

        by variable;

        if _n_ eq 1 then
            do;

            text = "proc means data= &libname..&data. n nmiss min max mean median std sum";
            call execute( text );

            text = "; ";
            call execute( text );

            text = "var ";
            call execute( text );

            end;

        call execute( variable );

        if last then
            do;

            text = ";";
            call execute( text );

            text = "run;";
            call execute( text );

            end;

        run;


    %_vdo_macnam(DATETIME);


    *----------------------------------------------------------------------------------------
    *    numeric
    *    ...if not a date....
    *            ...all - proc univariate;



    title3 "Date/Time/Datetime Variables";
    title4 "non-evaluated variables excluded";

    data _null_;
        file print
            linesleft= remain
            ;

        set _vvDt(where= ( values gt 0 ));

        if _n_ eq 1 then
            link title;

        if remain le 1 then
            do;
            put _page_;
            goto title;
            end;

        put
            @15 _n_ 3.0
            @20 20*'.'
            @19 variable
            @53 label
            @107 values
            ;

        file log;

        return;

        title:
            put /
                @15 " # "
                @20 "Variables listed:"
                @40 "Label"
                @85 "# of Values"
                /
                @15 3*'-'
                @20 18*'-'
                @40 40*'-'
                @85 15*'-'
                /
                ;

        return;

        run;



    *----------------------------------------------------------------------------------------
    * 2/14/96 -- Date/Time/Datetime processing --
    *    Changing this to provide Univariate data and maybe a plot
    *
    * ...General structure:
    *    Get Univariate statistics for each date/time/datetime variable
    *    Format the output accordingly for date, time, or datetime
    *    Data _null_ to output Univariate statistics and percentile values.
    ;

    data _null_;

        length
            text     $ 200
            DispFmt  $ 20
            _format  $ 20
            timetype $ 8
            ;

        set _vvDt(where= ( values gt 0 ));

        * Assign display format according to assigned format;
        *    - DispFmt --  display format
        *    - DispUnt -- unit text (days, mins, secs)
        *    - DispAgr -- aggregate text (years, days, hours)
        *    - DispFact-- conversion factor from display unit to aggregate unit
        ;

            * ...temp value for testing;
        _format = upcase( compress( format, '0123456789. ' ));

        * dateFmt dttimFmt timeFmt;

        select( _format );

            when( &DateFmt. )
                do;

                TimeType= "date";
                DispFmt= "mmddyy10.";
                DispUnt= "days";
                DispAgr= "years";
                DispFact= 365.25;

                end;  * date formats processing;

            when( &dttimFmt. )
                do;

                TimeType= "datetime";
                DispFmt= "datetime7.";
                DispUnt= "secs";
                DispAgr= "days";
                DispFact= 60 * 60 * 24 ;  * Seconds by minutes by hours per day;

                end;  * datetime formats processing;

            when( &timeFmt. )
                do;

                TimeType= "time";
                DispFmt= "time5.";
                DispUnt= "secs";
                DispAgr= "hours";
                DispFact= 60 * 60 ;   * Seconds per minute per hour;

                end;  * time formats processing;

            otherwise
                do;

                error "ERROR: (VV macro) bad format value encountered, contact developer -- Karsten Self";
                error "ERROR: (VV macro) email to:  kmself@ix.netcom.com";
                error "ERROR: (VV macro) Date/Time/Datetime variable processing" variable= format= ;
                stop;

                end;  * Otherwise (format) processing;


            end; * Select processing;




        * Generate proc univariate for each variable (do this for all _n_);

        text= "proc univariate data= &libname..&data. noprint; " ;
        call execute( text );

        text= "   var " || variable || " ; " ;
        call execute( text );

        text= "    output " ;
        call execute( text );

        text= "        out= " || variable || "( label= 'VV - Date Univariate - " || variable || "' ) " ;
        call execute( text );

        text= "        n        = n ";
        call execute( text );

        text= "        nmiss        = nmiss ";
        call execute( text );

        text= "        mean        = mean ";
        call execute( text );

        text= "        std        = std ";
        call execute( text );

        text= "        max        = max ";
        call execute( text );

        text= "        min        = min ";
        call execute( text );

        text= "        range        = range ";
        call execute( text );

        text= "        qrange        = qrange ";
        call execute( text );

        text= "        p1        = p1 ";
        call execute( text );

        text= "        p5        = p5 ";
        call execute( text );

        text= "        p10        = p10 ";
        call execute( text );

        text= "        q1        = q1 ";
        call execute( text );

        text= "        median        = median ";
        call execute( text );

        text= "        q3        = q3 ";
        call execute( text );

        text= "        p90        = p90 ";
        call execute( text );

        text= "        p95        = p95 ";
        call execute( text );

        text= "        p99        = p99 ";
        call execute( text );

        text= "        ; " ;
        call execute( text );



        text= "   run; " ;
        call execute( text );



        * Generate proc datasets to modify formats -- this is done according to
        *  date/time/datetime format;

        text= "proc datasets lib= work nolist; " ;
        call execute( text );

        text= "    modify " || variable || " ; " ;
        call execute( text );

        text= "    format " ;
        call execute( text );

        text= "        n nmiss     comma10. " ;
        call execute( text );



        * ...the next bit depends on the kind of data;

        text= "        mean min max median q1 q3 p99 p95 p90 p10 p5 p1 " || DispFmt ;
        call execute( text );

        text= "        range qrange comma12. " ;
        call execute( text );

        text= "        std comma12.2 " ;
        call execute( text );

        text= "        ; " ;
        call execute( text );


        text= "    run; " ;
        call execute( text );

        text= "    quit; " ;
        call execute( text );




        * Generate data _null_ for report;
        * ...setups and initalizations;
        *    specify n= pagesize to allow relocation over page;


        text= "title3 'Date/Time/Datetime Variables';" ;
        call execute( text );

        text= "title4 'non-evaluated variables excluded'; ";
        call execute( text );



        text= "data _null_; ";
        call execute( text );

        text= "    set " || variable || "; " ;
        call execute( text );

        text= "    file print  n= pagesize; " ;
        call execute( text );


        text= "    format dRange dQrange   comma12.2  dStd comma12.2; " ;
        call execute( text );


        text= "    dStd  = std  / " || put( DispFact, 8. ) || " ; " ;
        call execute( text );

        text= "    dRange  = range  / " || put( DispFact, 8. ) || " ; " ;
        call execute( text );

        text= "    dQRange = qrange / " || put( DispFact, 8. ) || " ; " ;
        call execute( text );


        * Define postional column and row variables -- as text, since we are just writing them out;
        yTitle=  '4';    * Vertical position of first row of titles;
        yData=   '9';   * Vertical position of first row data output;

        xTitle=  '5';   * Horizontal position titles;

        xTtl1=  '5';    * Position first title column;


        * Following is depending on date, time, or datetime;

        select( TimeType );

            when( 'date' )
                do;

                xVal1a=  '10';   * Position first values column 'a' (n, nmiss);
                xVal1b=  '29';   * Position first values column 'b' (  );
                xVal1c=  '26';   * Position first values column 'c' (mean);
                xVal1d=  '27';   * Position first values column 'd' (std, decimal range);
                xVal1e=  '24';   * Position first values column 'e' (range);

                xTtl2=  '50';   * Position second title column;
                xVal2=  '60';   * Position second values column;

                end;  * date;

            when( 'time' )
                do;

                xVal1a=  '10';   * Position first values column 'a' (n, nmiss);
                xVal1b=  '29';   * Position first values column 'b' (  );
                xVal1c=  '30';   * Position first values column 'c' (mean);
                xVal1d=  '26';   * Position first values column 'd' (std, decimal range);
                xVal1e=  '23';   * Position first values column 'e' (range);

                xTtl2=  '50';   * Position second title column;
                xVal2=  '60';   * Position second values column;

                end;  * time;

            when( 'datetime' )
                do;

                xVal1a=  '10';   * Position first values column 'a' (n, nmiss);
                xVal1b=  '26';   * Position first values column 'b' (  );
                xVal1c=  '28';   * Position first values column 'c' (mean);
                xVal1d=  '26';   * Position first values column 'd' (std, decimal range);
                xVal1e=  '23';   * Position first values column 'e' (range);

                xTtl2=  '50';   * Position second title column;
                xVal2=  '60';   * Position second values column;

                end;  * datetime;

            otherwise
                do;
                error "ERROR: (VV macro) %upcase( &libname. ).%upcase( &data. )  invalid time time "
                    TimeType= format= variable= ;
                end;

            end;  * select( TimeType );


        * ...report put statements;

        *    titles;

        text= "    if _n_ eq 1 then";
        call execute( text );

        text= "        do; ";
        call execute( text );

        text= "        put ";
        call execute( text );

        text= "           #" || yTitle || " @" || xTitle ;
        call execute( text );

        text= "         'Univariate distribution of " || trim( variable ) || " ( " || trim( label ) || " ) values' /";
        call execute( text );

        text= "           @"|| xTitle || " 'Distinct values: " || trim( left( put( values, comma10.-l))) || "' /" ;
        call execute( text );

        text= "           @"|| xTitle || " 'Format in dataset:  " || trim( left( format ) ) || "' /";
        call execute( text );

        text= "           @"|| xTitle || " 65*'-' /";
        call execute( text );

        text= "           ;" ;
        call execute( text );

        text= "        end;" ;
        call execute( text );




        *    data;

        *    ...title 1;

        text= "    put // ";
        call execute( text );

        text= "    #" || yData || " @" || xTtl1 || " 'n'      / ";
        call execute( text );

        text= "                     @" || xTtl1 || " 'nmiss'  / ";
        call execute( text );

        text= "    / ";
        call execute( text );

        text= "                     @" || xTtl1 || " 'mean'   / ";
        call execute( text );

        text= "                     @" || xTtl1 || " 'std - " || DispUnt || "'    / ";
        call execute( text );

        text= "                     @" || xTtl1 || " 'std - " || DispAgr || "'    / ";
        call execute( text );

        text= "    / ";
        call execute( text );

        text= "                     @" || xTtl1 || " 'range - " || DispUnt || "' / " ;
        call execute( text );

        text= "                     @" || xTtl1 || " 'range - " || DispAgr || "' / " ;
        call execute( text );

        text= "    / ";
        call execute( text );

        text= "                     @" || xTtl1 || " 'Q1-Q3 - " || DispUnt || "' / ";
        call execute( text );

        text= "                     @" || xTtl1 || " 'Q1-Q3 - " || DispAgr || "' / ";
        call execute( text );



        *    ...value 1;

        text= "           #" || yData || " @" || xVal1a || " n comma10.-r     / " ;
        call execute( text );

        text= "                            @" || xVal1a || " nmiss comma10.-r / " ;
        call execute( text );

        text= "    / ";
        call execute( text );

        text= "                            @" || xVal1c || " mean " || DispFmt ||"-r  / " ;
        call execute( text );

        text= "                            @" || xVal1d || " std  comma12.2-r  / " ;
        call execute( text );

        text= "                            @" || xVal1d || " dStd  comma12.2-r  / " ;
        call execute( text );

        text= "    / ";
        call execute( text );

        text= "                            @" || xVal1e || " Range comma12.-r / " ;
        call execute( text );

        text= "                            @" || xVal1d || " dRange comma12.2-r / " ;
        call execute( text );

        text= "    / ";
        call execute( text );

        text= "                            @" || xVal1e || " QRange comma12.-r / " ;
        call execute( text );

        text= "                            @" || xVal1d || " dQRange comma12.2-r / " ;
        call execute( text );


        *    ...title 2;

        text= "           #" || yData || " @" || xTtl2 || " 'min'    / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'P1'     / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'P5'     / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'P10'    / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'Q1'     / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'median' / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'Q3'     / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'P90'    / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'P95'    / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'P99'    / " ;
        call execute( text );

        text= "                            @" || xTtl2 || " 'max'    " ;
        call execute( text );



        *    ...value 2;

        text= "           #" || yData || " @" || xVal2 || " min    / " ;
        call execute( text );

        text= "                            @" || xVal2 || " P1     / " ;
        call execute( text );

        text= "                            @" || xVal2 || " P5     / " ;
        call execute( text );

        text= "                            @" || xVal2 || " P10    / " ;
        call execute( text );

        text= "                            @" || xVal2 || " Q1     / " ;
        call execute( text );

        text= "                            @" || xVal2 || " median / " ;
        call execute( text );

        text= "                            @" || xVal2 || " Q3     / " ;
        call execute( text );

        text= "                            @" || xVal2 || " P90    / " ;
        call execute( text );

        text= "                            @" || xVal2 || " P95    / " ;
        call execute( text );

        text= "                            @" || xVal2 || " P99    / " ;
        call execute( text );

        text= "                            @" || xVal2 || " max    " ;
        call execute( text );



        *    end of put statement;

        text= ";";
        call execute( text );



        * Close out data step;

        text= "run;";
        call execute( text );

 run;

 %_vdo_macnam(UNIVAR);

 %if &univar ne 0 %then %do;

    * moved univariate to here;

    *----------------------------------------------------------------------------------------
    *    numeric
    *    ...if not a date....
    *            ...all - proc univariate;

    title3 "Univariate tabulations non-date/time/datetime Numeric variables";
    title4 "includes only variables with TWO or more values";


    * order by variable utilizing index;

    data _null_;
        file print
            linesleft= remain
            ;

        set
            _vvnum1(where= ( values gt 1 ))
            _vvnum2(where= ( values gt 1 ))
            ;

        by variable;

        if _n_ eq 1 then
            link title;

        if remain le 1 then
            do;
            put _page_;
            goto title;
            end;


        put
            @15 _n_ 3.0
            @20 20*'.'
            @19 variable
            @53 label
            ;

        file log;

        return;

        title:
            put /
                @15 " # "
                @20 "Variables listed:"
                /
                @15 3*'-'
                @20 18*'-'
                /
                ;

        return;

        run;


       data _null_;

        set
            _vvnum1(where= ( values gt 1 ))
            _vvnum2(where= ( values gt 1 ))  /* rjd 2/1/2015 */
            end= last
            ;

        by variable;

        if _n_ eq 1 then

          do;

            /*text = "ods exclude all;ods listing;proc univariate data= &libname..&data."; */
            text = "proc univariate data= &libname..&data. plots";

            call execute( text );

            if &UniPlot. ne 0 then
                do;

                text= "plots";
                call execute ( text );

                end;  * UniPlot processing;


            text = "; ";
            call execute( text );

            text = "var ";
            call execute( text );

         end;

        call execute( variable );

        if last then
            do;

            text = ";";
            call execute( text );

            text = "run;";
            call execute( text );

            end;
        run;

        * end univariate;


        /* Cleanup, if selected

        data _null_;

          set _vvnum1
              _vvmum2;   * rjd 2/1/2015 ;

          if upcase("&Cleanup.") eq upcase("true") then
            do;

            text= "proc delete data= work." || variable || " ; " ;
            call execute( text );

            text= "run; " ;
            call execute( text );

            end;  * Cleanup processing;

        * We are done here;

        run;
        */

   %end;

   %exit:

%mend _vdo_basic;


*   *  *****   ***    ***   ****     *    *****
** **    *    *   *  *   *  *   *   * *     *
* * *    *     *      *     *   *  *   *    *
*   *    *      *      *    ****   *****    *
*   *    *       *      *   *      *   *    *
*   *    *    *   *  *   *  *      *   *    *
*   *  *****   ***    ***   *      *   *    *

#! MISSPAT ;

%macro qcmprltb(text);
%*********************************************************************;
%*                                                                   *;
%*  MACRO: QCMPRLTB                                                  *;
%*                                                                   *;
%*  USAGE: 1) %qcmprltb(argument)                                    *;
%*                                                                   *;
%*  DESCRIPTION:                                                     *;
%*    form with multiple blanks compressed to single blanks but with *;
%*    with leading and trailing blanks retained, unlike %qcmpres.    *;
%*                                                                   *;
%*    Eg. %let macvar=%qcmprltb(&argtext)                            *;
%*                                                                   *;
%*  NOTES:                                                           *;
%*    The %QLEFT macro in the autocall library is used in this macro.*;
%*                                                                   *;
%*********************************************************************;
%local i;
%let i=%index(&text,%str(  ));
%do %while(&i^=0);
  %let text=%qsubstr(&text,1,&i)%qleft(%qsubstr(&text,&i+1));
  %let i=%index(&text,%str(  ));
%end;
&text
%mend;

%macro qblankta(text);
%*********************************************************************;
%*                                                                   *;
%*  MACRO: QBLANKTA                                                  *;
%*                                                                   *;
%*  USAGE: 1) %qblankta(argument)                                    *;
%*                                                                   *;
%*  DESCRIPTION:                                                     *;
%*    REPLACE BLANKS BY ASTERISKS:  MODELED AFTER QCMPRES FOUND IN   *;
%*    !SASROOT\core\sasmacro.                                        *;
%*                                                                   *;
%*    Eg. %let macvar=%qblankta(&argtext)                            *;
%*                                                                   *;
%*  NOTES:                                                           *;
%*                                                                   *;
%*********************************************************************;
%local i;
%let i=%index(&text,%str( ));
%do %while(&i^=0);
  %IF &I GT 1 %THEN
    %if &i lt %length(&text) %then
    %let text=%qsubstr(&text,1,&i-1)%str(*)%qsubstr(&text,&i+1);
    %else %let text=%qsubstr(&text,1,&i-1)%str(*);
  %ELSE %let text=%str(*)%qsubstr(&text,&i+1);
  %let i=%index(&text,%str( ));
%end;
&text
%mend;

%macro qblanktc(text) ;
%*********************************************************************;
%*                                                                   *;
%*  MACRO: QBLANKTC                                                  *;
%*                                                                   *;
%*  USAGE: 1) %qblanktc(argument)                                    *;
%*                                                                   *;
%*  DESCRIPTION:                                                     *;
%*    REPLACE BLANKS IN ARGUMENT BY COMMAS.                          *;
%*                                                                   *;
%*    Eg. %let macvar=%qblanktc(&argtext)                            *;
%*                                                                   *;
%*  NOTES:                                                           *;
%*    USES %QSYSFUNC AND TRANSLATE FUNCTIONS TO ACCOMPLISH THE       *;
%*    OBJECTIVE.                                                     *;
%*                                                                   *;
%*********************************************************************;

  %if &text ne %then %qsysfunc(translate(&text,%str(,),%str( ))) ;
%mend  qblanktc ;

%macro qlastvar(text);
%*********************************************************************;
%*                                                                   *;
%*  MACRO: QLASTVAR                                                  *;
%*                                                                   *;
%*  USAGE: 1) %qlastvar(argument)                                    *;
%*                                                                   *;
%*  DESCRIPTION:                                                     *;
%*    Finds the last variable name in the argument, which consists   *;
%*    of variable names delimited by blanks.  The name is returned   *;
%*    without leading or trailing blanks.                            *;
%*                                                                   *;
%*    Eg. %let macvar=%qlastvar(&argtext)                            *;
%*                                                                   *;
%*  NOTES:                                                           *;
%*    The %QCMPRES macro in the autocall library is used in this     *;
%*    macro.                                                         *;
%*                                                                   *;
%*********************************************************************;
%local i;
%let text=%qcmpres(&text);
%let i=%index(&text,%str( ));
%do %while(&i^=0);
  %let text=%qsubstr(&text,&i+1);
  %let i=%index(&text,%str( ));
%end;
&text
%mend;
*END OF UTILITY MACROS;


%macro _vdo_misspat(
      dat=&libname..&data,        /* name of input dataset */
      varin=_ALL_,                /* allow selecting VAR=_NUMERIC_ or _CHARACTER_ */
      sortby=descending percent,
      BYE=,                        /* list of blank-separated BY variables         */
      COLLAPSE=NO,
      out=_misspat                /* name of output dataset */
      );

    %local data var by;

    %let data=&dat;
    %let var=&varin;
    %let by=&bye;

    %put &=data;


    * MISS_PAT.SAS VERSION 1.0;
    * THIS MACRO WILL RUN A MISSING PATTERN ANALYSIS ON THE DATA SET &data;
    * 'BY' VARIABLES CAN BE USED BY SPECIFYING &BY;
    * If 'by' variables are specified and collapse=YES, then statistics
      for missing patterns collapsed accross all 'by' variables are
      also printed;
    * All VAR= variables except the 'by' variable are used in the analysis;

    * TITLES CREATED BY THIS MACRO ARE PUT IN TITLE3, and TITLE4 LINES;

    %LOCAL SORTIN; * TO CONTROL DATA SET USE IN SORT BELOW, DEPENDING ON CIRCUMSTANCE;

    %* Guarantee at least one trailing blanks at end of &by;
    %* so as to be able to search the &by list for unique individual
       substrings that do not contain blanks (i.e., search for the
       individual 'by' variables;
    %* Guarantee no more than one blank seperating variable names in &by;
    %* so as to replace the blanks with asterisks for use in proc freq;
    %* also guarantee that characters are upper case for comparisons later;

    %LET BY=%UPCASE(%CMPRES(&BY))%STR( );

    %PUT BY=&BY***;

    * Determine the variables in the data set &data using proc contents ;
    proc contents data=&data noprint
      out=cont_ds(
        keep=name varnum label type
        rename=(name=variable)
        label="Contents of &data: Selected Variables"
         );
    run;

    proc print;

    * Set up formats for the variable 'type' and for missingness;
    proc format;
      value typef
        1='NUMERIC'
        2='CHARACTER'
            ;
      value miss
        0='X'
        1='.'
            ;
    run;

    %let var=%upcase(&var);

    TITLE3 "Variable Name Aliases for &data for Use in Mising Value Pattern Analysis";
    * Note: the proc contents above sorted the data set by 'VARIABLE';
    * If &VAR^=_ALL_, the variable aliases will be inconveniently labeled;
    data aliases;
      attrib alias length=$6;
      format type typef.;
      set cont_ds;
      alias='V'||left(put(varnum,5.0));
            *-- Subset by TYPE or variable name;
            %if &var=_ALL_       %then %str(;);
      %else %if &var=_NUMERIC_   %then %do;  if type=1; %end;
      %else %if &var=_CHARACTER_ %then %do;  if type=2; %end;
      %else %do;  if index("&var", upcase(trim(variable)));     %end;

    run;
    TITLE4 "Sorted by VARIABLE";
    proc print data=aliases;
          id variable;
          var alias label type;
    run;

    TITLE4 "Sorted by ALIAS";
    proc sort data=aliases;
    * note: sorting by alias leads to trouble when greater than 9 variables;
      by varnum;
    run;
    proc print data=aliases;
          id alias;
          var variable label type;
    run;

    * Create macro variables for 'rename' statement, etc. later;
    * exclude variables in the &by list;
    * create the macro variables in order sorted by varnum;
    * also create a list of macro variables from the &by list;
    %local aliases variables;
    data _null_;
      retain  bylist "&by"; * previously added a blank at the end to be
                              able to delimit a string by a trailing blank;
      set aliases end=eof;
      retain aliases variables;
      length aliases variables $200;
      * Seperate processing if variable is in the &by list;
       if not index(bylist,trim(upcase(variable))||' ')
      then do;
        nanalyze+1;
          aliases = trim(aliases) || ' ' || trim(alias);
          variables = trim(variables) || ' ' || trim(variable);
        put 'Outputting macro variables for variable ' NANALYZE '(' variable ') -> ' alias;
          %* &A1, &A2, etc. will contain the alias names;
        call symput('A'||left(put(nanalyze,5.0)),trim(alias));
          %* &VAR1, &VAR2, etc. will contain the variable names;
        call symput ('VAR'||left(put(nanalyze,5.0)),variable);
        %* &T1, &T2, etc. will contain the variable type;
        call symput ('T'||left(put(nanalyze,5.0)),type);
      end;
      else do;
        nbyvars+1;
          put 'Outputting macro variable for BY variable ' NBYVARS '(' variable ')';
        CALL SYMPUT ('BYVAR'||LEFT(PUT(NBYVARS,5.0)),VARIABLE);
      end;
      if eof then do;
        call symput('nanalyze',nanalyze);
          call symput('nbyvars',nbyvars);
          call symput('aliases',aliases);
          call symput('variables',variables);
      end;
    run;

    %put aliases = &aliases;
    %put variables = &variables;


    * Create a data set 'shortnam' with aliases substituted for variable
      names, etc.;
    DATA SHORTNAM;
    * SET UP THE V1, ETC. AS ONE BYTE CHARACTER VARIABLES;
      length &aliases $1;

      * Set up mispat as character of length &nanalyze;
      length mispat $ &nanalyze;
      set &data;
      ;
      * CREATE THE V1_MISS, ETC. VARIABLES, DEPENDING ON VARIABLE TYPE;
      %DO I=1 %TO &NANALYZE;
         ;
         %IF &&T&I = 1 %THEN
             %STR(* NUMERIC VARIABLE;)
         %STR(IF &&VAR&I LE .Z THEN &&A&I='.'; ELSE &&A&I='X';);
             %ELSE
         %STR(* CHARACTER VARIABLE;)
             %STR(IF &&VAR&I EQ ' ' THEN &&A&I='.'; ELSE &&A&I='X';);

             * ITERATIVELY CONSTRUCT MISPAT;
         %IF &I EQ 1 %THEN %STR(MISPAT=&&A&I;);
             %ELSE %STR(MISPAT=TRIM(MISPAT)||&&A&I;);
      %END;
      ;
      * TRANSLATE MISPAT TO A BINARY STRING;
      MISPAT=TRANSLATE(MISPAT,'01','X.');
      ;
      * DROP UNNEEDED DATA;
      DROP &variables;
      /*
      %DO I=1 %TO &NANALYZE;
           &&VAR&I
      %END;
      */
      ;
    RUN;
    *proc print data=shortnam(obs=20);


    PROC FREQ DATA=SHORTNAM;
      TABLE MISPAT%qblankta(%qtrim(%str( )%qcmpres(&by)))/OUT=BYMISPAT NOPRINT MISSING;
    RUN;

    * Add the group number to the observations;
    data bymispa2;
      set bymispat;
      by mispat ;
      if first.mispat then Group+1;
      rename count=Freq;
    run;


    * Print the missing patterns if by groups not specified via &by, otherwise
      print the ungrouped missing patterns if requested via &collapse;
    %IF (%QTRIM(&BY) EQ ) OR ((%QTRIM(&BY) NE ) AND %UPCASE(&COLLAPSE) EQ YES)
     %THEN %DO;

       %IF %QTRIM(&BY) NE %THEN %DO;

        * Summarize collapsing on &by variables ;
        proc freq data=bymispa2;
              table mispat/out=bymispa3 noprint missing;
              weight freq;
        run;

        * Now read the group number to the observations;
        data bymispa4;
          set bymispa3;
          by mispat ;
          if first.mispat then Group+1;
              rename count=Freq;
        run;

            %LET SORTIN=BYMISPA4;
       %END;
       %ELSE %LET SORTIN=BYMISPA2;


        * Results to be printed without by variables;
        * Translate mispat back into v1, v2, etc.;
          *-- Use binary (0/1) variables rather than character and formats for printing as X or .;
          *-- Provide variable name as label for Vi in output dataset;
          *-- Delete OBS variable;
        DATA &out;
          set &sortin;
    *      obs+1;
          drop mispat;
    *        misbin = mispat;  *-- save binary pattern (for testing);

                %do i=1 %to &nanalyze;
                      &&A&i = (substr(mispat,&i,1) = '1');
                      format &&A&i miss.;
                      label  &&A&i = "&&var&i";
                      %end;
          nmiss = sum(of &aliases);      *-- number of missing variables;

          LABEL Group='Group'
                Freq='Freq'
                Percent='Percent'
                      nmiss = 'missing';
          FORMAT PERCENT 5.1;
    *      LABEL OBS='Obs';
        RUN;

        * Next sort the results by percent descending;
        proc sort data=&out;
              by &sortby;
            run;
        PROC PRINT DATA=&out /* LABEL */;
        TITLE4 "Missing data patterns: sorted by &sortby";
          VAR &aliases
             group freq percent;
        RUN;
    %END;

    * If appropriate do computations then print grouped (by &by) missing
      patterns ;
    %IF &BY NE %STR( ) %THEN %DO;
          proc sql;
          create table work.groupct as
          select mispat %qblanktc(%qtrim(%str( )%qcmpres(&by))),freq,group,
                 sum(freq) as bytot,
                 100*freq/sum(freq) as percent
                   from bymispa2
                   group by %qblanktc(%qcmpres(&by))
                   order by %qblanktc(%qcmpres(&by)),percent desc,group
              ;
          quit;

    * TRANSLATE MISPAT BACK INTO V1, V2, ETC.;
    DATA &out;
    /*
    * SET UP THE V1, ETC. AS ONE BYTE CHARACTER VARIABLES;
      LENGTH
      %DO I=1 %TO &NANALYZE;
          &&A&I
      %END;
      $ 1;
    */

      SET GROUPCT(DROP=BYTOT);
     %IF &BY NE %STR( ) %THEN %STR(
      BY &BY ;
      IF FIRST.%QLASTVAR(&BY)THEN OBS=0 ;
    );
      OBS+1;
                %do i=1 %to &nanalyze;
                      &&A&i = (substr(mispat,&i,1) = '1');
                      format &&A&i miss.;
                      label  &&A&i = "&&var&i";
                      %end;
    *  DROP MISPAT;
       * TRANSLATE BACK FROM BINARY STRING;
     /*
      MISPAT=TRANSLATE(MISPAT,'X.','01');
      %DO I=1 %TO &NANALYZE;
      &&A&I=SUBSTR(MISPAT,&I,1);
      %END;
     */
      LABEL GROUP='Group';
      LABEL FREQ='Freq';
      LABEL PERCENT='Percent';
      FORMAT PERCENT 5.1;
      LABEL OBS='ByObs';
    RUN;

    PROC PRINT DATA=&out LABEL ;
      %IF &BY NE %STR( ) %THEN %DO;
        %STR(    BY &BY ;);
        %DO I=1 %TO &NBYVARS;
            *Suppress the label so by variable names, rather than labels, are printed;
            LABEL &&BYVAR&I..=' ';
        %END;
      %END;
      TITLE4 'Missing Data Patterns: Sorted By Descending Percent';
      VAR &aliases
         GROUP FREQ PERCENT;
    RUN;
    %END;

      * switch variable names and labels;
    data _null_;
      set &out (obs=1);
      cmd="proc datasets lib=work;modify &out; rename";
      call execute(cmd);
      putlog cmd;
    /* Array of all character variables */
    array temp1 (*) _character_;

    /* Array of all numeric variables */
    array temp2 (*) _numeric_;

      /* For each element in the character array, assign its label */
      /* as the value of NEWLABEL, and output the observation      */
      do i=1 to dim(temp1);
        lbl=vlabel(temp2[i]);
        nam=vname(temp2[i]);
        cmd=cats(nam,'=',lbl);
        if upcase(strip(nam)) ne upcase(strip(lbl)) then call execute(cmd);
        putlog cmd=;
      end;

      /* For each element of the numeric array, assign its label as */
      /* the value of NEWLABEL, and output the observation          */
      do j=1 to dim(temp2);
        lbl=vlabel(temp2[j]);
        nam=vname(temp2[j]);
        cmd=cats(nam,'=',lbl);
        if upcase(strip(nam)) ne upcase(strip(lbl)) then call execute(cmd);
        putlog cmd=;
      end;
      call execute(';run;quit;');
    stop;
    run;

    proc print data=&out width=min;
    ;run;quit;
    *-- Clear title statements;
    title3; run;
%MEND _vdo_misspat;


 ***   ****   *****  *      *****  *   *
*   *  *   *    *    *      *      **  *
*   *  *   *    *    *      *      * * *
*   *  ****     *    *      ****   *  **
*   *  *        *    *      *      *   *
*   *  *        *    *      *      *   *
 ***   *        *    *****  *****  *   *;

%macro _vdo_optlen(
        lib=&libname
       ,mem=&data
       ,out=_vdo_maxmin
       );

  /*
  %let lib=sashelp;
  %let mem=zipcode;
  %let out=_vdo_maxmin;
  */

  data _vdo_nummax /view=_vdo_nummax ;
      * Rick Langstoon;
      set %str(&lib).%str(&mem)(drop=_character_);
      retain __typ 'N';
      length __nam $32;
      keep __typ __nam __len __vlen;
      array num[*] _numeric_;
      do i=1 to dim(num);
         __nam=vname(num[i]);
         __vlen=vlength(num[i]);
         if num[i]=. then __len=3;
         else do;
            if num[i] ne trunc( num[i], 7 ) then __len = 8 ; else
            if num[i] ne trunc( num[i], 6 ) then __len = 7 ; else
            if num[i] ne trunc( num[i], 5 ) then __len = 6 ; else
            if num[i] ne trunc( num[i], 4 ) then __len = 5 ; else
            if num[i] ne trunc( num[i], 3 ) then __len = 4 ; else __len=3;
            output;
         end;
       end;
   run;quit;

   * long and skinny for char vars;
   data _vdo_chrmaxmin/view=_vdo_chrmaxmin;
      do until(dne);
         set %str(&lib).%str(&mem)(drop=_numeric_) end=dne;
         array chr[*] _character_;
         retain __typ 'C';
         length __nam $32;
         keep __typ __nam __len __vlen;
         do i=1 to dim(chr);
            __nam  = vname(chr[i]);
            __vlen = vlength(chr[i]);
            __len  = length(chr[i]);
            output;
         end;
      end;
      dne=0;
      do until(dne);
        set _vdo_nummax;
        output;
      end;
      stop;
   run;

   proc sql;
      create
         table &out as
      select
         __typ
        ,case (__typ)
           when ('C') then 'Character'
           else            'Numeric'
         end as Variable_Type
        ,__nam      as Name
        ,max(__len)  as New_Length
        ,max(__vlen) as Original
        ,max(__vlen) - max(__len)  as Savings
      from
        _vdo_chrmaxmin
      group
        by __typ, __nam
   ;quit;

    title;footnote;
    title1 ' ';title2 ' ';title3 ' ' ;
    title4 "Maximum Number of Bytes to hold Character and Numeric Vales Exactly";

    proc print data=&out width=min;
    run;quit;

%mend _vdo_optlen;

*   *    *    *   *  *   *  *****  *   *
** **   * *   *   *  ** **    *    **  *
* * *  *   *   * *   * * *    *    * * *
*   *  *****    *    *   *    *    *  **
*   *  *   *   * *   *   *    *    *   *
*   *  *   *  *   *  *   *    *    *   *
*   *  *   *  *   *  *   *  *****  *   *;

%macro _vdo_getmaxmin(
       lib=&libname
      ,mem=&data
      ,out=_vdo_jst
      ) /DES= "Used by utl_voodoo for max mins";

    %local lst mx n ;

    /*
    * for testing;
    %let lib=sashelp;
    %let mem=shoes;
    */

    options nonotes;

    proc sql noprint;
       select name into :lst separated by ' ' from _vvcolumn
       where upcase(libname)=upcase("&lib.") and upcase(memname)=upcase("&mem.");
    quit;

    options notes;

    %put lst=&lst;

    %macro _vdo_getmaxmin001;

       proc sql;
         create table _vdo_getmax(drop=done) as
         %let n=1;
         select
           'max' as typ,
           %do %until (%scan(&lst,&n)=);
             %let mx=%scan(&lst,&n);
             %str(max(&mx) as &mx,)
             %let n=%eval(&n + 1);
           %end;
             1 as done
         from
             %str(&lib.).%str(&mem.)
      ;
         create table _vdo_getmin(drop=done) as
         %let n=1;
         select
           'min' as typ,
           %do %until (%scan(&lst,&n)=);
             %let mx=%scan(&lst,&n);
             %str(min(&mx) as &mx,)
             %let n=%eval(&n + 1);
           %end;
             1 as done
         from
             %str(&lib.).%str(&mem.)
      ;
      quit;

    %mend _vdo_getmaxmin001;

    %_vdo_getmaxmin001;

    data _vdo_bth;
       set _vdo_getmax _vdo_getmin;
    run;

    proc transpose data=_vdo_bth out=_vdo_bthxpo;
    var _character_ _numeric_;
    id typ;
    run;

    data &out(rename=_name_=variable where=(upcase(variable) ne 'TYP'));
      retain min max;
      set _vdo_bthxpo;
      min=left(min);
      max=left(max);
    run;

    title;footnote;
    title1 ' ';title2 ' ';title3 ' ' ;
    title4 "Maximums and Minimums %str(&lib.).%str(&mem.)";
    proc print data=&out noobs width=min uniform;
    var variable min max;
    run;

%mend _vdo_getmaxmin;

****   *****   ***          *   *  *****  ****          *****  *   *  ****
 *  *  *      *   *         ** **    *     *  *         *      **  *   *  *
 *  *  *      *             * * *    *     *  *         *      * * *   *  *
 ***   ****   * ***         *   *    *     *  *         ****   *  **   *  *
 *  *  *      *   *         *   *    *     *  *         *      *   *   *  *
 *  *  *      *   *         *   *    *     *  *         *      *   *   *  *
****   *****   ***          *   *  *****  ****          *****  *   *  ****;

%macro _vdo_begmidend(dummy);

    *----------------------------------
    * Contents and prints;

    * ...clear all secondary titles;
    title3;

    proc contents data= &libname..&data.
        position
        details
        ;
        run;

    title3 "Sample observations";

    * We need to get this again, polluted the value above;
    %nobs( libname= &libname., data= &data. );

    * If less than 60 records, print them all;
    %if &nobs. le 60 %then
        %do;

        title5 "The following is a complete listing of %upcase( &libname. ).%upcase( &data. )";
        proc print data= &libname..&data.
            rows= page
            label
            uniform
            n
            ;

            run;

        %end;

    %else
        %do;

        title3 "Sample observations  --  showing first, last, and middle 20 records";

        title5 "Records 1 - 20";
        proc print data= &libname..&data.( obs= 20 )
            rows= page
            label
            uniform
            n
            ;

            run;

        title5 "Last 20 Records  --  %eval( &nobs. - 19) - &nobs.";
        proc print data= &libname..&data.(
            firstobs= %eval( &nobs. - 19)
            )

            rows= page
            label
            uniform
            n
            ;

            run;


        %let Middle = %eval( &nobs. / 2 );

        title5 "Middle 20 Records  --  %eval( &Middle. - 9 ) - %eval( &Middle. + 10 )";
        proc print data= &libname..&data.(
            firstobs= %eval( &Middle. - 9 )
            obs     = %eval( &Middle. + 10 )
            )

            rows= page
            label
            uniform
            n
            ;

            run;


        %end;


 %mend _vdo_begmidend;

 ***   *      *****    *    *   *
*   *  *      *       * *   **  *
*      *      *      *   *  * * *
*      *      ****   *****  *  **
*      *      *      *   *  *   *
*   *  *      *      *   *  *   *
 ***   *****  *****  *   *  *   *;

 %macro _vdo_clean(dummy);

    *--------------------------------;
    %exit:

    * Clear titles;
    title;

    * Reset obs to its default;
    data _null_;
        set work._vvRSOpt;

        select( optname );
        when( 'OBS' )
            call execute( 'options obs= ' || trim( setting ) || ';' );
        when( 'FIRSTOBS' )
            call execute( 'options firstobs= ' || trim( setting ) || ';' );
        when( 'LINESIZE' )
            call execute( 'options linesize= ' || trim( setting ) || ';' );
        when( 'PAGESIZE' )
            call execute( 'options pagesize= ' || trim( setting ) || ';' );

        otherwise
            do;
            * nothing;
            end;

        end; * select;

        run;

    proc delete  data= work._vvRSOpt;
    run;quit;

    * Clear temp datasets if they exist;
    proc datasets lib= work nolist;


        %if %upcase(&Cleanup.) eq %upcase(TRUE) %then
            %do;
            delete
                _vv1M
                _vv1D
                _vvCh1
                _vvCh2
                _vvNumA
                _vvDt
                _vvNum1
                _vvNum2
                _vvOne
                _vvOneS1
                _vvOneV1
                _vvErr
                _vvNul
                ;

            %end;
    run;
    quit;

%mend _vdo_clean;

 ***   *   *    *    ****   *****
*   *  *   *   * *   *   *    *
*      *   *  *   *  *   *    *
*      *****  *****  ****     *
*      *   *  *   *  * *      *
*   *  *   *  *   *  *  *     *
 ***   *   *  *   *  *   *    *;


%macro _vdo_chartx(var,typ,val,lbl,fmt,lib=&libname,mem=&data)/minoperator;

  %if &xeqcnt = 0 %then %do;

     proc sql;
       create
       table _vv_tic (tic num);
       insert into _vv_tic values(1000000000  );
       insert into _vv_tic values(100000000  );
       insert into _vv_tic values(10000000  );
       insert into _vv_tic values(1000000  );
       insert into _vv_tic values(500000  );
       insert into _vv_tic values(250000  );
       insert into _vv_tic values(100000  );
       insert into _vv_tic values(50000  );
       insert into _vv_tic values(10000  );
       insert into _vv_tic values(5000  );
       insert into _vv_tic values(1000  );
       insert into _vv_tic values(500   );
       insert into _vv_tic values(200   );
       insert into _vv_tic values(100   );
       insert into _vv_tic values(80    );
       insert into _vv_tic values(70    );
       insert into _vv_tic values(60    );
       insert into _vv_tic values(50    );
       insert into _vv_tic values(40    );
       insert into _vv_tic values(30    );
       insert into _vv_tic values(20    );
       insert into _vv_tic values(10    );
       insert into _vv_tic values(5     );
       insert into _vv_tic values(2     );
       insert into _vv_tic values(1     );
       insert into _vv_tic values(0.5   );
       insert into _vv_tic values(0.2   );
       insert into _vv_tic values(0.1   );
       insert into _vv_tic values(0.05  );
       insert into _vv_tic values(0.02  );
       insert into _vv_tic values(0.01  );
       insert into _vv_tic values(0.005 );
       insert into _vv_tic values(0.002 );
       insert into _vv_tic values(0.001 );
       insert into _vv_tic values(0.0005);
       insert into _vv_tic values(0.0002);
       insert into _vv_tic values(0.0001);
    ;quit;

  %end;

  %let xeqcnt=%eval(&xeqcnt + 1);

  title "Variable=&var Type=%upcase(&typ) label=&lbl";

  /* turn into macro if you like
    %let lib=qhp;
    %let mem=qhp.QHP_200DEM_CATOLDNEWFIX;
    %let mem=QHP_200DEM_CATOLDNEWFIX;
    %let var=coverage_level;

  %let skp1=%sysfunc(reverse(&var));
  %put &=skp1;
  %let skp2=%substr(&skp1,1,1);
  %let skp=%eval(1 - (&skp2 in 2 3 4 5 6 7 8 9 ));

  %put *** &var ** &skp ****;
  run;quit;
  */

  %if (&val le 100) and %upcase(&typ)=%upcase(char)  %then %do;

    /* Produce the chart  %let var=drg2_cd; %let libname=work; %let data=tstdat;  */
    %put *** <= 90 ***;
    proc sql;
       create table _vv_unqdes as select distinct &var as maxis1 from %str(&lib).%str(&mem)
    ;quit;

    proc sql;
       create
         table _vv_odr as
       select
        substr(&var,1,16) as sixtee
       ,&var
       ,length(&var) as len
       ,count(*)  as wgt
       from
        %str(&lib).%str(&mem)(keep=&var)
       group
        by substr(&var,1,16), &var, length(&var)
       order
        by sixtee, &var
    ;quit;

    data _vv_uplo(keep=sixtee &var wgt idx);
      set _vv_odr;
      by sixtee &var;
      if first.sixtee and last.sixtee then output;
      else do;
        idx=int(16*uniform(len))+1;
        slg=substr(sixtee,idx,1);
        if slg=upcase(slg) then substr(sixtee,idx,1)=lowcase(slg);
        else substr(sixtee,idx,1)=upcase(slg);
        output;
      end;
    run;

    proc datasets nolist ; delete _vv_h;run;quit;

    ods exclude all;
    options ls=116;
    ods output hbar=_vv_h;
    ods listing close;
    proc chart data=_vv_uplo;
         label &var="";
         hbar sixtee/type=freq discrete descending freq=wgt;
    run;
    ods output close;
    ods select all;
    ods listing;
    options ls=171;

         proc print data=_vv_h width=min label split='#' noobs;
         title1 ' ';title2 ' ';title3 ' ' ;
         title4 "Histogram for character variable &var";
          label batch="#";
          var batch;
         run;

    %end;
    /* Produce the chart  %let var=water; %let libname=work; %let data=tstdat;  */
    %else %do;
      %if %upcase(&typ)=%upcase(num) /* and &skp=1 and &val<3000*/ %then %do;

       %put &=val;

       proc sql noprint;
        select
             min(&var), max(&var)
        into
             :min,   :max
        from
            %str(&lib).%str(&mem)
       ;quit;

       /* do not use datastep so user can turn into macro if he likes */

       %let finmax=;

       data _null_;
         retain range %sysevalf(&max - &min);
         set  _vv_tic end=dne;
         seq=_n_;
         max    = ceil (&max/tic)*tic;
         min    = floor(&min/tic)*tic;
         ceilmax  = max - &max;
         floormin = min - &min;
         *put _all_;
         if (( ceilmax > tic       or  floormin  <  -tic )    or
             ( ceilmax > .1*range  or  floormin  <= -.1*range or tic > .2 *range)) then flg=1;
         else do;
             call symputx('finmax',put(max,best14.));
             call symputx('finmin',put(min,best14.));
             call symputx('finint',put(tic,best14.));
             stop;
         end;
       run;

     %put finmax=&finmax finmin=&finmin finint=&finint;

     %if &finmax ne %then %do;

       proc datasets nolist; delete _vv_h9 nolist;run;quit;

       ods exclude all;
       ods listing close;
       ods output hbar=_vv_h9;
       proc chart data=%str(&lib).%str(&mem);
          %if %upcase(&fmt)=%upcase(date7,) %then %do; %str(format &var date7.;); %end;
          hbar &var/midpoints=(&finmin to &finmax by &finint) type=freq;
       run;

       ods select all;
       ods listing;

       proc print data=_vv_h9(where=(not (SCAN(batch,-4) = '0' or substr(batch,40)='')))  noobs label split='#';

        title1 ' ';title2 ' ';title3 ' ';
        title4 "Histogram for numeric variable &var";
        label batch="#";
        var batch;
       run;quit;

     %end;

     %end;
   %end;
%mend _vdo_chartx;

*   *  *****   ***   ****    ***   ****
** **    *    *   *  *   *  *   *  *   *
* * *    *     *     *   *  *   *  *   *
*   *    *      *    ****   *   *  ****
*   *    *       *   *      *   *  *
*   *    *    *   *  *      *   *  *
*   *  *****   ***   *       ***   *;

%macro _vdo_mispop(lib=&libname,mem=&data);

    title1 "Missing vs Populated Frequencies";

    Proc format;
         value msspop
          . = 'Missing'
          0 = 'Zero'
          0<-high = "Positive"
          low-<0 = 'Negative'
          other='Special Missing'
    ;
         value $mscpop
          'Unknown',' ','NAN','UNK','U','NA','UNKNOWN',
          'Miss','Mis'
          'Missing','MISSING','MISS','MIS'
             ='Missing'
          other='Populated'
    ;
    run;

    proc freq
              compress
                data=%str(&lib).%str(&mem)
    ;
    format  _character_ $mscpop.;
      tables _character_ / missing ;
    run;

    proc freq
              compress
                data=%str(&lib).%str(&mem)
    ;
    format _numeric_ msspop. ;
      tables _numeric_ / missing ;
    run;

%mend _vdo_mispop;

%macro _vdo_mispoptbl(lib=&libname,mem=&data);

    /*

      data zipcode;
        set sashelp.zipcode;
      run;quit;

      %let lib=work;
      %let mem=zipcode;
    */

    title1 "Missing vs Populated Frequencies";

    Proc format;
         value mispopn
          . = 'MIS'
          other='POP';
    ;
         value $mispopc
          ' ' = 'MIS'
          other='POP'
    ;
    run;

    proc sql noprint;
       select count(*) into :_vdo_popmiscnt trimmed from %str(&lib).%str(&mem)
    ;quit;

    ods exclude all;
    ods output onewayfreqs=_vdo_mispop(keep=table frequency);
    proc freq data=%str(&lib).%str(&mem) ;
    format _character_ $mispopc. _numeric_ mispopn.;
    run;quit;
    ods select all;

    /*
    Up to 40 obs WORK.HSP_QA1_FRQ total obs=21

    Obs    TABLE                FREQUENCY

      1    Table ZIP              41267
      2    Table Y                41267
      3    Table X                41267
      4    Table ZIP_CLASS        11455
      5    Table CITY             41267
    */

    data _vdo_mispop001;
       retain variable pop mis mispct;
       length variable $32;
       keep variable pop mis mispct;
       set _vdo_mispop (rename=FREQUENCY=pop);
       mis=sum(&_vdo_popmiscnt, -1*pop);
       mispct=mis/&_vdo_popmiscnt;
       variable=scan(table,2);
    run;quit;

    proc report data=_vdo_mispop001 nowd;
      cols ( "Populated,  Missing and Missing Frequencies and Percents" variable pop mis mispct);
      define variable /display "Variable"   ;
      define pop      /display "Populated"           format=comma18.;
      define mis      /display "Missing"             format=comma18.;
      define mispct   /display "Missing#Percent"     format=percent10.2;
    run;quit;


%mend _vdo_mispoptbl;


/*
proc datasets lib=work kill nolist;
run;quit;

data zipcode;
  set sashelp.zipcode;
run;quit;

%_vdo_mispoptbl(lib=work,mem=zipcode)
*/


*   *  *****  *   *  *   *  *   *   ***
*  *   *      *   *  *   *  **  *  *   *
* *    *       * *   *   *  * * *  *   *
**     ****     *    *   *  *  **  *   *
* *    *        *    *   *  *   *  * * *
*  *   *        *    *   *  *   *  *  **
*   *  *****    *     ***   *   *   ****;

%macro _vdo_keyunq(
       lib=&libname
      ,mem=&data
      ,key=_all_
       );
  /*
   %let lib=work ;
   %let mem=tstdat    ;
   %let key=_all_    ;
  */

  %local obsdup;

  proc sort data=%str(&lib).%str(&mem) out=_vdo_dup nouniquekey;
     by &key;
  run;quit;

  proc sql noprint;select count(*) into :obsdup separated by ' ' from _vdo_dup;quit;

  %If &obsdup ne 0 %Then %do;

    * need a key for transpose;
    data _vdo_addkey;
       set _vdo_dup;
       rec=_n_;
    run;quit;

    Proc Transpose Data=_vdo_addkey Out=_vdo_addkeyxpo;
    Var _all_;
    id rec;
    run;

    Data _vdo_addkeyfix;

      length _character_ $16.;

      Set _vdo_addkeyxpo;

      Array chr[*] _character_;

      Do i=1 to dim(Chr);

        Chr[i]=Left(Chr[i]);

      End;
      drop i;

    Run;
    ods select all;
    ods listing;

    Proc Print data=_vdo_addkeyfix width=min;
    title1 ' ';title2 ' ';title3 ' ' ;
    title4 "Vertical List of Duplicates (&key -- &obsdup duplicates)";
    run;quit;

    Proc Print data=_vdo_dup width=min;
    title1 ' ';title2 ' ';title3 ' ' ;
    title4 "Horizontal List of Duplicates (&key -- &obsdup duplicates)";
    run;

  %end;
  %else %do;
    data _null_;
     file print;
     put "******************************************************************";
     put "*                                                                *";
     put "* No duplicates in &key in &lib &mem"                              ;
     put "*                                                                *";
     put "******************************************************************";
   run;
 %end;

%mend _vdo_keyunq;

****   *   *  ****    ***    ***   *
 *  *  *   *  *   *  *   *  *   *  *
 *  *  *   *  *   *  *      *   *  *
 *  *  *   *  ****   *      *   *  *
 *  *  *   *  *      *      *   *  *
 *  *  *   *  *      *   *  *   *  *
****    ***   *       ***    ***   *****;

%macro _vdo_dupcol(
       lib=&libname
      ,mem=&data
      ,typ=Char
      );

     /* %let typ=num;  */
      options nonotes;
      data _vvren;
         retain _vvvls;
         length _vvvls $32560;
         set _vvcolumn (where=( upcase(type)=%upcase("&typ") and
           libname=%upcase("&lib") and memname = %upcase("&mem"))) end=dne;
           _vvvls=catx(' ',_vvvls,quote(strip(name)));
         if dne then call symputx('_vvvls',_vvvls);
      run;quit;
      option notes;

      %put &_vvvls;
      %let _vvdim=%sysfunc(countw(&_vvvls));
      %*put &=_vvdim;

      data _null_;;
       length var wth $32560;
       array nam[&_vvdim]  $32 (&_vvvls);
       do i=1 to (dim(nam)-1);
         do j=i+1 to dim(nam);
          var=catx(' ',var,nam[i]);
          wth=catx(' ',wth,nam[j]);
        end;
       end;
       call symputx('_vvtop',var);
       call symputx('_vvbot',wth);
      run;

      %put &_vvtop;
      %put &_vvbot;

      ods listing close;
      ods output comparesummary=_vvcmpsum;
      proc compare data=%str(&lib).%str(&mem) compare=%str(&lib).%str(&mem) listequalvar novalues;
         var &_vvtop;
         with &_vvbot;
      run;quit;
      ods listing;

      data _vveql(keep=batch);
        retain flg 0;
        set _vvcmpsum;
        if index(batch,'Variables with All Equal Values')>0 then flg=1;
        if index(batch,'Variables with Unequal Values'  )>0 then flg=0;
        if flg=1;
      run;quit;

      proc sql noprint;select count(*) into :_vvcntstar from _vveql;quit;
      title;footnote;
      %put &=_vvcntstar;

      %if &_vvcntstar ^= 0 %then %do;
         proc print data=_vveql;
         title1 ' ';title2 ' ';title3 ' ' ;
         title4 "These &typ variables have equal values for all observations";
         run;quit;
      %end;
      %else %do;
         data _null_;
           file print;
           put //;
           put "Comparison of Numeric variables to see if a variable is duplicated exactly";
           put //;
           put "*** NO equal &typ Variables with All Equal Values found ***";
           put ' ' //;
         run;
      %end;

%mend _vdo_dupcol;

 ***    ***   ****
*   *  *   *  *   *
*      *   *  *   *
*      *   *  ****
*      *   *  * *
*   *  *   *  *  *
 ***    ***   *   *;

%macro _vdo_cor(
       lib=&libname
      ,mem=&data
      );

    data _vcor0th/view=_vcor0th;
      set %str(&lib).%str(&mem) (keep=_numeric_);
      _rec=_n_;
      if _n_=1 then _rec=.;
    run;

    ods exclude all;
    ods output spearmancorr=_vvcor1st;
    proc corr data=_vcor0th (keep=_numeric_) spearman;
       var _numeric_;
       with _numeric_;
    run;
    ods select all;

    proc sql noprint;select count(*) into :_vv_num separated by ' ' from _vvcor1st;quit;

    %put &=_vv_num;

    data _vvcor2nd;
      keep var wth n val p_rec;
      set _vvcor1st(drop=label );
      array num[*] _numeric_;
      do _i_=1 to &_vv_num;
        if num[_i_] ne . then do;
           var=variable;
           wth=vname(num[_i_]);
           n=num[_i_+ %eval(2 * &_vv_num)];
           val=abs(num[_i_]);
           if (_i_ < _n_
          and not (var='_REC' or wth = '_REC')) then output;
        end;
      end;
    run;

    proc sort data=_vvcor2nd out=vv_corsrt;
    by descending val;
    run;

    title "Variable Correlations (Spearman)";
    proc print data=vv_corsrt(obs=100) noobs width=min label split='#';
    label
        var = "Variable"
        wth = "Correlated#With"
        val = "Correlation#Coef"
        n   = "Number of Obs"
        p_rec   = "Spearman P";
    var var wth val n p_rec;
    run;
    ods select all;

   options ls=64 ps=44;
    data _null_;
        set vv_corsrt(obs=10);
        call execute(catx(" ",
           "proc plot data=%str(&lib).%str(&mem); plot", var,"*",wth,"='*';run;quit;"));
    run;quit;
    options ls=255 ps=5000;


%mend _vdo_cor;

*   *  *   *  *   *  *   *  *   *  *   *
** **  **  *  *   *  ** **  **  *  *   *
* * *  * * *   * *   * * *  * * *   * *
*   *  *  **    *    *   *  *  **    *
*   *  *   *    *    *   *  *   *    *
*   *  *   *    *    *   *  *   *    *
*   *  *   *    *    *   *  *   *    *;

%macro _vdo_mnymny(
       lib=&libname
      ,mem=&data
      ,maxval=2000
      ,maxvar=100
      )/des="Many to Many, One to Many, Many to One and Many to Many" ;

    /*
     %let maxval=2000;
     %let maxvar=15;
     %let lib=work;
     %let mem=tstdat;
    */


    %macro _vdo_relhow(varlft=,varrgt=);

        /*
          %let varrgt=STATE;
          %let varlft=MONMON;
          %let lib=work;
          %let mem=tot_nrol;
        */

        proc sort data=%str(&lib).%str(&mem)(keep=&varlft &varrgt) out=__varlft nodupkey noequals;
        by  &varlft &varrgt;
        run;quit;

        data __onemny(keep=onemny);
          retain onemny "UNKNOW";
          do until (dne);
              set __varlft end=dne;
              by &varlft;
              if not (first.&varlft  and last.&varlft) then do;
                 onemny="ONEMNY";
                 leave;
              end;
          end;
          output;
          stop;
        run;quit;

        proc sort data=__varlft out=__varrgt nodupkey noequals;
        by  &varrgt &varlft;
        run;quit;

        data __mnyone(keep=mnyone);
          retain mnyone "UNKNOW";
          do until (dne);
              set __varrgt end=dne;
              by &varrgt;
              if not (first.&varrgt  and last.&varrgt) then do;
                 mnyone="MNYONE";
                 leave;
              end;
          end;
          output;
          stop;
        run;quit;

        data __mnymnytwo;
           length out $64;
           merge __onemny __mnyone;
           select;
              when ( mnyone="MNYONE" and onemny="ONEMNY" ) out = "Many to Many    &varlft to &varrgt";
              when ( mnyone="MNYONE" and onemny="UNKNOW" ) out = "Many to One     &varlft to &varrgt";
              when ( mnyone="UNKNOW" and onemny="ONEMNY" ) out = "One to Many     &varlft to &varrgt";
              when ( mnyone="UNKNOW" and onemny="UNKNOW" ) out = "One to One      &varlft to &varrgt";
           end; * leave off otherwise to force error;
           output;
        run;quit;

        proc append data=__mnymnytwo base=__basmnymny;
        run;quit;

     %mend _vdo_relhow;

    data _vvboth;

      set _vvnuma _vvch1 _vvch2;

      if 1 <  values < &maxval;

      keep variable values;

    run;

    proc sql noprint;
       select count(*) into :nobs separated by ' ' from _vvboth;
       select variable into :vars separated by ' ' from _vvboth;
    quit;

    %put &=vars;

    %if &nobs. > &maxvar %then %let nbs=&maxvar;
    %else %let nbs=&nobs.;

    proc datasets nolist;
      delete __basmnymny;
    run;quit;

    %do i=1 %to %eval(&nbs.-1);

      %do j=%eval(&i + 1) %to %eval(&nbs.);

         %if &i ne &j %then %do;

           Data _null_;
             cmd=cats('%_vdo_relhow(varlft=',"%scan(&vars.,&i),",'varrgt=',"%scan(&vars.,&j));");
             put cmd;
             call execute(cmd);
           run;

         %end;

      %end;

    %end;

    title1 ' ';title2 ' ';title3 ' ' ;
    TITLE4 "Relationship OF VARIABLES WHERE MAX LEVELS IS &MAXVAL AND MAX NUMBER OF VARIABLES IS &MAXVAR";
    title5 "One to One  -- One to many  --  Many to One -- Many to Many ";
    proc print data=__basmnymny;
    var out;
    run;quit;

%mend _vdo_mnymny;


 ***   ****     *    *   *  *****  ****          *   *
*   *  *   *   * *   ** **  *      *   *         *   *
*      *   *  *   *  * * *  *      *   *         *   *
*      ****   *****  *   *  ****   ****          *   *
*      * *    *   *  *   *  *      * *           *   *
*   *  *  *   *   *  *   *  *      *  *           * *
 ***   *   *  *   *  *   *  *****  *   *           *;


%macro _vdo_cmh(
       lib=&libname
      ,mem=&data
      ,maxval=31
      ,maxvar=10
      )/des="Defaults to all two way cross tabs forupto 10 variables with less than 11 levels 45 cross tabs 10 choose 2" ;


    /*
     %let maxval=11;
     %let maxvar=15;
     %let lib=work;
     %let mem=tstdat;
    */

    data _vvboth;

      set _vvnuma _vvch1 _vvch2;

      if 2 <=  values < &maxval;

      keep variable values;

    run;

    proc sql noprint;
       select count(*) into :nobs separated by ' ' from _vvboth;
       select variable into :vars separated by ' ' from _vvboth where upcase(variable) not in ("LIBREF","COUNT");
    quit;

    %if &nobs. > &maxvar %then %let nbs=&maxvar;
    %else %let nbs=&nobs.;

    proc datasets nolist;
      delete _vvcramer;
    run;quit;

    %do i=1 %to %eval(&nbs.-1);

      %do j=%eval(&i + 1) %to %eval(&nbs.);

         /*
           %let j=2;  %let i=1;
           %let vars=DRG9 DRG10 DRG1_CD DRG2_CD DRG3_CD;
         */

         %if &i ne &j %then %do;

           ods exclude all;
           ods output ChiSq=_vvz&i&j(where=(statistic="Cramer's V") drop=df prob);
           proc freq data=%str(&lib).%str(&mem.);
           tables %scan(&vars.,&i) * %scan(&vars.,&j) / chisq missing;
           run;

           proc append base=_vvcramer data=_vvz&i&j force;
           run;quit;

           ods select all;
           proc datasets nolist;
             delete _vvz&i&j;
           run;quit;
         %end;

      %end;

    %end;

    proc sort data=_vvcramer out=_vvcramersrt;
    by descending value;
    run;quit;

    title1 ' ';title2 ' ';title3 ' ' ;
    TITLE4 "Cramer V";
    TITLE5 "ALL PAIRS OF VARIABLES WHERE MAX LEVELS IS &MAXVAL AND MAX NUMBER OF VARIABLES IS &MAXVAR";
    title6 "%scan(&vars.,&i) * %scan(&vars.,&j) ";

    proc print data=_vvcramersrt width=min;
    run;quit;

%mend _vdo_cmh;

%*_vdo_cmh(
       lib=work
      ,mem=tstdat
      ,maxval=2000
      ,maxvar=30
     );

*****    *    ****    ***   *   *  *****
  *     * *    *  *  *   *  **  *  *
  *    *   *   *  *  *   *  * * *  *
  *    *****   ***   *   *  *  **  ****
  *    *   *   *  *  *   *  *   *  *
  *    *   *   *  *  *   *  *   *  *
  *    *   *  ****    ***   *   *  *****;

%macro _vdo_tabone(
       lib=&libname
      ,mem=&data
      ,maxval=10000
      ,maxvar=100
      ,top=50
      ,tab=&tabone
      )/des="tab one variable versus all other variables - tab variable must have fewer than maxval levels " ;


    /*
     %let maxval=11;
     %let lib=work;
     %let mem=tstdat;
    */

    data _vvboth;

      set _vvnuma _vvch1(obs=&maxvar where=(0 <  values < &maxval))
           _vvch1(obs=&maxvar where=(0 <  values < &maxval))
          _vvch2(obs=&maxvar where=(0 <  values < &maxval))
      ;
      keep variable values;

    run;

    proc sql noprint;
       select count(*) into :nbs separated by ' ' from _vvboth;
       select variable into :vars separated by ' ' from _vvboth where upcase(variable) not in ("LIBREF","COUNT");;
    quit;

    %put &=vars;

    %do j=1 %to %eval(&nbs.);

         proc freq data=%str(&lib).%str(&mem.) noprint order=freq;
            title1 ' ';title2 ' ';title3 ' ' ;
            TITLE4 "TOP &TOP FOR &TAB WITH ALL OTHER VARIABLES WHERE MAX LEVELS IS &MAXVAL AND MAX NUMBER OF VARIABLES IS &MAXVAR";
            title5 " &tab with %scan(&vars.,&j) other variables ";
            tables %str(&tab) * %scan(&vars.,&j) / list nocol norow nopercent missing out=_vvx&j;
         run;

         proc sort data=_vvx&j out=_vvz&j noequals;
         by descending count;
         run;

         proc print data=_vvz&j(obs=&top) width=min;
         run;quit;

         proc datasets nolist;
           delete _vvx&j _vvz&j;
         run;quit;

    %end;

%mend _vdo_tabone;

*****    *    ****     *    *      *
  *     * *    *  *   * *   *      *
  *    *   *   *  *  *   *  *      *
  *    *****   ***   *****  *      *
  *    *   *   *  *  *   *  *      *
  *    *   *   *  *  *   *  *      *
  *    *   *  ****   *   *  *****  *****;

%macro _vdo_taball(
       lib=&libname
      ,mem=&data
      ,maxval=2000
      ,maxvar=100
      ,top=20
      ,taball=
      )/des="Defaults to all two way cross tabs forupto 10 variables with less than 11 levels 45 cross tabs 10 choose 2" ;


    /*
     %let maxval=11;
     %let maxvar=15;
     %let lib=work;
     %let mem=tstdat;
    */

    data _vvboth;

      set _vvnuma _vvch1;

      if 0 <  values < &maxval;

      keep variable values;

    run;

    /*
    proc sql noprint;
       select count(*) into :nobs separated by ' ' from _vvboth;
       select variable into :vars separated by ' ' from _vvboth where upcase(variable) not in ("LIBREF","COUNT");
    quit;

    %if &nobs. > &maxvar %then %let nbs=&maxvar;
    %else %let nbs=&nobs.;
    */

    %let vars=&taball;
    %let dim=%sysfunc(countw(&vars.));

    %do i=1 %to %eval(&dim.-1);

      %do j=%eval(&i+1) %to &dim.;

         %if (&i ne &j)  %then %do;

           proc freq data=%str(&lib).%str(&mem.) noprint order=freq;
              title1 ' ';title2 ' ';title3 ' ' ;
              TITLE4 "TOP &TOP ALL PAIRS OF VARIABLES WHERE MAX LEVELS IS &MAXVAL AND MAX NUMBER OF VARIABLES IS &MAXVAR";
              title5 " %scan(&vars.,&i) * %scan(&vars.,&j) top &top frequent ";
              tables %scan(&vars.,&i) * %scan(&vars.,&j) / list nocol norow nopercent missing out=_vvx&i&j;
           run;

           proc sort data=_vvx&i&j out=_vvz&i&j noequals;
           by descending count;
           run;

           proc print data=_vvz&i&j(obs=&top) width=min;
           run;quit;

           proc datasets nolist;
             delete _vvx&i&j _vvz&i&j;
           run;quit;

         %end;

      %end;

    %end;

%mend _vdo_taball;

*   *  *   *   ***          *****  *   *   ***
*   *  **  *  *   *           *    *   *  *   *
*   *  * * *  *   *           *    *   *  *   *
*   *  *  **  *   *           *    *   *  *   *
*   *  *   *  * * *           *    * * *  *   *
*   *  *   *  *  **           *    ** **  *   *
 ***   *   *   ****           *    *   *   ***;

%macro _vdo_unqtwo(
       lib=&libname
      ,mem=&data
      ,unqtwo=&unqtwo
      )/des="unique variable * other var counts " ;


    /*
     %let lib=fix;
     %let mem   =MON_QHP16MOSTRECENT ;
     %let unqtwo=ZIP_CLASS STATECODE STATENAME AREACODES TIMEZONE PONAME Y X STATE COUNTY MSA AREACODE GMTOFFSET;     ;
    */

    ods exclude all;
    data _vvboth;
      set  _vvnuma
           _vvch1
           _vvch2
      ;
      keep variable values;
    run;

    proc sql noprint;
       select count(*) into :nbs separated by ' ' from _vvboth;
       select variable into :vars separated by ' ' from _vvboth where upcase(variable) not in ("LIBREF","COUNT");;
    quit;

    /* %let nbs=13;
       %let vars=ZIP_CLASS STATECODE STATENAME AREACODES TIMEZONE PONAME Y X STATE COUNTY MSA AREACODE GMTOFFSET;
    */

  %let nvar=%sysfunc(countw(&unqtwo.));

  proc sql;
      select put(count(*),comma15.) into :_allobs from %str(&lib).%str(&mem.)
  ;quit;


  proc datasets nolist;
     delete _vvunqall;
  run;quit;

  %do i=1 %to &nvar;

    %let unqone=%scan(&unqtwo.,&i);

    %put &=unqone;

    proc sql;
        select
           put(count (distinct &unqone.),comma15.)
        into
           :_vr1
        from
           %str(&lib).%str(&mem.)
    ;quit;

    %do j=1 %to %eval(&nbs.);

        /* %let j=3;    */

        %if %scan(&vars.,&j) ne &unqone %then %do;

            proc sql;
              create
                 table _vvunq as
              select
                  "&_allobs."          as records  length=32
                 ,%upcase("&unqone")  as var1st    length=32
                 ,"&_vr1."             as levels_1  length=32
                 ,"%scan(&vars.,&j)"   as var2nd    length=32
                 ,(select put(count (distinct %scan(&vars.,&j)),comma15.)  from %str(&lib).%str(&mem.)) as levels_2  length=32
                 ,put(sum(unq),comma15.) as unique length=32
              from
                 (
                  select
                      count (distinct
                          %scan(&vars.,&j)) as unq
                  from
                    %str(&lib).%str(&mem.)
                  group
                    by &unqone
                 )
            ;quit;

            proc append base=_vvunqall data=_vvunq;
            ;quit;
        %end;
    %end;
  %end;

  ods select all;
  title;
  footnote;

  proc print data=_vvunqall;
  title1 "Count of unique levels for &unqtwo vs all  other variables in %str(&lib).%str(&mem.)";
  run;quit;

%mend _vdo_unqtwo;

%*_vdo_unqtwo(
       lib   =sashelp
      ,mem   =zipcode
      ,unqtwo=ZIP_CLASS STATECODE STATENAME
      );

*****  *   *  ****
*      **  *   *  *
*      * * *   *  *
****   *  **   *  *
*      *   *   *  *
*      *   *   *  *
*****  *   *  ****;

proc datasets kill nolist ;
run;quit;


/*

options fullstimer;run;
data tstdat (compress=binary);
  retain mis ' ' one '1' two '1' mismix ' ' onemix . misa 'A' misnum .;
  format date1-date10 date9.;
  array date[10] date1-date10;
  array bil[10] grocery electric water carpayment mortagepayment gas lunch dinner maint daycare;
  array codes{10]   $5 drg1-drg10;
  array decodes[3]  $5 drg1_cd drg2_cd drg3_cd;
  array age[10]  age1-age10;
  alpha='ABCDEFGHIJKLMNOPQRSTUVWXYZ';
    do rec=1 to 3000;
      if mod(rec,1000)=0 then do; mismix='A';onemix=32;end;
      else                    do; mismix=' ';onemix=. ;end;
      lvl10=mod(rec,10);
      lvl9 =mod(rec,9);
      do i=1 to 10;
         age[i]    =  mod(int(200*uniform(-1)),100);
         date[i]   =  mod(int(1000000*uniform(-1)),100000) - mod(int(1000000*uniform(-1)),100000);
         bil[i]    =  round(10000*uniform(-1),1);
         codes[i]  =  substr(alpha,int(20*uniform(-1))+1,2);
         if i<= 3 then decodes[i]=cats(codes[i],'-',codes[9]);
      end;
      drop alpha;
      recdup=rec;
      output;
    end;
run;

proc freq data=tstdat;
tables drg1_cd*drg1/ all missing;
run;quit;



*Here is what the Validation and Verification Tool can do(see below for
samples)

data best12;
 *infile "csv.txt";
 input num ;
 put num best.;
cards4;
300000000000
3000000000000
;;;;
run;quit;



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
20   Printout of first 20, middle 20 and last 20 observations.
*/

%macro utl_getstm(pth);
   %local revstr cutstr gotstm;
   %if %qupcase(&pth) = OUTPUT %then %do;%let gotstm=1;%end;
   %else %do;
      /* extract the path without the file name */
      %let revstr=%qleft(%qsysfunc(reverse(&pth)));
      %let cutstr=%qsubstr(&revstr,%qsysfunc(indexc(&revstr,%str(/\))));
      %let gotstm=%qleft(%qsysfunc(reverse(&cutstr)));
      %if &gotstm= %then %let gotstm=0;
   %end;
   %str(&gotstm)
%mend utl_getstm;

%macro DirExist(dir) ;
  /* directory exist */
  %LOCAL rc fileref return;
  %if &dir=1 or &dir=0 %then %let return=&dir;
  %else %do;
     %let rc = %sysfunc(filename(fileref,&dir)) ;
     %if %sysfunc(fexist(&fileref))  %then %let return=1;
     %else %let return=0;
  %end;
  &return
%mend DirExist;



%MACRO _vdo_UNICHR
     (
      UTITLE=PROC UNIVARIATE ON CHAR VARIABLES
      ,UOBJ=UTLMMCH
    )
    /DES = "PROC MEANS ON CHAR VARIABLES";

 %let uin=&libname..&data;

 %put %sysfunc(ifc(%sysevalf(%superq(uin)=,boolean),**** Please Provide SAS dataset ****,));
 %if %sysfunc(ifc(%sysevalf(%superq(uin)=,boolean),1,0)) eq 0 %then %do;

  /*------------------------------------------------------------*\
  |                                                              |
  |    TABLE DESCRIPTION---   Fictitious Shoe Company Data       |
  |    TABLE NAME----------   sashelp.shoes                      |
  |                                                              |
  |    ROWS----------------   395                                |
  |                                                              |
  |    VARIABLE NAME-------   SUBSIDIARY                         |
  |    COLUMN LABEL--------                                      |
  |                                                              |
  |    FIELD LENGTH--------   12                                 |
  |    COLUMN NUMBER-------   3                                  |
  |                                                              |
  |    NUMBER BLANKS-------   0                                  |
  |    NUMBER NON BLANK----   395                                |
  |                                                              |
  |    LOWEST VALUE--------   Addis Ababa                        |
  |    HIGHEST VALUE-------   Warsaw                             |
  |                                                              |
  |    SHORTEST LENGTH-----   4                                  |
  |    LONGEST LENGTH------   12                                 |
  |                                                              |
  |    RANGE---------------   Addis Ababa - Warsaw               |
  |                                                              |
  |    INTERQTILE LTR RANGE   C  - P                             |
  |                                                              |
  |    INTERQTILE RANGE----   Copenhagen - Paris                 |
  |                                                              |
  |    MODE LETTER---------   M  61                              |
  |                                                              |
  |    MEDIAN--------------   Los Angeles                        |
  |                                                              |
  |    MODE----------------   Addis Ababa  8                     |
  |                                                              |
  |    COVERAGE % ALPHA-NUM   48                                 |
  |                                                              |
  |    EACH * REPRESENTS---   1  OCCURANCES                      |
  |                                                              |
  |    A  ! *******************************                      |
  |    B  ! *****************************                        |
  |    C  ! ***********************************************      |
  |    D  ! *********                                            |
  |    G  ! *********                                            |
  |    H  ! *********                                            |
  |    J  ! **************                                       |
  |    K  ! ********************************                     |
  |    L  ! ****************************************             |
  |    M  ! *****************************************************|
  |    N  ! ***************                                      |
  |    O  ! ********                                             |
  |    P  ! *****************                                    |
  |    R  ! *********                                            |
  |    S  ! **********************************************       |
  |    T  ! ******************                                   |
  |    V  ! *********                                            |
  \*------------------------------------------------------------*/

%LOCAL
         UIN
         RC
         UI
         UDSID
         UOBS
         UVARS
         ULBL
         VARNAM
         VARTYPE
         VARLABEL
         VARLEN
         QTR1ST
         QTR3RD
         MEDIAN
         MODE
         MODECNT
         UVARNAM
         UVARTYP
         UVARLBL
         UVARLEN
         Qtr1st
         Qtr3rd
         median
         mode
         codecnt
;


PROC DATASETS
               LIBRARY=work
               NOLIST
;
DELETE utlmmch:;
QUIT;



/*-------------------------------------*\
! CREATE TEMPLATE FOR APPEND            !
\*-------------------------------------*/

PROC SQL;

    CREATE
           TABLE
                  UTLMMCH1
           (
            TYPE=DATA            LABEL="METADATA &UIN."         ,
            NAME      CHAR(32)    LABEL="VARIABLE NAME"                    ,
            LABEL     CHAR(200)  LABEL="VARIABLE DESCRIPTION"             ,
            LENGTH    CHAR(8)   LABEL="VARIABLE LENGTH BYTES"            ,
            COLNUM    INT        LABEL="COLUMN NUMBER"                    ,
            NBLANK    INT        LABEL='NUMBER OF BLANK OBSERVATIONS'     ,
            NNONBL    INT        LABEL='NUMBER OF NON-BLANK OBSERVATIONS' ,
            LOWVAL    CHAR(200)  LABEL='LOWEST NON-BLANK VALUE'           ,
            HIGHVAL   CHAR(200)  LABEL='HIGHEST VALUE'                    ,
            SHORTEST  INT        LABEL='SHORTEST LENGTH'                  ,
            LONGEST   INT        LABEL='LONGEST LENGTH'
           )
    ;
QUIT;

%LET UDSID = %SYSFUNC( OPEN ( &uin., I ) );

    /*-------------------------------------*\
    ! GET THE NUMBER OF COLUMNS FOR LOOP    !
    \*-------------------------------------*/

    %LET UOBS = %SYSFUNC(ATTRN(&UDSID,NLOBS));

    %LET UVARS = %SYSFUNC(ATTRN(&UDSID,NVARS));

    %LET ULBL = %SYSFUNC(ATTRC(&UDSID,LABEL));

    %LET UMAX = 0;

    %DO UI = 1 %TO &UVARS;

       /*-------------------------------------*\
       !  GET ATTRIBUTES                       !
       \*-------------------------------------*/

       %LET UVARNAM = %SYSFUNC ( VARNAME  ( &UDSID, &UI  ) );
       %LET UVARTYP = %SYSFUNC ( VARTYPE  ( &UDSID, &UI  ) );
       %LET UVARLBL = %SYSFUNC ( VARLABEL ( &UDSID, &UI  ) );
       %LET UVARLEN = %SYSFUNC ( VARLEN   ( &UDSID, &UI  ) );

       %IF &UVARTYP EQ %QUPCASE(C) %THEN %DO;

          %IF &UVARLEN GT &UMAX %THEN %LET UMAX = &UVARLEN;

       *      *****  ****           ***   *   *  *****   ***
       *        *    *   *         *   *  **  *    *    *   *
       *        *    *   *         *      * * *    *     *
       *        *    ****          *      *  **    *      *
       *        *    * *           *      *   *    *       *
       *        *    *  *          *   *  *   *    *    *   *
       *****    *    *   *          ***   *   *    *     ***

       ****   *****  *****  *      *****   ***
       *   *    *      *    *      *      *   *
       *   *    *      *    *      *       *
       ****     *      *    *      ****     *
       *        *      *    *      *         *
       *        *      *    *      *      *   *
       *        *    *****  *****  *****   ***

       #! PTILES ;


        /*
         %let libname        = work   ;
         %let data          = zipcode ;

         %let uin=&libname..&data;

         %let uvarnam=state;
         %let uobs=41267;

        */


       proc sort data=&uin (keep=&uvarnam)  out=utlmmch5;
         by &uvarnam;
       run;quit;

       data _null_;

           qtr1st=round(&uobs/4);
           set utlmmch5 point=qtr1st;
           call symputx('QTR1ST',&uvarnam);
           put &uvarnam =;

           qtr3rd=round(&uobs - &uobs/4);
           set utlmmch5 point=qtr3rd;
           call symputx('QTR3RD',&uvarnam);
           put &uvarnam =;

           median=round(&uobs/2);
           set utlmmch5 point=median;
           call symputx('MEDIAN',&uvarnam);
           put &uvarnam =;

           stop;

       run;quit;

       proc sql noprint;
         select count(distinct &uvarnam ) into :UNQS separated by '' from &uin
       ;quit;

       %put &=unqs;

       proc freq data=utlmmch5 noprint order=freq;
       tables &uvarnam/ out=utlmmch6;
       ;run;quit;

       data _null_;
          set utlmmch6(obs=1);
          call symputx('MODE',&uvarnam.);
          call symputx('MODECNT',put(count,8.));
       run;

       %put
           &=QTR1ST
           &=QTR3RD
           &=MEDIAN
           &=MODE
           &=MODECNT;

              DATA
                 UTLMMCH0
                  (
                   LABEL="EXTRACT SUMMARY STATS MAX MIN"

                   KEEP =
                          NAME
                          LABEL
                          LENGTH
                          COLNUM
                          NBLANK
                          NNONBL
                          LOWVAL
                          HIGHVAL
                          SHORTEST
                          LONGEST
                          QTR1ST
                          QTR3RD
                          MEDIAN
                          MODE
                          MODECNT
                          UNIQUES
                          _A
                          _B
                          _C
                          _D
                          _E
                          _F
                          _G
                          _H
                          _I
                          _J
                          _K
                          _L
                          _M
                          _N
                          _O
                          _P
                          _Q
                          _R
                          _S
                          _T
                          _U
                          _V
                          _W
                          _X
                          _Y
                          _Z
                          _0
                          _1
                          _2
                          _3
                          _4
                          _5
                          _6
                          _7
                          _8
                          _9
                          __

                  );


                SET
                     &uin.
                     (
                      KEEP = &UVARNAM
                     )
                     END = DONE
                ;

                ATTRIB

                    NAME     LABEL='VARIABLE NAME               ' LENGTH=$32
                    LABEL    LABEL='VARIABLE LABEL              ' LENGTH=$200
                    LENGTH   LABEL='VARIABLE LENGTH             '

                    NBLANK   LABEL='NUMBER OF BLANK OBSERVATIONS'  length=8

                    NNONBL   LABEL='NUMBER OF NON-BLANK OBSERVATIONS' length=8

                    LOWVAL   LABEL='LOWEST NON-BLANK VALUE'
                             LENGTH=$&UVARLEN

                    HIGHVAL  LABEL='HIGHEST VALUE'
                             LENGTH=$&UVARLEN

                    SHORTEST LABEL='SHORTEST LENGTH'   length=8

                    LONGEST  LABEL='LONGEST LENGTH'    length=8
            ;

            length
                QTR1ST  $44
                QTR3RD  $44
                MEDIAN  $44
                MODE    $44
                MODECNT $12
            ;


             RETAIN

                    NBLANK
                    NNONBL
                    SHORTEST
                    LONGEST     0
                    LOWVAL
                    HIGHVAL

               ;


               LENGTH
                       CHAR    $1
                       NAME    $8
            ;

               RETAIN           _A _B _C _D _E _F _G _H _I _J _K _L _M _N _O _P
                                _Q _R _S _T _U _V _W _X _Y _Z _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 __ 0;

               ARRAY OUTVAR {*} _A _B _C _D _E _F _G _H _I _J _K _L _M _N _O _P
                                _Q _R _S _T _U _V _W _X _Y _Z _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 __;


               RETAIN POSITION 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';

               IF &UVARNAM NE ' ' THEN NNONBL = NNONBL + 1;
               ELSE                    NBLANK = NBLANK + 1;

               IF _N_ EQ 1 THEN DO;

                    LOWVAL   = PUT(&UVARNAM,$&UVARLEN..);
                    HIGHVAL  = PUT(&UVARNAM,$&UVARLEN..);

                    SHORTEST = LENGTH(&UVARNAM );
                    LONGEST  = LENGTH(&UVARNAM );

               END;
               ELSE DO;

                   IF &UVARNAM NE ' ' AND
                      LOWVAL   GT     PUT(&UVARNAM,$&UVARLEN..)
                               THEN   LOWVAL = PUT(&UVARNAM,$&UVARLEN..);

                   IF HIGHVAL LT PUT(&UVARNAM,$&UVARLEN..)
                              THEN HIGHVAL= PUT(&UVARNAM,$&UVARLEN..);

                   IF SHORTEST  GT LENGTH(&UVARNAM) THEN SHORTEST = LENGTH(&UVARNAM);
                   IF LONGEST   LT LENGTH(&UVARNAM) THEN LONGEST  = LENGTH(&UVARNAM);

               END;

               CHAR = LEFT(UPCASE(&UVARNAM));

               SLOT = INDEX ( POSITION, CHAR );

               IF SLOT EQ 0 THEN OUTVAR{37}    = OUTVAR{37}   + 1;
               ELSE              OUTVAR{SLOT}  = OUTVAR{SLOT} + 1;

               IF DONE THEN DO;

                  NAME  = "&UVARNAM";
                  LABEL = "&UVARLBL";
                  LENGTH= "&UVARLEN";
                  COLNUM= &UI;

                  LOWVAL  = INPUT( LOWVAL, $&UVARLEN..);
                  HIGHVAL = INPUT( HIGHVAL, $&UVARLEN..);

                  QTR1ST =  "&QTR1ST  ";
                  QTR3RD =  "&QTR3RD  ";
                  MEDIAN =  "&MEDIAN  ";
                  MODE   =  "&MODE    ";
                  MODECNT=  "&MODECNT ";
                  UNIQUES = "&UNQS"    ;

                  OUTPUT;

               END;

         RUN;



         /*
         Middle Observation(1 ) of UTLMMCH0 - Total Obs 1

          -- CHARACTER --
         NAME                 C    8       SUBSIDIA            VARIABLE NAME
         LABEL                C    40                          VARIABLE LABEL
         LENGTH               C    2       12                  VARIABLE LENGTH
         LOWVAL               C    12      Addis Ababa         LOWEST NON-BLANK VALUE
         HIGHVAL              C    12      Warsaw              HIGHEST VALUE
         TOTOBS               C    16      1                   TOTOBS


          -- NUMERIC --
         NBLANK               N    8       0                   NUMBER OF BLANK OBSERVATIONS
         NNONBL               N    8       395                 NUMBER OF NON-BLANK OBSERVATIONS
         SHORTEST             N    8       4                   SHORTEST LENGTH
         LONGEST              N    8       12                  LONGEST LENGTH
         _A                   N    8       30                  _A
         _B                   N    8       28                  _B
         _C                   N    8       46                  _C
         _D                   N    8       8                   _D
         _E                   N    8       0                   _E
         _F                   N    8       0                   _F
         ....
         _K                   N    8       31                  _K
         _L                   N    8       39                  _L
         _M                   N    8       61                  _M
         ....
         _6                   N    8       0                   _6
         _7                   N    8       0                   _7
         _8                   N    8       0                   _8
         _9                   N    8       0                   _9
         __                   N    8       0                   __
         COLNUM               N    8       3                   COLNUM
        */

        *   *  ****   ****            *    *   *  ****           ***   *   *  *****   ***
        *   *   *  *  *   *          * *   **  *   *  *         *   *  **  *    *    *   *
        *   *   *  *  *   *         *   *  * * *   *  *         *      * * *    *     *
        *****   *  *  ****          *****  *  **   *  *         *      *  **    *      *
        *   *   *  *  * *           *   *  *   *   *  *         *      *   *    *       *
        *   *   *  *  *  *          *   *  *   *   *  *         *   *  *   *    *    *   *
        *   *  ****   *   *         *   *  *   *  ****           ***   *   *    *     ***

        #! HDRaANDaCNTS ;

         DATA
              UTLMMCH1
               (
                LABEL="APPEND AND USE LARGEST CHAR LENGTHS"
               );

              length
                      lowval  $&umax
                      highval $&umax
               ;



              SET UTLMMCH0
                  UTLMMCH1
               ;


         RUN;

         proc sort data=&uin(keep=&uvarnam) out=utlmmch5 noequals;
         by &uvarnam;
         ;run;quit;

         * first quartile, median and third quartile and median;

         /*
            %let uvarnam=subsidiary;
            %let uin=sashelp.shoes;
            %let uobs=395;
         */

    %END; /* END VARTYPE */

  %END; /* END CYCLE THROUGH VARS */

  %LET RC = %SYSFUNC ( CLOSE ( &UDSID ) );


  /* CREATE FINAL DATASET AND PROC UNIVARIATES */

  *      *****  ****           ***   *****    *    *****   ***
  *        *    *   *         *   *    *     * *     *    *   *
  *        *    *   *          *       *    *   *    *     *
  *        *    ****            *      *    *****    *      *
  *        *    * *              *     *    *   *    *       *
  *        *    *  *          *   *    *    *   *    *    *   *
  *****    *    *   *          ***     *    *   *    *     ***

  #! LTRaSTATS ;

  DATA
        utlmmch3
         (
          LABEL="UNIVARIATE CHR VARS 1 PAGE PER OB"
         );

         LENGTH
                RANGE   $%EVAL ( 2 * &UMAX + 3 )
                STARS   $101
                VRNAME  $32
                MODELOC $8
                P25LOC  $8
                P75LOC  $8
        ;


         SET
             UTLMMCH1 END=DONE;

         ARRAY OUTVAR {*} _A _B _C _D _E _F _G _H _I _J _K _L _M _N _O _P
                          _Q _R _S _T _U _V _W _X _Y _Z _0 _1 _2 _3 _4 _5 _6 _7 _8 _9 __;

         MODE = MAX ( OF _A -- __ );

         RANGE = COMPBL(LOWVAL!!' - '!!HIGHVAL);

         ZEROS=0;

         P25=0;
         P75=0;

         P25LOC=' ';
         P75LOC=' ';

         DO I = 1 TO 37;

            IF OUTVAR{I} EQ 0 THEN ZEROS = ZEROS + 1;
            IF OUTVAR{I} EQ MODE THEN DO;

               CALL VNAME ( OUTVAR{I}, MODELOC );
               MODELOC = SUBSTR(MODELOC,2);

            END;

            P25 = P25 + 100*OUTVAR{I}/&UOBS;

            IF
               P25    GE  25 AND
               P25LOC EQ ' ' THEN DO;

               CALL VNAME ( OUTVAR{I}, P25LOC );
               P25LOC = SUBSTR(P25LOC,2);

            END;


            P75 = P75 + 100*OUTVAR{I}/&UOBS;

            IF
               P75    GE  75 AND
               P75LOC EQ ' ' THEN DO;

               CALL VNAME ( OUTVAR{I}, P75LOC );
               P75LOC = SUBSTR(P75LOC,2);

            END;

         END;

         COVERAGE = INT ( 100 - 100 * ZEROS / 37 );


         FILE PRINT;

         SCALE = INT ( LOG10(MODE) + 1 );

         REP=10**(SCALE-2);


         PUT
               ///
              'TABLE DESCRIPTION---'     @24      "&ULBL"  /
              'TABLE NAME----------'     @24      "&UIN."  //
              'ROWS----------------'     @24      "&UOBS"  //
              'VARIABLE NAME-------'     @24      NAME     /
              'COLUMN LABEL--------'     @24      LABEL    //
              'FIELD LENGTH--------'     @24      LENGTH   /
              'COLUMN NUMBER-------'     @24      COLNUM   //
              'NUMBER BLANKS-------'     @24      NBLANK   /
              'NUMBER NON BLANK----'     @24      NNONBL   //
              'UNIQUES----'              @24      UNIQUES   //
              'LOWEST VALUE--------'     @24      LOWVAL   /
              'HIGHEST VALUE-------'     @24      HIGHVAL  //
              'SHORTEST LENGTH-----'     @24      SHORTEST /
              'LONGEST LENGTH------'     @24      LONGEST  //
              'RANGE---------------'     @24      RANGE    //
              'INTERQTILE LTR RANGE'     @24      P25LOC ' - ' P75LOC    //
              'INTERQTILE RANGE----'     @24      qtr1st ' - ' qtr3rd    //
              'MODE LETTER---------'     @24      MODELOC $3. ' Value ' MODE  //
              'MEDIAN--------------'     @24      MEDIAN         //
              'MODE----------------'     @24      mode ' Count ' modecnt //
              'COVERAGE % ALPHA-NUM'     @24      COVERAGE //
              'EACH * REPRESENTS---'     @24      REP ' OCCURANCES ' /
        ;

        DO I = 1 TO 37;

           IF OUTVAR{I} NE 0 THEN DO;

              NSTARS = ROUND (OUTVAR{I} / 10**(SCALE - 2), 1 );

              STARS = REPEAT('*', NSTARS);

              CALL VNAME (OUTVAR{I} , VRNAME );

              VRNAME = SUBSTR( VRNAME, 2);

              PUT VRNAME ' ! ' STARS  @110 OUTVAR{I} ;

           END;

        END;

        PUT _PAGE_;

   RUN;


%END; /* failed macro arguments */

%MEND _vdo_UNICHR;

 ***   *   *  *****  *      *   *  ****
*   *  *   *    *    *      *   *  *   *
*   *  *   *    *    *       * *   *   *
*   *  *   *    *    *        *    ****
*   *  *   *    *    *        *    * *
*   *  *   *    *    *        *    *  *
 ***    ***     *    *****    *    *   *

#! OUTLYR ;


%macro _vdo_outlyr(lib=&libname,mem=&data);

  /*
    %let libname =sashelp;
    %let data=bweight;
    %let var=weight;
    %let uin=&libname..&data;
  */

 %LOCAL
         UIN
         RC
         UI
         UDSID
         UOBS
         UVARS
         ULBL
         VARNAM
         VARTYPE
         VARLABEL
         VARLEN
         OBS
         EXPECTED OUTLIERS
         OBS
         NOBS
         DOTHEREST
;

 %let uin=&lib..&mem;

 %put &=uin;

 %put %sysfunc(ifc(%sysevalf(%superq(uin)=,boolean),**** Please Provide SAS dataset ****,));

 %if %sysfunc(ifc(%sysevalf(%superq(uin)=,boolean),1,0)) eq 0 %then %do;

    %local nobs cutoff expected_outliers dotherest;

    * grubs test is better but it is too computationally expensive
      It recomputes thowing ot the worst outlier;

    *  I do have a couple of grub test macros in R and SAS;

    * arguments have already been checked by the call driver;

    * estimate the best cuttoff in terms of the sigma on standardized values;

    * number of obs in source data;
    proc sql noprint; select count(*) into :nobs from &uin ;quit;

    %put &=nobs;

    * get the best cutoff in terms of N * sigma and the expected outliers;
    data _null;
      * (e^x)^2) -> sqrt(log(x));
      cutoff=sqrt(log(&nobs));
      call symputx('cutoff',cutoff);
      put cutoff=;
      expected_outliers=2 * &nobs *(1-cdf('normal',cutoff));
      call symputx('expected_outliers',expected_outliers);
      put expected_outliers=;
    run;quit;

    %LET UDSID = %SYSFUNC( OPEN ( &uin, I ) );

   /*-------------------------------------*\
   ! GET THE NUMBER OF COLUMNS FOR LOOP    !
   \*-------------------------------------*/

   %LET UOBS = %SYSFUNC(ATTRN(&UDSID,NLOBS));

   %LET UVARS = %SYSFUNC(ATTRN(&UDSID,NVARS));

   %LET ULBL = %SYSFUNC(ATTRC(&UDSID,LABEL));

   %LET UMAX = 0;

   %DO UI = 1 %TO &UVARS;


     /*-------------------------------------*\
     !  GET ATTRIBUTES                       !
     \*-------------------------------------*/

     /* %let ui=1;  */

     %LET UVARNAM = %SYSFUNC ( VARNAME  ( &UDSID, &UI  ) );
     %LET UVARTYP = %SYSFUNC ( VARTYPE  ( &UDSID, &UI  ) );
     %LET UVARLBL = %SYSFUNC ( VARLABEL ( &UDSID, &UI  ) );
     %LET UVARLEN = %SYSFUNC ( VARLEN   ( &UDSID, &UI  ) );

     %IF &UVARTYP EQ %QUPCASE(N) %THEN %DO;

       %let var=&UVARNAM;

       * robust regression - only interested in outliers - leverage values might also be interesting;

     

       /*
            %let uin=tst;
           %let var=hgba3;
          %let cutoff=3;
          %let expected_outliers=3;
          %let uin=work.s28_f31dem;
       */

       proc datasets lib=work nolist nodetails;
         delete __vvdag
       run;quit;

       data __vvmis;
          set &uin(where=(not missing(&var)));
       run;quit;

       ods exclude all;
       ods output diagnostics=__vvdag;
       proc robustreg data=__vvmis /*_vvad1*/ method=MM;
       model &var = /diagnostics  cutoff=&cutoff /* stadardized sigma &cutoff * sigma */;
       run;
       ods select all;
       ods listing;

       * number of potential outliers;
       proc sql noprint;select count(*) into :obs from __vvdag;quit;

       * do we have more than the expected outliers;
       %let dotherest=%sysfunc(ifn(&obs > &expected_outliers,1,0));

       %put &=dotherest;

           * set up for sort by abs value;
           data __vvdagabs;
            set __vvdag;
            rresidual=abs(rresidual);
           run;quit;

           /* sort by abs value so we can remove the lower expected_outliers */
           proc sort data=__vvdagabs out=__vvdagsrt noequals;
           by rresidual;
           run;quit;

           /* drop the lower values */
           data __vvdagsel;
            set __vvdagsrt(firstobs=%sysfunc(int(&expected_outliers))); * remove expected outliers;
           run;quit;

           * go back to full data and get orginal value;
           * get bad values;
           data __vvdagget;
             do until (dne);
               set __vvdagsel(drop=outlier) end=dne;
               rec=obs;
               set __vvmis(keep=&var)  point=rec;
               output;
             end;
             stop;
           run;quit;

           proc sort data=__vvdagget  out=__vvdagfin noequals;
           by descending rresidual;
           run;quit;

           title1 ' ';title2 ' ';title3 ' ' ;
           TITLE4 "&var 10 worst outliers up out of &obs outliers (expected_outliers=&expected_outliers)";
           TITLE5 "Robust Regression with &cutoff * sigma cuttoff and removal of expected outliers?";

           proc print data=__vvdagfin(obs=10 rename=rresidual=sigmas) width=min;
           run;quit;
           title;
   proc datasets lib=work nolist;
   delete __vvdag:;
   run;quit;
   %end;
  %end;
%end;

%mend _vdo_outlyr;

/*

%let libname =work;
%let data=tst;

%_vdo_outlyr(lib=work,mem=s28_f31dem);

*/



****   ****   *****  *   *  *****  ****
 *  *  *   *    *    *   *  *      *   *
 *  *  *   *    *    *   *  *      *   *
 *  *  ****     *    *   *  ****   ****
 *  *  * *      *    *   *  *      * *
 *  *  *  *     *     * *   *      *  *
****   *   *  *****    *    *****  *   *

#! DRIVER ;


%macro utlvdoc
    (
    libname        = work    /* libname of input dataset */

    ,data          = tstdat  /* name of input dataset */

    ,key           = 0       /* 0 or variable */

    ,univar        = 0       /* 0 or variable */

    ,UniPlot       = 0       /* 'true' enables ('false' disables) plot option on univariate output */

    ,ExtrmVal      = 100     /* Levesl for switch to top extrmval and botton extrmval */

    ,misspat       = 0       /* 0 or 1 patterns of missings */

    ,chart         = 0       /* 0 or 1 proc chart for all variables with 100 or fewer levels */

    ,taball        = 0       /* 0 or 1 all pairwise crosstabs */

    ,tabone        = 0       /* 0 or variable name          */

    ,mispop        = 0       /* 0 or 1                      */

    ,mispoptbl     = 0       /* 0 or 1                      */

    ,maxmin        = 0       /* 0 or 1                      */

    ,unichr        = 0       /* 0 or 1                      */

    ,outlier       = 0       /* 0 or 1                      */

    ,dupcol        = 0       /* 0 or 1                      */

    ,unqtwo        = 0       /* 0 or ZIP_CLASS STATECODE STATENAME   */

    ,optlength     = 0

    ,vdocor        = 0       /* 0 or 1                      */

    ,oneone        = 0       /* 0 or 1                      */

    ,cramer        = 0       /* 0 or 1                      */

    ,printto       = output  /* file or output if output window */

    ,Cleanup       = 0       /* 0 or 1 */

    ) / minoperator  des = "Validate and verify your data";


    %let valctoff=&ExtrmVal;

    * Data step to generate SQL statements;
    * ..."vv" stands for "verify + validate";

    *--------------------

    proc optsave out=sasuser.optsave;
    run;quit;

    %local
        nobs
        nvar

        exit

        pagesize
        linesize


        MaxFreq

        doOnes    /* Boolean, enables 'Ones' variable processing if necessary */

        MaxLen
        Width
    ;


   %* Initialize internal macrovariables;

   %let title=&data;
   %*let ExtrmVal = 10;     * Number of high/low frequency values to show;
   %let MaxFreq  = 999999; * SAS system maximum number of freq levels in an dataset;
   %let PageSize = 56;     * Optimal pagesize setting (may parameterize later);
   %let LineSize = 183;    * Optimal linesize setting (may parameterize later);
   %let exit     = 0;      * Exit flag value;
   %let FreqOrdr = freq;   * display order of frequency output - see PROC FREQ for details ;

   %let outchk= %eval(%direxist(%utl_getstm(&printto.))=0) ; /* 0 if output or dir exists*/

   %put &=outchk;

   proc optsave out=sasuser.optsave;
   run;quit;

    /*
    %let libname  = work     ;
    %let data     = tstdatchr;
    */


           proc sql;
              create
                view _vvtable as
              select
                *
              from
                sashelp.vtable
              where
                    libname= %upcase( "&libname" )
                and memname eq %upcase ( "&data" )
           ;quit;

           proc sql;
              create
                view _vvcolumn as
              select
                *
              from
                sashelp.vcolumn
              where
                    libname= %upcase( "&libname" )
                and memname eq %upcase ( "&data" )
           ;quit;


           %nvar(libname=&libname,data=&data);

           data _null_;
             set _vvnumchr;
             * need at least one numeric and one char;
             prb=min(num_numeric,num_character) = 0;
             call symputx( 'no_numchr', putn( prb, format, width ));
           run;quit;

           %if &no_numchr %then
               %do;
               %put ERROR{}  Not at least one character or numeric variable;
               %put ERROR{}  Not at least one character or numeric variable;
               %put ERROR{}  Not at least one character or numeric variable;
               %goto finish;
           %end;

           * because it taks so long to query the SAS dictionaries using a datastep
             lets use a view to get the data;

           * if output is a file then direct output to file;
           %if %qupcase(&printto) ne OUTPUT %then %do;
              proc printto print="&printto" new;
              run;quit;
           %end;

           %_vdo_basic;      /* Orginal program cardinality and frequencies */

           %if &unqtwo. ne 0 %then %do;  /* one variable crossed with all others */
              %_vdo_macnam(UNIQUEx2WAY);
              %_vdo_unqtwo(unqtwo=&unqtwo);
           %end;

           /* deprecated
           %If %Upcase(&codedecode) ne 0  %Then %Do;
              %_vdo_cdedec;     checks code/decode mappings but variables pairs have to have names like VAR and VRA_CD
           %end;
           */

           %If %Upcase(&maxmin)  ne 0  %Then %Do;
              %_vdo_macnam(MAXMIN);
              %_vdo_getmaxmin;  /* max and min listing */
           %end;

           %If %Upcase(&misspat)  ne 0  %Then %Do;
              %_vdo_macnam(MISSaPAT);
              %_vdo_misspat(dat=&libname..&data);  /* missing patterns */
           %end;

           %If %Upcase(&unichr)  ne 0  %Then %Do;
              %_vdo_macnam(UNICHAR);
              %_vdo_unichr;  /* proc univariate on char vars */
           %end;

           %If %Upcase(&outlier)  ne 0  %Then %Do;
              %_vdo_macnam(OUTLIER);
              %_vdo_outlyr(lib=&libname,mem=&data);  /* robustreg outlier analysis */
           %end;

           %If %Upcase(&optlength)  ne 0  %Then %Do;
              %_vdo_macnam(OPTLENGTH);
              %_vdo_optlen;     /* optimum lengths */
           %end;

           %If %Upcase(&Key) ne 0 %Then %Do;
              %_vdo_macnam(PRIMARYaKEY);
               %_vdo_keyunq(key=&key);  /* is the key unique - print dups */
           %end;

           %if %upcase(&chart) ne 0 %then %do; /* proc chart variables */
              %_vdo_macnam(CHART);

               %let xeqcnt=0;

               %utlfkil(&pth/cmd.sas);
               %let pth=%sysfunc(pathname(work));
               %put &pth;

               options ls=171;
               ods listing;
               data _null_;
                file "&pth/cmd.sas";
                set _vv1m(where=(values>0));
                if substr(left(reverse(variable)),1,1) not in ("2","3","4","5","6","7","8","9")  then do;
                  lbl=cats("'",label,"'");
                  cmd=catx(',',cats('%_vdo_chartx(',variable),type,put(values,best.),lbl,cats(format,');'));
                  put cmd;
                  putlog cmd;
                end;
               run;

               %include "&pth/cmd.sas";

           %end;


           %if %upcase(&mispoptbl) ne 0 %then %do; /* one to one -- many to one -- many to many */
               %_vdo_macnam(MISaPOPaTBL);
                %_vdo_mispoptbl ;run;         /* missing and populated */
           %end;


           %if %upcase(&mispop) ne 0 %then %do; /* one to one -- many to one -- many to many */
               %_vdo_macnam(MISaPOP);
                %_vdo_mispop ;run;         /* missing and populated */
           %end;

           %if %upcase(&dupcol) ne 0 %then %do; /* one to one -- many to one -- many to many */
              %_vdo_macnam(DUPaCOL);
              %_vdo_dupcol(typ=char);    /* duplicate character columns */
              %_vdo_dupcol(typ=num);     /* duplicate numeric columns */
           %end;

           %if %upcase(&vdocor) ne 0 %then %do; /* one to one -- many to one -- many to many */
              %_vdo_macnam(NUMaCORR);
              %_vdo_cor;
           %end;

           %if %upcase(&oneone) ne 0 %then %do; /* one to one -- many to one -- many to many */
              %_vdo_macnam(1aTOa1);
              %_vdo_mnymny(
                 maxval=2000
                ,maxvar=100
               );
           %end;

           %if &tabone ne 0 %then %do;  /* one variable crossed with all others */
              %_vdo_macnam(TABaONE);
              %_vdo_tabone(
                  maxval=2000
                 ,maxvar=100
                 ,top=50
                 );
           %end;

           %if %upcase(&cramer) ne 0 %then %do;  /* one variable crossed with all others */
              %_vdo_macnam(CRAMER);
              %_vdo_cmh(
                  maxval=2000
                 ,maxvar=30
                 );
           %end;

           %if %upcase(&taball) ne 0 %then %do;   /* all pairwise cross tables with limits */
              %_vdo_macnam(TABaLIST);
              %_vdo_taball(
                    maxval=2000 /* max number of levels */
                   ,maxvar=30  /* max number of variables 10 10!/(8! * 2!)=45 */
                   ,top=60  /* number of most frequent to print */
                   ,taball=&taball
                  );
              ods select all;
              ods listing;
           %end;

           %_vdo_macnam(CONTENTS);

           %_vdo_begmidend;  /* first 20 obs - middle 20 obs and last 20 obs */
           %_vdo_clean;      /* cleanup the work directory */

           * direct output back to the output window;
           %if %qupcase(&printto) ne OUTPUT %then %do;
              proc printto;
              run;quit;
           %end;

    %finish:

    proc optload data=sasuser.optsave;
    run;quit;

    %utl_close;

%mend utlvdoc;




%macro offcall(dummy)/des="Turn off examples";

/*                              _
  _____  ____ _ _ __ ___  _ __ | | ___  ___
 / _ \ \/ / _` | `_ ` _ \| `_ \| |/ _ \/ __|
|  __/>  < (_| | | | | | | |_) | |  __/\__ \
 \___/_/\_\__,_|_| |_| |_| .__/|_|\___||___/
                         |_|
*

%let libname= work   ;
%let data=zipcode ;

* add a date;
data zipcode;
  retain fake_onevalue "A" date 0 fake_samevalue 23 fake_allmissing ''  fake_somemissing . fake_misswithconstant '';
  format date date9.;
  set sashelp.zipcode;
  date=_n_+ 19000;
  if _n_>22000 then fake_somemissing=_n_;
  if _n_>40000 then fake_misswithconstant='A';
run;quit;

* RUN THIS FIRST TO GET INFOR FOR A SECOND RUN;

%*utlvdoc
    (
    libname        = work         /* libname of input dataset */
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
    ,cramer        = 0            /* 0 or cramer V variable crossed with all others */
    ,optlength     = 0            /* 0 optimum length for character and numeric variables */
    ,maxmin        = 0            /* 0 or max min for every varuiable */
    ,unichr        = 0            /* 0 univariate analysis of character variiables */
    ,outlier       = 0            /* 0 robust regression determination of outliers */
    ,printto       = c:\txt\vdo\&data..txt  /* save the voluminous output */
    ,Cleanup       = 0
    );

%*utlvdoc
    (
    libname        = work         /* libname of input dataset */
    ,data          = zipcode      /* name of input dataset */
    ,key           = zip          /* 0 or variable */
    ,ExtrmVal      = 10           /* display top and bottom 10 frequencies */
    ,UniPlot       = 1            /* 0 or univariate plots    */
    ,UniVar        = 1            /* 0 or univariate analysis *
    ,misspat       = 1            /* 0 or 1 missing patterns */
    ,chart         = 1            /* 0 or 1 line printer chart */
    ,taball        = AREACODES DST STATECODE STATENAME ZIP_CLASS STATE Y COUNTY /* variable 0 */
    ,tabone        = STATECODE    /* 0 or  variable vs all other variables          */
    ,mispop        = 1            /* 0 or 1  missing vs populated*/
    ,mispoptbl     = 1            /* 0 or 1  missing vs populated*/
    ,dupcol        = 1            /* 0 or 1  columns duplicated  */
    ,unqtwo        = AREACODES DST STATECODE STATENAME ZIP_CLASS STATE Y COUNTY COUNTYNM   /* 0 */
    ,vdocor        = 1            /* 0 or 1  correlation of numeric variables */
    ,oneone        = 1            /* 0 or 1  one to one - one to many - many to many */
    ,cramer        = 1            /* 0 or 1  association of character variables    */
    ,optlength     = 1            /* 0 optimum length for character and numeric variables */
    ,maxmin        = 1            /* 0 or max min for every varuiable */
    ,unichr        = 1            /* 0 univariate analysis of character variiables */
    ,outlier       = 1            /* 0 robust regression determination of outliers */
    ,printto       = d:\txt\vdo\&data..txt        /* file or output if output window */
    ,Cleanup       = 0            /* 0 or 1 delete intermediate datasets */
    );


* RUN THIS FIRST;

%*utlvdoc
    (
    libname        = sashelp         /* libname of input dataset */
    ,data          = zipcode      /* name of input dataset */
    ,key           = 0            /* 0 or variable */
    ,ExtrmVal      = 10           /* display top and bottom 30 frequencies */
    ,UniPlot       = 0            /* 0 or univariate plots    */
    ,UniVar        = 0            /* 0 or univariate analysis */
    ,chart         = 0            /* 0 or proc chart horizontal histograme */
    ,misspat       = 0            /* 0 or 1 missing patterns */
    ,taball        = 0            /* 0 or 1 line printer chart */
    ,tabone        = 0            /* 0 or all pairwise cross tabs with limits */
    ,mispop        = 0            /* 0 0 negative positive or missing on each variable */
    ,mispoptbl     = 0            /* 0 missing populated table */
    ,dupcol        = 0            /* 0 do two columns have the same values in all rows */
    ,unqtwo        = 0            /* 0 only use to find primary key unique leveels of compund keys */
    ,vdocor        = 0            /* 0 or all pairwise parametric and non parametric collolations */
    ,oneone        = 0            /* 0 or 1:1  1:many many:many */
    ,cramer        = 0            /* 0 or cramer V variable crossed with all others */
    ,optlength     = 0            /* 0 optimum length for character and numeric variables */
    ,maxmin        = 0            /* 0 or max min for every varuiable */
    ,unichr        = 0            /* 0 univariate analysis of character variiables */
    ,outlier       = 0            /* 0 robust regression determination of outliers */
    ,printto       = c:\vdo\&data..txt  /* save the voluminous output */
    ,Cleanup       = 0
    );

* THEN FILL IN AND RUN THIS;

%*utlvdoc
    (
    libname        = work         /* libname of input dataset */
    ,data          = zipcode      /* name of input dataset */
    ,key           = zip          /* 0 or variable */
    ,ExtrmVal      = 10           /* display top and bottom 10 frequencies */
    ,UniPlot       = 1            /* 0 or univariate plots    */
    ,UniVar        = 1            /* 0 or univariate analysis *
    ,misspat       = 1            /* 0 or 1 missing patterns */
    ,chart         = 1            /* 0 or 1 line printer chart */
    ,taball        = AREACODES DST STATECODE STATENAME ZIP_CLASS STATE Y COUNTY /* variable 0 */
    ,tabone        = STATECODE    /* 0 or  variable vs all other variables          */
    ,mispop        = 1            /* 0 or 1  missing vs populated*/
    ,mispoptbl     = 1            /* 0 or 1  missing vs populated*/
    ,dupcol        = 1            /* 0 or 1  columns duplicated  */
    ,unqtwo        = AREACODES DST STATECODE STATENAME ZIP_CLASS STATE Y COUNTY COUNTYNM   /* 0 */
    ,vdocor        = 1            /* 0 or 1  correlation of numeric variables */
    ,oneone        = 1            /* 0 or 1  one to one - one to many - many to many */
    ,cramer        = 1            /* 0 or 1  association of character variables    */
    ,optlength     = 1            /* 0 optimum length for character and numeric variables */
    ,maxmin        = 1            /* 0 or max min for every varuiable */
    ,unichr        = 1            /* 0 univariate analysis of character variiables */
    ,outlier       = 1            /* 0 robust regression determination of outliers */
    ,printto       = d:\txt\vdo\&data..txt        /* file or output if output window */
    ,Cleanup       = 0            /* 0 or 1 delete intermediate datasets */
    );

%*utlvdoc
    (
    libname        = sashelp         /* libname of input dataset */
    ,data          = cars         /* name of input dataset */
    ,key           = 0          /* 0 or variable */
    ,ExtrmVal      = 10           /* display top and bottom 10 frequencies */
    ,UniPlot       = 0            /* 0 or univariate plots    */
    ,UniVar        = 0            /* 0 or univariate analysis *
    ,misspat       = 0            /* 0 or 1 missing patterns */
    ,chart         = 0            /* 0 or 1 line printer chart */
    ,taball        = 0
    ,tabone        = 0
    ,mispop        = 0            /* 0 or 1  missing vs populated*/
    ,mispoptbl     = 0            /* 0 or 1  missing vs populated*/
    ,dupcol        = 0            /* 0 or 1  columns duplicated  */
    ,unqtwo        = 0
    ,vdocor        = 0            /* 0 or 1  correlation of numeric variables */
    ,oneone        = 0            /* 0 or 1  one to one - one to many - many to many */
    ,cramer        = 0            /* 0 or 1  association of character variables    */
    ,optlength     = 0            /* 0 optimum length for character and numeric variables */
    ,maxmin        = 0            /* 0 or max min for every varuiable */
    ,unichr        = 0            /* 0 univariate analysis of character variiables */
    ,outlier       = 1            /* 0 robust regression determination of outliers */
    ,printto       = d:\txt\vdo\&data..txt        /* file or output if output window */
    ,Cleanup       = 0            /* 0 or 1 delete intermediate datasets */
    );




%mend offcall;
