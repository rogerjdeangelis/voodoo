/* Adapted from oto_voodoo.sas (rogerjdeangelis/voodoo) -- the %_vdo_begmidend
   sub-macro from the VOODOO table-profiling library. This macro prints a
   PROC CONTENTS report plus a representative sample of observations (all
   rows if <=60, else first/middle/last 20 if more) for the dataset named
   by &libname..&data. -- exactly as the macro is written upstream.
*/

%let libname = sashelp;
%let data    = class;

%_vdo_begmidend;
