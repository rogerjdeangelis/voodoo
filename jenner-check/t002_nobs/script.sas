/* Adapted from oto_voodoo.sas (rogerjdeangelis/voodoo) -- the %nobs sub-macro
   used throughout the VOODOO library to get a row count for a two-level SAS
   table reference via PROC SQL COUNT(*). Exercised here against sashelp.cars.
*/

%nobs(libname=sashelp, data=cars);
%put NOTE: sashelp.cars has &nobs. observations.;
