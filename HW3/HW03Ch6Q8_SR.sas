/*
 This file is auto-generated by the statrep package.
 Do not edit this file or your changes will be lost.
 Edit the LaTeX file instead.
 
 See the statrep package documentation and the file
 statrep.cfg for information on these settings.
 */
 
 
%include "./HW03Ch6Q8_SR_preamble.sas" /nosource;
/* Remove all output files. */
%cleandir(., tex, tex);
%cleandir(., png, png);
%cleandir(., lst, lst);


/* Start program with a null title. */
title;

data Weld;
input Gauge $ Time $ Strength @@;
datalines;
 G1 T1 10 G1 T1 12 G1 T2 13 G1 T2 17 G1 T3 21 G1 T3 30
 G1 T4 18 G1 T4 16 G1 T5 17 G1 T5 21 G2 T1 15 G2 T1 19
 G2 T2 14 G2 T2 12 G2 T3 30 G2 T3 38 G2 T4 15 G2 T4 11
 G2 T5 14 G2 T5 12 G3 T1 10 G3 T1 8  G3 T2 12 G3 T2 9
 G3 T3 19 G3 T3 5  G3 T4 14 G3 T4 15 G3 T5 19 G3 T5 11
;
run;

proc glm data = weld;
class gauge time;
model strength = gauge time gauge*time;
contrast 'linear gauge contrast' gauge -1 0 1;
run;

