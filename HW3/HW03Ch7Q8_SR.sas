/*
 This file is auto-generated by the statrep package.
 Do not edit this file or your changes will be lost.
 Edit the LaTeX file instead.
 
 See the statrep package documentation and the file
 statrep.cfg for information on these settings.
 */
 
 
%include "./HW03Ch7Q8_SR_preamble.sas" /nosource;
/* Remove all output files. */
%cleandir(., tex, tex);
%cleandir(., png, png);
%cleandir(., lst, lst);


/* Start program with a null title. */
title;

data PaperTowelStrength;
input TreatCombo $ amount $ brand $ type $ strength order @@;
datalines;
 111 1 1 1 3279.0  3  111 1 1 1 4330.7 15  111 1 1 1 3843.7 16
 112 1 1 2 3260.8 11  112 1 1 2 3134.2 20  112 1 1 2 3206.7 22
 121 1 2 1 2889.6  5  121 1 2 1 3019.5  6  121 1 2 1 2451.5 21
 122 1 2 2 2323.0  1  122 1 2 2 2603.6  2  122 1 2 2 2893.8 14
 211 2 1 1 2964.5  4  211 2 1 1 4067.8 10  211 2 1 1 3327.0 18
 212 2 1 2 3114.2 12  212 2 1 2 3009.3 13  212 2 1 2 3242.0 19
 221 2 2 1 2883.4  9  221 2 2 1 2581.4 23  221 2 2 1 2385.9 24
 222 2 2 2 2142.3  7  222 2 2 2 2364.9  8  222 2 2 2 2189.9 17
;
run;

proc glm data = papertowelstrength;
class amount brand;
model strength = amount brand amount*brand;
run;

proc glm data = papertowelstrength;
class amount type;
model strength = amount type amount*type;
run;

proc glm data = papertowelstrength;
class brand type;
model strength = brand type brand*type;
run;

proc reg data=papertowelstrength;
model strength = amount brand type AB;
run;
