# seqSHP R package

The library 'seqSHP' is intended for data from the Swiss Household Panel (SHP). It provides function `seqFromWaves()` that extracts sequence data from the successive SHP wave files together with covariates from other SHP files. Function `seqFromWaves()` returns the extracted data in a tibble. A `getColumnIndex()` function retrieves the column indexes of a given protoname in the tibble.

See the help page of function `seqFromWaves()', which provides examples.

SHP data are available for free from FORS <https://forscenter.ch/projects/swiss-household-panel/data/>
