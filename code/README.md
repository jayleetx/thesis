# code

This folder contains scripts that perform the analyses for the thesis. To save knitting time, these processes were moved out to scripts that don't need to be re-executed every time.

Three of the scripts must be run in order: [A_areal_interpolation.R](A_areal_interpolation.R)
, [B_precinct_math.R](B_precinct_math.R), and [C_regression.R](C_regression.R). The results from A and B are called directly by the thesis document, but the results from C must be updated manually in the regression summaries in chapter 3.
