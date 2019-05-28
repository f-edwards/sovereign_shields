/*-------------------------------------------------------------------------*
 |                                                                         
 |            STATA SUPPLEMENTAL SYNTAX FILE FOR ICPSR 04439
 |                CENSUS OF TRIBAL JUSTICE AGENCIES, 2002
 |
 |
 | This Stata missing value recode program is provided for optional use with
 | the Stata system version of this data file as distributed by ICPSR.
 | The program replaces user-defined numeric missing values (e.g., -9)
 | with generic system missing "."  Note that Stata allows you to specify
 | up to 27 unique missing value codes.  Only variables with user-defined
 | missing values are included in this program.
 |
 | To apply the missing value recodes, users need to first open the
 | Stata data file on their system, apply the missing value recodes if
 | desired, then save a new copy of the data file with the missing values
 | applied.  Users are strongly advised to use a different filename when
 | saving the new file.
 |
 *------------------------------------------------------------------------*/

replace A2 = . if (A2 == 9)
replace A3_1 = . if (A3_1 == 9)
replace A3_2 = . if (A3_2 == 9)
replace A3_3 = . if (A3_3 == 9)
replace A3_4 = . if (A3_4 == 9)
replace A3_5 = . if (A3_5 == 9)
replace A4 = . if (A4 == 9)
replace A5 = . if (A5 == 9)
replace A6 = . if (A6 == 9)
replace A10_1 = . if (A10_1 == 9)
replace A10_2 = . if (A10_2 == 9)
replace A10_3 = . if (A10_3 == 9)
replace A10_4 = . if (A10_4 == 9)
replace A10_5 = . if (A10_5 == 9)
replace A11 = . if (A11 == 9)
replace A12A = . if (A12A == 9)
replace A13_1 = . if (A13_1 == 9)
replace A13_2 = . if (A13_2 == 9)
replace A13_3 = . if (A13_3 == 9)
replace A13_4 = . if (A13_4 == 9)
replace A14 = . if (A14 == 9)
replace A15_1 = . if (A15_1 == 9)
replace A15_2 = . if (A15_2 == 9)
replace A15_3 = . if (A15_3 == 9)
replace A16 = . if (A16 == 9)
replace A18 = . if (A18 == 9)
replace A20_1 = . if (A20_1 == 9)
replace A20_2 = . if (A20_2 == 9)
replace A20_3 = . if (A20_3 == 9)
replace A20_4 = . if (A20_4 == 9)
replace A20_5 = . if (A20_5 == 9)
replace A20_6 = . if (A20_6 == 9)
replace A20_7 = . if (A20_7 == 9)
replace B1 = . if (B1 == 9)
replace B6_1 = . if (B6_1 == 9)
replace B6_2 = . if (B6_2 == 9)
replace B6_3 = . if (B6_3 == 9)
replace B6_4 = . if (B6_4 == 9)
replace B6_5 = . if (B6_5 == 9)
replace B6_6 = . if (B6_6 == 9)
replace B6_7 = . if (B6_7 == 9)
replace B6_8 = . if (B6_8 == 9)
replace B6_9 = . if (B6_9 == 9)
replace B7_1 = . if (B7_1 == 9)
replace B7_2 = . if (B7_2 == 9)
replace B7_3 = . if (B7_3 == 9)
replace B7_4 = . if (B7_4 == 9)
replace B7_5 = . if (B7_5 == 9)
replace B7_6 = . if (B7_6 == 9)
replace B7_7 = . if (B7_7 == 9)
replace B7_8 = . if (B7_8 == 9)
replace B7_9 = . if (B7_9 == 9)
replace B8 = . if (B8 == 9)
replace B16 = . if (B16 == 9)
replace B18_1 = . if (B18_1 == 9)
replace B18_2 = . if (B18_2 == 9)
replace B18_3 = . if (B18_3 == 9)
replace B18_4 = . if (B18_4 == 9)
replace B19_1 = . if (B19_1 == 9)
replace B19_2 = . if (B19_2 == 9)
replace B19_3 = . if (B19_3 == 9)
replace B19_4 = . if (B19_4 == 9)
replace B20A = . if (B20A == 9)
replace B21A = . if (B21A == 9)
replace B22A = . if (B22A == 9)
replace B23A = . if (B23A == 9)
replace B24 = . if (B24 == 9)
replace B25 = . if (B25 == 9)
replace B26 = . if (B26 == 9)
replace C1 = . if (C1 == 9)
replace C2 = . if (C2 == 9)
replace C3 = . if (C3 == 9)
replace C4 = . if (C4 == 9)
replace C5_1 = . if (C5_1 == 9)
replace C5_2 = . if (C5_2 == 9)
replace C5_3 = . if (C5_3 == 9)
replace C5_4 = . if (C5_4 == 9)
replace C5_5 = . if (C5_5 == 9)


