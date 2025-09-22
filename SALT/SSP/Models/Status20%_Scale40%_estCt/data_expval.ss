#V3.30.24.00-prerel;_safe;_compile_date:_Jul  8 2025;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.2
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:_https://groups.google.com/g/ss3-forum_and_NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:_https://nmfs-ost.github.io/ss3-website/
#_Source_code_at:_https://github.com/nmfs-ost/ss3-source-code

#_Start_time: Tue Sep  9 15:20:22 2025
#_expected_values
#C should work with SS version:
#C file created using an r4ss function
#C file write time: 2025-09-09  13:28:07
#V3.30.24.00-prerel;_safe;_compile_date:_Jul  8 2025;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.2
1892 #_StartYr
2020 #_EndYr
1 #_Nseas
 12 #_months/season
2 #_Nsubseasons (even number, minimum is 2)
1 #_spawn_month
2 #_Nsexes: 1, 2, -1  (use -1 for 1 sex setup with SSB multiplied by female_frac parameter)
55 #_Nages=accumulator age, first age is always age 0
1 #_Nareas
3 #_Nfleets (including surveys)
#_fleet_type: 1=catch fleet; 2=bycatch only fleet; 3=survey; 4=predator(M2) 
#_sample_timing: -1 for fishing fleet to use season-long catch-at-age for observations, or 1 to use observation month;  (always 1 for surveys)
#_fleet_area:  area the fleet/survey operates in 
#_units of catch:  1=bio; 2=num (ignored for surveys; their units read later)
#_catch_mult: 0=no; 1=yes
#_rows are fleets
#_fleet_type fishery_timing area catch_units need_catch_mult fleetname
 1 -1 1 1 1 Commercial  # 1
 1 -1 1 1 1 Recreational  # 2
 3 1 1 1 0 RSS  # 3
#Bycatch_fleet_input_goes_next
#a:  fleet index
#b:  1=include dead bycatch in total dead catch for F0.1 and MSY optimizations and forecast ABC; 2=omit from total catch for these purposes (but still include the mortality)
#c:  1=Fmult scales with other fleets; 2=bycatch F constant at input value; 3=bycatch F from range of years
#d:  F or first year of range
#e:  last year of range
#f:  not used
# a   b   c   d   e   f 
#_Catch data: year, seas, fleet, catch, catch_se
#_catch_se:  standard error of log(catch)
#_NOTE:  catch data is ignored for survey fleets
-999 1 1 1.93875e-18 0.01
1892 1 1 0.0021678 0.01
1893 1 1 0.0021678 0.01
1894 1 1 0.0021678 0.01
1895 1 1 0.000558326 0.01
1896 1 1 0.000133257 0.01
1897 1 1 0.000137719 0.01
1898 1 1 7.66183e-05 0.01
1899 1 1 0.000130909 0.01
1900 1 1 0.00018514 0.01
1901 1 1 0.000236771 0.01
1902 1 1 0.000291002 0.01
1903 1 1 0.000345233 0.01
1904 1 1 0.000399463 0.01
1905 1 1 0.000451094 0.01
1906 1 1 0.000505325 0.01
1907 1 1 0.000559556 0.01
1908 1 1 0.000611187 0.01
1909 1 1 0.000665418 0.01
1910 1 1 0.000719649 0.01
1911 1 1 0.000773879 0.01
1912 1 1 0.00082551 0.01
1913 1 1 0.000879741 0.01
1914 1 1 0.000933972 0.01
1915 1 1 0.000988203 0.01
1916 1 1 0.00103983 0.01
1917 1 1 0.00109406 0.01
1918 1 1 0.00114829 0.01
1919 1 1 0.00119993 0.01
1920 1 1 0.00125416 0.01
1921 1 1 0.00130839 0.01
1922 1 1 0.00136262 0.01
1923 1 1 0.00141425 0.01
1924 1 1 0.00146848 0.01
1925 1 1 0.00152271 0.01
1926 1 1 0.00157434 0.01
1927 1 1 0.0011765 0.01
1928 1 1 0.00196337 0.01
1929 1 1 0.420953 0.01
1930 1 1 0.755672 0.01
1931 1 1 0.362314 0.01
1932 1 1 0.0007831 0.01
1933 1 1 0.0732793 0.01
1934 1 1 0.113098 0.01
1935 1 1 0.00124938 0.01
1936 1 1 0.428189 0.01
1937 1 1 1.40168 0.01
1938 1 1 1.63946 0.01
1939 1 1 1.98027 0.01
1940 1 1 2.35385 0.01
1941 1 1 1.5758 0.01
1942 1 1 1.89813 0.01
1943 1 1 2.15218 0.01
1944 1 1 2.97364 0.01
1945 1 1 3.35221 0.01
1946 1 1 3.62133 0.01
1947 1 1 1.19397 0.01
1948 1 1 2.44138 0.01
1949 1 1 2.60903 0.01
1950 1 1 0.943282 0.01
1951 1 1 0.845781 0.01
1952 1 1 1.68356 0.01
1953 1 1 0.570297 0.01
1954 1 1 0.376404 0.01
1955 1 1 1.08219 0.01
1956 1 1 0.529917 0.01
1957 1 1 1.13304 0.01
1958 1 1 0.111197 0.01
1959 1 1 0.357225 0.01
1960 1 1 0.455499 0.01
1961 1 1 0.852941 0.01
1962 1 1 0.472336 0.01
1963 1 1 0.81996 0.01
1964 1 1 0.468083 0.01
1965 1 1 2.36784 0.01
1966 1 1 1.48786 0.01
1967 1 1 4.24416 0.01
1968 1 1 4.03724 0.01
1969 1 1 7.87101 0.01
1970 1 1 3.69162 0.01
1971 1 1 8.36355 0.01
1972 1 1 10.8246 0.01
1973 1 1 11.7552 0.01
1974 1 1 15.0205 0.01
1975 1 1 7.77317 0.01
1976 1 1 10.3955 0.01
1977 1 1 14.603 0.01
1978 1 1 15.3142 0.01
1979 1 1 10.0365 0.01
1980 1 1 10.6347 0.01
1981 1 1 5.69576 0.01
1982 1 1 6.43539 0.01
1983 1 1 7.86192 0.01
1984 1 1 7.29459 0.01
1985 1 1 11.1157 0.01
1986 1 1 13.5211 0.01
1987 1 1 12.5442 0.01
1988 1 1 13.1724 0.01
1989 1 1 12.9984 0.01
1990 1 1 14.1698 0.01
1991 1 1 4.69205 0.01
1992 1 1 5.60374 0.01
1993 1 1 18.1128 0.01
1994 1 1 5.29968 0.01
1995 1 1 2.32551 0.01
1996 1 1 7.05294 0.01
1997 1 1 5.92404 0.01
1998 1 1 6.13551 0.01
1999 1 1 1.87459 0.01
2000 1 1 3.88993 1000
2001 1 1 6.25728 1000
2002 1 1 2.70691 1000
2003 1 1 2.86719 1000
2004 1 1 2.29789 1000
2005 1 1 2.18401 1000
2006 1 1 3.1549 1000
2007 1 1 2.67903 1000
2008 1 1 5.19466 1000
2009 1 1 5.31154 1000
2010 1 1 2.13606 1000
2011 1 1 3.84837 1000
2012 1 1 3.6348 1000
2013 1 1 4.46053 1000
2014 1 1 2.96817 1000
2015 1 1 1.90972 1000
2016 1 1 2.63029 1000
2017 1 1 4.251 1000
2018 1 1 4.02115 1000
2019 1 1 5.02956 1000
2020 1 1 3.97306 1000
-999 1 2 1.8023e-18 0.01
1892 1 2 0 0.01
1893 1 2 0 0.01
1894 1 2 0 0.01
1895 1 2 0 0.01
1896 1 2 0 0.01
1897 1 2 0 0.01
1898 1 2 0 0.01
1899 1 2 0 0.01
1900 1 2 0 0.01
1901 1 2 0 0.01
1902 1 2 0 0.01
1903 1 2 0 0.01
1904 1 2 0 0.01
1905 1 2 0 0.01
1906 1 2 0 0.01
1907 1 2 0 0.01
1908 1 2 0 0.01
1909 1 2 0 0.01
1910 1 2 0 0.01
1911 1 2 0 0.01
1912 1 2 0 0.01
1913 1 2 0 0.01
1914 1 2 0 0.01
1915 1 2 0 0.01
1916 1 2 0 0.01
1917 1 2 0 0.01
1918 1 2 0 0.01
1919 1 2 0 0.01
1920 1 2 0 0.01
1921 1 2 0 0.01
1922 1 2 0 0.01
1923 1 2 0 0.01
1924 1 2 0 0.01
1925 1 2 0 0.01
1926 1 2 0 0.01
1927 1 2 0 0.01
1928 1 2 0 0.01
1929 1 2 0 0.01
1930 1 2 0 0.01
1931 1 2 0 0.01
1932 1 2 0 0.01
1933 1 2 0 0.01
1934 1 2 0 0.01
1935 1 2 0 0.01
1936 1 2 0 0.01
1937 1 2 0 0.01
1938 1 2 0 0.01
1939 1 2 0 0.01
1940 1 2 0 0.01
1941 1 2 0 0.01
1942 1 2 0 0.01
1943 1 2 0 0.01
1944 1 2 0 0.01
1945 1 2 0 0.01
1946 1 2 0 0.01
1947 1 2 0 0.01
1948 1 2 0 0.01
1949 1 2 0 0.01
1950 1 2 0 0.01
1951 1 2 0 0.01
1952 1 2 0 0.01
1953 1 2 0 0.01
1954 1 2 0 0.01
1955 1 2 0 0.01
1956 1 2 0 0.01
1957 1 2 0 0.01
1958 1 2 0 0.01
1959 1 2 0 0.01
1960 1 2 0 0.01
1961 1 2 0 0.01
1962 1 2 0 0.01
1963 1 2 0 0.01
1964 1 2 0 0.01
1965 1 2 0 0.01
1966 1 2 0 0.01
1967 1 2 0 0.01
1968 1 2 0 0.01
1969 1 2 0 0.01
1970 1 2 0 0.01
1971 1 2 0 0.01
1972 1 2 0 0.01
1973 1 2 0 0.01
1974 1 2 0 0.01
1975 1 2 0 0.01
1976 1 2 0 0.01
1977 1 2 0 0.01
1978 1 2 0 0.01
1979 1 2 0.339478 0.01
1980 1 2 0.547978 0.01
1981 1 2 1.90735 0.01
1982 1 2 2.31886 0.01
1983 1 2 0.973812 0.01
1984 1 2 1.74844 0.01
1985 1 2 0.732642 0.01
1986 1 2 3.65753 0.01
1987 1 2 0.138683 0.01
1988 1 2 1.44927 0.01
1989 1 2 7.18712 0.01
1990 1 2 5.97135 0.01
1991 1 2 2.71215 0.01
1992 1 2 5.79796 0.01
1993 1 2 14.9357 0.01
1994 1 2 5.3479 0.01
1995 1 2 2.5942 0.01
1996 1 2 2.7001 0.01
1997 1 2 4.6426 0.01
1998 1 2 7.3491 0.01
1999 1 2 1.80793 0.01
2000 1 2 2.97878 1000
2001 1 2 3.71768 1000
2002 1 2 3.6846 1000
2003 1 2 4.83311 1000
2004 1 2 4.01719 1000
2005 1 2 6.9686 1000
2006 1 2 6.2313 1000
2007 1 2 7.8735 1000
2008 1 2 6.50032 1000
2009 1 2 4.57701 1000
2010 1 2 5.48836 1000
2011 1 2 7.00941 1000
2012 1 2 10.5113 1000
2013 1 2 7.24291 1000
2014 1 2 4.53605 1000
2015 1 2 5.34495 1000
2016 1 2 4.23833 1000
2017 1 2 10.1067 1000
2018 1 2 10.5683 1000
2019 1 2 10.6292 1000
2020 1 2 9.46401 1000
-9999 0 0 0 0
#
#_CPUE_and_surveyabundance_and_index_observations
#_units: 0=numbers; 1=biomass; 2=F; 30=spawnbio; 31=exp(recdev); 36=recdev; 32=spawnbio*recdev; 33=recruitment; 34=depletion(&see Qsetup); 35=parm_dev(&see Qsetup)
#_errtype:  -1=normal; 0=lognormal; 1=lognormal with bias correction; >1=df for T-dist
#_SD_report: 0=not; 1=include survey expected value with se
#_note that link functions are specified in Q_setup section of control file
#_dataunits = 36 and 35 should use Q_type 5 to provide offset parameter
#_fleet units errtype SD_report
1 1 0 0 # Commercial
2 1 0 0 # Recreational
3 34 0 0 # RSS
#_year month index obs err
1892 1 3 1 1e-05 #_orig_obs: 1 RSS
2020 1 3 0.2 1e-05 #_orig_obs: 0.2 RSS
-9999 1 1 1 1 # terminator for survey observations 
#
0 #_N_fleets_with_discard
#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)
#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal; -3 for trunc normal with CV
# note: only enter units and errtype for fleets with discard 
# note: discard data is the total for an entire season, so input of month here must be to a month in that season
#_fleet units errtype
# -9999 0 0 0.0 0.0 # terminator for discard data 
#
0 #_use meanbodysize_data (0/1)
#_COND_0 #_DF_for_meanbodysize_T-distribution_like
# note:  type=1 for mean length; type=2 for mean body weight 
#_year month fleet part type obs stderr
#  -9999 0 0 0 0 0 0 # terminator for mean body size data 
#
# set up population length bin structure (note - irrelevant if not using size data and using empirical wtatage
2 # length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector
2 # binwidth for population size comp 
2 # minimum size in the population (lower edge of first bin and size at age 0.00) 
74 # maximum size in the population (lower edge of last bin) 
1 # use length composition data (0/1/2) where 2 invokes new comp_comtrol format
#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.
#_addtocomp:  after accumulation of tails; this value added to all bins
#_combM+F: males and females treated as combined sex below this bin number 
#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation
#_Comp_Error:  0=multinomial, 1=dirichlet using Theta*n, 2=dirichlet using beta, 3=MV_Tweedie
#_ParmSelect:  consecutive index for dirichlet or MV_Tweedie
#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001
#
#_Using old format for composition controls
#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize
-1 0.001 0 0 0 0 0.001 #_fleet:1_Commercial
-1 0.001 0 0 0 0 0.001 #_fleet:2_Recreational
-1 0.001 0 0 0 0 0.001 #_fleet:3_RSS
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sex*length distribution
# partition codes:  (0=combined; 1=discard; 2=retained
37 #_N_LengthBins
 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74
#_year month fleet sex part Nsamp datavector(female-male)
-9999 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
#
55 #_N_age_bins
 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54
1 #_N_ageerror_definitions
 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001
#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.
#_addtocomp:  after accumulation of tails; this value added to all bins
#_combM+F: males and females treated as combined sex below this bin number 
#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation
#_Comp_Error:  0=multinomial, 1=dirichlet using Theta*n, 2=dirichlet using beta, 3=MV_Tweedie
#_ParmSelect:  parm number for dirichlet or MV_Tweedie
#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001
#
#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize
-1 0.001 0 0 0 0 0.001 #_fleet:1_Commercial
-1 0.001 0 0 0 0 0.001 #_fleet:2_Recreational
-1 0.001 0 0 0 0 0.001 #_fleet:3_RSS
3 #_Lbin_method_for_Age_Data: 1=poplenbins; 2=datalenbins; 3=lengths
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sex*length distribution
# partition codes:  (0=combined; 1=discard; 2=retained
#_year month fleet sex part ageerr Lbin_lo Lbin_hi Nsamp datavector(female-male)
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
0 #_Use_MeanSize-at-Age_obs (0/1)
#
0 #_N_environ_variables
# -2 in year will subtract mean for that env_var; -1 will subtract mean and divide by stddev (e.g. Z-score)
#_year variable value
#
# Sizefreq data. Defined by method because a fleet can use multiple methods
0 # N sizefreq methods to read (or -1 for expanded options)
#
0 # do tags (0/1)
#
0 #    morphcomp data(0/1) 
#  Nobs, Nmorphs, mincomp
#_year, seas, type, partition, Nsamp, datavector_by_Nmorphs
#
0  #  Do dataread for selectivity priors(0/1)
#_year, seas, fleet, age/size, bin, selex_prior, prior_sd
# feature not yet implemented
#
999

