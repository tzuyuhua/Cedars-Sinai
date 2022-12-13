Assignment3
================
Tzu Yu Huang
2022-12-08

## 1. Setup and read in the data.

``` r
# load the required package
library(stringr)
library(dplyr)
library(lubridate)
library(xlsx)    

# read in the file as "CCU" and inspect, understand the dataset 
CCU <- read.csv('Assignment3/202210-EDW-CCU_assessment.csv')
CCU$X <- NULL
```

## 2. Data Analysis: first entry and first exit of ICU

My general strategy is to identify rows in the dataset where the first
ICU admission happened, and then the the rows where the ICU discharge
happened, then use these two columns to calculate the length of stay
(LOS).

Because we need to evaluate the chronological order of both admission
and discharge of ICU, I start by identifying all rows of admission and
discharge of ICU through text mining and “ifelse” statement, so that we
can compare them in the future.

For entering of the ICU “ENTER_ICU”, there are two possible
scenarios. 1. Patients being directly admitted to the ICU per encounter
2. Patients being transferred to the ICU per encounter

``` r
CCU$ENTER_ICU <- 
  ifelse(
    # directly admitted to the ICU
    CCU$ADT_EVNT_NM == "ADMISSION" & grepl("ICU", CCU$FROM_DEPT, fixed = TRUE), CCU$ADT_DTTM_hidden,
    
    # being transferred to the ICU from non-ICU department
      ifelse(CCU$ADT_EVNT_NM == "TRANSFER OUT" &  grepl("ICU", CCU$FROM_DEPT, fixed = TRUE) == FALSE &  grepl("ICU", CCU$TO_DEPT, fixed = TRUE) , CCU$ADT_DTTM_hidden, NA)
         )

head(CCU)
```

    ##   MRN_hidden ENC_NO_hidden  ADT_EVNT_NM           FROM_DEPT ADT_DTTM_hidden
    ## 1     203470   29204845127 TRANSFER OUT                7-OR   5/19/22 11:54
    ## 2     203470   29204845127 TRANSFER OUT             4N-CICU   5/22/22 13:25
    ## 3     219133   29202551651 TRANSFER OUT ASAP EMERGENCY DEPT   3/18/22 22:12
    ## 4     219133   29202551651 TRANSFER OUT             4N-CICU   3/19/22 22:04
    ## 5     255606   29200807911 TRANSFER OUT ASAP EMERGENCY DEPT   1/28/22 14:40
    ## 6     255606   29200807911 TRANSFER OUT             4N-CICU   1/31/22 12:18
    ##   TO_DEPT     ENTER_ICU
    ## 1 4N-CICU 5/19/22 11:54
    ## 2    6-SE          <NA>
    ## 3 4N-CICU 3/18/22 22:12
    ## 4    6-SE          <NA>
    ## 5 4N-CICU 1/28/22 14:40
    ## 6 3N-UNIV          <NA>

Now we get the information on every time the patient entered the ICU.

Next, I tried to get the information on every time the patient left the
ICU. Three possible scenarios. 1. Transfer from an ICU department to an
non-ICU department 2. Discharge from an ICU department 3. Still in the
ICU after the date “10/3/2022”

``` r
# The first two scenarios
CCU$OUT_ICU <- ifelse(
  # being transferred out
  CCU$ADT_EVNT_NM == "TRANSFER OUT" & grepl("ICU", CCU$FROM_DEPT, fixed = TRUE) & grepl("ICU", CCU$TO_DEPT, fixed = TRUE)== FALSE, CCU$ADT_DTTM_hidden, 
    # discharge from ICU
    ifelse(CCU$ADT_EVNT_NM == "DISCHARGE" &  grepl("ICU", CCU$FROM_DEPT, fixed = TRUE) , CCU$ADT_DTTM_hidden,  NA )
                      )

# Third scenario, still in the ICU after “10/3/2022"
stillinICU <- CCU %>% 
  group_by(ENC_NO_hidden) %>%
  filter(
    # identified by the last record per encounter indicating still in the ICU, and earlier than “10/3/2022"
    row_number()==n() &  grepl("ICU", TO_DEPT, fixed = TRUE) &  strptime( ADT_DTTM_hidden, format="%m/%d/%y %H:%M" ) < strptime( "10/3/22", format="%m/%d/%y" )
         )  %>% ungroup()

# Making the date "ADT_DTTM_hidden" of the third scenario the same as "OUT_ICU" in the main dataset "CCU"
CCU$OUT_ICU[match(stillinICU$ADT_DTTM_hidden, CCU$ADT_DTTM_hidden)] <- CCU$ADT_DTTM_hidden[match(stillinICU$ADT_DTTM_hidden, CCU$ADT_DTTM_hidden)]

head(CCU)
```

    ##   MRN_hidden ENC_NO_hidden  ADT_EVNT_NM           FROM_DEPT ADT_DTTM_hidden
    ## 1     203470   29204845127 TRANSFER OUT                7-OR   5/19/22 11:54
    ## 2     203470   29204845127 TRANSFER OUT             4N-CICU   5/22/22 13:25
    ## 3     219133   29202551651 TRANSFER OUT ASAP EMERGENCY DEPT   3/18/22 22:12
    ## 4     219133   29202551651 TRANSFER OUT             4N-CICU   3/19/22 22:04
    ## 5     255606   29200807911 TRANSFER OUT ASAP EMERGENCY DEPT   1/28/22 14:40
    ## 6     255606   29200807911 TRANSFER OUT             4N-CICU   1/31/22 12:18
    ##   TO_DEPT     ENTER_ICU       OUT_ICU
    ## 1 4N-CICU 5/19/22 11:54          <NA>
    ## 2    6-SE          <NA> 5/22/22 13:25
    ## 3 4N-CICU 3/18/22 22:12          <NA>
    ## 4    6-SE          <NA> 3/19/22 22:04
    ## 5 4N-CICU 1/28/22 14:40          <NA>
    ## 6 3N-UNIV          <NA> 1/31/22 12:18

Now we’ve identified the record every time a patient entered and left
the ICU. Since we are only interested in the first admission and
discharge per encounter, I’ll acquire those information by calculating
the earliest (minimum) date per encounter.

``` r
CCU <- CCU %>% 
  group_by(ENC_NO_hidden) %>%
  mutate(
    FIRST_ICU_ADMISSION = min(strptime(ENTER_ICU, format="%m/%d/%y %H:%M"), na.rm = T), 
    FIRST_ICU_DISCHARGE = min(strptime(OUT_ICU, format="%m/%d/%y %H:%M"), na.rm = T)
  ) %>%
  ungroup()
```

## 3. Data Analysis: re-entery of ICU

Now that “FIRST_ICU_ADMISSION” is being establish, but for
“FIRST_ICU_DISCHARGE”, I need to consider the possibility of reentering
the ICU within an hour.

``` r
# calculate the time difference between every time a patient entered the ICU "ENTER_ICU" and the earliest time they left the ICU "FIRST_ICU_DISCHARGE".
diff <- difftime(  strptime( CCU$ENTER_ICU, format="%m/%d/%y %H:%M" ), CCU$FIRST_ICU_DISCHARGE, units = "hours")

#diff >0 indicates reenter after the first discharge, diff <1 indicates the reenter is within 1 hour of the first discharge.
CCU$more <- ifelse( diff < 1 & diff >0, 1, 0 )

# record encounter identifier that has "more" equals 1 (indicating enter the ICU at least the second time)
list_more <- CCU[CCU$more == 1 & !is.na(CCU$more), ]$ENC_NO_hidden %>% unique()

# calculate the second earliest discharge date for those recorded in "list_more" 
returnICU <- 
  CCU[CCU$ENC_NO_hidden %in% list_more, ] %>%
  filter( strptime( OUT_ICU, format="%m/%d/%y %H:%M" ) != FIRST_ICU_DISCHARGE ) %>%
  group_by(ENC_NO_hidden) %>%
  mutate( 
    SECOND_ICU_DISCHARGE = min(strptime( OUT_ICU, format="%m/%d/%y %H:%M" ), na.rm = T)
  ) %>%
  ungroup()

# replacing "FIRST_ICU_DISCHARGE" from the main dataset "CCU" with the acquired "SECOND_ICU_DISCHARGE" from dataset "returnICU". Encounter identifier "ENC_NO_hidden" used as leverage.
CCU[CCU$ENC_NO_hidden %in% returnICU$ENC_NO_hidden, ]$FIRST_ICU_DISCHARGE <- returnICU$SECOND_ICU_DISCHARGE[match(CCU[CCU$ENC_NO_hidden %in% list_more, ]$ENC_NO_hidden, returnICU$ENC_NO_hidden )]
```

Now that the first discharged date has been updated to include patients
that reenter the ICU within an hour at most once. I wish to repeat the
process until all first discharged date has been updated, regardless of
times of reentering with an hour.

``` r
diff2 <- difftime(  strptime( CCU$ENTER_ICU, format="%m/%d/%y %H:%M" ), CCU$FIRST_ICU_DISCHARGE, units = "hours")
CCU$more2 <- ifelse( diff2 < 1 & diff2 >0, 1, 0 )
sum(CCU$more2 == 1, na.rm = T)
```

    ## [1] 3

Since there are only three encounters left, I’ll just update those
manually.

``` r
CCU[CCU$ENC_NO_hidden %in% CCU[CCU$more2 == 1 & !is.na(CCU$more2), ]$ENC_NO_hidden, ]
```

    ## # A tibble: 18 × 12
    ##    MRN_hidden ENC_NO_hidden ADT_EVNT_NM  FROM_…¹ ADT_D…² TO_DEPT ENTER…³ OUT_ICU
    ##         <int>         <dbl> <chr>        <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1  200440415   29195678735 TRANSFER OUT 6-SE    9/17/2… 4N-CICU 9/17/2… <NA>   
    ##  2  200440415   29195678735 TRANSFER OUT 4N-CICU 9/17/2… CARDIA… <NA>    9/17/2…
    ##  3  200440415   29195678735 TRANSFER OUT CARDIA… 9/17/2… 4N-CICU 9/17/2… <NA>   
    ##  4  200440415   29195678735 TRANSFER OUT 4N-CICU 9/18/2… CARDIA… <NA>    9/18/2…
    ##  5  200440415   29195678735 TRANSFER OUT CARDIA… 9/18/2… 4N-CICU 9/18/2… <NA>   
    ##  6  200440415   29195678735 TRANSFER OUT 4N-CICU 9/20/2… 5-NE    <NA>    9/20/2…
    ##  7  200851151   29190725868 TRANSFER OUT ASAP E… 4/22/2… 4N-CICU 4/22/2… <NA>   
    ##  8  200851151   29190725868 TRANSFER OUT 4N-CICU 4/23/2… CARDIA… <NA>    4/23/2…
    ##  9  200851151   29190725868 TRANSFER OUT CARDIA… 4/23/2… 4N-CICU 4/23/2… <NA>   
    ## 10  200851151   29190725868 TRANSFER OUT 4N-CICU 4/23/2… CARDIA… <NA>    4/23/2…
    ## 11  200851151   29190725868 TRANSFER OUT CARDIA… 4/23/2… 4N-CICU 4/23/2… <NA>   
    ## 12  200851151   29190725868 TRANSFER OUT 4N-CICU 4/23/2… CARDIA… <NA>    4/23/2…
    ## 13  202160695   29188010087 TRANSFER OUT 6-NE    3/21/2… 4N-CICU 3/21/2… <NA>   
    ## 14  202160695   29188010087 TRANSFER OUT 4N-CICU 3/21/2… CARDIA… <NA>    3/21/2…
    ## 15  202160695   29188010087 TRANSFER OUT CARDIA… 3/21/2… 4N-CICU 3/21/2… <NA>   
    ## 16  202160695   29188010087 TRANSFER OUT 4N-CICU 3/21/2… AHSP IR <NA>    3/21/2…
    ## 17  202160695   29188010087 TRANSFER OUT AHSP IR 3/21/2… 4N-CICU 3/21/2… <NA>   
    ## 18  202160695   29188010087 TRANSFER OUT 4N-CICU 3/26/2… 6-NE    <NA>    3/26/2…
    ## # … with 4 more variables: FIRST_ICU_ADMISSION <dttm>,
    ## #   FIRST_ICU_DISCHARGE <dttm>, more <dbl>, more2 <dbl>, and abbreviated
    ## #   variable names ¹​FROM_DEPT, ²​ADT_DTTM_hidden, ³​ENTER_ICU

``` r
CCU[CCU$ENC_NO_hidden == 29195678735, ]$FIRST_ICU_DISCHARGE <- strptime( "9/20/21 16:35", format="%m/%d/%y %H:%M" )
CCU[CCU$ENC_NO_hidden == 29190725868, ]$FIRST_ICU_DISCHARGE <- strptime( "4/23/21 9:36", format="%m/%d/%y %H:%M" )
CCU[CCU$ENC_NO_hidden == 29188010087, ]$FIRST_ICU_DISCHARGE <- strptime( "3/26/21 14:14", format="%m/%d/%y %H:%M" )
```

## 4. Condense and Organize output dataset

After the data analysis, we have all the original data and the two
required new variables, next I’ll remove the unnecessary data and
calculate the length of stay in terms of hours.

``` r
output<- unique(CCU[, c("MRN_hidden", "ENC_NO_hidden", "FIRST_ICU_ADMISSION", "FIRST_ICU_DISCHARGE")])

output$FIRST_ICU_LOS <- difftime( output$FIRST_ICU_DISCHARGE, output$FIRST_ICU_ADMISSION, units = "hours") %>% floor() %>% as.numeric()

colnames(output)[1:2] <- c("MRN", "ENC_NO")
```

Double check if my results agree with those in file “Template”.

``` r
output[output$ENC_NO %in% c(29204845127, 29188820095, 29192933522), ]
```

    ## # A tibble: 3 × 5
    ##        MRN      ENC_NO FIRST_ICU_ADMISSION FIRST_ICU_DISCHARGE FIRST_ICU_LOS
    ##      <int>       <dbl> <dttm>              <dttm>                      <dbl>
    ## 1   203470 29204845127 2022-05-19 11:54:00 2022-05-22 13:25:00            73
    ## 2 51455895 29188820095 2021-03-04 21:55:00 2021-03-05 21:24:00            23
    ## 3 51455895 29192933522 2021-09-27 08:42:00 2021-09-27 14:47:00             6

It matches. Write a xlsx file with the output file.

``` r
write.xlsx(output, 'Assignment3_Result.xlsx')
```

A few things I noticed is that, interestingly, some encounters seems to
be missing the required data. For example, the first ICU admission
cannot be established with encounter number 29203421031.
