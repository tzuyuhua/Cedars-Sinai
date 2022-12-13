Assignment1
================
Tzu Yu Huang
2022-12-08

## 1. Set up and read in the data

``` r
library(dplyr)
library(lmtest)
master <- read.csv('Assignment1/master_data.csv')
lab <- read.csv('Assignment1/t016.csv')
adv <- read.csv('Assignment1/t05xc.csv')
```

## 2. Take a look and organize the data.

According to the instructions, I’ll acquire the patients treatment
assignment and region information from dataset “master”, and potassium
value information from the datasets “lab” and “adv”.

There are too many variables in the master dataset, so I’ll just extract
those that are needed.

``` r
mas <- master[, c("master_id", "treat", "region")]
```

Take a look at how many patients are there and how many different region
and treatment assignments.

``` r
length(mas$master_id)
```

    ## [1] 3445

``` r
length(unique(mas$master_id))
```

    ## [1] 3445

``` r
table(mas$region)
```

    ## 
    ##    1    2 
    ## 1767 1678

``` r
table(mas$treat)
```

    ## 
    ##    0    1 
    ## 1723 1722

So we can see that there are two treatment groups, two regions, and no
duplicated master_id in dataset “mas”.

``` r
head(lab)
```

    ##   master_id sub_init visit labs_yn   labs_dt labs_dose na_unit  na k_unit   k
    ## 1  10450015      CWD  BASE       1 8/17/2006         0       2 144      2 4.4
    ## 2  16690019      MMH  BASE       1 8/17/2006         0       1 140      1 4.1
    ## 3  16690019      MMH  WK04       1  9/7/2006         0       1 140      1 4.0
    ## 4  16690019      MMH  WK04       1 9/14/2006         1       1 142      1 4.1
    ## 5  10450015      CWD  WK04       1 9/13/2006         1       2 143      2 4.8
    ## 6  10450015      CWD  WK04       1  9/7/2006         0       2 145      2 4.6
    ##   cl_unit  cl co2_unit co2 bun_unit bun cr_unit  cr chng_dose formstat_id
    ## 1       2 104        2  30        1  35       1 1.6         0        9727
    ## 2       1 103        1  28        1  13       1 0.8         0        9744
    ## 3       1 104        1  27        1  12       1 0.8         1       10293
    ## 4       1 106        1  27        1  13       1 0.8         0       10304
    ## 5       2 105        2  29        1  30       1 1.5         0       10317
    ## 6       2 105        2  29        1  38       1 1.5         1       10318
    ##   destatus statusofform sigstatus
    ## 1        C            C         2
    ## 2        C            C         2
    ## 3        C            C         2
    ## 4        C            C         2
    ## 5        C            C         2
    ## 6        C            C         2

``` r
head(adv)
```

    ##   form        test_type test_result test_units master_id sae_report sae_num
    ## 1 T05X       cREATININE         2.0      MG/DL  10010025          1       2
    ## 2 T05X        POTASSIUM         4.9      MEQ/l  10010025          1       2
    ## 3 T05X       cREATININE         2.0      MG/DL  10010025          2       2
    ## 4 T05X        POTASSIUM         4.9      MEQ/l  10010025          2       2
    ## 5 T05X Serum creatinine         0.8      mg/dl  10010051          2       1
    ## 6 T05X  Serum potassium         4.5      meq/l  10010051          2       1
    ##     onset_dt   labdate test_res  cr   k aemonth aeday aeyr     aedate
    ## 1 04/25/2007 4/25/2007      2.0 2.0  NA       4    25 2007 04/25/2007
    ## 2 04/25/2007 4/25/2007      4.9  NA 4.9       4    25 2007 04/25/2007
    ## 3 04/25/2007 4/25/2007      2.0 2.0  NA       4    25 2007 04/25/2007
    ## 4 04/25/2007 4/25/2007      4.9  NA 4.9       4    25 2007 04/25/2007
    ## 5 03/05/2008  3/5/2008      0.8 0.8  NA       3     5 2008 03/05/2008
    ## 6 03/05/2008  3/5/2008      4.5  NA 4.5       3     5 2008 03/05/2008

## 3. Discover patients that have experienced Hyperkalemia before (defined as any potassium value \>=5.5)

First acquire a list from dataset “lab”

``` r
lab_list <- lab[lab$k >= 5.5 & !is.na(lab$k), ]$master_id
```

Next from dataset “adv”

``` r
adv_list <- adv[adv$k >= 5.5 & !is.na(adv$k), ]$master_id
```

Then combine the list to get patients that have experienced hyperkalemia
before.

``` r
list <- c(lab_list, adv_list) %>% unique()
```

Combine these information with the dataset “mas”. “hyperk” = 1 indicates
this patient has experienced hyperkalemia before.

``` r
mas$hyperk <- ifelse(mas$master_id %in% list, 1, 0 ) 
```

Now that we have a dataset that has information about treatment,
hyperkalemia, region, and patient identifier. And No NA value or
unusually small or large value being observed.

``` r
summary(mas)
```

    ##    master_id            treat            region          hyperk     
    ##  Min.   :10010013   Min.   :0.0000   Min.   :1.000   Min.   :0.000  
    ##  1st Qu.:17580512   1st Qu.:0.0000   1st Qu.:1.000   1st Qu.:0.000  
    ##  Median :31170345   Median :0.0000   Median :1.000   Median :0.000  
    ##  Mean   :27397096   Mean   :0.4999   Mean   :1.487   Mean   :0.139  
    ##  3rd Qu.:32010665   3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.:0.000  
    ##  Max.   :65220289   Max.   :1.0000   Max.   :2.000   Max.   :1.000

## 4. Answer the questions

Since the outcome of interest for this question is binary by nature
(whether or not a patient has ever experienced hyperkalemia), I will
implement a logistic regression model for the following questions.

## a) Is there evidence that the drug is associated with hyperkalemia?

The first model I fit only have treatment assignmnet as independent
variable.

``` r
mas$treat <- as.factor(mas$treat)
mas$region <- as.factor(mas$region)
logit <- glm(hyperk ~ treat, data = mas, family = "binomial" (link = 'logit'))
summary(logit)
```

    ## 
    ## Call:
    ## glm(formula = hyperk ~ treat, family = binomial(link = "logit"), 
    ##     data = mas)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.6434  -0.6434  -0.4371  -0.4371   2.1889  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.30003    0.08371  -27.48  < 2e-16 ***
    ## treat1       0.83036    0.10406    7.98 1.47e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2778.2  on 3444  degrees of freedom
    ## Residual deviance: 2710.9  on 3443  degrees of freedom
    ## AIC: 2714.9
    ## 
    ## Number of Fisher Scoring iterations: 5

And yes, there is evidence that the drug is associated with
hyperkalemia, the treatment is statistically significantly associated
with hyperkalemia (p value = 1.47e-15).

Next, I fit another model while controlling for the main effect of
geographic region.

``` r
reglogit <- glm(hyperk ~ treat + region, data = mas, family = "binomial" (link = 'logit'))
summary(reglogit)
```

    ## 
    ## Call:
    ## glm(formula = hyperk ~ treat + region, family = binomial(link = "logit"), 
    ##     data = mas)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.7181  -0.5583  -0.4899  -0.3757   2.3179  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.05953    0.09242 -22.285  < 2e-16 ***
    ## treat1       0.83576    0.10450   7.998 1.27e-15 ***
    ## region2     -0.55626    0.10240  -5.432 5.57e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2778.2  on 3444  degrees of freedom
    ## Residual deviance: 2680.6  on 3442  degrees of freedom
    ## AIC: 2686.6
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
lrtest(reglogit, logit)
```

    ## Likelihood ratio test
    ## 
    ## Model 1: hyperk ~ treat + region
    ## Model 2: hyperk ~ treat
    ##   #Df  LogLik Df  Chisq Pr(>Chisq)    
    ## 1   3 -1340.3                         
    ## 2   2 -1355.4 -1 30.326  3.651e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The treatment is still statistically significantly associated with
hyperkalemia after adjusting for geographic region (p value = 1.27e-15).
And the new model with region also shows a statistically significant
better fit compare to model without region.

## b) Does the drug effect depend on geographic region?

To see if the effect of drug depend on geographic region, I included an
interaction term.

``` r
intlogit <- glm(hyperk ~ treat*region, data = mas, family = "binomial"(link = 'logit'))
summary(intlogit)
```

    ## 
    ## Call:
    ## glm(formula = hyperk ~ treat * region, family = binomial(link = "logit"), 
    ##     data = mas)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.7615  -0.5021  -0.4439  -0.4306   2.2020  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    -2.33165    0.11860 -19.660  < 2e-16 ***
    ## treat1          1.24204    0.14163   8.770  < 2e-16 ***
    ## region2         0.06384    0.16744   0.381    0.703    
    ## treat1:region2 -0.98170    0.21327  -4.603 4.16e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2778.2  on 3444  degrees of freedom
    ## Residual deviance: 2659.3  on 3441  degrees of freedom
    ## AIC: 2667.3
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
lrtest(intlogit, reglogit)
```

    ## Likelihood ratio test
    ## 
    ## Model 1: hyperk ~ treat * region
    ## Model 2: hyperk ~ treat + region
    ##   #Df  LogLik Df  Chisq Pr(>Chisq)    
    ## 1   4 -1329.6                         
    ## 2   3 -1340.3 -1 21.288  3.952e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
lrtest(intlogit, logit)
```

    ## Likelihood ratio test
    ## 
    ## Model 1: hyperk ~ treat * region
    ## Model 2: hyperk ~ treat
    ##   #Df  LogLik Df  Chisq Pr(>Chisq)    
    ## 1   4 -1329.6                         
    ## 2   2 -1355.4 -2 51.614  6.195e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

It appears that the effect of drug does depend on geographic region (p
value = 4.16e-06). Interestingly, geographical region itself no longer
has a statistically significant relationship with hyperkalemia (p value
= 0.703). This model with interaction term provides a lower AIC (2667.3)
and statistically better fit compare to the previous two model with and
without region (LR test: p value = 3.952e-06, 6.195e-12, respectively).
