---
title: "Paper 2, Econ 980x"
author: "Alice Chen"
date: "2022-11-27"
output:
  pdf_document: default
  html_document:
    df_print: paged
---




```r
csv <- read_csv(file = "atus_00001.csv.gz") %>%
  clean_names()
```

```
## Rows: 228455 Columns: 38
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## dbl (38): YEAR, CASEID, PERNUM, LINENO, WT06, WT20, AGE, SEX, RACE, MARST, E...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
# race 
race_file <- read_excel("race_980x.xlsx")

race_file <- race_file %>%
  rename(code1 = race) %>%
  rename(race = code) 

csv <- left_join(csv, race_file, by = "race") %>%
  rename(race_code = race) %>%
  rename(race = code1)


# education
education_file <- read_excel("Education.xlsx")
education_file <- education_file %>%
  rename(code_educ = educ) %>%
  rename(educ = code_education) %>%
  mutate(educ = as.numeric(educ))

csv <- left_join(csv, education_file, by = "educ") %>%
  rename(code_education = educ) %>%
  rename(educ = code_educ) 

# marital status
marital_file <- read_excel("marital_status.xlsx")
education_file <- marital_file %>%
  mutate(marst = as.numeric(marst))
  
csv <- left_join(csv, marital_file, by = "marst") %>%
  rename(code_marst = marst) %>%
  rename(marst = marst_code) %>%
  mutate(marst_simple = ifelse(code_marst <= 2, "Married", ifelse(code_marst >= 6, "Never Married", "Separated/Divorced")))



# age
csv <- csv %>%
  filter(age > 18)

# children
csv <- csv %>%
  mutate(child = ifelse(yngch < 19, 1, 0) )

# gender

csv <- csv %>%
  mutate(gender = ifelse(sex == 1, "male", "female")) 

# years 
```



```r
#leisure hours aggregate
csv %>%
  ggplot(mapping = aes(x = bls_leis)) +
  geom_histogram(binwidth=50) +
  labs(
  title = "Distribution of annual individual leisure hours",
  x = "Annual Hours",
  y = "Count",
  caption = "Data from ATUS",
) 
```

![](Paper2_files/figure-latex/graphs-1.pdf)<!-- --> 

```r
csv %>%
  filter(year %in% c(2007, 2009)) %>%
  ggplot(mapping = aes(x = bls_leis)) +
  geom_histogram(binwidth=50) +
  facet_wrap(~year) +
  labs(
  title = "Distribution of annual individual leisure hours",
  x = "Annual Hours",
  y = "Count",
  caption = "Data from ATUS",
)
```

![](Paper2_files/figure-latex/graphs-2.pdf)<!-- --> 

```r
#leisure hours aggregate by gender
csv %>%
  group_by(gender) %>%
  ggplot(mapping = aes(x = bls_leis) )+
  geom_histogram(binwidth=50) +
  facet_wrap(~ gender) +
    labs(
  title = "Distribution of annual individual leisure hours by gender",
  x = "Annual Hours",
  y = "Count",
  caption = "Data from ATUS",
)
```

![](Paper2_files/figure-latex/graphs-3.pdf)<!-- --> 

```r
csv %>%
  filter(year %in% c(2007, 2009)) %>%
  ggplot(mapping = aes(x = bls_leis)) +
  geom_histogram(binwidth=50) +
  facet_wrap(~ gender + year) +
  labs(
  title = "Distribution of annual individual leisure hours by gender",
  x = "Annual Hours",
  y = "Count",
  caption = "Data from ATUS",
)
```

![](Paper2_files/figure-latex/graphs-4.pdf)<!-- --> 

```r
# Median
csv %>%
  group_by(year, gender) %>%
  summarize(median(bls_leis)) %>%
  clean_names() %>%
  ggplot(aes(x = year, y = median_bls_leis)) +
  geom_line() +
  facet_wrap(~ gender) +
  labs(
  title = "Median annual individual leisure hours by gender",
  y = "Annual Hours",
  x = "Year",
  caption = "Data from ATUS",
) 
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

![](Paper2_files/figure-latex/graphs-5.pdf)<!-- --> 

```r
# All Quantile
csv %>%
  group_by(year, gender) %>%
  summarize(quantile_25 = quantile(bls_leis, 0.25), quantile_50 = quantile(bls_leis, 0.5), quantile_75 = quantile(bls_leis, 0.75)) %>%
  clean_names() %>%
  pivot_longer(quantile_25:quantile_75, names_to = "quantile", values_to = "hours") %>%
  ggplot(aes(x = year, y = hours, group = quantile, colour = quantile)) +
  geom_line() +
    facet_wrap(~ gender) +
  labs(
  title = "Annual individual leisure hours by gender",
  y = "Annual Hours",
  x = "Year",
  caption = "Data from ATUS",
) 
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

![](Paper2_files/figure-latex/graphs-6.pdf)<!-- --> 

```r
csv %>%
  group_by(year) %>%
  summarize(quantile(bls_leis, 0.25), quantile(bls_leis, 0.5), quantile(bls_leis, 0.75)) %>%
  clean_names() %>%
  pivot_longer(!year, names_to = "quantile", values_to = "hours") %>%
  ggplot(aes(x = year, y = hours, group = quantile)) +
  geom_line()
```

![](Paper2_files/figure-latex/graphs-7.pdf)<!-- --> 

```r
csv %>%
  group_by(year) %>%
  summarize(quantile(bls_leis, 0.25), quantile(bls_leis, 0.5), quantile(bls_leis, 0.75)) %>%
  clean_names() %>%
  pivot_longer(!year, names_to = "quantile", values_to = "hours") %>%
  filter(year > 2006) %>%
  filter(year < 2011) %>%
  ggplot(aes(x = year, y = hours, group = quantile)) +
  geom_line()
```

![](Paper2_files/figure-latex/graphs-8.pdf)<!-- --> 

```r
# by year, gender
csv %>%
  group_by(year, gender) %>%
  summarize(quantile_25 = quantile(bls_leis, 0.25), quantile_50 = quantile(bls_leis, 0.5), quantile_75 = quantile(bls_leis, 0.75)) %>%
  clean_names() %>%
  pivot_longer(quantile_25:quantile_75, names_to = "quantile", values_to = "hours") %>%
  filter(year > 2005) %>%
  filter(year < 2011) %>%
  ggplot(aes(x = year, y = hours, group = quantile, colour = quantile)) +
  geom_line() +
  facet_wrap(~gender) +
    labs(
  title = "Annual individual leisure hours by gender",
  y = "Annual Hours",
  x = "Year",
  caption = "Data from ATUS",
) 
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

![](Paper2_files/figure-latex/graphs-9.pdf)<!-- --> 

```r
# by year, gender
csv %>%
  group_by(year, gender) %>%
  summarize(median = median(bls_leis)) %>%
  clean_names() %>%
  filter(year > 2005) %>%
  filter(year < 2011) %>%
  ggplot(aes(x = year, y = median)) +
  geom_line() +
  facet_wrap(~gender) +
      labs(
  title = "Annual individual leisure hours by gender",
  y = "Annual Hours",
  x = "Year",
  caption = "Data from ATUS",
) 
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

![](Paper2_files/figure-latex/graphs-10.pdf)<!-- --> 

```r
#csv %>%
 # ggplot(mapping = aes(x = year, y = bls_leis)) + 
 # geom_jitter(alpha = 0.01)
```



```
## # A tibble: 216,683 x 44
##     year  caseid pernum lineno    wt06  wt20   age   sex race_~1 code_~2 code_~3
##    <dbl>   <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>
##  1  2003 2.00e13      1      1  8.16e6    NA    60     1     110       1      41
##  2  2003 2.00e13      1      1  1.74e6    NA    41     2     100       1      30
##  3  2003 2.00e13      1      1  3.83e6    NA    26     2     100       2      31
##  4  2003 2.00e13      1      1  6.62e6    NA    36     2     110       1      21
##  5  2003 2.00e13      1      1  3.07e6    NA    51     1     100       1      42
##  6  2003 2.00e13      1      1  3.46e6    NA    32     2     100       4      40
##  7  2003 2.00e13      1      1  1.64e6    NA    44     2     100       3      21
##  8  2003 2.00e13      1      1  6.57e6    NA    21     2     100       6      30
##  9  2003 2.00e13      1      1  1.53e6    NA    33     2     100       1      31
## 10  2003 2.00e13      1      1  4.28e6    NA    39     2     110       1      31
## # ... with 216,673 more rows, 33 more variables: educyrs <dbl>, fullpart <dbl>,
## #   earnweek <dbl>, eldch <dbl>, yngch <dbl>, nchild <dbl>, bls_carehh <dbl>,
## #   bls_comm <dbl>, bls_educ <dbl>, bls_food <dbl>, bls_hhact <dbl>,
## #   bls_leis <dbl>, bls_leis_arts <dbl>, bls_leis_attend <dbl>,
## #   bls_leis_attsport <dbl>, bls_leis_partsport <dbl>, bls_leis_relax <dbl>,
## #   bls_leis_soc <dbl>, bls_leis_soccom <dbl>, bls_leis_soccomex <dbl>,
## #   bls_leis_sport <dbl>, bls_leis_travel <dbl>, bls_leis_tv <dbl>, ...
```

```
## 
## Call:
## lm(formula = bls_leis ~ gender + year + age + race, data = .)
## 
## Coefficients:
##                        (Intercept)                          gendermale  
##                          -623.9103                             49.6546  
##                               year                                 age  
##                             0.3811                              3.6478  
##                     raceAsian only                      raceBlack only  
##                           -63.9278                             22.5268  
##          raceBlack-American Indian  raceHawaiian Pacific Islander only  
##                            -0.8574                            -29.6285  
##                     raceWhite only           raceWhite-American Indian  
##                           -20.1391                             -0.5591  
##                    raceWhite-Asian                     raceWhite-Black  
##                             3.2668                             13.7155  
##                 raceWhite-Hawaiian  
##                           -55.6804
```

```
## 
## Call:
## lm(formula = bls_leis ~ gender + year + age + race, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -521.35 -159.52  -31.24  135.26 1135.93 
## 
## Coefficients:
##                                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                        -623.91035  167.92625  -3.715 0.000203 ***
## gendermale                           49.65458    0.91800  54.090  < 2e-16 ***
## year                                  0.38115    0.08351   4.564 5.02e-06 ***
## age                                   3.64783    0.02732 133.540  < 2e-16 ***
## raceAsian only                      -63.92776    5.87353 -10.884  < 2e-16 ***
## raceBlack only                       22.52681    5.49110   4.102 4.09e-05 ***
## raceBlack-American Indian            -0.85744   17.09077  -0.050 0.959987    
## raceHawaiian Pacific Islander only  -29.62851   11.37959  -2.604 0.009224 ** 
## raceWhite only                      -20.13907    5.37485  -3.747 0.000179 ***
## raceWhite-American Indian            -0.55913    7.79704  -0.072 0.942833    
## raceWhite-Asian                       3.26685   12.93655   0.253 0.800633    
## raceWhite-Black                      13.71548   11.05354   1.241 0.214673    
## raceWhite-Hawaiian                  -55.68038   26.98951  -2.063 0.039110 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 211.6 on 216670 degrees of freedom
## Multiple R-squared:  0.09435,	Adjusted R-squared:  0.0943 
## F-statistic:  1881 on 12 and 216670 DF,  p-value: < 2.2e-16
```

```
## # A tibble: 216,683 x 44
##     year  caseid pernum lineno    wt06  wt20   age   sex race_~1 code_~2 code_~3
##    <dbl>   <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>
##  1  2003 2.00e13      1      1  8.16e6    NA    60     1     110       1      41
##  2  2003 2.00e13      1      1  1.74e6    NA    41     2     100       1      30
##  3  2003 2.00e13      1      1  3.83e6    NA    26     2     100       2      31
##  4  2003 2.00e13      1      1  6.62e6    NA    36     2     110       1      21
##  5  2003 2.00e13      1      1  3.07e6    NA    51     1     100       1      42
##  6  2003 2.00e13      1      1  3.46e6    NA    32     2     100       4      40
##  7  2003 2.00e13      1      1  1.64e6    NA    44     2     100       3      21
##  8  2003 2.00e13      1      1  6.57e6    NA    21     2     100       6      30
##  9  2003 2.00e13      1      1  1.53e6    NA    33     2     100       1      31
## 10  2003 2.00e13      1      1  4.28e6    NA    39     2     110       1      31
## # ... with 216,673 more rows, 33 more variables: educyrs <dbl>, fullpart <dbl>,
## #   earnweek <dbl>, eldch <dbl>, yngch <dbl>, nchild <dbl>, bls_carehh <dbl>,
## #   bls_comm <dbl>, bls_educ <dbl>, bls_food <dbl>, bls_hhact <dbl>,
## #   bls_leis <dbl>, bls_leis_arts <dbl>, bls_leis_attend <dbl>,
## #   bls_leis_attsport <dbl>, bls_leis_partsport <dbl>, bls_leis_relax <dbl>,
## #   bls_leis_soc <dbl>, bls_leis_soccom <dbl>, bls_leis_soccomex <dbl>,
## #   bls_leis_sport <dbl>, bls_leis_travel <dbl>, bls_leis_tv <dbl>, ...
```

```
## # A tibble: 216,683 x 45
##     year  caseid pernum lineno    wt06  wt20   age   sex race_~1 code_~2 code_~3
##    <dbl>   <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>
##  1  2003 2.00e13      1      1  8.16e6    NA    60     1     110       1      41
##  2  2003 2.00e13      1      1  1.74e6    NA    41     2     100       1      30
##  3  2003 2.00e13      1      1  3.83e6    NA    26     2     100       2      31
##  4  2003 2.00e13      1      1  6.62e6    NA    36     2     110       1      21
##  5  2003 2.00e13      1      1  3.07e6    NA    51     1     100       1      42
##  6  2003 2.00e13      1      1  3.46e6    NA    32     2     100       4      40
##  7  2003 2.00e13      1      1  1.64e6    NA    44     2     100       3      21
##  8  2003 2.00e13      1      1  6.57e6    NA    21     2     100       6      30
##  9  2003 2.00e13      1      1  1.53e6    NA    33     2     100       1      31
## 10  2003 2.00e13      1      1  4.28e6    NA    39     2     110       1      31
## # ... with 216,673 more rows, 34 more variables: educyrs <dbl>, fullpart <dbl>,
## #   earnweek <dbl>, eldch <dbl>, yngch <dbl>, nchild <dbl>, bls_carehh <dbl>,
## #   bls_comm <dbl>, bls_educ <dbl>, bls_food <dbl>, bls_hhact <dbl>,
## #   bls_leis <dbl>, bls_leis_arts <dbl>, bls_leis_attend <dbl>,
## #   bls_leis_attsport <dbl>, bls_leis_partsport <dbl>, bls_leis_relax <dbl>,
## #   bls_leis_soc <dbl>, bls_leis_soccom <dbl>, bls_leis_soccomex <dbl>,
## #   bls_leis_sport <dbl>, bls_leis_travel <dbl>, bls_leis_tv <dbl>, ...
```

```
## 
## Call:
## lm(formula = bls_leis ~ gender + year + age + race + new_educ, 
##     data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -540.08 -157.16  -30.31  134.72 1125.90 
## 
## Coefficients:
##                                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                        -1.465e+03  1.675e+02  -8.743  < 2e-16 ***
## gendermale                          5.018e+01  9.120e-01  55.024  < 2e-16 ***
## year                                8.183e-01  8.335e-02   9.818  < 2e-16 ***
## age                                 3.516e+00  2.727e-02 128.908  < 2e-16 ***
## raceAsian only                     -3.776e+01  5.855e+00  -6.449 1.13e-10 ***
## raceBlack only                      2.835e+01  5.455e+00   5.197 2.03e-07 ***
## raceBlack-American Indian           5.276e+00  1.698e+01   0.311   0.7560    
## raceHawaiian Pacific Islander only -1.861e+01  1.130e+01  -1.646   0.0997 .  
## raceWhite only                     -7.804e+00  5.344e+00  -1.460   0.1442    
## raceWhite-American Indian           5.349e+00  7.746e+00   0.691   0.4898    
## raceWhite-Asian                     2.130e+01  1.285e+01   1.657   0.0975 .  
## raceWhite-Black                     2.235e+01  1.098e+01   2.035   0.0418 *  
## raceWhite-Hawaiian                 -4.264e+01  2.681e+01  -1.590   0.1117    
## new_educ>Four Year Degree          -7.571e+01  1.611e+00 -46.994  < 2e-16 ***
## new_educHigh School Graduate       -3.253e+01  1.524e+00 -21.351  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 210.2 on 216668 degrees of freedom
## Multiple R-squared:  0.1065,	Adjusted R-squared:  0.1064 
## F-statistic:  1844 on 14 and 216668 DF,  p-value: < 2.2e-16
```

```
## # A tibble: 216,683 x 45
##     year  caseid pernum lineno    wt06  wt20   age   sex race_~1 code_~2 code_~3
##    <dbl>   <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>
##  1  2003 2.00e13      1      1  8.16e6    NA    60     1     110       1      41
##  2  2003 2.00e13      1      1  1.74e6    NA    41     2     100       1      30
##  3  2003 2.00e13      1      1  3.83e6    NA    26     2     100       2      31
##  4  2003 2.00e13      1      1  6.62e6    NA    36     2     110       1      21
##  5  2003 2.00e13      1      1  3.07e6    NA    51     1     100       1      42
##  6  2003 2.00e13      1      1  3.46e6    NA    32     2     100       4      40
##  7  2003 2.00e13      1      1  1.64e6    NA    44     2     100       3      21
##  8  2003 2.00e13      1      1  6.57e6    NA    21     2     100       6      30
##  9  2003 2.00e13      1      1  1.53e6    NA    33     2     100       1      31
## 10  2003 2.00e13      1      1  4.28e6    NA    39     2     110       1      31
## # ... with 216,673 more rows, 34 more variables: educyrs <dbl>, fullpart <dbl>,
## #   earnweek <dbl>, eldch <dbl>, yngch <dbl>, nchild <dbl>, bls_carehh <dbl>,
## #   bls_comm <dbl>, bls_educ <dbl>, bls_food <dbl>, bls_hhact <dbl>,
## #   bls_leis <dbl>, bls_leis_arts <dbl>, bls_leis_attend <dbl>,
## #   bls_leis_attsport <dbl>, bls_leis_partsport <dbl>, bls_leis_relax <dbl>,
## #   bls_leis_soc <dbl>, bls_leis_soccom <dbl>, bls_leis_soccomex <dbl>,
## #   bls_leis_sport <dbl>, bls_leis_travel <dbl>, bls_leis_tv <dbl>, ...
```

```
## 
## Call:
## lm(formula = bls_leis ~ gender + year + age + race + new_educ + 
##     nchild, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -539.73 -154.35  -29.01  133.50 1119.92 
## 
## Coefficients:
##                                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                        -1.166e+03  1.660e+02  -7.027 2.12e-12 ***
## gendermale                          4.745e+01  9.041e-01  52.479  < 2e-16 ***
## year                                7.034e-01  8.256e-02   8.520  < 2e-16 ***
## age                                 2.801e+00  2.914e-02  96.120  < 2e-16 ***
## raceAsian only                     -3.911e+01  5.798e+00  -6.746 1.52e-11 ***
## raceBlack only                      2.046e+01  5.404e+00   3.786 0.000153 ***
## raceBlack-American Indian           1.239e+00  1.681e+01   0.074 0.941243    
## raceHawaiian Pacific Islander only -1.644e+01  1.120e+01  -1.469 0.141879    
## raceWhite only                     -1.060e+01  5.292e+00  -2.003 0.045178 *  
## raceWhite-American Indian           1.489e-01  7.671e+00   0.019 0.984508    
## raceWhite-Asian                     1.063e+01  1.273e+01   0.835 0.403774    
## raceWhite-Black                     1.742e+01  1.087e+01   1.602 0.109231    
## raceWhite-Hawaiian                 -3.995e+01  2.655e+01  -1.505 0.132373    
## new_educ>Four Year Degree          -7.832e+01  1.596e+00 -49.076  < 2e-16 ***
## new_educHigh School Graduate       -3.773e+01  1.511e+00 -24.971  < 2e-16 ***
## nchild                             -2.819e+01  4.313e-01 -65.352  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 208.2 on 216667 degrees of freedom
## Multiple R-squared:  0.1238,	Adjusted R-squared:  0.1237 
## F-statistic:  2040 on 15 and 216667 DF,  p-value: < 2.2e-16
```

```
## 
## Call:
## lm(formula = bls_leis ~ gender + year + age + race + new_educ + 
##     nchild + marst_simple, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -559.24 -153.58  -28.34  133.35 1112.47 
## 
## Coefficients:
##                                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                        -827.70534  165.76859  -4.993 5.95e-07 ***
## gendermale                           49.21292    0.90980  54.092  < 2e-16 ***
## year                                  0.51202    0.08251   6.206 5.46e-10 ***
## age                                   3.16552    0.03357  94.297  < 2e-16 ***
## raceAsian only                      -32.06597    5.78459  -5.543 2.97e-08 ***
## raceBlack only                       16.45575    5.38889   3.054  0.00226 ** 
## raceBlack-American Indian            -2.10998   16.76170  -0.126  0.89983    
## raceHawaiian Pacific Islander only  -14.46619   11.16218  -1.296  0.19498    
## raceWhite only                       -6.52367    5.27747  -1.236  0.21641    
## raceWhite-American Indian             2.39777    7.64806   0.314  0.75389    
## raceWhite-Asian                      12.24182   12.69238   0.965  0.33480    
## raceWhite-Black                      15.28628   10.84182   1.410  0.15856    
## raceWhite-Hawaiian                  -39.51993   26.46932  -1.493  0.13543    
## new_educ>Four Year Degree           -72.84907    1.60042 -45.519  < 2e-16 ***
## new_educHigh School Graduate        -34.95019    1.50857 -23.168  < 2e-16 ***
## nchild                              -21.28270    0.47146 -45.142  < 2e-16 ***
## marst_simpleNever Married            46.52934    1.35044  34.455  < 2e-16 ***
## marst_simpleSeparated/Divorced       22.65070    1.14949  19.705  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 207.5 on 216665 degrees of freedom
## Multiple R-squared:  0.129,	Adjusted R-squared:  0.1289 
## F-statistic:  1888 on 17 and 216665 DF,  p-value: < 2.2e-16
```

```
## 
## Various Models
## =============================================================================================================================
##                                                                       Dependent variable:                                    
##                                    ------------------------------------------------------------------------------------------
##                                                                             bls_leis                                         
##                                                 (1)                           (2)                           (3)              
## -----------------------------------------------------------------------------------------------------------------------------
## gendermale                                   44.200***                     43.935***                     49.655***           
##                                               (0.958)                       (0.957)                       (0.918)            
##                                                                                                                              
## year                                                                       1.458***                       0.381***           
##                                                                             (0.087)                       (0.084)            
##                                                                                                                              
## age                                                                                                       3.648***           
##                                                                                                           (0.027)            
##                                                                                                                              
## raceAsian only                                                                                           -63.928***          
##                                                                                                           (5.874)            
##                                                                                                                              
## raceBlack only                                                                                           22.527***           
##                                                                                                           (5.491)            
##                                                                                                                              
## raceBlack-American Indian                                                                                  -0.857            
##                                                                                                           (17.091)           
##                                                                                                                              
## raceHawaiian Pacific Islander only                                                                       -29.629***          
##                                                                                                           (11.380)           
##                                                                                                                              
## raceWhite only                                                                                           -20.139***          
##                                                                                                           (5.375)            
##                                                                                                                              
## raceWhite-American Indian                                                                                  -0.559            
##                                                                                                           (7.797)            
##                                                                                                                              
## raceWhite-Asian                                                                                            3.267             
##                                                                                                           (12.937)           
##                                                                                                                              
## raceWhite-Black                                                                                            13.715            
##                                                                                                           (11.054)           
##                                                                                                                              
## raceWhite-Hawaiian                                                                                       -55.680**           
##                                                                                                           (26.990)           
##                                                                                                                              
## Constant                                    310.025***                   -2,621.242***                  -623.910***          
##                                               (0.634)                      (174.290)                     (167.926)           
##                                                                                                                              
## -----------------------------------------------------------------------------------------------------------------------------
## Observations                                  216,908                       216,908                       216,683            
## R2                                             0.010                         0.011                         0.094             
## Adjusted R2                                    0.010                         0.011                         0.094             
## Residual Std. Error                    221.309 (df = 216906)         221.166 (df = 216905)         211.630 (df = 216670)     
## F Statistic                        2,129.097*** (df = 1; 216906) 1,207.362*** (df = 2; 216905) 1,880.976*** (df = 12; 216670)
## =============================================================================================================================
## Note:                                                                                             *p<0.1; **p<0.05; ***p<0.01
```


```r
#Change

change <- newcsv1 %>%
  group_by(gender,  year, new_educ, child, marst_simple) %>%
  summarize(median(bls_leis)) %>%
  clean_names() %>%
  pivot_wider(values_from = median_bls_leis, names_from = year) %>%
  clean_names() %>%
  mutate(leisure_change = x2009 - x2007)
```

```
## `summarise()` has grouped output by 'gender', 'year', 'new_educ', 'child'. You
## can override using the `.groups` argument.
```

```r
change2 <- newcsv1 %>%
  group_by(gender, year) %>%
  summarize(median(bls_leis)) %>%
  clean_names() %>%
  pivot_wider(values_from = median_bls_leis, names_from = year) %>%
  clean_names() %>%
  mutate(leisure_change = x2009 - x2007)
```

```
## `summarise()` has grouped output by 'gender'. You can override using the
## `.groups` argument.
```

```r
change2
```

```
## # A tibble: 2 x 21
## # Groups:   gender [2]
##   gender x2003 x2004 x2005 x2006 x2007 x2008 x2009 x2010 x2011 x2012 x2013 x2014
##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 female   270   270   270   270   270   270   280   266   275   285   280   280
## 2 male     300   315   300   300   300   310   318   310   303   330   320   320
## # ... with 8 more variables: x2015 <dbl>, x2016 <dbl>, x2017 <dbl>,
## #   x2018 <dbl>, x2019 <dbl>, x2020 <dbl>, x2021 <dbl>, leisure_change <dbl>
```

```r
delta <- change %>%
  lm(formula = leisure_change ~ gender+ new_educ+ child+ marst_simple)
summary(delta)
```

```
## 
## Call:
## lm(formula = leisure_change ~ gender + new_educ + child + marst_simple, 
##     data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -79.708 -30.594  -0.549  18.368 148.556 
## 
## Coefficients:
##                                Estimate Std. Error t value Pr(>|t|)  
## (Intercept)                      28.569     22.031   1.297   0.2049  
## gendermale                       13.194     16.654   0.792   0.4346  
## new_educ>Four Year Degree       -38.542     20.397  -1.890   0.0689 .
## new_educHigh School Graduate    -45.417     20.397  -2.227   0.0339 *
## child                             7.639     16.654   0.459   0.6499  
## marst_simpleNever Married        14.750     20.397   0.723   0.4754  
## marst_simpleSeparated/Divorced   33.542     20.397   1.644   0.1109  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 49.96 on 29 degrees of freedom
## Multiple R-squared:  0.2431,	Adjusted R-squared:  0.08656 
## F-statistic: 1.553 on 6 and 29 DF,  p-value: 0.1966
```



