Gym Attendance RCT Data Cleaning and Regression Analysis
================
Georgianna James
October 20th, 2021

# Set up

## Required Packages

``` r
library(tidyverse)
library(ggplot2)
library(stringr)
library(plotrix)
library(here)
library(stargazer)
```

## Import Data

``` r
raw_data <- read_csv(here("data", "hoursatgymRCT.csv"))

gym_attendance <- read_csv(here("data", "hoursatgymRCT.csv"))
```

## Calculating Average Treatment Effect before cleaning, with no controls

    ## 
    ## Effects of Treatment (Compensation for attending gym)
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                                Hours           
    ## -----------------------------------------------
    ## Treatment                      6.594           
    ##                               (5.992)          
    ##                                                
    ## Control Mean                 20.952***         
    ##                               (4.237)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   1,000           
    ## R2                             0.001           
    ## Adjusted R2                   0.0002           
    ## Residual Std. Error      94.745 (df = 998)     
    ## F Statistic             1.211 (df = 1; 998)    
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

After running a regression, I found an average treatment effect and
standard error as follows:

Average Treatment Effect = 6.594 Standard Error = 5.992

This regression is not statistically significant, with a p value of
0.2714. However, I must clean the data and add controls to determine
whether or not this relationship is truly statistically significant.

# Data Cleaning

## Hours Cleaning

After 30 hours, the hour distribution drops off until around 60 hours
and then forms a distribution of a similar shape. I assume that this is
because these seemingly unreasonable hour reportings were reported in
minutes, so I am going to convert them back to hours by dividing by 60.

``` r
gym_attendance <- gym_attendance %>% 
  mutate(hours = if_else(hours > 30, hours / 60, hours))
```

## Community Center Cleaning

The community center column is not organized. There are a variety of
inputs for both hyde park and woodlawn, when we only have 2 variables. I
am going to change all hyde park variables to “H” and all Woodlawn
variables to “W” so that there are only two community center groups.

``` r
## clean community center
gym_attendance <- 
  gym_attendance %>%
  mutate(`community_center` = substr(`community_center`,1,1),
         `community_center` = toupper(`community_center`))
```

## Gender Cleaning

I am going to change all “female” to 1 and all “male” to 0, creating a
female dummy variable.

``` r
gym_attendance <-
  gym_attendance %>%
  mutate(`female` = str_replace(`female`, c("female"),"1"),
         `female` = str_replace(`female`, "male", "0"),
         `female` = as.double(`female`))
```

## Age Cleaning

I am going change all of the participants whose age is reported as -99
with NA.

``` r
gym_attendance <- gym_attendance %>% 
  mutate(age = na_if(age, -99))
```

## BMI Cleaning

``` r
gym_attendance <- gym_attendance %>% 
  mutate(bmi = if_else(bmi < 1, bmi * 100, bmi))
```

\`

Because these outliers form a similar shaped distribution to the rest of
the bmi data and BMI typically falls between 15% and 30%, I assume all
the recorded BMI values that are less than 1 were actually bmi’s
reported as percentages, so I changed these from decimal percentages to
numbers by multiplying by 100. For example 0.22 becomes 22.

## Race_ethnicity cleaning

There are two different black variables. Assuming that they mean the
same thing. I am going to change all black variables to “B.” For
continuity I will change hispanic to “H”, and white to “W”.

``` r
gym_attendance <- 
  gym_attendance %>%
  mutate(`race_ethnicity` = substr(`race_ethnicity`,1,1),
         `race_ethnicity` = toupper(`race_ethnicity`))
```

## Regressing with clean data

``` r
regression <- lm(hours~treatment, data = gym_attendance)




stargazer(regression,
           type = "text",
  title="Effects of Treatment (Compensation for attending gym)", 
  dep.var.labels=c("Hours"),
  covariate.labels=c("Treatment","Control Mean"),
  digits=3, out = "table2.txt")
```

    ## 
    ## Effects of Treatment (Compensation for attending gym)
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                                Hours           
    ## -----------------------------------------------
    ## Treatment                    1.874***          
    ##                               (0.223)          
    ##                                                
    ## Control Mean                 5.612***          
    ##                               (0.158)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   1,000           
    ## R2                             0.066           
    ## Adjusted R2                    0.065           
    ## Residual Std. Error      3.531 (df = 998)      
    ## F Statistic           70.415*** (df = 1; 998)  
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

After running a regression with the cleaned data, I found a treatment
effect and standard deviation of:

average treatment effect: 1.8740 standard deviation: 0.2233

While these results are significant, I must to ensure there was true
randomization before I can interpret these results.

## Evaluating Randomization

In order to have true randomization, the treatment and control groups
must be otherwise identical to each other. Additionally, there must not
be any selection bias in your population that may come from self
selection into the group, non-compliance, attrition, and many other
confounding variables. In order to analyze what went wrong within this
randomization, I must evaluate how the covariates influence the output
of each group.

    ## # A tibble: 2 × 4
    ##   treatment average_age average_bmi average_hours
    ##       <dbl>       <dbl>       <dbl>         <dbl>
    ## 1         0        32.9        29.8          5.61
    ## 2         1        31.7        27.8          7.49

Looking at race distribution across treatment and control:

    ## # A tibble: 8 × 3
    ## # Groups:   treatment [2]
    ##   treatment race_ethnicity count
    ##       <dbl> <chr>          <int>
    ## 1         0 B                194
    ## 2         0 H                 86
    ## 3         0 W                206
    ## 4         0 <NA>              14
    ## 5         1 B                315
    ## 6         1 H                 51
    ## 7         1 W                120
    ## 8         1 <NA>              14

The control group has 194 black participants, 206 white participants,
and 86 hispanic participants, and the treated group has 315 black
participants, 120 white participants, and 51 hispanic participants,
which are similar distributions.

Now, let’s look at community center distributions:

    ## # A tibble: 4 × 6
    ## # Groups:   treatment [2]
    ##   treatment community_center count average_hours average_bmi average_age
    ##       <dbl> <chr>            <int>         <dbl>       <dbl>       <dbl>
    ## 1         0 H                  350          4.84        31.3        34.7
    ## 2         0 W                  150          7.42        26.5        28.6
    ## 3         1 H                  150          6.09        31.2        36.0
    ## 4         1 W                  350          8.09        26.4        29.8

The treated group had 350 at Woodlawn and 150 at Hyde Park. The
distribution between community center is very imbalanced between control
and treatment. Additionally, the type of patrons who attend the two
different commmunity centers differ by average bmi and age.

    ## 
    ## Effects of Community Center (Compensation for attending gym)
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                                Hours           
    ## -----------------------------------------------
    ## Woodlawn                     2.674***          
    ##                               (0.215)          
    ##                                                
    ## Intercept                    5.212***          
    ##                               (0.152)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   1,000           
    ## R2                             0.134           
    ## Adjusted R2                    0.133           
    ## Residual Std. Error      3.400 (df = 998)      
    ## F Statistic          154.672*** (df = 1; 998)  
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

After running a regression I found that simply participating in this
experiment at Woodlawn had a large, positive, statistically significant
impact on hours reported. The effect of being at Woodlawn on hours was
2.674. The treatment group had 350/500 participants at Woodlawn while
the control group had 150/500. This is a very significant confounding
variale. While mediators such as convenience, accessibility, or
proximity to facility may be influencing this relationship, it was
captured by the community center variable.

    ## # A tibble: 2 × 4
    ##   treatment count female percentfemale
    ##       <dbl> <int>  <dbl>         <dbl>
    ## 1         0   500    318         0.636
    ## 2         1   500    291         0.582

There are different distributions of gender in treatment and control.

## Controling for Community Center and Gender

In order to know the true treatment effect, you must control for
community center and gender. Running a regression, controlling for
community center and gender, you find:

    ## 
    ## Effects of Treatment (Controlling for community center and gender)
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                                Hours           
    ## -----------------------------------------------
    ## Treatment                    0.948***          
    ##                               (0.219)          
    ##                                                
    ## Community Center             2.001***          
    ##                               (0.221)          
    ##                                                
    ## Female                       -2.333***         
    ##                               (0.208)          
    ##                                                
    ## Control Mean                 6.495***          
    ##                               (0.210)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   1,000           
    ## R2                             0.244           
    ## Adjusted R2                    0.242           
    ## Residual Std. Error      3.179 (df = 996)      
    ## F Statistic          107.330*** (df = 3; 996)  
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01
