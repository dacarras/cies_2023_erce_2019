CIES: Intro to ERCE with R
================
Carrasco, D.
February 18th, 2023

<style>
  .main-container {
    max-width: 1600px !important;
  }
  .list-group-item.active, 
  .list-group-item.active:focus, 
  .list-group-item.active:hover {
    background-color: #373334;
  }
</style>

# Libraries we need to install

``` r
# -------------------------------------------------------------------
# libraries
# -------------------------------------------------------------------

#------------------------------------------------
# library to install libraries from github
#------------------------------------------------

install.packages('devtools')

#------------------------------------------------
# library with ERCE data
#------------------------------------------------

# library is in development
devtools::install_github('dacarras/erce',force = TRUE)

#------------------------------------------------
# libraries we have been usoibg
#------------------------------------------------

install.packages('tidyverse')
install.packages('mitools')
install.packages('survey')
install.packages('srvyr')

#------------------------------------------------
# libraries to fit models
#------------------------------------------------

install.packages('MplusAutomation') # requires Mplus
install.packages('RStata')          # requires Stata
install.packages('WeMix')

#------------------------------------------------
# to extract estimates
#------------------------------------------------

install.packages('texreg')
install.packages('miceadds')
```

# Code 1.1: prepare data for multilevel modelling

``` r
# -------------------------------------------------------------------
# prepara data for multilevel models
# -------------------------------------------------------------------


#------------------------------------------------
# load main library
#------------------------------------------------

library(dplyr)

#------------------------------------------------
# add common clustering variables
#------------------------------------------------

data_a6 <- erce::erce_2019_qa6 %>%
erce::remove_labels() %>%
mutate(id_k = as.numeric(as.factor(paste0(IDCNTRY)))) %>%
mutate(id_s = as.numeric(as.factor(paste0(IDCNTRY, "_", STRATA)))) %>%
mutate(id_j = as.numeric(as.factor(paste0(IDCNTRY, "_", IDSCHOOL)))) %>%
mutate(id_i = seq(1:nrow(.)))

#------------------------------------------------
# add country names
#------------------------------------------------

country_names <- read.table(
text="
IDCNTRY ctry_name
  32    'Argentina'              
  76    'Brasil'
 170    'Colombia'
 188    'Costa Rica'
 192    'Cuba'
 214    'República Dominicana'
 218    'Ecuador'   
 222    'El Salvador'
 320    'Guatemala'
 340    'Honduras'
 484    'México'
 558    'Nicaragua'
 591    'Panamá'
 600    'Paraguay'
 604    'Perú'
 858    'Uruguay'
",
header=TRUE, stringsAsFactors = FALSE) %>%
mutate(IDCNTRY = as.numeric(IDCNTRY))

data_a6 <- data_a6 %>%
           dplyr::left_join(., country_names, by = 'IDCNTRY')

#------------------------------------------------
# add senate weight (scaled up to 1000)
#------------------------------------------------

data_a6 <- data_a6 %>%
           # rename the original name
           rename(WS_previous = WS) %>%
           # we add the new WS variable
           erce::senate_weights(.,
           scale = 1000,
           wt = 'WT',
           id_k = 'id_k')
     
# Note: we are erasing the original variable.
#       This is a redundant operation.
#       We are including this step just to illustrate
#       how to use this function.

#------------------------------------------------
# add multilevel survey weights
#------------------------------------------------

data_a6 <- data_a6 %>%
           erce::lsa_weights(.,
           id_i = 'id_i',
           id_j = 'id_j',
           id_k = 'id_k',
           wt = 'WT',
           wi = 'WI',
           wj = 'WJ' )

#------------------------------------------------
# show the sum of weights
#------------------------------------------------

data_a6 %>%
group_by(id_k, ctry_name) %>%
summarize(
  sum_of_raw_weights = sum(WT),
  sum_of_senate_weights = sum(ws),
  sum_of_normalized_weights = sum(wa1*wa2),
  sum_of_effective_sample_weights = sum(wb1*wb2),
  number_of_observations = n()
  ) %>%
knitr::kable()
```

    ## [1m[22m`summarise()` has grouped output by 'id_k'. You can override using the
    ## `.groups` argument.

| id_k | ctry_name            | sum_of_raw_weights | sum_of_senate_weights | sum_of_normalized_weights | sum_of_effective_sample_weights | number_of_observations |
|-----:|:---------------------|-------------------:|----------------------:|--------------------------:|--------------------------------:|-----------------------:|
|    1 | Colombia             |             847931 |                  1000 |                      4467 |                            4467 |                   4467 |
|    2 | Costa Rica           |              67429 |                  1000 |                      3699 |                            3699 |                   3699 |
|    3 | Cuba                 |             100032 |                  1000 |                      5126 |                            5126 |                   5126 |
|    4 | República Dominicana |             158845 |                  1000 |                      4899 |                            4899 |                   4899 |
|    5 | Ecuador              |             292011 |                  1000 |                      6758 |                            6758 |                   6758 |
|    6 | El Salvador          |              99394 |                  1000 |                      5920 |                            5920 |                   5920 |
|    7 | Argentina            |             820043 |                  1000 |                      5004 |                            5004 |                   5004 |
|    8 | Guatemala            |             307831 |                  1000 |                      4895 |                            4895 |                   4895 |
|    9 | Honduras             |             184823 |                  1000 |                      4423 |                            4423 |                   4423 |
|   10 | México               |            2291802 |                  1000 |                      4824 |                            4824 |                   4824 |
|   11 | Nicaragua            |              58864 |                  1000 |                      4868 |                            4868 |                   4868 |
|   12 | Panamá               |              66555 |                  1000 |                      5632 |                            5632 |                   5632 |
|   13 | Paraguay             |             105751 |                  1000 |                      4849 |                            4849 |                   4849 |
|   14 | Perú                 |             534576 |                  1000 |                      5938 |                            5938 |                   5938 |
|   15 | Brasil               |            3123877 |                  1000 |                      4349 |                            4349 |                   4349 |
|   16 | Uruguay              |              49882 |                  1000 |                      5176 |                            5176 |                   5176 |

# Code 1.2: Prepare covariates

``` r
# -------------------------------------------------------------------
# add school covariates
# -------------------------------------------------------------------

#------------------------------------------------
# create variable representing school population density
#------------------------------------------------

urba_a6 <- erce::erce_2019_qd6 %>%
           mutate(urba = case_when(
            DDIT19 == 1 ~ 0, # 2.000 people or less
            DDIT19 == 2 ~ 0, # Between 2.001 and 5.000 people
            DDIT19 == 3 ~ 0, # Between 5.001 and 10.000 people
            DDIT19 == 4 ~ 1, # Between 10.001 and 100.000 people
            DDIT19 == 5 ~ 1  # More than 100.000 people
            )) %>%
           dplyr::select(
            IDCNTRY, IDSCHOOL, urba)


#------------------------------------------------
# add school covariate
#------------------------------------------------

data_model <- data_a6 %>%
              dplyr::left_join(., urba_a6,
                by = c('IDCNTRY', 'IDSCHOOL'))

#------------------------------------------------
# centering variables (manually), Z == ses
#------------------------------------------------

data_model <- data_model %>%
## socio economic status
mutate(z = ISECF)  %>%                      # mean score
mutate(z_c = r4sda::c_mean(z, id_j)) %>%    # means by group
mutate(z_g = r4sda::c_mean(z, id_k)) %>%    # grand mean    
mutate(z_w = z - z_c   )    %>%  # centering within cluster
mutate(z_m = z - z_g   )    %>%  # centering to the grand mean
mutate(z_b = z_c - z_g )         # centered cluster means


#------------------------------------------------
# centering variables (manually), V == urba
#------------------------------------------------

data_model <- data_model %>%
## student responses
mutate(v = urba)  %>%                       # mean score
mutate(v_c = r4sda::c_mean(v, id_j)) %>%    # means by group
mutate(v_g = r4sda::c_mean(v, id_k)) %>%    # grand mean    
mutate(v_w = v - v_c   )    %>%  # centering within cluster
mutate(v_m = v - v_g   )    %>%  # centering to the grand mean
mutate(v_b = v_c - v_g )         # centered cluster means
```

# Code 1.3 multilevel model with `lmerTest`

``` r
# -------------------------------------------------------------------
# multilevel model without sampling design
# -------------------------------------------------------------------

#------------------------------------------------
# configure R console output
#------------------------------------------------

options(scipen = 999)
options(digits = 10)

#------------------------------------------------
# fit model
#------------------------------------------------
           
data_model %>%
dplyr::filter(ctry_name == 'Paraguay') %>%
lmerTest::lmer(MAT_1 ~ 1 + z_w + z_b + v_b + (1 | id_j), data = ., REML = FALSE) %>%
broom.mixed::tidy() %>%
knitr::kable(., digits = 2)
```

| effect   | group    | term              | estimate | std.error | statistic |      df | p.value |
|:---------|:---------|:------------------|---------:|----------:|----------:|--------:|--------:|
| fixed    |          | (Intercept)       |   653.67 |      3.00 |    218.01 |  221.86 |    0.00 |
| fixed    |          | z_w               |    13.68 |      1.47 |      9.32 | 4226.13 |    0.00 |
| fixed    |          | z_b               |    45.76 |      5.35 |      8.55 |  246.39 |    0.00 |
| fixed    |          | v_b               |    10.73 |      7.62 |      1.41 |  215.33 |    0.16 |
| ran_pars | id_j     | sd\_\_(Intercept) |    42.40 |           |           |         |         |
| ran_pars | Residual | sd\_\_Observation |    67.40 |           |           |         |         |

# Code 1.4 multilevel model with `Mplus`, using plausible values

``` r
# -------------------------------------------------------------------
# multilevel model with Mplus
# -------------------------------------------------------------------

#------------------------------------------------
# prepared data for mplus
#------------------------------------------------

data_mixed <- data_model %>%
              dplyr::filter(ctry_name == 'Paraguay') %>%
              rename_all(tolower) %>%
              dplyr::select(
              mat_1, mat_2, mat_3, mat_4, mat_5,
              id_k, id_s, id_j, id_i,
              wb1, wb2, wi, wj,
              z_w, z_b, v_b
              )

#------------------------------------------------
# list of imputed data
#------------------------------------------------

data_1 <- data_mixed %>% mutate(y = mat_1)
data_2 <- data_mixed %>% mutate(y = mat_2)
data_3 <- data_mixed %>% mutate(y = mat_3)
data_4 <- data_mixed %>% mutate(y = mat_4)
data_5 <- data_mixed %>% mutate(y = mat_5)

data_list <- list(
data_1,
data_2,
data_3,
data_4,
data_5
)

#------------------------------------------------
# create mplus object
#------------------------------------------------

mplus_model <- MplusAutomation::mplusObject(
MODEL = '
%WITHIN%

y on z_w;

%BETWEEN%

y on v_b;
y on z_b;

',
ANALYSIS = '
TYPE = COMPLEX TWOLEVEL;
ESTIMATOR = MLR;
PROCESSORS = 4;
',
VARIABLE ='
STRATIFICATION = id_s;
CLUSTER  = id_j;
WEIGHT   = wi;
BWEIGHT  = wj;
WTSCALE  = ECLUSTER;
BWTSCALE = SAMPLE;

USEVARIABLES =
y
z_w
v_b
z_b
;

WITHIN  = z_w;
BETWEEN = v_b z_b;

',
OUTPUT ='
STANDARDIZED
CINTERVAL
RESIDUAL
;
',
rdata = data_list,
imputed = TRUE
)


#------------------------------------------------
# create list of data files
#------------------------------------------------


mlm_model_data_list <-
'
mlm_imp_1.dat
mlm_imp_2.dat
mlm_imp_3.dat
mlm_imp_4.dat
mlm_imp_5.dat
'

write(mlm_model_data_list, file="mlm.dat", ncolumns=1)

#------------------------------------------------
# fit models
#------------------------------------------------

fit_4 <- MplusAutomation::mplusModeler(mplus_model,
             modelout = 'mlm.inp',
             dataout = 'mlm.dat',
             run = 1L,
             hashfilename = FALSE,
             writeData = 'always'
             )
```

    ## The file(s)
    ##  'mlm_imp_1.dat;
    ## mlm_imp_2.dat;
    ## mlm_imp_3.dat;
    ## mlm_imp_4.dat;
    ## mlm_imp_5.dat' 
    ## currently exist(s) and will be overwritten

``` r
#------------------------------------------------
# extract estimates
#------------------------------------------------


ci_00  <- fit_4 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('ci.unstandardized') %>%
          tibble::tibble() %>%
          mutate(term = param)  %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(ll   = low2.5) %>%
          mutate(ul   = up2.5)  %>%
          mutate(index = paste0(paramHeader, "_", term,"_",level)) %>%
          dplyr::select(index, ll, ul)

r2_00  <- fit_4 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('r2') %>%
          tibble::tibble() %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(term = c('r2_w', 'r2_b')) %>%
          mutate(object = c('r2', 'r2')) %>%
          mutate(e  = as.numeric(est)) %>%
          mutate(se = as.numeric(se)) %>%
          mutate(p  = as.numeric(pval)) %>%
          mutate(missing  = as.numeric(rate_missing)) %>%
          mutate(index = paste0(term,"_",level)) %>%
          mutate(control = 'yes') %>%
          dplyr::select(index, level, object, term, e, se, p, missing, control)

tabla_1_4 <- fit_4 %>%
             purrr::pluck('results') %>%
             purrr::pluck('parameters') %>%
             purrr::pluck('unstandardized') %>%
             tibble::tibble() %>%
             mutate(term = param) %>%
             mutate(level  = tolower(BetweenWithin)) %>%
             mutate(e  = est) %>%
             mutate(se = se) %>%
             mutate(p  = pval) %>%
             mutate(missing  = rate_missing) %>%
             mutate(index = paste0(paramHeader, "_", term,"_",level)) %>%
             dplyr::left_join(., ci_00, by = 'index') %>%
             mutate(control = 'yes') %>%
             mutate(object = c(           'pi_w', 'epsilon', 
                               'gamma_b', 'pi_b', 'alpha', 'mu')) %>%
             dplyr::bind_rows(., r2_00) %>%
             dplyr::select(level, object, term, e, se, p, missing, ll, ul, control)

#------------------------------------------------
# display table
#------------------------------------------------

tabla_1_4 %>%
knitr::kable(., digits = 2)
```

| level   | object  | term |       e |     se |    p | missing |      ll |      ul | control |
|:--------|:--------|:-----|--------:|-------:|-----:|--------:|--------:|--------:|:--------|
| within  | pi_w    | Z_W  |   12.51 |   2.25 | 0.00 |    0.27 |    8.09 |   16.93 | yes     |
| within  | epsilon | Y    | 4490.40 | 167.57 | 0.00 |    0.35 | 4161.96 | 4818.85 | yes     |
| between | gamma_b | V_B  |   22.51 |  10.39 | 0.03 |    0.05 |    2.14 |   42.87 | yes     |
| between | pi_b    | Z_B  |   28.10 |   6.91 | 0.00 |    0.12 |   14.55 |   41.65 | yes     |
| between | alpha   | Y    |  655.07 |   4.60 | 0.00 |    0.15 |  646.05 |  664.08 | yes     |
| between | mu      | Y    | 2480.01 | 471.29 | 0.00 |    0.10 | 1556.29 | 3403.74 | yes     |
| within  | r2      | r2_w |    0.01 |   0.00 | 0.01 |    0.30 |         |         | yes     |
| between | r2      | r2_b |    0.18 |   0.07 | 0.01 |    0.15 |         |         | yes     |

# Code 1.5 multilevel model with `svylme`

``` r
# -------------------------------------------------------------------
# multilevel model with svylme
# -------------------------------------------------------------------

#------------------------------------------------
# configure R console
#------------------------------------------------

options(scipen = 999)
options(digits = 10)

#------------------------------------------------
# survey object
#------------------------------------------------

erce_svy2l <- data_model %>%
              dplyr::filter(ctry_name == 'Paraguay') %>%
              survey::svydesign(
              id = ~ id_j + id_i,
              strata  = ~id_s,
              data = .,
              weights = ~ wb1 + wb2
              )

#------------------------------------------------
# fit model
#------------------------------------------------

svylme_pv_results <- mitools::withPV(
   mapping = mat ~ MAT_1 + MAT_2 + MAT_3 + MAT_4 + MAT_5, #<<
   data = erce_svy2l,
   action = quote(
    svylme::svy2lme(
    mat ~ 1 + z_w + z_b + v_b + as.factor(id_s) + (1 | id_j),
    sterr  = TRUE,
    method = 'nested',
    design = erce_svy2l
    )
    ),
   rewrite=TRUE
   )

#------------------------------------------------
# create table
#------------------------------------------------

tabla_1_5 <- erce::combine_reg(svylme_pv_results)

#------------------------------------------------
# display table
#------------------------------------------------

tabla_1_5 %>%
knitr::kable(., digits = 2)
```

| term              |      e |    se | p_val |     lo |     hi |
|:------------------|-------:|------:|------:|-------:|-------:|
| (Intercept)       | 632.82 |  4.53 |  0.00 | 623.94 | 641.70 |
| z_w               |  10.85 |  3.19 |  0.00 |   4.57 |  17.14 |
| z_b               |  34.43 |  9.47 |  0.00 |  15.86 |  53.01 |
| v_b               |  16.20 |  9.11 |  0.08 |  -1.66 |  34.05 |
| as.factor(id_s)67 |  32.58 | 11.12 |  0.00 |  10.77 |  54.39 |
| as.factor(id_s)68 |  38.99 | 17.94 |  0.03 |   3.83 |  74.15 |
| as.factor(id_s)69 |  30.41 | 14.82 |  0.05 |  -0.48 |  61.30 |
| as.factor(id_s)70 |  37.65 | 11.00 |  0.00 |  16.08 |  59.23 |
| as.factor(id_s)71 |  53.87 | 50.41 |  0.29 | -44.95 | 152.68 |
| as.factor(id_s)72 | -24.11 | 12.87 |  0.11 | -55.06 |   6.83 |

# Code 1.6 multilevel model `WeMix`

``` r
# -------------------------------------------------------------------
# multilevel model with WeMix
# -------------------------------------------------------------------

#------------------------------------------------
# configurar console
#------------------------------------------------

options(scipen = 999)
options(digits = 4)

#------------------------------------------------
# survey object
#------------------------------------------------

data_wemix <- data_model %>%
              dplyr::filter(ctry_name == 'Paraguay')

#------------------------------------------------
# fit model
#------------------------------------------------

wemix_pv_results <- mitools::withPV(
   mapping = mat ~ MAT_1 + MAT_2 + MAT_3 + MAT_4 + MAT_5, #<<
   data = data_wemix,
   action = quote(
WeMix::mix(
  mat ~ 1 + z_w + z_b + v_b + as.factor(id_s) + (1 | id_j),
  data = data_wemix,
  weights = c('wb1','wb2'),
  cWeights = TRUE)
    ),
   rewrite=TRUE
   )
```

    ## Warning in WeMix::mix(MAT_1 ~ 1 + z_w + z_b + v_b + as.factor(id_s) + (1 | :
    ## There were 368 rows with missing data. These have been removed.

    ## Warning in WeMix::mix(MAT_2 ~ 1 + z_w + z_b + v_b + as.factor(id_s) + (1 | :
    ## There were 368 rows with missing data. These have been removed.

    ## Warning in WeMix::mix(MAT_3 ~ 1 + z_w + z_b + v_b + as.factor(id_s) + (1 | :
    ## There were 368 rows with missing data. These have been removed.

    ## Warning in WeMix::mix(MAT_4 ~ 1 + z_w + z_b + v_b + as.factor(id_s) + (1 | :
    ## There were 368 rows with missing data. These have been removed.

    ## Warning in WeMix::mix(MAT_5 ~ 1 + z_w + z_b + v_b + as.factor(id_s) + (1 | :
    ## There were 368 rows with missing data. These have been removed.

``` r
#------------------------------------------------
# create table
#------------------------------------------------

tabla_1_6 <- summary(
  miceadds::pool_mi(
  qhat = list(
    wemix_pv_results[[1]]$coef,
    wemix_pv_results[[2]]$coef,
    wemix_pv_results[[3]]$coef,
    wemix_pv_results[[4]]$coef,
    wemix_pv_results[[5]]$coef
    ), 
  se   = list(
    wemix_pv_results[[1]]$SE,
    wemix_pv_results[[2]]$SE,
    wemix_pv_results[[3]]$SE,
    wemix_pv_results[[4]]$SE,
    wemix_pv_results[[5]]$SE
    )
  )
) %>%
tibble::rownames_to_column("term") %>%
tibble::as_tibble()
```

    ## Multiple imputation results:
    ## Call: miceadds::pool_mi(qhat = list(wemix_pv_results[[1]]$coef, wemix_pv_results[[2]]$coef, 
    ##     wemix_pv_results[[3]]$coef, wemix_pv_results[[4]]$coef, wemix_pv_results[[5]]$coef), 
    ##     se = list(wemix_pv_results[[1]]$SE, wemix_pv_results[[2]]$SE, 
    ##         wemix_pv_results[[3]]$SE, wemix_pv_results[[4]]$SE, wemix_pv_results[[5]]$SE))
    ##                   results     se        t            p  (lower upper) missInfo
    ## (Intercept)        629.47  4.203 149.7575 0.0000000000 621.226 637.71    4.8 %
    ## z_w                 12.51  2.261   5.5340 0.0000006068   7.997  17.03   27.1 %
    ## z_b                 26.01  9.330   2.7882 0.0059586992   7.585  44.44   17.1 %
    ## v_b                 20.51  7.906   2.5938 0.0095786298   5.000  36.02    5.1 %
    ## as.factor(id_s)67   30.41 10.473   2.9033 0.0038703467   9.825  50.99    9.7 %
    ## as.factor(id_s)68   53.30 15.428   3.4550 0.0006213926  22.955  83.65   11.5 %
    ## as.factor(id_s)69   39.92 12.966   3.0789 0.0070359228  12.487  67.36   54.6 %
    ## as.factor(id_s)70   48.29  9.751   4.9518 0.0000008109  29.160  67.41      5 %
    ## as.factor(id_s)71   33.91 40.816   0.8308 0.4061726894 -46.130 113.95    4.3 %
    ## as.factor(id_s)72  -15.95 17.509  -0.9111 0.3673224780 -51.259  19.36   33.5 %

``` r
#------------------------------------------------
# display table
#------------------------------------------------

tabla_1_6 %>%
knitr::kable(., digits = 2)
```

| term              | results |    se |      t |    p | (lower | upper) | missInfo |
|:------------------|--------:|------:|-------:|-----:|-------:|-------:|:---------|
| (Intercept)       |  629.47 |  4.20 | 149.76 | 0.00 | 621.23 | 637.71 | 4.8 %    |
| z_w               |   12.51 |  2.26 |   5.53 | 0.00 |   8.00 |  17.03 | 27.1 %   |
| z_b               |   26.01 |  9.33 |   2.79 | 0.01 |   7.58 |  44.44 | 17.1 %   |
| v_b               |   20.51 |  7.91 |   2.59 | 0.01 |   5.00 |  36.02 | 5.1 %    |
| as.factor(id_s)67 |   30.41 | 10.47 |   2.90 | 0.00 |   9.83 |  50.99 | 9.7 %    |
| as.factor(id_s)68 |   53.30 | 15.43 |   3.45 | 0.00 |  22.95 |  83.65 | 11.5 %   |
| as.factor(id_s)69 |   39.92 | 12.97 |   3.08 | 0.01 |  12.49 |  67.36 | 54.6 %   |
| as.factor(id_s)70 |   48.29 |  9.75 |   4.95 | 0.00 |  29.16 |  67.41 | 5 %      |
| as.factor(id_s)71 |   33.91 | 40.82 |   0.83 | 0.41 | -46.13 | 113.95 | 4.3 %    |
| as.factor(id_s)72 |  -15.95 | 17.51 |  -0.91 | 0.37 | -51.26 |  19.36 | 33.5 %   |

# Code 1.7 modelo multinivel con `STATA`

``` r
# -------------------------------------------------------------------
# multilevel model with Stata
# -------------------------------------------------------------------

#------------------------------------------------
# create data frame for Stata
#------------------------------------------------

data_stata <- data_model %>%
dplyr::filter(ctry_name == 'Paraguay')

#------------------------------------------------
# configure RStata library
#------------------------------------------------

library(RStata)
options("RStata.StataPath"='/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp')
options("RStata.StataVersion"=15)

#------------------------------------------------
# Stata code
#------------------------------------------------

stata_code <- '

* ===============================================.
* define folder and open data
* ===============================================.

* ===============================.
* fit mlm model with @pv
* ===============================.

pv, pv(MAT_1 MAT_2 MAT_3 MAT_4 MAT_5): ///
mixed @pv               ///
z_w                     ///
z_b                     ///
v_b                     ///
i.id_s                  ///
[pw=wb1] || id_j:, ml pweight(wb2)
estimates store m01

* ===============================.
* variance
* ===============================.

/*return residual variance */
_diparm lnsig_e,  f(exp(@)^2) d(2*exp(@)^2)    
/*return between  variance */
_diparm lns1_1_1, f(exp(@)^2) d(2*exp(@)^2)  

* ===============================.
* estimates
* ===============================.

esttab m01, b(%9.2f) se(%9.2f) wide transform(ln*: exp(2*@) exp(2*@))

'

#----------------------------------------------------------
# display Stata results within R
#----------------------------------------------------------

RStata::stata(stata_code, data.in = data_stata)
```

    ## . 
    ## . 
    ## . * ===============================================.
    ## . * define folder and open data
    ## . * ===============================================.
    ## . 
    ## . * ===============================.
    ## . * fit mlm model with @pv
    ## . * ===============================.
    ## . 
    ## . pv, pv(MAT_1 MAT_2 MAT_3 MAT_4 MAT_5): ///
    ## > mixed @pv               ///
    ## > z_w                     ///
    ## > z_b                     ///
    ## > v_b                     ///
    ## > i.id_s                  ///
    ## > [pw=wb1] || id_j:, ml pweight(wb2)
    ## 
    ## command(s) run for each plausible value:
    ## 
    ##      mixed @pv               z_w                     z_b                     v_b                     i.id_s                  [pw=wb1] || id_j:, ml pweight(wb2)
    ## 
    ## Estimates for MAT_1   complete
    ## Estimates for MAT_2   complete
    ## Estimates for MAT_3   complete
    ## Estimates for MAT_4   complete
    ## Estimates for MAT_5   complete
    ## 
    ## Number of observations: 4481
    ## Average R-Squared: .
    ## 
    ## 
    ##                       Coef     Std Err           t     t Param       P>|t|
    ##      MAT_5:z_w   12.512955   2.2611123   5.5339821           .           .
    ##      MAT_5:z_b   26.013356   9.3297681   2.7882103           .           .
    ##      MAT_5:v_b   20.507859   7.9064065   2.5938281           .           .
    ## MAT_5:66b.id_s           0           0           .           .           .
    ##  MAT_5:67.id_s   30.405715   10.472871   2.9032836           .           .
    ##  MAT_5:68.id_s   53.303288   15.427992   3.4549724           .           .
    ##  MAT_5:69.id_s    39.92108   12.965822    3.078947           .           .
    ##  MAT_5:70.id_s   48.286777   9.7514024   4.9517777           .           .
    ##  MAT_5:71.id_s   33.909887   40.815817   .83080261           .           .
    ##  MAT_5:72.id_s  -15.951792   17.508669  -.91107964           .           .
    ##    MAT_5:_cons   629.46964    4.203277   149.75688           .           .
    ## lns1_1_1:_cons   3.8361029   .10507108   36.509598           .           .
    ##  lnsig_e:_cons   4.2050067   .01882431   223.38176           .           .
    ## . estimates store m01
    ## . 
    ## . * ===============================.
    ## . * variance
    ## . * ===============================.
    ## . 
    ## . /*return residual variance */
    ## . _diparm lnsig_e,  f(exp(@)^2) d(2*exp(@)^2)    
    ##     /lnsig_e |   4491.821   169.1108                      4172.303    4835.807
    ## . /*return between  variance */
    ## . _diparm lns1_1_1, f(exp(@)^2) d(2*exp(@)^2)  
    ##    /lns1_1_1 |   2147.814   451.3463                      1422.733    3242.425
    ## . 
    ## . * ===============================.
    ## . * estimates
    ## . * ===============================.
    ## . 
    ## . esttab m01, b(%9.2f) se(%9.2f) wide transform(ln*: exp(2*@) exp(2*@))
    ## 
    ## -----------------------------------------
    ##                       (1)                
    ##                                          
    ## -----------------------------------------
    ## MAT_5                                    
    ## z_w                 12.51***       (2.26)
    ## z_b                 26.01**        (9.33)
    ## v_b                 20.51**        (7.91)
    ## 66.id_s              0.00             (.)
    ## 67.id_s             30.41**       (10.47)
    ## 68.id_s             53.30***      (15.43)
    ## 69.id_s             39.92**       (12.97)
    ## 70.id_s             48.29***       (9.75)
    ## 71.id_s             33.91         (40.82)
    ## 72.id_s            -15.95         (17.51)
    ## _cons              629.47***       (4.20)
    ## -----------------------------------------
    ## lns1_1_1                                 
    ## _cons             2147.81***     (225.67)
    ## -----------------------------------------
    ## lnsig_e                                  
    ## _cons             4491.82***      (84.56)
    ## -----------------------------------------
    ## N                    4481                
    ## -----------------------------------------
    ## Standard errors in parentheses
    ## * p<0.05, ** p<0.01, *** p<0.001
    ## .

``` r
#----------------------------------------------------------
# create table with obtained results
#----------------------------------------------------------

tabla_1_7 <- read.table(
text = "
variable            e          star       se
z_w                 12.51      '***'    2.26
z_b                 26.01      '** '    9.33
v_b                 20.51      '** '    7.91
66.id_s              0.00      '   '      NA
67.id_s             30.41      '** '   10.47
68.id_s             53.30      '***'   15.43
69.id_s             39.92      '** '   12.97
70.id_s             48.29      '***'    9.75
71.id_s             33.91      '   '   40.82
72.id_s            -15.95      '   '   17.51
_cons              629.47      '***'    4.20
",
header=TRUE, stringsAsFactors = FALSE)



#------------------------------------------------
# display table
#------------------------------------------------

tabla_1_7 %>%
knitr::kable(., digits = 2)
```

| variable |      e | star   |    se |
|:---------|-------:|:-------|------:|
| z_w      |  12.51 | \*\*\* |  2.26 |
| z_b      |  26.01 | \*\*   |  9.33 |
| v_b      |  20.51 | \*\*   |  7.91 |
| 66.id_s  |   0.00 |        |       |
| 67.id_s  |  30.41 | \*\*   | 10.47 |
| 68.id_s  |  53.30 | \*\*\* | 15.43 |
| 69.id_s  |  39.92 | \*\*   | 12.97 |
| 70.id_s  |  48.29 | \*\*\* |  9.75 |
| 71.id_s  |  33.91 |        | 40.82 |
| 72.id_s  | -15.95 |        | 17.51 |
| \_cons   | 629.47 | \*\*\* |  4.20 |

# Annexes 1: comparing different settings with Mplus

- `fit_4` mixed model with survey weights (effective sample size),
  including stratification information

- `fit_8` mixed model with survey weights (effective sample size),
  including stratification information as fixed effects

- `fit_9` mixed model with survey weights (effective sample size) as
  provided by the user, including stratification information as fixed
  effects

- `fit_10` mixed model with survey weights (normalized) as provided by
  the user, including stratification information as fixed effects

- `fit_11` mixed model without, including stratification information as
  fixed effects

## Code 1.8 mixed mode with `Mplus`, with strata as fixed effects

``` r
# -------------------------------------------------------------------
# mixe model with Mplus
# -------------------------------------------------------------------

#------------------------------------------------
# prepare data for Mplus
#------------------------------------------------

data_mixed <- data_model %>%
              dplyr::filter(ctry_name == 'Paraguay') %>%
              rename_all(tolower) %>%
              mutate(strat_1 = dplyr::if_else(id_s == 66, 1, 0 )) %>%
              mutate(strat_2 = dplyr::if_else(id_s == 67, 1, 0 )) %>%
              mutate(strat_3 = dplyr::if_else(id_s == 68, 1, 0 )) %>%
              mutate(strat_4 = dplyr::if_else(id_s == 69, 1, 0 )) %>%
              mutate(strat_5 = dplyr::if_else(id_s == 70, 1, 0 )) %>%
              mutate(strat_6 = dplyr::if_else(id_s == 71, 1, 0 )) %>%
              mutate(strat_7 = dplyr::if_else(id_s == 72, 1, 0 )) %>%
              dplyr::select(
              mat_1, mat_2, mat_3, mat_4, mat_5,
              id_k, id_s, id_j, id_i,
              wb1, wb2, wi, wj,
              z_w, z_b, v_b,
#              strat_1, # strata of reference
              strat_2,
              strat_3,
              strat_4,
              strat_5,
              strat_6,
              strat_7
              )

#------------------------------------------------
# list of imputed data
#------------------------------------------------

data_1 <- data_mixed %>% mutate(y = mat_1)
data_2 <- data_mixed %>% mutate(y = mat_2)
data_3 <- data_mixed %>% mutate(y = mat_3)
data_4 <- data_mixed %>% mutate(y = mat_4)
data_5 <- data_mixed %>% mutate(y = mat_5)

data_list <- list(
data_1,
data_2,
data_3,
data_4,
data_5
)

#------------------------------------------------
# create mplus object
#------------------------------------------------

mplus_model <- MplusAutomation::mplusObject(
MODEL = '
%WITHIN%

y on z_w;

%BETWEEN%

y on v_b;
y on z_b;
y on strat_2; 
y on strat_3; 
y on strat_4; 
y on strat_5; 
y on strat_6; 
y on strat_7; 

',
ANALYSIS = '
TYPE = TWOLEVEL;
ESTIMATOR = MLR;
PROCESSORS = 4;
',
VARIABLE ='
CLUSTER  = id_j;
WEIGHT   = wi;
BWEIGHT  = wj;
WTSCALE  = ECLUSTER;
BWTSCALE = SAMPLE;

USEVARIABLES =
y
z_w
v_b
z_b
strat_2
strat_3
strat_4
strat_5
strat_6
strat_7
;

WITHIN  = z_w;
BETWEEN = v_b z_b strat_2-strat_7;

',
OUTPUT ='
STANDARDIZED
CINTERVAL
RESIDUAL
;
',
rdata = data_list,
imputed = TRUE
)


#------------------------------------------------
# list of data files
#------------------------------------------------


mlm_model_data_list <-
'
mlm_imp_1.dat
mlm_imp_2.dat
mlm_imp_3.dat
mlm_imp_4.dat
mlm_imp_5.dat
'

write(mlm_model_data_list, file="mlm.dat", ncolumns=1)

#------------------------------------------------
# fit models
#------------------------------------------------

fit_8 <- MplusAutomation::mplusModeler(mplus_model,
             modelout = 'mlm.inp',
             dataout = 'mlm.dat',
             run = 1L,
             hashfilename = FALSE,
             writeData = 'always'
             )
```

    ## The file(s)
    ##  'mlm_imp_1.dat;
    ## mlm_imp_2.dat;
    ## mlm_imp_3.dat;
    ## mlm_imp_4.dat;
    ## mlm_imp_5.dat' 
    ## currently exist(s) and will be overwritten

``` r
#------------------------------------------------
# extract estimates
#------------------------------------------------


ci_00  <- fit_8 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('ci.unstandardized') %>%
          tibble::tibble() %>%
          mutate(term = param)  %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(ll   = low2.5) %>%
          mutate(ul   = up2.5)  %>%
          mutate(index = paste0(paramHeader, "_", term,"_",level)) %>%
          dplyr::select(index, ll, ul)

r2_00  <- fit_8 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('r2') %>%
          tibble::tibble() %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(term = c('r2_w', 'r2_b')) %>%
          mutate(object = c('r2', 'r2')) %>%
          mutate(e  = as.numeric(est)) %>%
          mutate(se = as.numeric(se)) %>%
          mutate(p  = as.numeric(pval)) %>%
          mutate(missing  = as.numeric(rate_missing)) %>%
          mutate(index = paste0(term,"_",level)) %>%
          mutate(control = 'yes') %>%
          dplyr::select(index, level, object, term, e, se, p, missing, control)

tabla_1_8 <- fit_8 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('unstandardized') %>%
          tibble::tibble() %>%
          mutate(term = param) %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(e  = est) %>%
          mutate(se = se) %>%
          mutate(p  = pval) %>%
          mutate(missing  = rate_missing) %>%
          mutate(index = paste0(paramHeader, "_", term,"_",level)) %>%
          dplyr::left_join(., ci_00, by = 'index') %>%
          mutate(control = 'yes') %>%
          mutate(object = c(           'pi_w', 'epsilon', 
                            'gamma_b', 'pi_b', 
                            'delta_b1','delta_b2','delta_b3',
                            'delta_b4','delta_b5','delta_b6',
                            'alpha', 'mu')) %>%
          dplyr::bind_rows(., r2_00) %>%
          dplyr::select(level, object, term, e, se, p, missing, ll, ul, control)

#------------------------------------------------
# display table
#------------------------------------------------

tabla_1_8 %>%
knitr::kable(., digits = 2)
```

| level   | object   | term    |       e |     se |    p | missing |      ll |      ul | control |
|:--------|:---------|:--------|--------:|-------:|-----:|--------:|--------:|--------:|:--------|
| within  | pi_w     | Z_W     |   12.51 |   2.26 | 0.00 |    0.27 |    8.09 |   16.94 | yes     |
| within  | epsilon  | Y       | 4492.12 | 169.39 | 0.00 |    0.35 | 4160.11 | 4824.12 | yes     |
| between | gamma_b  | V_B     |   20.52 |   7.90 | 0.01 |    0.05 |    5.03 |   36.01 | yes     |
| between | pi_b     | Z_B     |   25.99 |   9.36 | 0.00 |    0.17 |    7.66 |   44.33 | yes     |
| between | delta_b1 | STRAT_2 |   30.39 |  10.46 | 0.00 |    0.10 |    9.89 |   50.89 | yes     |
| between | delta_b2 | STRAT_3 |   53.32 |  15.42 | 0.00 |    0.11 |   23.10 |   83.53 | yes     |
| between | delta_b3 | STRAT_4 |   39.95 |  12.97 | 0.00 |    0.54 |   14.53 |   65.37 | yes     |
| between | delta_b4 | STRAT_5 |   48.30 |   9.77 | 0.00 |    0.05 |   29.16 |   67.44 | yes     |
| between | delta_b5 | STRAT_6 |   33.90 |  40.72 | 0.41 |    0.04 |  -45.91 |  113.72 | yes     |
| between | delta_b6 | STRAT_7 |  -15.98 |  17.46 | 0.36 |    0.33 |  -50.22 |   18.25 | yes     |
| between | alpha    | Y       |  629.46 |   4.21 | 0.00 |    0.05 |  621.22 |  637.71 | yes     |
| between | mu       | Y       | 2155.03 | 452.29 | 0.00 |    0.11 | 1268.55 | 3041.51 | yes     |
| within  | r2       | r2_w    |    0.01 |   0.00 | 0.01 |    0.30 |         |         | yes     |
| between | r2       | r2_b    |    0.28 |   0.07 | 0.00 |    0.20 |         |         | yes     |

## Code 1.9 mixed model with `Mplus`, strata as fixed effects, using provided wb1, wb2

``` r
# -------------------------------------------------------------------
# mixed model with Mplus
# -------------------------------------------------------------------

#------------------------------------------------
# prepare data for Mplus
#------------------------------------------------

data_mixed <- data_model %>%
              dplyr::filter(ctry_name == 'Paraguay') %>%
              rename_all(tolower) %>%
              mutate(strat_1 = dplyr::if_else(id_s == 66, 1, 0 )) %>%
              mutate(strat_2 = dplyr::if_else(id_s == 67, 1, 0 )) %>%
              mutate(strat_3 = dplyr::if_else(id_s == 68, 1, 0 )) %>%
              mutate(strat_4 = dplyr::if_else(id_s == 69, 1, 0 )) %>%
              mutate(strat_5 = dplyr::if_else(id_s == 70, 1, 0 )) %>%
              mutate(strat_6 = dplyr::if_else(id_s == 71, 1, 0 )) %>%
              mutate(strat_7 = dplyr::if_else(id_s == 72, 1, 0 )) %>%
              dplyr::select(
              mat_1, mat_2, mat_3, mat_4, mat_5,
              id_k, id_s, id_j, id_i,
              wb1, wb2, wi, wj,
              z_w, z_b, v_b,
#              strat_1, # strata of reference
              strat_2,
              strat_3,
              strat_4,
              strat_5,
              strat_6,
              strat_7
              )

#------------------------------------------------
# list of imputed data
#------------------------------------------------

data_1 <- data_mixed %>% mutate(y = mat_1)
data_2 <- data_mixed %>% mutate(y = mat_2)
data_3 <- data_mixed %>% mutate(y = mat_3)
data_4 <- data_mixed %>% mutate(y = mat_4)
data_5 <- data_mixed %>% mutate(y = mat_5)

data_list <- list(
data_1,
data_2,
data_3,
data_4,
data_5
)

#------------------------------------------------
# create Mplus object
#------------------------------------------------

mplus_model <- MplusAutomation::mplusObject(
MODEL = '
%WITHIN%

y on z_w;

%BETWEEN%

y on v_b;
y on z_b;
y on strat_2; 
y on strat_3; 
y on strat_4; 
y on strat_5; 
y on strat_6; 
y on strat_7; 

',
ANALYSIS = '
TYPE = TWOLEVEL;
ESTIMATOR = MLR;
PROCESSORS = 4;
',
VARIABLE ='
CLUSTER  = id_j;
WEIGHT   = wb1;
BWEIGHT  = wb2;
WTSCALE  = UNSCALED;
BWTSCALE = UNSCALED;

USEVARIABLES =
y
z_w
v_b
z_b
strat_2
strat_3
strat_4
strat_5
strat_6
strat_7
;

WITHIN  = z_w;
BETWEEN = v_b z_b strat_2-strat_7;

',
OUTPUT ='
STANDARDIZED
CINTERVAL
RESIDUAL
;
',
rdata = data_list,
imputed = TRUE
)


#------------------------------------------------
# list of data files
#------------------------------------------------


mlm_model_data_list <-
'
mlm_imp_1.dat
mlm_imp_2.dat
mlm_imp_3.dat
mlm_imp_4.dat
mlm_imp_5.dat
'

write(mlm_model_data_list, file="mlm.dat", ncolumns=1)

#------------------------------------------------
# fit models
#------------------------------------------------

fit_9 <- MplusAutomation::mplusModeler(mplus_model,
             modelout = 'mlm.inp',
             dataout = 'mlm.dat',
             run = 1L,
             hashfilename = FALSE,
             writeData = 'always'
             )
```

    ## The file(s)
    ##  'mlm_imp_1.dat;
    ## mlm_imp_2.dat;
    ## mlm_imp_3.dat;
    ## mlm_imp_4.dat;
    ## mlm_imp_5.dat' 
    ## currently exist(s) and will be overwritten

``` r
#------------------------------------------------
# extract estimates
#------------------------------------------------


ci_00  <- fit_9 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('ci.unstandardized') %>%
          tibble::tibble() %>%
          mutate(term = param)  %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(ll   = low2.5) %>%
          mutate(ul   = up2.5)  %>%
          mutate(index = paste0(paramHeader, "_", term,"_",level)) %>%
          dplyr::select(index, ll, ul)

r2_00  <- fit_9 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('r2') %>%
          tibble::tibble() %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(term = c('r2_w', 'r2_b')) %>%
          mutate(object = c('r2', 'r2')) %>%
          mutate(e  = as.numeric(est)) %>%
          mutate(se = as.numeric(se)) %>%
          mutate(p  = as.numeric(pval)) %>%
          mutate(missing  = as.numeric(rate_missing)) %>%
          mutate(index = paste0(term,"_",level)) %>%
          mutate(control = 'yes') %>%
          dplyr::select(index, level, object, term, e, se, p, missing, control)

tabla_1_9 <- fit_9 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('unstandardized') %>%
          tibble::tibble() %>%
          mutate(term = param) %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(e  = est) %>%
          mutate(se = se) %>%
          mutate(p  = pval) %>%
          mutate(missing  = rate_missing) %>%
          mutate(index = paste0(paramHeader, "_", term,"_",level)) %>%
          dplyr::left_join(., ci_00, by = 'index') %>%
          mutate(control = 'yes') %>%
          mutate(object = c(           'pi_w', 'epsilon', 
                            'gamma_b', 'pi_b', 
                            'delta_b1','delta_b2','delta_b3',
                            'delta_b4','delta_b5','delta_b6',
                            'alpha', 'mu')) %>%
          dplyr::bind_rows(., r2_00) %>%
          dplyr::select(level, object, term, e, se, p, missing, ll, ul, control)


#------------------------------------------------
# display table
#------------------------------------------------

tabla_1_9 %>%
knitr::kable(., digits = 2)
```

| level   | object   | term    |       e |     se |    p | missing |      ll |      ul | control |
|:--------|:---------|:--------|--------:|-------:|-----:|--------:|--------:|--------:|:--------|
| within  | pi_w     | Z_W     |   12.51 |   2.26 | 0.00 |    0.27 |    8.09 |   16.94 | yes     |
| within  | epsilon  | Y       | 4492.12 | 169.39 | 0.00 |    0.35 | 4160.12 | 4824.12 | yes     |
| between | gamma_b  | V_B     |   20.52 |   7.90 | 0.01 |    0.05 |    5.03 |   36.01 | yes     |
| between | pi_b     | Z_B     |   25.99 |   9.36 | 0.00 |    0.17 |    7.66 |   44.33 | yes     |
| between | delta_b1 | STRAT_2 |   30.39 |  10.46 | 0.00 |    0.10 |    9.89 |   50.89 | yes     |
| between | delta_b2 | STRAT_3 |   53.32 |  15.42 | 0.00 |    0.11 |   23.10 |   83.53 | yes     |
| between | delta_b3 | STRAT_4 |   39.95 |  12.97 | 0.00 |    0.54 |   14.53 |   65.37 | yes     |
| between | delta_b4 | STRAT_5 |   48.30 |   9.77 | 0.00 |    0.05 |   29.16 |   67.44 | yes     |
| between | delta_b5 | STRAT_6 |   33.90 |  40.72 | 0.41 |    0.04 |  -45.92 |  113.72 | yes     |
| between | delta_b6 | STRAT_7 |  -15.98 |  17.46 | 0.36 |    0.33 |  -50.22 |   18.25 | yes     |
| between | alpha    | Y       |  629.46 |   4.21 | 0.00 |    0.05 |  621.22 |  637.71 | yes     |
| between | mu       | Y       | 2155.03 | 452.29 | 0.00 |    0.11 | 1268.55 | 3041.52 | yes     |
| within  | r2       | r2_w    |    0.01 |   0.00 | 0.01 |    0.30 |         |         | yes     |
| between | r2       | r2_b    |    0.28 |   0.07 | 0.00 |    0.20 |         |         | yes     |

## Code 1.10 mixed model with `Mplus`, strata as fixed effects, using provided wa1, wa2

``` r
# -------------------------------------------------------------------
# mixed model with Mplus
# -------------------------------------------------------------------

#------------------------------------------------
# prepare data for Mplus
#------------------------------------------------

data_mixed <- data_model %>%
              dplyr::filter(ctry_name == 'Paraguay') %>%
              rename_all(tolower) %>%
              mutate(strat_1 = dplyr::if_else(id_s == 66, 1, 0 )) %>%
              mutate(strat_2 = dplyr::if_else(id_s == 67, 1, 0 )) %>%
              mutate(strat_3 = dplyr::if_else(id_s == 68, 1, 0 )) %>%
              mutate(strat_4 = dplyr::if_else(id_s == 69, 1, 0 )) %>%
              mutate(strat_5 = dplyr::if_else(id_s == 70, 1, 0 )) %>%
              mutate(strat_6 = dplyr::if_else(id_s == 71, 1, 0 )) %>%
              mutate(strat_7 = dplyr::if_else(id_s == 72, 1, 0 )) %>%
              dplyr::select(
              mat_1, mat_2, mat_3, mat_4, mat_5,
              id_k, id_s, id_j, id_i,
              wa1, wa2, wi, wj,
              z_w, z_b, v_b,
#              strat_1, # strata of reference
              strat_2,
              strat_3,
              strat_4,
              strat_5,
              strat_6,
              strat_7
              )

#------------------------------------------------
# list of imputed data
#------------------------------------------------

data_1 <- data_mixed %>% mutate(y = mat_1)
data_2 <- data_mixed %>% mutate(y = mat_2)
data_3 <- data_mixed %>% mutate(y = mat_3)
data_4 <- data_mixed %>% mutate(y = mat_4)
data_5 <- data_mixed %>% mutate(y = mat_5)

data_list <- list(
data_1,
data_2,
data_3,
data_4,
data_5
)

#------------------------------------------------
# create Mplus object
#------------------------------------------------

mplus_model <- MplusAutomation::mplusObject(
MODEL = '
%WITHIN%

y on z_w;

%BETWEEN%

y on v_b;
y on z_b;
y on strat_2; 
y on strat_3; 
y on strat_4; 
y on strat_5; 
y on strat_6; 
y on strat_7; 

',
ANALYSIS = '
TYPE = TWOLEVEL;
ESTIMATOR = MLR;
PROCESSORS = 4;
',
VARIABLE ='
CLUSTER  = id_j;
WEIGHT   = wa1;
BWEIGHT  = wa2;
WTSCALE  = UNSCALED;
BWTSCALE = UNSCALED;

USEVARIABLES =
y
z_w
v_b
z_b
strat_2
strat_3
strat_4
strat_5
strat_6
strat_7
;

WITHIN  = z_w;
BETWEEN = v_b z_b strat_2-strat_7;

',
OUTPUT ='
STANDARDIZED
CINTERVAL
RESIDUAL
;
',
rdata = data_list,
imputed = TRUE
)


#------------------------------------------------
# list of data
#------------------------------------------------


mlm_model_data_list <-
'
mlm_imp_1.dat
mlm_imp_2.dat
mlm_imp_3.dat
mlm_imp_4.dat
mlm_imp_5.dat
'

write(mlm_model_data_list, file="mlm.dat", ncolumns=1)

#------------------------------------------------
# fit models
#------------------------------------------------

fit_10 <- MplusAutomation::mplusModeler(mplus_model,
             modelout = 'mlm.inp',
             dataout = 'mlm.dat',
             run = 1L,
             hashfilename = FALSE,
             writeData = 'always'
             )
```

    ## The file(s)
    ##  'mlm_imp_1.dat;
    ## mlm_imp_2.dat;
    ## mlm_imp_3.dat;
    ## mlm_imp_4.dat;
    ## mlm_imp_5.dat' 
    ## currently exist(s) and will be overwritten

``` r
#------------------------------------------------
# extract estimates
#------------------------------------------------


ci_00  <- fit_10 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('ci.unstandardized') %>%
          tibble::tibble() %>%
          mutate(term = param)  %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(ll   = low2.5) %>%
          mutate(ul   = up2.5)  %>%
          mutate(index = paste0(paramHeader, "_", term,"_",level)) %>%
          dplyr::select(index, ll, ul)

r2_00  <- fit_10 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('r2') %>%
          tibble::tibble() %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(term = c('r2_w', 'r2_b')) %>%
          mutate(object = c('r2', 'r2')) %>%
          mutate(e  = as.numeric(est)) %>%
          mutate(se = as.numeric(se)) %>%
          mutate(p  = as.numeric(pval)) %>%
          mutate(missing  = as.numeric(rate_missing)) %>%
          mutate(index = paste0(term,"_",level)) %>%
          mutate(control = 'yes') %>%
          dplyr::select(index, level, object, term, e, se, p, missing, control)

tabla_1_10 <- fit_10 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('unstandardized') %>%
          tibble::tibble() %>%
          mutate(term = param) %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(e  = est) %>%
          mutate(se = se) %>%
          mutate(p  = pval) %>%
          mutate(missing  = rate_missing) %>%
          mutate(index = paste0(paramHeader, "_", term,"_",level)) %>%
          dplyr::left_join(., ci_00, by = 'index') %>%
          mutate(control = 'yes') %>%
          mutate(object = c(           'pi_w', 'epsilon', 
                            'gamma_b', 'pi_b', 
                            'delta_b1','delta_b2','delta_b3',
                            'delta_b4','delta_b5','delta_b6',
                            'alpha', 'mu')) %>%
          dplyr::bind_rows(., r2_00) %>%
          dplyr::select(level, object, term, e, se, p, missing, ll, ul, control)

#------------------------------------------------
# display estimates
#------------------------------------------------

tabla_1_10 %>%
knitr::kable(., digits = 2)
```

| level   | object   | term    |       e |     se |    p | missing |      ll |      ul | control |
|:--------|:---------|:--------|--------:|-------:|-----:|--------:|--------:|--------:|:--------|
| within  | pi_w     | Z_W     |   12.51 |   2.26 | 0.00 |    0.27 |    8.09 |   16.94 | yes     |
| within  | epsilon  | Y       | 4492.12 | 169.39 | 0.00 |    0.35 | 4160.12 | 4824.12 | yes     |
| between | gamma_b  | V_B     |   20.52 |   7.90 | 0.01 |    0.05 |    5.03 |   36.01 | yes     |
| between | pi_b     | Z_B     |   25.99 |   9.36 | 0.00 |    0.17 |    7.66 |   44.33 | yes     |
| between | delta_b1 | STRAT_2 |   30.39 |  10.46 | 0.00 |    0.10 |    9.89 |   50.89 | yes     |
| between | delta_b2 | STRAT_3 |   53.32 |  15.42 | 0.00 |    0.11 |   23.10 |   83.53 | yes     |
| between | delta_b3 | STRAT_4 |   39.95 |  12.97 | 0.00 |    0.54 |   14.53 |   65.37 | yes     |
| between | delta_b4 | STRAT_5 |   48.30 |   9.77 | 0.00 |    0.05 |   29.16 |   67.44 | yes     |
| between | delta_b5 | STRAT_6 |   33.90 |  40.72 | 0.41 |    0.04 |  -45.92 |  113.72 | yes     |
| between | delta_b6 | STRAT_7 |  -15.98 |  17.46 | 0.36 |    0.33 |  -50.22 |   18.25 | yes     |
| between | alpha    | Y       |  629.46 |   4.21 | 0.00 |    0.05 |  621.22 |  637.71 | yes     |
| between | mu       | Y       | 2155.03 | 452.29 | 0.00 |    0.11 | 1268.55 | 3041.52 | yes     |
| within  | r2       | r2_w    |    0.01 |   0.00 | 0.01 |    0.30 |         |         | yes     |
| between | r2       | r2_b    |    0.28 |   0.07 | 0.00 |    0.20 |         |         | yes     |

## Code 1.11 mixed model with `Mplus`, strata as fixed effects, without using weights

``` r
# -------------------------------------------------------------------
# mixed model with Mplus
# -------------------------------------------------------------------

#------------------------------------------------
# prepare data for Mplus
#------------------------------------------------

data_mixed <- data_model %>%
              dplyr::filter(ctry_name == 'Paraguay') %>%
              rename_all(tolower) %>%
              mutate(strat_1 = dplyr::if_else(id_s == 66, 1, 0 )) %>%
              mutate(strat_2 = dplyr::if_else(id_s == 67, 1, 0 )) %>%
              mutate(strat_3 = dplyr::if_else(id_s == 68, 1, 0 )) %>%
              mutate(strat_4 = dplyr::if_else(id_s == 69, 1, 0 )) %>%
              mutate(strat_5 = dplyr::if_else(id_s == 70, 1, 0 )) %>%
              mutate(strat_6 = dplyr::if_else(id_s == 71, 1, 0 )) %>%
              mutate(strat_7 = dplyr::if_else(id_s == 72, 1, 0 )) %>%
              dplyr::select(
              mat_1, mat_2, mat_3, mat_4, mat_5,
              id_k, id_s, id_j, id_i,
              wa1, wa2, wi, wj,
              z_w, z_b, v_b,
#              strat_1, # strata of reference
              strat_2,
              strat_3,
              strat_4,
              strat_5,
              strat_6,
              strat_7
              )

#------------------------------------------------
# list of imputed data
#------------------------------------------------

data_1 <- data_mixed %>% mutate(y = mat_1)
data_2 <- data_mixed %>% mutate(y = mat_2)
data_3 <- data_mixed %>% mutate(y = mat_3)
data_4 <- data_mixed %>% mutate(y = mat_4)
data_5 <- data_mixed %>% mutate(y = mat_5)

data_list <- list(
data_1,
data_2,
data_3,
data_4,
data_5
)

#------------------------------------------------
# create Mplus object
#------------------------------------------------

mplus_model <- MplusAutomation::mplusObject(
MODEL = '
%WITHIN%

y on z_w;

%BETWEEN%

y on v_b;
y on z_b;
y on strat_2; 
y on strat_3; 
y on strat_4; 
y on strat_5; 
y on strat_6; 
y on strat_7; 

',
ANALYSIS = '
TYPE = TWOLEVEL;
ESTIMATOR = MLR;
PROCESSORS = 4;
',
VARIABLE ='
CLUSTER  = id_j;
!WEIGHT   = wa1;
!BWEIGHT  = wa2;
!WTSCALE  = UNSCALED;
!BWTSCALE = UNSCALED;

USEVARIABLES =
y
z_w
v_b
z_b
strat_2
strat_3
strat_4
strat_5
strat_6
strat_7
;

WITHIN  = z_w;
BETWEEN = v_b z_b strat_2-strat_7;

',
OUTPUT ='
STANDARDIZED
CINTERVAL
RESIDUAL
;
',
rdata = data_list,
imputed = TRUE
)


#------------------------------------------------
# list of data files
#------------------------------------------------


mlm_model_data_list <-
'
mlm_imp_1.dat
mlm_imp_2.dat
mlm_imp_3.dat
mlm_imp_4.dat
mlm_imp_5.dat
'

write(mlm_model_data_list, file="mlm.dat", ncolumns=1)

#------------------------------------------------
# fit models
#------------------------------------------------

fit_11 <- MplusAutomation::mplusModeler(mplus_model,
             modelout = 'mlm.inp',
             dataout = 'mlm.dat',
             run = 1L,
             hashfilename = FALSE,
             writeData = 'always'
             )
```

    ## The file(s)
    ##  'mlm_imp_1.dat;
    ## mlm_imp_2.dat;
    ## mlm_imp_3.dat;
    ## mlm_imp_4.dat;
    ## mlm_imp_5.dat' 
    ## currently exist(s) and will be overwritten

``` r
#------------------------------------------------
# extract estimates
#------------------------------------------------


ci_00  <- fit_11 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('ci.unstandardized') %>%
          tibble::tibble() %>%
          mutate(term = param)  %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(ll   = low2.5) %>%
          mutate(ul   = up2.5)  %>%
          mutate(index = paste0(paramHeader, "_", term,"_",level)) %>%
          dplyr::select(index, ll, ul)

r2_00  <- fit_11 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('r2') %>%
          tibble::tibble() %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(term = c('r2_w', 'r2_b')) %>%
          mutate(object = c('r2', 'r2')) %>%
          mutate(e  = as.numeric(est)) %>%
          mutate(se = as.numeric(se)) %>%
          mutate(p  = as.numeric(pval)) %>%
          mutate(missing  = as.numeric(rate_missing)) %>%
          mutate(index = paste0(term,"_",level)) %>%
          mutate(control = 'yes') %>%
          dplyr::select(index, level, object, term, e, se, p, missing, control)

tabla_1_11 <- fit_11 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('unstandardized') %>%
          tibble::tibble() %>%
          mutate(term = param) %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(e  = est) %>%
          mutate(se = se) %>%
          mutate(p  = pval) %>%
          mutate(missing  = rate_missing) %>%
          mutate(index = paste0(paramHeader, "_", term,"_",level)) %>%
          dplyr::left_join(., ci_00, by = 'index') %>%
          mutate(control = 'yes') %>%
          mutate(object = c(           'pi_w', 'epsilon', 
                            'gamma_b', 'pi_b', 
                            'delta_b1','delta_b2','delta_b3',
                            'delta_b4','delta_b5','delta_b6',
                            'alpha', 'mu')) %>%
          dplyr::bind_rows(., r2_00) %>%
          dplyr::select(level, object, term, e, se, p, missing, ll, ul, control)

#------------------------------------------------
# display estimates
#------------------------------------------------

tabla_1_11 %>%
knitr::kable(., digits = 2)
```

| level   | object   | term    |       e |     se |    p | missing |      ll |      ul | control |
|:--------|:---------|:--------|--------:|-------:|-----:|--------:|--------:|--------:|:--------|
| within  | pi_w     | Z_W     |   12.69 |   1.79 | 0.00 |    0.37 |    9.19 |   16.19 | yes     |
| within  | epsilon  | Y       | 4579.00 | 121.29 | 0.00 |    0.13 | 4341.28 | 4816.72 | yes     |
| between | gamma_b  | V_B     |   11.96 |   5.98 | 0.05 |    0.01 |    0.23 |   23.68 | yes     |
| between | pi_b     | Z_B     |   42.73 |   8.74 | 0.00 |    0.07 |   25.60 |   59.86 | yes     |
| between | delta_b1 | STRAT_2 |   27.09 |   8.12 | 0.00 |    0.03 |   11.18 |   43.00 | yes     |
| between | delta_b2 | STRAT_3 |   36.99 |  11.94 | 0.00 |    0.04 |   13.60 |   60.39 | yes     |
| between | delta_b3 | STRAT_4 |   22.30 |  11.54 | 0.05 |    0.54 |   -0.31 |   44.92 | yes     |
| between | delta_b4 | STRAT_5 |   38.33 |   8.40 | 0.00 |    0.02 |   21.86 |   54.79 | yes     |
| between | delta_b5 | STRAT_6 |   35.57 |  33.14 | 0.28 |    0.04 |  -29.38 |  100.52 | yes     |
| between | delta_b6 | STRAT_7 |   10.38 |  36.81 | 0.78 |    0.09 |  -61.77 |   82.53 | yes     |
| between | alpha    | Y       |  637.36 |   3.36 | 0.00 |    0.11 |  630.77 |  643.94 | yes     |
| between | mu       | Y       | 1534.32 | 276.14 | 0.00 |    0.04 |  993.09 | 2075.56 | yes     |
| within  | r2       | r2_w    |    0.02 |   0.00 | 0.00 |    0.38 |         |         | yes     |
| between | r2       | r2_b    |    0.45 |   0.07 | 0.00 |    0.06 |         |         | yes     |

## Code 1.12 mixed model with `Mplus`, without sampling information

``` r
# -------------------------------------------------------------------
# mixed model with Mplus
# -------------------------------------------------------------------

#------------------------------------------------
# prepare data for Mplus
#------------------------------------------------

data_mixed <- data_model %>%
              dplyr::filter(ctry_name == 'Paraguay') %>%
              rename_all(tolower) %>%
              dplyr::select(
              mat_1, mat_2, mat_3, mat_4, mat_5,
              id_k, id_s, id_j, id_i,
              wb1, wb2, wi, wj,
              z_w, z_b, v_b
              )

#------------------------------------------------
# list of imputed data
#------------------------------------------------

data_1 <- data_mixed %>% mutate(y = mat_1)
data_2 <- data_mixed %>% mutate(y = mat_2)
data_3 <- data_mixed %>% mutate(y = mat_3)
data_4 <- data_mixed %>% mutate(y = mat_4)
data_5 <- data_mixed %>% mutate(y = mat_5)

data_list <- list(
data_1,
data_2,
data_3,
data_4,
data_5
)

#------------------------------------------------
# create Mplus object
#------------------------------------------------

mplus_model <- MplusAutomation::mplusObject(
MODEL = '
%WITHIN%

y on z_w;

%BETWEEN%

y on v_b;
y on z_b;

',
ANALYSIS = '
TYPE = TWOLEVEL;
ESTIMATOR = MLR;
PROCESSORS = 4;
',
VARIABLE ='
CLUSTER  = id_j;
WEIGHT   = wi;
BWEIGHT  = wj;
WTSCALE  = ECLUSTER;
BWTSCALE = SAMPLE;

USEVARIABLES =
y
z_w
v_b
z_b
;

WITHIN  = z_w;
BETWEEN = v_b z_b;

',
OUTPUT ='
STANDARDIZED
CINTERVAL
RESIDUAL
;
',
rdata = data_list,
imputed = TRUE
)


#------------------------------------------------
# list of data
#------------------------------------------------


mlm_model_data_list <-
'
mlm_imp_1.dat
mlm_imp_2.dat
mlm_imp_3.dat
mlm_imp_4.dat
mlm_imp_5.dat
'

write(mlm_model_data_list, file="mlm.dat", ncolumns=1)

#------------------------------------------------
# fit models
#------------------------------------------------

fit_12 <- MplusAutomation::mplusModeler(mplus_model,
             modelout = 'mlm.inp',
             dataout = 'mlm.dat',
             run = 1L,
             hashfilename = FALSE,
             writeData = 'always'
             )
```

    ## The file(s)
    ##  'mlm_imp_1.dat;
    ## mlm_imp_2.dat;
    ## mlm_imp_3.dat;
    ## mlm_imp_4.dat;
    ## mlm_imp_5.dat' 
    ## currently exist(s) and will be overwritten

``` r
#------------------------------------------------
# extract estimates
#------------------------------------------------


ci_00  <- fit_12 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('ci.unstandardized') %>%
          tibble::tibble() %>%
          mutate(term = param)  %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(ll   = low2.5) %>%
          mutate(ul   = up2.5)  %>%
          mutate(index = paste0(paramHeader, "_", term,"_",level)) %>%
          dplyr::select(index, ll, ul)

r2_00  <- fit_12 %>%
          purrr::pluck('results') %>%
          purrr::pluck('parameters') %>%
          purrr::pluck('r2') %>%
          tibble::tibble() %>%
          mutate(level  = tolower(BetweenWithin)) %>%
          mutate(term = c('r2_w', 'r2_b')) %>%
          mutate(object = c('r2', 'r2')) %>%
          mutate(e  = as.numeric(est)) %>%
          mutate(se = as.numeric(se)) %>%
          mutate(p  = as.numeric(pval)) %>%
          mutate(missing  = as.numeric(rate_missing)) %>%
          mutate(index = paste0(term,"_",level)) %>%
          mutate(control = 'yes') %>%
          dplyr::select(index, level, object, term, e, se, p, missing, control)

tabla_1_12 <- fit_12 %>%
             purrr::pluck('results') %>%
             purrr::pluck('parameters') %>%
             purrr::pluck('unstandardized') %>%
             tibble::tibble() %>%
             mutate(term = param) %>%
             mutate(level  = tolower(BetweenWithin)) %>%
             mutate(e  = est) %>%
             mutate(se = se) %>%
             mutate(p  = pval) %>%
             mutate(missing  = rate_missing) %>%
             mutate(index = paste0(paramHeader, "_", term,"_",level)) %>%
             dplyr::left_join(., ci_00, by = 'index') %>%
             mutate(control = 'yes') %>%
             mutate(object = c(           'pi_w', 'epsilon', 
                               'gamma_b', 'pi_b', 'alpha', 'mu')) %>%
             dplyr::bind_rows(., r2_00) %>%
             dplyr::select(level, object, term, e, se, p, missing, ll, ul, control)

#------------------------------------------------
# Display table
#------------------------------------------------

tabla_1_12 %>%
knitr::kable(., digits = 2)
```

| level   | object  | term |       e |     se |    p | missing |      ll |      ul | control |
|:--------|:--------|:-----|--------:|-------:|-----:|--------:|--------:|--------:|:--------|
| within  | pi_w    | Z_W  |   12.51 |   2.26 | 0.00 |    0.27 |    8.09 |   16.94 | yes     |
| within  | epsilon | Y    | 4490.40 | 168.40 | 0.00 |    0.34 | 4160.33 | 4820.48 | yes     |
| between | gamma_b | V_B  |   22.51 |  10.52 | 0.03 |    0.05 |    1.89 |   43.12 | yes     |
| between | pi_b    | Z_B  |   28.10 |   7.04 | 0.00 |    0.11 |   14.30 |   41.89 | yes     |
| between | alpha   | Y    |  655.07 |   4.78 | 0.00 |    0.14 |  645.71 |  664.42 | yes     |
| between | mu      | Y    | 2480.01 | 471.79 | 0.00 |    0.10 | 1555.30 | 3404.73 | yes     |
| within  | r2      | r2_w |    0.01 |   0.00 | 0.01 |    0.30 |         |         | yes     |
| between | r2      | r2_b |    0.18 |   0.07 | 0.01 |    0.14 |         |         | yes     |

## Comparison

``` r
# -------------------------------------------------------------------
# comparing results
# -------------------------------------------------------------------

#------------------------------------------------
# display estimates
#------------------------------------------------

texreg::screenreg(list(fit_11, fit_4, fit_8, fit_9, fit_10), 
    type = 'un',
    star.symbol = "*", 
    center = TRUE, 
    doctype = FALSE,
    dcolumn = TRUE, 
    booktabs = TRUE,
    single.row=TRUE,
    custom.model.names = 
    c(
      'no design',
      'with design (mplus wb2)',
      'STRATA as fixed effects',
      'STRATA as fixed effects (wb2)',
      'STRATA as fixed effects (wa2)'
      )
    )
```

    ## 
    ## =====================================================================================================================================================
    ##                  no design             with design (mplus wb2)  STRATA as fixed effects  STRATA as fixed effects (wb2)  STRATA as fixed effects (wa2)
    ## -----------------------------------------------------------------------------------------------------------------------------------------------------
    ## W Y<-Z_W           12.69   (1.79) ***    12.51   (2.25) ***       12.51   (2.26) ***       12.51   (2.26) ***             12.51   (2.26) ***         
    ## B Y<-V_B           11.96   (5.98) *      22.51  (10.39) *         20.52   (7.90) **        20.52   (7.90) **              20.52   (7.90) **          
    ## B Y<-Z_B           42.73   (8.74) ***    28.10   (6.91) ***       25.99   (9.36) **        25.99   (9.36) **              25.99   (9.36) **          
    ## B Y<-STRAT_2       27.09   (8.12) **                              30.39  (10.46) **        30.39  (10.46) **              30.39  (10.46) **          
    ## B Y<-STRAT_3       36.99  (11.94) **                              53.32  (15.42) **        53.32  (15.42) **              53.32  (15.42) **          
    ## B Y<-STRAT_4       22.30  (11.54)                                 39.95  (12.97) **        39.95  (12.97) **              39.95  (12.97) **          
    ## B Y<-STRAT_5       38.33   (8.40) ***                             48.30   (9.77) ***       48.30   (9.77) ***             48.30   (9.77) ***         
    ## B Y<-STRAT_6       35.57  (33.14)                                 33.90  (40.72)           33.90  (40.72)                 33.90  (40.72)             
    ## B Y<-STRAT_7       10.38  (36.81)                                -15.98  (17.46)          -15.98  (17.46)                -15.98  (17.46)             
    ## B Y<-Intercepts   637.36   (3.36) ***   655.07   (4.60) ***      629.46   (4.21) ***      629.46   (4.21) ***            629.46   (4.21) ***         
    ## W Y<->Y          4579.00 (121.29) ***  4490.40 (167.57) ***     4492.12 (169.39) ***     4492.12 (169.39) ***           4492.12 (169.39) ***         
    ## B Y<->Y          1534.32 (276.14) ***  2480.01 (471.29) ***     2155.03 (452.29) ***     2155.03 (452.29) ***           2155.03 (452.29) ***         
    ## =====================================================================================================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05

``` r
#------------------------------------------------
# with and without stratification
#------------------------------------------------

texreg::screenreg(list(fit_4, fit_12), 
    type = 'un',
    star.symbol = "*", 
    center = TRUE, 
    doctype = FALSE,
    dcolumn = TRUE, 
    booktabs = TRUE,
    single.row=TRUE,
    custom.model.names = 
    c(
      'with design (mplus wb2)',
      'no strata info'
      )
    )
```

    ## 
    ## ==============================================================
    ##                  with design (mplus wb2)  no strata info      
    ## --------------------------------------------------------------
    ## W Y<-Z_W           12.51   (2.25) ***       12.51   (2.26) ***
    ## B Y<-V_B           22.51  (10.39) *         22.51  (10.52) *  
    ## B Y<-Z_B           28.10   (6.91) ***       28.10   (7.04) ***
    ## B Y<-Intercepts   655.07   (4.60) ***      655.07   (4.78) ***
    ## W Y<->Y          4490.40 (167.57) ***     4490.40 (168.40) ***
    ## B Y<->Y          2480.01 (471.29) ***     2480.01 (471.79) ***
    ## ==============================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05

``` r
#------------------------------------------------
# stratification as fixed effects
#------------------------------------------------

texreg::screenreg(list(fit_4, fit_8), 
    type = 'un',
    star.symbol = "*", 
    center = TRUE, 
    doctype = FALSE,
    dcolumn = TRUE, 
    booktabs = TRUE,
    single.row=TRUE,
    custom.model.names = 
    c(
      'with design (mplus wb2)',
      'stratification as fixed effects'
      )
    )
```

    ## 
    ## =========================================================================
    ##                  with design (mplus wb2)  stratification as fixed effects
    ## -------------------------------------------------------------------------
    ## W Y<-Z_W           12.51   (2.25) ***       12.51   (2.26) ***           
    ## B Y<-V_B           22.51  (10.39) *         20.52   (7.90) **            
    ## B Y<-Z_B           28.10   (6.91) ***       25.99   (9.36) **            
    ## B Y<-Intercepts   655.07   (4.60) ***      629.46   (4.21) ***           
    ## W Y<->Y          4490.40 (167.57) ***     4492.12 (169.39) ***           
    ## B Y<->Y          2480.01 (471.29) ***     2155.03 (452.29) ***           
    ## B Y<-STRAT_2                                30.39  (10.46) **            
    ## B Y<-STRAT_3                                53.32  (15.42) **            
    ## B Y<-STRAT_4                                39.95  (12.97) **            
    ## B Y<-STRAT_5                                48.30   (9.77) ***           
    ## B Y<-STRAT_6                                33.90  (40.72)               
    ## B Y<-STRAT_7                               -15.98  (17.46)               
    ## =========================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05

# Annexes 2: estimates ignoring stratification

## Code 1.13 mixed model with `STATA` without stratification

``` r
# -------------------------------------------------------------------
# mixed model with Stata
# -------------------------------------------------------------------

#------------------------------------------------
# prepare data for Stata
#------------------------------------------------

data_stata <- data_model %>%
dplyr::filter(ctry_name == 'Paraguay')

#------------------------------------------------
# configure Rstata library
#------------------------------------------------

library(RStata)
options("RStata.StataPath"='/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp')
options("RStata.StataVersion"=15)

#------------------------------------------------
# code stata
#------------------------------------------------

stata_code <- '

* ===============================================.
* define folder and open data
* ===============================================.

* ===============================.
* fit mlm model with @pv
* ===============================.

pv, pv(MAT_1 MAT_2 MAT_3 MAT_4 MAT_5): ///
mixed @pv               ///
z_w                     ///
z_b                     ///
v_b                     ///
[pw=wb1] || id_j:, ml pweight(wb2)
estimates store m01

* ===============================.
* variance
* ===============================.

/*return residual variance */
_diparm lnsig_e,  f(exp(@)^2) d(2*exp(@)^2)    
/*return between  variance */
_diparm lns1_1_1, f(exp(@)^2) d(2*exp(@)^2)  

* ===============================.
* estimates
* ===============================.

esttab m01, b(%9.2f) se(%9.2f) wide transform(ln*: exp(2*@) exp(2*@))

'

#----------------------------------------------------------
# display stata output
#----------------------------------------------------------

RStata::stata(stata_code, data.in = data_stata)
```

    ## . 
    ## . 
    ## . * ===============================================.
    ## . * define folder and open data
    ## . * ===============================================.
    ## . 
    ## . * ===============================.
    ## . * fit mlm model with @pv
    ## . * ===============================.
    ## . 
    ## . pv, pv(MAT_1 MAT_2 MAT_3 MAT_4 MAT_5): ///
    ## > mixed @pv               ///
    ## > z_w                     ///
    ## > z_b                     ///
    ## > v_b                     ///
    ## > [pw=wb1] || id_j:, ml pweight(wb2)
    ## 
    ## command(s) run for each plausible value:
    ## 
    ##      mixed @pv               z_w                     z_b                     v_b                     [pw=wb1] || id_j:, ml pweight(wb2)
    ## 
    ## Estimates for MAT_1   complete
    ## Estimates for MAT_2   complete
    ## Estimates for MAT_3   complete
    ## Estimates for MAT_4   complete
    ## Estimates for MAT_5   complete
    ## 
    ## Number of observations: 4481
    ## Average R-Squared: .
    ## 
    ## 
    ##                      Coef    Std Err          t    t Param      P>|t|
    ##      MAT_5:z_w  12.512955  2.2611123  5.5339821          .          .
    ##      MAT_5:z_b  28.119717  6.9967324  4.0189786          .          .
    ##      MAT_5:v_b  22.475981  10.545518  2.1313303          .          .
    ##    MAT_5:_cons  655.06926   4.784621  136.91142          .          .
    ## lns1_1_1:_cons  3.9059473   .0948134  41.196152          .          .
    ##  lnsig_e:_cons  4.2049035  .01879628  223.70933          .          .
    ## . estimates store m01
    ## . 
    ## . * ===============================.
    ## . * variance
    ## . * ===============================.
    ## . 
    ## . /*return residual variance */
    ## . _diparm lnsig_e,  f(exp(@)^2) d(2*exp(@)^2)    
    ##     /lnsig_e |   4490.893   168.8242                        4171.9    4834.278
    ## . /*return between  variance */
    ## . _diparm lns1_1_1, f(exp(@)^2) d(2*exp(@)^2)  
    ##    /lns1_1_1 |   2469.805   468.3413                      1703.147    3581.569
    ## . 
    ## . * ===============================.
    ## . * estimates
    ## . * ===============================.
    ## . 
    ## . esttab m01, b(%9.2f) se(%9.2f) wide transform(ln*: exp(2*@) exp(2*@))
    ## 
    ## -----------------------------------------
    ##                       (1)                
    ##                                          
    ## -----------------------------------------
    ## MAT_5                                    
    ## z_w                 12.51***       (2.26)
    ## z_b                 28.12***       (7.00)
    ## v_b                 22.48*        (10.55)
    ## _cons              655.07***       (4.78)
    ## -----------------------------------------
    ## lns1_1_1                                 
    ## _cons             2469.81***     (234.17)
    ## -----------------------------------------
    ## lnsig_e                                  
    ## _cons             4490.89***      (84.41)
    ## -----------------------------------------
    ## N                    4481                
    ## -----------------------------------------
    ## Standard errors in parentheses
    ## * p<0.05, ** p<0.01, *** p<0.001
    ## .

``` r
#----------------------------------------------------------
# create table
#----------------------------------------------------------

tabla_1_13 <- read.table(
text = "
variable            e          star       se
z_w                 12.51      '***'      2.26
z_b                 28.12      '***'      7.00
v_b                 22.48      '*  '     10.55
_cons              655.07      '***'      4.78
",
header=TRUE, stringsAsFactors = FALSE)

#------------------------------------------------
# display  table
#------------------------------------------------

tabla_1_13 %>%
knitr::kable(., digits = 2)
```

| variable |      e | star   |    se |
|:---------|-------:|:-------|------:|
| z_w      |  12.51 | \*\*\* |  2.26 |
| z_b      |  28.12 | \*\*\* |  7.00 |
| v_b      |  22.48 | \*     | 10.55 |
| \_cons   | 655.07 | \*\*\* |  4.78 |

## Code 1.14 multilevel model with `WeMix` ignorign stratification

``` r
# -------------------------------------------------------------------
# multilevel model with WeMix
# -------------------------------------------------------------------

#------------------------------------------------
# configure R console
#------------------------------------------------

options(scipen = 999)
options(digits = 10)

#------------------------------------------------
# prepare data for WeMix
#------------------------------------------------

data_wemix <- data_model %>%
              dplyr::filter(ctry_name == 'Paraguay')

#------------------------------------------------
# fit model
#------------------------------------------------

wemix_pv_results <- mitools::withPV(
   mapping = mat ~ MAT_1 + MAT_2 + MAT_3 + MAT_4 + MAT_5, #<<
   data = data_wemix,
   action = quote(
WeMix::mix(
  mat ~ 1 + z_w + z_b + v_b + (1 | id_j),
  data = data_wemix,
  weights = c('wb1','wb2'),
  cWeights = TRUE)
    ),
   rewrite=TRUE
   )
```

    ## Warning in WeMix::mix(MAT_1 ~ 1 + z_w + z_b + v_b + (1 | id_j), data =
    ## data_wemix, : There were 368 rows with missing data. These have been removed.

    ## Warning in WeMix::mix(MAT_2 ~ 1 + z_w + z_b + v_b + (1 | id_j), data =
    ## data_wemix, : There were 368 rows with missing data. These have been removed.

    ## Warning in WeMix::mix(MAT_3 ~ 1 + z_w + z_b + v_b + (1 | id_j), data =
    ## data_wemix, : There were 368 rows with missing data. These have been removed.

    ## Warning in WeMix::mix(MAT_4 ~ 1 + z_w + z_b + v_b + (1 | id_j), data =
    ## data_wemix, : There were 368 rows with missing data. These have been removed.

    ## Warning in WeMix::mix(MAT_5 ~ 1 + z_w + z_b + v_b + (1 | id_j), data =
    ## data_wemix, : There were 368 rows with missing data. These have been removed.

``` r
#------------------------------------------------
# extract estimates
#------------------------------------------------

options(scipen = 999)
options(digits = 4)

tabla_1_14 <- summary(
  miceadds::pool_mi(
  qhat = list(
    wemix_pv_results[[1]]$coef,
    wemix_pv_results[[2]]$coef,
    wemix_pv_results[[3]]$coef,
    wemix_pv_results[[4]]$coef,
    wemix_pv_results[[5]]$coef
    ), 
  se   = list(
    wemix_pv_results[[1]]$SE,
    wemix_pv_results[[2]]$SE,
    wemix_pv_results[[3]]$SE,
    wemix_pv_results[[4]]$SE,
    wemix_pv_results[[5]]$SE
    )
  )
) %>%
tibble::rownames_to_column("term") %>%
tibble::as_tibble()
```

    ## Multiple imputation results:
    ## Call: miceadds::pool_mi(qhat = list(wemix_pv_results[[1]]$coef, wemix_pv_results[[2]]$coef, 
    ##     wemix_pv_results[[3]]$coef, wemix_pv_results[[4]]$coef, wemix_pv_results[[5]]$coef), 
    ##     se = list(wemix_pv_results[[1]]$SE, wemix_pv_results[[2]]$SE, 
    ##         wemix_pv_results[[3]]$SE, wemix_pv_results[[4]]$SE, wemix_pv_results[[5]]$SE))
    ##             results     se       t
    ## (Intercept)  655.07  4.785 136.911
    ## z_w           12.51  2.261   5.534
    ## z_b           28.12  6.997   4.019
    ## v_b           22.48 10.546   2.131
    ##                                                                                                                                                                                                                                            p
    ## (Intercept) 0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009604
    ## z_w         0.000000606808194612780723595582294677130974491774395573884248733520507812500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
    ## z_b         0.000073243804656854551138854547698286978629766963422298431396484375000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
    ## v_b         0.033214235936485730915634917437273543328046798706054687500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
    ##              (lower upper) missInfo
    ## (Intercept) 645.641 664.50   14.1 %
    ## z_w           7.997  17.03   27.1 %
    ## z_b          14.353  41.89   11.9 %
    ## v_b           1.791  43.16    5.1 %

``` r
#------------------------------------------------
# display table
#------------------------------------------------

tabla_1_14 %>%
knitr::kable(., digits = 2)
```

| term        | results |    se |      t |    p | (lower | upper) | missInfo |
|:------------|--------:|------:|-------:|-----:|-------:|-------:|:---------|
| (Intercept) |  655.07 |  4.78 | 136.91 | 0.00 | 645.64 | 664.50 | 14.1 %   |
| z_w         |   12.51 |  2.26 |   5.53 | 0.00 |   8.00 |  17.03 | 27.1 %   |
| z_b         |   28.12 |  7.00 |   4.02 | 0.00 |  14.35 |  41.89 | 11.9 %   |
| v_b         |   22.48 | 10.55 |   2.13 | 0.03 |   1.79 |  43.16 | 5.1 %    |
