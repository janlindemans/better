
<!-- README.md is generated from README.Rmd. Please edit that file -->

# better

<!-- badges: start -->
<!-- badges: end -->

The goal of better is to make the world a better place through impact
evaluation.

## Installation

You can install the development version of better from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("janlindemans/better")
```

## Set up

Create a folder to save the Global Burden of Disease project data to.
You can create it anywhere and name it anything. It may be something
like: `Users/johnwilliam/data/Global Burdern of Disease Study data`.
Let’s call it the GBD folder.

Enter the path of the GBD folder as the default GBD path in your
`.Rprofile` file - it will be used by most `better` functions.

``` r
options(better.gbd_path = paste0(
  "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared/IRB Projects",
  "/Health Projects/Health Team/Public datasets on health",
  "/Global Burden of Disease Study Data" 
))
```

If you haven’t used `.Rprofile` before, it’s a hidden file located
somewhere like here: `Users/johnwilliam/.Rprofile`. Learn more about
`.Rprofile` on [the Posit Support
page](https://support.posit.co/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf).

Download the Global Burden of Disease project data. The `gbd_download()`
convenience function (see below) will open the website where you can
download it. It also opens some other useful pages, as well as your GBD
folder where you’ll want to store the data. You can store it anywhere in
(a subfolder of) your GBD folder. Just don’t change the names of the
data files and codebook files, because `better` searches for them using
these names.

## Usage

``` r
library(better)

# Download, clean and save GBD data and codebooks

gbd_download()
#> The page to download data and the user guide are opened in your browser.
#> Your GBD data folder is also opened. Download the data you want, and save it
#> somewhere in the GBD data folder or a subfolder. It doesn't matter where, as
#> long as you don't change the name of the data file.
#> 
#> Webpages opened:
#> - guide: https://ghdx.healthdata.org/sites/default/files/ihme_query_tool/
#> GBD_Results_Tool_User_Guide_2019.pdf
#> - main: https://ghdx.healthdata.org/gbd-2019
#> - results: https://vizhub.healthdata.org/gbd-results/
#> 
#> Folder opened:
#> /Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared/IRB Projects/Health
#> Projects/Health Team/Public datasets on health/Global Burden of Disease Study
#> Data
gbd_save()

# Load and analyze

gbd_list <- gbd_load()

str(gbd_list, max.level = 2) # nested lists
#> List of 2
#>  $ gbd     : tibble [4,950 × 18] (S3: tbl_df/tbl/data.frame)
#>  $ gbd_meta:List of 4
#>   ..$ gbd_cb        : tibble [18 × 4] (S3: tbl_df/tbl/data.frame)
#>   ..$ gbd_cb_cause  : tibble [364 × 9] (S3: tbl_df/tbl/data.frame)
#>   ..$ gbd_cb_measure: tibble [10 × 6] (S3: tbl_df/tbl/data.frame)
#>   ..$ gbd_cb_risk   : tibble [200 × 6] (S3: tbl_df/tbl/data.frame)
attach(gbd_list) # easier
gbd
#> # A tibble: 4,950 × 18
#>    measure  locat…¹ sex   age   cause rei   metric  year     val   upper   lower
#>    <chr>    <chr>   <chr> <chr> <chr> <chr> <chr>  <dbl>   <dbl>   <dbl>   <dbl>
#>  1 YLDs (Y… United… Male  All … All … Hear… Number  2019 1.20e+6 1.71e+6 8.27e+5
#>  2 YLDs (Y… United… Fema… All … All … Hear… Number  2019 1.03e+6 1.42e+6 7.25e+5
#>  3 YLDs (Y… United… Both  All … All … Hear… Number  2019 2.24e+6 3.11e+6 1.56e+6
#>  4 YLDs (Y… United… Male  All … All … Hear… Perce…  2019 4.22e-2 5.55e-2 3.24e-2
#>  5 YLDs (Y… United… Fema… All … All … Hear… Perce…  2019 3.02e-2 3.88e-2 2.36e-2
#>  6 YLDs (Y… United… Both  All … All … Hear… Perce…  2019 3.56e-2 4.65e-2 2.76e-2
#>  7 YLDs (Y… United… Male  All … All … Hear… Rate    2019 7.47e+2 1.06e+3 5.12e+2
#>  8 YLDs (Y… United… Fema… All … All … Hear… Rate    2019 6.19e+2 8.51e+2 4.35e+2
#>  9 YLDs (Y… United… Both  All … All … Hear… Rate    2019 6.82e+2 9.49e+2 4.75e+2
#> 10 YLDs (Y… United… Male  All … All … Hear… Number  2019 3.09e+5 4.35e+5 2.05e+5
#> # … with 4,940 more rows, 7 more variables: measure_id <dbl>,
#> #   location_id <dbl>, sex_id <dbl>, age_id <dbl>, cause_id <dbl>,
#> #   rei_id <dbl>, metric_id <dbl>, and abbreviated variable name ¹​location

str(gbd_meta, max.level = 2) # nested lists
#> List of 4
#>  $ gbd_cb        : tibble [18 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ gbd_cb_cause  : tibble [364 × 9] (S3: tbl_df/tbl/data.frame)
#>  $ gbd_cb_measure: tibble [10 × 6] (S3: tbl_df/tbl/data.frame)
#>  $ gbd_cb_risk   : tibble [200 × 6] (S3: tbl_df/tbl/data.frame)
attach(gbd_meta) # easier
gbd_cb
#> # A tibble: 18 × 4
#>    variable     label                                  value_coding  variable_…¹
#>    <chr>        <chr>                                  <named list>  <chr>      
#>  1 measure_id   Measure ID                             <chr [10]>    measure_id 
#>  2 measure      Measure Name                           <chr [10]>    measure_na…
#>  3 location_id  Location ID                            <chr [1,758]> location_id
#>  4 location     Location Name                          <chr [1,758]> location_n…
#>  5 sex_id       Sex ID                                 <chr [4]>     sex_id     
#>  6 sex_label    Sex                                    <chr [4]>     sex_label  
#>  7 age_group_id Age Group ID                           <chr [58]>    age_group_…
#>  8 age_group    Age Group Name                         <chr [58]>    age_group_…
#>  9 cause_id     Cause ID                               <chr [364]>   cause_id   
#> 10 cause        Cause Name                             <chr [364]>   cause_name 
#> 11 rei_id       REI ID                                 <chr [200]>   rei_id     
#> 12 rei          REI Name                               <chr [200]>   rei_name   
#> 13 metric_id    Metric ID                              <chr [4]>     metric_id  
#> 14 metric       Metric Name                            <chr [4]>     metric_name
#> 15 year_id      Year ID                                <chr [30]>    year_id    
#> 16 val          Value                                  <chr [0]>     val        
#> 17 upper        95% Uncertainty Interval - Upper Bound <chr [0]>     upper      
#> 18 lower        95% Uncertainty Interval - Lower Bound <chr [0]>     lower      
#> # … with abbreviated variable name ¹​variable_origi
gbd_cb_cause
#> # A tibble: 364 × 9
#>    cause_id cause_name     paren…¹ paren…² level cause…³ sort_…⁴ yll_o…⁵ yld_o…⁶
#>       <dbl> <chr>            <dbl> <chr>   <dbl> <chr>     <dbl> <chr>   <chr>  
#>  1      294 All causes         294 All ca…     0 Total         1 <NA>    <NA>   
#>  2      295 Communicable,…     294 All ca…     1 A             2 <NA>    <NA>   
#>  3      955 HIV/AIDS and …     295 Commun…     2 A.1           3 <NA>    <NA>   
#>  4      298 HIV/AIDS           955 HIV/AI…     3 A.1.1         4 <NA>    <NA>   
#>  5      948 HIV/AIDS - Dr…     298 HIV/AI…     4 A.1.1.1       5 <NA>    <NA>   
#>  6      949 HIV/AIDS - Mu…     298 HIV/AI…     4 A.1.1.2       6 <NA>    <NA>   
#>  7      950 HIV/AIDS - Ex…     298 HIV/AI…     4 A.1.1.3       7 <NA>    <NA>   
#>  8      300 HIV/AIDS resu…     298 HIV/AI…     4 A.1.1.4       8 <NA>    <NA>   
#>  9      393 Sexually tran…     955 HIV/AI…     3 A.1.2         9 <NA>    <NA>   
#> 10      394 Syphilis           393 Sexual…     4 A.1.2.1      10 <NA>    <NA>   
#> # … with 354 more rows, and abbreviated variable names ¹​parent_id,
#> #   ²​parent_name, ³​cause_outline, ⁴​sort_order, ⁵​yll_only, ⁶​yld_only
gbd_cb_measure
#> # A tibble: 10 × 6
#>    measure                                number     percent rate  years proba…¹
#>    <chr>                                  <chr>      <chr>   <chr> <chr> <chr>  
#>  1 Deaths                                 Number of… Propor… Deat… n/a   n/a    
#>  2 Disability adjusted life years (DALYs) Number of… Propor… DALY… n/a   n/a    
#>  3 Years of life lost (YLLs)              Number of… Propor… YLLs… n/a   n/a    
#>  4 Years lived with disability (YLDs)     Number of… Propor… YLDs… n/a   n/a    
#>  5 Prevalence                             Total num… Propor… Tota… n/a   n/a    
#>  6 Incidence                              Number of… Propor… New … n/a   n/a    
#>  7 Maternal mortality ratio (MMR)         n/a        n/a     Deat… n/a   n/a    
#>  8 Life expectancy                        n/a        n/a     n/a   Year… n/a    
#>  9 Healthy life expectancy (HALE)         n/a        n/a     n/a   Year… n/a    
#> 10 Summary exposure value (SEV)           n/a        n/a     0 to… n/a   n/a    
#> # … with abbreviated variable name ¹​probability_of_death
gbd_cb_risk
#> # A tibble: 200 × 6
#>    rei_id rei_name                                 paren…¹ paren…² level sort_…³
#>     <dbl> <chr>                                      <dbl> <chr>   <dbl>   <dbl>
#>  1    169 All risk factors                             169 All ri…     0       1
#>  2    202 Environmental/occupational risks             169 All ri…     1       2
#>  3    203 Behavioral risks                             169 All ri…     1       3
#>  4    104 Metabolic risks                              169 All ri…     1       4
#>  5    171 Etiologies                                   171 Etiolo…     0       5
#>  6    191 Impairments                                  191 Impair…     0       6
#>  7    362 Injuries                                     362 Injuri…     0       7
#>  8     82 Unsafe water, sanitation, and handwashi…     202 Enviro…     2       8
#>  9     83 Unsafe water source                           82 Unsafe…     3       9
#> 10     84 Unsafe sanitation                             82 Unsafe…     3      10
#> # … with 190 more rows, and abbreviated variable names ¹​parent_id,
#> #   ²​parent_name, ³​sort_order
```
