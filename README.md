
<!-- Edit this to prevent git error: 9827634 -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

# better

<!-- badges: start -->
<!-- badges: end -->

IMPORTANT: THIS PACKAGE IS STILL UNDER DEVELOPMENT. IT’S NOT YET
INTENDED FOR USE. IT’S HERE FOR DEVELOPMENT PURPOSES ONLY.

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

<!-- COPY from better vignette: edit original there -->

`better` has several functions that rely on data from the Global Burden
of Disease study (GBD). This requires some setup.

### Download Global Burden of Disease codebook and data

Download the **GBD codebook** from the website of the Institute for
Health Metrics and Evaluation (IHME) by clicking
[here](https://www.healthdata.org/sites/default/files/files/Data_viz/GBD_2019_Data_Tools_Guide.zip).

Save the downloaded folder `IHME_GBD_2019_CODEBOOK` into a root folder
in which you’ll later also save GBD data. Let’s call this root folder
the *GBD folder*. You can create the GBD folder anywhere and name it
whatever you like. Your GBD folder could be something like:
`Users/johnwilliam/datasets/Global Burden of Disease`, with the codebook
folder
`Users/johnwilliam/datasets/Global Burden of Disease/IHME_GBD_2019_CODEBOOK`.

Download your first **GBD data** from the Global Health Data Exchange
website. Click [here](https://vizhub.healthdata.org/gbd-results/) to go
to the website.

You’ll need to create an account first. Click `Sign in`, then
`Register`, etc.

Then go back to the [GBD Results
page](https://vizhub.healthdata.org/gbd-results/). It’s a huge database,
so you can’t download everything.

Start by downloading a basic dataset on *causes of death*, with the
following parameters in the drop-down menus on the page:

- `GBD Estimate`: Keep that one as is, so `Cause of death or injury`.
- `Measure`: Keep `Deaths` and `DALYs`, and add `YLDs` from the
  drop-down.
- `Metric`: Fine as is, keep all three.
- `Cause`: Keep `All causes`, and add `Cardiovascular diseases` from the
  drop-down, under `Non-communicable diseases`, and
  `Diabetes and kidney diseases`.
- `Location`: Keep `Global`, and add `Low SDI`, `Middle SDI`, and
  `High SDI`, and `United States of America` (under
  `High-income North America`) and `Latin America and Caribbean` (below
  `Western Europe`).
- `Ages`: Keep `All ages`, and add `Age-standardized`, `<20 years`,
  `20+ years`, `<70 years` and `70+ years`.
- `Sex`: Keep `Both`, and add `Male` and `Female`.
- `Year`: Keep `2019`, and add `2009`.

Then click `Download`, and, for `Include ID Values`, you can keep
`Both`. When it says “You can also monitor progress `here`”, click
`here` and then `Download`.

Then go back by clicking the left arrow on top of the page, to the left
of `Data Download`. It will take you back to the Search page, where you
just entered all these parameters. It will remember all these
parameters, so it’s best you really go back like I described.

Now download the same dataset, but with the following parameters:

- `Measure`: Select `YLLs`, `Prevalence`, and `Incidence`.

After you did that, go back and download a dataset on *risk factors*:

- `GBD Estimate`: Select `Risk factor`.
- `Measure`: Keep `Deaths` and `DALYs`, and add `YLDs`.
- `Metric`: Fine as is, keep all three.
- `Risk`: Keep `All risk factors`, add `behavioral risks`, `Tobacco` and
  `Dietary risks`.
- `Cause` until `Year`: Keep it all as is, if it remembered your
  selections from the dataset on causes of death (see above).

After you did that, go back, and download the same dataset but with
`Measure` being `YLLs`.

After you did that, go back and download a dataset on *summary exposure
values*: \* `GBD Estimate`: Select `Summary exposure values (SEV)`. \*
For `Measure` and `Metric`, you won’t have any choice. \* `Risk` until
`Year`: Keep it all as is, if it remembered your selections from earlier
on (see above).

After unzipping, the downloaded data folder should have a name similar
to this: `IHME-GBD_2019_DATA-64c5a7b3-1`. Store this data folder
anywhere in (a subfolder of) your GBD folder. For instance:
`Users/johnwilliam/datasets/Global Burden of Disease/Raw GBD data/IHME-GBD_2019_DATA-64c5a7b3-1`.
You’ll do the same for future data downloads. Just don’t change the
names of the data files, and codebook files for that matter, because
`better` searches for them using these names.

### Set global option in your `.Rprofile` file

Enter the path of your GBD folder (e.g.,
`Users/johnwilliam/datasets/Global Burden of Disease`) as the default
GBD path via global options set in your `.Rprofile` file. It will be
used by several `better` functions. It is not required, since you can
also set global options manually in each session, or even set the path
manually for each function call (see below). But it’s easier to take
care of this once and for all in `.Rprofile`.

It will look like this:

``` r
options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease")
```

If you haven’t used `.Rprofile` before, it’s a hidden file located
somewhere like here: `Users/johnwilliam/.Rprofile`. Learn more about
`.Rprofile` on [the Posit Support
page](https://support.posit.co/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf).

## Usage

<!-- COPY from better vignette: edit original there -->

The main purpose of `better` is to help you better guestimate the effect
of health nudges, that is, behavioral interventions targeted at health
behaviors.

Imagine you are designing a public health campaign. You are thinking
about focusing on preventing cardiovascular diseases, and you are
considering making information about healthy lifestyle more accessible.
What effects can you expect from a campaign like that? This is how we
would do it with `better`.

First, we look up the effect of an information nudge on health behavior.

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
#> ✔ ggplot2 3.4.0      ✔ purrr   1.0.0 
#> ✔ tibble  3.1.8      ✔ dplyr   1.0.10
#> ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
#> ✔ readr   2.1.3      ✔ forcats 0.5.2 
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
library(better)
#> Welcome to the better package!
#> To learn more, read the vignette by calling `vignette("better")`.
nudge_to_behavior_d(nudge = "information", behavior = "health") 
#> Looking up data on nudges...
#> Citation: Mertens, S., Herberz, M., Hahnel, U. J. J., & Brosch, T. (2022). The effectiveness of nudging: A meta-analysis of choice architecture interventions across behavioral domains. Proceedings of the National Academy of Sciences, 119(1), e2107346118. https://doi.org/10.1073/pnas.2107346118
#> This message is displayed once per session.
#>   Effect:
#> 
#> 0.26 standard deviations, 95% CI [0.09, 0.43]
#> 
#>   Parameters:
#> 
#> rei = behavioral risks, location = Global, behavior = health, nudge =
#> information
#> 
#>   Explanation:
#> 
#> This statistic comes from a meta-analysis on the effectiveness of nudging.
#> This is the abstract of the original paper: "Over the past decade, choice
#> architecture interventions or socalled nudges have received widespread attention
#> from both researchers and policy makers. Built on insights from the be... [TRUNCATED] ...y making."
#> 
#> See:
#> Mertens, S., Herberz, M., Hahnel, U. J. J., & Brosch, T. (2022). The effectiveness of nudging: A meta-analysis of choice architecture interventions across behavioral domains. Proceedings of the National Academy of Sciences, 119(1), e2107346118. https://doi.org/10.1073/pnas.2107346118
#> 
#> 
#> * This is a `better_effect` object. Call `str(.)` to see it's structure.
#> ** Call `explanation(.)` to get the full explanation.
#> This message is displayed once per session.
```

This gives us a Cohen’s *d*, a standardized effect size for the health
information nudge, namely, *d* = 0.26.

Second, as an intermediate step, we translate Cohen’s *d* into a
percentage point difference.

``` r
nudge_to_behavior_d(nudge = "information", behavior = "health") %>%
  behavior_d_to_pct
#> Getting data from object `gbd`.
#> Object `gbd` not found. Loading GBD data and assigning it to `gbd`.
#> Getting path from `getOption("better.gbd_path")`.
#>   Path found:
#> /Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My
#>   Drive/Offline Drive/R - Offline Drive/R packages JW/better/ignore/Global
#>   Burden of Disease Study Data
#> Reading GBD data with codebook:
#>   /Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My
#> Drive/Offline Drive/R - Offline Drive/R packages JW/better/ignore/Global Burden
#> of Disease Study Data/gbd.rds
#> Loaded GBD data with codebook, assigned to global variable `gbd`, and invisibly returning it.
#> 
#> This message is displayed once per session.
#>   Effect:
#> 
#> 61.1%, 95% CI [21.1, 100.0]
#> 
#>   Parameters:
#> 
#> behavior = health, nudge = information, rei = behavioral risks, location =
#> Global, sex = both, age = all ages, year = 2019
#> 
#>   Explanation:
#> 
#> Note: If the estimate has a minus sign, it indicates an increase in behavioral
#> risks.
#> 
#> We derive the estimate from the Cohen's d for nudge category "information"
#> and behavior category "health", which is 0.26, 95% CI [0.09, 0.43]. Now, to
#> calculate the percentage decrease, we make use of data on expo... [TRUNCATED] ...decrease is 61.09
#> percent, 95% CI [21.15, 100.00].
#> 
#> See:
#> Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from https://vizhub.healthdata.org/gbd-results/.   Effect:
#> 
#> 61.1%, 95% CI [21.1, 100.0]
#> 
#>   Parameters:
#> 
#> behavior = health, nudge = information, rei = behavioral risks, location =
#> Global, sex = both, age = all ages, year = 2019
#> 
#>   Explanation:
#> 
#> Note: If the estimate has a minus sign, it indicates an increase in behavioral
#> risks.
#> 
#> We derive the estimate from the Cohen's d for nudge category "information"
#> and behavior category "health", which is 0.26, 95% CI [0.09, 0.43]. Now, to
#> calculate the percentage decrease, we make use of data on expo... [TRUNCATED] ...decrease is 61.09
#> percent, 95% CI [21.15, 100.00].
#> 
#> See:
#> Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from https://vizhub.healthdata.org/gbd-results/.
#> 
#>   Terms:
#> 
#> Source: Institute for Health Metrics and Evaluation. Used with permission. All rights reserved. For details, including how to cite the source, call `gbd_license()`.
```

This gives us a decrease of 0.6 percentage points.

Finally, we can get our guestimate of the effect:

``` r
nudge_to_behavior_d(nudge = "information", behavior = "health") %>%
  behavior_d_to_pct %>%
  behavior_pct_to_disease("cardiovascular diseases")
#>   Effect:
#> 
#> 1,630 DALYs per 100,000 people, 95% CI [502, 3,029]
#> 
#>   Parameters:
#> 
#> behavior = health, nudge = information, cause = cardiovascular diseases, rei =
#> behavioral risks, metric = rate, measure = DALYs, location = Global, sex = both,
#> age = all ages, year = 2019
#> 
#>   Explanation:
#> 
#> Here is how we arrive at this estimate. We previously estimated that the
#> percentage decrease in exposure to behavioral risks is 61.1%, 95% CI [21.1,
#> 100.0]. We also know that at 15.3%, exposure to behavioral risks costs 2,668
#> DALYs per 100,000 people due to cardiovascular diseases. That is, we know
#> ... [TRUNCATED] ...630
#> DALYs per 100,000 people, 95% CI [502, 3,029].
#> 
#> See:
#> Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from https://vizhub.healthdata.org/gbd-results/.   Effect:
#> 
#> 1,630 DALYs per 100,000 people, 95% CI [502, 3,029]
#> 
#>   Parameters:
#> 
#> behavior = health, nudge = information, cause = cardiovascular diseases, rei =
#> behavioral risks, metric = rate, measure = DALYs, location = Global, sex = both,
#> age = all ages, year = 2019
#> 
#>   Explanation:
#> 
#> Here is how we arrive at this estimate. We previously estimated that the
#> percentage decrease in exposure to behavioral risks is 61.1%, 95% CI [21.1,
#> 100.0]. We also know that at 15.3%, exposure to behavioral risks costs 2,668
#> DALYs per 100,000 people due to cardiovascular diseases. That is, we know
#> ... [TRUNCATED] ...630
#> DALYs per 100,000 people, 95% CI [502, 3,029].
#> 
#> See:
#> Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from https://vizhub.healthdata.org/gbd-results/.
#> 
#>   Terms:
#> 
#> Source: Institute for Health Metrics and Evaluation. Used with permission. All rights reserved. For details, including how to cite the source, call `gbd_license()`.   Effect:
#> 
#> 1,630 DALYs per 100,000 people, 95% CI [502, 3,029]
#> 
#>   Parameters:
#> 
#> behavior = health, nudge = information, cause = cardiovascular diseases, rei =
#> behavioral risks, metric = rate, measure = DALYs, location = Global, sex = both,
#> age = all ages, year = 2019
#> 
#>   Explanation:
#> 
#> Here is how we arrive at this estimate. We previously estimated that the
#> percentage decrease in exposure to behavioral risks is 61.1%, 95% CI [21.1,
#> 100.0]. We also know that at 15.3%, exposure to behavioral risks costs 2,668
#> DALYs per 100,000 people due to cardiovascular diseases. That is, we know
#> ... [TRUNCATED] ...630
#> DALYs per 100,000 people, 95% CI [502, 3,029].
#> 
#> See:
#> Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from https://vizhub.healthdata.org/gbd-results/.
#> 
#>   Terms:
#> 
#> Source: Institute for Health Metrics and Evaluation. Used with permission. All rights reserved. For details, including how to cite the source, call `gbd_license()`.
```

``` r
gbd_cb <- gbd_codebook()
nudge_cb <- nudge_codebook()
nudge_cb$nudge
#> # A tibble: 3 × 2
#>   nudge       explain                                                           
#>   <chr>       <chr>                                                             
#> 1 information intervention that focuses on the description of alternatives (dec…
#> 2 structure   intervention that targets the organization and structure of choic…
#> 3 assistance  intervention that reinforces behavioral intentions (decision assi…
nudge_cb$behavior
#> # A tibble: 6 × 1
#>   behavior   
#>   <chr>      
#> 1 health     
#> 2 food       
#> 3 environment
#> 4 finance    
#> 5 pro-social 
#> 6 other
```

We learned something! If we’re going to roll out an information nudge in
100,000 people, targeted at cardiovascular diseases, we’re expected to
boost disability adjusted life years by about 1630 years! That’s not
bad.

Note that this is based on the following statistic:

``` r
gbd_describe(cause = "cardiovascular diseases")
#> In 2019, globally, the estimated cost of cardiovascular diseases, for both
#> men and women of all ages, was 5,081 DALYs per 100,000 people, 95% CI [4,753,
#> 5,393]. Note that DALYs refers to disability adjusted life years.
```

Let’s see, what if we would shift the focus of our intervention to
diabetes and kidney diseases?

``` r
nudge_to_behavior_d(nudge = "information", behavior = "health") %>%
  behavior_d_to_pct %>%
  behavior_pct_to_disease("diabetes and kidney diseases")
#>   Effect:
#> 
#> 251.5 DALYs per 100,000 people, 95% CI [69.5, 505.5]
#> 
#>   Parameters:
#> 
#> behavior = health, nudge = information, cause = diabetes and kidney diseases,
#> rei = behavioral risks, metric = rate, measure = DALYs, location = Global, sex =
#> both, age = all ages, year = 2019
#> 
#>   Explanation:
#> 
#> Here is how we arrive at this estimate. We previously estimated that the
#> percentage decrease in exposure to behavioral risks is 61.1%, 95% CI [21.1,
#> 100.0]. We also know that at 15.3%, exposure to behavioral risks costs 412
#> DALYs per 100,000 people due to diabetes and kidney diseases. That is, we kn... [TRUNCATED] ....5
#> DALYs per 100,000 people, 95% CI [69.5, 505.5].
#> 
#> See:
#> Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from https://vizhub.healthdata.org/gbd-results/.   Effect:
#> 
#> 251.5 DALYs per 100,000 people, 95% CI [69.5, 505.5]
#> 
#>   Parameters:
#> 
#> behavior = health, nudge = information, cause = diabetes and kidney diseases,
#> rei = behavioral risks, metric = rate, measure = DALYs, location = Global, sex =
#> both, age = all ages, year = 2019
#> 
#>   Explanation:
#> 
#> Here is how we arrive at this estimate. We previously estimated that the
#> percentage decrease in exposure to behavioral risks is 61.1%, 95% CI [21.1,
#> 100.0]. We also know that at 15.3%, exposure to behavioral risks costs 412
#> DALYs per 100,000 people due to diabetes and kidney diseases. That is, we kn... [TRUNCATED] ....5
#> DALYs per 100,000 people, 95% CI [69.5, 505.5].
#> 
#> See:
#> Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from https://vizhub.healthdata.org/gbd-results/.
#> 
#>   Terms:
#> 
#> Source: Institute for Health Metrics and Evaluation. Used with permission. All rights reserved. For details, including how to cite the source, call `gbd_license()`.   Effect:
#> 
#> 251.5 DALYs per 100,000 people, 95% CI [69.5, 505.5]
#> 
#>   Parameters:
#> 
#> behavior = health, nudge = information, cause = diabetes and kidney diseases,
#> rei = behavioral risks, metric = rate, measure = DALYs, location = Global, sex =
#> both, age = all ages, year = 2019
#> 
#>   Explanation:
#> 
#> Here is how we arrive at this estimate. We previously estimated that the
#> percentage decrease in exposure to behavioral risks is 61.1%, 95% CI [21.1,
#> 100.0]. We also know that at 15.3%, exposure to behavioral risks costs 412
#> DALYs per 100,000 people due to diabetes and kidney diseases. That is, we kn... [TRUNCATED] ....5
#> DALYs per 100,000 people, 95% CI [69.5, 505.5].
#> 
#> See:
#> Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from https://vizhub.healthdata.org/gbd-results/.
#> 
#>   Terms:
#> 
#> Source: Institute for Health Metrics and Evaluation. Used with permission. All rights reserved. For details, including how to cite the source, call `gbd_license()`.
```

What if we tried a structural nudge?

``` r
nudge_to_behavior_d(nudge = "structure", behavior = "health") %>%
  behavior_d_to_pct %>%
  behavior_pct_to_disease("diabetes and kidney diseases")
#>   Effect:
#> 
#> 412 DALYs per 100,000 people, 95% CI [224, 506]
#> 
#>   Parameters:
#> 
#> behavior = health, nudge = structure, cause = diabetes and kidney diseases, rei
#> = behavioral risks, metric = rate, measure = DALYs, location = Global, sex =
#> both, age = all ages, year = 2019
#> 
#>   Explanation:
#> 
#> Here is how we arrive at this estimate. We previously estimated that the
#> percentage decrease in exposure to behavioral risks is 100.0%, 95% CI [68.1,
#> 100.0]. We also know that at 15.3%, exposure to behavioral risks costs 412 DALYs
#> per 100,000 people due to diabetes and kidney diseases. That is, we k... [TRUNCATED] ...s 412 DALYs
#> per 100,000 people, 95% CI [224, 506].
#> 
#> See:
#> Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from https://vizhub.healthdata.org/gbd-results/.   Effect:
#> 
#> 412 DALYs per 100,000 people, 95% CI [224, 506]
#> 
#>   Parameters:
#> 
#> behavior = health, nudge = structure, cause = diabetes and kidney diseases, rei
#> = behavioral risks, metric = rate, measure = DALYs, location = Global, sex =
#> both, age = all ages, year = 2019
#> 
#>   Explanation:
#> 
#> Here is how we arrive at this estimate. We previously estimated that the
#> percentage decrease in exposure to behavioral risks is 100.0%, 95% CI [68.1,
#> 100.0]. We also know that at 15.3%, exposure to behavioral risks costs 412 DALYs
#> per 100,000 people due to diabetes and kidney diseases. That is, we k... [TRUNCATED] ...s 412 DALYs
#> per 100,000 people, 95% CI [224, 506].
#> 
#> See:
#> Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from https://vizhub.healthdata.org/gbd-results/.
#> 
#>   Terms:
#> 
#> Source: Institute for Health Metrics and Evaluation. Used with permission. All rights reserved. For details, including how to cite the source, call `gbd_license()`.   Effect:
#> 
#> 412 DALYs per 100,000 people, 95% CI [224, 506]
#> 
#>   Parameters:
#> 
#> behavior = health, nudge = structure, cause = diabetes and kidney diseases, rei
#> = behavioral risks, metric = rate, measure = DALYs, location = Global, sex =
#> both, age = all ages, year = 2019
#> 
#>   Explanation:
#> 
#> Here is how we arrive at this estimate. We previously estimated that the
#> percentage decrease in exposure to behavioral risks is 100.0%, 95% CI [68.1,
#> 100.0]. We also know that at 15.3%, exposure to behavioral risks costs 412 DALYs
#> per 100,000 people due to diabetes and kidney diseases. That is, we k... [TRUNCATED] ...s 412 DALYs
#> per 100,000 people, 95% CI [224, 506].
#> 
#> See:
#> Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from https://vizhub.healthdata.org/gbd-results/.
#> 
#>   Terms:
#> 
#> Source: Institute for Health Metrics and Evaluation. Used with permission. All rights reserved. For details, including how to cite the source, call `gbd_license()`.
```
