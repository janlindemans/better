
<!-- Edit this to prevent git error: 9827634 -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

# better

IMPORTANT: THIS PACKAGE IS UNDER DEVELOPMENT. IT’S AVAILABLE HERE FOR
DEVELOPMENT AND TESTING PURPOSES ONLY.

<!-- COPY from better.rmd -->

The `better` package helps you guesstimate the effect of health nudges,
that is, behavioral interventions targeted at health behaviors. With
`better` guesstimates, you can make the world a `better` place! In
addition to guesstimating effect sizes, `better` also helps you
guesstimate sample sizes for properly powered studies on health nudges.
Finally, `better` also gives guesstimates of how much money you would
save in terms of health care.

## Installation

You can install the development version of better from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("janlindemans/better", build_vignettes = TRUE)
```

## Usage

<!-- COPY from better vignette: edit original there -->

## An example: A diet-related nudge for diabetics

<!-- ORIGINAL HERE - COPY PASTE it to elsewhere -->

The main purpose of `better` is to help you better guesstimate the
effect of health nudges, that is, behavioral interventions targeted at
health behaviors.

Imagine you are designing a public health campaign. You are thinking
about focusing on diabetics, and you are considering making information
about healthy foods more accessible. What effects can you expect from a
campaign like that?

This is how we would make a guesstimate with `better`. Let’s load it,
and let’s also load `tidyverse`, so we can use pipes and other tidy
constructions.

``` r
library(better)
library(tidyverse)

nudge("information", behavior = "food") %>% 
  disease("diabetes mellitus") %>%
  describe
#> This is a `better_effect` object. For more details, including terms of use, run `?disease`.
#> A behavioral intervention in nudge category "information" and behavior category "food" that targets diabetes mellitus saves 85.2 DALYs per 100,000 people, 95% CI [29.5, 157.3].
```

Note that “DALYs” are *disability-adjusted life years*, a measure of
overall disease burden, expressed as the number of years lost due to
disease, disability or early death. The output above shows DALYs *saved*
by the nudge, that is, the number of years *saved* due to *decreased*
disease, disability and death.

As for the code: We basically told `nudge()` what kind of nudge we are
envisioning, `"information"`, and what kind of behavior we are
targeting: `"food"`-related behavior. We piped the result into
`disease()` and told it what disease we are targeting:
`"diabetes mellitus"`. Finally, `describe()` simplified the output to a
brief description of the guesstimate, which is what you saw.

So, we got our guesstimate in terms of DALYs saved. There’s many other
measures we can get estimates for. Let’s see how many *years lived with
disability* or “YLDs” our nudge would save:

``` r
nudge("information", behavior = "food") %>%
  disease("diabetes mellitus", measure = "YLDs") %>%
  describe
#> This is a `better_effect` object. For more details, including terms of use, run `?disease`.
#> A behavioral intervention in nudge category "information" and behavior category "food" that targets diabetes mellitus saves 46.9 YLDs per 100,000 people, 95% CI [14.0, 95.9].
```

The reason why I picked YLDs is because it allows us to guesstimate the
cost of healthcare saved by the nudge. We do that as follows, with
`cost()`:

``` r
nudge("information", behavior = "food") %>%
  disease("diabetes mellitus", measure = "YLDs") %>%
  cost %>%
  describe
#> This is a `better_effect` object. For more details, including terms of use, run `?disease`.
#> The annual health care costs saved because of reducing YLDs per 100,000 people is $499,525; 95% CI [126,528; 1,178,042].
```

Note that cost calculation is still a feature in development: for now it
doesn’t yet distinguish between the costs of different diseases. Run
`?cost` to read more.

Once we have the cost savings, we can also guesstimate the *net* savings
in cost, that is, the savings from reduce healthcare costs minus the
cost of the nudge, of running the campaign. We use the `net()` function:

``` r
nudge("information", behavior = "food") %>%
  disease("diabetes mellitus", measure = "YLDs") %>%
  cost %>%
  net %>%
  describe
#> This is a `better_effect` object. For more details, including terms of use, run `?disease`.
#> The annual net cost savings, after deducting campaign costs, is $469,852; 95% CI [69,162; 1,176,064].
```

Next, say you are in charge of a campaign at an insurance company with
350,000 members. You can also calculate the net savings for 350,000
members, with the `per()` function:

``` r
nudge("information", behavior = "food") %>%
  disease("diabetes mellitus", measure = "YLDs") %>%
  cost %>%
  net %>%
  per(350000) %>%
  describe
#> This is a `better_effect` object. For more details, including terms of use, run `?disease`.
#> Annual net cost of YLDs per 350,000 people saved is $1,644,483; 95% CI [242,066; 4,116,223].
```

Finally, say you are considering running an experiment with your
members, to see if your nudge has a positive impact. You can do quick
power calculations with `sample_size()`. Here is how to do a power
calculation for an experiment that wants to detect an impact on
food-related behaviors.

``` r
nudge("information", behavior = "food") %>%
  describe
#> This is a `better_effect` object. For more details, including terms of use, run `?disease`.
#> Cohen's d for nudge category "information" and behavior category "food" is 0.44, 95% CI [0.19, 0.70].
nudge("information", behavior = "food") %>%
  sample_size
#> 
#>      Two-sample t test power calculation 
#> 
#>               n = 64.55656
#>               d = -0.44
#>       sig.level = 0.05
#>           power = 0.8
#>     alternative = less
#> 
#> NOTE: n is number in *each* group
```

Here is how to do a power calculation for a much more ambitious
experiment that wants to detect on impact on saving lives (preventing
deaths):

``` r
nudge("information", behavior = "food") %>%
  disease("diabetes mellitus", measure = "deaths") %>%
  describe
#> This is a `better_effect` object. For more details, including terms of use, run `?disease`.
#> A behavioral intervention in nudge category "information" and behavior category "food" that targets diabetes mellitus saves 1.90 deaths per 100,000 people, 95% CI [0.71, 3.26].
nudge("information", behavior = "food") %>%
  disease("diabetes mellitus", measure = "deaths") %>%
  sample_size
#> The effect implies a drop from the baseline of 4.83 deaths to 2.93 deaths per 100,000. We translate these proportions into a Cohen's h with pwr::ES.h() for the power calculation.
#> 
#>      Difference of proportion power calculation for binomial distribution (arcsine transformation) 
#> 
#>               h = -0.003065874
#>               n = 1315496
#>       sig.level = 0.05
#>           power = 0.8
#>     alternative = less
#> 
#> NOTE: same sample sizes
```

The low number of yearly deaths means you’ll need a huge sample size!
Better just look at behavior change!

Alright, that’s a quick overview of what `better` can do for you!

<!-- END COPY -->
