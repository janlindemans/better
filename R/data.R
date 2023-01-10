#' Global Burden of Disease starter dataset
#'
#' To make data-driven guesstimates, `better` uses the [Global Burden of Disease (GBD) dataset](https://www.healthdata.org/gbd/2019), an incredibly rich dataset on the impact of a long list of diseases. This starter dataset is an extremely small subset of the full GBD dataset. The goal is to get you started. When you need more, you can go download it from the GBD website. You can use the helper `gbd_download()` for that purpose.
#'
#' # Terms of use, license and citation
#'
#' The GBD dataset is created and maintained by the [Institute for Health Metrics and Evaluation](https://www.healthdata.org/) at the University of Washington. The dataset is owned by the University of Washington. The `better` package uses the dataset in accordance with the [IHME free-of-charge non-commercial user agreement]( https://www.healthdata.org/Data-tools-practices/data-practices/ihme-free-charge-non-commercial-user-agreement).
#'
#' Cite the data as follows:
#'
#' Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from [https://vizhub.healthdata.org/gbd-results/](https://vizhub.healthdata.org/gbd-results/).
#'
#' @format ## `gbd_start`
#' A data frame with `r nrow(gbd_start)` rows and `r ncol(gbd_start)` columns:
#' \describe{
#'   \item{cause}{A single disease or injury or an aggregation of diseases and injuries that causes death or disability.}
#'   \item{rei}{Risk/etiology/impairment/injury.}
#'   \item{metric}{The unit by which a measure is expressed. E.g., number, percent, rate, etc.}
#'   \item{val}{The mean value of an estimate.}
#'   \item{measure}{The indicator for which estimates are produced.}
#'   \item{upper}{Upper value of confidence interval.}
#'   \item{lower}{Lower value of confidence interval.}
#'   \item{location}{Includes country, non-sovereign region, principal administrative unit of a country (e.g., state, province), GBD region, or other custom administrative division, such as World Bank Income Level or WHO region.}
#'   \item{sex}{Male, female or both sexes combined.}
#'   \item{age}{A population segment within a specified age range.}
#'   \item{year}{The period of 365 days (or 366 days in leap years) in the Gregorian calendar divided into 12 months beginning with January and ending with December.}
#'   \item{file}{Name of the file downloaded from the GBD website that contains the data point.}
#' }
#' @source <https://vizhub.healthdata.org/gbd-results/>
"gbd_start"

#' GBD effects dataset
#'
#' This dataset contains estimates of effects.
#'
#' # Terms of use, license and citation
#'
#' The effects dataset is based on the GBD dataset, created and maintained by the [Institute for Health Metrics and Evaluation](https://www.healthdata.org/) at the University of Washington. The dataset is owned by the University of Washington. The `better` package uses the dataset in accordance with the [IHME free-of-charge non-commercial user agreement]( https://www.healthdata.org/Data-tools-practices/data-practices/ihme-free-charge-non-commercial-user-agreement).
#'
#' Cite the original data as follows:
#'
#' Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from [https://vizhub.healthdata.org/gbd-results/](https://vizhub.healthdata.org/gbd-results/).
#'
#' @format ## `gbd_effects`
#' A data frame with `r nrow(gbd_effects)` rows and `r ncol(gbd_effects)` columns:
#' \describe{
#'   \item{cause}{A single disease or injury or an aggregation of diseases and injuries that causes death or disability.}
#'   \item{rei}{Risk/etiology/impairment/injury.}
#'   \item{metric}{The unit by which a measure is expressed. E.g., number, percent, rate, etc.}
#'   \item{measure}{The indicator for which estimates are produced.}
#'   \item{location}{Includes country, non-sovereign region, principal administrative unit of a country (e.g., state, province), GBD region, or other custom administrative division, such as World Bank Income Level or WHO region.}
#'   \item{sex}{Male, female or both sexes combined.}
#'   \item{age}{A population segment within a specified age range.}
#'   \item{year}{The period of 365 days (or 366 days in leap years) in the Gregorian calendar divided into 12 months beginning with January and ending with December.}
#'   \item{val}{The mean value of an estimate.}
#'   \item{upper}{Upper value of confidence interval.}
#'   \item{lower}{Lower value of confidence interval.}
#' }
"gbd_effects"
