# TODO: test
# automatic gbd_save if no rds file found (in gbd_read)
# default rds file with package - needs agreement. Also loads if no data found on given path

# Figure out risk behaviors: sometimes POSITIVE impact on health. Code for that!


txt <- list(
  gbd_cite = "Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from https://vizhub.healthdata.org/gbd-results/.",
  nudge_cite = "Mertens, S., Herberz, M., Hahnel, U. J. J., & Brosch, T. (2022). The effectiveness of nudging: A meta-analysis of choice architecture interventions across behavioral domains. Proceedings of the National Academy of Sciences, 119(1), e2107346118. https://doi.org/10.1073/pnas.2107346118",

  gbd_source = "Source: Institute for Health Metrics and Evaluation. Used with permission. All rights reserved.",
  gbd_license_explain = "The Global Burden of Disease dataset is created and maintained by the Institute for Health Metrics and Evaluation at the University of Washington. The dataset is owned by the University of Washington. The `better` package uses the dataset in accordance with the IHME free-of-charge non-commercial user agreement, retrieved from https://www.healthdata.org/Data-tools-practices/data-practices/ihme-free-charge-non-commercial-user-agreement.",
  gbd_agreement = "TERMS AND CONDITIONS OF USE: IHME follows the University of Washington's website terms and conditions: http://www.washington.edu/online/terms/. Commercial organizations wishing to acquire a license to access and download IHME data should contact services@healthdata.org. For non-commercial users of IHME websites, including all pages and datasets found on the healthdata.org, vizhub.healthdata.org, and ghdx.healthdata.org domains, the following constitute prior written permission, as required under Section 7 of the University of Washington's Website Terms and Conditions of Use. [...] Data made available for download on IHME Websites can be used, shared, modified or built upon by non-commercial users in accordance with the IHME FREE-OF-CHARGE NON-COMMERCIAL USER AGREEMENT. https://www.healthdata.org/data-tools-practices/data-practices/terms-and-conditions",
  gbd_agreement_acknowledge = "Data Source Acknowledgment. Proper acknowledgment shall be made for the contributions of each party to any Results being published in accordance with generally accepted scientific standards. In any instance when a Publication includes IHME Data or is derived from IHME Data, User shall include the following source identifier: Source: Institute for Health Metrics and Evaluation. Used with permission. All rights reserved.",
  gbd_agreement_share = "User may not without written permission from UW provide to third parties the ability to download IHME Data Sets from User-provided hosting facilities; User may publish links to IHME's hosting and downloading facilities.",
  gbd_agreement_5 = "5 Restrictions on Use of the IHME Data and Data Sets. Except as expressly provided in this Agreement, or with written consent from UW, User will not (i) copy, reproduce or duplicate the IHME Data or Data Sets; (ii) reveal, publish, transfer, disclose, disseminate, distribute, assign, rent, lease, loan, license, or otherwise make the IHME Data or Data Sets available to any person or organization other than User's employees who have a \"need to know\" with respect to work they are currently undertaking in the course and scope of their employment for internal research purposes or Publication purposes; (iii) modify, translate, merge, alter, reverse compile/assemble, decrypt, reverse engineer or create derivative works from the IHME Data or Data Sets; (iv) utilize in any manner the IHME Data or Data Sets to develop data sets similar in function to the Data Sets; or (v) knowingly permit or enable any third party to do the foregoing. When using the IHME Data or Data Sets for purposes of a Publication, User shall not use them or refer to UW and/or IHME in any way that conveys, either directly or implicitly, an impression that UW and/or IHME support, promote or endorse User's analyses, conclusions, recommendations, services or products.",
  gbd_get_license = "Call `gbd_license()` to access the user agreement.",

  gbd_error_missing = "No `gbd_dataset` object provided, and no default object named `gbd` found. To learn more about `gbd` and `gbd_dataset`, call `vignette(\"better\")`.",

  gbd_doc_dataset = "GBD dataset, of `gbd_dataset` class. By default, `gbd` is used, if it exists. Consider `gbd <- gbd_read()`. To learn more, call `vignette(\"better\")`."
)
txt <- with(txt, c(
  txt,
  gbd_boiler = paste0(gbd_source," For details, including how to cite the source, call `gbd_license()`.")
))
# I used to wrap this, but I think it is better to wrap on demand (and in bulk)
# old code:
#txt <- purrr::map(txt, stringr::str_wrap) # after this, will NOT be wrapped
# note: if you don't want wrapped stuff wrapped, unwrap it with something like stringr::str_squish
txt <- c(
  txt,
  gbd_path_dev = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data",
  gbd_path = "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared/IRB Projects/Health Projects/Health Team/Public datasets on health/Global Burden of Disease Study Data",
  gbd_path_show = "/Users/johnwilliam/datasets/Global Burden of Disease data"
)

dvl_options <- function(path_dev = TRUE) {
  # for development only
  # make all dev functions start with dev
  if (path_dev) {
    options(better.gbd_path = txt$gbd_path_dev)
  } else {
    options(better.gbd_path = txt$gbd_path)
  }
}#; dvl_options(path_dev = FALSE)
dvl_load <- function() {
  gbd <<- gbd_read(path = getOption("better.gbd_path"))
  gbd2 <<- gbd_read(path = getOption("better.gbd2_path"))
}
dvl_save <- function() {
  gbd_save(path = getOption("better.gbd_path"))
  gbd_save(path = getOption("better.gbd2_path"))
}

tolower_1 <- function(x) {
  # useful for changing sentences
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}#; paste0("Although a bit cold, ",tolower_1("The soup is great. Thank you."))

#' Global Burden of Disease data license
#'
#' @param full Whether to return full text of the license
#'
#' @return String, license
#' @export
#'
#' @examples
#' gbd_license()
gbd_license <- function(full = TRUE) {
  y <- c(
    txt$gbd_license_explain,
    " Cite the data as follows: ", txt$gbd_cite
  )
  y <- paste(y,collapse="\n")
  cat(y)
  invisible(y)
}

# TODO: Once I have a beta version: Ask for permission to use a local dataset.

pretty_nr <- function(..., always_vector = FALSE) {
  #browser()
  x <- unlist(list(...))
  if (any(x < 10)) {
    nsmall <- 2
  } else if (any(x < 100)) {
    nsmall <- 1
  } else {
    nsmall <- 0
  }
  y <- format(x, trim = TRUE, scientific = FALSE, big.mark = ",", digits = 2, nsmall = nsmall)
  names(y) <- names(x)
  #browser()
  if (!is.null(names(y)) & !always_vector) {
    y <- as.list(y)
  }
  return(y)
}#; pretty_nr(a=873.34,b=98.30948,c(c=0.09393,d=.3)) #list(7, c(b = 7.2,7.1), c(c= 10.2,e=10.1), c(90.4,90.5), c(190.4,190.5), 1000000, 92834592, 0.1, 0.000387239487234) %>% map(pretty_nr)

get_path <- function(path, path_missing = FALSE) {
  #browser()
  path_str <- rlang::as_string(rlang::ensym(path))

  if (path_missing) {
    message("Getting path from `getOption(\"better.gbd_path\")`.")
    y <- getOption("better.gbd_path")
    if (length(y) == 0) {
      stop("No path found with `getOption(\"better.gbd_path\")`. Did you forget to add `options(better.gbd_path = \"path/to/gbd/data\")` to your .Rprofile? See `vignette(\"better\")`.")
    } else {
      message("  Path found:\n",y)
    }
  } else {
    message("You provided the GBD path explicitly in the `path` argument. Consider adding `options(better.gbd_path = \"path/to/gbd/data\")` to your .Rprofile, and make use of the default path. See `vignette(\"better\")`.")
    y <- path
  }
  y <- as.character(y)

  if (!file.exists(y)) {
    stop("The path doesn't exist:\n  ",y,ifelse(path_missing,"","\nYou probably want to edit `options(better.gbd_path = \"correct/path/to/gbd/data\")` in your .Rprofile."))
  }



  return(y)
}#; get_path("/user/some/kind/path/bla", path_missing = FALSE)

list_recursive_path_names <- function(x) {
  list.files(path = as.character(x), full.names = TRUE, recursive = TRUE)
  # keep this code as is:
  # as.character is needed because otherwise installing (but nothing else!!) throws an error that list.files has an invalid path argument. It's something weird with list.files: don't try to solve the bug. But as.character works.
}

#' Read Global Burden of Disease data
#'
#' Read the GBD data from your local folder. It looks for data files in all subfolders of the GBD folder and merges them into one dataframe.
#'
#' @param path Path of the GBD folder, where the data are stored. By default, automatically read from options, namely, `better.gbd_path`, ideally set in .Rprofile. To learn more, call `vignette(\"better\")`.
#'
#' @return Dataframe containing the GBD data.
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#'
#' gbd_data <- gbd_data_read() # by default the GBD path is taken from .Rprofile
#' gbd_data
#' }
gbd_data_read <- function(path) {

  path <- get_path(path, missing(path))

  #browser()

  FILES_GBD_DATA <- stringr::str_subset(
    list_recursive_path_names(path),
    "/IHME-GBD_20\\d{2}_DATA-.+\\.csv"
  ) #%>% print
  if (length(FILES_GBD_DATA) == 0) {
    stop("No data files found in ", path, ".")
  }
  y <- tibble::tibble()
  message(
    "  Reading files with readr::read_csv:\n",
    paste(basename(FILES_GBD_DATA), collapse = ", ")
  )
  #cat("\n")
  for (i in FILES_GBD_DATA) {
    #if (i == "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared/IRB Projects/Health Projects/Health Team/Public datasets on health/Global Burden of Disease Study Data/Raw Data GBDS/IHME-GBD_2019_DATA-c2ed89a6-1/IHME-GBD_2019_DATA-c2ed89a6-1.csv") browser()
    #browser()
    message(
      "Read so far: ", pretty_nr(nrow(y)),
      " rows. Now reading: ", basename(i), "..."
    )
    dfi <- suppressMessages(readr::read_csv(i))
    dfi$file <- i
    y <- dplyr::bind_rows(
      y,
      dfi
    )
  }
  # browser()

  # Todo: make the file column, a list of vectors of ALL the files in which the row was present
  # Trick: sort the list of files from large file to small file, and then the duplicates later on from smaller files will be deleted
  # y$id <- paste...
  # y$duplicated <- duplicated(y["file"])

  # For coding, don't delete:
  # y %>% names %>% PrettyVector(quotes = "")
  y <- dplyr::distinct(
    y,
    dplyr::across(tidyselect::all_of(c("measure_id", "measure_name", "location_id", "location_name", "sex_id", "sex_name", "age_id", "age_name", "cause_id", "cause_name", "rei_id", "rei_name", "metric_id", "metric_name", "year", "val", "upper", "lower"))),
    .keep_all = TRUE
  )
  message(
    "Finished looping through all the GBD data files. Read in total: ", pretty_nr(nrow(y)),
    " rows.\n",
    txt$gbd_boiler
  )
  return(y)
}
#load_all(); gbd_data_read()
#gbd <- gbd_data_read()

pretty_paragraphs <- function(..., sep="\n\n") {
  # Make paragraphs etc pretty: wrapping them; respecting intentional newlines and indentation; adding newlines between paragraphs
  # Assumes all line-starting blanks, including multiple blanks, are intentional (contra stringr::str_wrap).
  x <- unlist(list(...))
  y <- c()
  for (i in x) {
    strt <- stringr::str_extract(i, "^[[:blank:]]+")
    if (is.na(strt)) strt <- ""
    yi <-    stringr::str_remove(i, "^[[:blank:]]+")
    splitters <- unlist(stringr::str_extract_all(yi,"\n+[[:blank:]]*"))
    yi <-        unlist(      stringr::str_split(yi,"\n+[[:blank:]]*"))
    yi <- stringr::str_wrap(yi)
    yi <- paste0(c(strt,splitters),yi)
    yi <- paste0(yi, collapse = "")
    y <- c(y,yi)
  }
  y <- paste0(y,collapse=sep)
  return(y)
}
# pretty_paragraphs(c("  Objects loaded from .Rprofile:  BOX_FOLDER, DRIVE_FOLDER, easyr, FOLDER_BOX, FOLDER_CAH_BOX, FOLDER_CAH_DRIVE, FOLDER_CAH_SHARED_BOX, FOLDER_CAH_SHARED_DRIVE, FOLDER_DRIVE, FOLDER_DRIVE_ROOT, FOLDER_JWR, FOLDER_PACKAGES, FOLDER_R, FOLDER_USER, JW_RPROFILE_NAMES, jwr, jwr_path, make_command, NAMES_WITH_JW_RPROFILE, NAMES_WITHOUT_JW_RPROFILE, print.command, USER_FOLDER
#
#   Enter jwr if you want to source jwr, JW's amazing package. :)
#
# getwd(): [1] /Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My Drive/Offline Drive/R - Offline Drive/R packages JW/better", "  test\n\n\ntest", 1)) %>% cat


clean_measure <- function(x) { # only pick first word, except for SEV
  purrr::map_chr(x,~{
    ifelse(
      . %in% c("Summary exposure value", "summary exposure value"), .,
      stringr::str_split(.," ")[[1]][1]
    )
  })
}
clean_string <- function(x) {
  # tolower, but respect abbreviations
  purrr::map_chr(x,~ifelse(is.na(.x), NA_character_ ,paste(lapply(
    strsplit(.x," "),
    function(x) ifelse(stringr::str_detect(x,"^[[:upper:]]([[:lower:]]|-)+$"), tolower(x), x)
  )[[1]], collapse=" ")))
}#; clean_string("Age-standardized")#; clean_string(c("The HIV/AIDS Issue","DALYs", NA, "Why NOT I don t know Really"));

#' Clean Global Burden of Disease data
#'
#' Cleans the GBD data in a sensible way.
#'
#' @param x Dataframe of raw GBD data to be cleaned.
#'
#' @return Dataframe with cleaned data, by default read with `gbd_data_read()`.
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#'
#' library(tidyverse)
#'
#' gbd_data <- gbd_data_read() %>%
#'   gbd_data_clean
#' gbd_data
#' }
gbd_data_clean <- function(x = gbd_data_read()) {
  message("Started cleaning the GBD data (can take a couple of minutes for large amounts of data)...")
  y <- x
  names(y) <- stringr::str_remove(names(y), "_name")
  y$measure_descr <- y$measure
  message("Cleaning the measures...")
  y$measure <- clean_measure(y$measure)
  y <- dplyr::select(y,-dplyr::ends_with("_id"), tidyselect::everything())
  y <- dplyr::select(
    y,
    "cause", "rei", "metric", "val", "measure", "upper", "lower",
    "measure_descr",
    tidyselect::everything(),
    )
  message("Cleaning strings (can take a while)...")
  for (i in c("cause", "rei", "metric", "sex", "age", "measure")) {
    message("Cleaning strings in ", i, "...")
    y[i] <- clean_string(y[[i]])
  }
  #browser()

  if (anyNA(y$rei[y$val<0])) {
    stop("Some NAs for rei where values are smaller than 0. This should not happen, because the GBD codebook says that negative values are for rei's that are 'protective'. This introduces a bug: Now `better` should also take into account `protective` causes - diseases that are good for people. This doesn't really make sense. It's probably a bug.")
  }
  y$measure_with_metric <- paste(y$metric, y$measure, sep = " of ")

  #y <- dplyr::select(y, -"measure", "measure")
  message("Finished cleaning the data.")
  return(y)
}# gbd_data_clean(gbd_data_read()) %>% pull(cause) %>% unique()
# gbd_data_read() %>% pull(cause_name) %>% unique()

clean_negatives <- function(x, prefix = "Absence of ") {
  #browser()
  x$rei <-   ifelse(x$val < 0, paste0(prefix,x$rei), x$rei)
  x$lower <- ifelse(x$val < 0, -x$lower, x$lower)
  x$upper <- ifelse(x$val < 0, -x$upper, x$upper)
  x$val <-   ifelse(x$val < 0, -x$val, x$val)
  return(x)
}#; View(clean_negatives(gbd)[gbd$val < 0,])

#' Read Global Burden of Disease codebook
#'
#' Read the GBD codebook from your local folder. It looks for codebook files in all subfolders of the GBD folder and puts them into a list.
#'

#' @inheritParams gbd_data_read
#'
#' @return List with dataframes of GBD codebooks.
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#' }
#'
#' gbd_codebook <- gbd_codebook_read()
#' gbd_codebook
gbd_codebook_read <- function(path) {
  #browser()

  path <- get_path(path, missing(path))

  if (length(path) == 0) {
    stop("`path` has length 0.")
  }
  variables <- suppressMessages(readr::read_csv(
    as.character(utils::tail(stringr::str_subset(
      list_recursive_path_names(path),
      "/IHME_GBD_20\\d{2}_CODEBOOK_Y\\d{4}M\\d{2}D\\d{2}\\.CSV"
    ),1))
  ))

  measures <- suppressMessages(readxl::read_excel(
    as.character(utils::tail(stringr::str_subset(
      list_recursive_path_names(path),
      "/IHME_GBD_20\\d{2}_MEASURE_METRIC_DEFINITIONS_Y\\d{4}M\\d{2}D\\d{2}\\.XLSX"
    ),1)),
    skip=1
  ))
  reis <- suppressMessages(readxl::read_excel(
    utils::tail(stringr::str_subset(
      list_recursive_path_names(path),
      "/IHME_GBD_20\\d{2}_REI_HIERARCHY_Y\\d{4}M\\d{2}D\\d{2}\\.XLSX"
    ),1)
  ))
  causes <- suppressMessages(readxl::read_excel(
    utils::tail(stringr::str_subset(
      list_recursive_path_names(path),
      "/IHME_GBD_20\\d{2}_CAUSE_HIERARCHY_Y\\d{4}M\\d{2}D\\d{2}\\.XLSX"
    ),1)
  ))
  message("Read all codebook files.")

  y <- list(
    variables = variables,
    causes = causes,
    measures = measures,
    reis = reis
  )
  return(y)
}#; gbd_codebook <- gbd_codebook_read()

#' Clean Global Burden of Disease codebook
#'
#' @param x List with codebooks to be cleaned.
#'
#' @return List with cleaned codebooks.
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#' }
#'
#' library(tidyverse)
#'
#' gbd_codebook <- gbd_codebook_read() %>%
#'   gbd_codebook_clean
gbd_codebook_clean <- function(x = gbd_codebook_read()) {
  #browser()
  y <- x
  y$variables <- tibble::tibble(
    variable = stringr::str_remove(names(y$variables)[-1],"_name"),
    label = unname(unlist(y$variables[1,-1])),
    value_coding = purrr::map(y$variables[-1,-1], ~{.[!is.na(.)]}),
    variable_origi = names(y$variables)[-1]
  )
  #browser()
  #y$causes
  for (i in c("measures","reis","causes")) {
    names(y[[i]]) <- snakecase::to_snake_case(names(y[[i]]))
    names(y[[i]]) <- stringr::str_remove(names(y[[i]]), "_name$")
  }
  #y$causes
  y$variables$values <- purrr::map_chr(y$variables$value_coding, ~paste0(.,collapse=", "))
  y$variables <- dplyr::select(
    y$variables,
    "variable", "label", "values", tidyselect::everything()
  )

  #browser()
  y$measures$explain <- stringr::str_to_lower(stringr::str_remove(y$measures$measure, " \\(.+"))

  bracketed <- stringr::str_extract(y$measures$measure, "\\(.+")
  bracketed <- stringr::str_remove_all(bracketed, "\\(|\\)")
  y$measures$short <- bracketed
  y$measures$measure <- clean_string(
    ifelse(
      is.na(bracketed), y$measures$measure,
      bracketed
    )
  )
  y$measures$measure <- dplyr::recode(y$measures$measure,
    "SEV" = "summary exposure value"
  )

  y$causes$cause <- clean_string(y$causes$cause)
  y$causes$parent <- clean_string(y$causes$parent)
  y$reis$rei <- clean_string(y$reis$rei)
  y$reis$parent <- clean_string(y$reis$parent)

  return(y)
}#; gbd_codebook_clean(gbd_codebook_read())

# gbd_rdata_path <- function(path = getOption("better.gbd_path")) {
#   paste0(path,"/gbd.RData")
# }
gbd_rds_path <- function(path = getOption("better.gbd_path")) {
  paste0(path,"/gbd.rds")
}

#' Save Global Burden of Disease data and codebook
#'
#' @inheritParams gbd_data_read
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#'
#' gbd_save()
#' }
gbd_save <- function(path) {
  #browser()
  path <- get_path(path, missing(path))
  gbd_data <- gbd_data_read(path = path)
  gbd_data <- gbd_data_clean(gbd_data)
  gbd_codebook <- gbd_codebook_read(path = path)
  gbd_codebook <- gbd_codebook_clean(gbd_codebook)
  y <- new_gbd(gbd_data, gbd_codebook)
  message("")
  message("Saving GBD data and codebook in:\n  ", stringr::str_wrap(gbd_rds_path(path)))
  saveRDS(y, file = gbd_rds_path(path))
  return(y)
}

#' Read cleaned Global Burden of Disease data
#'
#' Calls `base::readRDS` on the `.rds` file with the cleaned GBD data.
#'
#' @inheritParams gbd_data_read
#' @param load Whether or not to automatically assign the returned value to the global variable `gbd`. The default is `load = FALSE`. Rather than `gbd_save(load = TRUE)`, consider the cleaner alternative `gbd <- gbd_save()`.
#' @param assign_to What variable name to assign the GBD dataset to.
#'
#' @return The value of `base::readRDS`, which this function wraps.
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#' }
#' gbd <- gbd_read()
gbd_read <- function(path, load = FALSE, assign_to) {
  path <- get_path(path, missing(path))
  message("Reading GBD data with codebook:\n  ",stringr::str_wrap(gbd_rds_path(path)))
  y <- readRDS(gbd_rds_path(path))
  if (load) {
    assign(assign_to, y, envir=globalenv()) # this generates a note in devtools::check(), but it's fine
    message("Loaded GBD data with codebook, assigned to global variable `",assign_to,"`, and invisibly returning it.")
    invisible(y)
  } else {
    return(y)
  }
}

#' Load cleaned Global Burden of Disease data
#'
#' @param path `r txt$gbd_cite`
#' @param assign_to What variable name to assign the GBD dataset to.
#'
#' @return The value of `base::readRDS`, which this function wraps.
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#' }
#'
#' gbd_load()
gbd_load <- function(path, assign_to = "gbd") {
  path <- get_path(path, missing(path))
  gbd_read(path = path, load = TRUE, assign_to = assign_to)
}

#' Download Global Burden of Disease data
#'
#' @inheritParams gbd_data_read
#'
#' @return URLs of webpages opened, invisible.
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#' }
#'
#' # gbd_download()
gbd_download <- function(path) {
  path <- get_path(path, missing(path))
  GBD_SITES <- c(
    guide = "https://ghdx.healthdata.org/sites/default/files/ihme_query_tool/GBD_Results_Tool_User_Guide_2019.pdf",
    main = "https://ghdx.healthdata.org/gbd-2019",
    results = "https://vizhub.healthdata.org/gbd-results/"
  )
  message(
    stringr::str_wrap("The page to download data and the user guide are opened in your browser. Your GBD data folder is also opened. Download the data you want, and save it somewhere in the GBD data folder or a subfolder. It doesn't matter where, as long as you don't change the name of the data file."),
    "\n\nWebpages opened:\n- ",
    paste0(stringr::str_wrap(purrr::map2_chr(
      GBD_SITES, names(GBD_SITES), ~{paste0(.y,": ",.x)}
      )), collapse="\n- "),
    "\n\nFolder opened:\n", stringr::str_wrap(path)
  )
  gbd_open(path = path)
  purrr::walk(GBD_SITES, utils::browseURL)
  invisible(GBD_SITES)
}

#' Open the folder with the Global Burden of Disease data.
#'
#' Wrapper around `system2("open", path)`.
#'
#' @inheritParams gbd_data_read
#'
#' @return The value of `base::system2("open", path)`, which this function wraps.
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#'
#' gbd_open()
#' }
gbd_open <- function(path) {
  path <- get_path(path, missing(path))
  # OLD - NOT GOOD: If `path` does not exist, it is newly created.
  # if (!file.exists(path)) {
  #   warning("The folder ",path," foes not exist. Created a new folder with that path.")
  #   dir.create(path)
  # }
  system2("open", rev(shQuote(c(
    path
  ))))
}

get_gbd <- function(data, data_missing) {
  # Todo: make sure gbd is assigned globally?? Would solve the issue? No: keep it flexible for flexible users working in local envirs

  data_str <- rlang::as_string(rlang::ensym(data))
  if (data_missing) {
    message("Getting data from object `gbd`.")
    if (exists("gbd", envir = rlang::caller_env())) {
      y <- get("gbd", envir = rlang::caller_env())
      #message("Got data from object named `gbd`.")
    } else {
      #stop("get gbd nrow gbd", get("gbd"))
      message("Object `gbd` not found. Loading GBD data and assigning it to `gbd`.")
      y <- gbd_load()
      #stop("Object `gbd` not found. Did you forget to `gbd_load()`? To learn more, call `vignette(\"better\")`.")
    }
  } else {
    # if (data_str == "gbd") {
    #   #
    # }
    # if (exists(data_str)) {
    #
    # }
    y <- data
  }
  return(y)
}

#' GBD codebook
#'
#' @param ... NOT YET IMPLEMENTED: Parts of the codebook you want to subset.
#' @param data GBD dataset as `gbd_dataset` object
#'
#' @return GBD codebook, list
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#' }
#'
#' # GBD functions by default read data from `gbd`. See `vignette(\"better\")`
#' gbd <- gbd_read()
#'
#' gbd_codebook()
gbd_codebook <- function(..., data) {

  srch <- unlist(list(...))

  # TODO: if it searches "DALYs" or whatever, you get info
  if (exists("gbd", envir = rlang::caller_env())) {
    gbd <- get("gbd", envir = rlang::caller_env())
  }
  data <- get_gbd(data, missing(data))

  #browser()
  attr(data,"codebook")
}#; gbd_codebook("reis")#; stop("n")

gbd_codebooks <- function(book, ..., data) {

  #browser()

  srch <- unlist(list(...))#purrr::map_chr(rlang::ensyms(...),rlang::as_string) # enexprs

  #browser()
  if (exists("gbd", envir = rlang::caller_env())) {
    gbd <- get("gbd", envir = rlang::caller_env())
  }
  data <- get_gbd(data, missing(data))

  y <- attr(data,"codebook")[[book]]

  #browser()
  if (length(srch)>0) {
    i <- stringr::str_detect(y[[stringr::str_remove(book,"s$")]], stringr::regex(srch[1],ignore_case=TRUE))
    y <- y[i,]
  }
  return(y)
}#; gbd_codebooks("measures")

#' Get codebook for GBD variables
#'
#' @param ... Which variables you want a codebook for, string.
#'
#' @return Codebook, dataframe
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' gbd_variables("metric")
gbd_variables <- function(...) {
  gbd_codebooks("variables", ...)
}#; gbd_measures(DALYs)

#' Get codebook for GBD causes
#'
#' @param ... Which causes you want a codebook for, string.
#'
#' @return Codebook, dataframe
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' gbd_causes("cardiovascular diseases")
gbd_causes <- function(...) {

  gbd_codebooks("causes", ...)
}

#' Get codebook for GBD measures
#'
#' @param ... Which measures you want a codebook for, string.
#'
#' @return Codebook, dataframe
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' gbd_measures("DALYs")
gbd_measures <- function(...) {
  gbd_codebooks("measures", ...)
}

#' Get codebook for GBD reis
#'
#' @param ... Which reis you want a codebook for, string.
#'
#' @return Codebook, dataframe
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' gbd_reis("behavioral risks")
gbd_reis <- function(...) {
  gbd_codebooks("reis", ...)
}

#' Subset the Global Burden of Disease dataset
#'
#' Subset the Global Burden of Disease dataset with sensible defaults. Note that `argument = NA` means filtering out `NA`s, while `argument = NULL` means not filtering out anything (keeping all). `gbd_filter` is all about the sensible defaults. If you need many arguments to be `NULL`, consider using `dplyr::filter(gbd, ...)` instead.
#'
#' @param cause Cause(s) of death/disability
#' @param measure Measure of returned values
#' @param metric Metric for returned values
#' @param rei Risk factor for the disease, impairment, or etiology
#' @param location Location for the data
#' @param sex Sex of the population
#' @param age Age of the population
#' @param year Year of data collection
#' @param data `r txt$gbd_doc_dataset`
#' @param params List of parameters. Rather than feeding the parameters as arguments, like for instance gbd_filter(cause = "all causes", measure = "DALYs"), you can feed the parameters as a list, like for instance gbd_filter(params = list(cause = "all causes", measure = "DALYs"))
#'
#' @return GBD data for given parameters, as dataframe.
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#' }
#'
#' # GBD functions by default read data from `gbd`. See `vignette(\"better\")`
#' gbd <- gbd_read()
#'
#' gbd_filter(cause = "cardiovascular diseases")
gbd_filter <- function(
  cause = "all causes",
  measure = "DALYs",
  metric = "rate",
  rei = NA, #"Behavioral risks", #NA,#"Behavioral risks",
  location = "Global", # locations are with capital
  sex = "both", age = "all ages", year = max(data$year),
  data,
  params = list()
  ) {

  list2env(params,environment())

  if (exists("gbd", envir = rlang::caller_env())) {
    gbd <- get("gbd", envir = rlang::caller_env())
  }
  data <- get_gbd(data, missing(data))

  #browser()
  if (any(measure %in% c("prevalence"))) {
    rei <- NA
  } else if (any(measure %in% c("summary exposure value"))) {
    cause <- NA
  }

  #browser()
  year <- year # needed (maybe force?)

  #browser()

  y <- data
  for (i in c(
    "cause", "measure", "metric", "rei",
    "location", "sex", "age", "year"
  )) {
    if (!is.null(get(i))) { # used to be !all(is.na(get(i))) before semantic change to NULL meaning don't filter
      y <- y[y[[i]] %in% get(i),]
      #x[[i]]
    }
    #browser()
  }

  #browser()

  for (i in rev(c(
    "cause",
    "rei",
    "measure", "metric", "location", "sex", "age", "year"
    ))) {
    #if (i=="rei") browser()
    if ( length(unique(y[[i]]))>1 ) {
      ordi <- factor(y[[i]],levels=get(i))
      y <- dplyr::arrange( y, ordi )
    }
  }
  if (nrow(y) == 0) {
    warning("No data found for the given parameters. Typo? Or should you download the data?")
  }

  return(y)
}#; gbd_filter(data=gbd_filter(params=list(cause = "diabetes and kidney diseases", metric = "percent")))



#' Filter Global Burden of Disease data on behavioral risks
#'
#' Wrapper around `gbd_filter`
#'
#' @param ... Parameters for `gbd_filter`
#' @param rei Risk factor for the disease, impairment, or etiology
#' @param data `r txt$gbd_doc_dataset`
#'
#' @return GBD data for given parameters, as dataframe.
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#' }
#'
#' # GBD functions by default read data from `gbd`. See `vignette(\"better\")`
#' gbd <- gbd_read()
#'
#' gbd_behavior(cause = "cardiovascular diseases")
gbd_behavior <- function(..., rei = "Behavioral risks", data) {
  if (exists("gbd", envir = rlang::caller_env())) {
    gbd <- get("gbd", envir = rlang::caller_env())
  }
  data <- get_gbd(data, missing(data))
  gbd_filter(
    data = data,
    rei = rei,
    ...
  )
}


gbd_analyze <- function() {
  # do DALYs, etc, comparisons etc
  # give it all parameters, but then it gives useful comparison parameters and other useful stats
}


gbd_summary <- function(...) {
  gbd_describe(..., summary = TRUE)
}#; gbd_summary(cause = "cardiovascular diseases", rei = "behavioral risks")


#' Describe gbd_dataset object
#'
#' @param ... Parameters determining what GBD data you want to describe. Arguments passed on to `gbd_filter`.
#' @param summary Whether or not to provide a brief summary instead of a description, logical
#' @param explain Whether or not to add an explanation of the measure, logical
#'
#' @return Description as string
#' @export
#'
#' @examples
#' gbd_describe(cause = "cardiovascular diseases")
gbd_describe <- function(..., explain = TRUE, summary = FALSE) {

  #browser()
  sct <- gbd_filter(...)

  # feed it either cause or rei
  if (is.na(sct$rei)) {
    y <- gbd_describe_cause(sct, explain = explain, summary = summary)
  } else if (sct$measure == "summary exposure value") {
    y <- gbd_describe_sev(sct, explain = explain, summary = summary)
  } else {
    y <- gbd_describe_rei(sct, explain = explain, summary = summary)
  }
  #browser()
  cat(pretty_paragraphs(y))
  invisible(y)
}#; gbd_describe(cause = "cardiovascular diseases", rei = "behavioral risks")
gbd_describe_mold <- function(
    x,
    estimated_what,
    summary,
    explain
) {
  if (summary) {
    y <- paste0(
      "Estimated ", estimated_what, ":\n  ",
      pretty_interval(x$val, x$lower, x$upper, measure = x$measure, metric = x$metric),
      "\nParameters: year = ", x$year, ", location = ", x$location,
      ", sex = ", x$sex, ", age = ", x$age
    )
  } else {
    y <- paste0(
      "In ", x$year, ", ", pretty_location(x$location),
      ", the estimated ", estimated_what,
      ", for ", pretty_sex(x$sex),
      " of ", pretty_age(x$age),
      ", was ", pretty_interval(x$val, x$lower, x$upper, measure = x$measure, metric = x$metric),".",
      ifelse(explain,paste0(" ", explain_measure(x$measure)),"")
    ) #%>% cat
  }

  #browser()
  return(y)
}

pretty_location <- function(x) {
  ifelse(x == "Global", "globally", paste("in",x))
}
pretty_cause <- function(x) {
  x
}
pretty_sex <- function(x) {
  dplyr::case_when(
    x == "male" ~ "men",
    x == "female" ~ "women",
    x == "both" ~ "both men and women"
  )
}
pretty_age <- function(x) {
  #gbd$age %>% unique
  dplyr::case_when(
    x == "age-standardized" ~ "all ages (age-standardized)",
    stringr::str_ends(x, "years") ~ paste(x, "old"),
    TRUE ~ x
  )
}
explain_measure <- function(measure) { # , metric
  if (measure %in% c("deaths", "prevalence", "incidence")) {
    y <- ""
  } else {
    y <- paste0(
      "Note that ", measure, " refers to ",
      gbd_measures(measure)$explain, "."
    )
  }
  #browser()
  return(y)
}#; explain_measure("DALYs")#; explain_measure(measure = "DALYs", metric = "rate")

gbd_describe_cause <- function(x, ...) {
  y <- gbd_describe_mold(x, paste0(
    "cost of ", pretty_cause(x$cause)
    ), ...) #%>% cat
  #browser()
  return(y)
}#; gbd_describe_cause(gbd_filter("cardiovascular diseases"))
pretty_rei <- function(x) {
  x
}
gbd_describe_rei <- function(x, ...) {
  #browser()
  y <- gbd_describe_mold(x, paste0(
    "cost of ", pretty_cause(x$cause),
    " due to ", pretty_cause(x$rei)
    ), ...) #%>% cat
  #browser()
  return(y)
}#; gbd_describe(cause = "cardiovascular diseases", rei = "behavioral risks")
pretty_sev <- function(x) {
  x
}
gbd_describe_sev <- function(x, ...) {
  #browser()
  y <- gbd_describe_mold(x, paste0(
    "exposure to ", pretty_sev(x$rei)
    ), ...) #%>% cat
  #browser()
  return(y)

}#; gbd_describe(rei = "behavioral risks", measure = "summary exposure value")



gdb_codebook <- function(data) {
  if (exists("gbd", envir = rlang::caller_env())) {
    gbd <- get("gbd", envir = rlang::caller_env())
  }
  data <- get_gbd(data, missing(data))
  attr(data,"codebook")
}

browse_recursion <- function(data, codebook, parent, var) {

  #browser()
  ifparent <- codebook[[var]] %in% parent
  if (sum(ifparent) == 0) {
    #browser()
    stop("Category not found: ", parent)
  }
  message("Category:")
  cb0 <- codebook[ifparent,]
  print(cb0, n = Inf)

  message("\nData already downloaded for this parent category:")
  #browser()
  di <- data[data[[var]] %in% parent,]
  if (nrow(di) == 0) {
    message("  No data downloaded for this category.")
    #return(parent)
  } else {
    print(di, n = 5)
  }

  message("If you want to download more data, you can quit and run: gbd_download()\n\nSubcategories:")
  cbi <- codebook[
    codebook$parent %in% parent &
      !codebook[[var]] %in% c("all causes", "all risk factors"),
    ]
  if (nrow(cbi) == 0) {
    message("  No subcategories to browse: quit ('q') or go back ('b').")
    #return(parent)
  } else {
    print(cbi, n = Inf)
  }
  cat(" ")
  PRMPT <- paste0(
    "Enter row nr of category to browse and hit return: \n  Other options:\n  - ",
    paste(
      "Hit return without input to pick first row",
      "Enter search term",
      "Enter 'b' to go back",
      "Enter 'q' to quit",
      sep = ";\n  - "
    )
  )#; nchar(PRMPT)
  inp <- readline(PRMPT)
  if (inp %in% c("q","Q")) {
    return(parent)
  } else if (inp %in% c("b","B")) {
    #browser()
    y <- browse_recursion(data, codebook, cb0$parent[1], var)
    return(y)
  } else if (inp %in% "") {
    rwi <- 1
  } else if (is.na(as.numeric(inp))) {
    #browser()
    hits <- stringr::str_subset(
      codebook[[var]],
      stringr::regex(inp, ignore_case = TRUE)
      )
    if (length(hits) > 0) {
      y <- browse_recursion(data, codebook, hits, var)
    } else {
      message("Search term not found.")
      y <- browse_recursion(data, codebook, parent, var)
    }

    return(y)

  } else {
    rwi <- as.numeric(inp)
  }
  cbi_rwi <- cbi[[var]][rwi]
  if (length(cbi_rwi) == 0) {
    message("Invalid row number: Try again...\n")
    y <- browse_recursion(data, codebook, parent, var)
  } else {
    y <- browse_recursion(data, codebook, cbi_rwi, var)
  }
  return(y)
}
browse_var <- function(x, var) {
  #browser()
  y <- browse_recursion(
    x,
    attr(x,"codebook")[[paste0(var,"s")]], # ?? ifelse(var == "rei", "risk", var)
    ifelse(var == "rei", "all risk factors", "all causes"),
    var
    )
  message("\nReturning last category chosen...")
  return(y)
}

#' Browse Global Burden of Disease causes
#'
#' @param data `r txt$gbd_doc_dataset`
#'
#' @return Last cause category browsed, as string.
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#'
#' # GBD functions by default read data from `gbd`. See `vignette(\"better\")`
#' gbd <- gbd_read()
#'
#' gbd_browse_cause()
#' }
gbd_browse_cause <- function(data) {
  if (exists("gbd", envir = rlang::caller_env())) {
    gbd <- get("gbd", envir = rlang::caller_env())
  }
  data <- get_gbd(data, missing(data))
  browse_var(data,"cause")
}

#' Browse Global Burden of Disease risks, etiologies and impairments
#'
#' @param data `r txt$gbd_doc_dataset`
#'
#' @return Last cause category browsed, as string.
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#'
#' # GBD functions by default read data from `gbd`. See `vignette(\"better\")`
#' gbd <- gbd_read()
#'
#' gbd_browse_rei()
#' }
gbd_browse_rei <- function(data) {
  if (exists("gbd", envir = rlang::caller_env())) {
    gbd <- get("gbd", envir = rlang::caller_env())
  }
  data <- get_gbd(data, missing(data))
  browse_var(data,"rei")
}

new_gbd <- function(x = tibble::tibble(), codebook = list()) {
  stopifnot(tibble::is_tibble(x))
  stopifnot(is.list(codebook))
  structure(x,
            class = c("gbd_dataset", "tbl_df", "tbl", "data.frame"),
            codebook = codebook
  )
}
validate_gbd <- function(x) {
  # if (!all(purrr::map_chr(x,typeof) == attr(x,"codebook")$type)) {
  #   stop(
  #     "Codebook lists incorrect type.",
  #     call. = FALSE
  #   )
  # }
  x
}
#' Print objects of the `gbd_dataset` class
#'
#' @param x `gbd_dataset` object
#' @param ... Arguments passed on to `tibble::print.tbl_df` for the main dataframe, which is a `tibble`.
#'
#' @return original `gbd_dataset` object
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#' }
#'
#' gbd <- gbd_read()
#' print(gbd)
#' gbd
print.gbd_dataset <- function(x, ...) {
  message(
    stringr::str_wrap(paste0(
      "This is a `gbd_dataset` object - a dataframe with \"codebook\" attribute containing metadata (",
      paste(names(attr((x), "codebook")), collapse = ", "),
      "). Call attr(.,\"codebook\") to retrieve it."
    )),
    "\n  ",
    txt$gbd_boiler
  )
  #browser()
  # class(gbd) <- class(gbd)[!class(gbd) %in% "gbd_dataset"] # not good
  NextMethod(...)
}

new_effect <- function(
  value,
  lower = NA_real_,
  upper = NA_real_,
  stat = NA,
  unit = NA,
  explain = NA,
  call = NA,
  params = list()
) {
  stopifnot(is.numeric(value))
  stopifnot(is.numeric(lower))
  stopifnot(is.numeric(upper))
  y <- list(
    value = value,
    lower = lower,
    upper = upper,
    stat = stat,
    unit = unit,
    explain = explain,
    call = call,
    params = params
  )
  structure(y,
            class = c("better_effect", "list")
  )
}

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

pretty_value <- function(val, low, high, unit = "", sep = " ", spec = ", 95% CI", pct_100 = TRUE) {
  if (unit %in% c("","%")) sep <- ""
  if (unit == "%" & pct_100) {
    val <- 100*val
    low <- 100*low
    high <- 100*high
  }
  nrs <- pretty_nr(val,low,high)
  y <- paste0(nrs[1],sep,unit,spec," [",nrs[2],", ",nrs[3],"]", collapse = "")
  return(y)
}#; pretty_value(837.9283745, 398.83, 0.0038, "days"); pretty_value(837.9283745, 398.83, 0.0038)

#' Print objects of `better_effect` class
#'
#' @param x `better_effect` object
#'
#' @return Original `better_effect` object
#' @export
#'
#' @examples
#' eff <- nudge_to_behavior()
#' print(eff)
print.better_effect <- function(x) {
  # Contrary to print, I don't allow .... NOT: argums <- list(...)
  # This generates a "checking S3 generic/method consistency" warning, but so be it
  #browser()
  msg <- paste0(
    if (length(x$explain) > 1) "\n\n",
    "Call:   ", stringr::str_wrap(x$call, exdent = 2),
    "\n",
    ifelse(
      length(x$params)>0,
      stringr::str_wrap(
        paste0("Param.: ",
               paste( paste0(names(x$params)," = ",x$params), collapse=", ")
               ),
        exdent = 2
        ),
      ""
    ),
    "\n\n  Value:\n\n",
    x$stat," in ",ifelse(is.na(x$unit),"",x$unit),":\n\n",
    pretty_value(x$value,x$lower,x$upper),
   "\n\n  Explanation:\n\n",
   quiet(explanation(x, full = FALSE, truncated = TRUE)),
   if (length(x$explain) == 1) {
     paste0(
       "\n\n* This is a `better_effect` object. Call `str(.)` to see it's structure.",
       "\n** Call `explanation(.)` to get the full explanation."
     )
   }
  )
  cat(msg)
  invisible(x)
}

#' Get explanation of `better_effect` object
#'
#' @param x `better_effect` object
#' @param full Get full explanation or partial explanation
#' @param truncated Whether or not to truncate long explanations
#'
#' @return Explanation as string
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#' }
#'
#' library(tidyverse)
#'
#' gbd <- gbd_read()
#'
#' nudge_to_behavior() %>%
#'   behavior_cd_to_pp %>%
#'   explanation
explanation <- function(x, full = TRUE, truncated = FALSE) {

  expl <- x$explain
  if (full) {
    expl <- paste(expl,collapse = "\n\n----\n\n")
  } else {
    expl <- utils::tail(expl, 1)
  }
  if (truncated) {
    expln <- 300
    if (nchar(expl) > expln*3) {
      y <- paste0(substr(expl,1,expln), "...\n  [truncated - call `explanation(*)` for full explanation]\n...",
                  substr(expl,nchar(expl)-expln,nchar(expl)))
    } else {
      y <- expl
    }
  } else {
    y <- expl
  }
  cat(y)
  invisible(y)
}

#' Get data on the effect sizes of nudges
#'
#' This data is used by other `better` functions.
#'
#' @return Data on the effect sizes of nudges
#' @export
#'
#' @examples
#' nudge_data()
nudge_data <- function() {

  # TODO: other nonhealth behaviors??

  ds <- tibble::tribble(
    ~behavior, ~nudge, ~d, ~low, ~high,
    NA, NA, 0.43, 0.38, 0.48,
    "health" , NA, .34, .25, .43,
    "food" , NA, .65, .47, .83,
    NA, "information", .34, 0.27, 0.42,
    NA, "structure", .54, 0.46, 0.62,
    NA, "assistance", .28, 0.21, 0.35,
    "health", "information", .26, 0.09, 0.43,
    "health", "structure", .44, 0.29, 0.59,
    "health", "assistance", .20, 0.05, 0.35,
    "food", "information", .44, 0.19, 0.70,
    "food", "structure", .78, 0.54, 1.01,
    "food", "assistance", .43, 0.28, 0.59
  )
  ds <- replace(ds, is.na(ds), "all")

  cb <- list(
    behaviors = tibble::tribble(
      ~behavior,
      "health",
      "food",
      "environment",
      "finance",
      "pro-social",
      "other"
    ),
    nudges = tibble::tribble(
      ~nudge, ~explain,
      "information", "intervention that focuses on the description of alternatives (decision information), by increasing the availability, comprehensibility, and/or personal relevance of information (e.g., visibility of information, social normative information)",
      "structure", "intervention that targets the organization and structure of choice alternatives (decision structure), by altering the utility of choice options through their arrangement in the decision environment or the format of decision making (e.g., defaults, incentives)",
      "assistance", "intervention that reinforces behavioral intentions (decision assistance), by facilitating self-regulation (e.g., reminders, commitment)",
    )
  )

  attr(ds,"codebook") <- cb

  message("Citation: ", txt$nudge_cite)
  return(ds)
}

#' Get the codebook for the nudge data
#'
#' @param x Nudge dataset, by default automatically read.
#'
#' @return Codebook, dataframe.
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' nudge_codebook()
nudge_codebook <- function(x = nudge_data()) {
  attr(x,"codebook")
}

pretty_call <- function(call, defaults) {
  defs <- defaults[!names(defaults) %in% names(call)] #%>% print
  defs <- purrr::map_chr(defs,~paste0(.x,collapse=", "))
  defs <- defs[defs != ""]
  #browser()
  y <- paste0(
    utils::capture.output(call),
    ifelse(
      length(defs) > 0,
      paste0(", with default arguments: ", paste0(names(defs)," = ",defs,collapse=", ")),
      ""
    )
  )# %>% print
  return(y)
}#; pretty_call(call, defaults)

pretty_nudge <- function(behavior, nudge) {
  paste0(
    "behavior category \"", behavior,
    "\" and nudge category \"", nudge, "\""
  )
}

#' Given a nudge, get its effect on behavior
#'
#' @param behavior Category of behavior
#' @param nudge Category of nudge
#'
#' @return Effect size, object of the `better_class` effect
#' @export
#'
#' @examples
#' nudge_to_behavior()
nudge_to_behavior <- function(nudge = "all", behavior = "all") {

  fcall <- pretty_call(sys.call(), formals())

  #browser()

  message("Looking up data on nudges...")

  ds <- nudge_data()

  dsi <- ds[ds$behavior %in% behavior & ds$nudge %in% nudge,]

  #browser()

  # reason:
  stati <- paste0("Cohen's d for ",pretty_nudge(behavior=dsi$behavior,nudge=dsi$nudge))
  rsn <- pretty_paragraphs(
    paste0(stati, " is ",pretty_value(dsi$d, dsi$low, dsi$high),"."),
    "This statistic comes from a meta-analysis on the effectiveness of nudging. This is the abstract of the original paper: \"Over the past decade, choice architecture interventions or socalled nudges have received widespread attention from both researchers and policy makers. Built on insights from the behavioral sciences, this class of behavioral interventions focuses on the design of choice environments that facilitate personally and socially desirable decisions without restricting people in their freedom of choice. Drawing on more than 200 studies reporting over 440 effect sizes (n = 2,148,439), we present a comprehensive analysis of the effectiveness of choice architecture interventions across techniques, behavioral domains, and contextual study characteristics. Our results show that choice architecture interventions overall promote behavior change with a small to medium effect size of Cohen's d = 0.43 (95% CI [0.38, 0.48]). In addition, we find that the effectiveness of choice architecture interventions varies significantly as a function of technique and domain. Across behavioral domains, interventions that target the organization and structure of choice alternatives (decision structure) consistently outperform interventions that focus on the description of alternatives (decision information) or the reinforcement of behavioral intentions (decision assistance). Food choices are particularly responsive to choice architecture interventions, with effect sizes up to 2.5 times larger than those in other behavioral domains. Overall, choice architecture interventions affect behavior relatively independently of contextual study characteristics such as the geographical location or the target population of the intervention. Our analysis further reveals a moderate publication bias toward positive results in the literature. We end with a discussion of the implications of our findings for theory and behaviorally informed policy making.\"",
    "Citation: ", txt$nudge_cite
  )#; cat(rsn)
  #message(rsn)

  #print_estimate(y)
  #browser()
  new_effect(
    value = dsi$d,
    lower = dsi$low,
    upper = dsi$high,
    stat = stati,
    unit = "standard deviations",
    explain = rsn,
    call = fcall,
    params = list(
      rei = "behavioral risks",
      location = "Global", # the cohen d s are from studies all over the world (since one of the variables they have is inside vs outside the US)
      behavior = behavior, nudge = nudge
    )
  )
}#; nudge_to_behavior(behavior = "health") #; behavior_impact()

nudge <- function(nudge, behavior = "all") {

}
behavior <- function(behavior, nudge = "all") {

}

#' Given a behavior's Cohen's, get its percentage points difference
#'
#' @param effect A `better_effect` object containing the Cohen's d.
#' @param ... Parameters to be passed on to `gbd_filter`.
#' @param data GBD data as `gbd_dataset` object.
#'
#' @return A `better_effect` object containing the percentage difference.
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#' }
#'
#' library(tidyverse)
#'
#' # GBD functions by default read data from `gbd`. See `vignette(\"better\")`
#' gbd <- gbd_read()
#'
#' nudge_to_behavior() %>%
#'   behavior_cd_to_pp
behavior_cd_to_pp <- function(effect, ..., data) {

  force(effect)

  fcall <- pretty_call(sys.call(), formals())

  if (exists("gbd", envir = rlang::caller_env())) {
    gbd <- get("gbd", envir = rlang::caller_env())
  }
  message("\nTranslating Cohen's d into percentage point difference. Looking up data on summary exposure value...")
  data <- get_gbd(data, missing(data))

  rei <- effect$params$rei # TODO: allow it to be set manually

  #browser()
  pars <- c(
    list(...), effect$params[names(effect$params) %in% attr(gbd,"codebook")$variables$variable],
    list(measure = "summary exposure value", data = data) # for SEV, metric is always "Rate"
  )
  # todo: delete duplicates, and deal with inconsistent multiple values for arguments, otherwise dubble matching error
  sev <- gbd_filter(params = pars)

  # d = sqrt(p*(1-p))
  # d^2 = p*(1-p)
  # d^2 = p1-p^2

  # x1 <- 0:100/100
  # plot(x1, sqrt(x1-x1^2))

  sd_prop <- function(x) {
    sqrt(x*(1-x))
  }

  #browser()

  SD <- sqrt( (sev$val/100)*(1-(sev$val/100)) )*100

  # ---------
  # Theory (keep: shows that proportion SDs is same as Cohen s d)
  # diffs <- c()
  # for (afx in c(.1,.3,.5,.7,.9, .99)) {
  #   for (befx in c(.1,.3,.5,.7,.9)) {
  #     nx <- 10000
  #     #befx <- .8 #%
  #     #afx <- .01
  #     dta <- tibble( # weird map_dbl is because a bug in rep
  #       bin = c(rep(1,nx*befx), map_dbl(seq(from = 1, to = nx*(1-befx)),~0), rep(1,nx*afx), rep(0,nx*(1-afx))),
  #       group = c(rep("before",nx), rep("after",nx))
  #     )
  #     print(effsize::cohen.d(bin ~ group, dta, pooled=FALSE))
  #     print((befx-afx)/sd_prop(befx))
  #     #(befx-afx)/sd_prop(afx)
  #     diffs <- c( diffs, (-(befx-afx)/sd_prop(befx)) - effsize::cohen.d(bin ~ group, dta, pooled=FALSE)$estimate )
  #     print(diffs)
  #     cat("sd\n")
  #     print(sd(dta$bin[dta$group == "before"]))
  #     print(sd_prop(befx))
  #     #browser()
  #   }
  # }
  # ---------

  #browser()
  # gbd_summary(params = pars)
  # tolower_1(gbd_describe(params = pars, explain = FALSE))

  new_effect(
    value = effect$value*SD,
    lower = effect$lower*SD,
    upper = effect$upper*SD,
    stat = paste0("percentage point decrease in exposure to ",sev$rei),
    unit = "percentage points",
    explain = c(effect$explain, pretty_paragraphs(
      paste0(
        "We estimate that the percentage point decrease in ",sev$rei," exposure is ",
        pretty_value(effect$value*SD, effect$lower*SD, effect$upper*SD, unit = "percentage points"),"."
        ),

      paste0(
        "Note that, if the estimate has a minus sign, it indicates an increase in ",sev$rei,". ",
        "We derive this from the ",effect$stat,
        ", which is ",pretty_value(effect$value,effect$lower,effect$upper),
        ". Since Cohen's d describes the effect in terms of standard deviations, we estimate the effect in percentage point difference based on the fact that, for proportions, the standard deviation equals the square root of p*(1-p), where p is the proportion. For computational simplicity, we assume Cohen's d is calculated using the standard deviation of the control group (without a behavioral nudge) rather than the pooled standard deviation. The baseline proportion, in turn, is derived from data from the Global Burden of Disease study. According to the GBD dataset, ",
        tolower_1(quiet(gbd_describe(params = pars, explain = FALSE))),
        " Note that GBD defines the Summary Exposure Value (SEV) as a measure of a population's exposure to a risk factor that takes into account the extent of exposure by risk level and the severity of that risk's contribution to disease burden. SEV takes the value zero when no excess risk for a population exists and the value one when the population is at the highest level of risk; they report SEV on a scale from 0% to 100% to emphasize that it is risk-weighted prevalence. So the SEV is the proportion exposed. The standard deviation is then sqrt(p*(1-p)), that is, ",pretty_nr(SD)," percentage points. Our estimate of the percentage point difference simply multiplies the Cohen's d and its confidence interval by this standard deviation. If the jump from Cohen's d to proportions is difficult to follow, think of proportion data, like the Summary Exposure Value data, as numeric data with two values: 0 and 1. You can calculate the mean of that (bivalent) numeric variable as usual: you would find it equals the proportion. You can also calculate the standard deviation as usual: you would find it equals sqrt(p*(1-p)). Since we have two groups - a control group and a nudged group - you can also calculate Cohen's d as usual."
      ),

      txt$gbd_boiler
    )),
    call = fcall,
    params = effect$params
  ) #%>% explanation # uncomment ???

}
# nudge_to_behavior(behavior = "health", nudge = "information") %>%
#   print %>%
#   behavior_cd_to_pp(cause = "cardiovascular diseases") %>%
#   print %>%
#   explanation

#gbd_read(load = TRUE) # ???

gbd_unit <- function(measure, metric = NA) {
  #gbd$measure %>% unique
  #browser()

  if (measure == "summary exposure value") {
    y <- "percent"

  } else {
    if (measure %in% c("YLDs", "YLLs", "DALYs", "deaths")) {
      y <- measure
    } else if (measure %in% c("incidence", "prevalence")) {
      y <- "cases"
    }

    #gbd$metric %>% unique
    if (metric == "rate") {
      y <- paste0(y," per 100,000 people")
    } else if (metric == "percent") {
      y <- paste0("percent of ", y," relative to all causes")
    }
  }
  return(y)
}#; gbd_unit("summary exposure value")#; gbd_unit("DALYs", "rate");

pretty_interval <- function(val, lower, upper, measure, metric) {
  pretty_value(
    val = val, low = lower, high = upper,
    unit = gbd_unit(measure = measure, metric = metric)
  )
}#; pretty_interval(10.8987,8.1,11.89,"DALYs","rate")

#' Given a behavior effect, get a disease effect
#'
#' @param effect The behavior effect to be translated into a disease effect, class `better_effect`
#' @param cause The disease, that is, the cause of mortality and/or disability
#' @param measure The measure for mortality/disability
#' @param metric The metric (rate, percentage, number) for the measure
#' @param ... Other arguments for `gbd_filter`.
#' @param data The GDB dataset to use. By default, `gbd`.
#'
#' @return The effect on disease mortality and/or disability, class `better_effect`
#' @export
#'
#' @examples
#' \dontshow{
#' options(better.gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data")
#' }
#' \dontrun{
#' # If you haven't already:
#' # Add the following to your .Rprofile (and restart):
#' options(better.gbd_path = "/Users/johnwilliam/datasets/Global Burden of Disease data")
#' # See `vignette(\"better\")`
#' }
#'
#' library(tidyverse)
#'
#' nudge_to_behavior(behavior = "health", nudge = "information") %>%
#' behavior_cd_to_pp %>%
#'   behavior_to_disease(cause = "cardiovascular diseases")
behavior_to_disease <- function(
  effect,
  cause,
  measure = "DALYs", #best just display: c("deaths", "YLLs", "YLDs", "DALYs"), # They don't have this for Risks: "life expectancy","HALE"
  metric = "rate", #best just display: c("number", "percent", "rate"),
  ..., data
  ) {
  # from behavior impact to DALYs impact

  #browser()

  fcall <- pretty_call(sys.call(), formals())

  if (exists("gbd", envir = rlang::caller_env())) {
    gbd <- get("gbd", envir = rlang::caller_env())
  }
  message("Looking up data on cause of disease/disability...")
  data <- get_gbd(data, missing(data))
  #browser()
  #cause <- c(list(...),effect$params)$cause
  rei <- c(list(...),effect$params)$rei

  # What is a DALY?
  #   DALY is an abbreviation for disability-adjusted life year. It is a universal metric that allows researchers and policymakers to compare very different populations and health conditions across time. DALYs equal the sum of years of life lost (YLLs) and years lived with disability (YLDs). One DALY equals one lost year of healthy life. DALYs allow us to estimate the total number of years lost due to specific causes and risk factors at the country, regional, and global levels.
  #
  # Back to top
  #
  # What is a YLL?
  #   Years of life lost (YLLs) are years lost due to premature mortality. YLLs are calculated by subtracting the age at death from the longest possible life expectancy for a person at that age. For example, if the longest life expectancy for men in a given country is 75, but a man dies of cancer at 65, this would be 10 years of life lost due to cancer.
  #
  # Back to top
  #
  # What is a YLD?
  #   YLD is an abbreviation for years lived with disability, which can also be described as years lived in less than ideal health. This includes conditions such as influenza, which may last for only a few days, or epilepsy, which can last a lifetime. It is measured by taking the prevalence of the condition multiplied by the disability weight for that condition. Disability weights reflect the severity of different conditions and are developed through surveys of the general public.
  #
  # Back to top
  #
  # What is life expectancy?
  #   Life expectancy is the number of years a person can expect to live at any given age.
  #
  # Back to top
  #
  # What is HALE?
  #   HALE is often referred to as healthy life expectancy. Unlike life expectancy, HALE takes into account mortality and nonfatal outcomes. HALE does this by summarizing years lived in less than ideal health (YLDs) and years lost due to premature mortality (YLLs) in a single measure of average population health for individual countries.

  #browser()
  pars <- list(
      measure = NULL,
      metric = NULL,
      rei = c(NA,rei),
      cause = c(NA,cause),
      data = data
  )
  sct <- gbd_filter(params = pars)

  parsi <- c(
    pars,
    list(
      rei = rei,
      measure = measure,
      metric = metric
    )
  )
  scti <- gbd_filter(params = parsi) # sct[i,]

  if (nrow(scti) == 0) {
    #browser()
    stop("Data subsetted is empty.")
  } else if (nrow(scti) > 1) {
    stop("Subset of data not unique (multiple rows).")
  }

  msrs <- attr(data,"codebook")$measures

  #browser()

  # For testing
  # effect
  # effect %>% explanation
  # effect$value
  #
  # sct[sct$measure %in% "summary exposure value",]
  # sct$val[sct$measure %in% "summary exposure value"]
  # gbd %>%
  #   filter(measure == "summary exposure value", round(val, 1) == 15.3)

  # Reasoning:
  # Lowering the risk exposure means proportionally lowering deaths etc
  # We assume that zero risk exposure means zero deaths (= correct, because of GBD definitions)

  before <- sct$val[sct$measure %in% "summary exposure value"]; before
  after <- min(100, max(0, sct$val[sct$measure %in% "summary exposure value"] - effect$value)); after
  # SEV between 0 and 100
  pct <- (before - after)/before; pct
  before*pct
  before - after

  before
  afterL <- min(100, max(0, sct$val[sct$measure %in% "summary exposure value"] - effect$lower)); afterL
  pctL <- (before - afterL)/before; pctL

  before
  afterU <- min(100, max(0, sct$val[sct$measure %in% "summary exposure value"] - effect$upper)); afterU
  pctU <- (before - afterU)/before; pctU

  #effect %>% str

  #scti$val*pct


  #browser()

  nrs <- pretty_nr(
    exposure_change = effect$value,
    exposure_before = before,
    exposure_after = after,
    harm_before = scti$val,
    pct = pct,
    pctL = pctL,
    pctU = pctU
    #, harm_disease = NA
  ) #%>% print

  vl <- scti$val*pct
  lw <- scti$lower*pctL # ? how calculate ? scti$lower - scti$lower*pctL ?
  up <- scti$upper*pctU

  #browser()

  y <- quiet(new_effect(
    value = vl,
    lower = lw,
    upper = up,
    stat = measure,
    unit = gbd_unit(measure = measure, metric = metric),
    explain = c( effect$explain, pretty_paragraphs(
      paste0(
        "We estimate that a behavioral intervention in ",pretty_nudge(behavior=effect$params$behavior,nudge=effect$params$nudge), # msrs$explain[msrs$measure == measure]
        " that targets " , cause,
        " saves ", pretty_interval(vl,lw,up, measure = measure, metric = metric),"."
      ),
      paste0(
        "Here is how we arrive at this estimate. We previously estimated that the ",
        effect$stat, " is ",
        pretty_value(effect$value,effect$lower,effect$upper,unit=effect$unit),
        ", taking ",rei," exposure from ",nrs$exposure_before, "% without nudge to ", pretty_value(after,afterU,afterL,unit="%", pct_100 = FALSE),
        ", with nudge. The percentage point difference is ",pretty_value(pct, pctL, pctU, unit = "%"),
        ", of the exposure value without nudge. We also know that at ",nrs$exposure_before,"%, exposure to ",rei,
        " costs ",nrs$harm_before," ",gbd_unit(measure = measure, metric = metric)," due to ",cause, ". That is, we know from the GBD data that ",
        tolower_1(gbd_describe(params = parsi, explain = FALSE)),
        " The last step is to take ", pretty_value(pct, pctL, pctU, unit = "%"),
        ", of ",pretty_value(scti$val, scti$lower, scti$upper, unit = measure),
        ", and to combine both confidence intervals. That gives us the estimate that the nudge saves ", pretty_interval(vl,lw,up, measure = measure, metric = metric),"."
      ), #<----
      txt$gbd_boiler
    ) ),
    call = fcall,
    params = effect$params
  )) #%>% explanation # ???

  return(y)

}
if (FALSE) {

  dvl_options()
  gbd_load()
  nudge_to_behavior(behavior = "health", nudge = "information") %>%
    behavior_cd_to_pp %>%
    explanation
  nudge_to_behavior(behavior = "health", nudge = "information") %>%
    behavior_cd_to_pp %>%
    behavior_to_disease(cause = "cardiovascular diseases") # is with global nudges, so can't do: location = "United States of America"
}

disease <- function(cause) {

}

nudge_to_disease <- function(nudge, disease) {

}

#gbd_describe_cause("cardiovascular diseases")
