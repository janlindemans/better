
# Strings -----------------------------------------------------------------

txt <- list(
  bugs = "The `better` package is still in development. For info on reporting bugs, run `?better`.",
  tou = "For terms of use, call `terms_of_use(.)`.",

  gbd_cite = "Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from https://vizhub.healthdata.org/gbd-results/",
  gbd_cite_short = "Global Burden of Disease Collaborative Network (2020)",

  nudge_cite = "Mertens, S., Herberz, M., Hahnel, U. J. J., & Brosch, T. (2022). The effectiveness of nudging: A meta-analysis of choice architecture interventions across behavioral domains. Proceedings of the National Academy of Sciences, 119(1), e2107346118. https://doi.org/10.1073/pnas.2107346118",
  nudge_cite_short = "Mertens, Herberz, Hahnel, & Brosch (2022)",

  gbd_source_short = "Institute for Health Metrics and Evaluation. Used with permission. All rights reserved.",
  gbd_source = "Source: Institute for Health Metrics and Evaluation. Used with permission. All rights reserved.",

  gbd_source_plus = "Global Burden of Disease Collaborative Network (2020). Used with permission. All rights reserved. For terms of use and more, run `?gbd_filter`.",

  gbd_license_explain = "The Global Burden of Disease dataset is created and maintained by the Institute for Health Metrics and Evaluation at the University of Washington. The dataset is owned by the University of Washington. The `better` package uses the dataset in accordance with the IHME free-of-charge non-commercial user agreement, retrieved from https://www.healthdata.org/Data-tools-practices/data-practices/ihme-free-charge-non-commercial-user-agreement.",
  gbd_agreement = "TERMS AND CONDITIONS OF USE: IHME follows the University of Washington's website terms and conditions: http://www.washington.edu/online/terms/. Commercial organizations wishing to acquire a license to access and download IHME data should contact services@healthdata.org. For non-commercial users of IHME websites, including all pages and datasets found on the healthdata.org, vizhub.healthdata.org, and ghdx.healthdata.org domains, the following constitute prior written permission, as required under Section 7 of the University of Washington's Website Terms and Conditions of Use. [...] Data made available for download on IHME Websites can be used, shared, modified or built upon by non-commercial users in accordance with the IHME FREE-OF-CHARGE NON-COMMERCIAL USER AGREEMENT. https://www.healthdata.org/data-tools-practices/data-practices/terms-and-conditions",
  gbd_agreement_acknowledge = "Data Source Acknowledgment. Proper acknowledgment shall be made for the contributions of each party to any Results being published in accordance with generally accepted scientific standards. In any instance when a Publication includes IHME Data or is derived from IHME Data, User shall include the following source identifier: Source: Institute for Health Metrics and Evaluation. Used with permission. All rights reserved.",
  gbd_agreement_share = "User may not without written permission from UW provide to third parties the ability to download IHME Data Sets from User-provided hosting facilities; User may publish links to IHME's hosting and downloading facilities.",
  gbd_agreement_5 = "5 Restrictions on Use of the IHME Data and Data Sets. Except as expressly provided in this Agreement, or with written consent from UW, User will not (i) copy, reproduce or duplicate the IHME Data or Data Sets; (ii) reveal, publish, transfer, disclose, disseminate, distribute, assign, rent, lease, loan, license, or otherwise make the IHME Data or Data Sets available to any person or organization other than User's employees who have a \"need to know\" with respect to work they are currently undertaking in the course and scope of their employment for internal research purposes or Publication purposes; (iii) modify, translate, merge, alter, reverse compile/assemble, decrypt, reverse engineer or create derivative works from the IHME Data or Data Sets; (iv) utilize in any manner the IHME Data or Data Sets to develop data sets similar in function to the Data Sets; or (v) knowingly permit or enable any third party to do the foregoing. When using the IHME Data or Data Sets for purposes of a Publication, User shall not use them or refer to UW and/or IHME in any way that conveys, either directly or implicitly, an impression that UW and/or IHME support, promote or endorse User's analyses, conclusions, recommendations, services or products.",
  gbd_get_license = "Call `gbd_license()` to access the user agreement.",

  gbd_error_missing = "No `gbd_dataset` object provided, and no default object named `gbd` found. To learn more about `gbd` and `gbd_dataset`, call `better_vignette(better)`.",

  gbd_doc_dataset = "Path of the GBD folder, where the data are stored. By default, automatically read from options, namely, `better.gbd_path`, ideally set in .Rprofile. To learn more, call `better_vignette(better)`.",

  better_effect_message = "This is a `better_effect` object. For more details, including terms of use, run `?disease`.",
  guesstimate = "This is a guesstimate. Although based on aggregated data, it is no substitute for a rigorous impact analysis when worth the effort.",
  mash = "This calculation makes use of data owned by others."
)
txt <- with(txt, c(
  txt,
  gbd_boiler = paste0(gbd_source," For more details, call `vignette(\"gbd\")`."),
  gbd_cite_source = paste0(gbd_cite," Used with permission. All rights reserved. The `better` package uses the dataset in accordance with the IHME free-of-charge non-commercial user agreement, retrieved from https://www.healthdata.org/Data-tools-practices/data-practices/ihme-free-charge-non-commercial-user-agreement.")
))
# I used to wrap this, but I think it is better to wrap on demand (and in bulk)
# old code:
#txt <- purrr::map(txt, stringr::str_wrap) # after this, will NOT be wrapped
# note: if you don't want wrapped stuff wrapped, unwrap it with something like stringr::str_squish

txt <- c(
  txt,
  gbd_path_dev = "/Users/jwl38/Library/Mobile\ Documents/com~apple~CloudDocs/R\ -\ iCloud/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data in Drive", # big dataset for JW as developper # "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Offline\ Drive/R\ -\ Offline\ Drive/R\ packages\ JW/better/ignore/Global\ Burden\ of\ Disease\ Study\ Data in Drive",
  gbd_path = "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com/My\ Drive/Research\ JW/Data\ JW/Public\ datasets/Global\ Burden\ of\ Disease\ Study\ Data", # local dataset of a user, JW as user #"/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared/IRB Projects/Health Projects/Health Team/Public datasets on health/Global Burden of Disease Study Data",
  gbd_path_show = "/Users/johnwilliam/datasets/Global Burden of Disease data"
)
#file.exists(txt$gbd_path_dev)
#file.exists(txt$gbd_path)

# Developer functions -----------------------------------------------------

dvloptions <- function(path_dev = TRUE) {
  # for development only
  # make all dev functions start with dev
  if (path_dev) {
    options(better.gbd_path = txt$gbd_path_dev)
  } else {
    options(better.gbd_path = txt$gbd_path)
  }
}#; dvl_options(path_dev = FALSE)
dvlload <- function(x = "both") {
  if (x == "start") {
    load(file = system.file("data", "gbd_start.rda", package = "better"))
    gbd <<- gbd_start
  } else {
    if (x == "both") {
      in_cah <- TRUE
      in_better <- TRUE
    } else if (x == "cah") {
      in_cah <- TRUE
      in_better <- FALSE
    } else if (x == "package") {
      in_cah <- FALSE
      in_better <- TRUE
    }
    if (in_cah) {
      gbd2 <<- gbd_read(path = getOption("better.gbd2_path"))
    }
    if (in_better) {
      gbd <<- gbd_read(path = getOption("better.gbd_path"))
    }
  }
  invisible(gbd)
}
dvlsave <- function(x = "both") {
  gbd_cah <- FALSE
  gbd_package <- FALSE
  if (x == "both") {
    gbd_cah <- TRUE
    gbd_package <- TRUE
  } else if (x == "cah") {
    gbd_cah <- TRUE
  } else if (x == "package") {
    gbd_package <- TRUE
  }
  if (gbd_cah) {
    gbd_save(path = txt$gbd_path)
  }
  if (gbd_package) {
    gbd_save(path = txt$gbd_path_dev)
  }
  # gbd_save(path = getOption("better.gbd_path"))
  # gbd_save(path = getOption("better.gbd2_path"))
}
dvl_before_install <- function() {

  # copy html s into inst
  PACKAGE_PATH <- find.package("better")
  ARTICLES <- list.files(
    paste0(PACKAGE_PATH,"/vignettes"),
    full.names = TRUE,
    recursive = TRUE
  )
  ARTICLES <- stringr::str_subset(ARTICLES,".html$")
  ARTICLES <- tibble::tibble(path = ARTICLES)
  ARTICLES$basename <- basename(ARTICLES$path)
  ARTICLES <- dplyr::arrange(ARTICLES, dplyr::desc(nchar(ARTICLES$path)))
  ARTICLES <- dplyr::mutate(ARTICLES, duplicated = duplicated(basename))
  ARTICLES <- ARTICLES[!ARTICLES$duplicated,]
  ARTICLES <- ARTICLES$path
  file.copy(ARTICLES, paste0(PACKAGE_PATH,"/inst/vignettes_htmls"), overwrite = TRUE)
}


# Utils -------------------------------------------------------------------

message_guess <- function(diagnostic = TRUE) {
  msg <- paste0("Important: ",txt$guesstimate)
  if (diagnostic) {
    message(pretty_paragraphs(msg))
    invisible(msg)
  } else {
    return(msg)
  }
}

cite_shorter <- function(x, brackets = FALSE) {
  y <- stringr::str_replace(x," \\(",", ")
  if (brackets) {
    y <- paste0(" (",y)
  } else {
    y <- stringr::str_remove(y,"\\)")
  }
  return(y)
}#; cite_shorter(txt$gbd_cite_short); cite_shorter(txt$gbd_cite_short, brackets = TRUE)
tolower_1 <- function(x) {
  # useful for changing sentences
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}#; paste0("Although a bit cold, ",tolower_1("The soup is great. Thank you."))
toupper_1 <- function(x) {
  # useful for changing sentences
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}; toupper_1("why not")

pretty_nr <- function(..., always_vector = FALSE, digits = NA) {
  # Get a list of pretty numbers.
  # All rounded similarly.
  x_syms <- rlang::enexprs(...)
  #browser()
  x <- unlist(list(...))
  if (!is.na(digits)) {
    x <- round(x, digits = digits)
  }
  #browser()
  if (is.na(digits)) {
    if (any(x[!is.na(x)] < 10)) { # we want at least 3 digits in total
      nsmall <- 2 # at least 2 digits
    } else if (any(x[!is.na(x)] < 100)) {
      nsmall <- 1
    } else {
      nsmall <- 0
    }
  } else {
    nsmall <- digits
  }
  x <- round(x, digits = nsmall)
  y <- format(x, trim = TRUE, scientific = FALSE, big.mark = ",", nsmall = nsmall) # , digits = nsmall # dunno why not works
  names(y) <- names(x)
  #browser()
  if (!length(x) == 1 & !all(purrr::map_lgl(x_syms,is.numeric)) & !always_vector) {
    #browser()
    if (is.null(names(x)) & !all(purrr::map_lgl(x_syms,is.call))) {
      #browser()
      names(y) <- purrr::map_chr(x_syms,rlang::as_string)
    }
    y <- as.list(y)
  }
  return(y)
}#; pretty_nr(a=873.34,b=98.30948,c(c=0.09393,d=.3)) #list(7, c(b = 7.2,7.1), c(c= 10.2,e=10.1), c(90.4,90.5), c(190.4,190.5), 1000000, 92834592, 0.1, 0.000387239487234) %>% map(pretty_nr)


# GBD ---------------------------------------------------------------------


gbd_license <- function(full = TRUE) {
  y <- c(
    txt$gbd_license_explain,
    " Cite the data as follows: ", txt$gbd_cite
  )
  y <- paste(y,collapse="\n")
  cat(pretty_paragraphs(y))
  invisible(y)
}

get_path <- function(path, path_missing = FALSE) {
  # Get the GBD path, deal with all weird situations (no path provided, path doesn't exist, etc).
  # All better functions use this function rather than direct references to a path.

  #browser()
  #path_str <- rlang::as_string(rlang::ensym(path))

  if (path_missing) {
    # rlang::inform("Getting path from `getOption(\"better.gbd_path\")`.", .frequency = "once", .frequency_id = "Getting path from `getOption(\"better.gbd_path\")`.") # Don't delete this
    y <- getOption("better.gbd_path")
    if (length(y) == 0) {
      # message("No path found with `getOption(\"better.gbd_path\")`. If you want to load extra GBD data in addition to the starter dataset, consider adding `options(better.gbd_path = \"path/to/gbd/data\")` to your .Rprofile. See `vignette(better)`.")
      return(NULL)
    } else {
      # rlang::inform(paste0("  Path found:\n",stringr::str_wrap(y, exdent = 2)), .frequency = "once", .frequency_id = "Path found") # don't delete this
    }
  } else {
    rlang::inform(
      "You provided the GBD path explicitly in the `path` argument. Consider adding `options(better.gbd_path = \"path/to/gbd/data\")` to your .Rprofile, and make use of the default path. See `better_vignette(better)`.",
      .frequency = "once", .frequency_id = "You provided the GBD path explicitly in the `path` argument"
    )
    y <- path
  }
  y <- as.character(y)

  if (!file.exists(y)) {
    #browser()
    stop("The path doesn't exist:\n  ",y,ifelse(path_missing,"","\nYou probably want to edit `options(better.gbd_path = \"correct/path/to/gbd/data\")` in your .Rprofile."))
  }

  return(y)
}#; get_path("/user/some/kind/path/bla", path_missing = FALSE)

list_recursive_path_names <- function(x) {
  list.files(path = as.character(x), full.names = TRUE, recursive = TRUE)
  # keep this code as is:
  # as.character is needed because otherwise installing (but nothing else!!) throws an error that list.files has an invalid path argument. It's something weird with list.files: don't try to solve the bug. But as.character works.
}

gbd_data_read <- function(path) {
  # Read the raw GBD data from your local folder. `gbd_data_read()` looks for data files in all subfolders of the GBD folder and merges them into one data frame. Except for merging the data, there's no cleaning involved.
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

    # Option for either both names and IDs or just names
    # Todo: special error if only ID "Please select names only."
    if (any(stringr::str_detect(names(dfi),"measure_id"))) { # If both names and IDs
      names(dfi) <- stringr::str_remove(names(dfi), "_name")
    }

    dfi$file <- i

    #browser()

    y <- dplyr::bind_rows(
      y,
      dfi
    )
  }
  # browser()

  y <- dplyr::select(
    y,
    "cause", "rei", "metric", "val", "measure", "lower", "upper",
    "location", "sex", "age", "year",
    "file"
    # #"measure_descr",
    # tidyselect::everything(),
  )
  # c("cause", "rei", "metric", "val", "measure", "upper", "lower", "measure_descr", "location", "sex", "age", "year", "file", "measure_id", "location_id", "sex_id", "age_id", "cause_id", "rei_id", "metric_id", "measure_with_metric")

  # Todo: make the file column, a list of vectors of ALL the files in which the row was present
  # Trick: sort the list of files from large file to small file, and then the duplicates later on from smaller files will be deleted
  # y$id <- paste...
  # y$duplicated <- duplicated(y["file"])

  # For coding, don't delete:
  # y %>% names %>% PrettyVector(quotes = "")
  y <- dplyr::distinct(
    y,
    dplyr::across(tidyselect::all_of(c(
      "cause", "rei", "metric", "val", "measure", "lower", "upper",
      "location", "sex", "age", "year"
      # OLD: "measure_id", "measure_name", "location_id", "location_name", "sex_id", "sex_name", "age_id", "age_name", "cause_id", "cause_name", "rei_id", "rei_name", "metric_id", "metric_name", "year", "val", "upper", "lower"
      ))),
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

pretty_paragraphs <- function(..., sep="\n\n", width = 80) {
  # Make paragraphs etc pretty: wrapping them; respecting intentional newlines and indentation; adding newlines between paragraphs
  # Assumes all line-starting blanks, including multiple blanks, are intentional (contra stringr::str_wrap).
  x <- unlist(list(...))
  if (sep == " ") { # otherwise str_wrap will be done per item, but then it's space-separator bound, and you have one long sentence in the middle
    x <- paste0(x, collapse = " ")
  }
  y <- c()
  for (i in x) {
    strt <- stringr::str_extract(i, "^[[:blank:]]+") # blanks at the start
    if (is.na(strt)) strt <- ""
    yi <-    stringr::str_remove(i, "^[[:blank:]]+")
    splitters <- unlist(stringr::str_extract_all(yi,"\n+[[:blank:]]*"))
    yi <-        unlist(      stringr::str_split(yi,"\n+[[:blank:]]*"))
    yi <- stringr::str_wrap(yi, width = width)
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
    function(x) ifelse(stringr::str_detect(x,"^[[:upper:]][[[:lower:]]-///]+$"), tolower(x), x)
  )[[1]], collapse=" ")))
}#; clean_string("Age-standardized"); clean_string(c("The HIV/AIDS Issue","DALYs", NA, "Why NOT I don t know Really")); clean_string("Environmental/occupational risks")

gbd_data_clean <- function(x = gbd_data_read()) {
  message("\nStarted cleaning the GBD data. This can take a couple of minutes for large amounts of data (> 100,000 rows)...")
  # Clean, keep it small

  y <- x

  #browser()
  if (anyNA(y$rei[y$val<0])) {
    stop(
      "Issue with the following rows:\n\n",
      paste0(utils::capture.output(print(y[is.na(y$rei) & y$val<0,], N = Inf)), collapse = "\n"),
      "\n\nSome NAs for rei where values are smaller than 0. This should not happen, because the GBD codebook says that negative values are for rei's that are 'protective'. This introduces a bug: Now `better` should also take into account `protective` causes - diseases that are good for people. This doesn't really make sense. It's probably a bug."
    )
  }

  # if (any(stringr::str_detect(names(y),"measure_id"))) { # If both names and IDs
  #   names(y) <- stringr::str_remove(names(y), "_name")
  # }
  # y <- dplyr::select(
  #   y,
  #   "cause", "rei", "metric", "val", "measure", "upper", "lower",
  #   "location", "sex", "age", "year",
  #   "file"
  #   # #"measure_descr",
  #   # tidyselect::everything(),
  # )
  # # c("cause", "rei", "metric", "val", "measure", "upper", "lower", "measure_descr", "location", "sex", "age", "year", "file", "measure_id", "location_id", "sex_id", "age_id", "cause_id", "rei_id", "metric_id", "measure_with_metric")

  #y$measure_descr <- y$measure
  message("Cleaning the measures...")
  y$measure <- clean_measure(y$measure)
  #y <- dplyr::select(y,-dplyr::ends_with("_id"), tidyselect::everything())

  to_clean <- c("cause", "rei", "metric", "sex", "age", "measure")
  message("To be cleaned: ", paste0(to_clean, collapse = ", "),". This can take a while...")
  for (i in to_clean) {
    message("Cleaning strings in ", i, "...")
    y[i] <- clean_string(y[[i]])
  }
  #browser()

  y$estimate <- dplyr::case_when(
    is.na(y$rei) ~ "cause",
    y$rei %in% gbd_codebook()$reis$rei[gbd_codebook()$reis$parent == "etiologies"] ~ "etiology",
    TRUE ~ "rei"
  )

  #y$measure_with_metric <- paste(y$metric, y$measure, sep = " of ")

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

  y$variables$explanation <- NA
  y$variables$explanation[y$variables$variable == "val"] <- "The mean value of an estimate. SOURCE: IHME_GBD_2019_1_DATA_TOOLS_GUIDE_Y2020M10D15.PDF"
  y$variables$explanation[y$variables$variable %in% c("upper", "lower")] <- "A range of values that reflects the certainty of an estimate. In GBD, every estimate is calculated 1,000 times, each time sampling from distributions rather than point estimates for data inputs, data transformations and model choice. The 95th uncertainty interval is determined by the 25th and 975th value of the 1,000 values after ordering them from smallest to largest. Larger uncertainty intervals can result from limited data availability, small studies, and conflicting data, while smaller uncertainty intervals can result from extensive data availability, large studies, and data that are consistent across sources. SOURCE: IHME_GBD_2019_1_DATA_TOOLS_GUIDE_Y2020M10D15.PDF. IMPORTANT NOTE: a 95th uncertainty interval is not the same as a 95% confidence interval. The latter means that, if you sample many times, and each time you calculate the statistic and its confidence interval, then 95% of the intervals should contain the statistic's true value. This doesn't mean that the interval should contain 95% of the calculated statistics for all the samples, which is the approach of GBD's 95th uncertainty interval. Given the unfortunate lack of uniformity in uncertainty interval computation, `better` simply collapses them."

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

  # error in codebook
  y$reis$parent[y$reis$parent == "non-optimal temperature"] <- "suboptimal temperature"

  return(y)
}#; gbd_codebook_clean(gbd_codebook_read())

gbd_meta_read <- function(path) {
  path <- get_path(path, missing(path))

  #browser()

  FILES <- stringr::str_subset(
    list_recursive_path_names(path),
    "/GBD-DATA-INPUT-SOURCES.+\\.csv"
  ) #%>% print
  if (length(FILES) == 0) {
    stop("No GBD input sources metadata files found in ", path, ".")
  }
  y <- tibble::tibble()
  message(
    "  Reading files with readr::read_csv:\n",
    paste(basename(FILES), collapse = ", ")
  )
  #cat("\n")
  for (i in FILES) {
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
  #browser()
  #names(y) %>% PrettyVector()
  # y %>% pull(Location) %>% unique %>% sort
  # ysav <- y
  ynrow <- nrow(y)
  y <- dplyr::distinct(
    y,
    dplyr::across(tidyselect::all_of(c(
      "Citation", "Component", "Subcomponent", "Location", "Cause", "Risk", "Covariate", "Sustainable Development Goal", "Publication Status", "Provider", "Provider URL", "GHDX URL", "Secondary GHDx URL", "Data Collection Method", "Year Start", "Year End", "Sex", "Age Start", "Age End", "Age Type", "Representativeness", "Urbanicity Type", "Population Representativeness Covariates", "Sample Size", "Sample Size Unit", "Standard Error"
    ))),
    .keep_all = TRUE
  )
  message("Deleted ", pretty_nr(ynrow - nrow(y)), " duplicate rows.")
  message(
    "Finished looping through all the GBD metadata files. Read in total: ", pretty_nr(nrow(y)),
    " rows.\n",
    txt$gbd_boiler
  )
  return(y)
}
gbd_meta_clean <- function(x = gbd_meta_read()) {
  y <- x
  #browser()
  names(y) <- snakecase::to_snake_case(names(y))
  #names(y) %>% PrettyVector()
  for (i in c("component", "subcomponent", "cause", "risk", "covariate", "sustainable_development_goal", "publication_status", "data_collection_method", "sex", "representativeness", "urbanicity_type", "population_representativeness_covariates", "sample_size_unit")) {
    message("Cleaning strings in ", i, "...")
    y[i] <- clean_string(y[[i]])
  }
  #x$sample_size %>% class
  return(y)
}#; gbd_meta_clean()




# gbd_rdata_path <- function(path = getOption("better.gbd_path")) {
#   paste0(path,"/gbd.RData")
# }
gbd_rds_path <- function(path = getOption("better.gbd_path")) { # get_path ??
  paste0(path,"/gbd.rds")
}

#' Save Global Burden of Disease dataset
#'
#' Save the GBD dataset as an RDS file. `gbd_save()` looks for the raw GBD data files and codebook files in subfolders of your GBD root folder, as specified in `getOption("better.gbd_path")`. It cleans the data and codebooks, and saves it as a `gbd_dataset` object in your root GBD folder.
#'
#' Several `better` functions look for the GBD dataset in your GBD root folder, specified in `getOption("better.gbd_path")`. If they don't find the proper RDS file, these functions automatically try to call `gbd_save()`. However, you may also want to manually run `gbd_save()`, for instance, after you have downloaded additional data.
#'
#' @param path Path of the GBD folder, where the data are stored. By default, automatically read from options, namely, `better.gbd_path`, ideally set in .Rprofile. To learn more, call `better_vignette(better)`.
#' @param load Whether or not to load the GBD dataset as object `gbd`.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontshow{
#' NULL
#' }
#' \dontrun{
#' gbd_save()
#' }
gbd_save <- function(path, load = TRUE) {
  #browser()
  path <- get_path(path, missing(path))
  gbd_data <- gbd_data_read(path = path)
  gbd_data <- gbd_data_clean(gbd_data)
  gbd_codebook <- gbd_codebook_read(path = path)
  gbd_codebook <- gbd_codebook_clean(gbd_codebook)

  y <- new_gbd(gbd_data, gbd_codebook)
  y_old <- gbd_read(path)

  if (identical( sort(names(y)), sort(names(y_old)) )) {
    y <- dplyr::bind_rows(
      y_old,
      y
    )
    #browser()
  } else {
    warning("The existing .rds data and the new GBD data cleaned have different formats. NOT including the existing .rds data into the output.")
  }

  y_start <- gbd_start
  y_start$file <- gbd_rds_path(path)
  if (identical( sort(names(y)), sort(names(y_start)) )) {
    y <- dplyr::bind_rows(
      y_start,
      y
    )
    #browser()
  } else {
    warning("`gbd_start` and the new GBD data cleaned have different formats. NOT including gbd_start data into the output.")
  }
  y <- dplyr::distinct(y, dplyr::across(-file))
  message("")
  message("Saving GBD data and codebook in:\n  ", stringr::str_wrap(gbd_rds_path(path)))
  saveRDS(y, file = gbd_rds_path(path))
  if (load) {
    gbd_load(path)
  }
  return(y)
}


gbd_read <- function(path, load = FALSE, assign_to) {
  #browser()
  path <- get_path(path, missing(path))
  if (is.null(path)) { #
    message(
      "No GBD data folder found. Taking data from `gbd_start` (starter dataset provided with package)."
    )
    y <- gbd_start
  } else {
    if (!file.exists(gbd_rds_path(path))) {
      message(
        "No RDS file with GBD dataset found in:\n  ", stringr::str_wrap(gbd_rds_path(path)),
        "\n  Saving GBD dataset in that location..."
      )
      gbd_save(path)
    }
    rlang::inform(paste0("Read from:\n  ",stringr::str_wrap(gbd_rds_path(path))), .frequency = "once", .frequency_id = "Reading GBD data with codebook")
    y <- readRDS(gbd_rds_path(path))
  }

  if (load) {
    assign(assign_to, y, envir=globalenv()) # this generates a note in devtools::check(), but it's fine
    #message("Loaded GBD data with codebook, assigned to global variable `",assign_to,"`, and invisibly returning it.")
    invisible(y)
  } else {
    return(y)
  }
}
gbd_load <- function(..., assign_to = "gbd") {
  #path <- get_path(path, missing(path))
  gbd_read(..., load = TRUE, assign_to = assign_to)
}

#' Download Global Burden of Disease data
#'
#' @inheritParams gbd_save
#'
#' @return URLs of webpages opened, invisible.
#' @export
#'
#' @examples
#' \dontshow{
#' NULL
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
#' @inheritParams gbd_save
#'
#' @return The value of `base::system2("open", path)`, which this function wraps.
#' @export
#'
#' @examples
#' \dontshow{
#' NULL
#' }
#' \dontrun{
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

  data_str <- rlang::as_string(rlang::ensym(data)) # you don't do anything with this (yet)
  if (data_missing) {
    # rlang::inform( # don't delete this
    #   "Getting GBD dataset from object `gbd`.",
    #   .frequency = "once",
    #   .frequency_id = "Getting data from object `gbd`."
    # )
    if (exists("gbd", envir = rlang::caller_env())) {
      y <- get("gbd", envir = rlang::caller_env())
      #message("Got data from object named `gbd`.")
    } else {
      #stop("get gbd nrow gbd", get("gbd"))
      rlang::inform(
        "Object `gbd` not found. Loading GBD dataset and assigning it to `gbd`.",
        .frequency = "once",
        .frequency_id = "Object `gbd` not found. Reading GBD data and assigning it to `gbd`."
      )
      y <- gbd_load() # because it's also assigned to gbd
      #stop("Object `gbd` not found. Did you forget to `gbd_load()`? To learn more, call `better_vignette(better)`.")
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
#' @param data GBD dataset, of `gbd_dataset` class. By default, `gbd` is used, if it exists. To learn more, call `better_vignette(better)`.
#'
#' @return GBD codebook, list
#' @export
#'
#' @examples
#' \dontshow{
#' NULL
#' }
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
#' @return Codebook, data frame
#' @export
#'
#' @examples
#' \dontshow{
#' NULL
#' }
#' gbd_variables("metric")
gbd_variables <- function(...) {
  gbd_codebooks("variables", ...)
}#; gbd_measures(DALYs)

#' Get codebook for GBD causes
#'
#' @param ... Which causes you want a codebook for, string.
#'
#' @return Codebook, data frame
#' @export
#'
#' @examples
#' \dontshow{
#' NULL
#' }
#' gbd_causes("diabetes mellitus")
gbd_causes <- function(...) {

  gbd_codebooks("causes", ...)
}

#' Get codebook for GBD measures
#'
#' @param ... Which measures you want a codebook for, string.
#'
#' @return Codebook, data frame
#' @export
#'
#' @examples
#' \dontshow{
#' NULL
#' }
#' gbd_measures("DALYs")
gbd_measures <- function(...) {
  gbd_codebooks("measures", ...)
}


#' Get the GBD dataset
#'
#' You rarely need it. But, since there's [nudge_data()] and [nudge_codebook()], there may as well be [gbd_data()] and [gbd_codebook()].
#'
#' @inheritParams gbd_save
#'
#' @return GBD dataset, `gbd_dataset` class
#' @export
#'
#' @examples
#' gbd_data()
gbd_data <- function(path) {
  path <- get_path(path, missing(path))
  if (exists("gbd", envir = rlang::caller_env())) {
    gbd <- get("gbd", envir = rlang::caller_env())
  }
  data <- NA
  data <- get_gbd(data, data_missing = TRUE)
  return(data)
}

#' Get codebook for GBD reis
#'
#' @param ... Which reis you want a codebook for, string.
#'
#' @return Codebook, data frame
#' @export
#'
#' @examples
#' \dontshow{
#' NULL
#' }
#' gbd_reis("behavioral risks")
gbd_reis <- function(...) {
  gbd_codebooks("reis", ...)
}

pars_all <- c(
  "cause", "measure", "metric", "rei",
  "location", "sex", "age", "year"
)
gbd_check <- function(data, params) {
  # check which not yet present
  ideal <- expand.grid()

  gbd_params(gbd_filter(), location = "Spain")
}#; gbd_check(gbd, list())

gbd_arrange <- function(x) {
  y <- x
  for (i in rev(c(
    "cause",
    "rei",
    "measure", "metric", "location", "sex", "age", "year"
  ))) {
    #if (i=="rei") browser()
    if ( length(unique(y[[i]]))>1 ) {
      ordi <- factor(y[[i]],levels=sort(unique(y[[i]])))
      y <- dplyr::arrange( y, ordi )
    }
  }
  return(y)
}

default <- list(
  cause = "all causes",
  measure = "DALYs",
  metric = "rate",
  rei = NA,
  location = "Global",
  sex = "both",
  age = "age-standardized",
  year = NA,
  calculate = "disease"
)
#options(better.metric = "number")
get_default <- function(x) {
  optx <- getOption(paste0("better.",x))
  y <- ifelse(
    is.null(optx), default[[x]],
    optx
  )
  return(y)
}#; get_default("metric")
# cause = ifelse(is.null(getOption("better.cause")),"all causes",getOption("better.cause")),
# measure = ifelse(is.null(getOption("better.measure")),"DALYs",getOption("better.measure")),
# metric = ifelse(is.null(getOption("better.metric")),"rate",getOption("better.metric")),
# rei = ifelse(is.null(getOption("better.rei")),NA,getOption("better.rei")), #"Behavioral risks", #NA,#"Behavioral risks",
# location = ifelse(is.null(getOption("better.location")),"Global",getOption("better.location")), # locations are with capital # "United States of America"
# sex = ifelse(is.null(getOption("better.sex")),"both",getOption("better.sex")), age = "age-standardized", # "all ages"

#' Subset the Global Burden of Disease dataset
#'
#' Subset the Global Burden of Disease dataset with sensible defaults. Note that `argument = NA` means filtering out `NA`s, while `argument = NULL` means not filtering out anything (keeping all). `gbd_filter` is all about the sensible defaults. If you need many arguments to be `NULL`, consider using `dplyr::filter(gbd, ...)` instead.
#'
#' # Terms of use
#'
#' The GBD dataset was created and is maintained by the [Institute for Health Metrics and Evaluation](https://www.healthdata.org/) at the University of Washington. The dataset is owned by the University of Washington, which reserves all rights to it.
#'
#' Cite the data as follows:
#'
#' Global Burden of Disease Collaborative Network. *Global Burden of Disease Study 2019 (GBD 2019) Results.* Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. Available from [https://vizhub.healthdata.org/gbd-results/](https://vizhub.healthdata.org/gbd-results/).
#'
#' The `better` package uses the dataset in accordance with the [IHME free-of-charge non-commercial user agreement]( https://www.healthdata.org/Data-tools-practices/data-practices/ihme-free-charge-non-commercial-user-agreement). The authors and owners of the GBD data were not involved in the creation of `better`, and bear no responsibility for any possible issues with the representation and/or use of the data.
#'
#' @inheritParams gbd_codebook
#' @param cause Cause(s) of death/disability
#' @param measure Measure of returned values
#' @param metric Metric for returned values
#' @param rei Risk factor for the disease, impairment, or etiology
#' @param location Location for the data
#' @param sex Sex of the population
#' @param age Age of the population
#' @param year Year of data collection
#' @param params List of parameters. Rather than feeding the parameters as arguments, like for instance gbd_filter(cause = "all causes", measure = "DALYs"), you can feed the parameters as a list, like for instance gbd_filter(params = list(cause = "all causes", measure = "DALYs"))
#'
#' @return GBD data for given parameters, as data frame.
#' @export
#'
#' @examples
#' \dontshow{
#' NULL
#' }
#'
#' gbd_filter(cause = "diabetes mellitus")
gbd_filter <- function(
  cause = get_default("cause"),
  measure = get_default("measure"),
  metric = get_default("metric"),
  rei = get_default("rei"), #"Behavioral risks", #NA,#"Behavioral risks",
  location = get_default("location"), # locations are with capital # "United States of America"
  sex = get_default("sex"),
  age = get_default("age"), # "all ages"
  year = get_default("year"), #ifelse(is.null(getOption("better.year")),max(data$year),getOption("better.year")),
  data = NA,
  params = list()
) {

  #browser()

  argg <- as.list(environment()) # c(,.list(...))

  # Arguments should override same-name parameters in params
  # So omit from params the ones that are in nonmissing argums
  argums <- names(formals())
  #browser()
  envf <- environment()
  notmiss <- purrr::map_lgl(argums,~{
    #browser()
    !do.call(missing, list(x = .x), envir = envf)
    }) #%>% print
  params <- params[!names(params) %in% argums[notmiss]]

  if ( any( is.na(names(params)) | length(names(params))==0 ) ) {
    #browser()
    stop("Names issue:\n\n", utils::capture.output(params))
  }

  list2env(params,environment())

  # if (exists("gbd", envir = rlang::caller_env())) { # not sure if needed
  #   gbd <- get("gbd", envir = rlang::caller_env())
  # }
  data <- get_gbd(data, all(is.na(data))) # missing(data)
  if (all(is.na(year))) {
    year <- ifelse(
      is.null(getOption("better.year")), max(data$year),
      getOption("better.year")
    )
  }

  #browser()
  if (any(measure %in% c("prevalence"))) {
    rei <- unique(c(NA,rei)) # used to just be NA
  }
  if (any(measure %in% c("summary exposure value"))) {
    cause <- unique(c(NA,cause)) # used to just be NA
    if (all(is.na(rei))) {
      rei <- "all risk factors" # summary exposure value must have SOME risk factor
    }
  }

  #browser()
  year <- year # needed (maybe force?)

  #browser()

  y <- data

  notfound <- list()
  for (i in c(
    "cause", "measure", "metric", "rei",
    "location", "sex", "age", "year"
  )) {
    if (!is.null(get(i))) { # used to be !all(is.na(get(i))) before semantic change to NULL meaning don't filter
      geti <- get(i)
      #geticlean <- geti[!is.na(geti)]
      for (j in geti) {
        if (length(!j %in% y[[i]]) > 1) {
          stop("length(!j %in% y[[i]]) > 1\nlength(j) = ",length(j),"\nlength(y[[i]]) = ", length(y[[i]]))
        }
        #if (i == "location") browser()
        if (!j %in% y[[i]]) {
          notfound[[i]] <- c(notfound[[i]],j)
          #browser()
        }
      }
      y <- y[y[[i]] %in% geti,] # subsetting y - MOST IMPORTANT LINE
      #x[[i]]
    }
    #browser()
  }
  #browser()
  if (length(notfound) >= 1) {
    wrn <- "No data for the following parameters:"
    for (i in names(notfound)) {
      wrn <- paste0(wrn,"\n- ",i,":")
      for (j in notfound[[i]]) {
        wrn <- paste0(wrn,"\n  - ",j)
      }
    }
    wrn <- paste0(
      "\nThese are all the parameters entered:\n",
      paste0(utils::capture.output(argg),collapse="\n")
    )
    #browser()
    warning(wrn)
  }

  #browser()

  # Order in order of values entered for parameters:
  for (i in rev(c(
    "cause",
    "rei",
    "measure", "metric", "location", "sex", "age", "year"
  ))) {
    #if (i=="rei") browser()
    if ( length(unique(y[[i]]))>1 ) {
      ordi <- factor(y[[i]],levels=get(i)) # order of values entered
      y <- dplyr::arrange( y, ordi )
    }
  }

  if (nrow(y) == 0) {
    warning("No data found for the given parameters. Typo? Or should you download the data?")
  }

  return(y)
}#; gbd_filter(params = parbig)#; gbd_filter(data=gbd_filter(params=list(cause = "diabetes and kidney diseases", metric = "percent")))

gbd_subset <- function(...) {
  # browser()
  xlist <- list(...)
  if (length(xlist) == 0) {
    y <- gbd_filter()
  } else if (is.data.frame(xlist[[1]])) { # subset of GBD dataframe
    y <- xlist[[1]]
  } else if (is.list(xlist[[1]])) { # list of parameters
    y <- gbd_filter(params = xlist[[1]])
  } else { # comma-separated arguments
    y <- gbd_filter(...)
  }
  return(y)
}


#' Filter Global Burden of Disease data on behavioral risks
#'
#' Wrapper around `gbd_filter`
#'
#' @inheritParams gbd_codebook
#' @param ... Parameters for `gbd_filter`
#' @param rei Risk factor for the disease, impairment, or etiology
#'
#' @return GBD data for given parameters, as data frame.
#' @export
#'
#' @examples
#' \dontshow{
#' NULL
#' }
#'
#' gbd_behavior(cause = "diabetes mellitus")
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
}#; gbd_summary(cause = "diabetes mellitus", rei = "behavioral risks")


#' Describe gbd_dataset object
#'
#' @param ... Parameters determining what GBD data you want to describe. Arguments passed on to `gbd_filter`.
#' @param summary Whether or not to provide a brief summary instead of a description, logical
#' @param explain Whether or not to add an explanation of the measure, logical
#' @param reference Whether or not to add a reference, logical
#'
#' @return Description as string
#' @export
#'
#' @examples
#' gbd_describe(cause = "diabetes mellitus")
gbd_describe <- function(..., explain = TRUE, summary = FALSE, reference = FALSE) {
  #browser()
  sct <- gbd_subset(...)
  # feed it either cause or rei
  if (is.na(sct$rei)) {
    y <- gbd_describe_cause(sct, explain = explain, summary = summary, reference = reference)
  } else if (sct$measure == "summary exposure value") {
    y <- gbd_describe_sev(sct, explain = explain, summary = summary, reference = reference)
  } else {
    y <- gbd_describe_rei(sct, explain = explain, summary = summary, reference = reference)
  }
  #browser()
  cat(pretty_paragraphs(y))
  invisible(y)
}#; gbd_describe(cause = "diabetes mellitus", rei = "behavioral risks")
gbd_describe_mold <- function(
    x,
    estimated_what,
    summary,
    explain,
    reference
) {
  if (summary) {
    y <- paste0(
      "Estimated ", estimated_what, ":\n  ",
      pretty_gbd_interval_long(x$val, x$lower, x$upper, measure = x$measure, metric = x$metric),
      "\nParameters: year = ", x$year, ", location = ", x$location,
      ", sex = ", x$sex, ", age = ", x$age
    )
  } else {
    y <- paste0(
      "In ", x$year, ", ", pretty_location(x$location),
      ", the estimated ", estimated_what,
      ", for ", pretty_sex(x$sex),
      " of ", pretty_age(x$age),
      ", was ", pretty_gbd_interval(x),ifelse(reference,cite_shorter(txt$gbd_cite_short,brackets = TRUE),""),".",
      ifelse(explain,paste0(" ", explain_measure(x$measure)),"")
    ) #%>% cat
  }

  #browser()
  return(y)
}

pretty_location <- function(x, add_in = TRUE) {
  y <- dplyr::case_when(
    x == "United States of America" ~ "the USA",
    TRUE ~ x
  )
  # Socio-demographic Index SDI
  if (add_in) {
    y <- ifelse(y == "Global", "globally", paste("in",y))
  } else {
    y <- ifelse(y == "Global", "the world", y)
  }
  return(y)
}#; pretty_location(c("Global","United States of America"), add_in = FALSE); pretty_location(c("Global","United States of America"))

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
}#; gbd_describe_cause(gbd_filter("diabetes mellitus"))
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
}#; gbd_describe(cause = "diabetes mellitus", rei = "behavioral risks")
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
    cbi <- cb0 # to avoid crashing when picking 1
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
browse_var <- function(x, var, subvar=NA) {
  #browser()
  y <- browse_recursion(
    data = x,
    codebook = attr(x,"codebook")[[paste0(var,"s")]], # ?? ifelse(var == "rei", "risk", var)
    parent = ifelse(var == "rei", subvar, "all causes"),
    var = var
    )
  message("\nReturning last category chosen...")
  return(y)
}


#' Browse the full Global Burden of Disease codebook
#'
#' Browse the GBD codebook: the available causes, or the available risk, etiologies and impairments. Since not all the GBD data is available (because it's huge), you can browse what is available on the GBD website for downloading.
#'
#' @inheritParams gbd_codebook
#'
#' @return Last category browsed, as string.
#' @export
#'
#' @examples
#' \dontrun{
#' gbd_browse()
#' gbd_download()
#' }
gbd_browse <- function(data) {
  if (exists("gbd", envir = rlang::caller_env())) {
    gbd <- get("gbd", envir = rlang::caller_env())
  }
  data <- get_gbd(data, missing(data))

  inp <- readline("Hit enter to browse causes, r to browse risks, e for etiologies, and i for impairments.")
  if (inp == "") {
    browse_var(data,"cause")
  } else if (inp == "r") {
    browse_var(data,"rei",subvar="all risk factors")
  } else if (inp == "e") {
    browse_var(data,"rei",subvar="etiologies")
  } else if (inp == "i") {
    browse_var(data,"rei",subvar="impairments")
  } else {
    gbd_browse(data)
  }
}

new_gbd <- function(x = tibble::tibble(), codebook = list()) {
  stopifnot(tibble::is_tibble(x))
  stopifnot(is.list(codebook))
  structure(x,
            class = c("gbd_dataset", "tbl_df", "tbl", "data.frame"),
            codebook = codebook,
            ownership = quiet(gbd_license())
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

#' @export
print.gbd_dataset <- function(x, ...) {
  message(pretty_paragraphs(
    "This is a `gbd_dataset` object. For more details, including terms of use, run `?gbd_filter`.",
    #txt$tou,
    sep = " "
  ))
  # rlang::inform(
  #   paste0(
  #     stringr::str_wrap(paste0(
  #       "It's a data frame with \"codebook\" attribute containing metadata: ",
  #       paste(names(attr(x, "codebook")), collapse = ", "),
  #       ". Use `gbd_codebook()` to retrieve it."
  #     )),
  #     "\n  ",
  #     txt$gbd_boiler
  #   ),
  #   .frequency = "once",
  #   .frequency_id = "It's a data frame with \"codebook\" attribute containing metadata"
  # )
  #browser()
  # class(gbd) <- class(gbd)[!class(gbd) %in% "gbd_dataset"] # not good
  NextMethod(...)
  rlang::inform(
    "Source: Institute for Health Metrics and Evaluation. Used with permission. All rights reserved."
  )
  invisible(x)
}


# Effects -----------------------------------------------------------------

new_effect <- function(
  value,
  lower = NA_real_,
  upper = NA_real_,
  stat = NA,
  unit = NA,
  describe = NA,
  explain = NA,
  facts = NA,
  reference = NA,
  source = NA,
  terms = NA,
  call = NA,
  params = list()
) {
  stopifnot(is.numeric(value))
  stopifnot(is.na(lower) | is.numeric(lower))
  stopifnot(is.na(upper) | is.numeric(upper))
  y <- list(
    value = value,
    lower = lower,
    upper = upper,
    stat = stat,
    unit = unit,
    describe = describe,
    facts = facts,
    explain = explain,
    reference = reference,
    source = source,
    terms = terms,
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
  invisible(force(suppressMessages(x)))
}

pretty_interval <- function(val, low, high, unit = "", sep = " ", spec = ", 95% CI", pct_100 = TRUE, ...) {
  # pct_100 whether or not to multiply pct by 100
  suppressWarnings({
    val <- as.numeric(val)
    low <- as.numeric(low)
    high <- as.numeric(high)
    if (unit %in% c("","%","$")) sep <- ""
    if (unit %in% c("%","percent") & pct_100) {
      #browser()
      val <- 100*val
      low <- 100*low
      high <- 100*high
    }
    if (!is.character(val)) {
      nrs <- pretty_nr(val,low,high, ...)
    } else {
      nrs <- c(val, low, high)
    }
    if (any(stringr::str_detect(nrs,","))) {
      spec <- stringr::str_replace_all(spec,",",";")
      comma <- "; "
    } else {
      comma <- ", "
    }
    if (unit %in% "$") {
      #browser()
      nr_unit <- paste0(unit,sep,nrs[1])
      if (stringr::str_detect(nr_unit,"-")) { # turn "$-72" into "-$72"
        nr_unit <- paste0("-",stringr::str_remove(nr_unit,"-"))
      }
    } else {
      nr_unit <- paste0(nrs[1],sep,unit)
    }
    y <- paste0(nr_unit,spec," [",nrs[2],comma,nrs[3],"]", collapse = "")
  })
  return(y)
}#; pretty_interval(837.9283745, 398.83, 0.0038, "days"); pretty_interval(837.9283745, 398.83, 0.0038); pretty_interval(-2,-3,-1,unit="$")

omit_summary <- function(x) {
  # Omit the summary sentence (first paragraph) from an explanation
  #browser()
  y <- unlist(strsplit(x,"\n\n"))
  y <- y[-1]
  y <- paste(y, collapse = "\n\n")
  return(y)
}

#' Print the effect only
#'
#' @param x `better_effect` object to be printed.
#' @return The inputted `better_effect` object, silently.
#'
#' @export
#' @examples
#' \dontshow{
#' library(tidyverse)
#' NULL
#' }
#' nudge("information", behavior = "food") %>%
#'   effect_only
effect_only <- function(x) {
  print.better_effect(x, effect_only = TRUE)
}

#

#' Get Terms of Use
#'
#' Get the Terms of Use for an object.
#'
#' @param x Object to get terms of use on
#' @param ... Passed on to class-specific methods.
#'
#' @return Original object, invisible.
#' @export
#'
#' @examples
#' terms_of_use(nudge_data())
terms_of_use <- function(x, ...) {
  UseMethod("terms_of_use")
}

#' @export
terms_of_use.gbd_dataset <- function(x, ...) {
  args <- list(...)
  #NextMethod()
  #browser()
  cat(pretty_paragraphs(txt$gbd_boiler))
  invisible(x)
}

#' @export
terms_of_use.better_effect <- function(x, ...) {
  args <- list(...)
  #NextMethod()
  #browser()
  tocat <- paste0(
    # "Learn more about better_effect objects and related functions in the better vignette. Call `better_vignette(better)`.\n  Important: ",
    disclaimer_calculation(x), txt$bugs,
    )
  cat(pretty_paragraphs(tocat))
  invisible(x)
}

#' @export
terms_of_use.default <- function(x, ...) {
  args <- list(...)
  dtls <- attr(x, "terms")
  if (length(dtls)==0) {
    msg <- "No terms_of_use methods or terms attributes found."
  } else {
    msg <- dtls
  }
  #browser()
  cat(pretty_paragraphs(msg))
  invisible(x)
}

message_details <- function(x, special_class = TRUE) {
  if (special_class) {
    what <- paste0("a `",x,"` object")
  } else {
    what <- x
  }
  what <-
  y <- pretty_paragraphs(paste0(
    "This is ", what,
    ". For terms of use, call `terms_of_use(.)`."
  ))
  return(y)
}#; message_details("gbd_dataset")

#' @export
print.better_effect <- function(x, effect_only = FALSE) {
  # Contrary to print, I don't allow .... NOT: argums <- list(...)
  # This generates a "checking S3 generic/method consistency" warning, but so be it
  #browser()
  pretty_effect <- pretty_interval(x$value,x$lower,x$upper, unit = x$unit)
  if (effect_only) {
    msg <- pretty_effect
  } else {
    #browser()
    msg <- paste0(
      #if (length(x$explain) > 1) "\n\n", # to leave spaces between printed stuff,
      # e.g., effectize_1 %>% print %>% effectize_2
      "  Effect:\n\n",
      pretty_effect,
      "\n\nSummary: ", utils::tail(x$describe,1),
      # "Call:   ", stringr::str_wrap(x$call, exdent = nchar("Param.: ")),
      # "\n",
      ifelse(
        length(x$params)>0,
        paste0("\n\n  Parameters:\n\n",stringr::str_wrap(
          paste( paste0(names(x$params)," = ",x$params), collapse=", "),
          exdent = 0
        )),
        ""
      ),
      # "\n\n",
      # "in ",ifelse(is.na(x$unit),"",x$unit),
      "\n\n  Explanation:\n\n",
      quiet(explanation(x, full = FALSE, truncated = TRUE)), # used to have: omit_summary
      # ifelse(
      #   is.na(utils::tail(x$terms,1)), "",
      #   paste0("\n\n  Terms:\n\n", utils::tail(x$terms,1))
      # )
      paste0(
        "\n\n  Source",
        ifelse(length(unique(x$source))>1,"s:\n\n- ",":\n\n"),
        paste0(clean_source(unique(x$source[!is.na(x$source)])), collapse = "\n- ")
      )
    )
  }
  if (length(msg)>1) {
    stop("length(msg)>1")
  }
  #browser()
  #message_guess()
  #browser()
  #message(pretty_paragraphs(disclaimer_calculation(x)))

  message(txt$better_effect_message) # message_details("better_effect")
  # rlang::inform(
  #   pretty_paragraphs(paste0("Important: ",disclaimer_calculation(x))),
  #   .frequency = "once", .frequency_id = paste0("This is a `better_effect` object",utils::tail(x$source,1))
  # )

  cat(pretty_paragraphs(msg))

  # rlang::inform( # don't delete
  #   paste0(
  #     "\n\n* Call `str(.)` to see the structure of this better_effect.",
  #     "\n** Call `explanation(.)` to get the full explanation."
  #   ),
  #   .frequency = "once", .frequency_id = "This is a `better_effect` object"
  # )
  invisible(x)
}

#' Get explanation of `better_effect` object
#'
#' @param x `better_effect` object
#' @param full Get full explanation of all steps or only last step
#' @param truncated Whether or not to truncate long explanations
#'
#' @return Explanation as string
#' @export
#'
#' @examples
#' \dontshow{
#' NULL
#' library(tidyverse)
#' }
#'
#' nudge_to_behavior_d(nudge = "information", behavior = "health") %>%
#'   behavior_d_to_pct %>%
#'   explanation
explanation <- function(x, full = TRUE, truncated = FALSE) {

  #browser()

  expl <- x$explain
  refs <- x$reference[!is.na(x$reference)]
  if (length(refs) > 0) {
    expl <- paste(expl, refs, sep = "\n\nSee:\n")
  }

  if (full & length(expl) > 1) {
    expl <- purrr::map_chr(1:length(expl),~{
      paste0("Step ", .x, ":\n\n", paste0("Estimate: ", x$describe[.x]), "\n\n", expl[.x])
    })
    expl <- paste(expl, collapse = "\n\n")

  } else {
    expl <- utils::tail(expl, 1)
  }

  if (truncated) {
    expln <- 300
    if (nchar(expl) > expln*3) {
      #browser()
      y <- paste0(substr(expl,1,expln), "... [TRUNCATED]\n...",
                  substr(expl,nchar(expl)-expln,nchar(expl)) )
    } else {
      y <- expl
    }
  } else {
    y <- expl
  }
  cat(pretty_paragraphs(y))
  invisible(y) # explanation
}
# nudge_to_behavior_d(nudge = "information", behavior = "health") %>%
#   explanation(truncated = TRUE)
# nudge_to_behavior_d(nudge = "information", behavior = "health") %>%
#   explanation(truncated = FALSE)


#' Get data on the effect sizes of nudges
#'
#' This data is used by other `better` functions.
#'
#' # Source and citation
#'
#' The data comes from the following article:
#'
#' `r txt$nudge_cite`
#'
#' When using this data, cite it appropriately.
#'
#' @return Data on the effect sizes of nudges
#' @export
#'
#' @examples
#' nudge_data()
nudge_data <- function() {

  # TODO: other nonhealth behaviors??

  ds <- dta$mertens

  rlang::inform(paste0("Citation: ", txt$nudge_cite), .frequency = "once", .frequency_id = "Citation: Mertens, Herberz, Hahnel & Brosch (2022)")
  print(ds)
  rlang::inform("Source: Mertens, Herberz, Hahnel & Brosch (2022)")
  invisible(ds)
}

dta <- list() # all the better data used by functions
dta$mertens <- tibble::tribble(
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
dta$mertens <- replace(dta$mertens, is.na(dta$mertens), "all")
attr(dta$mertens,"codebook") <- list(
  nudges = tibble::tribble(
    ~nudge, ~explain,
    "information", "intervention that focuses on the description of alternatives (decision information), by increasing the availability, comprehensibility, and/or personal relevance of information (e.g., visibility of information, social normative information)",
    "structure", "intervention that targets the organization and structure of choice alternatives (decision structure), by altering the utility of choice options through their arrangement in the decision environment or the format of decision making (e.g., defaults, incentives)",
    "assistance", "intervention that reinforces behavioral intentions (decision assistance), by facilitating self-regulation (e.g., reminders, commitment)",
  ),
  behaviors = tibble::tribble(
    ~behavior,
    "health",
    "food",
    "environment",
    "finance",
    "pro-social",
    "other"
  )
)
attr(dta$mertens,"ownership") <- txt$nudge_cite


#' Get the codebook for the nudge data
#'
#' @param x Nudge dataset, by default automatically read.
#'
#' @return Codebook, data frame.
#' @export
#'
#' @examples
#' \dontshow{
#' NULL
#' }
#' nudge_codebook()
nudge_codebook <- function(x = quiet(nudge_data())) {
  attr(x,"codebook")
}

pretty_call <- function(call, defaults) {
  defs <- defaults[!names(defaults) %in% names(call)] #%>% print
  defs <- purrr::map_chr(defs,~paste0(.x,collapse=", "))
  defs <- defs[defs != ""]
  #browser()
  y <- paste0(
    stringr::str_squish(paste(utils::capture.output(call), collapse = "")),
    ifelse(
      length(defs) > 0,
      paste0(", with default arguments: ", paste0(names(defs)," = ",defs,collapse=", ")),
      ""
    )
  )# %>% print
  #browser()
  return(y)
}#; pretty_call(call, defaults)

pretty_nudge <- function(behavior, nudge) {
  paste0(
    "nudge category \"", nudge,
    "\" and behavior category \"", behavior, "\""
  )
}


#' Given a nudge, get its effect on behavior
#'
#' Given a certain kind of nudge, get its effect on a certain kind of behavior
#'
#' `r disclaimer_calculation(new_effect(value = 1, source = txt$nudge_cite))`
#'
#' @param behavior Category of behavior: "health", "food", or "all" (any kind of behavior). By default "health". Call `nudge_codebook()` for more details.
#' @param nudge Category of nudge: "information", "structure", "assistance", or "all" (any kind of nudge). By default "all". Call `nudge_codebook()` for more details.
#'
#' @return Effect size, object of the `better_effect` class
#' @export
#'
#' @examples
#' \dontshow{
#' NULL
#' library(tidyverse)
#' }
#' nudge("information")
#' nudge("information", behavior = "food") %>%
#'   disease("diabetes mellitus")
nudge <- function(nudge = "all", behavior = "health") {
  nudge_to_behavior_d(nudge = nudge, behavior = behavior)
}

clean_source <- function(x) {
  # y <- stringr::str_remove_all(x, "Source: ")
  # y <- stringr::str_remove_all(y, "\\.$")
  x
}

disclaimer_calculation <- function(x, simple = FALSE) {
  y <- paste0(
    ifelse(
      simple, "This function",
      "A better_effect is a guesstimate rather than a rigorous impact analysis. Use it with caution. Check out the full calculation with `explanation(.)`.\n  The calculation"
    ),
    " uses data from the following source",
    ifelse(length(x$source)>1,"s:\n- ",":\n  "),
    paste0(clean_source(x$source), collapse = "\n- "),
    ".\n  The authors and owners of these data retain all rights to their data. Also, they were not involved in the creation of `better`, and bear no responsibility for any possible issues with the representation and/or use of the data."
  )
  return(y)
}#; disclaimer_calculation

#' @describeIn nudge More explicit form with `behavior = "all"` as default
#' @export
#' @examples
#' \dontshow{
#' NULL
#' library(tidyverse)
#' }
#'
#' # The more explicit form allows you to write the following...
#' nudge(nudge = "information") %>%
#'   disease(cause = "diabetes mellitus")
#' # ...as this here...
#' nudge_to_behavior_d(nudge = "information", behavior = "health") %>%
#'   behavior_d_to_pct %>%
#'   behavior_pct_to_disease(cause = "diabetes mellitus")
#'
nudge_to_behavior_d <- function(nudge = "all", behavior = "all") {

  fcall <- pretty_call(sys.call(), formals())

  #browser()

  #rlang::inform("Looking up data on nudges...", .frequency = "once", .frequency_id = "Looking up data on nudges...") # don't delete

  #message("Looking up data on the effect of nudges...")

  ds <- suppressMessages(quiet(nudge_data()))

  dsi <- ds[ds$behavior %in% behavior & ds$nudge %in% nudge,]

  #browser()

  # reason:
  stati <- paste0("Cohen's d for ",pretty_nudge(behavior=dsi$behavior,nudge=dsi$nudge))
  rsn <- paste0(
    #paste0(stati, " is ",pretty_interval(dsi$d, dsi$low, dsi$high),"."),
    "This statistic comes from a meta-analysis on the effectiveness of nudging. This is the abstract of the original paper: \"Over the past decade, choice architecture interventions or socalled nudges have received widespread attention from both researchers and policy makers. Built on insights from the behavioral sciences, this class of behavioral interventions focuses on the design of choice environments that facilitate personally and socially desirable decisions without restricting people in their freedom of choice. Drawing on more than 200 studies reporting over 440 effect sizes (n = 2,148,439), we present a comprehensive analysis of the effectiveness of choice architecture interventions across techniques, behavioral domains, and contextual study characteristics. Our results show that choice architecture interventions overall promote behavior change with a small to medium effect size of Cohen's d = 0.43 (95% CI [0.38, 0.48]). In addition, we find that the effectiveness of choice architecture interventions varies significantly as a function of technique and domain. Across behavioral domains, interventions that target the organization and structure of choice alternatives (decision structure) consistently outperform interventions that focus on the description of alternatives (decision information) or the reinforcement of behavioral intentions (decision assistance). Food choices are particularly responsive to choice architecture interventions, with effect sizes up to 2.5 times larger than those in other behavioral domains. Overall, choice architecture interventions affect behavior relatively independently of contextual study characteristics such as the geographical location or the target population of the intervention. Our analysis further reveals a moderate publication bias toward positive results in the literature. We end with a discussion of the implications of our findings for theory and behaviorally informed policy making.\""
    #paste0("Citation: ", txt$nudge_cite)
  )#; cat(rsn)
  #message(rsn)

  #print_estimate(y)
  #browser()

  locopt <- getOption("better.location")
  if (length(locopt)==0) {
    locopt <- "Global" # the cohen d s are from studies all over the world (since one of the variables they have is inside vs outside the US)
  } else if (locopt == "Global") {
    # great
  } else {
    rlang::inform(
      paste0("Option sets location to ",locopt,", while Cohen d's are from studies all over the world",cite_shorter(txt$nudge_cite_short,brackets=TRUE),"."),
      .frequency = "once", .frequency_id = "Option sets location to"
    )
  }

  #browser()
  if (behavior == "food") {
    reii <- "dietary risks"
    msgend <- ", since `behavior` equals \"food\"."
  } else {
    reii <- "behavioral risks"
    msgend <- " (default)."
  }
  rlang::inform(
    paste0("nudge() set `rei` parameter to \"",reii,"\"",msgend),
    .frequency = "once", .frequency_id = "nudge() set `rei` parameter to"
  )

  new_effect(
    value = dsi$d,
    lower = dsi$low,
    upper = dsi$high,
    stat = stati,
    unit = "standard deviations",
    describe = paste0(stati, " is ",pretty_interval(dsi$d, dsi$low, dsi$high),"."),
    explain = rsn,
    facts = list(paste0(stati, " is ",pretty_interval(dsi$d, dsi$low, dsi$high)," (",stringr::str_remove(txt$nudge_cite_short,"\\("),".")),
    reference = txt$nudge_cite,
    source = paste0(txt$nudge_cite_short,". For details, run `?nudge_data`."),
    terms = NA,
    call = fcall,
    params = list(
      rei = reii,
      location = locopt,#sex = "both", age = "all ages",
      behavior = behavior, nudge = nudge
    )
  )
}#; nudge_to_behavior_d(behavior = "health") #; behavior_impact()

behavior <- function(behavior = "all", nudge = "all") {
  nudge_to_behavior_d(nudge = nudge, behavior = behavior)
}

gbd_params <- function(x, ..., add = TRUE) {
  # for a gbd_dataset object, get it's parameters
  # you can manually edit parameters
  #x <- sev
  stopifnot(nrow(x)==1)
  y <- list(
    cause = x$cause,
    rei = x$rei,
    metric = x$metric,
    measure = x$measure,
    location = x$location,
    sex = x$sex,
    age = x$age,
    year = x$year
  )
  args <- list(...)
  if (length(args) > 0 ){
    for (i in names(args)) {
      if (add) {
        y[[i]] <- unique(c(y[[i]],args[[i]]))
      } else {
        y[[i]] <- args[[i]]
      }
    }
  }
  return(y)
}
params_gbd <- function(x) {
  # for certain GBD paramers, get the GBD dataset
  # better use gbd_filter(params = x)
  gbd_filter(params = x)
}

sd_prop <- function(x) {
  # standard deviation of proportion
  sqrt(x*(1-x))
}

#' Given a behavior's Cohen's d, get its percentage difference
#'
#' Given a behavior's Cohen's d as better_effect, get its percentage difference as better_effect. The primary use of this function is as an intermediate step between `nudge_to_behavior_d()` and `behavior_pct_to_disease()`. It is implicitly used by `disease()`.
#'
#' `r disclaimer_calculation(new_effect(value = 1, source = txt$gbd_source))`
#'
#' @inheritParams gbd_codebook
#' @param effect A `better_effect` object containing the Cohen's d.
#' @param ... Parameters to be passed on to `gbd_filter`.
#'
#' @return A `better_effect` object containing the percentage difference.
#' @export
#'
#' @examples
#' \dontshow{
#' NULL
#' library(tidyverse)
#' }
#'
#' nudge_to_behavior_d("information") %>%
#'   behavior_d_to_pct
behavior_d_to_pct <- function(effect, ..., data) {

  force(effect)
  #browser()
  if (length(effect$params$behavior) != 0 && !effect$params$behavior %in% c("health","food")) {
    #browser()
    warning(
      "The effect has a `behavior` parameter other than \"health\" or \"food\", namely: \"",
      effect$params$behavior,
      "\". Are you sure you don't want a health-related effect fed into `behavior_d_to_pct()`, since it uses health data?"
    )
  }

  fcall <- pretty_call(sys.call(), formals())

  #browser()
  if (effect$unit != "standard deviations") {
    stop("behavior_d_to_pct requires a Cohen's d as input, that is, effect$unit is \"standard deviations\".")
  }

  # if (exists("gbd", envir = rlang::caller_env())) { # ?what's the use ? Let me block it out... delete later??
  #   gbd <- get("gbd", envir = rlang::caller_env())
  # }
  #message("Translating Cohen's d into percentage difference. Looking up data on summary exposure value...")
  data <- get_gbd(data, missing(data))

  #browser()
  params_manual <- clean_args(...)
  pars <- c(
    params_manual,
    effect$params[
      names(effect$params) %in% attr(data,"codebook")$variables$variable &
        !names(effect$params) %in% names(params_manual)
    ],
    list(measure = "summary exposure value", data = data) # for SEV, metric is always "Rate"
  )

  # todo: delete duplicates, and deal with inconsistent multiple values for arguments, otherwise dubble matching error
  sev <- gbd_filter(params = pars)

  # d = sqrt(p*(1-p))
  # d^2 = p*(1-p)
  # d^2 = p1-p^2

  # x1 <- 0:100/100
  # plot(x1, sqrt(x1-x1^2))

  #browser()

  SD <- sd_prop(sev$val/100)*100
  #SD <- sqrt( (sev$val/100)*(1-(sev$val/100)) )*100


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

  # percentage point decrease:
  pp <- effect$value*SD
  ppl <- effect$lower*SD
  ppu <- effect$upper*SD

  before <- sev$val; before
  after <- min(100, max(0, sev$val - pp)); after
  # SEV between 0 and 100

  # percentage decrease
  pct <- (before - after)/before; pct
  before*pct
  before - after

  before
  afterL <- min(100, max(0, sev$val - ppl)); afterL
  pctL <- (before - afterL)/before; pctL

  before
  afterU <- min(100, max(0, sev$val - ppu)); afterU
  pctU <- (before - afterU)/before; pctU

  pcth <- 100*pct
  pcthl <- 100*pctL
  pcthu <- 100*pctU

  prt <- pretty_nr(
    pp, ppl, ppu,
    before,
    after, afterL, afterU,
    pcth, pcthl, pcthu
  ) #%>% print

  params_new <- gbd_params(sev)
  params_new <- c(
    effect$params[!names(effect$params) %in% names(params_new)],
    params_new
  )
  params_new$cause <- NULL
  params_new$metric <- NULL
  params_new$measure <- NULL

  #browser()

  new_effect(
    value = pct,
    lower = pctL,
    upper = pctU,
    stat = paste0("percentage decrease in exposure to ",sev$rei),
    unit = "percent",
    describe = c(effect$describe, paste0(
      "The percentage decrease in ",sev$rei," exposure is ",
      pretty_interval(pct,pctL,pctU,unit="percent"),"."
    )),
    explain = c(effect$explain, paste0(
        "Note: If the estimate has a minus sign, it indicates an increase in ",
        sev$rei,".\n\n",
        "We derive the estimate from the ",effect$stat,
        ", which is ",pretty_interval(effect$value,effect$lower,effect$upper),
        ". Now, to calculate the percentage decrease, we make use of data on exposure to ",sev$rei,
        " from the Global Burden of Disease study (GBD). The GBD codebook defines the Summary Exposure Value (SEV) as a measure of a population's exposure to a risk factor that takes into account the extent of exposure by risk level and the severity of that risk's contribution to disease burden. SEV takes the value zero when no excess risk for a population exists and the value one when the population is at the highest level of risk. They report SEV on a scale from 0% to 100% to emphasize that it is risk-weighted prevalence. So, exposure is here a proportion. Since exposure is not a numeric variable, we can't use a traditional Cohen's d. However, Cohen's d essentially describes the effect in terms of standard deviations. For proportions, the standard deviation equals the square root of p*(1-p), where p is the proportion. (For computational simplicity, we assume Cohen's d is calculated using the standard deviation of the control group, without a behavioral nudge, rather than the pooled standard deviation.) We get the baseline proportion from the GBD dataset. According to this dataset, ",
        tolower_1(quiet(gbd_describe(params = pars, explain = FALSE))),
        " The standard deviation is then sqrt(p*(1-p)), that is, ",pretty_nr(SD)," percentage points. (Note that we chose not to add an extra confidence interval here for the error on the baseline proportion, because `behavior_pct_to_disease()` already adds a confidence interval based on GBD data error.) The percentage point difference (decrease) is then simply the Cohen's d and its confidence interval multiplied by this standard deviation. That tells us the percentage point decrease in exposure to ", sev$rei, " is ",
        pretty_interval(pp,ppl,ppu,unit="percentage points"),
        ". So, exposure to ", sev$rei, " drops from ",
        prt$before, " percent without nudge, to ",
        pretty_interval(prt$after,prt$afterU,prt$afterL,unit="percent",pct_100=FALSE),
        " with nudge. Note: If the jump from Cohen's d to proportions is difficult to follow, think of proportion data, like the Summary Exposure Value data, as numeric data with two values: 0 and 1. You can calculate the mean of that (bivalent) numeric variable as usual: you would find it equals the proportion. You can also calculate the standard deviation as usual: you would find it equals sqrt(p*(1-p)). Since we have two groups - a control group and a nudged group - you can also calculate Cohen's d as usual.",
        "\n\nThe last step is to translate this percentage point decrease into a percentage decrease. Now, a percentage decrease is just the percentage points decrease divided by the baseline proportion, that is, ",
        pretty_interval(prt$pp,prt$ppl,prt$ppu,unit="percentage points"),
        ", divided by ", prt$before, ". This tells us the percentage decrease is ",
        pretty_interval(prt$pcth,prt$pcthl,prt$pcthu,unit="percent",pct_100=FALSE), "."
    )),
    facts = c(effect$facts,
      quiet(gbd_describe(params = pars, explain = FALSE, reference = TRUE))
    ),
    reference = c(effect$reference,txt$gbd_cite),
    source = c(effect$source, txt$gbd_source_plus),
    terms = c(effect$terms,txt$gbd_boiler),
    call = fcall,
    params = params_new
  ) #%>% explanation # uncomment

}
if (FALSE) {
  eff <- nudge_to_behavior_d(behavior = "health", nudge = "information") %>%
    #print %>%
    behavior_d_to_pct(cause = "diabetes mellitus") #%>% print %>% explanation
  eff
  eff %>% explanation
  eff$describe
  eff$facts
  #eff$explain
  eff$reference
}

odds <- function(p) {
  y <- p/(1-p)
  return(y)
}
oddsratio <- function(pt, pc) { # probability treatment, probability control
  y <- odds(pt)/odds(pc)
  message("OR = ",round(odds(pt),2),"/",round(odds(pc),2))
  return(y)
}#; oddsratio(.4,.2); oddsratio(.6,.4); oddsratio(.8,.1); oddsratio(.9,.6)

d_to_OR_raw <- function(d) {

  # Theory - don't delete:

  # The odds ratio is defined as the ratio of the odds of A in the presence of B and the odds of A in the absence of B
  # The odds ratio is the probability of success/probability of failure. As an equation, that s P(A)/P(-A), where P(A) is the probability of A, and P(-A) the probability of not A

  # Odds = p/(1-p)
  # Odds of Behavior/ Odds of Not Behavior

  # Try:
  # d <- .2
  # LOR <- (d*pi/sqrt(3)) %>% print
  # effectsize::d_to_oddsratio(.2, log = TRUE) # same
  # OR <- exp(d*pi/sqrt(3)) %>% print # this is the good one!
  # effectsize::d_to_oddsratio(.2, log = FALSE) # same

  y <- exp(d*pi/sqrt(3))

  return(y)
}#; d_to_OR_raw(.2); d_to_OR_raw(.5); d_to_OR_raw(-.2); 1/d_to_OR_raw(.2)


#' @describeIn disease Convert Cohen's d into odds ratio.
#' @param effect Cohen's d, of `better_effect` class
#' @export
#' @examples
#' \dontshow{
#' NULL
#' library(tidyverse)
#' }
#' nudge_to_behavior_d(nudge = "information", behavior = "health") %>%
#'   d_to_OR
d_to_OR <- function(effect){

  force(effect)
  if (effect$unit != "standard deviations") {
    stop("d_to_OR requires a Cohen's d as input, that is, effect$unit has to be \"standard deviations\".")
  }

  #browser()

  ev <- d_to_OR_raw(effect$value)
  el <- d_to_OR_raw(effect$lower)
  eu <- d_to_OR_raw(effect$upper)

  new_effect(
    value = ev,
    lower = el,
    upper = eu,
    stat = "odds ratio",
    unit = "",
    describe = c(effect$describe, paste0(
      "The odds ratio is ",pretty_interval(ev, el, eu),"."
    )),
    explain = c(effect$explain, paste0(
      "Following the method of Hasselblad and Hedges (1995), we convert Cohen's d into an odds ratio (OR) with the following formula: exp(d*pi/sqrt(3)). Since the log odds ratio (LOR) is d*pi/sqrt(3), the odds ratio (OR) is exp(d*pi/sqrt(3))."
    )),
    reference = c(effect$reference,"Hasselblad, V., & Hedges, L. V. (1995). Meta-analysis of screening and diagnostic tests. Psychological Bulletin, 117(1), 167-178. https://doi.org/10.1037/0033-2909.117.1.167"),
    facts = c(effect$facts,NA),
    source = c(effect$source,NA), # "Hasselblad, V., & Hedges 1995" # no, no source of data
    terms = c(effect$terms,NA),
    call = NA,
    params = effect$params
  ) #%>% print %>% explanation # uncomment ???

}#; nudge("information") %>% d_to_OR

OR_to_pt <- function(OR,pc) {

  # Theory - don't delete

  # https://stats.stackexchange.com/questions/324410/converting-odds-ratio-to-percentage-increase-reduction

  # OR = ( pt/(1-pt) ) / ( pc/(1-pc) )
  # OR*pc/(1-pc) = chunk = C = pt/(1-pt)
  # pt = C*(1 - pt) = C - pt*C
  # pt*(1 + C) = C
  # pt = C/(1 + C) = OR*pc/(1-pc)*(1 + OR*pc/(1-pc))
  # = OR*pc/(1 + OR*pc/(1-pc) - pc - pc*(OR*pc/(1-pc)))
  # = OR*pc/(1-pc + OR*pc -pc + pc^2 ...)

  # OR <- .75
  # pc <- .65 # prob control
  #
  # pt <- OR*pc/(1+OR*pc-pc)# prob treatment
  #
  # pt <- .7
  # pc <- .3
  # OR = ( pt/(1-pt) ) / ( pc/(1-pc) )
  # OR*pc/(1+OR*pc-pc)
  # https://stats.stackexchange.com/questions/324410/converting-odds-ratio-to-percentage-increase-reduction

  pt <- OR*pc/(1 + OR*pc - pc)
  return(pt)
}
pt_to_pct <- function(pt,pc) {
  y <- (pc - pt)/pc
  return(y)
}
OR_to_pct_raw <- function(OR,pc) {
  pt <- OR_to_pt(OR,pc)
  y <- pt_to_pct(pt,pc)
  #browser()
  return(y)
}

clean_args <- function(...) {
  argums <- list(...)
  if (length(argums) == 0) { # list(), that is, no arguments entered
    return(argums)
  } else if (length(names(argums)) == 0) { # no names whatsoever
    names(argums) <- methods::formalArgs(gbd_filter)[1:length(argums)]
  } else { # some names
    if (any(names(argums) == "")) {
      if (any(names(argums) != "")) {
        if (min(which(names(argums) != "")) < max(which(names(argums) == ""))) {
          stop("Nonnamed arguments after a named argument. Fix how you input arguments.")
        }
      }
      which_names <- which(names(argums) == "")
      names(argums)[which_names] <- methods::formalArgs(gbd_filter)[which_names]
    }
  }
  return(argums)
}#; clean_args("diabetes"); clean_args("diabetes", "dalys", rei = "all", sex = "male"); clean_args("diabetes", "dalys", rei = "all", sex = "male", "something wrong", year = 2019)

#' @describeIn disease Convert odds ratio into percentage decrease.
#' @export
#' @examples
#' \dontshow{
#' NULL
#' library(tidyverse)
#' }
#' nudge_to_behavior_d(nudge = "information", behavior = "health") %>%
#'   d_to_OR %>%
#'   behavior_OR_to_pct
behavior_OR_to_pct <- function(
  effect, ..., data
  ) {

  #browser()
  if (effect$stat != "odds ratio") {
    #browser()
    stop("behavior_OR_to_pct requires that effect$stat is \"behavior_OR_to_pct\". Actual value: ", effect$stat)
  }

  data <- get_gbd(data, missing(data))

  #browser()
  params_manual <- clean_args(...)
  pars <- c(
    params_manual,
    effect$params[
      names(effect$params) %in% attr(data,"codebook")$variables$variable &
        !names(effect$params) %in% names(params_manual)
    ],
    list(measure = "summary exposure value", data = data) # for SEV, metric is always "Rate"
  )
  #rei <- c(effect$params$rei, params_manual$rei)

  #browser()
  # todo: delete duplicates, and deal with inconsistent multiple values for arguments, otherwise double matching error
  sev <- gbd_filter(params = pars)

  # browser()

  ev <- OR_to_pct_raw(1/effect$value, sev$val/100) #%>% print
  el <- OR_to_pct_raw(1/effect$lower, sev$val/100) #%>% print
  eu <- OR_to_pct_raw(1/effect$upper, sev$val/100) #%>% print

  params_new <- gbd_params(sev)
  params_new <- c(
    effect$params[!names(effect$params) %in% names(params_new)],
    params_new
  )
  params_new$cause <- NULL
  params_new$metric <- NULL
  params_new$measure <- NULL

  #browser()

  new_effect(
    value = ev,
    lower = el,
    upper = eu,
    stat = paste0("percentage decrease in exposure to ",sev$rei),
    unit = "percent",
    describe = c(effect$describe, paste0(
      "The percentage decrease in ",sev$rei," exposure is ",
      pretty_interval(ev,el,eu,unit="percent"),"."
    )),
    explain = c(effect$explain, paste0(
      "Note: If the estimate has a minus sign, it indicates an increase in ",
      sev$rei,".\n\n",
      "The ",effect$stat,
      " is ",pretty_interval(effect$value,effect$lower,effect$upper),
      ". Because the effect represents a decrease in the occurrence of the behavior, we take the inverse of this OR, wich gives: ",
      pretty_interval(1/effect$value,1/effect$upper,1/effect$lower),
      ". To convert the OR into the percentage decrease in behavioral risk exposure, we make use of the following formula: OR = Ot/Oc, where Ot refers to the odds of the behavior occurring given the nudge (\"treatment\"), and Oc refers to the odds without nudge (\"control\"). We also know that Ot = Pt/(1 - Pt) and Oc = Pc/(1 - Pc), with Pt and Pc referring to the probabilities of the behavior occurring given either the nudge (\"treatment\") or no nudge (\"control\"). From these formulas, one can derive that Pt = OR*Pc/(1 + OR*Pc - Pc). So, to calculate Pt we need not only OR but also Pc. We obtain Pc from data on exposure to ",sev$rei,
      " from the Global Burden of Disease study (GBD). The GBD codebook defines the Summary Exposure Value (SEV) as a measure of a population's exposure to a risk factor that takes into account the extent of exposure by risk level and the severity of that risk's contribution to disease burden. SEV takes the value zero when no excess risk for a population exists and the value one when the population is at the highest level of risk. They report SEV on a scale from 0% to 100% to emphasize that it is risk-weighted prevalence. So, exposure is here a proportion. So we can get Pc from the GBD dataset. According to this dataset, ",
      tolower_1(quiet(gbd_describe(params = pars, explain = FALSE))),
      " Given the formula mentioned higher, Pt is then ",
      pretty_interval(
        OR_to_pt(1/effect$value,sev$val/100),
        OR_to_pt(1/effect$upper,sev$val/100),
        OR_to_pt(1/effect$lower,sev$val/100)
      ),
      ". Finally, the percentage difference is simply (Pc - Pt)/Pc."
    )),
    facts = c(effect$facts,quiet(gbd_describe(params = pars, explain = FALSE, reference = TRUE))),
    reference = c(effect$reference,txt$gbd_cite),
    source = c(effect$source, txt$gbd_source_plus),
    terms = c(effect$terms,txt$gbd_boiler),
    call = NA,
    params = params_new
  ) #%>% print %>% explanation # uncomment

}#; nudge("information") %>% d_to_OR %>% behavior_OR_to_pct

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

pretty_gbd_interval <- function(x) {
  # feed it a line of gbd
  pretty_interval(
    val = x$val, low = x$lower, high = x$upper,
    unit = gbd_unit(measure = x$measure, metric = x$metric),
    pct_100 = FALSE
  )
}
pretty_gbd_interval_long <- function(val, lower, upper, measure, metric) {
  # deprecated: use pretty_gbd_interval() instead
  # todo: get rid of this long one
  pretty_interval(
    val = val, low = lower, high = upper,
    unit = gbd_unit(measure = measure, metric = metric)
  )
}#; pretty_gbd_interval_long(10.8987,8.1,11.89,"DALYs","rate")


#' Given a behavior effect, get the disease effect
#'
#' Given a behavior effect, possibly Cohen's d, get the disease effect
#'
#' # The `better_effect` class
#'
#' Check out the full calculation with `explanation(.)`.
#'
#' # Terms of use
#'
#' A `better_effect` is a guesstimate rather than a rigorous impact analysis. Use it with caution.
#'
#' `better` functions that generate `better_effect`s make use of data owned by others. The authors and owners of these data retain all rights to their data. Also, they were not involved in the creation of `better`, and bear no responsibility for any possible issues with the representation and/or use of the data.
#'
#' @param effect The behavior effect to be translated into a disease effect, class `better_effect`. Either Cohen's d or percentage decrease in exposure. By default `nudge(nudge = "all", behavior = "health")`.
#' @param cause The disease, that is, the cause of mortality and/or disability
#' @param measure The measure for mortality/disability
#' @param metric The metric (rate, percentage, number) for the measure
#' @param ... Other arguments for `gbd_filter`.
#' @param method Method to convert Cohen's d into percentage change. By default, `"Hasselblad and Hedges"`. Alternatively, `"standard deviations"`. The `"Hasselblad and Hedges"` method converts Cohen's d into an Odds Ratio following Hasselblad, V., & Hedges, L. V. (1995). Meta-analysis of screening and diagnostic tests. Psychological Bulletin, 117(1), 167-178. The `"standard deviations"` converts Cohen's d into percentage change based on the definition of Cohen's d in terms of standard deviations together with the formula for the standard deviation of proportions.
#'
#' @return The effect on disease mortality and/or disability, class `better_effect`
#' @export
#'
#' @examples
#' \dontshow{
#' library(tidyverse)
#' NULL
#' }
#' nudge("information", behavior = "food") %>%
#'   disease("diabetes mellitus")
disease <- function(
    effect = nudge(nudge = "all", behavior = "health"),
    # cause = "all causes", # now defaults taken care of in gbd_filter()
    # measure = "DALYs",
    # metric = "rate",
    ...,
    method = "Hasselblad and Hedges" # standard deviations
) {
  #browser()

  if (effect$unit == "standard deviations") {
    if (method == "standard deviations") {
      y <- quiet(behavior_d_to_pct(effect, ...))
    } else if (method == "Hasselblad and Hedges") {
      y <- quiet(behavior_OR_to_pct(d_to_OR(effect), ...))
    }
  } else if (effect$stat == paste0("percentage decrease in exposure to ",effect$params$rei)) {
    y <- effect
  } else {
    stop("No valid effect inputted: Input either a Cohen's d, or a percentage diffference.")
  }

  y <- quiet(behavior_pct_to_disease(
    effect = y,
    # cause = cause,
    # measure = measure,
    # metric = metric,
    ...
  ))
  return(y)
}
#nudge("information") %>% disease("diabetes mellitus")

#' @describeIn disease Given a behavior effect in percentage decrease, get the disease effect
#' @param data `r txt$gbd_doc_dataset`
#' @export
#' @examples
#' \dontshow{
#' NULL
#' library(tidyverse)
#' }
#' nudge_to_behavior_d(nudge = "information", behavior = "health") %>%
#'   behavior_d_to_pct %>%
#'   behavior_pct_to_disease(cause = "diabetes mellitus")
behavior_pct_to_disease <- function(
  effect,
  # cause,
  # measure = "DALYs", #best just display: c("deaths", "YLLs", "YLDs", "DALYs"), # They don't have this for Risks: "life expectancy","HALE"
  # metric = "rate", #best just display: c("number", "percent", "rate"),
  ..., data
  ) {
  # from behavior impact to DALYs impact

  #browser()
  if (effect$stat != paste0("percentage decrease in exposure to ",effect$params$rei)) {
    #browser()
    stop("behavior_pct_to_disease requires that effect$stat is \"percentage decrease in exposure to ",effect$params$rei,"\". Actual value: ", effect$stat)
  }

  fcall <- pretty_call(sys.call(), formals())

  # if (exists("gbd", envir = rlang::caller_env())) {
  #   gbd <- get("gbd", envir = rlang::caller_env())
  # }
  #message("Looking up data on cause of disease/disability...")
  data <- get_gbd(data, missing(data))

  #browser()
  params_manual <- clean_args(...)
  pars <- c(
    params_manual,
    effect$params[
      names(effect$params) %in% attr(data,"codebook")$variables$variable &
        !names(effect$params) %in% names(params_manual)
    ],
    list(data = data) # for SEV, metric is always "Rate"
  )

  #browser()
  #cause <- c(list(...),effect$params)$cause
  #rei <- c(list(...),effect$params)$rei

  # pars$rei <- c(pars$rei,NA)
  # pars$cause <- c(pars$cause,NA)

  sct <- gbd_filter(params = pars) # sct[i,] # parsi

  parsev <- pars
  parsev$cause <- NULL
  parsev$measure <- "summary exposure value"
  sev <- gbd_filter(params = parsev)

  #browser()
  # sct <- gbd_filter(params = pars)
  #
  # parsi <- c(
  #   pars,
  #   list(
  #     rei = rei,
  #     measure = measure,
  #     metric = metric
  #   )
  # )

  if (nrow(sct) == 0) {
    #browser()
    stop("Data subsetted is empty.")
  } else if (nrow(sct) > 1) {
    stop("Subset of data not unique (multiple rows).")
  }

  msrs <- attr(data,"codebook")$measures

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

  before <- sev$val #[sct$measure %in% "summary exposure value"]

  nrs <- pretty_nr(
    # exposure_change = effect$value,
    # exposure_before = before,
    # exposure_after = after,
    #harm_before = sct$val,
    pct = effect$value*100,
    pctL = effect$lower*100,
    pctU = effect$upper*100
    #, harm_disease = NA
  ) #%>% print

  vl <- sct$val*effect$value
  lw <- sct$lower*effect$lower # ? how calculate ? sct$lower - sct$lower*effect$lower ?
  up <- sct$upper*effect$upper

  #browser()

  params_new <- gbd_params(sct) # this is the right approach: explicit, not implicit in gbd_filter
  params_new <- c(
    effect$params[!names(effect$params) %in% names(params_new)],
    params_new
  )

  #browser()

  y <- quiet(new_effect(
    value = vl,
    lower = lw,
    upper = up,
    stat = params_new$measure,
    unit = gbd_unit(measure = params_new$measure, metric = params_new$metric),
    describe = c(
      effect$describe,
      paste0(
        "A behavioral intervention in ",pretty_nudge(behavior=effect$params$behavior,nudge=effect$params$nudge), # msrs$explain[msrs$measure == measure]
        " that targets " , params_new$cause,
        " saves ", pretty_gbd_interval_long(vl,lw,up, measure = params_new$measure, metric = params_new$metric),"."
      )
    ),
    explain = c( effect$explain, paste0(
      "We previously estimated that the ",
      effect$stat, " is ",
      pretty_interval(effect$value,effect$lower,effect$upper,unit=effect$unit),
      ". We also know that at ",pretty_nr(before, always_vector = TRUE),"%, exposure to ",params_new$rei,
      " costs ",pretty_nr(sct$val)," ",gbd_unit(measure = params_new$measure, metric = params_new$metric)," due to ",params_new$cause, ". That is, we know from the GBD data that ",
      tolower_1(gbd_describe(params = pars, explain = FALSE)),
      " The last step is to take ", pretty_interval(effect$value,effect$lower,effect$upper,unit=effect$unit),
      ", of ",pretty_interval(sct$val, sct$lower, sct$upper, unit = params_new$measure),
      ", and to combine both confidence intervals. That gives us the estimate that the nudge saves ", pretty_gbd_interval_long(vl,lw,up, measure = params_new$measure, metric = params_new$metric),"."
    ) ),
    facts = c(effect$facts,paste0("At ",pretty_nr(before, always_vector = TRUE),"%, exposure to ",params_new$rei,
      " costs ",pretty_nr(sct$val)," ",gbd_unit(measure = params_new$measure, metric = params_new$metric)," due to ",params_new$cause, cite_shorter(txt$gbd_cite_short, brackets = TRUE),".")),
    reference = c(effect$reference,txt$gbd_cite),
    source = c(effect$source, txt$gbd_source_plus),
    terms = c(effect$terms,txt$gbd_boiler),
    call = fcall,
    params = params_new
  )) #%>% explanation # ???
  #browser()

  return(y)

}
if (FALSE) {
  dvl_options()
  gbd_load()
  eff <- nudge_to_behavior_d(behavior = "health", nudge = "information") %>%
    behavior_d_to_pct %>%
    explanation
  nudge_to_behavior_d(behavior = "health", nudge = "information") %>%
    behavior_d_to_pct %>%
    behavior_pct_to_disease(cause = "diabetes mellitus") # is with global nudges, so can't do: location = "United States of America"
}

#' Manually create a Cohen's d effect
#'
#' Create an effect of the class `better_effect` based on a Cohen's d, with `behavior_d()`, or based on a percentage difference, with `behavior_pct()`.
#'
#' @param value Value of the effect, either Cohen's d or percentage difference, with default
#' @param lower Lower boundary of 95% confidence interval, by default NA
#' @param upper Upper boundary of 95% confidence interval, by default NA
#'
#' @return Effect, class `better_effect`
#' @export
#'
#' @examples
#' behavior_d(.2)
behavior_d <- function(value = .5, lower = NA, upper = NA) {
  fcall <- pretty_call(sys.call(), formals())
  new_effect(
    value = value,
    lower = lower,
    upper = upper,
    stat = "Cohen's d",
    unit = "standard deviations",
    explain = paste0(
      pretty_interval(value,lower,upper,unit = "standard deviations"),
      "\n\nThis is a manually inserted Cohen's d."
    ),
    call = fcall,
    params = list(
      rei = "behavioral risks"
    )
  )
}#; behavior_d(.56)

#' @describeIn behavior_d Create an effect based on a percentage difference.
#' @export
#' @examples
#' behavior_pct(15)
behavior_pct <- function(value = 1, lower = NA, upper = NA) {
  fcall <- pretty_call(sys.call(), formals())
  new_effect(
    value = value,
    lower = lower,
    upper = upper,
    stat = "percentage decrease in exposure to behavioral risks",
    unit = "percent",
    explain = paste0(
      pretty_interval(value,lower,upper,unit = "percent"),
      "\n\nThis is a manually inserted percentage difference."
    ),
    call = fcall,
    params = list(
      rei = "behavioral risks"
    )
  )

}

nudge_to_disease <- function(nudge, disease) {
  # not good! just do nudge %>% disease, or just do disease dry?? implement
}


#' Get the etiology effect of a disease
#'
#' Convert a disease effect, like `"lower respiratory infections"`, into an etiology effect, like `"influenza"`. In our example, this boils down to investigating how influenza (the etiology) causes lower respiratory infections (the disease), which cause disability and death.
#'
#' @param effect The disease effect (e.g., `"lower respiratory infections"`) to be converted into an etiology effect (e.g., `"influenza"`).
#' @param x Etiology
#' @param data `r txt$gbd_doc_dataset`
#'
#' @return The etiology effect, as `better_effect` object
#' @export
#'
#' @examples
#' \dontshow{
#' library(tidyverse)
#' NULL
#' }
#' nudge("information") %>%
#'   disease("lower respiratory infections") %>%
#'   etiology("influenza")
etiology <- function(effect, x, data) {

  #browser()

  if (!stringr::str_detect(utils::tail(effect$call,1),"disease")) {
    stop("etiology requires input from a disease function. Actual value: ", effect$call)
  }

  data <- get_gbd(data, missing(data))

  pars <- c(
    effect$params[names(effect$params) %in% attr(data,"codebook")$variables$variable],
    list(data = data)
  ) #%>% print
  pars$rei <- NA_character_
  causei <- gbd_filter(params = pars) #%>% print

  pars$rei <- x
  reii <- gbd_filter(params = pars) #%>% print

  deflator <- reii$val/causei$val

  v <- effect$value*deflator
  l <- effect$lower*deflator
  h <- effect$upper*deflator

  params_new <- gbd_params(reii)

  params_new <- c(
    effect$params[!names(effect$params) %in% names(params_new)],
    params_new
  )

  new_effect(
    value = v,
    lower = l,
    upper = h,
    stat = effect$stat,
    unit = effect$unit,
    describe = c(effect$describe, paste0(
      "The nudge aimed at fighting ",effect$params$cause,
      " by targeting ",x,
      " saves ",pretty_interval(v,l,h,unit=effect$unit),"."
    )),
    explain = c(effect$explain, paste0(
      "Note: If the estimate has a minus sign, it indicates an increase in ",
      effect$stat,".\n\n",
      "To convert the estimate in terms of ",effect$params$cause,
      " into ",x," we consider the following two statistics from the GBD dataset: \n- ",
      quiet(gbd_describe(causei)),"\n- ",quiet(gbd_describe(reii)),
      "\nFrom that we derive a deflator to apply to the effect inputed. The deflator is ",
      pretty_nr(reii$val, causei$val)[[1]]," divided by ",pretty_nr(reii$val, causei$val)[[2]],
      ", which gives ",pretty_nr(deflator),
      ". We multiply the inputted effect (value and confidence) by this multiplicator to get the estimate."
    )),
    facts = c(effect$facts,list(quiet(gbd_describe(causei)),quiet(gbd_describe(reii)))),
    reference = c(effect$reference,txt$gbd_cite),
    source = c(effect$source, txt$gbd_source_plus),
    terms = c(effect$terms,txt$gbd_boiler),
    call = NA,
    params = params_new
  ) #%>% print %>% explanation # uncomment
}

#' Get sample size with power calculation
#'
#' For a given effect, get the sample size through a power calculation with [pwr::pwr.t.test()], which `sample_size` wraps.
#'
#' @param effect Effect, of `better_effect` class.
#' @param power Power of the test. By default `.8`.
#' @param alternative Alternative hypothesis. By default "less". Can also be "two.sided" or "greater".
#' @param ... Arguments passed on to test function of `pwr` package. Depending on whether the effect is a number or a proportion, this will be `pwr::pwr.t.test` or `pwr::pwr.2p.test`.
#'
#' @return Object of class "power.htest", from `pwr` package.
#' @inherit pwr::pwr.t.test return details
#' @export
#'
#' @examples
#' \dontshow{
#' library(tidyverse)
#' NULL
#' }
#' nudge("information", behavior = "food") %>%
#'   sample_size
sample_size <- function(effect, power = .8, alternative = "less", ...) {

  # Given an effect, get the sample size needed for an experiment that tests the effect.
  # pwr::pwr.2p.test()
  # pwr::pwr.t.test()
  #browser()
  if (effect$unit == "standard deviations") {
    d_to_sample_size(effect = effect, power = power, alternative = alternative, ...)
  } else if (effect$unit == "percent") {
    stop("Power calculation for percentages not yet implemented.")
    #pct_to_sample_size(effect = effect, power = power, ...)
  } else if (effect$unit == "deaths per 100,000 people") { # proportion
    count_to_sample_size(effect = effect, power = power, alternative = alternative, ...)
  } else { # numeric
    #num_to_sample_size(effect = effect, power = power, ...)
    effd <- disease_num_to_d(effect)
    d_to_sample_size(effect = effd, power = power, alternative = alternative, ...)
  }
}

count_to_sample_size <- function(effect, ...) {
  #browser()
  # params <- effect$params
  # params$metric <- "rate"
  gbdi <- gbd_subset(effect$params)
  #gbd_describe(gbdi)
  n1 <- gbdi$val
  p1 <- n1/(10^5)
  n2 <- (gbdi$val - effect$value)
  p2 <- n2/(10^5)
  pn <- pretty_nr(n1, n2)
  pp <- pretty_nr(p1*100, p2*100)
  hi <- pwr::ES.h(p2,p1)
  message(
    "The effect implies a drop from the baseline of ",
    pn$n1, " deaths to ", pn$n2, " deaths per 100,000. We translate these proportions into a Cohen's h with pwr::ES.h() for the power calculation."
    )
  pwr::pwr.2p.test(h = hi, ...)
}#; sample_size(nudge("information") %>% disease("diabetes mellitus", measure = "deaths"))


d_to_sample_size <- function(effect, ...) {
  #browser()
  pwr::pwr.t.test(d = -effect$value, type = "two.sample", ...)
}#; sample_size(nudge()); sample_size(nudge(),alternative="two.sided"); sample_size(nudge(),alternative="greater")


disease_num_to_d <- function(effect) {

  # Todo:
  # Probably too tough to get n data

  #browser()
  #effect %>% explanation
  gi <- gbd_filter(params = effect$params)
  # browser()
  # The standard error is most useful as a means of calculating a confidence interval. For a large sample, a 95% confidence interval is obtained as the values 1.96xSE either side of the mean.
  # range(CI) = 2*1.96*SE
  # SE = range(CI)/(2*1.96)
  # SE = sd / sqrt(n)
  # sd = SE*sqrt(n)
  # sd = sqrt(n)*range(CI)/(2*1.96)
  twoSE <- stats::qnorm(.975) # 1.96 standard errors
  rng <- gi$upper - gi$lower
  SE <- rng/(2*twoSE) # 1 standard error
  n <- NA
  stdev <- SE*sqrt(n)
  y <- effect$value/stdev
  #effect %>% explanation


  msg <- paste0(
    "We estimate Cohen's d on the basis of the 95% confidence interval of the underlying statistic. The GBD dataset tells us that, ",
    tolower_1(quiet(gbd_describe(gi, explain = FALSE))),
    " The range of this 95% confidence interval is ",
    pretty_nr(rng), " ", gi$measure,
    ". If we assume a normal distribution, then this range covers twice 1.96 standard deviations. From this, we derive that one standard deviation is ",
    pretty_nr(SE), " ", gi$measure,
    ". "
  ); cat(msg)
  message(msg)
  return(y)
}#; sample_size(disease())

# num_to_sample_size <- function(effect, ...) {
#   # if (exists("gbd", envir = rlang::caller_env())) {
#   #   gbd <- get("gbd", envir = rlang::caller_env())
#   # }
#   # data <- get_gbd(data, missing(data))
#   browser()
#   #effect %>% explanation
#   gbdi <- gbd_filter(params = effect$params)
#   # browser()
#   # effect$value
#   # effect$params
#   msg <- "d"
#   message(msg)
#   pwr::pwr.t.test(d = effect$value, ...)
# }#; sample_size(disease())


#gbd_describe_cause("diabetes mellitus")

#' Get the Dollar cost of disability
#'
#' For any disease effect, get a guesstimate of its Dollar cost. STILL IN DEVELOPMENT: For now, it's a very simple guesstimate based on one general statistic. In the future, it may be smarter and take into account the different costs of different diseases.
#'
#' # Source
#'
#' The cost estimate per year of disability comes from the following paper:
#'
#' Kennedy, J., Wood, E. G., & Frieden, L. (2017). Disparities in Insurance Coverage, Health Services Use, and Access Following Implementation of the Affordable Care Act: A Comparison of Disabled and Nondisabled Working-Age Adults. Inqruiry: The Journal of Health Care Organization, Provision, and Financing, 54, 004695801773403. https://doi.org/10.1177/0046958017734031
#'
#' @param effect Disease effect, of `better_effect` class
#' @param ... Arguments passed on to dispatched cost function, depending on chosen method. IN DEVELOPMENT
#' @param method Method of cost calculation, by default `"Kennedy, Wood, & Frieden, 2017"`. IN DEVELOPMENT
#'
#' @return Dollar cost, of `better_effect` class
#' @export
#'
#' @examples
#' \dontshow{
#' library(tidyverse)
#' NULL
#' }
#' nudge("information", behavior = "food") %>%
#'   disease("diabetes mellitus", measure = "YLDs") %>%
#'   cost
cost <- function(effect, ..., method = "Kennedy, Wood, & Frieden, 2017") {

  params <- list(...)
  #data <- get_gbd(data, missing(data))

  if (effect$params$measure != "YLDs") {
    stop("Cost can (for now) only be calculated on the basis of YLDs, that is, Years Lived with Disability.")
    # # TODO: allow for this, and translate yourself
    # pars <- effect$params
    # pars$measure <- "YLDs"
    # gbd_subset(pars)
  }

  cost_dis <- 13492
  cost_dis_SE <- 728

  cost_no <- 2835
  cost_no_SE <- 99

  SEs <- stats::qnorm(.975) # 1.959964

  cost_diff <- cost_dis - cost_no
  cost_diff_lower <- cost_diff - SEs*(cost_dis_SE + cost_no_SE)
  cost_diff_upper <- cost_diff + SEs*(cost_dis_SE + cost_no_SE)

  v <- effect$value*cost_diff
  l <- effect$lower*cost_diff_lower
  u <- effect$upper*cost_diff_upper

  pr <- pretty_nr(
    cost_dis, cost_dis_SE, cost_no, cost_no_SE,
    cost_diff, cost_diff_lower, cost_diff_upper,
    v,l,u,
    digits = 0
    ) #%>% print
  #browser()
  effect$params$measure <- paste0("cost of ", effect$params$measure)
  y <- suppressMessages(quiet(new_effect(
    value = v,
    lower = l,
    upper = u,
    stat = paste0("annual cost of ", effect$unit),
    unit = "$",
    describe = c(
      effect$describe,
      paste0(
        "The annual health care costs saved because of reducing ", effect$unit,
        " is $", pretty_interval(v,l,u), "."
      )
    ),
    explain = c( effect$explain, paste0(
      "We know from the scientific literature that the annual health care cost for a working-age adult with disabilities is on average $",
      pr$cost_dis, ", with a standard error of $", pr$cost_dis_SE,
      ", while for those without disabities the cost is $", pr$cost_no,
      ", with a standard error of $", pr$cost_no_SE,
      " (Kennedy, Wood, & Frieden, 2017). For our calculation, we need the difference between the costs for those with and without disabilities. To get the 95% confidence interval, we multiply the reported standard errors by 1.96 and aggregate these in the difference. This tells us the annual cost difference due to disabilty is ",
      pretty_interval(cost_diff,cost_diff_lower,cost_diff_upper,unit="$",digits = 0),
      ". The last step is to multiply these amounts with the amounts of the original effect, that is, a reduction of ",
      utils::capture.output(effect_only(effect)),", to get the estimate of the cost savings."
    ) ),
    facts = c(effect$facts, paste0("The annual health care cost for a working-age adult with disabilities is on average $",
      pr$cost_dis, ", with a standard error of $", pr$cost_dis_SE,
      ", while for those without disabities the cost is $", pr$cost_no,
      ", with a standard error of $", pr$cost_no_SE,
      " (Kennedy, Wood, & Frieden, 2017).")),
    reference = c(effect$reference,"Kennedy, Wood, & Frieden (2017). For details, run `?cost`"),
    source = c(effect$source,"Kennedy, J., Wood, E. G., & Frieden, L. (2017). Disparities in Insurance Coverage, Health Services Use, and Access Following Implementation of the Affordable Care Act: A Comparison of Disabled and Nondisabled Working-Age Adults. Inqruiry: The Journal of Health Care Organization, Provision, and Financing, 54, 004695801773403. https://doi.org/10.1177/0046958017734031"),
    terms = c(effect$terms,NA),
    call = NA,
    params = effect$params
  ))) #%>% explanation # ???
  #browser()

  return(y)

}

dta$benartzi <- tibble::tribble(
  ~nudge, ~cost, ~structural,

  # information
  "education", .93, FALSE,
  # assistance
  "plan", .33, FALSE,

  # structure
  "default", 3.21, TRUE,
  "incentive", 6.03, TRUE,
  "free", 14.28, TRUE
)
txt$benartzi_cite <- "Benartzi, S., Beshears, J., Milkman, K. L., Sunstein, C. R., Thaler, R. H., Shankar, M., Tucker-Ray, W., Congdon, W. J., & Galing, S. (2017). Should Governments Invest More in Nudging? Psychological Science, 28(8), 1041-1055. https://doi.org/10.1177/0956797617702501"
txt$benartzi_cite_short <- "Benartzi, Beshears, Milkman, et al. (2017)"
attr(dta$benartzi, "ownership") <- attr(dta$benartzi, "citation") <- txt$benartzi_cite


clean_N <- function(effect) {
  if (effect$params$metric == "rate" & is.null(effect$params$N)) {
    effect$params$N <- 100000
  }
  return(effect)
}

cost_nonstructural <- c(dta$benartzi$cost[dta$benartzi$nudge == "education"], dta$benartzi$cost[dta$benartzi$nudge == "plan"])
error_nonstructural <- stats::qnorm(0.975)*stats::sd(cost_nonstructural)/sqrt(length(cost_nonstructural))
cost_structural <- c(dta$benartzi$cost[dta$benartzi$nudge == "default"], dta$benartzi$cost[dta$benartzi$nudge == "incentive"], dta$benartzi$cost[dta$benartzi$nudge == "free"])
error_structural <- stats::qnorm(0.975)*stats::sd(cost_structural)/sqrt(length(cost_structural))

dta$benartzi_str <- tibble::tribble(
  ~nudge, ~value, ~lower, ~upper,
  "nonstructural", mean(cost_nonstructural), mean(cost_nonstructural)-error_nonstructural, mean(cost_nonstructural)+error_nonstructural,
  "structural", mean(cost_structural), mean(cost_structural)-error_structural, mean(cost_structural)+error_structural
)

#' Get the net cost savings
#'
#' Calculate the net cost savings, that is, the savings in terms of reduced healthcare cost minus the costs of the nudge campaign. The `"Benartzi, Beshears, Milkman, et al., 2017"` method makes use of cost estimates of nudges in two categories: structural (more expensive) and nonstructural (less expensive).
#'
#' @param effect The cost savings effect to be converted into a net cost savings effect.
#' @param ... Currently not used, in development.
#' @param method The method with which to calculate the net cost.
#'
#' @return The effect on net savings, as `better_effect` object.
#' @export
#'
#' @examples
#' \dontshow{
#' library(tidyverse)
#' NULL
#' }
#' nudge("information", behavior = "food") %>%
#'   disease("diabetes mellitus", measure = "YLDs") %>%
#'   cost %>%
#'   net
net <- function(effect, ..., method = "Benartzi, Beshears, Milkman, et al., 2017") {

  #browser()

  params <- list(...)
  #data <- get_gbd(data, missing(data))

  if (!stringr::str_detect(effect$stat,"cost")) {
    #browser()
    stop("Net cost can only be calculated on the basis of costs. In contrast, the stat parameter of the effect is the following: ",effect$stat)
  }

  effect <- clean_N(effect)
  #effect %>% explanation
  params_lookup <- effect$params
  params_lookup$measure <- "summary exposure value" # todo: should it be prevalence of the disease rather than summary exposure value of the behavioral risk?
  #gbd %>% filter(measure == "prevalence")
  gbd_lookup <- gbd_filter(params = params_lookup)
  n_target <- effect$params$N*gbd_lookup$val/100

  if (effect$params$nudge == "structure") {
    benartzi <- dta$benartzi_str[dta$benartzi_str$nudge == "structural",]
    structural <- TRUE
  } else {
    benartzi <- dta$benartzi_str[dta$benartzi_str$nudge == "nonstructural",]
    structural <- FALSE
  }
  v <- effect$value - benartzi$value*n_target
  l <- effect$lower - benartzi$upper*n_target
  u <- effect$upper - benartzi$lower*n_target

  # pr <- pretty_nr(
  #   cost_dis, cost_dis_SE, cost_no, cost_no_SE,
  #   cost_diff, cost_diff_lower, cost_diff_upper,
  #   v,l,u,
  #   digits = 0
  # ) #%>% print

  benartzi_table <- stringr::str_replace_all(paste0(utils::capture.output(dta$benartzi),collapse="\n")," ",".")

  fact <- paste0(
    "These are some costs per person of flu vaccination interventions: ",
    paste0(unlist(purrr::pmap(
      dta$benartzi[dta$benartzi$structural == structural,], ~{ paste0("$",.y," (",.x,")") }
    )),collapse="; "),
    cite_shorter(txt$benartzi_cite_short, brackets = TRUE),"."
  ) #%>% print

  #browser()

  y <- suppressMessages(quiet(new_effect(
    value = v,
    lower = l,
    upper = u,
    stat = stringr::str_replace(effect$stat,"cost","net cost"),
    unit = "$",
    describe = c(
      effect$describe,
      paste0(
        "The annual net cost savings, after deducting campaign costs, is ",pretty_interval(v,l,u, unit = ifelse(v>=0,"$","Dollars")), ".",
        ifelse(v<0, " The minus sign indicates that there is a net cost rather than savings.","")
      )
    ),
    explain = c( effect$explain, paste0(
      "If the savings is a negative number, this means that there is a net cost to the nudge. That is, the nudge is expected to cost more than it will save. Calculating the net cost savings was done based on estimates for the costs of vaccination nudges by Benartzi, Beshears, Milkman, et al. (2017) in their paper on the cost effectiveness of nudges. The authors estimated the costs per person of several flu vaccination interventions, some structural, others not:\n\n", benartzi_table,
      "\n\n We also assume that the intervention would only be rolled out to people exposed to the risk. GBD data tells us this is ",round(gbd_lookup$val,1),"%, which totals to a target population of ",pretty_nr(n_target),". Given the nudge is ",ifelse(effect$params$nudge == "structure","","not ")," structural, we limit estimates of costs to that subset of the cost data. Finally, we calculate a mean and a 95% confidence interval for the cost, namely ",pretty_interval(benartzi$value,benartzi$lower,benartzi$upper,unit="$"),"."
    )),
    facts = c(effect$facts,fact),
    reference = c(effect$reference,"Benartzi, Beshears, Milkman, et al. (2017) For details, run `?net`"),
    source = c(effect$source,txt$benartzi_cite),
    terms = c(effect$terms,NA),
    call = NA,
    params = effect$params
  ))) #%>% explanation # ???
  #browser()

  return(y)

}
# ; nudge("structure") %>%
#   disease("hypertensive heart disease", measure = "YLDs") %>%
#   cost %>%
#   net

# Todo:
per_dollar <- function() {

  cost_nonstructural <- c(dta$benartzi$cost[dta$benartzi$nudge == "education"], dta$benartzi$cost[dta$benartzi$nudge == "plan"])
  mean(cost_nonstructural)
  stats::sd(cost_nonstructural)
  error_nonstructural <- stats::qnorm(0.975)*stats::sd(cost_nonstructural)/sqrt(length(cost_nonstructural))
  cost_structural <- c(dta$benartzi$cost[dta$benartzi$nudge == "default"], dta$benartzi$cost[dta$benartzi$nudge == "incentive"], dta$benartzi$cost[dta$benartzi$nudge == "free"])
  mean(cost_structural)
  stats::sd(cost_structural)
  error_structural <- stats::qnorm(0.975)*stats::sd(cost_structural)/sqrt(length(cost_structural))

}
# per_population <- function() {
#
# }

#' Set the denominator for rates
#'
#' Convert a rate per 100,000 into any kind of rate by specifying the denominator.
#'
#' @param effect Effect, `metric` has to be `"rate`".
#' @param n The denominator for that rate: per how many? By default 1.
#'
#' @return Effect, `better_effect`.
#' @export
#'
#' @examples
#' \dontshow{
#' library(tidyverse)
#' NULL
#' }
#' nudge("information", behavior = "food") %>%
#'   disease("diabetes mellitus", measure = "YLDs") %>%
#'   cost %>%
#'   per(350000)
per <- function(effect, n = 1) {

  # n parameter: by default 100,000 if metric is "rate"
  # change to n = ...

  #browser()

  if (effect$params$metric != "rate") {
    #browser()
    stop("The metric parameter has to be \"rate\", but it is the following: ",effect$params$metric)
  }

  effect <- clean_N(effect)

  multiplier <- n/effect$params$N

  v <- effect$value*multiplier
  l <- effect$lower*multiplier
  u <- effect$upper*multiplier

  #browser()

  params_new <- effect$params
  params_new$N <- n

  #browser()

  stt <- stringr::str_replace_all(effect$stat,
                                  pretty_nr(effect$params$N),pretty_nr(n))
  unt <- stringr::str_replace_all(effect$unit,
                                  pretty_nr(effect$params$N),pretty_nr(n))

  y <- quiet(new_effect(
    value = v,
    lower = l,
    upper = u,
    stat = effect$stat,
    unit = unt,
    describe = c(
      effect$describe,
      paste0(
        toupper_1(stt),
        " saved is ",pretty_interval(v,l,u, unit = unt), ".",
        ifelse(v<0, " Be mindful of the minus sign in this result.","")
      )
    ),
    explain = c( effect$explain, paste0(
      "Before, the rate had a denominator of ",pretty_nr(effect$params$N),
      ". If we want to have a denominator of ",pretty_nr(n),
      ", we need to multiply all values by ",pretty_nr(multiplier),"."
    )),
    facts = c(effect$facts,NA),
    reference = c(effect$reference,NA),
    source = c(effect$source,NA),
    terms = c(effect$terms,NA),
    call = NA,
    params = params_new
  )) #%>% describe
  #browser()

  return(y)

}

# nudge("information") %>%
#   disease("diabetes and kidney diseases", measure = "YLDs") %>%
#   cost


# Use effects -------------------------------------------------------------

#' Succinctly describe the effect
#'
#' @param effect Effect to be described, `better_effect`
#'
#' @return The inputted effect, invisibly.
#' @export
#'
#' @examples
#' \dontshow{
#' library(tidyverse)
#' }
#' nudge("information") %>%
#'   describe
describe <- function(effect) {
  #browser()
  #message(txt$better_effect_message)
  cat(utils::tail(effect$describe,1))
  invisible(effect)
}#; describe(nudge("information"))

#' List the facts
#'
#' List the facts on which the guesstimate of the effect is based.
#'
#' @param effect The effect to be fact checked.
#'
#' @return The inputted effect, invisibly.
#' @export
#'
#' @examples
#' \dontshow{
#' library(tidyverse)
#' }
#' nudge("information") %>%
#'   facts
facts <- function(effect) {

  fcts <- unlist(effect$facts)
  fcts <- fcts[!is.na(fcts)]
  cat("-", paste0(fcts,collapse="\n- "))
  invisible(effect)
}

# GBD effects dataframe

gbd_effects_create <- function(data) {
  #dvlload("start")

  #browser()

  data <- get_gbd(data, missing(data))

  #gbd <- sample_n(gbd, 300) # FOR TESTING

  nudge_subs <- nudge_data()[nudge_data()$behavior %in% c("health", "food"),c("behavior","nudge")]
  #browser()
  gbd_subs <- x[x$estimate == "rei",pars_all]
  message("Nr of rows of GBD data: ",nrow(gbd_subs))

  message("Merging and expanding...")
  # merge: If by or both by.x and by.y are of length 0 (a length zero vector or NULL), the result, r, is the Cartesian product of x and y, i.e., dim(r) = c(nrow(x)*nrow(y), ncol(x) + ncol(y)).
  gbd_effects <- merge(
    nudge_subs,
    gbd_subs
  )
  message("Nr of rows of expanded data: ",nrow(gbd_effects))
  stopifnot(
    nrow(gbd_effects) ==
      nrow(nudge_subs)*nrow(gbd_subs)
  )
  #gbd_effects <- distinct(gbd_effects) # not necessary
  gbd_effects <- tibble::as_tibble(gbd_effects)

  #gbd_effects <- gbd_effects[1:100,] # TO TEST

  #y <- gbd_effects

  message("Calculating nudge effects (may take a while)...")
  gbd_effects$calculate_nudge <- purrr::pmap(
    gbd_effects,
    function(behavior, nudge, ...) {
      nudge(behavior = behavior, nudge = nudge)
    }
  )
  message("Calculating disease effects (may take a while)...")
  gbd_effects$calculate_disease <- purrr::pmap(
    gbd_effects,
    function(calculate_nudge, cause, measure, metric, rei, location, sex, age, year, ...) {
      y <- try(disease(calculate_nudge, cause = cause, measure = measure, metric = metric, rei = rei, location = location, sex = sex, age = age, year = year), silent = TRUE)
      if (class(y)[1] == "try-error") y <- NULL
      y
    }
  )

  #gbd_effects_saved <- gbd_effects
  gbd_effects <- tidyr::pivot_longer(
    gbd_effects,
    cols = tidyr::starts_with("calculate_"),
    names_to = "calculate",
    names_pattern = "calculate_(.*)",
    values_to = "effect"
    )

  message("Creating columns on basis of effects...")
  if ( any( names(gbd_effects$effect[[1]]) %in% names(gbd_effects) ) ) stop("Bug: better_effect object has overlapping names with gbd object.")
  for (i in c("value", "lower", "upper") ) { # names(gbd_effects[1,j][[1]][[1]])
    gbd_effects[[i]] <- purrr::map_dbl(
      gbd_effects$effect,
      function(x) {
        if (is.null(x)) {
          y <- NA_real_
        } else {
          y <- x[[i]]
        }
        y
      }
    )
  }

  # for (i in c("eff_nudge", "eff_disease")) {
  #   message("...for ",i,"...")
  #   for (j in c("value", "lower", "upper") ) { # names(gbd_effects[1,j][[1]][[1]])
  #     gbd_effects[[paste0(i,"_",j)]] <- purrr::map_dbl(
  #       gbd_effects[[i]],
  #       function(x) {
  #         if (is.null(x)) {
  #           y <- NA_real_
  #         } else {
  #           y <- x[[j]]
  #         }
  #         y
  #       }
  #     )
  #   }
  # }
  message("Finished")
  return(gbd_effects)
}
#gbd_effects <- gbd_effects_create(gbd)


gbd_effects_rda_path <- function(path = system.file("data", package = "better")) {
  # Obsolete: now part of package data
  if (!file.exists(path)) {
    stop("Path directory does not exist: ", path)
  }
  paste0(path,"/gbd_effects.rda")
}
gbd_effects_read <- function(path = gbd_effects_rda_path()) {
  # Obsolete: now part of package data

  #path <- get_path(path, missing(path))
  #path <- gbd_effects_rda_path() # in package data

  # if (is.null(path)) {
  #   message(
  #     "No path found."
  #   )
  # } else {

  if (file.exists(path)) {
    rlang::inform(paste0("Read from:\n  ",stringr::str_wrap(path)), .frequency = "once", .frequency_id = "Reading gbd_effects")
    #gbd_effects <- readRDS(path)
    load(path, envir = globalenv())
  } else {
    message(
      "No file found in:\n  ", stringr::str_wrap(path),
      "\n  Saving in that location..."
    )
    gbd_effects <- gbd_effects_save(path)
  }

  return(gbd_effects)
}

gbd_effects_save <- function(path, data, effects_path = gbd_effects_rda_path()) {
  # superfluous if added to package
  # Keep in case you want to build during package building
  path <- get_path(path, missing(path))
  data <- get_gbd(path, data_missing = missing(data))
  #browser()
  gbd_effects <- gbd_effects_create(data)
  save(gbd_effects, file = effects_path)
  message("gbd_effects with ",nrow(gbd_effects)," rows saved in: ",effects_path)
  invisible(gbd_effects)
}

get_gbd_effects <- function(x) {
  # obsolete because package data
  y <- gbd_effects_read()
  return(y)
}

gbd_effects_params <- c("behavior", "nudge", "cause", "measure", "metric", "rei", "location", "sex", "age", "year", "calculate")

cut_points <- function(x, N) {
  #browser()
  x <- sort(x)
  y <- data.frame(
    x = x,
    interval = cut(x,N-1, labels = FALSE)
  )
  y$first <- !duplicated(y$interval)
  y <- unique(c(y$x[y$first],max(x)))
  return(y)
}#; cut_points(c(1,2,3,7,14,16,20,21),5)

children <- function(..., reis = gbd_reis()$rei, inclusive = FALSE) {
  xs <- unlist(list(...))
  #browser()
  gbdreis <- gbd_reis()
  gbdreis$parent[gbdreis$rei == gbdreis$parent] <- "universal set"
  #gbdreis %>% View
  #browser()
  y <- c()
  for (x in xs) {
    if (inclusive) {
      yx <- x
    } else {
      yx <- c()
    }
    childs <- gbdreis$rei[gbdreis$parent %in% x]
    yx <- c(yx,childs)
    if (length(childs) > 0) {
      for (i in childs) {
        yx <- c(yx, children(i))
      }
    }
    y <- c(y,yx)
  }
  y <- y[y %in% reis]
  return(y)
}#; children("metabolic risks","minor injuries",inclusive=TRUE)#; children("environmental/occupational risks")
# children("all risk factors",reis = children("metabolic risks"))

add_code <- function(x) {
  if ("cause" %in% names(x)) {
    vari <- "cause"
  } else if ("rei" %in% names(x)) {
    vari <- "rei"
  }

  ylist <- rep(list(NULL),nrow(x))
  x$code <- NA
  x$code_id <- NA
  x$code_standard <- NA
  for (i in sort(unique(x$level))) {
    #browser()
    xi <- x[x$level == i,]

    if (i == 0) {
      for (j in 1:nrow(xi)) {
        #browser()
        #if (i>0) browser()
        #x$code
        x$code_id[x[[vari]] == xi[[vari]][j]] <- j
        ylist[x[[vari]] == xi[[vari]][j]] <- list(j)
      }
    } else {
      for (k in unique(xi$parent)) {
        #browser()
        xk <- xi[xi$parent == k,]
        #if(nrow(xk)==0) browser()
        for (j in 1:nrow(xk)) {
          #browser()
          #if (i>0) browser()
          if ( is.na(any(x[[vari]] == xk$parent[j]) )) browser()
          #x$code
          x$code_id[x[[vari]] == xk[[vari]][j]] <- j
          ylist[[which(x[[vari]] == xk[[vari]][j])]] <- c(unlist(ylist[x[[vari]] == xk$parent[j]]), j)

        }
      }
    }

  }
  #x$code_list <- ylist # better not

  x$code <- purrr::map_chr(ylist, function(t){
    #browser()
    y <- paste(t, collapse = ".")
    y
  }) #%>% print

  maxchar <- nchar(as.character(max(unlist(ylist)))) #%>% print
  maxlevel <- (max(x$level)+1) #%>% print
  x$code_standard <- purrr::map_chr(ylist, function(t){
    #browser()
    y <- c(t,rep(0,maxlevel-length(t)))
    y <- formatC(y, width = maxchar, flag="0")
    y <- paste(y, collapse = "-")
    y
  }) #%>% print
  #browser()
  return(x)
}#; gbd_causes() %>% add_code %>% dplyr::arrange(code_standard) %>% View

#' Create tree diagram of codebook
#'
#' @param x Codebook. By default, gbd_causes().
#'
#' @return The tree diagram, string, invisible.
#' @export
#'
#' @examples
#' tree()
tree <- function(x = gbd_causes()) {

  #browser()

  if ("cause" %in% names(x)) {
    names(x)[names(x)=="cause"] <- "rei"
    vari <- "cause"
  } else if ("rei" %in% names(x)) {
    vari <- "rei"
  }

  reis <- dplyr::arrange(add_code(x),code_standard)
  y <- purrr::pmap_chr(reis, function(rei, level, code, ...){
    y <- paste0(paste0(rep("  ",level),collapse=""),"\\",code,": ",rei)
    if(length(y)>1) browser()
    y
  }) #%>% print
  y <- paste0(y,collapse="\n")
  cat(y)
  invisible(y)
}#; gbd_causes() %>% tree#; gbd_reis() %>% tree

food_reis <- function() {
  # todo: what about BMI etc?
  c(
    "dietary risks",children("dietary risks"),
    "child and maternal malnutrition",children("child and maternal malnutrition")
  )
}
behavior_reis <- function(reis = gbd_reis()$rei) {
  # gbd_reis() %>% View
  # gbd_reis() %>% tree
  #browser()
  behaviors <- c(
    children("all risk factors",reis=reis,inclusive=TRUE)
  ) #%>% print
  nonbehaviors <- c(
    children("metabolic risks",reis=reis,inclusive=TRUE)
  ) #%>% print
  behaviors[!behaviors %in% nonbehaviors]
}#; behavior_reis()
country_locations <- function(reis = gbd_reis()$rei) {
  # gbd_reis() %>% View
  # gbd_reis() %>% tree
  #browser()
  behaviors <- c(
    children("all risk factors",reis=reis,inclusive=TRUE)
  ) #%>% print
  nonbehaviors <- c(
    children("metabolic risks",reis=reis,inclusive=TRUE)
  ) #%>% print
  behaviors[!behaviors %in% nonbehaviors]
}#; behavior_reis()

# # # FOR TEST
#
# gbd_load()
#
#
# toupper_1 <- function(x) {
#   # useful for changing sentences
#   substr(x, 1, 1) <- toupper(substr(x, 1, 1))
#   x
# }
#
# choices_cause <- as.list(
#   unique(c(
#     "all causes",
#     #txt$na, # I used to enable just nudge effects
#     sort(gbd$cause)
#   ))
# ) #%>% print
# names(choices_cause) <- toupper_1(unlist(choices_cause))
# names(choices_cause)[names(choices_cause) == "All causes"] <- "All diseases"
#
# choices_rei <- unique(c("all risk factors",sort(better:::behavior_reis(reis = gbd$rei)))) #%>% print
# choices_rei <- choices_rei[choices_rei != "behavioral risks"]
# choices_rei <- as.list(choices_rei)
# names(choices_rei) <- toupper_1(unlist(choices_rei))
# names(choices_rei)[names(choices_rei) == "All risk factors"] <- "All behaviors (risk factors)"
# #names(choices_rei)[names(choices_rei) == "Behavioral risks"] <- "Behaviors related to diet, exercise, drug use, sex, violence"
#
# # Todo: clean look of all these buttons
# choices_metric <- as.list(
#   unique(c(
#     "rate","number"
#   ))
# ) %>% print
# choices_measure <- as.list(
#   unique(c("deaths","DALYs","YLLs"))
# ) %>% print
# #names(choices_measure) <- toupper_1(unlist(choices_measure))
#
# choices_location_man <- unique(c("United States of America",sort(gbd$location))) %>% print
# #PrettyVector(choices_location_man)
# choices_location <- c(
#   "Global",
#   "United States of America",
#   "California",
#   "Mississippi",
#   "North America",
#   "Latin America & Caribbean - WB",
#   "Europe & Central Asia - WB",
#   "East Asia & Pacific - WB",
#   "South Asia - WB",
#   "Middle East & North Africa - WB",
#   "Sub-Saharan Africa - WB",
#   "High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI"
# )
# stopifnot(identical(sort(choices_location),sort(choices_location)))
# choices_location <- as.list(choices_location)
# names(choices_location) <- choices_location
# names(choices_location)[names(choices_location) == "Global"] <- "Globally"
# names(choices_location)[names(choices_location) == "Latin America & Caribbean - WB"] <- "Latin America & Caribbean - WB (World Bank region)"
# names(choices_location)[names(choices_location) == "High SDI"] <- "High SDI (Socio-Demographic Index)"
#
# #names(choices_cause)[names(choices_cause) == "All causes"] <- "All diseases"
#
# choices_sex <- as.list(
#   unique(c(sort(gbd$sex)))
# ) %>% print
# choices_age <- as.list(
#   unique(c("age-standardized",sort(gbd$age)))
# ) %>% print
# choices_year <- as.list(
#   unique(c("2019",sort(gbd$year)))
# ) %>% print
#
# # # FOR TEST

#' Graph with range of effects
#'
#' @param measure Measure
#' @param metric Metric
#' @param calculate What to calculate, by default disease.
#' @param year Year
#' @param ... Other
#' @param params Not yet developed
#' @param N How many comparisons, integer.
#' @param gbdeffects GBD effects data frame, by default `gbd_effects`.
#'
#' @return The data for the graph plotted, invisible.
#' @export
#'
#' @examples
#' compare_effects()
#' compare_effects(measure = "deaths")
compare_effects <- function(
  measure = get_default("measure"),
  metric = get_default("metric"),
  calculate = get_default("calculate"),
  year = get_default("year"),
  ...,
  params, N = 10, gbdeffects = gbd_effects
) {


  argums <- as.list(environment())
  argums <- c(argums,list(...))

  eff_pars <- argums[names(argums) %in% gbd_effects_params]

  # compare effects within
  #browser()
  # gbd_effects <- get_gbd_effects() # obsolete because package data
  #browser()

  if (is.na(eff_pars$year)) eff_pars$year <- max(gbdeffects$year)

  y <- gbdeffects

  diet <- food_reis()
  risk_factors <- c("all risk factors",children("all risk factors"))
  metabolic_risks <- c("metabolic risks",children("metabolic risks"))

  # Todo: only select risks that are furthest in the branches, otherwise it s always behavioral and dietary risks

  #browser()

  y <- y[
    # clean errors:
    !is.na(y$value) &
      !( y$behavior == "food" & ! y$rei %in% diet ) &
      !( y$behavior == "health" & y$rei %in% diet ) &
      # zoom in:
      !is.na(y$rei) &
      y$rei %in% risk_factors &
      !y$rei %in% metabolic_risks &
      y$sex == "both" &
      y$nudge != "all" &
      !y$rei %in% c("all risk factors") &
      !y$cause %in% c("all causes") &
      !y$location %in% c("Global") &
      y$behavior %in% c("food","health")
    ,
  ]

  #browser()

  #y %>% filter(value %in% max(y$value, na.rm = TRUE))

  #browser()

  #gbd_effects %>% names %>% PrettyVector()
  for (i in names(eff_pars)) {
    # Todo: for cause, rei and location, it should allow for the CHILDREN to be kept
    #browser()
    #if (i == "calculate") browser()
    # Todo: first try children of the category provided, THEN try the category
    ytemp <- y[y[[i]] %in% eff_pars[[i]],]
    # browser()
    # i
    # y$rei %>% table
    # ytemp$rei %>% table
    #y[!y[[i]] %in% eff_pars[[i]],]
    if (nrow(ytemp)!=0) { # We ve gotten rid of all risk factors, all causes, etc. Otherwise it may cause trouble
      y <- ytemp
    }
  }

  #browser()
  #y$rei %>% table
  # alcohol use         dietary risks low physical activity               smoking
  # 18                   111                    12                    15
  ytemp <- y[
    y$value >= .05*max(y$value)
    ,
  ]
  #ytemp$rei %>% table
  #ytemp$cause %>% table
  ysaved <- y
  #ysaved$rei %>% table
  #ysaved$cause %>% table
  y <- ytemp

  #browser()
  #y$rei %>% table
  # here already mostly dietary risks, and smoking

  cutted <- cut_points(y$value, N)

  # quants <- stats::quantile(
  #   y$value[y$value > 0],
  #   probs = seq(0, 1, length.out = N),
  #   na.rm = TRUE,
  #   type = 3
  #   ) #%>% print
  ytemp <- y[y$value %in% cutted,]
  #ytemp$rei %>% table
  #ytemp$cause %>% table
  more_reis <- ysaved[!ysaved$rei %in% ytemp$rei,]
  if (length(unique(ytemp$rei)) <= 2) {
    ytemp <- dplyr::bind_rows(
      ytemp,
      dplyr::sample_n( more_reis, min(3-length(unique(ytemp$rei)),nrow(more_reis)) )
    )
  }
  more_causes <- ysaved[!ysaved$cause %in% ytemp$cause,]
  if (length(unique(ytemp$cause)) <= 2) {
    #browser()
    ytemp <- dplyr::bind_rows(
      ytemp,
      dplyr::sample_n( more_causes, min(3-length(unique(ytemp$cause)),nrow(more_causes)) )
    )
  }

  y <- ytemp

  y <- dplyr::arrange(y, dplyr::desc(value))
  y <- dplyr::distinct(y, value, .keep_all = TRUE)

  y$summary <- paste0(
    toupper_1(y$rei)," in ",y$cause," ",pretty_location(y$location,add_in = TRUE) #" in ",y$location
  )

  #browser()
  # Since the graph won't show behavior, we don't want pseudo duplicates but with a different value:
  y$ID <- paste0(
    y$summary, " - ", y$nudge
  )
  y <- y[!duplicated(y$ID),]

  y$summary <- make.unique(y$summary, sep = " - ")
  #y$summary_duplicated <- duplicated(y$summary)

  # adjectivize <- function(x)
  #browser()

  plt <- ggplot2::ggplot(
    y,
    ggplot2::aes(x = stats::reorder(summary, value), y = value, fill = nudge, label = pretty_nr(value, always_vector = TRUE))
  ) +
    ggplot2::geom_col(position = "dodge2") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=lower, ymax=upper), width=.2,# position_dodge2(preserve = "single")
                  position = "dodge2") +
    ggplot2::geom_text(position = ggplot2::position_dodge(width = .9),    # move to center of bars
              vjust = -0.5,    # nudge above top of bar
              size = 3) +
    ggplot2::scale_x_discrete(labels = function(x){stringr::str_remove(x," -.*")}) +
    ggplot2::scale_y_continuous(labels = function(x){prettyNum(x, big.mark=",")}) +
    ggplot2::scale_fill_discrete(labels = toupper_1) +
    ggplot2::labs(x = NULL, y = toupper_1(measure), fill = "Nudge") +
    ggplot2::coord_flip() +
    ggplot2::labs(caption = stringr::str_wrap("Note: All both sexes. For dietary risks, the behavior category for the nudge effect is always food. For nondietary risks, the behavior category is health."))
  print(plt)
  invisible(y)

}#; compare_effects(sex = "both"); compare_effects(cause = "cardiovascular diseases")

if (FALSE) {

  compare_effects(rei = "all risk factors", cause = "all causes", measure = "deaths", country = NA,
                  gbdeffects = gbd_effects %>%
                    filter(
                      cause %in% unlist(choices_cause),
                      rei %in% unlist(choices_rei),
                      location %in% unlist(choices_location)
                    ))
}

# Todo: enter an effect, and get comparisons for that effect

#gbd_effects %>% names



# Other -------------------------------------------------------------------


#' Get pre-installed `better` vignettes
#'
#' Open the HTML files of pre-installed vignettes.
#'
#' Some `better` vignettes require things that you may not had set up yet at the time of installing `better`. Since such vignettes can't be built at the time of installation (which is how regular vignettes are created), they are pre-installed as HTML files. Use `better_vignette` to access them. Strictly speaking, they aren't vignettes. You can't access them with `vignette(*, package = "better")`. But they are articles that fulfill the role of typical vignettes. Note that you can also use `better_vignette()` for true vignettes that are also accessible with `vignette(*, package = "better")`. NOTE: UNDER DEVELOPMENT, ONLY ONE VIGNETTE SO FAR.
#'
#' @param ... The name(s) of the vignette(s) you want to read, unquoted. By default, if you don't specify anything, it's all the vignettes. Options are: `r paste(stringr::str_remove(list.files(system.file("vignettes_htmls", package = "better")),".html$"), collapse = ", ")`.
#'
#' @return Names of the vignette(s), invisible.
#' @export
#'
#' @examples
#' \dontrun{
#' better_vignette()
#' better_vignette(better)
#' }
better_vignette <- function(...) {
  arg <- rlang::ensyms(...)
  if (length(arg) > 0) {
    arg <- unname(purrr::map_chr(arg, rlang::as_string))
  } else {
    #browser()
    arg <- stringr::str_remove(
      list.files(system.file("vignettes_htmls", package = "better")),
      ".html$"
    )
    arg <- unique(c("better", arg)) # start with better, so that's the page that will be open
    #arg <- "better"
  }
  #browser()
  for (i in rev(arg)) {
    #browser()
    namei <- paste0(i,".html")
    filei <- system.file("vignettes_htmls", namei, package = "better")
    if (filei == "") {
      warning("The following file not found in vignettes_htmls: ", namei)
    } else {
      system2(
        "open",
        shQuote(filei)
      )
    }
  }
  invisible(arg)
}#; better_vignette()#; better_vignette(`better.demo`, better)


