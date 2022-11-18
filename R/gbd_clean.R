list_recursive_path_names <- function(x) {
  list.files(path = as.character(x), full.names = TRUE, recursive = TRUE)
  # as.character is needed because otherwise installing (but nothing else!!) throws an error that list.files has an invalid path argument. It's something weird with list.files: don't try to solve the bug. But as.character works.
}
#' Read Global Burden of Disease data
#'
#' Read the GBD data from your local folder. It looks for data files in all subfolders of the GBD folder and merges them into one dataframe.
#'
#' @param path Path of the parentfolder of the data.
#'
#' @return Dataframe containing the GBD data.
#' @export
#'
#' @examples
#' # Folder where you saved your GBD data:
#' GBD_PATH <- paste0(
#'   "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared/IRB Projects",
#'   "/Health Projects/Health Team/Public datasets on health",
#'   "/Global Burden of Disease Study Data"
#' )
#' gbd <- gbd_read(GBD_PATH)
#' gbd
#'
#' # Better set the default path in `options`,
#' # so you don't need to specify paths anymore:
#' options(better.gbd_path = GBD_PATH)
#' gbd <- gbd_read()
#' # Even better: specify the GBD path option in your .Rprofile file,
#' # so the path will be available accross sessions.
#'

#' gbd_meta <- gbd_codebook_read()
#' gbd_meta
#' gbd_meta <- gbd_codebook_clean(gbd_meta)
#' gbd_meta
#' attach(gbd_meta)
#' gbd_cb
#' gbd_cb_cause
#'
gbd_read <- function(path = as.character(getOption("better.gbd_path"))) {
  #browser()
  if (length(path) == 0) {
    stop("`path` has length 0.")
  }
  FILES_GBD_DATA <- stringr::str_subset(
    list_recursive_path_names(path),
    "/IHME-GBD_20\\d{2}_DATA-.+\\.csv"
  ) #%>% print
  if (length(FILES_GBD_DATA) == 0) {
    stop("No data files found in ", path, ".")
  }
  y <- tibble::tibble()
  for (i in FILES_GBD_DATA) {
    #browser()
    dfi <- readr::read_csv(i)
    y <- dplyr::bind_rows(
      y,
      dfi
    )
  }
  y <- dplyr::distinct(y)
  return(y)
}
#gbd <- gbd_read()


#' Clean Global Burden of Disease data
#'
#' Cleans the GBD data in a sensible way.
#'
#' @param x Dataframe of data to be cleaned.
#'
#' @return Dataframe with cleaned data.
#' @export
#'
#' @examples
#' GBD_PATH <- paste0(
#'   "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared/IRB Projects",
#'   "/Health Projects/Health Team/Public datasets on health",
#'   "/Global Burden of Disease Study Data"
#' )
#' gbd <- gbd_read(GBD_PATH) # better set default path in options (see ?gbd_read)
#' gbd <- gbd_clean(gbd)
#' gbd
gbd_clean <- function(x) {
  y <- x
  names(y) <- stringr::str_remove(names(y), "_name")
  y <-dplyr::select(y,-dplyr::ends_with("_id"), tidyselect::everything())
  return(y)
}#; gbd_clean(gbd)

#' Read Global Burden of Disease codebook
#'
#' Read the GBD codebook from your local folder. It looks for codebook files in all subfolders of the GBD folder and puts them into a list.
#'
#' @param path Path of the parentfolder of the codebooks.
#'
#' @return List with dataframes of GBD codebooks.
#' @export
#'
#' @examples
#' # Folder where you saved your GBD codebook:
#' GBD_PATH <- paste0(
#'   "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared/IRB Projects",
#'   "/Health Projects/Health Team/Public datasets on health",
#'   "/Global Burden of Disease Study Data"
#' )
#' gbd_meta <- gbd_codebook_read(GBD_PATH)
#'
#' # Better set the default path in `options`,
#' # so you don't need to specify paths anymore:
#' options(better.gbd_path = GBD_PATH)
#' gbd <- gbd_codebook_read()
#' # Even better: specify the GBD path option in your .Rprofile file,
#' # so the path will be available accross sessions.
gbd_codebook_read <- function(path = getOption("better.gbd_path")) {
  #browser()
  if (length(path) == 0) {
    stop("`path` has length 0.")
  }
  gbd_cb <- readr::read_csv(
    as.character(utils::tail(stringr::str_subset(
      list_recursive_path_names(path),
      "/IHME_GBD_20\\d{2}_CODEBOOK_Y\\d{4}M\\d{2}D\\d{2}\\.CSV"
    ),1))
  )

  gbd_cb_measure <- readxl::read_excel(
    as.character(utils::tail(stringr::str_subset(
      list_recursive_path_names(path),
      "/IHME_GBD_20\\d{2}_MEASURE_METRIC_DEFINITIONS_Y\\d{4}M\\d{2}D\\d{2}\\.XLSX"
    ),1)),
    skip=1
  )
  gbd_cb_risk <- readxl::read_excel(
    utils::tail(stringr::str_subset(
      list_recursive_path_names(path),
      "/IHME_GBD_20\\d{2}_REI_HIERARCHY_Y\\d{4}M\\d{2}D\\d{2}\\.XLSX"
    ),1)
  )
  gbd_cb_cause <- readxl::read_excel(
    utils::tail(stringr::str_subset(
      list_recursive_path_names(path),
      "/IHME_GBD_20\\d{2}_CAUSE_HIERARCHY_Y\\d{4}M\\d{2}D\\d{2}\\.XLSX"
    ),1)
  )

  y <- list(
    gbd_cb = gbd_cb,
    gbd_cb_cause = gbd_cb_cause,
    gbd_cb_measure = gbd_cb_measure,
    gbd_cb_risk = gbd_cb_risk
  )
}#; gbd_meta <- gbd_codebook_read()

#' Clean Global Burden of Disease codebook
#'
#' @param x List with codebooks to be cleaned.
#'
#' @return List with cleaned codebooks.
#' @export
#'
#' @examples
#' GBD_PATH <- paste0(
#'   "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared/IRB Projects",
#'   "/Health Projects/Health Team/Public datasets on health",
#'   "/Global Burden of Disease Study Data"
#' )
#' gbd_meta <- gbd_codebook_read(GBD_PATH) # better set default path in options (see ?gbd_read)
#' gbd_codebook_clean(gbd_meta)
gbd_codebook_clean <- function(x) {
  #browser()
  y <- x
  y$gbd_cb <- tibble::tibble(
    variable = stringr::str_remove(names(y$gbd_cb)[-1],"_name"),
    label = unname(unlist(y$gbd_cb[1,-1])),
    value_coding = purrr::map(y$gbd_cb[-1,-1], ~{.[!is.na(.)]}),
    variable_origi = names(y$gbd_cb)[-1]
  )
  names(y$gbd_cb_measure) <- snakecase::to_snake_case(names(y$gbd_cb_measure))
  names(y$gbd_cb_risk) <- snakecase::to_snake_case(names(y$gbd_cb_risk))
  names(y$gbd_cb_cause) <- snakecase::to_snake_case(names(y$gbd_cb_cause))
  return(y)
}#; gbd_codebook_clean(gbd_cb)

gbd_rdata_path <- function(path = getOption("better.gbd_path")) {
  paste0(path,"/gbd.RData")
}
#' Save Global Burden of Disease data and codebook
#'
#' @param path Path of the parentfolder of the raw data and raw codebook, where also the .RData file with the cleaned data and codebook will be saved.
#'
#' @return NULL
#' @export
#'
#' @examples
#' GBD_PATH <- paste0(
#'   "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared/IRB Projects",
#'   "/Health Projects/Health Team/Public datasets on health",
#'   "/Global Burden of Disease Study Data"
#' )
#' gbd_save(GBD_PATH) # better set default path in options (see ?gbd_read)
gbd_save <- function(path = getOption("better.gbd_path")) {
  gbd <- gbd_read(path = path)
  gbd <- gbd_clean(gbd)
  gbd_meta <- gbd_codebook_read(path = path)
  gbd_meta <- gbd_codebook_clean(gbd_meta)
  save(gbd, gbd_meta, file = gbd_rdata_path(path))
}

#' Load saved Global Burden of Disease data and codebook
#'
#' @param path Path of the folder of the .RData file.
#'
#' @return The value of `base::load`, which this function wraps.
#' @export
#'
#' @examples
#' GBD_PATH <- paste0(
#'   "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared/IRB Projects",
#'   "/Health Projects/Health Team/Public datasets on health",
#'   "/Global Burden of Disease Study Data"
#' )
#' gbd_load(GBD_PATH) # better set default path in options (see ?gbd_read)
gbd_load <- function(path = getOption("better.gbd_path")) {
  y <- new.env()
  load(paste0(path,"/gbd.RData"), envir = y)
  y <- as.list.environment(y)
  y <- y[c("gbd","gbd_meta")]
  return(y)
}



#' Download Global Burden of Disease data
#'
#' @param path Path of the parentfolder of the GBD data.
#'
#' @return URLs of webpages opened, invisible.
#' @export
#'
#' @examples
#' GBD_PATH <- paste0(
#'   "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared/IRB Projects",
#'   "/Health Projects/Health Team/Public datasets on health",
#'   "/Global Burden of Disease Study Data"
#' )
#' gbd_download(GBD_PATH) # better set default path in options (see ?gbd_read)
gbd_download <- function(path = getOption("better.gbd_path")) {
  GBD_SITES <- c(
    guide = "https://ghdx.healthdata.org/sites/default/files/ihme_query_tool/GBD_Results_Tool_User_Guide_2019.pdf",
    main = "https://ghdx.healthdata.org/gbd-2019",
    results = "https://vizhub.healthdata.org/gbd-results/"
  )
  message(
    "The page to download data and the user guide are opened in your browser. Your GBD data folder is also opened. Download the data you want, and save it somewhere in the GBD data folder or a subfolder. It doesn't matter where, as long as you don't change the name of the data file.\n\nWebpages opened:\n- ",
    paste0(purrr::map2_chr(
      GBD_SITES, names(GBD_SITES), ~{paste0(.y,": ",.x)}
      ),collapse="\n- "),
    "\n\nFolder opened:\n", path
  )
  gbd_open(path = path)
  purrr::walk(GBD_SITES, utils::browseURL)
  invisible(GBD_SITES)
}

#' Open the folder with the GBD data.
#'
#' Wrapper around `system2("open", path)`.
#'
#' @param path Path of the parentfolder of the GBD data.
#'
#' @return The value of `base::system2("open", path)`, which this function wraps.
#' @export
#'
#' @examples
#' GBD_PATH <- paste0(
#'   "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared/IRB Projects",
#'   "/Health Projects/Health Team/Public datasets on health",
#'   "/Global Burden of Disease Study Data"
#' )
#' gbd_open(GBD_PATH) # better set default path in options (see ?gbd_read)
gbd_open <- function(path = getOption("better.gbd_path")) {
  # OLD - NOT GOOD: If `path` does not exist, it is newly created.
  # if (!file.exists(path)) {
  #   warning("The folder ",path," foes not exist. Created a new folder with that path.")
  #   dir.create(path)
  # }
  system2("open", rev(shQuote(c(
    path
  ))))
}
