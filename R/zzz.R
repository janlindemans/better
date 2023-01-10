.onLoad <- function(libname, pkgname) {


}
better_options <- function() {
  opts <- stringr::str_subset(names(options()), "^better\\.")
  options()[opts]
}
.onAttach <- function(libname, pkgname) {
  opts <- better_options()
  if (length(opts) > 1) {
    opts_pr <- paste0(
      "Global options set:\n",
       stringr::str_replace_all(paste0(utils::capture.output(better_options()),collapse="\n"),"\n\n","\n")
    )
  } else {
    opts_pr <- "No global options set for this package.\n"
  }
  packageStartupMessage(
    "Welcome to the better package!\n\n",
    opts_pr,
    "\nTo learn more, read the vignette with `better_vignette(better)`."
  )
}
