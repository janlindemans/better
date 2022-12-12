.onLoad <- function(libname, pkgname) {


}
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to the better package!",
    "\nTo learn more, read the vignette by calling `vignette(\"better\")`."
  )
}
