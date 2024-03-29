% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gbd_clean.R
\name{gbd_save}
\alias{gbd_save}
\title{Save Global Burden of Disease dataset}
\usage{
gbd_save(path, load = TRUE)
}
\arguments{
\item{path}{Path of the GBD folder, where the data are stored. By default, automatically read from options, namely, \code{better.gbd_path}, ideally set in .Rprofile. To learn more, call \code{better_vignette(better)}.}

\item{load}{Whether or not to load the GBD dataset as object \code{gbd}.}
}
\description{
Save the GBD dataset as an RDS file. \code{gbd_save()} looks for the raw GBD data files and codebook files in subfolders of your GBD root folder, as specified in \code{getOption("better.gbd_path")}. It cleans the data and codebooks, and saves it as a \code{gbd_dataset} object in your root GBD folder.
}
\details{
Several \code{better} functions look for the GBD dataset in your GBD root folder, specified in \code{getOption("better.gbd_path")}. If they don't find the proper RDS file, these functions automatically try to call \code{gbd_save()}. However, you may also want to manually run \code{gbd_save()}, for instance, after you have downloaded additional data.
}
\examples{
\dontshow{
NULL
}
\dontrun{
gbd_save()
}
}
