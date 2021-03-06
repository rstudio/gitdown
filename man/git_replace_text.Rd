% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gitdown.R
\name{git_replace_text}
\alias{git_replace_text}
\title{Replaces text inside a file}
\usage{
git_replace_text(location, lookfor, replacewith)
}
\arguments{
\item{location}{Path to the file to be modified}

\item{lookfor}{The text to be found}

\item{replacewith}{The text to be used instead}
}
\description{
Replaces text inside a file
}
\examples{
git_copy_repo("tidyverse/dbplyr")

git_copy_file("dbplyr/vignettes/dbplyr.Rmd",
             "content/dplyr.Rmd")

git_replace_text("content/dplyr.Rmd",
             "Introduction to dbplyr",
             "Databases using dplyr" )

git_copy_file("dbplyr/vignettes/sql-translation.Rmd",
             "content/translation.Rmd")

git_remove_lines("content/translation.Rmd", 247, 248)

}
