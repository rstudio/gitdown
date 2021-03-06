% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gitdown.R
\name{git_copy_file}
\alias{git_copy_file}
\title{Copy an article from a copied repo}
\usage{
git_copy_file(from, to, copy_to = "repos")
}
\arguments{
\item{from}{The name of the file to be copied}

\item{to}{The destination path}

\item{copy_to}{Subfolder in the project where the repo will be copied to. Defaults to: "repos"}
}
\description{
Copy an article from a copied repo
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
