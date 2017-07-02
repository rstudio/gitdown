
#' Copy a repository from GitHub
#'
#' @param github_repo The user and repository name
#' @param copy_to Subfolder in the project where the repo will be copied to. Defaults to: "repos"
#' @examples
#' git_copy_repo("tidyverse/dbplyr")
#'
#' git_copy_file("dbplyr/vignettes/dbplyr.Rmd",
#'              "content/dplyr.Rmd")
#'
#' git_replace_text("content/dplyr.Rmd",
#'              "Introduction to dbplyr",
#'              "Databases using dplyr" )
#'
#' git_copy_file("dbplyr/vignettes/sql-translation.Rmd",
#'              "content/translation.Rmd")
#'
#' git_remove_lines("content/translation.Rmd", 247, 248)
#'
#' @export
git_copy_repo <- function(github_repo, copy_to = "repos"){
  project_name <- strsplit(github_repo, "/")
  project_name <- project_name[[1]][2]
  repo_path <- file.path(copy_to, project_name)
  unlink(file.path(repo_path), recursive = TRUE)
  system(paste0("git clone https://github.com/", github_repo, " -b master ", repo_path))
}

#' Copy an article from a copied repo
#'
#' @param from The name of the file to be copied
#' @param to The destination path
#' @param copy_to Subfolder in the project where the repo will be copied to. Defaults to: "repos"
#' @examples
#' git_copy_repo("tidyverse/dbplyr")
#'
#' git_copy_file("dbplyr/vignettes/dbplyr.Rmd",
#'              "content/dplyr.Rmd")
#'
#' git_replace_text("content/dplyr.Rmd",
#'              "Introduction to dbplyr",
#'              "Databases using dplyr" )
#'
#' git_copy_file("dbplyr/vignettes/sql-translation.Rmd",
#'              "content/translation.Rmd")
#'
#' git_remove_lines("content/translation.Rmd", 247, 248)
#'
#' @export
git_copy_file <- function(from, to, copy_to = "repos"){
  from <- file.path(copy_to, from)
  to <- file.path(to)
  file.copy(from, to, overwrite = TRUE, recursive = TRUE)
}

#' Replaces text inside a file
#'
#' @param location Path to the file to be modified
#' @param lookfor The text to be found
#' @param replacewith The text to be used instead
#' @examples
#' git_copy_repo("tidyverse/dbplyr")
#'
#' git_copy_file("dbplyr/vignettes/dbplyr.Rmd",
#'              "content/dplyr.Rmd")
#'
#' git_replace_text("content/dplyr.Rmd",
#'              "Introduction to dbplyr",
#'              "Databases using dplyr" )
#'
#' git_copy_file("dbplyr/vignettes/sql-translation.Rmd",
#'              "content/translation.Rmd")
#'
#' git_remove_lines("content/translation.Rmd", 247, 248)
#'
#' @export
git_replace_text <- function(location, lookfor, replacewith){
  read_in_file <- readLines(location)
  read_in_file <- gsub(lookfor, replacewith, x = read_in_file, ignore.case = TRUE)
  write(read_in_file, location)
}

#' Removes a range numbered lines from a file
#'
#' @param location Path to the file to be modified
#' @param start First line to be removed
#' @param end Last line to be removed
#' @examples
#' git_copy_repo("tidyverse/dbplyr")
#'
#' git_copy_file("dbplyr/vignettes/dbplyr.Rmd",
#'              "content/dplyr.Rmd")
#'
#' git_replace_text("content/dplyr.Rmd",
#'              "Introduction to dbplyr",
#'              "Databases using dplyr" )
#'
#' git_copy_file("dbplyr/vignettes/sql-translation.Rmd",
#'              "content/translation.Rmd")
#'
#' git_remove_lines("content/translation.Rmd", 247, 248)
#'
#' @export
git_remove_lines <- function(location, start, end){
  read_in_file <- readLines(location)
  read_in_file <- c(read_in_file[1:(start - 1)], read_in_file[(end + 1) : length(read_in_file)])
  write(read_in_file, location)
}

#' @export
git_list_files <- function(location, pattern = NULL){
  file_list <-list.files(location)
  if(!is.null(pattern)) file_list <- file_list[grep(pattern, file_list)]
  file_list
}

#' @export
git_clear_site_folder <- function(subfolder){
  unlink(file.path(git_root_folder(), "content", subfolder, "*.*"), recursive = TRUE)
  unlink(file.path(git_root_folder(), "static", subfolder, "*.*"), recursive = TRUE)
}

#' @export
git_root_folder <- function(path = rprojroot::find_rstudio_root_file()){
  path
}

#' @export
git_reference_md <- function(){
  reference_folder <- file.path(git_root_folder(), "content/reference")
  file_list <- file.path(reference_folder, list.files(reference_folder))
  file_list <- file_list[!grepl("_index", file_list)]
  file_split <- strsplit(file_list, "/")
  max_split <- sapply(file_split, function(x)length(x))
  folder_name <- sapply(1:length(file_split), function(x)file_split[[x]][max_split[[x]]])

  folder_name <- substr(folder_name, 1 , nchar(folder_name) - 3)


  reference_headers <- sapply(file_list,
         function(x)readLines(x)[1])
  end_title <- as.integer(regexpr("`: ", reference_headers))
  title <- as.character(substr(reference_headers, 4, end_title - 1))

  preview <- as.character(substr(reference_headers, end_title + 3, nchar(reference_headers)))

  ref_page <- paste0("<h1>Function Reference</h1>")
  ref_page <- c(ref_page, paste0("<p>[", title, "]", "(", folder_name , ") - ", preview))

  writeLines(ref_page, file.path(reference_folder, "_index.md"))

}


#' @export
git_docs_md <- function(){
  reference_folder <- file.path(git_root_folder(), "content/documents")
  file_list <- file.path(reference_folder, list.files(reference_folder))

  file_list <- file_list[!grepl("_index", file_list)]

  file_split <- strsplit(file_list, "/")
  max_split <- sapply(file_split, function(x)length(x))
  folder_name <- sapply(1:length(file_split), function(x)file_split[[x]][max_split[[x]]])

  folder_name <- substr(folder_name, 1 , nchar(folder_name) - 3)



  reference_headers <- sapply(file_list,
                              function(x)readLines(x)[1])

  title <- as.character(reference_headers)


  ref_page <- paste0("<h1>Articles</h1>")
  ref_page <- c(ref_page, paste0("<p>[", title, "]", "(", folder_name , ")"))

  writeLines(ref_page, file.path(reference_folder, "_index.md"))

}
