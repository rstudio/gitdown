#' @export
git_build <- function(repo, from = "", to = from, repos = "repos", reset_folder = TRUE){
  origin <- file.path(repos, repo, from)
  if(reset_folder)git_clear_site_folder(to)
  git_move_content(origin, to)
}
#' @export
git_build_vignettes <- function(repo = NULL){
  git_build(repo, from = "vignettes")

}
#' @export
git_build_reference <- function(repo = NULL){
  git_build(repo, from = "man", to = "reference")
  git_reference_md()
}

#' @export
git_build_root <- function(repo = NULL){
  git_build(repo)
}
#' @export
git_build_docs <- function(repo = NULL){
  git_build(repo, from = "docs", to = "documents")
  git_docs_md()
}
#' @export
git_build_news <- function(repo = NULL, repos = "repos"){
  git_move_content(file.path("repos", repo), pattern = "NEWS")
}
#' @export
git_build_readme <- function(repo = NULL, repos = "repos"){
  git_move_content(file.path("repos", repo), pattern = "README")
}
#' @export
git_build_site <- function(repo = NULL){
  git_build_news(repo)
  git_build_reference(repo)
  git_build_readme(repo)
}
