#' @import Rd2md
#' @export
git_move_content <- function(origin, destination = "", convert_rd = TRUE, exclude = c("\\.yml", "\\.Rproj"), pattern = NULL ){

  origin <- file.path(git_root_folder(), origin)

  destination_static <- file.path(git_root_folder(), "static", destination)
  destination_content <- file.path(git_root_folder(), "content", destination)

  if(!file.exists(destination_static))dir.create(destination_static)
  if(!file.exists(destination_content))dir.create(destination_content)

  all_files <- git_list_files(origin, pattern = pattern)

  if(length(exclude) > 0){
    invisible({
       for(i in 1:length(exclude)) all_files <- all_files[!grepl(exclude[i], all_files)]
      })}


  md_files <- all_files[grepl("\\.md", all_files)]
  all_files <- all_files[!grepl("\\.md", all_files)]
  file.copy(file.path(origin, md_files),
            file.path(destination_content, md_files))

  rmd_files <- all_files[grepl("\\.Rmd", all_files)]
  all_files <- all_files[!grepl("\\.Rmd", all_files)]


  md_filenames <- substr(md_files, 1 , nchar(md_files) - 3)

  if(length(md_files) > 0){
      for(i in 1:length(md_files)){
        rmd_files <- rmd_files[!grepl(md_filenames[i], rmd_files)]
      }

  }

  file.copy(file.path(origin, rmd_files),
            file.path(destination_content, rmd_files))

  if(convert_rd){
    rd_files <- all_files[grepl("\\.Rd", all_files)]
    if(length(rd_files) > 0){
      md1_files <- gsub("\\.Rd","\\.md", rd_files)

      all_files <- all_files[!grepl("\\.Rd", all_files)]

      invisible(sapply(1:length(rd_files),
                       function(x)convert_rd(x, rd_files, md1_files, origin, destination_content)))

    }


  }


  new_files <- sub("_files","", all_files)
  if(length(all_files) > 0){
    invisible(file.copy(file.path(origin, all_files), file.path(destination_static)))
    invisible({
      for(i in 1:length(all_files)){
        system(paste(
          "cp -R ",
          file.path(origin, all_files[i]),
          file.path(destination_static, new_files[i])))
      }
    })
  }


  }


convert_rd <- function(x, rd_files, md1_files, origin, destination){
  print(rd_files[x])
  tryCatch({
    Rd2md::Rd2markdown(file.path(origin, rd_files[x]), file.path(destination, md1_files[x]))
  }, error = function(e){ print(paste0("   - " ,rd_files[x] , " - error" )) })
}
