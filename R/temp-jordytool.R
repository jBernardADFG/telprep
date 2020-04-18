folder.to.R <- function(project_directory){
  destination_dir <- paste(project_directory, "R/", sep="")
  main_folder <- paste(project_directory, "Rfolder/", sep="")
  files <- list.files("D:/Jordy/telprep/telprep/Rfolder", recursive = T)
  for (file in files){
    file_dir <- paste(main_folder, file, sep="")
    file.copy(file_dir, destination_dir, overwrite = T)
  }
}
project_directory <- "D:/Jordy/telprep/telprep/"
folder.to.R(project_directory)
