library(googlesheets4)
library(googledrive)

gs4_auth(cache = ".secrets", email = "katiemurenbeeld@boisestate.edu")

folder_url <- "https://drive.google.com/drive/u/0/folders/1ob5sagTtT3svhc7ZKeemd9TiAq1_MsCL"
folder <- drive_get(as_id(folder_url))

gdrive_files <- drive_ls(folder)
id <- gdrive_files[gdrive_files$name == "New Article Coding Framework", ]$id
drive_download(id, path = "data/original/new_codes.csv", overwrite = TRUE)
