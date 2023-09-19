# Download csv from google drive

## Download Data from Lab Google Drivelibrary(googledrive)#this bit will open an authorization window in Google Drive asking if you want to allow RStudio to access your folders. You have to click yes and then it will store your credentials locally
options(  gargle_oauth_cache = ".secrets",  gargle_oauth_email = TRUE)

#Assign the link for the GDrive folder to an object
folder_url <- "https://docs.google.com/spreadsheets/d/1EFbr-GahLJ0Hl01YYheOAjRny8GFxHryFjnD5RNbQUY/edit#gid=0"

#drive_get will open the authorization window; you have to give Rstudio access to edit files once you do that you'll get the necessary attributes for the GDrive folder
folder <- drive_get(as_id(folder_url))

#drive_ls lists all files in the folder. Note that if there are subfolders, each folder has to be processed separately
gdrive_files <- drive_ls(folder)
#lapply steps through each file in the gdrive_list and downloads them to data/original/
lapply(gdrive_files$id, function(x) drive_download(as_id(x),                                                   
                                                   path = paste0(here::here("data/original/"), 
                                                                 gdrive_files[gdrive_files$id==x,]$name), 
                                                   overwrite = TRUE))