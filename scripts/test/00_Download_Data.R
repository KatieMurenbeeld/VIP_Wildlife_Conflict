# Set up to append to google sheet

library(googlesheets4)
# In console run:
# gs4_auth(email = "katiemurenbeeld@boisestate.edu", cache = ".secrets")
# usethis::use_git_ignore(".secrets")
# usethis::use_git_ignore("*/.secrets")

sheet_id <- "https://docs.google.com/spreadsheets/d/1EFbr-GahLJ0Hl01YYheOAjRny8GFxHryFjnD5RNbQUY/edit#gid=0"
