library(shiny)
library(qfieldcloudR)
library(magrittr)

# config
config <- config::get()

# qfieldcloud
qfieldcloud_url <- config$qfieldcloud

# waiter screens

# waiting screen spinners
logging_in_screen <- tagList(
  waiter::spin_loader(),
  h4("Logging in...")
)

# waiting screen spinners
download_screen <- tagList(
  waiter::spin_loader(),
  h4("Downloading...")
)

updating_screen <- tagList(
  waiter::spin_loader(),
  h4("Updating Collaborators...")
)

adding_screen <- tagList(
  waiter::spin_loader(),
  h4("Adding Collaborators...")
)

deleting_screen <- tagList(
  waiter::spin_loader(),
  h4("Deleting Collaborators...")
)

updating_user_status_screen <- tagList(
  waiter::spin_loader(),
  h4("Syncing Collaborators Status...")
)