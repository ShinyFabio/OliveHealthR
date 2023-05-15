# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
golem::fill_desc(
  pkg_name = "OliveHealthR", # The Name of the package containing the App 
  pkg_title = "A Shiny App for statistical and graphical analysis of data from the Olivehealth project", # The Title of the package containing the App 
  pkg_description = "OliveHealthR analyzes and maps all the information obtained from the OliveHealth project. The planned activities will concern the mapping of the olive groves of the 5 Campania provinces (Italy) and the analysis of the health component (eg polyphenols) on samples such as leaves, drupes and oil.", # The Description of the package containing the App 
  author_first_name = "Fabio", # Your First Name
  author_last_name = "Della Rocca", # Your Last Name
  author_email = "fabiodellarocca94@gmail.com", # Your Email
  repo_url = "https://github.com/ShinyFabio/OliveHealthR.git" # The URL of the GitHub Repo (optional) 
)     

## Set {golem} options ----
golem::set_golem_options()

#set package version
golem::set_golem_version(
  version = "1.0.2",
  talkative = TRUE
)

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license( "Fabio Della Rocca" )  # You can set another license here
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
usethis::use_news_md( open = FALSE )

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::remove_favicon()
golem::use_favicon(path = "C:/Users/fabio/Downloads/OliveHealthRfavicon.ico") # path = "path/to/ico". Can be an online file. 

## Add helper functions ----
golem::use_utils_ui()
golem::use_utils_server()

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

