# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

#golem::set_golem_wd("C:/Users/fabio/Documents/OliveHealthR")

# Document and reload your package
golem::document_and_reload()


# Run the application
run_OliveHealthR()
#run_app()
