##############################################
######### install dependencies ###############
##############################################
install.packages("remotes")
install.packages("devtools")
# install.packages("tidyverse")
devtools::install_version("tidyverse", version = "1.3.0")
install.packages("Hmisc")
remotes::install_github("paulc91/shinyauthr")

