# Getting started in github and R studio

# install.package('pak') # step 1. Lets install pak to help us manage installed packages
# # Details can be found here: https://pak.r-lib.org/

# # Install devtools, usethis, credentials, cli
# pak::pkg_install("devtools")
# pak::pkg_install("usethis")
# pak::pkg_install("gert")
# pak::pkg_install("credentials")
# pak::pkg_install("cli")

# # Check on your Git set up.  We will use a helper function I wrote for the class
devtools::source_url('https://raw.githubusercontent.com/chrischizinski/class_helper/main/git_checkR.R')

check_git_installation()

# Sign-in to your GitHub account
# Read: https://usethis.r-lib.org/articles/articles/git-credentials.html
# Read: https://usethis.r-lib.org/articles/articles/usethis-setup.html
# usethis::create_github_token()
# gitcreds::gitcreds_set()
# gh::gh_whoami()
# usethis::gh_token_help()
# usethis::use_git_config(user.name = "Jane Doe", user.email = "jane@example.com")
# usethis::gh_token_help()

##. Lets create
# # Start a new R Project
# # Name it getting_to_know_git
library(usethis)
use_git()          # initialize a git repo
use_git_ignore(".DS_Store")
use_readme.Rmd()
use_ccby_license()
# Before next step  → stage & commit changes
use_github()       # Connect local repo to GitHub


