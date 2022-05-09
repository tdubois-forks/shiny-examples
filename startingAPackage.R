# 1. CHECK IF THIS NAME IS AVAILABLE
# library(available)
# available("lynchlab") IT IS

# 2. CREATE THE PACKAGE
usethis::create_package("C:/r_pkg_dev/lynchlab") # THAT POPPED OPEN A NEW RSTUDIO PROJECT. I'LL KEEP DOING STUFF IN HERE TO DOCUMENT MY PROCESS WHERE POSSIBLE/APPROPRIATE?

### THIS IS WHAT i DID IN THE NEW PROJECT

# > library(devtools)
# Loading required package: usethis
# > use_git()
# fatal: unable to access 'C:\Program Files\R\R-4.2.0<br>/.config/git/config': Invalid argument
# fatal: unable to access 'C:\Program Files\R\R-4.2.0<br>/.config/git/config': Invalid argument
# Failed to call 'git help -a'
# v Setting active project to 'C:/r_pkg_dev/lynchlab'
# v Initialising Git repo
# v Adding '.Rhistory', '.Rdata', '.httr-oauth', '.DS_Store' to '.gitignore'
# There are 5 uncommitted files:
#   * '.gitignore'
# * '.Rbuildignore'
# * 'DESCRIPTION'
# * 'lynchlab.Rproj'
# * 'NAMESPACE'
# Is it ok to commit them?
#
#   1: I agree
# 2: Negative
# 3: Not now
#
# Selection: 1
# v Adding files
# v Making a commit with message 'Initial commit'
# * A restart of RStudio is required to activate the Git pane
# Restart now?
#
#   1: Nope
# 2: No
# 3: Yup

# Selection: 3
