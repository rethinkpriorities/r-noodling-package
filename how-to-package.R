# Update ------------------------------------------------------------------

# Update documentation and (re)install the package
devtools::document()
devtools::install()
# install.packages("roxygen2")
# library("roxygen2")

detach("package:rnoodling", unload = TRUE)
library(rnoodling)

# Restart sessions --------------------------------------------------------

.rs.restartR()
library(rnoodling)

# Create a vignette -------------------------------------------------------

# usethis::use_vignette("")

# Add a data set ----------------------------------------------------------

# usethis::use_data()

# CRAN submission ---------------------------------------------------------

# Check examples
# devtools::run_examples()

# Check package
# devtools::load_all()
#devtools::check()
#devtools::check(args = c('--run-donttest')) # Without examples test
#devtools::check(args = c('--as-cran'))

# run R CMD check on CRAN’s servers
#devtools::check_win_devel()
#devtools::check_win_release()

# Build tar
#devtools::build()
