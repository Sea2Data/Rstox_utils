devtools::install_github("Sea2Data/Rstox", ref="develop")

#
# point to local checkout of public eca version (https://github.com/NorskRegnesentral/Reca)
#
checkout <- "/Users/a5362/code/github/public_reca"

## Install new library
install.packages(checkout,repos = NULL, type="source")
