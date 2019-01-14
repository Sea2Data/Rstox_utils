devtools::install_github("Sea2Data/Rstox", ref="develop")
# Path to eca R package folder, which must be retrieved from the internal git of NR, which must be done by AJ or Edvin, and copied with a memory stick to e.g. Atle:
checkout <- "/Users/a5362/code/nr/ecatest/Test/reca_Version0.10"

## Install new library
install.packages(checkout,repos = NULL, type="source")
