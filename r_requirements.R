# This file is used to record R packages used by the project.
#
# We did look at using `renv` to manage this automatically but it added a layer of
# complexity and unexpected issues and workflow problems and so we have dropped it for
# now.

# This is required for R support in VSCode
library(languageserver)

# This is required to use R with Jupyter
library(IRkernel)

# This is required for some of the dataframe transformations
library(tibble)

# This is required for reading and writing Excel data
library(openxlsx2)
