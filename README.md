TwinFish is a package containing a function that allows you to run a dynamic spatial age-based population model for a fish species.

This package uses a combination of R and C++ code and therefore requires a code compiler. If your computer does not have a native C++ compiler one must be isntalled before the functions will run.

To install TwinFish from GitHub, first ensure you have the 'devtools' package, otherwise install using install.packages("devtools'). Then, use:

library(devtools) devtools::install_github("CharlotteLAston/TwinFish", build_vignettes=TRUE)

If you already have a version of the TwinFish package installed but want to update the package, usie the line of code below to ensure the updated version is installed. 
devtools::install_github("CharlotteLAston/TwinFish", build_vignettes=TRUE, force=TRUE)
