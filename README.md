# reviz

Visualize relationships between variables with model overlays.

## Installation

```r
# Install from GitHub (development version)
install.packages("devtools")  # if needed
devtools::install_github("yourusername/reviz")

library(reviz)
library(ggplot2)

# Basic usage with built-in data
reviz_plot(mtcars, "wt", "mpg")

Using with `critstats` data sets

# Install critstats from GitHub
install.packages("remotes")
remotes::install_github("professornaite/critstats", force = TRUE)

# Load both packages
library(critstats)
library(reviz) 

# Use any dataset from critstats
res <- reviz_plot(
  data = your_dataset_name,  # replace with actual dataset name
  x = "your_x_column",
  y = "your_y_column"
)

res  # shows the plot

Function reference

reviz_plot(
  data,     # data.frame with your variables
  x,        # x-axis variable name (string)
  y         # y-axis variable name (string)
)

Returns a ggplot2 object with scatterplot + linear model overlay
=======
A package to visualize relationships and model fits
