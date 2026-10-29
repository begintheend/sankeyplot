# sankeyplot

Draws the stacked Sankey diagram from the Excel workbook
"Stacked_column_stroke_blacks_noAF.xlsx" with ggplot2, with or without the
white space between bands and between bars and ribbons. Includes the stroke
risk-factor data from the workbook and the write-up as a vignette.

## Installation

```r
# install.packages("remotes")
remotes::install_github("begintheend/sankeyplot", build_vignettes = TRUE)
```

## Usage

```r
library(sankeyplot)

sankey_plot(stroke_blacks, title = "Risk Factors for Stroke in Blacks")
sankey_plot(stroke_blacks, gap = 0, white_lines = FALSE)  # no white space

vignette("sankeyplot")  # the full write-up
```
