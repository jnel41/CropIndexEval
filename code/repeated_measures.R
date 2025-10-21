# -----------------------------------------------------------------
#
#   Tukey Mean-diff plot for Jessica Stephenson | Oct 9, 2025
#   Rebekah Scott | Statistical Consulting Manager
#   Department of Statistics, Iowa State University 
#
# -----------------------------------------------------------------

library(dplyr)
library(ggplot2)

# data ----

ia_CPI <- readr::read_csv("data/raw/ia_cpi.csv") |> 
  mutate(NCCPIx100 = NCCPIall * 100,
         cpi_diff = abs(OCProdIdx - NCCPIx100),
         state = "IA")

# look at repeated measures ----

colnames(ia_CPI)

length(unique(ia_CPI$mukey)) # 9930

