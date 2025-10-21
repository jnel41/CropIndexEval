# -----------------------------------------------------------------
#
#   Demo improvement categories for Jessica Stephenson | Oct 21, 2025
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

ia_CPI2 <- ia_CPI |> filter(NCCPIx100 < 50) |> 
  dplyr::mutate(average = (OCProdIdx + NCCPIx100)/2, 
                difference = (OCProdIdx - NCCPIx100))

# look at != drain ----

ia_CPI2 |> filter(DrainCls != DrainClsWet) |> count(DrainCls, DrainClsWet)

ia_CPI2 |> select(DrainCls) |> unique() 
ia_CPI2 |> select(DrainClsWet) |> unique() 

# could make a factor ----

ia_CPI2 <- ia_CPI2 |> 
  mutate(DrainCls = factor(DrainCls, ordered = TRUE, 
                           levels = c("Excessively drained", "Well drained")))

# how to check it's a factor 
unique(ia_CPI2$DrainCls)

# and then mutate it with ifelse

ia_CPI2 <- ia_CPI2 |> # less than or equal 
  mutate(drain_intervention = ifelse(DrainCls <= DrainClsWet, "Yes", "No"))

ia_CPI2 <- ia_CPI2 |> # or just not equal 
  mutate(drain_equal = ifelse(DrainCls != DrainClsWet, "Different", "Equal"))


ia_CPI2 <- ia_CPI2 |> # or not equal by true false 
  # (I'd avoid this since above is easier to remember)
  mutate(drain_equal = DrainCls != DrainClsWet) 

# or use case_when ----

# go by cases 
ia_CPI2 |> 
  mutate(drain_intervention = case_when(
    DrainCls == DrainClsWet ~ "No", 
    DrainCls == "Well drained" & DrainClsWet == "Excessively drained" ~ "Yes", 
                              ))

# check ----
ia_CPI2 |> select(DrainCls, DrainClsWet, new_var) |> unique() 

# you can also use count() and plots to check it's done correctly 

