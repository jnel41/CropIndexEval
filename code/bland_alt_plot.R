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

# ba plot by hand ----
ia_CPI2 <- ia_CPI |> filter(NCCPIx100 < 50) |> 
  dplyr::mutate(average = (OCProdIdx + NCCPIx100)/2, 
                difference = (OCProdIdx - NCCPIx100))

bland_altman_plot <-  ia_CPI2 |> ggplot(aes(x = average, y = difference)) + theme_bw() + 
  geom_point(alpha = 0.7, color = "grey5") +
  geom_hline(yintercept = mean(ia_CPI2$difference), color = "skyblue", linewidth = 0.5) +
  geom_hline(yintercept = mean(ia_CPI2$difference) -                                                                                                             
               1.96 * sd(ia_CPI2$difference), color = "darkgreen", linewidth = 0.5) +
  geom_hline(yintercept = mean(ia_CPI2$difference) + 
               1.96 * sd(ia_CPI2$difference), colour = "darkgreen", linewidth = 0.5) + 
  labs(title = "Bland-Altman Comparison IA CPI vs NCPPI", x = "Average", y = "Difference") 

bland_altman_plot # not super helpful 
# appears to have heteroskedasticity 


# blandr ----
bland_stats <- blandr::blandr.statistics(ia_CPI2$OCProdIdx, 
                                 ia_CPI2$NCCPIx100, sig.level=0.95)

bland_stats
summary(bland_stats)
blandr::blandr.draw(ia_CPI2$OCProdIdx, 
            ia_CPI2$NCCPIx100)

# BA plot looks the same, which is good 

# other plots: facet wrap demo ----
basic_plot <- ia_CPI |> filter(NCCPIx100 < 50) |> ggplot(aes(x = OCProdIdx, 
                                   y = NCCPIx100, color = cpi_diff)) +
  geom_point(alpha = 0.6) +
  labs(title = "CSR2 vs NCCPI", x = "CSR2", y = "NCCPI") 

basic_plot + facet_grid(Droughty ~ DrainCls)

# quick look at lm ----
crop_lm <- lm(OCProdIdx ~ NCCPIx100, data = ia_CPI2)
summary(crop_lm)

plot(crop_lm$fitted.values, crop_lm$residuals) # yep, non-constant variance 
qqnorm(crop_lm$residuals)
qqline(crop_lm$residuals)
