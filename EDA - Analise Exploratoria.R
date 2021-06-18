wine <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";")

library(dplyr)

DataExplorer::create_report(wine,
                            config = configure_report(
                            add_plot_density = TRUE
                            )
                            )

help(create_report)

visdat::vis_dat(wine,sort_type = FALSE)

funModeling::plot_num(wine)


dlookr::plot_outlier(wine)


winenum <- wine %>% 
  select(where(is.numeric))

PerformanceAnalytics::chart.Correlation(winenum, histogram = TRUE, pch = 15)

