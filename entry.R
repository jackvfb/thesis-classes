library(tidyverse)
library(identidrift)

varsWanted <- c("duration", "peakTime", "peak", "fmax_3dB", "fmin_3dB", "centerkHz_3dB",
                "BW_3dB", "Q_3dB", "fmax_10dB", "fmin_10dB", "centerkHz_10dB",
                "BW_10dB", "Q_10dB")

stats <- train.ec %>%
   select(species, varsWanted) %>%
   pivot_longer(cols=-species, names_to = "var") %>%
   group_by(species, var) %>%
   summarize(mean=mean(value), sd=sd(value), median=median(value),
             min=min(value), max=max(value)) %>%
   mutate_if(is.numeric, round, 2) %>%
   nest(data=-var) %>%
   mutate(data=map(data, \(x) as.matrix(column_to_rownames(x, var="species")))) %>%
   pull(data, name = "var")

stats$peak["ks", "mean"]
