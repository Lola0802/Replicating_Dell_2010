# DML RDD attempt
library(ggplot2)

ggplot(data_try, aes(x = d_bnd)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Distribution of Running Variable (d_bnd)",
       x = "Distance to Cutoff (d_bnd)", y = "Frequency")

# (1) Extending Table II's results (living standards)  -------
