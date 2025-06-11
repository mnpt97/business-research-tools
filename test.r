# Load libraries

rm(list = ls())
library(ggplot2)
library(gridExtra)

# Create the data frame
df <- data.frame(
  Feature1 = c(0, 1, 2, 3, 4),
  Feature2 = c(10, 12, 15, 14, 13),
  Feature3 = c(5, 7, 6, 5, 8),
  Feature4 = c(1, 4, 3, 2, 4),
  sensor4 = c(20, 21, 19, 22, 20)
)

# Plot configuration
plot_config <- list(
  list(x = "Feature1", y = "Feature2"),
  list(x = "Feature1", y = "Feature3"),
  list(x = "Feature1", y = "Feature4"),
  list(x = "Feature2", y = "Feature3"),
  list(x = "Feature2", y = "Feature4"),
  list(x = "Feature3", y = "Feature4")
)


num_plots <- length(plot_config)
cols <- 2
rows <- ceiling(num_plots / cols)

# Create a list of ggplot objects
plots <- lapply(plot_config, function(cfg) {
  x <- df[[cfg$x]]
  y <- df[[cfg$y]]
  corr <- round(cor(x, y), 2)
  ggplot(df, aes_string(x = cfg$x, y = cfg$y)) +
    geom_point() +
    labs(title = paste("Correlation =", corr),
         x = cfg$x, y = cfg$y) +
    theme_minimal()
})

# Fill in empty plots if needed
if (length(plots) < rows * cols) {
  for (i in 1:(rows * cols - length(plots))) {
    plots[[length(plots) + 1]] <- ggplot() + theme_void()
  }
}

# Display the plots in a grid
do.call("grid.arrange", c(plots, ncol = cols))
