## ----setup, include=FALSE-----------------------------------------------------
library(GeometricMorphometricsMix)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
set.seed(123)

if (requireNamespace("MASS", quietly = TRUE)) {
  # Group A: smaller, more compact group
  grpA = MASS::mvrnorm(25, mu = rep(0, 8), Sigma = diag(8) * 0.5)
  # Group A: 25 observations, centered at origin, low variance
  
  # Group B: larger, more dispersed group  
  grpB = MASS::mvrnorm(40, mu = rep(2, 8), Sigma = diag(8) * 1.5)
  # Group B: 40 observations, shifted mean, higher variance
  
  # Group C: intermediate size and dispersion
  grpC = MASS::mvrnorm(30, mu = rep(-1, 8), Sigma = diag(8) * 1.0)
  # Group C: 30 observations, negative mean, intermediate variance
  
  # Combine data
  Data = rbind(grpA, grpB, grpC)
  groups = factor(c(rep("A", nrow(grpA)), rep("B", nrow(grpB)), rep("C", nrow(grpC))))
  # Combined dataset with group labels
  
  cat("Sample sizes:\n")
  table(groups)
  # Display sample sizes for each group
}

## -----------------------------------------------------------------------------
if (requireNamespace("MASS", quietly = TRUE)) {
  # Bootstrap multivariate variance
  boot_mv = disparity_resample(Data, 
                               group = groups, 
                               n_resamples = 1000,
                               statistic = "multivariate_variance",
                               bootstrap_rarefaction = "bootstrap",
                               CI = 0.95)
  # Bootstrap analysis of multivariate variance with 95% CI
  
  print(boot_mv)
  # Display formatted results with CI overlap assessment
  
  # Bootstrap mean pairwise Euclidean distance
  boot_ed = disparity_resample(Data,
                               group = groups,
                               n_resamples = 1000, 
                               statistic = "mean_pairwise_euclidean_distance",
                               bootstrap_rarefaction = "bootstrap")
  # Bootstrap analysis of mean pairwise Euclidean distances
  
  cat("\nMean pairwise Euclidean distance results:\n")
  boot_ed$results
  # Direct access to results table
}

## -----------------------------------------------------------------------------
if (requireNamespace("MASS", quietly = TRUE) && requireNamespace("geometry", quietly = TRUE)) {
  # Bootstrap convex hull volume
  rare_hull = disparity_resample(prcomp(Data)$x[,seq(3)],
                                 group = groups,
                                 n_resamples = 200,
                                 statistic = "convex_hull_volume",
                                 bootstrap_rarefaction = "rarefaction",
                                 sample_size = "smallest")
  # Rarefaction analysis of convex hull volume
  # Note: fewer resamples due to computational intensity, using the scores along
  # the first few principal components due to the potential issues with the convex hull and high dimensional data

  print(rare_hull)
  # Convex hull results - Group B should have largest volume
}

## ----fig.width=7, fig.height=5------------------------------------------------
  # Plot bootstrap multivariate variance results
   plot(boot_mv)
  # Plot rarefaction for convex hull volume
  plot(rare_hull) 
  
  
  cat("Plotting methods create ggplot2 confidence interval plots\n")
  cat("showing average values and confidence intervals for each group.\n")


## -----------------------------------------------------------------------------
if (requireNamespace("MASS", quietly = TRUE)) {
  # Test Groups A vs B (different variances expected)
  test_AB = disparity_test(grpA, grpB, perm = 999)
  # Permutation test between groups A and B
  
  cat("Groups A vs B comparison:\n")
  print(test_AB)
  # Results show observed values, differences, and p-values
  
  # Test Groups A vs C (more similar variances expected)  
  test_AC = disparity_test(grpA, grpC, perm = 999)
  # Permutation test between groups A and C
  
  cat("\nGroups A vs C comparison:\n")
  print(test_AC)
  # Compare groups with more similar dispersions
}

## -----------------------------------------------------------------------------
# Simulate univariate data
set.seed(456)
uni_A = rnorm(30, mean = 0, sd = 1)
# Group A: normal distribution, sd=1

uni_B = rnorm(35, mean = 0, sd = 2) 
# Group B: normal distribution, sd=2 (higher variance)

uni_data = c(uni_A, uni_B)
uni_groups = factor(c(rep("A", length(uni_A)), rep("B", length(uni_B))))
# Combined univariate dataset

# Bootstrap analysis of univariate variance
uni_boot = disparity_resample(uni_data,
                              group = uni_groups,
                              n_resamples = 1000,
                              bootstrap_rarefaction = "bootstrap")
# Bootstrap for univariate data (statistic argument ignored)

cat("Univariate variance analysis:\n")
print(uni_boot)
# Group B should show higher variance


plot(uni_boot)
# Plotting univariate bootstrap results


## -----------------------------------------------------------------------------
if (requireNamespace("MASS", quietly = TRUE)) {
  # Single group bootstrap analysis
  single_boot = disparity_resample(grpB,
                                   n_resamples = 500,
                                   statistic = "multivariate_variance",
                                   bootstrap_rarefaction = "bootstrap")
  # Analysis of Group B alone
  
  cat("Single group analysis (Group B):\n")
  print(single_boot)
  # Confidence interval for single group disparity
}

