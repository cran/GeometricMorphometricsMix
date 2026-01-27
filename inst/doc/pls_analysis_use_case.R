## ----setup, include=FALSE-----------------------------------------------------
library(GeometricMorphometricsMix)

## -----------------------------------------------------------------------------
set.seed(42)
X = matrix(rnorm(60), ncol=6) # Block 1: shape variables
Y = matrix(rnorm(60), ncol=6) # Block 2: ecological variables

## -----------------------------------------------------------------------------
pls_result = pls(X, Y, perm=999, global_RV_test=TRUE)
# Perform PLS analysis with permutation test
# pls_result contains scores, singular axes, and significance tests
pls_result$global_significance_RV
# Show global RV test result
pls_result$singular_axis_significance
# Show significance for each axis

## -----------------------------------------------------------------------------
set.seed(1234)
n = 50 # number of observations
p = 6  # variables per block

# Simulate a latent variable that drives covariation
latent = rnorm(n)
# latent variable

latent_mat = matrix(latent, nrow=n, ncol=p)
# latent variable replicated across columns for element-wise addition

X_assoc = matrix(rnorm(n*p), ncol=p) + 0.5 * latent_mat
# Block 1: shape variables influenced by latent + noise

Y_assoc = matrix(rnorm(n*p), ncol=p) + 0.4 * latent_mat
# Block 2: ecological variables influenced by latent + noise

# Now X_assoc and Y_assoc have shared structure and will show significant covariation

## -----------------------------------------------------------------------------
pls_assoc_result = pls(X_assoc, Y_assoc, perm=999, global_RV_test=TRUE)
# Perform PLS analysis with permutation test
pls_assoc_result$global_significance_RV # Show global RV test result
pls_assoc_result$singular_axis_significance # Show significance for each axis

## -----------------------------------------------------------------------------
Pred_major_axis = pls_major_axis(pls_assoc_result, axes_to_use=1)
# Compute projections and predictions based on the first pair of PLS axes

names(Pred_major_axis)
# Main elements returned (lists for projection, predictions, and optionally new data)

Proj_scores = Pred_major_axis$original_major_axis_projection[[1]]$original_data_PLS_projection
# Scores of original data projected on the major axis

hist(Proj_scores,
	main="Original data - projections on the major axis",
	xlab="Major axis score")
# Distribution of major axis scores (first axis pair)

Pred_block1 = Pred_major_axis$original_major_axis_predictions_reversed$Block1
# Predictions for block 1 back-transformed to original space

Pred_block2 = Pred_major_axis$original_major_axis_predictions_reversed$Block2
# Predictions for block 2 back-transformed to original space

## -----------------------------------------------------------------------------
set.seed(999)
X_new = X_assoc + matrix(rnorm(n * ncol(X_assoc), sd=0.2), ncol=ncol(X_assoc))
# New data for block 1 (perturbed version of original data)

Y_new = Y_assoc + matrix(rnorm(n * ncol(Y_assoc), sd=0.2), ncol=ncol(Y_assoc))
# New data for block 2 (perturbed version of original data)

Pred_major_axis_new = pls_major_axis(pls_assoc_result,
							  new_data_x = X_new,
							  new_data_y = Y_new,
							  axes_to_use = 1)
# Obtain major axis projections and predictions for new data

colnames(Pred_major_axis_new$new_data_results$new_data_major_axis_proj)
# Names (axes) for the major axis projections of new data

head(Pred_major_axis_new$new_data_results$new_data_major_axis_proj)
# Scores of new data on the major axis

head(Pred_major_axis_new$new_data_results$new_data_Block1_proj_prediction_revert)
# Predictions for block 1 new data (back-transformed)

head(Pred_major_axis_new$new_data_results$new_data_Block2_proj_prediction_revert)
# Predictions for block 2 new data (back-transformed)

