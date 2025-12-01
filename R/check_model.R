#' Check a regression model for issues
#'
#' Runs diagnostics on an lm model including multicollinearity,
#' residual normality, heteroskedasticity, leverage, and influence.
#'
#' @param model An lm model object
#'
#' @return A list containing diagnostics and warnings
#'
#' @examples
#' model <- lm(mpg ~ hp + wt, data = mtcars)
#' check_model(model)
#'
#' @export

check_model <- function(model) {
  if (!inherits(model,"lm")) {
    stop("check_model() currently supports only lm models.")
  }

  cat("Model checking started\n")

  warnings <- list()
  #1. VIF
  vif_vals <- try(car::vif(model), silent = TRUE)
  if (!inherits(vif_vals, "try-error")) {
    max_vif <- max(vif_vals)
    if (max_vif > 10) {
      cat("Issue with multicollinearity\n")
      cat(sprintf("   - Max VIF = %.24\n",max_vif))
      cat("   - Explanation: Predictors are strongly correlated\n")
      cat("   - Suggestion: Remove variables, standardize, or use PCA.\n\n")
      warnings$multicollinearity <- vif_vals
    } else {
      cat("No multicollinearity issues detected.\n\n")
    }
  }
  #2. normality
  sw <- stats::shapiro.test(stats::residuals(model))
  if (sw$p.value < 0.05) {
    cat("Issue: Non-normal residuals\n")
    cat(sprintf("   - Shapiro-wilk p= %.4f\n",sw$p.value))
    cat("   - Explaination: Residuals deviate from normality.\n")
    cat("   - Suggestion: Try transformation or robust regression.\n\n")
    warnings$normality <- sw
  } else {
    cat("Residuals appear normally distributed.\n\n")
  }

  #. Heteroskedasticity
  bp <- lmtest::bptest(model)
  if (bp$p.value < 0.05) {
    cat("Issue: Heteroskedasticity detected.\n")
    cat(sprintf("   - Breausch-Pagan p = %.4f\n",bp$p.value))
    cat("   - Explanation: Non-constant variance in residuals\n")
    cat("   - Suggestion: Consider robust SE or transformations.\n\n")
    warnings$heterskedasticity <- bp
  } else {
    cat("No heteroskedasticity detected.\n\n")
  }
  #4. cook's distance
  cooks <- stats::cooks.distance(model)
  n <- length(cooks)
  cutoff <- 4/n
  influential_points <- which(cooks > cutoff)

  if (length(influential_points) > 0) {
    cat("Issue: Influential observations\n")
    cat(sprintf("   - Cook's distance cutoff = %.4f\n",cutoff))
    cat("   - Observations: ", paste(influential_points, collapse = ", "), "\n")
    cat("   - Suggestion: Inspect using plot(model, which = 4).\n\n")
    warnings$influential <- influential_points
  } else {
    cat("No influential observations found.\n\n")
  }

  #5. leverage
  hat_vals <- stats::hatvalues(model)
  p <- length(stats::coef(model))
  leverage_cutoff <- 2 * p / n
  high_lev <- which(hat_vals > leverage_cutoff)

  if (length(high_lev) > 0) {
    cat("Issue: High-leverage points detected\n")
    cat(sprintf("   - Leverage cutoff = %.4f\n",leverage_cutoff))
    cat("   - Observations: ", paste(high_lev, collapse = ", "),"\n")
    cat("   - Suggestion: Recheck outliers or unusual values. \n\n")
    warnings$leverage <- high_lev
  } else {
    cat("No high leverage points detected.\n\n")
  }

  invisible(list(
    model = model,
    warnings = warnings,
    vif = vif_vals,
    shapiro = sw,
    bptest = bp,
    cooks_distance = cooks,
    hat_values = hat_vals
  ))
}
