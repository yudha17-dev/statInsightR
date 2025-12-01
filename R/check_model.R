#' Check a regression model for issues
#'
#' Runs diagnostics on an lm model including multicollinearity,
#' residual normality, heteroskedasticity, leverage, and influence.
#'
#' @param model An lm model object
#'
#' @return A list containing diagnostics and warnings
#'
#' @export
check_model <- function(model) {
  if (!inherits(model, "lm")) {
    stop("check_model() currently supports only lm models.")
  }

  cat("Model checking started\n")

  warnings <- list()

  ###############################################################
  # 1. VIF CHECK (with applicability logic)
  ###############################################################

  X <- model.matrix(model)
  X_no_intercept <- X[, colnames(X) != "(Intercept)", drop = FALSE]

  # Case A: fewer than 2 predictors
  if (ncol(X_no_intercept) < 2) {
    cat("VIF not applicable:\n")
    cat("   - Reason: Model has fewer than 2 predictors.\n\n")
    vif_vals <- NA

    # Case B: aliased coefficients
  } else if (any(is.na(coef(model)))) {
    cat("VIF not applicable:\n")
    cat("   - Reason: Model contains aliased coefficients (perfect collinearity).\n\n")
    vif_vals <- NA

    # Try VIF
  } else {
    vif_try <- try(car::vif(model), silent = TRUE)

    if (inherits(vif_try, "try-error")) {
      cat("VIF not applicable:\n")
      cat("   - Reason: VIF computation failed (likely factor structure or singularity).\n\n")
      vif_vals <- NA
    } else {
      vif_vals <- vif_try

      max_vif <- max(vif_vals)

      if (max_vif > 10) {
        cat("Issue with multicollinearity\n")
        cat(sprintf("   - Max VIF = %.4f\n", max_vif))
        cat("   - Explanation: Predictors are strongly correlated.\n")
        cat("   - Suggestion: Remove variables, standardize, or use PCA.\n\n")
        warnings$multicollinearity <- vif_vals
      } else {
        cat("No multicollinearity issues detected.\n\n")
      }
    }
  }

  ###############################################################
  # 2. NORMALITY CHECK
  ###############################################################
  sw <- stats::shapiro.test(stats::residuals(model))
  if (sw$p.value < 0.05) {
    cat("Issue: Non-normal residuals\n")
    cat(sprintf("   - Shapiro-Wilk p = %.4f\n", sw$p.value))
    cat("   - Explanation: Residuals deviate from normality.\n")
    cat("   - Suggestion: Try transformation or robust regression.\n\n")
    warnings$normality <- sw
  } else {
    cat("Residuals appear normally distributed.\n\n")
  }

  ###############################################################
  # 3. HETEROSKEDASTICITY
  ###############################################################
  bp <- lmtest::bptest(model)
  if (bp$p.value < 0.05) {
    cat("Issue: Heteroskedasticity detected\n")
    cat(sprintf("   - Breusch-Pagan p = %.4f\n", bp$p.value))
    cat("   - Explanation: Non-constant variance in residuals.\n")
    cat("   - Suggestion: Consider robust SE or transformations.\n\n")
    warnings$heteroskedasticity <- bp
  } else {
    cat("No heteroskedasticity detected.\n\n")
  }

  ###############################################################
  # 4. COOK'S DISTANCE
  ###############################################################
  cooks <- stats::cooks.distance(model)
  n <- length(cooks)
  cutoff <- 4 / n
  influential_points <- which(cooks > cutoff)

  if (length(influential_points) > 0) {
    cat("Issue: Influential observations\n")
    cat(sprintf("   - Cook's distance cutoff = %.4f\n", cutoff))
    cat("   - Observations: ", paste(influential_points, collapse = ", "), "\n")
    cat("   - Suggestion: Inspect using plot(model, which = 4).\n\n")
    warnings$influential <- influential_points
  } else {
    cat("No influential observations found.\n\n")
  }

  ###############################################################
  # 5. LEVERAGE
  ###############################################################
  hat_vals <- stats::hatvalues(model)
  p <- length(stats::coef(model))
  leverage_cutoff <- 2 * p / n
  high_lev <- which(hat_vals > leverage_cutoff)

  if (length(high_lev) > 0) {
    cat("Issue: High-leverage points detected\n")
    cat(sprintf("   - Leverage cutoff = %.4f\n", leverage_cutoff))
    cat("   - Observations: ", paste(high_lev, collapse = ", "), "\n")
    cat("   - Suggestion: Recheck outliers or unusual values.\n\n")
    warnings$leverage <- high_lev
  } else {
    cat("No high leverage points detected.\n\n")
  }

  ###############################################################
  # RETURN OBJECT
  ###############################################################
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
