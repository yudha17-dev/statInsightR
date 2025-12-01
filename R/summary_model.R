#' Detailed Narrative Regression Summary
#'
#' Generates a detailed human-readable summary of an lm model,
#' interpreting effect size, direction, statistical significance,
#' confidence intervals, and overall model fit.
#'
#' @param model A fitted lm model.
#'
#' @return Invisibly returns a data frame of coefficient details.
#' @export
summary_clean <- function(model) {

  if (!inherits(model, "lm")) {
    stop("summary_model() only supports lm models.")
  }

  sm <- summary(model)
  coeff <- sm$coefficients
  conf <- confint(model)
  df <- data.frame(
    Variable = rownames(coeff),
    Estimate = coeff[, 1],
    Std_Error = coeff[, 2],
    t_value = coeff[, 3],
    p_value = coeff[, 4],
    CI_low = conf[, 1],
    CI_high = conf[, 2],
    stringsAsFactors = FALSE
  )

  response <- names(model$model)[1]

  # ------- PRINT HEADER -------
  cat("\nDETAILED REGRESSION SUMMARY\n")
  cat("----------------------------------------\n\n")

  # ------- MODEL FIT -------
  r2 <- sm$r.squared
  adjr2 <- sm$adj.r.squared

  cat(sprintf("Model explains %.1f%% of the variance in %s (Adjusted R² = %.3f).\n\n",
              r2 * 100, response, adjr2))

  # ------- NARRATIVE FOR EACH TERM -------
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]

    var <- row$Variable

    if (var == "(Intercept)") next

    est <- row$Estimate
    se <- row$Std_Error
    p  <- row$p_value
    lo <- row$CI_low
    hi <- row$CI_high
    t  <- row$t_value

    direction <- if (est > 0) "increase" else "decrease"
    magnitude <- abs(est)

    # Effect strength labels
    strength <- if (magnitude < 0.1) {
      "very small effect"
    } else if (magnitude < 0.5) {
      "small effect"
    } else if (magnitude < 1) {
      "moderate effect"
    } else {
      "strong effect"
    }

    # Significance interpretation
    signif_text <- if (p < 0.001) {
      "highly significant evidence of an effect"
    } else if (p < 0.05) {
      "statistically significant evidence of an effect"
    } else {
      "no statistically significant evidence of an effect"
    }

    cat(sprintf(
      "- **%s**: A one-unit increase in *%s* is associated with a %.3f %s in *%s*, on average, holding other variables constant.\n",
      var, var, magnitude, direction, response
    ))

    cat(sprintf(
      "  (%s; t = %.2f, SE = %.3f, 95%% CI: [%.3f, %.3f])\n",
      strength, t, se, lo, hi
    ))

    cat(sprintf(
      "  Statistical conclusion: %s (p = %.3f).\n\n",
      signif_text, p
    ))
  }

  cat("----------------------------------------\n")
  invisible(df)
}
