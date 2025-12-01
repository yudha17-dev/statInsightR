#' Automatically generates a written conclusion for a regression model
#'
#' @param model A fitted lm() object
#'
#' @return A character string summarizing model interpretation
#'
#' @export

auto_conclusion <- function(model) {
  out <- c()
  sm <- summary(model)
  coefs <- sm$coefficients
  rn <- rownames(coefs)

  # ----------------------------
  # 1. Significant Predictors
  # ----------------------------
  sig_idx <- which(coefs[, 4] < 0.05 & rn != "(Intercept)")
  nonsig_idx <- which(coefs[, 4] >= 0.05 & rn != "(Intercept)")

  # Significant effects
  if (length(sig_idx) > 0) {
    txt <- "The model identified statistically significant predictors:"
    for (i in sig_idx) {
      name <- rn[i]
      est <- round(coefs[i, 1], 3)
      p <- round(coefs[i, 4], 4)

      direction <- if (est > 0) "positive" else "negative"

      txt <- paste0(
        txt, sprintf(
          "\n- **%s** (%s effect, estimate = %s, p = %s)",
          name, direction, est, p
        )
      )
    }
    out <- c(out, txt)
  } else {
    out <- c(out, "No predictors were statistically significant at the 5% level.")
  }

  # Non-significant predictors
  if (length(nonsig_idx) > 0) {
    txt <- "The following predictors did not show significant evidence of association:"
    for (i in nonsig_idx) {
      name <- rn[i]
      p <- round(coefs[i, 4], 4)
      txt <- paste0(txt, sprintf("\n- **%s** (p = %s)", name, p))
    }
    out <- c(out, txt)
  }

  # ----------------------------
  # 2. Model Fit (R-squared)
  # ----------------------------
  r2 <- sm$r.squared
  r2_adj <- sm$adj.r.squared

  strength <- if (r2 < 0.2) {
    "weak"
  } else if (r2 < 0.5) {
    "moderate"
  } else if (r2 < 0.8) {
    "strong"
  } else {
    "very strong"
  }

  out <- c(
    out,
    sprintf(
      "The model explains **%.1f%%** of the variation in the response (Adjusted R² = %.3f), indicating a **%s** overall fit.",
      r2 * 100, r2_adj, strength
    )
  )

  # ----------------------------
  # 3. Multicollinearity (VIF)
  # ----------------------------
  vif_msg <- ""
  can_vif <- TRUE
  # Try VIF
  vif_vals <- try(car::vif(model), silent = TRUE)

  if (!inherits(vif_vals, "try-error")) {
    high_vif <- names(vif_vals[vif_vals > 10])
    if (length(high_vif) > 0) {
      vif_msg <- paste0(
        "High multicollinearity detected for: ",
        paste(high_vif, collapse = ", "),
        ". This may affect coefficient stability."
      )
    } else {
      vif_msg <- "No multicollinearity concerns were detected (VIF values acceptable)."
    }
  } else {
    vif_msg <- "VIF could not be computed for this model."
  }
  out <- c(out, vif_msg)


  # ----------------------------
  # 4. Residual normality
  # ----------------------------
  sh <- try(shapiro.test(residuals(model)), silent = TRUE)

  if (!inherits(sh, "try-error")) {
    if (sh$p.value < 0.05) {
      out <- c(out, "Residuals deviate significantly from normality (Shapiro–Wilk p < 0.05).")
    } else {
      out <- c(out, "Residuals appear normally distributed (Shapiro–Wilk p ≥ 0.05).")
    }
  } else {
    out <- c(out, "Shapiro–Wilk normality test could not be computed.")
  }

  # ----------------------------
  # 5. Heteroskedasticity (BP test)
  # ----------------------------
  bp <- try(lmtest::bptest(model), silent = TRUE)

  if (!inherits(bp, "try-error")) {
    if (bp$p.value < 0.05) {
      out <- c(out, "Evidence of heteroskedasticity detected (Breusch–Pagan p < 0.05).")
    } else {
      out <- c(out, "No evidence of heteroskedasticity (Breusch–Pagan p ≥ 0.05).")
    }
  } else {
    out <- c(out, "Breusch–Pagan test could not be computed.")
  }

  # ----------------------------
  # 6. Influential Points (Cook's Distance)
  # ----------------------------
  cooks <- cooks.distance(model)
  inf_points <- which(cooks > (4 / length(cooks)))

  if (length(inf_points) > 0) {
    out <- c(
      out,
      paste0(
        "Influential observations detected at indices: ",
        paste(inf_points, collapse = ", "),
        ". These points may affect model stability."
      )
    )
  } else {
    out <- c(out, "No influential observations detected based on Cook’s distance.")
  }

  # ----------------------------
  # Final output
  # ----------------------------
  return(paste(out, collapse = "\n\n"))
}
