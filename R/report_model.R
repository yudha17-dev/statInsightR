#' Generate an HTML regression report
#'
#' @param model An lm model object.
#' @param file Output HTML file name.
#'
#' @return Path to created HTML file.
#' @export
report_model <- function(model, file = "model_report.html") {
  if (!inherits(model, "lm")) {
    stop("report_model() only supports lm models.")
  }

  template <- system.file("templates", "report_template.Rmd",
                          package = "statInsightR")
  if (template == "") {
    stop("Template not found. Check inst/templates/report_template.Rmd")
  }

  temp_rmd <- tempfile(fileext = ".Rmd")
  file.copy(template, temp_rmd, overwrite = TRUE)

  rmarkdown::render(
    input = temp_rmd,
    output_file = basename(file),
    output_dir = getwd(),
    params = list(model = model),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )

  message("HTML report saved to: ", normalizePath(file))
  return(normalizePath(file))
}
