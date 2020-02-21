#' @export
getPsModel2 <- function (propensityScore, cohortMethodData) {
  coefficients <- attr(propensityScore, "metaData")$psModelCoef
  result <- data.frame(coefficient = coefficients[1], covariateId = NA, 
                       covariateName = "(Intercept)")
  coefficients <- coefficients[2:length(coefficients)]
  coefficients <- coefficients[coefficients != 0]
  if (length(coefficients) != 0) {
    coefficients <- data.frame(coefficient = coefficients, 
                               covariateId = as.numeric(attr(coefficients, "names")))
    coefficients <- merge(ff::as.ffdf(coefficients), cohortMethodData$covariateRef)
    coefficients <- ff::as.ram(coefficients[, c("coefficient", 
                                                "covariateId", "covariateName")])
    result <- rbind(result, as.data.frame(coefficients))
    result <- result[order(-abs(result$coefficient)), ]
  }
  return(result)
}