#'Test deviance with chi square test
#'
#'This is used to see if the deviance of a glm follows a chi square distribution
#'
#'@return
#'  \item{value} p-value of the chi square test with residuals' degrees of freedom
#'  
#'@param model the glm model to eval
#'  
#'@export
#'@examples
#' ## Dobson (1990) Page 93: Randomized Controlled Trial :
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' d.AD <- data.frame(treatment, outcome, counts)
#' glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
#' summary(glm.D93)

deviance_test <- function(model){
  d <- deviance(model)
  d_df <- df.residual(model)
  pchisq(d, d_df, lower.tail = FALSE)
}

#' Model overdispertion
#' 
#' See if deviance/degrees of freedom is greater than one
#' 
#' @return
#'   Returns the value of deviance/degrees of freedom
#'   
#' @param model the glm model to test
#' @export
#' @examples
#' 
#' ## Dobson (1990) Page 93: Randomized Controlled Trial :
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' d.AD <- data.frame(treatment, outcome, counts)
#' glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
#' 
#' od(glm.D93)
#' 

od <- function(model){
  d <- deviance(model)
  d_df <- df.residual(model)
  d/d_df
}


#' Returns the model formula
#' 
#' This function produces the expression Y = a + bX with the a and b replaced 
#' by the model coefficients
#' 
#' 

formula_coef <- function(model, type = ""){
  coefs <- coefficients(model)
  link <- family(model)$link
  gx <- paste0(round(coefficients(model)[1],2), " + ", 
  paste(sprintf("%.2f * %s", 
                coefs[-1],  
                names(coefs)[-1])), 
  collapse=" + ")
  gx <- gsub("\\+ -", "- ", gx)
  link_formula <- eval(switch(link, 
                              logit = expression(paste("frac(e^(", gx,
                                                       "), 1 + e^(", gx, "))"))))
  paste("E(y) ==", link_formula, collapse = "")
}


#' BIC value
#' 
#' BIC = z^2-log(n)
#' 

BIC_coef <- function(model){
  n <- nobs(model)
  coefs <- coef(summary(model))[-1, "z value"]
  coefs^2 - log(n)
}