library(tidyverse)
library(varhandle)

#' Convert a character string into a numeric vector
#' @description Given a character that contains a list of objects, converts
#' it into a numeric vector if possible.
#' @param x A character string containing comma-separated numbers.
#' @return A numeric vector if conversion is successful, otherwise a string
#' indicating an error.
#' @examples
#' convert_char_to_vector("1, 2, 3")
#' # Returns: [1] 1 2 3
convert_char_to_vector = function(x){
  if (is.character(x) == FALSE){
    return("Invalid input: the input is not a character.")
  }
  x = str_replace_all(x, fixed(" "), "") # removes all spaces
  x = (strsplit(x, ",")[[1]])
  
  check_numeric_count = 0
  for(i in 1:length(x)){
    if(check.numeric(x[i])){
      check_numeric_count = check_numeric_count + 1
    }
  }
  if (check_numeric_count == length(x)){
    x = as.numeric(x) # converts to vector
    x = x[!is.na(x)]
    return(as.double(x))
  } else {
    return("Invalid vector. Not all numbers are numeric.")
  }
}

#' Validate whether a vector is numeric
#' @description Given a vector, determines whether it only contains numerical
#' values.
#' @param x A vector to be examined.
#' @return TRUE if all elements are numeric, FALSE otherwise.
#' @examples
#' valid_numeric_vector(c(1, 2, 3))
#' # Returns: TRUE
#' valid_numeric_vector(c(1, 2, "2"))
#' # Returns: FALSE
valid_numeric_vector = function(x){
  for (i in 1:length(x)) {
    if (is.numeric(x[[i]]) == FALSE){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

#' Turns text input into an R vector
#' @description Accepts input that may be either a character (capable of being
#' converted into a vector) or an existing vector.
#' If the input is a character, this function transforms the value into a vector.
#' If the input is already a vector, the function returns the vector as is.
#' @param x represents the input which may be a character or a vector.
#' @return A numeric vector if valid, otherwise a string indicating an error.
#' @examples
#' create_necessary_vector("1, 1, 1, 1")
#' # Returns: [1] 1 1 1 1
#' create_necessary_vector(c(1, 1, 1, 1))
#' # Returns: [1] 1 1 1 1
create_necessary_vector = function(x){
  if(is.character(x) == TRUE){
    return(convert_char_to_vector(x))
  } else if (typeof(x) == "double" | valid_numeric_vector(x) == TRUE){
    return(x)
  } else {
    return("Invalid vector.")
  }
}

#' Validate simulation parameters
#' @param p total number of features (num_features)
#' @param s the number of true relevant features (num_relevant)
#' @return TRUE if valid (s <= p), otherwise a string indicating an error.
check_sim_params <- function(p, s) {
  if (s > p) {
    return("Invalid parameters p and s.")
  }
  return(TRUE)
}
