usethis::create_package()

usethis::use_git()

usethis::use_mit_license()

usethis::use_package("dplyr")



########################################################################################################################

f = function() {
  print("hello world")
}

#' @title Fizzbuzz function
#'
#' @description Implements the fizzbuzz function
#'
#' @param x *Numeric* vector to run the fizzbuzz algorithm on. Must be finite and >0.
#'
#' @returns A character vector containing the values of x, fizz, buzz, or fizzbuzz
#'
#' @examples
#'
#' fizzbuzz(1:10)
#' fizzbuzz(10:1)
#'
#' @export
fizzbuzz = function(x) {
  stopifnot(all(x>0))
  stopifnot(all(is.finite(x)))
  stopifnot(is.numeric(x))
  
  dplyr::case_when(
    x %% 3 == 0 & x %% 5 == 0 ~ "fizzbuzz",
    x %% 3 == 0  ~ "fizz",
    x %% 5 == 0  ~ "buzz",
    TRUE ~ as.character(x)
  )
}


## Package: fizzbuzz
## Title: Implements the fizzbuzz algorithm
## Version: 1.0.0.9000
## Authors@R: 
##   person("Colin", "Rundel", , "rundel@gmail.com", role = c("aut", "cre"),
##          comment = c(ORCID = "YOUR-ORCID-ID"))
## Description: What the package does (one paragraph).
## License: MIT + file LICENSE
## Encoding: UTF-8
## Roxygen: list(markdown = TRUE)
## RoxygenNote: 7.2.2
## Imports: 
##   dplyr,
##   forcats

