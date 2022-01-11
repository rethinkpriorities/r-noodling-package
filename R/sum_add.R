#' Calculate mean, n, sd, and se
#'
#' \code{sum_add} takes a dataframe and a column name, and calculates mean, median, n,  sd, and se
#'
#' @param df
#' @param var

#'
#' @examples
#' # Here is an example
#' x <- sample(x = c(0, 1),  size = 10, replace = TRUE)
#' se_bin(x)
#'
#' @export

summ_add <- function(df, var) {
    df %>%
    mutate(
    mean = mean(as.numeric( {{ var }} )),
    n = n(),
    sd = sd(as.numeric({{cacr}})),
    se = sqrt((m*(1-m))/n)
    )
}
