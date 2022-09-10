#' Calculate mean, n, sd, and se
#'
#' \code{sum_add} takes a dataframe and a column name, and calculates mean, n,  sd, and se #'
#' @examples
#' # Here is an example
#' x <- sample(x = c(0, 1),  size = 10, replace = TRUE)
#' se_bin(x)
#'
#' @export

summ_add <- function(df, var) {
    df %>%
    mutate(
    n = n(),
    mean = mean(as.numeric( {{ var }} )),
    sd = sd(as.numeric({{var}}), na,rm=TRUE),
    se = sqrt(var(x) / length(x))
    )
}


