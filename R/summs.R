#' Calculate mean, n, sd, and se
#'
#' \code{sum_add2} takes a column name, and calculates mean, median, n,  sd, and se  and specified (default p20 and p80) quantiles
#' @examples
#' # Here is an example
#' x <- sample(x = c(0, 1),  size = 10, replace = TRUE)
#' se_bin(x)
#'



#' @export

summs <- function(df, var, qlow = 0.2, qhigh = 0.8) {
    df %>%
    dplyr::summarise(
    n = sum(!is.na({{var}})),
    mean = mean(as.numeric({{var}}), na.rm=TRUE),
    med = median(as.numeric( {{var}}), na.rm=TRUE),
    sd = sd(as.numeric({{var}}),  na.rm=TRUE),
    se = sqrt(var({{var}},  na.rm=TRUE)) / n,
    p20 = quantile({{var}}, qlow ),
    p80 = quantile({{var}}, qhigh )
    )
}


#' @export


summs0 <- function(df, var) {
    df %>%
    dplyr::summarise(
    n = sum(!is.na({{var}})),
    mean = mean(as.numeric({{var}}), na.rm=TRUE),
    med = median(as.numeric( {{var}}), na.rm=TRUE),
    sd = sd(as.numeric({{var}}),  na.rm=TRUE),
    se = sqrt(var({{var}},  na.rm=TRUE)) / n    )
}




