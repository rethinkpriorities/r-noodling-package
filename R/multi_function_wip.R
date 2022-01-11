#' Explanation in one sentence
#'
#' \code{functionname} does blah
#'
#' @param x what inputs to function
#'
#' @examples
#' # Here is an example
#' x <- sample(x = c(0, 1),  size = 10, replace = TRUE)
#' se_bin(x)
#'
#' @details Pasting functions in here to turn into package cormat later
#'
#' @examples
#' nothing(mtcars)

#' @export

###... Function to filter by given string: ####

filter_parse <- function(df, x) {
 {{df}} %>%
   filter(rlang::eval_tidy(rlang::parse_expr({{x}})))
}

# compare column types across two df (e.g., in advance of a
# merge); from

#' @export

# https://stackoverflow.com/questions/45743991/r-compare-column-types-between-two-dataframes
compareColumns <- function(df1, df2) {
  commonNames <- names(df1)[names(df1) %in% names(df2)]
  data.frame(Column = commonNames, df1 = sapply(df1[, commonNames],
    class), df2 = sapply(df2[, commonNames], class))
}


#' @export

# tabg: tabyl one way plus sort by descending frequency -- the version we normally want
tabg <- function(df, col) {
    janitor::tabyl({{df}},{{col}}) %>%
        arrange(-`n`)
}


#' @export

tabsum <- function(df = ADSX, yvar = donation, xvar = Stage, treatvar = Treatment) {
  yvar <- enquo(yvar)
  xvar <- enquo(xvar)
  treatvar <- enquo(treatvar)
  df %>% ungroup() %>% # mutate(xvar = as.factor(!!xvar)) %>%
  dplyr::group_by(!!xvar, !!treatvar) %>% # drop_na(!!yvar, !!treatvar) %>%
  dplyr::select(!!yvar, !!treatvar, !!xvar) %>% dplyr::summarise(meanyvar = mean(!!yvar,
    na.rm = TRUE))
}


