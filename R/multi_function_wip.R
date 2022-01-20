#' A variety of functions for dynamic use

# Help see 'what is in the data' ####

#' \code{yfind} -- help find where a column with a certain string name takes values across years (or other grouping)

#' @export
yfind <- function(df = eas_all, text, n=3, y=year) {
  df %>%
    dplyr::select(year, matches({text})) %>%
    group_by({{y}}) %>%
    sample_n(size = n)
}

#' Grouped summaries, listing of unique groups
#'
#' \code{grp_n} counts nonmissing observations of each column by group
#'
#' @examples
#'  grp_n(mtcars, cyl)
#' @export

grp_n <- function (df, groupvar) {
  df %>%
    group_by({{groupvar}}) %>%
    summarise(across(.cols = everything(),
                     .fns = list(n = ~ sum(!is.na(.x)))
    )
    )
}

#' \code{grp_uniq} counts nonmissing *distinct* observations of each column by group
#'
#' @examples
#'  grp_n(mtcars, cyl)


#' @export
grp_uniq <- function (df, groupvar) {
  df %>%
    group_by({{groupvar}}) %>%
    summarise(across(.cols = everything(),
                     .fns = list(uniq = ~ n_distinct(.x))
    )
    )
}

#' Function to filter by given string:
#' @export
filter_parse <- function(df, x) {
 {{df}} %>%
   filter(rlang::eval_tidy(rlang::parse_expr({{x}})))
}

# comparing things visually ####

#' \code{compareColumns} compares column types across two df (e.g., in advance of a  merge) #

#' @details https://stackoverflow.com/questions/45743991/r-compare-column-types-between-two-dataframes
#'
#' @export

compareColumns <- function(df1, df2) {
  commonNames <- names(df1)[names(df1) %in% names(df2)]
  data.frame(Column = commonNames, df1 = sapply(df1[, commonNames],
    class), df2 = sapply(df2[, commonNames], class))
}

#' \code{sidebyside} just lines up 2 or more data frames for comparison view

#' @examples
##' sidebyside(mtcars[,1:2], mtcars[,3:4])

#' @export
sidebyside <- function(..., width = 60) {
  l <- list(...)
  p <- lapply(l, function(x) {
    xx <- capture.output(print(x, width = width))
    xx <- gsub("\"", "", xx)
    format(xx, justify = "left", width = width)
  })
  p <- do.call(cbind, p)
  sapply(seq_len(nrow(p)), function(x) paste(p[x, ], collapse = ""))
}


####  SUMMARY tables function(s) ####

#' \code{tabg} --  tabyl one way plus sort by descending frequency -- the version we normally want
#' @export
#'
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



#'  \code{group_by_sum} Quick 'group by' function to look at NA or 0 values for each year

#' @export
group_by_sum <- function(df, col, group=year, value=NA, name="n_NA"){
  # col = column to summarise
  # value = values to aggregate, i.e value = NA means summarise the NA values in a column by year
  # name = output column name

  # Column name for proportion of col == value
  prop_name = paste("prop", name, sep="_")

  assertthat::assert_that(class(name) == "character", msg="Name must be a string")

  df %>% dplyr::group_by({{group}}) %>%
    dplyr::summarise(!!name := dplyr::if_else(is.na(value), # If value if NA then use is.na
                                              sum(is.na({{col}})),
                                              sum({{col}} == value, na.rm=TRUE)), # Else sum col == value
                     n = n()) %>%
    mutate(!!prop_name := !!parse_expr(name)/n)
}

#'  \code{group_mean_conf_int} Function to calculate confidence intervals for a variable given grouping variables

#' @export
group_mean_conf_int <- function(df, var, groups = NULL, se_func = se, ...){
  ci <- function(x, se, lower = TRUE){
    x + 1.96*se
  }
  var_s <- rlang::as_string(rlang::ensym(var))
  df %>%
    group_by(across({{groups}})) %>%

    summarise(across({{ var }},
                     .fns = list(mean = ~mean(.x, na.rm=TRUE),
                                 se = se_func),
                     .names = "{.col}_{.fn}")) %>%
    mutate("upper_ci_{{var}}" := .data[[stringr::str_c(var_s, "_mean")]] + 1.96*.data[[stringr::str_c(var_s, "_se")]],
           "lower_ci_{{var}}" := .data[[stringr::str_c(var_s, "_mean")]] - 1.96*.data[[stringr::str_c(var_s, "_se")]])
}


################# Coding shortcuts ####

#' \code{Sm} selects variables matching string 'x', case-sensitive
#' @export
Sm <- function(df, X) dplyr::select(df, matches({X},  ignore.case = FALSE))

#' \code{sm} selects variables matching string 'x', not case-sensitive
#' @export
sm <- function(df, X) dplyr::select(df, matches({X})) # ... not case-sensitive


#' \code{snotm} selects variables *not* matching that string, not case-sensitive
#' @export
snotm <- function(df, X) dplyr::select(df, -matches({X}))


#' \code{Snotm} selects variables *not* matching that string,  case-sensitive
#' @export
Snotm <- function(df, X) dplyr::select(df, -matches({X},  ignore.case = FALSE))


#' \code{Smn} creates vector of *names* of variables matching string 'x', case-sensitive
#' @export
Smn <- function(df, X) dplyr::select(df, matches({X}, ignore.case = FALSE)) %>% names()

#' \code{smn} creates vector of *names* of variables matching string 'x', not case-sensitive
#' @export
smn <- function(df, X) dplyr::select(df, matches({X})) %>% names() # not case-sensitive
