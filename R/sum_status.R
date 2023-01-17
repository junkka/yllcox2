#' summary wrapper function
#'
#' A details of function
#'
#' @title Sum wrapper
#' @param x any vector
#' @param ... other arguments
#' @importFrom forcats fct_explicit_na
#' @export

sum_wrapper <- function(x, ...) UseMethod("sum_wrapper")

#' @rdname sum_wrapper
#' @export
sum_wrapper.logical <- function(y, by){
  tibble(
    level  = names(table(y)) %>% as.character(),
    n      = length(y),
    na     = length(y[is.na(y)]),
    mean   = as.vector(table(y))/length(y),
    count  = as.vector(table(y)),
    sd     = sqrt(as.vector(table(y))/length(y) * (1-as.vector(table(y))/length(y))/length(y)),
    min    = NA,
    q25    = NA,
    median = NA,
    q75    = NA,
    max    = NA,
    dead   = if (!is.null(by)){
      as.vector(table(y[by == 1]))
    } else {
      NA
    },
    rate   = if(!is.null(by)){
      (as.vector(table(y[by == 1]))/as.vector(table(y)))*1000
    } else {
      NA
    }
  )
}


#' @rdname sum_wrapper
#' @export
sum_wrapper.factor <- function(y, by){
  nad <- fct_explicit_na(y)
  tibble(
    level  = names(table(nad)) %>% as.character(),
    n      = length(y),
    na     = length(y[is.na(y)]),
    mean   = as.vector(table(nad))/length(y),
    count  = as.vector(table(nad)),
    sd     = sqrt(as.vector(table(nad))/length(y) * (1-as.vector(table(nad))/length(y))/length(y)),
    min    = NA,
    q25    = NA,
    median = NA,
    q75    = NA,
    max    = NA,
    dead   = if (!is.null(by)){
      as.vector(table(y[by == 1]))
    } else {
      NA
    },
    rate   = if(!is.null(by)){
      (as.vector(table(nad[by == 1]))/as.vector(table(nad)))*1000
    }else {
      NA
    }
  )
}


#' @rdname sum_wrapper
#' @export
sum_wrapper.numeric <- function(y, by){
  y2 <- y[!is.na(y)]
  tibble(
    level  = NA,
    n      = length(y),
    na     = length(y[is.na(y)]),
    mean   = mean(y2),
    count  = length(y),
    sd     = sd(y2),
    min    = min(y2),
    q25    = quantile(y2)[2],
    median = quantile(y2)[3],
    q75    = quantile(y2)[4],
    max    = max(y2),
    rate   = NA
  )
}


#' @rdname sum_wrapper
#' @export
sum_wrapper.Date <- function(y, by){
  y <- decimal_date(y)
  y2 <- decimal_date(y[!is.na(y)])
  tibble(
    level  = NA,
    n      = length(y),
    na     = length(y[is.na(y)]),
    mean   = mean(y2),
    count  = length(y),
    sd     = sd(y2),
    min    = min(y2),
    q25    = quantile(y2)[2],
    median = quantile(y2)[3],
    q75    = quantile(y2)[4],
    max    = max(y2),
    dead   = NA,
    rate   = NA
  )
}

#' @rdname sum_wrapper
#' @export
sum_wrapper.character <- function(y, by){
  tibble(
    level  = NA,
    n      = NA,
    na     = NA,
    mean   = NA,
    count  = NA,
    sd     = NA,
    min    = NA,
    q25    = NA,
    median = NA,
    q75    = NA,
    max    = NA,
    dead   = NA,
    rate   = NA
  )
}


#' Summary table wrapper function
#'
#' A details of function
#'
#' @title Sum table
#' @param z a data.frame
#' @param by variable name for rates
#' @export

summary_table <- function(z, by = NULL){
  # for each variable type create summary statistc
  # colss <- apply(x, 2, class)
  # unnest <- unnest_legacy
  tibble(
    var = colnames(z),
    d = map(as.list(z), sum_wrapper, pluck(z, by))
  ) %>% unnest(d)

}
