#' Years of life lost calculation from coxph model
#'
#'
#'
#' @param model coxph model
#' @param data data used to fit coxph model
#' @param newdata New data at which to do predictions
#' @param predvar variables to predict yll for
#' @param tau Maximum life length
#' @param ci.level Confidence level, default set to 0.95
#' @param iterations default 1000
#' @param multicore boolean default FALSE
#' @param ncpus number of cpus to use
#'
#' @importFrom boot boot boot.ci
#' @import survival
#' @importFrom magrittr `%>%`
#' @importFrom dplyr mutate bind_cols as_tibble tibble select transmute filter
#' @importFrom purrr map_df pluck set_names map
#' @importFrom tidyr unnest
#' @importFrom stats formula
#'
#'
#'
#' @examples
#' library(survival)
#'
#' mdl <- coxph(Surv(age_end, event)~ gender*dis + ses, data = popum_dis)
#'
#' newdata <- with(popum_dis, expand.grid(
#'   gender = unique(gender)[1],
#'   ses = unique(ses)[1],
#'   dis = unique(dis)
#' ))
#'
#'
#' yll_cox(mdl, popum_dis, newdata = newdata, predvar = c("dis", "gender"), tau = 43)
#'
#' \dontrun{
#' yll_cox(
#'   mdl,
#'   popum_dis,
#'   newdata = newdata,
#'   tau = 43,
#'   ci.level = .95,
#'   iterations = 500,
#'   multicore = T,
#'   ncpus = 11
#' )
#' }
#' @export

yll_cox <- function(model, data, newdata, predvar = NULL, tau = 18, ci.level = NULL, iterations = 1000, multicore = FALSE, ncpus = 1) {

  if(!inherits(model, "coxph")){
    stop("Error: model must be a coxph object")
  }

  if(is.null(newdata)){
    stop("Error: newdata required")
  }

  if(!is.numeric(tau)){
    stop("Error: tau must be set to a number")
  }



  surv_x <- centred_surv(
    model,
    newdata = newdata, predvar = predvar)

  # alpha <- -qnorm((1- ci.level)/2)

  x <- summary(surv_x)$table
  res <- as_tibble(x) %>%
    set_names(c("records","n", "nstart", "events", "rmean", "se", "median", "ci_la", "ci_ha")) %>%
    select(n, events, rmean, se) %>%
    bind_cols(newdata, .) %>%
    mutate(
      yll = tau - rmean
    )

  if (!is.null(ci.level)){
    if (ci.level > 0 & ci.level < 1){
      ci_d <- yll_ci(model, data, newdata, predvar,tau, ci.level, iterations, multicore, ncpus)

      res <- bind_cols(res, ci_d)
    }

  }

  as_tibble(res)

}




#' YLL CI
#'
#' Calculate bootstraped CI
#'
#' @param model coxph model
#' @param data data used to fit model
#' @param newdata newdata same as YLL
#' @param tau max observation length
#' @param conf_int Confidence interval
#' @param iterations default 1000
#' @param multicore boolean default FALSE
#' @param ncpus number of cpus to use
#'


yll_ci <- function(model, data, newdata, predvar, tau, conf_int = 0.95, iterations = 1000, multicore = FALSE, ncpus = 1){

  get_yll_params <- function(d = NULL, nd, formula){
    tryCatch(
      {
        frm <- as.character(formula)[c(2,3)] %>% paste(collapse = "~")
        fit <-  coxph(formula(frm), data =d)

        surv_x <- centred_surv(
          fit,
          newdata = newdata, predvar = predvar)

        # alpha <- -qnorm((1- ci.level)/2)

        x <- summary(surv_x)$table
        res <- as_tibble(x) %>%
          set_names(c("records","n", "nstart", "events", "rmean", "se", "median", "ci_la", "ci_ha")) %>%
          select(n, events, rmean, se) %>%
          bind_cols(newdata, .) %>%
          mutate(
            yll = tau - rmean
          )
        res %>% pluck("yll")

    }, error=function(cond){
      message("Boot error:\n", cond)
      # Choose a return value in case of error
      return(NA)
    }
    )
  }


  # function to obtain regression weights
  bs <- function(data, indices) {
    d <- data[indices,] # allows boot to select sample

    fit <- get_yll_params(d, newdata, formula = formula(model))

    return(fit)
  }

  multic <- ifelse(multicore, "multicore", "no")
  suppressWarnings(
    results <- boot(
      data=data,
      statistic=bs,
      R=iterations,
      parallel = multic,
      ncpus=ncpus)
    # results
  )


  est <- apply(results$t, 2, mean)
  temp_f <- function(x){
    ro <- boot.ci(results, conf = conf_int, index = x, type = "perc")$percent[c(4,5)]
    tibble(
      ci_l = ro[1],
      ci_h = ro[2]
    )
  }
  map_df(1:nrow(newdata), temp_f) %>% mutate(boot_est = est) %>%
    mutate(t = lapply(1:ncol(results$t), function(i) results$t[ ,i]))
}




