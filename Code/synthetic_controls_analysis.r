
source('r_defaults.r')  # set common parameters and call common_functions.r
# install_lazy('Synth')
library(nloptr)
library(memoise)
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
# Steps:
# 1. Residual out any covariates (FWL)
# 2. Construct matrices (a la Synth::dataprep, but without predictors)
# 3. Find control weights (a la Synth::synth)

residualize_once <- function(.tbl, resid_formula) {
    df <- collect(.tbl, n = Inf)  # collect from remote if necessary

    resids <- my_felm(resid_formula, data = df) %>%
        residuals() %>%
        as.numeric()
    return(resids)
}


dataprep <- function(.tbl, dependent, unit_variable, time_variable, treated_unit,
        control_units = NULL, time_optimize_ssr = NULL, post_period = NULL) {
    stopifnot(is.character(dependent),
              is.character(unit_variable), length(unit_variable) == 1,
              is.character(time_variable), length(time_variable) == 1,
              length(treated_unit) == 1,
              ! anyNA(treated_unit)
              )

    .tbl <- select_(.tbl, .dots = c(dependent, unit_variable, time_variable))

    filter_var <- function(.data, var, val) {
        filter_call <- lazyeval::interp(~var %in% val,
            var = as.name(var),
            val = val)
        return(filter_(.data, .dots = filter_call))
    }
    filter_units <- purrr::partial(filter_var, var = unit_variable)
    filter_time <- purrr::partial(filter_var, var = time_variable)
    if (! is.null(control_units)) {
        # Do this filtering before the collect
        # (bad program flow, but potentially much more quick)
        .tbl <- filter_units(.tbl, c(treated_unit, control_units))
    }
    df <- collect(.tbl, n = Inf)  # collect from remote if necessary
    stopifnot(is.data.frame(df))
    # In the default case with control_units == NULL, make all non-treatment into possible controls
    if (is.null(control_units)) {
        control_units <- setdiff(unique(df[[unit_variable]]), treated_unit)
    }
    if (length(control_units) < 2) {
        stop("\n please specify at least two control units\n")
    }
    if (anyDuplicated(control_units) > 0) {
        stop("\n duplicate control units in control_units\n")
    }
    if (treated_unit %in% control_units) {
        stop("\n treated unit among controls\n")
    }

    if (is.null(time_optimize_ssr)) {
        if (0 %in% df[[time_variable]]) {
            all_time_periods <- unique(df[[time_variable]])
            time_optimize_ssr <- all_time_periods[all_time_periods < 0]
        } else {
            stop("time_optimize_ssr not specified and I couldn't take a guess")
        }
    }
    if (is.null(post_period)) {
        if (0 %in% df[[time_variable]]) {
            all_time_periods <- unique(df[[time_variable]])
            post_period <- all_time_periods[all_time_periods >= 0]
        } else {
            stop("post_period not specified and I couldn't take a guess")
        }
    }
    df <- filter_time(df, c(time_optimize_ssr, post_period))

    if (dependent %in% c('sale_count', 'sales_pr_mean', 'sale_tot')) {
        df <- force_panel_balance(df, c(unit_variable, time_variable), fill_na = TRUE)
    } else {
        stop("Need to fill in NAs, but not necessarily with zeros.")
        df <- force_panel_balance(df, c(unit_variable, time_variable))
    }
    stopifnot(! anyNA(df),
              setequal(c(control_units, treated_unit), unique(df[[unit_variable]])),
              all(time_optimize_ssr %in% df[[time_variable]])
    )

    Y_t0 <- df %>% filter_units(treated_unit) %>% filter_time(time_optimize_ssr) %>%
        select_(.dots = dependent) %>%
        setNames(treated_unit) %>%
        as.matrix()

    Y_c0 <- df %>% filter_units(control_units)  %>% filter_time(time_optimize_ssr) %>%
        select_(.dots = c(dependent, unit_variable, time_variable)) %>%
        tidyr::spread_(key_col = unit_variable, value_col = dependent)
    time_col_pre <- Y_c0[[time_variable]] %>% as.matrix()
    Y_c0 <- Y_c0 %>% select_(.dots = paste0('-', time_variable)) %>% as.matrix()

    Y_t1 <- df %>% filter_units(treated_unit) %>% filter_time(post_period) %>%
        select_(.dots = dependent) %>%
        rename_(treated_unit = dependent) %>%
        as.matrix()

    Y_c1 <- df %>% filter_units(control_units)  %>% filter_time(post_period) %>%
        select_(.dots = c(dependent, unit_variable, time_variable)) %>%
        tidyr::spread_(key_col = unit_variable, value_col = dependent)
    time_col_post <- Y_c1[[time_variable]] %>% as.matrix()
    Y_c1 <- Y_c1 %>% select_(.dots = paste0('-', time_variable)) %>% as.matrix()
    stopifnot(nrow(Y_t1) == nrow(Y_c1))

    output <- list(Y_t0 = Y_t0, Y_c0 = Y_c0, Y_t1 = Y_t1, Y_c1 = Y_c1,
        time_col_pre = time_col_pre, time_col_post = time_col_post)
    output <- check_matrices(output)
    invisible(output)
}


check_matrices <- function(dataprep_list) {
    # to_check <- list(Y_t0 = dataprep_list$Y_t0, Y_c0 = dataprep_list$Y_c0)
    to_check <- dataprep_list
    for (i in seq_along(to_check)) {
        if (is.null(to_check[[i]])) {
            stop(paste("\n", names(to_check)[i], "is missing \n"))
        }
        if (! is.matrix(to_check[[i]])) {
            stop(paste("\n", names(to_check)[i], "is not a matrix object\n"))
        }
        if (anyNA(to_check[[i]])) {
            cols_with_NA <- apply(to_check[[i]], FUN = anyNA, MARGIN = 2)
            name_cols_with_NA <- names(cols_with_NA)[cols_with_NA]
            warning(sprintf("Removing units with NAs: %s", vec2string(name_cols_with_NA)))
            dataprep_list[[ names(to_check)[i] ]] <- to_check[[i]][, ! cols_with_NA]
        }
    }
    matrix_row_counts0 <- vapply(list(dataprep_list$Y_t0, dataprep_list$Y_c0),
        nrow, FUN.VALUE = integer(1))
    if (n_distinct(matrix_row_counts0) > 1) {
        stop("\n Different number of periods isn't allowed: ",
            vec2string(matrix_row_counts0, quoted = FALSE))
    }
    matrix_row_counts1 <- vapply(list(dataprep_list$Y_t1, dataprep_list$Y_c1),
        nrow, FUN.VALUE = integer(1))

    if (n_distinct(matrix_row_counts1) > 1) {
        stop("\n Different number of periods isn't allowed: ",
            vec2string(matrix_row_counts1, quoted = FALSE))
    }
    if (any(matrix_row_counts0 == 0)) {
        stop("No pre-periods specified")
    }
    if (any(matrix_row_counts1 == 0)) {
        warning("No post-periods specified")
    }

    if (ncol(dataprep_list$Y_t0) != 1 || ncol(dataprep_list$Y_t1) != 1) {
        stop("\n Please specify only one treated unit: Y_t0 must have ncol = 1")
    }
    if (ncol(dataprep_list$Y_c0) < 2 || ncol(dataprep_list$Y_c1) < 2) {
        stop("\n Please specify only one treated unit: Y_c0 must have ncol >= 2")
    }
    return(dataprep_list)
}


constrained_regression <- function(dataprep_list, verbose = FALSE,
        adding_up = TRUE, no_intercept = FALSE,
        penalty_param = list(l0 = 0, l1 = 0, l2 = 0), ...) {
    stopifnot(is.logical(adding_up), is.logical(no_intercept))
    if (! adding_up || no_intercept) {
        stop('not implemented yet')
    }
    # dataprep_list <- check_matrices(dataprep_list)

    for (wt in c('l0', 'l1', 'l2')) {
        if (! wt %in% penalty_param) {
            penalty_param[[wt]] <- 0
        }
    }

    # This is for the case with adding_up = TRUE and no_intercept = FALSE.
    # It would be great if I could figure out how to use quadratic programming here.
    # Oh well.
    Ncontrol <- ncol(dataprep_list$Y_c0)
    starting_weights <- rep(1 / Ncontrol, Ncontrol)
    starting_mu <- 0
    # Add a column of ones for the intercept
    Y_c0 <- cbind(1, dataprep_list$Y_c0)
    Y_t0 <- dataprep_list$Y_t0


    penalize_weights <- function(par) {
        # Remember that we're also forcing these weights to add to 1, so it's not particularly
        # useful to penalize the l1 (abs value) metric. Instead I'll penalize l0 (sign).
        omegas <- par[-1]
        penalty <- penalty_param$l2 * sum(omegas ^ 2) +
                   penalty_param$l1 * sum(abs(omegas)) +
                   penalty_param$l0 * sum(omegas > 0)
        return(penalty)
    }

    objective_fn <- function(par) {
        # Rely on matrices defined above.
        # TODO these Ys should maybe be standardized
        par <- as.matrix(par)
        par_Y_c0 <- Y_c0 %*% par
        sum_sq <- crossprod(Y_t0 - par_Y_c0) %>% as.vector() %>% unname
        penalty <- penalize_weights(par)
        return(sum_sq + penalty)
    }

    grad_fn <- function(par) {
        grad <- -2 * crossprod(Y_t0, Y_c0) + 2 * par %*% crossprod(Y_c0) %>%
            as.vector() %>% unname()

        # This part is probably lost in the noise compared with the grad vector.
        # The derivative of the l0 term is zero, but discontinuous at zero.
        omegas <- par[-1]
        elastic_grad <- c(0, 2 * penalty_param$l2 * omegas + penalty_param$l1)
        return(grad + elastic_grad)
    }

    eval_g_eq <- function(par) {  # equality constraint fn
        omegas <- par[-1]
        # Constraint is that the sum of weights (omegas) must be one.
        constr <- sum(omegas) - 1
        # Increasing any weight by 1 increases the sum by 1.
        # Increasing the intercept (first element) by 1 increases the sum of weights not at all.
        grad <- c(0, rep(1, length(omegas)))
        return(list("constraints" = constr, "jacobian" = grad))
    }

    opt_res <- nloptr(
        x0 = c(starting_mu, starting_weights),
        eval_f = objective_fn,
        eval_grad_f = grad_fn,
        lb = c(-Inf, rep(0, Ncontrol)),
        ub = c( Inf, rep(1, Ncontrol)),
        eval_g_eq = eval_g_eq,
        ...,
        opts = list(algorithm = "NLOPT_LD_AUGLAG", xtol_rel = 1e-4, maxeval = 1e6,
            local_opts = list(algorithm = "NLOPT_LD_MMA", xtol_rel = 1.0e-7)
        )
    )
    # Algorithms that work with equality constraints:
        # NLOPT_LD_AUGLAG, NLOPT_LN_AUGLAG, NLOPT_LD_AUGLAG_EQ, NLOPT_LN_AUGLAG_EQ, NLOPT_GN_ISRES, NLOPT_LD_SLSQP
    if (verbose) {
        print(opt_res)
    }
    if (opt_res$status < 0) {
        stop(paste("NLopt didn't find a solution\n", opt_res$message))
    }

    solution <- opt_res$solution
    stopifnot(! anyNA(solution))
    omega <- solution[-1]
    names(omega) <- colnames(dataprep_list$Y_c0)
    very_small_weights <- omega[omega > 0 & omega < 1e-3]
    if (length(very_small_weights) > 0) {
        message(sprintf("Some controls (%s) got small positive weights. You may want to adjust the penalty_param.",
            vec2string(names(very_small_weights))))
    }
    out <- list(mu = solution[1], omega = omega)
    return(out)
}

make_synthetic_alaska <- function(dataprep_list, sales_counts, optimized_coef, time_variable = 'event_week') {
    mu <- optimized_coef$mu
    omega <- optimized_coef$omega
    # dataprep_list <- check_matrices(dataprep_list)
    synth0 <- mu + dataprep_list$Y_c0 %*% omega
    synth1 <- mu + dataprep_list$Y_c1 %*% omega
    synth_df0 <- data_frame(time = as.numeric(dataprep_list$time_col_pre),
                            synth_ak = as.numeric(synth0))
    synth_df1 <- data_frame(time = as.numeric(dataprep_list$time_col_post),
                            synth_ak = as.numeric(synth1))
    real_alaska <- sales_counts %>% filter(buy_state == 'AK') %>%
        rename_('time' = time_variable) %>%
        select(time, sale_count) %>%
        rename(real_count = sale_count)

    synth_df <- bind_rows(synth_df0, synth_df1) %>% ensure_id_vars(time) %>%
        full_join(real_alaska, by = 'time') %>%
        reshape2::melt(id.vars = 'time',
            variable.name = "synth", value.name = "sale_count") %>%
        as.tbl() %>%
        mutate(synth = as.character(synth)) %>%
        fill_tbl()
    plt <- ggplot(synth_df, aes(x = time, y = sale_count, color = synth)) +
        geom_line() +
        PLOT_THEME
    print(plt)
    invisible(synth_df)
}

if (! exists('sales_counts')) {

    sales_counts <- auctions %>%
        select(sale_date, buy_state, sales_pr, vin_pattern, msrp) %>%
        filter(! is.na(buy_state), ! buy_state == c('VI')) %>%
        filter_event_window(years = 2013, days_before = 70,
                            days_after = 70) %>%
        add_event_time() %>%
        get_sales_counts(date_var = 'event_time', id_var = 'buy_state') %>%
        collect(n = Inf)

    #
    #  <- pull_data_one_year(2013, days_before = 70) %>%
    #     select(sale_date, buy_state, buyer_id, sales_pr) %>%
    #     add_event_time() %>% add_sale_year() %>%
    #     get_sales_counts('event_time', summarize_vars = 'sale_count')

}
x <- dataprep(sales_counts, dependent = 'sale_count', unit_variable = 'buy_state',
        time_variable = 'event_time', treated_unit = 'AK')
params <- constrained_regression(x, verbose = TRUE, penalty_param = list(l0 = 10) )
synth_df <- make_synthetic_alaska(x, sales_counts, params, 'event_time')
