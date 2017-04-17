

source('r_defaults.r')  # set common parameters and call common_functions.r
# install_lazy('Synth')
stopifnot(is_pkg_installed('dplyr', '0.5.0'))  # dplyr 0.6.0 is going to change things that I don't want to change yet.

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

    # There are some state-week pairs without observations. We can either fill those
    # with zeros (which only make sense some of the time), or only keep states that have
    # observations for all weeks.
    fill_zeros_to_balance <- dependent %in% c('sale_count', 'sales_pr_mean', 'sale_tot')
    if (fill_zeros_to_balance) {
        df <- force_panel_balance(df, c(unit_variable, time_variable), fill_na = TRUE)
    } else {
        # First, cut df down to the places we have our treated unit.
        df_treated_time <- filter_units(df, treated_unit)[[time_variable]] %>%
            unique()
        df <- filter_time(df, df_treated_time)

        # Now, keep only complete cases:
        df <- df %>% group_by_(unit_variable) %>% mutate(.count = n())
        max_row_counts <- max(df$.count)
        # print(filter(df, .count !=  max_row_counts) %>% select(buy_state))
        df <- filter(df, .count == max_row_counts) %>%
            select(-.count) %>%
            ungroup()
    }
    # In the default case with control_units == NULL, make all non-treatment into possible
    # control units.
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
    if (! treated_unit %in% df[[unit_variable]]) {
        stop('Treated unit not in data')
    }
    if (! all(df[[unit_variable]] %in% c(control_units, treated_unit))) {
        stop("Data contain units not in treated or control (this is a bug)")
    }
    if (! all(control_units %in% df[[unit_variable]])) {
        bad_control_unit_idx <- which(! control_units %in% df[[unit_variable]])

        message("Some controls requested are not present: ",
            vec2string(control_units[bad_control_unit_idx]))
        control_units <- control_units[-bad_control_unit_idx]
    }

    if (anyNA(df)) {
        stop("NAs in the data!")
    }
    if (! all(time_optimize_ssr %in% df[[time_variable]])) {
        stop("Specified time periods not in the data")
    }

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
        setNames(treated_unit) %>%
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


constrained_regression <- function(prepped_matrices, verbose = FALSE,
        adding_up = TRUE, no_intercept = FALSE,
        penalty_param = list(l0 = 0, l1 = 0, l2 = 0), ...,
        algo = 'ISRES', l0_break = 0.0001) {
    stopifnot(is.logical(adding_up), is.logical(no_intercept))
    if (! adding_up || no_intercept) {
        stop('not implemented yet')
    }
    # lo_break is the point used to make the l0 penalty continuouly differentiable
    # (by making the penalty function quadratic in the interval [0, l0_break])
    for (wt in c('l0', 'l1', 'l2')) {
        if (! wt %in% penalty_param) {
            penalty_param[[wt]] <- 0
        }
    }


    # This is for the case with adding_up = TRUE and no_intercept = FALSE.
    # It would be great if I could figure out how to use quadratic programming here.
    # Oh well.
    Ncontrol <- ncol(prepped_matrices$Y_c0)
    starting_weights <- rep(1 / Ncontrol, Ncontrol)
    starting_mu <- 0
    # Add a column of ones for the intercept
    Y_c0 <- cbind(1, prepped_matrices$Y_c0)
    Y_t0 <- prepped_matrices$Y_t0
    stopifnot(is.matrix(Y_c0), is.matrix(Y_t0), nrow(Y_t0) == nrow(Y_c0))

    penalize_weights <- function(omegas) {
        # Remember that we're also forcing these weights to add to 1, so it's not particularly
        # useful to penalize the l1 (abs value) metric. Instead I'll penalize l0 (sign).
        # penalize_weights takes already-normalized omegas

        # But, to make things continuously differentiable, I'm going to implement l0
        # as a quadratic with its max at l0_break (0.0001)
        # Multiply to get the max up to penalty_param$l0.
        # penalty_param$l0 if x >= 0.0001.
        penalty_l0 <- penalty_param$l0 * sum(
            if_else(omegas < l0_break, (l0_break^(-2) * (omegas - l0_break)^2 + 1), 1))
        penalty <- penalty_param$l2 * sum(omegas ^ 2) +
                   penalty_param$l1 * sum(abs(omegas)) +
                   penalty_l0
        return(penalty)
    }

    objective_fn <- function(par) {
        # Rely on matrices defined above.
        # Build the sum-to-one constraint into the function.
        omegas <- par[-1]

        if (sum(omegas) < 1e-14) {
            # message('picked omega = 0')
            # Very strongly penalize picking (close to) all zeros, since
            # that will be trouble when we divide by the sum.
            return(.Machine$double.xmax)
        }
        omegas <- omegas / sum(omegas)
        par <- as.matrix(c(par[1], omegas))
        # TODO these Ys should maybe be standardized
        par_Y_c0 <- Y_c0 %*% par
        # Objective is sum of squares:
        # (Y_t0 - mu - omega * Y_c0)' * (Y_t0 - mu - omega * Y_c0)
        sum_sq <- crossprod(Y_t0 - par_Y_c0) %>% as.vector() %>% unname
        penalty <- penalize_weights(omegas)
        out <- sum_sq + penalty
        if (! is.finite(out)) {
            # stop("Infinite values for ", par)
            return(.Machine$double.xmax)
            # return(.Machine$integer.max - 1)
        }
        return(out)
    }
    # Compute these outside grad_fn (for speed) because they don't change.
    Y_t0_times_Y_c0 <- crossprod(Y_t0, Y_c0)
    Y_c0_times_Y_c0 <- crossprod(Y_c0)
    grad_fn <- function(par) {
        omegas <- par[-1]
        if (sum(omegas) < 1e-15) {
            # If we pick (close to) all zeros, provide a gradient that points
            # away (increasing any omega lowers the value of fn)
            return(c(0, rep(-1e3, length(omegas))))
        }
        omegas <- omegas / sum(omegas)
        par <- c(par[1], omegas)

        grad <- -2 * Y_t0_times_Y_c0 + 2 * par %*% Y_c0_times_Y_c0 %>%
            as.vector() %>% unname()

        # This part is probably lost in the noise compared with the grad vector.
        # The derivative of the l0 term is zero, except for the part I've added to make it
        # continuous.
        # omegas <- par[-1]
        l0_grad <- if_else(omegas < l0_break,
            -2 * penalty_param$l0 * l0_break^(-2) * (omegas - l0_break), 0)
        penalty_grad <- c(0, 2 * penalty_param$l2 * omegas + penalty_param$l1 + l0_grad)
        return(grad + penalty_grad)
    }

    # Get the min and max across all columns and all timer periods.
    # The difference in means shouldn't be larger than that.
    data_max <- max(vapply(prepped_matrices, max, numeric(1)))
    data_min <- min(vapply(prepped_matrices, min, numeric(1)))
    mu_bound <- max(abs(c(data_max, data_min)))
    par_lower_bound <- c(-mu_bound, rep(0, Ncontrol))
    par_upper_bound <- c( mu_bound, rep(1, Ncontrol))
    # algo <- 'genoud'

    # algo <- 'DIRECT'
    if (algo == 'genoud') {
        library(rgenoud)
        bounds <- matrix(c(par_lower_bound, par_upper_bound), nrow = Ncontrol + 1)
        # Very slow, haven't run to completion.
        global_opt_res <- genoud(
            objective_fn,
            nvars = Ncontrol + 1,
            # gr = grad_fn,  # doesn't work well with grad_fn, not sure why.
            max = FALSE,
            Domains = bounds,
            boundary.enforcement = 2,
            print.level = 0,
            max.generations = 500
            )
        return(global_opt_res)
    } else if (algo == 'DIRECT') {
        global_opt_res <- nloptr(
            x0 = c(starting_mu, starting_weights),
            eval_f = objective_fn,
            # eval_grad_f = grad_fn,
            lb = par_lower_bound,
            ub = par_upper_bound,
            ...,
            # opts = list(algorithm = "NLOPT_LN_AUGLAG", ftol_rel = 1e-5, maxeval = 1e5,
            #     local_opts = list(algorithm = "NLOPT_LN_SBPLX", ftol_rel = 1.0e-7)
            # Gives bad results (maybe better with xtol)
            # opts = list(algorithm = "NLOPT_GN_DIRECT_L", ftol_rel = 1e-5, maxeval = 1e5)
            # Pretty slow:
            opts = list(algorithm = "NLOPT_GN_DIRECT", xtol_rel = 1e-3, maxeval = 1e7)
            # Seems to work:
            # opts = list(algorithm = "NLOPT_GN_ISRES", ftol_rel = 1e-4, maxeval = 1e7)
            # This one is unbelievably slow and takes too much memory:
            # opts = list(algorithm = "NLOPT_GD_STOGO_RAND", ftol_rel = 1e-5, maxeval = 1e7)
            # Gives bad results:
            # opts = list(algorithm = "NLOPT_GN_DIRECT", ftol_rel = 1e-8, maxeval = 1e7)
            # Gives bad results:
            # opts = list(algorithm = "NLOPT_GN_CRS2_LM", ftol_rel = 1e-5, maxeval = 1e7)
            # Way to slow:
            # opts = list(algorithm = "NLOPT_GD_MLSL_LDS", ftol_rel = 1e-5, maxeval = 1e7,
            #     local_opts = list(algorithm = "NLOPT_LD_MMA", ftol_rel = 1.0e-7))
            # Gives bad results:
            )
    } else if (algo == 'ISRES') {
        global_opt_res <- nloptr(
            x0 = c(starting_mu, starting_weights),
            eval_f = objective_fn,
            lb = par_lower_bound,
            ub = par_upper_bound,
            ...,
            # Seems to work:
            opts = list(algorithm = "NLOPT_GN_ISRES", ftol_rel = 1e-4, maxeval = 1e7)
            )
    } else {
        stop("bad algorithm")
    }
    if (verbose) {
        message("Global optimization results:\n")
        print(global_opt_res)
    }
    if (global_opt_res$status < 0 || global_opt_res$status %in% c(5, 6)) {
        if (! verbose) {  # because otherwise we just printed.
            print(global_opt_res)
        }
        stop("NLopt failed to find a solution:\n ", global_opt_res$message)
    }
    # Now 'polish' the result:
    # http://ab-initio.mit.edu/wiki/index.php/NLopt_Algorithms#Global_optimization
    opt_res <- nloptr(
        x0 = global_opt_res$solution,
        eval_f = objective_fn,
        # eval_grad_f = grad_fn,
        lb = par_lower_bound,
        ub = par_upper_bound,
        ...,
        opts = list(algorithm = "NLOPT_LN_SBPLX", ftol_rel = 1e-7, maxeval = 1e8)
        # check_derivatives = TRUE, check_derivatives_print = 'errors')
        )
    if (verbose) {
        message("Local polishing optimization results:\n")
        print(opt_res)
    }
    if (opt_res$status < 0 || opt_res$status %in% c(5, 6)) {
        if (! verbose) {  # because otherwise we just printed.
            print(opt_res)
        }
        stop("NLopt failed to find a solution:\n ", opt_res$message)
    }
    solution <- opt_res$solution
    stopifnot(! anyNA(solution))
    omega <- solution[-1]
    omega <- omega / sum(omega)
    names(omega) <- colnames(prepped_matrices$Y_c0)

    unchanged_weights <- omega[abs(omega -  starting_weights) < 1e-10]
    if (length(unchanged_weights) > 0) {
        stop("Program did not change weights for ",
            length(unchanged_weights), " units.")
    }
    if (penalty_param$l0 > 0) {
        very_small_weights <- omega[omega > 0 & omega < (l0_break * .67)]
        if (length(very_small_weights) > 0) {
            if (verbose) {
                message("Zeroing out weights for ", vec2string(names(very_small_weights)))
            }
            # use if_else to keep the same vector length.
            non_small_weights <- if_else(omega >= (l0_break * .67), omega, 0)

            # Redistribute away from these very small weights. I view them as artifacts
            # of the l0_break. Remember that non_small_weights and very_small_weights sum
            # to 1. (And enforce that in the stopifnot.)
            sum_small_weights <- sum(very_small_weights)
            sum_large_weights <- sum(non_small_weights)
            stopifnot(any(non_small_weights > 0),
                      abs(sum_large_weights + sum_small_weights - 1) < 1e-8)
            omega <- non_small_weights * (1 + sum_small_weights) / sum_large_weights
        }
    } else {
        very_small_weights <- omega[omega > 0 & omega < 1e-4]
        if (length(very_small_weights) > 0) {
            message(sprintf("Some controls (%s) got small positive weights. You may want to adjust the penalty_param.",
                vec2string(names(very_small_weights))))
        }
    }
    out <- list(mu = solution[1], omega = omega)
    return(out)
}


constrained_regression_explicit_eq_constraint <- function(dataprep_list, verbose = FALSE,
        adding_up = TRUE, no_intercept = FALSE,
        penalty_param = list(l0 = 0, l1 = 0, l2 = 0), ...) {
    stop("This function is old")
    stopifnot(is.logical(adding_up), is.logical(no_intercept))
    if (! adding_up || no_intercept) {
        stop('not implemented yet')
    }

    for (wt in c('l0', 'l1', 'l2')) {
        if (! wt %in% penalty_param) {
            penalty_param[[wt]] <- 0
        }
    }
    if (penalty_param$l0 != 0) {
        # TODO: make it continuous by having a quadratic or something smoother near zero.
        stop("Having an l0 penalty makes the problem discontinuous, which Karl hasn't fixed yet.")
    }
    l0_break <- 0.0001  # the point used to make the l0 penalty continuouly differentiable

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
        # penalize_weights takes already-normalized omegas
        omegas <- par[-1]

        # But, to make things continuously differentiable, I'm going to implement l0
        # as a quadratic with its max at l0_break (0.0001)
        # Multiply to get the max up to penalty_param$l0.
        # penalty_param$l0 if x >= 0.0001.
        penalty_l0 <- penalty_param$l0 * sum(
            if_else(omegas < l0_break, (l0_break^(-2) * (omegas - l0_break)^2 + 1), 1))
        penalty <- penalty_param$l2 * sum(omegas ^ 2) +
                   penalty_param$l1 * sum(abs(omegas)) +
                   penalty_l0
        return(penalty)
    }

    objective_fn <- function(par) {
        # Rely on matrices defined above.
        # Build the sum-to-one constraint into the function.
        par <- as.matrix(par)
        # TODO these Ys should maybe be standardized
        par_Y_c0 <- Y_c0 %*% par
        # Objective is sum of squares:
        # (Y_t0 - mu - omega * Y_c0)' * (Y_t0 - mu - omega * Y_c0)
        sum_sq <- crossprod(Y_t0 - par_Y_c0) %>% as.vector() %>% unname
        penalty <- penalize_weights(par)
        return(sum_sq + penalty)
    }

    # Compute these outside grad_fn (for speed) because they don't change.
    Y_t0_times_Y_c0 <- crossprod(Y_t0, Y_c0)
    Y_c0_times_Y_c0 <- crossprod(Y_c0)
    grad_fn <- function(par) {
        grad <- -2 * Y_t0_times_Y_c0 + 2 * par %*% Y_c0_times_Y_c0 %>%
            as.vector() %>% unname()
        # This part is probably lost in the noise compared with the grad vector.
        # The derivative of the l0 term is zero, except for the part I've added to make it
        # continuous.
        omegas <- par[-1]
        l0_grad <- if_else(omegas < l0_break,
            -2 * penalty_param$l0 * l0_break^(-2) * (omegas - l0_break), 0)
        penalty_grad <- c(0, 2 * penalty_param$l2 * omegas + penalty_param$l1 + l0_grad)
        return(grad + penalty_grad)
    }
    # Note: there's a way to reformulate the problem in a way that
    # builds in the linear equality constraint:
    # [0, 1, 1, 1, ..., 1]' %*% par = 1
    eval_g_eq <- function(par) {  # equality constraint fn
        omegas <- par[-1]
        # Constraint is that the sum of weights (omegas) must be one.
        constr <- sum(omegas) - 1
        # Increasing any weight by 1 increases the sum by 1.
        # Increasing the intercept (first element) by 1 increases the sum of weights not at all.
        grad <- c(0, rep(1, length(omegas)))
        # return(list("constraints" = constr, "jacobian" = grad))
        return(list("constraints" = constr))
    }
    # Get the min and max across all columns and all timer periods.
    # The difference in means shouldn't be larger than that.
    data_max <- max(vapply(dataprep_list, max, numeric(1)))
    data_min <- min(vapply(dataprep_list, min, numeric(1)))
    mu_bound <- max(abs(c(data_max, data_min)))

    # Here's a non-gradient method that seems to work.
    opt_res <- nloptr(
        x0 = c(starting_mu, starting_weights),
        eval_f = objective_fn,
        lb = c(-mu_bound, rep(0, Ncontrol)),
        ub = c( mu_bound, rep(1, Ncontrol)),
        ...,
        eval_g_eq = eval_g_eq,
        opts = list(algorithm = "NLOPT_GN_ISRES", ftol_abs = 1e-2, maxeval = 1e7)
    )

    if (verbose) {
        print(opt_res)
    }
    if (opt_res$status < 0 || opt_res$status %in% c(5, 6)) {
        if (! verbose) {
            print(opt_res)
        }
        stop("NLopt failed to find a solution:\n ", opt_res$message)
    }

    solution <- opt_res$solution
    stopifnot(! anyNA(solution))
    omega <- solution[-1]
    names(omega) <- colnames(dataprep_list$Y_c0)

    unchanged_weights <- omega[abs(omega -  starting_weights) < 1e-10]
    if (length(unchanged_weights) > 0) {
        stop("Program did not change weights for ",
            length(unchanged_weights), " units.")
    }
    if (abs(sum(omega) - 1) > 1e-8) {
        stop("Weights should sum to 1 but actually sum to ", sum(omega))
    }

    if (penalty_param$l0 > 0) {
        very_small_weights <- omega[omega > 0 & omega < (l0_break * .67)]
        if (length(very_small_weights) > 0) {
            # use if_else to keep the same vector length.
            non_small_weights <- if_else(omega >= (l0_break * .67), omega, 0)

            # Redistribute away from these very small weights. I view them as artifacts
            # of the l0_break. Remember that non_small_weights and very_small_weights sum
            # to 1. (And enforce that in the stopifnot.)
            sum_small_weights <- sum(very_small_weights)
            sum_large_weights <- sum(non_small_weights)
            stopifnot(any(non_small_weights > 0),
                      abs(sum_large_weights + sum_small_weights - 1) < 1e-8)
            omega <- non_small_weights * (1 + sum_small_weights) / sum_large_weights
        }
    } else {
        very_small_weights <- omega[omega > 0 & omega < 1e-4]
        if (length(very_small_weights) > 0) {
            message(sprintf("Some controls (%s) got small positive weights. You may want to adjust the penalty_param.",
                vec2string(names(very_small_weights))))
        }
    }
    out <- list(mu = solution[1], omega = omega)
    return(out)
}

make_synthetic_alaska <- function(years, optimized_coef, dependent = 'sale_count', time_variable = 'event_week') {
    stopifnot(is.numeric(years), length(years) >= 1, length(optimized_coef) == 2,
        setequal(names(optimized_coef), c('mu', 'omega')),
        is.character(dependent), length(dependent) == 1, is.character(time_variable),
        length(time_variable) == 1)
    mu <- optimized_coef$mu
    omega <- optimized_coef$omega
    treated_unit <- 'AK'
    calculate_synth_one_year <- function(yr) {
        # This function relies on constants defined in the parent function.

        real_data <- pull_data_once(yr, date_var = time_variable, dependent = dependent)
        control_units <- intersect(names(omega), unique(real_data$buy_state))
        prepped_matrices <- dataprep(real_data, dependent = dependent, unit_variable = 'buy_state',
            time_variable = time_variable, treated_unit = treated_unit,
            # Only use the ones we've estimated weights for (the intersection of
            # complete cases across years)
            control_units = control_units)
        control_units2 <- intersect(names(omega), colnames(prepped_matrices$Y_c0))
        omega <- omega[control_units2]
        Y_c0 <- prepped_matrices$Y_c0[, control_units2]
        Y_c1 <- prepped_matrices$Y_c1[, control_units2]
        synth0 <- mu + Y_c0 %*% omega
        synth1 <- mu + Y_c1 %*% omega
        synth_df0 <- data_frame(time = as.numeric(prepped_matrices$time_col_pre),
                                synth_ak = as.numeric(synth0))
        synth_df1 <- data_frame(time = as.numeric(prepped_matrices$time_col_post),
                                synth_ak = as.numeric(synth1))
        real_data <- real_data %>% filter(buy_state == treated_unit) %>%
            rename_('time' = time_variable) %>%
            select_(.dots = c('time', dependent)) %>%
            rename_('real' = dependent)

        synth_df <- bind_rows(synth_df0, synth_df1) %>%
            ensure_id_vars(time) %>%
            full_join(real_data, by = 'time') %>%
            reshape2::melt(id.vars = 'time',
                variable.name = "synth", value.name = 'dep_value') %>%
            as.tbl() %>%
            mutate(synth = as.character(synth)) %>%
            fill_tbl() %>%
            mutate(year = yr)
        return(synth_df)
    }

    synth_df <- lapply_bind_rows(years, calculate_synth_one_year, parallel_cores = 1)
    return(synth_df)
}

plot_synth_alaska <- function(synth_df, labels = labs(color = ''), filename = NULL) {
    stopifnot(all(c('time', 'synth', 'dep_value') %in% names(synth_df)))
    # First, if we have multiple years, make an average.
    to_plot <- synth_df %>% group_by(time, synth) %>%
        summarize(dep_value = mean(dep_value)) %>%
        ungroup() %>%
        mutate(synth = factor(synth, levels = c('real', 'synth_ak'),
            labels = c('True AK', 'Synth. AK')))
    plt <- ggplot(to_plot, aes(x = time, y = dep_value, color = synth)) +
        geom_line() +
        PLOT_THEME +
        labels
    if (! is.null(filename)) {
        save_plot(plt, filename)
    }
    return(plt)
}


run_for_2013 <- function(weekly = TRUE) {
    if (weekly) {
        date_var <- 'event_week'
    } else {
        date_var <- 'event_time'
    }
    sales_counts <- pull_data_once(2013, weekly = weekly)
    #
    #  <- pull_data_one_year(2013, days_before = 70) %>%
    #     select(sale_date, buy_state, buyer_id, sales_pr) %>%
    #     add_event_time() %>% add_sale_year() %>%
    #     get_sales_counts('event_time', summarize_vars = 'sale_count')

    x <- dataprep(sales_counts, dependent = 'sale_count',
        unit_variable = 'buy_state', time_variable = date_var,
        treated_unit = 'AK')
    params <- constrained_regression(x, verbose = TRUE, penalty_param = list(l0 = 5))
    # synth_df <- make_synthetic_alaska(x, sales_counts, params, date_var)
    synth_df <- make_synthetic_alaska(2013, params, dependent = 'sale_count', time_variable = date_var)
    out <- list(param = params, synth_df = synth_df)
    return(out)
}


run_multiyear <- function(years, date_var = 'event_week', dependent = 'sale_count') {
    matrix_list <- dataprep_multiyear(years = years,
        dependent = dependent,
        unit_variable = 'buy_state', time_variable = date_var,
        treated_unit = 'AK')
    params <- constrained_regression(matrix_list, verbose = TRUE,
        penalty_param = list(l0 = 5))
    synth_df <- make_synthetic_alaska(years, params,
        dependent = dependent, time_variable = date_var)
    out <- list(param = params, synth_df = synth_df)
    return(out)
}


pull_data_once_unmemoized <- function(one_year, date_var = 'event_week', dependent = 'sale_count') {
    stopifnot(date_var %in% c('event_week', 'event_time'))

    partial_res <- auctions %>%
        select(sale_date, buy_state, sales_pr, vin_pattern, msrp) %>%
        # Drop VI and PR because they're noisy and probably have other things going on.
        filter(! is.na(buy_state), ! buy_state %in% c('VI', 'PR')) %>%
        filter_event_window(years = one_year, days_before = 70,
                            days_after = 70) %>%
        add_event_time()

    if (dependent %in% c("sale_count", "sale_tot", "sales_pr_mean_log", "sales_pr_mean",
        "msrp_mean", "msrp_mean_log", "sale_count_log", "sale_tot_log")) {
        out <- partial_res %>%
            get_sales_counts(date_var = date_var, id_var = 'buy_state') %>%
            select_(.dots = 'buy_state', date_var, dependent)
    } else if (dependent %in% c("fuel_cons", "fuel_cons_log")) {
        out <- partial_res %>%
            get_sales_efficiency(date_var = date_var, id_var = 'buy_state') %>%
            select_(.dots = 'buy_state', date_var, dependent)
    }
    return(collect(out, n = Inf))
}
if (! existsFunction('pull_data_once') || ! is.memoized(pull_data_once)) {
    pull_data_once <- memoize(pull_data_once_unmemoized)
}

dataprep_multiyear <- function(years, dependent = 'sale_count', ...) {

    pulled_data <- lapply(years, pull_data_once, dependent = dependent)
    datapreped_list <- lapply(pulled_data, dataprep, dependent = dependent, ...)
    # Get the control units that are present for every year.
    control_units <- lapply(datapreped_list, function(x) colnames(x$Y_c0)) %>%
        purrr::reduce(intersect)

    # Then do the dataprep again with those units.
    # (this will raise an error if control_units is part of "...")
    datapreped_list <- lapply(pulled_data, dataprep, dependent = dependent,
        control_units = control_units, ...)
    .extract_and_bind <- function(mat_name, datapreped_list) {
        mats_list <- lapply(datapreped_list, function(x) x[[mat_name]])
        return(do.call(rbind, mats_list))
    }
    # For each matrix Y_c1, Y_c0, etc, pull it out of the pulled_data
    # list and rbind to make one tall matrix for each.
    # Then repackage as a list with the same names.
    matrix_list <- lapply(names(datapreped_list[[1]]), .extract_and_bind,
        datapreped_list = datapreped_list) %>%
        setNames(names(datapreped_list[[1]]))

    return(matrix_list)
}

# We might be interested in how the weights change from year to year or between dependent
# variables.
compare_weights <- function(years = 2002:2014, date_var = 'event_week', dependent = 'sale_count') {
    # database backend etc etc, harder to parallelize.
    pulled_data <- lapply(years, pull_data_once,
        date_var = date_var, dependent = dependent)
    # Matrix reshaping is fast; not worth parallelizing
    prepped_list_by_year <- lapply(pulled_data, dataprep,
        time_variable = date_var, dependent = dependent,
        unit_variable = 'buy_state', treated_unit = 'AK')
    # Definitely parallelize the optimization.
    params_list <- lapply_parallel(prepped_list_by_year, constrained_regression,
        penalty_param = list(l0 = 5))
    return(params_list)
}

weights_to_table <- function(synth_outcomes) {
    make_one_df <- function(varname) {
        omega <- synth_outcomes[[varname]]$param$omega
        omega <- omega[omega > 0]
        df <- tibble::enframe(omega[omega > 0], name = 'state', value = varname)
    }
    weights_df <- lapply(names(synth_outcomes), make_one_df) %>%
        purrr::reduce(full_join, by = 'state') %>%
        fill_tbl() %>%
        arrange(state)
    return(weights_df)
}

plot_weight_comparisons <- function(synth_outcomes,
        to_compare = c('sale_count', 'fuel_cons')) {
    stopifnot(all(to_compare %in% names(synth_outcomes)))
    synth_pred_own_weights <- lapply_bind_rows(to_compare, function(varname) {
        synth_outcomes[[varname]]$synth_df %>%
        mutate(pred_var = varname,
            # Here (and only here), don't filter  out synth == 'real', so we can
            # have a reality line for synth_var
            synth_var = if_else(synth == 'real', 'real', varname)) %>%
            select(-synth) %>% return()
        }, parallel_cores = 1)

    # next, get cross-variable predictions
    n_cross_var_pred <- (length(synth_outcomes) - 1) * (length(to_compare) - 1)
    if (n_cross_var_pred <= 0) {
        stop("Not enough variables provided to do cross-variable prediction. (",
            length(synth_outcomes), " outcomes and ", length(to_compare),
            " comparison variables were provided.)")
    }
    synth_pred_cross_weights_list <- vector(mode = 'list', length = n_cross_var_pred)
    i <- 1
    for (synth_var in names(synth_outcomes)) {
        for (pred_var in to_compare) {
            if (pred_var == synth_var) {
                next
            }
            # synth_var is the parameter set we're using to predict
            # pred_var is the actual value we're looking at
            synth_pred_cross_weights_list[[i]] <- make_synthetic_alaska(years = 2002:2014,
                optimized_coef = synth_outcomes[[synth_var]]$param,
                dependent = pred_var, time_variable = 'event_week') %>%
                filter(synth == 'synth_ak') %>% select(-synth) %>%
                mutate(pred_var = pred_var, synth_var = synth_var) %>% return()
            i <- i + 1
        }
    }
    synth_pred_cross_weights <- bind_rows(synth_pred_cross_weights_list)
    # Make some parameter 'results' for uniform weights on the selected states

    dd_control_states <- c("VT", "WA", "WY", "MT", "IL", "NM", "NH", "WV", "WI", "DC")
    dd_omega <- tibble::enframe(synth_outcomes[[1]]$param$omega, name = 'state') %>%
        mutate(value = if_else(state %in% dd_control_states,
            1 / length(dd_control_states), 0)) %>%
        tibble::deframe()
    stopifnot(all(dd_control_states %in% names(dd_omega)))
    # mu = 0 because we're going to demean later, not because that's the actual
    # DD intercept
    dd_param <- list(mu = 0, omega = dd_omega)
    synth_pred_dd <- lapply_bind_rows(to_compare, # fill in everything else so
        # to_compare goes in as dependent.  This is poor style; too fragile.
        make_synthetic_alaska,
        years = 2002:2014,
        optimized_coef = dd_param,
        time_variable = 'event_week',
        parallel_cores = 1,
        rbind_src_id = 'pred_var') %>%
        filter(synth == 'synth_ak') %>% select(-synth) %>%
        mutate(synth_var = 'dd_even_weight')


    # First, if we have multiple years, make an average.
    to_plot <- bind_rows(synth_pred_cross_weights, synth_pred_own_weights,
            synth_pred_dd) %>%
        group_by(time, pred_var, synth_var) %>%
        # Take the mean across years, within each group
        summarize(dep_value = mean(dep_value)) %>%
        group_by(pred_var, synth_var) %>%
        # Scale the variables so we can compare on a common axis
        mutate(dep_value = scale(dep_value)) %>%
        ungroup() %>%
        filter(synth_var %in% c('real', 'dd_even_weight', 'sale_count', 'fuel_cons', 'sales_pr_mean')) %>%
        mutate(
            synth_var = factor(synth_var,
                levels = c('real', 'dd_even_weight', 'sale_count', 'fuel_cons', 'sales_pr_mean'),
                labels = c('Reality', 'DD weights', 'Sale count weights', 'Fuel cons. weights', 'Sale price weights')),
            pred_var = factor(pred_var,
                levels = c('sale_count', 'fuel_cons'),
                labels = c('Sale count', 'Fuel cons.')))
    plt <- ggplot(to_plot, aes(x = time, y = dep_value, color = synth_var,
            linetype = synth_var)) +
        geom_line() +
        facet_grid(pred_var ~ ., scales = 'free_y') +
        guides(linetype = 'none') +
        scale_color_manual(values = PALETTE_8_COLOR_START_WITH_BLACK) +
        PLOT_THEME +
        labs(x = 'Event weeks', y = 'Outcome (standardized)',
            color = 'Predicting variable', linetype = '')
    save_plot(plt, 'synth_cross_var_weights_comparison.pdf')
    return(plt)
}



main <- function() {
    outcome_vars <- c('sale_count', 'fuel_cons', 'sales_pr_mean', 'msrp_mean')

    synth_outcomes <- lapply(outcome_vars, run_multiyear,
        years = 2002:2014, date_var = 'event_week') %>%
        setNames(outcome_vars)

    print(weights_to_table(synth_outcomes))
    top_10_total_weight <- weights_to_table(synth_outcomes) %>% select(-state) %>%
        rowSums() %>% setNames(weights_to_table(synth_outcomes)$state) %>%
        tibble::enframe() %>% arrange(-value) %>% slice(1:10) %>% extract2('name')


    plt_fuel_cons <- plot_synth_alaska(synth_outcomes$fuel_cons$synth_df,
        labs(x = 'Event week', y = 'Fuel consumption (L/100km)', color = ''),
        filename = 'synthetic_fuel_cons.pdf')

    plt_sales_pr_mean <- plot_synth_alaska(synth_outcomes$sales_pr_mean$synth_df,
        labs(x = 'Event week', y = 'Mean sales price ($)', color = ''),
        filename = 'synthetic_sales_pr_mean.pdf')

    plt_msrp_mean <- plot_synth_alaska(synth_outcomes$msrp_mean$synth_df,
        labs(x = 'Event week', y = 'Mean MSRP ($)', color = ''),
        filename = 'synthetic_msrp_mean.pdf')


    par_sale_count <- compare_weights(dependent = 'sale_count')
    print(par_sale_count)
    par_fuel_cons  <- compare_weights(dependent = 'fuel_cons')
    print(par_fuel_cons)
}
# matrix_list <- dataprep_multiyear(2012:2013)
# res <- run_for_2013(weekly = FALSE)
# res <- run_multiyear(years = c(2012, 2013), dependent = 'fuel_cons')
