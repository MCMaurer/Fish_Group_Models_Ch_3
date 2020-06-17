library(tidyverse)
library(performance)

marginal_effects_plot <- function(model, effects, color = effects[2], ci_type = "band", ...){
  initial_conditions = list()
  
  # loop through all of your listed effects, *inside* the model's data, and generate initial conditions based on the values in your data
  for (x in 1:length(effects)) {
    initial_conditions[[effects[x]]] = unique(model$data[[effects[x]]])
  }
  
  # if you just have one effect, just use its name for effects
  if(length(effects) == 1){
    e <- marginal_effects(model, effects = effects, int_conditions = initial_conditions, ... = ...)
  } else {
    # otherwise, you have to paste your effect names together
    e <- marginal_effects(model, effects = paste0(effects[1],":",effects[2]), int_conditions = initial_conditions, ... = ...)
  }
  
  response <- attributes(e[[1]])$response
  if(response == "hu"){
    response <- "probability of\ncrossing hurdle"
  }
  
  # pull out the dataframe from the marginal effects call
  e <- e[[1]]
  
  effects_factors <- e %>% 
    select(effects) %>% 
    select_if(is.factor) %>% 
    names()
  
  e <- e %>%
    mutate_at(effects_factors, fct_inorder)
  
  if(response == "probability of\ncrossing hurdle"){
    e <- e %>% 
      mutate(estimate__ = 1 - estimate__,
             lower__ = 1 - lower__,
             upper__ = 1 - upper__)
  }
  
  if(length(effects) == 1){
    e %>% 
      ggplot(aes(x= !!sym(effects[1]), y=estimate__), color= "black") +
      geom_line() +
      geom_ribbon(aes(ymin=lower__, ymax=upper__), fill = "black", 
                  color = "transparent", alpha = 0.1) +
      ylab(response)
    
  } else{
    
    p <- e %>% 
      ggplot(aes(x= !!sym(effects[1]), y=estimate__, color= !!sym(color))) +
      geom_line(size = 1.5) +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      ylab(response) +
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5), text = element_text(size = 16))
    
    if(ci_type == "both"){
      p +
        geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = !!sym(color)), color = "transparent", alpha = 0.1) +
        geom_line(aes(x = !!sym(effects[1]), y = lower__, 
                      color = !!sym(color)), linetype = 2, alpha = 0.5) +
        geom_line(aes(x = !!sym(effects[1]), y = upper__, 
                      color = !!sym(color)), linetype = 2, alpha = 0.5)
    } else{
      if(ci_type == "lines"){
        p + geom_line(aes(x = !!sym(effects[1]), y = lower__, 
                          color = !!sym(color)), linetype = 2, alpha = 0.7) +
          geom_line(aes(x = !!sym(effects[1]), y = upper__, 
                        color = !!sym(color)), linetype = 2, alpha = 0.7)
      } else{
        p + geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = !!sym(color)), color = "transparent", alpha = 0.3)
      }
    }
  }
}

param_estimate_plot <- function(model, num_params = NULL, params_list = NULL){
  if(is.null(params_list) & !is.null(num_params)){
    p <- posterior_summary(model) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "variable") %>% 
    head(x = ., n=num_params)} else {
      p <- posterior_summary(model) %>% 
        unlist() %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "variable") %>% 
        filter(variable %in% params_list)
    }
  pp <- p %>% 
    ggplot(aes(y=Estimate, x = variable))+
    geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5, group=variable), size = 3/5, shape = 20) +
    geom_hline(yintercept=0, color = "gray25", alpha = 0.25) +
    coord_flip() +
    ylab("Estimate with 95% credible interval")
  return(pp)
}

my_icc <- function (model, re.form = NULL, robust = TRUE, ci = 0.95, newdata = NULL, ...) 
{
  mi <- insight::model_info(model)
  if (insight::is_multivariate(model)) {
    resp <- insight::find_response(model)
    is.mixed <- sapply(resp, function(i) mi[[i]]$is_mixed, 
                       simplify = TRUE)
    if (!any(is.mixed)) {
      warning("'model' has no random effects.", call. = FALSE)
      return(NULL)
    }
  }
  else if (!mi$is_mixed) {
    warning("'model' has no random effects.", call. = FALSE)
    return(NULL)
  }
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("Package `brms` needed for this function to work. Please install it.", 
         call. = FALSE)
  }
  PPD <- brms::posterior_predict(model, re.form = re.form, newdata = newdata, 
                                 summary = FALSE)
  var_total <- apply(PPD, MARGIN = 1, FUN = stats::var)
  PPD_0 <- brms::posterior_predict(model, re.form = NA, newdata = newdata, summary = FALSE)
  var_rand_intercept <- apply(PPD_0, MARGIN = 1, FUN = stats::var)
  if (robust) 
    fun <- get("median", asNamespace("stats"))
  else fun <- get("mean", asNamespace("base"))
  var_icc <- var_rand_intercept/var_total
  var_residual <- var_total - var_rand_intercept
  ci_icc <- rev(1 - stats::quantile(var_rand_intercept/var_total, 
                                    probs = c((1 - ci)/2, (1 + ci)/2)))
  result <- structure(class = "icc_decomposed", list(ICC_decomposed = 1 - 
                                                       fun(var_icc), ICC_CI = ci_icc))
  attr(result, "var_rand_intercept") <- fun(var_rand_intercept)
  attr(result, "var_residual") <- fun(var_residual)
  attr(result, "var_total") <- fun(var_total)
  attr(result, "ci.var_rand_intercept") <- bayestestR::ci(var_rand_intercept, 
                                                          ci = ci)
  attr(result, "ci.var_residual") <- bayestestR::ci(var_residual, 
                                                    ci = ci)
  attr(result, "ci.var_total") <- bayestestR::ci(var_total, 
                                                 ci = ci)
  attr(result, "ci") <- ci
  attr(result, "re.form") <- re.form
  attr(result, "ranef") <- model$ranef$group[1]
  attr(attr(result, "ci.var_rand_intercept"), "data") <- NULL
  attr(attr(result, "ci.var_residual"), "data") <- NULL
  attr(attr(result, "ci.var_total"), "data") <- NULL
  result
}

my_icc_tibble <- function(model, total_re.form, lesser_re.form){
  PPD <- posterior_predict(model, re.form = total_re.form)
  vars <- apply(PPD, MARGIN = 1, FUN = var)
  
  PPD_0 <- posterior_predict(model, re.form = lesser_re.form)
  vars_0 <- apply(PPD_0, MARGIN = 1, FUN = var)
  
  icc_draws <- tibble(icc_draws = 1 - (vars_0/vars))
  model_name <- deparse(substitute(model))
  icc_draws %>% 
    median_hdi() %>% 
    rename(median_icc = icc_draws, ci_2.5 = .lower, ci_97.5 = .upper) %>% 
    mutate(model_name = model_name) %>% 
    select(model_name, median_icc, ci_2.5, ci_97.5)
}

my_icc_plot <- function(model, total_re.form, lesser_re.form, plot_each_var = F){
  PPD <- posterior_predict(model, re.form = total_re.form)
  vars <- apply(PPD, MARGIN = 1, FUN = var)
  
  PPD_0 <- posterior_predict(model, re.form = lesser_re.form)
  vars_0 <- apply(PPD_0, MARGIN = 1, FUN = var)
  
  icc_draws <- tibble(icc_draws = 1 - (vars_0/vars))
  
  g <- icc_draws %>% 
    ggplot(aes(x = icc_draws)) +
    geom_histogram(bins = 100) +
    ggtitle(deparse(substitute(model)))
  
  if (plot_each_var == T) {
    g2 <- vars %>% 
      as.data.frame() %>% 
      ggplot(aes(x = vars)) +
      geom_histogram(bins = 100) +
      xlab("Variance Draws Conditional on Random Effects")
    
    g3 <- vars_0 %>% 
      as.data.frame() %>% 
      ggplot(aes(x = vars_0)) +
      geom_histogram(bins = 100) +
      xlab("Variance Draws Not Conditional on Random Effects")
    
    # exctracting axis limits from each plot
    g2x <- ggplot_build(g2)$layout$panel_scales_x[[1]]$range$range
    g3x <- ggplot_build(g3)$layout$panel_scales_x[[1]]$range$range
    
    # making the limits the same while fitting both sets of data
    g2 <- g2 + xlim(min(g2x, g3x), max(g2x, g3x))
    g3 <- g3 + xlim(min(g2x, g3x), max(g2x, g3x))
    
    g <- list(icc = g, conditional_variance = g2, unconditional_variance = g3)
  }
  return(g)
}
