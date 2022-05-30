
# libraries ---------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(purrr)
library(mvtnorm)
library(tidyr)
library(magrittr)





# Dataset and bayes optimal boundary plot ---------------------------------

#' @name: dataset generator
#'
#' @param size numeric:
#' @param nVar numeric:
#' @param n_g numeric:
#' @param class_fun function:
#' @param treshold numeric:
#' @return results list: list(dataset_plot = dataset_plot, dataset = dataset, cond = grid, border_plot = dataset_plot_border)


dataset_gen_unif <- function(size = 1000, nVar = 2, n_g = 2, class_fun = NULL, treshold = 0.5) 
  {
    
    # Verify if inputs are correct data types
    stopifnot("A numeric value needs to be provided for the size of the dataset" = is.numeric(size))
    stopifnot("A numeric value needs to be provided for the number of variables to be produced" = is.numeric(nVar))
    stopifnot("The classification function needs to be of the type function" = is.function(class_fun))
    stopifnot("Number of variables needs to be equal or above 2" = nVar >= 2)
    
    # Random sample of data
    sample <- replicate(nVar,stats::runif(size, min = 0, max = 10))
    sample <- dplyr::as_tibble(sample) %>% magrittr::set_colnames(paste0("x", 1:nVar))
    
    # Applies classification function if nVar = 2
    dataset <- sample %>% 
      mutate(
        g = purrr::pmap_dbl(., class_fun )
      )
    
    # Creates plot
    dataset_plot <- ggplot(dataset, aes(x1, x2, color = factor(g))) + 
      geom_point(size = 3, shape = 1) +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) +
      theme_bw() +
      theme(
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line = element_blank(),
        legend.position="bottom",
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.caption.position = "plot"
      ) +
      scale_colour_brewer(palette = "Set1")
    
  
    ## Build grid for contour
    x1_range <-  seq(0, 10, by = 0.05)
    x2_range <-  seq(0, 10, by = 0.05)
    grid <-  expand.grid(x1 = x1_range, x2 = x2_range)
    
    # conditional probability of (x1, x2) given y = 0
    grid <- grid %>% 
      mutate(
        g = purrr::pmap_dbl(., class_fun )
      )
    
    l <- list()
    
    for (i in 1:n_g) {
      
      l[[i]] <- ifelse(grid$g == i, 1, 0)
      
    }
    
    # Calculates conditional probabilities
    conditional_prb = do.call(cbind.data.frame, l) %>% 
      set_colnames(paste0("px_G",0:(n_g-1))) %>% 
      mutate(
        py0_x = treshold * px_G0,
        py1_x = (1-treshold) * px_G1,
        bayesborder = py1_x - py0_x ,
        predictclass = ifelse(py0_x > py1_x, 0, 1) # posterior class
      )
    
    
    
    
    
    grid <- cbind(grid, conditional_prb)
    
    dataset_plot_border <- dataset_plot +
      geom_contour(data = grid, aes(x = x1,y = x2, z = bayesborder), color = "black", linetype = "dashed", breaks = 0) 
    
    # return results
    results <- list(dataset_plot = dataset_plot, dataset = dataset, cond = grid, border_plot = dataset_plot_border)
    return(results)
  
  }




# Dataset gen with multivariated normal dist ------------------------------

#' @name: dataset generator with multivariated normal distribution
#'
#' @param 
#' @return 


dataset_gen_mvnorm <- function(l_mu, l_cvm,l_w, size = 1000, nVar = 2, n_g = 2, class_fun = NULL, treshold = 0.5) {
  
  # generates samples
  
  l_sample <- list()
  
  for (i in 1:length(l_mu)) {
    
    s <- cbind(rmvnorm(size/length(l_w), l_mu[[i]], l_cvm[[i]]),i-1)
    l_sample[[i]] <- s
    
  }
  
  dataset <- do.call(rbind.data.frame,l_sample) %>% magrittr::set_colnames( c(paste0("x", 1:nVar),"g" ) )
  
  # dataset <- sample %>% 
  #   mutate(
  #     g = purrr::pmap_dbl(., class_fun )
  #   )
  
  # Creates plot
  dataset_plot <- ggplot(dataset, aes(x1, x2, color = factor(g))) + 
    geom_point(size = 3, shape = 1) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw() +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.line = element_blank(),
      legend.position="bottom",
      panel.border = element_rect(colour = "black", fill=NA, size=1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.caption.position = "plot"
    ) +
    scale_colour_brewer(palette = "Set1")
  
  
  ## Build grid for contour
  x1_range <-  seq(min(dataset$x1), max(dataset$x1), by = 0.05)
  x2_range <-  seq(min(dataset$x2), max(dataset$x2), by = 0.05)
  grid <-  expand.grid(x1 = x1_range, x2 = x2_range)
  
  # conditional probability of (x1, x2) given y = 0
  grid <- grid %>% 
    mutate(
      g = purrr::pmap_dbl(., class_fun )
    )
  
  grid_merge <- merge(grid, data.frame( class = 0:(n_g-1) ), all=TRUE)
  
  
  l <- list()
  
  for (i in 1:n_g) {
    
    l[[i]] <- ifelse(grid$g == i, 1, 0)
    
  }
  
 new_grid <- grid_merge %>% mutate(p_class = ifelse(class == g, 1, 0))
  
  
  # Calculates conditional probabilities
  conditional_prb = do.call(cbind.data.frame, l) %>% 
    set_colnames(paste0("px_G",0:(n_g-1))) %>% 
    mutate(
       py0_x = treshold * px_G0,
       py1_x = (1-treshold) * px_G1,
       bayesborder = py1_x - py0_x ,
       predictclass = ifelse(py0_x > py1_x, 0, 1) # posterior class
     )
  
  grid <- cbind(grid, conditional_prb)
  
  
  dataset_plot_border <- dataset_plot +
    geom_contour(data = grid, aes(x = x1, y = x2, z = bayesborder), color = "black", linetype = "dashed", breaks = 0)
    
  dataset_plot_border_newgrid <- dataset_plot +
    geom_contour(data = new_grid, aes(x = x1, y = x2, z = p_class, color = as.factor(class), group = as.factor(class)), bins = 1)
  
  
  # return results
  results <- list(dataset_plot = dataset_plot, dataset = dataset, cond = new_grid, border_plot = dataset_plot_border_newgrid)
  return( results )
  
}




# Classification metrics function -----------------------------------------

model_metrics <- function(test_data = NULL, model = NULL )  {
  
      
  # Verify if inputs are correct data types
  stopifnot("A test dataframe should be provided" = is.data.frame(test_data))
  stopifnot("A model should be provided" = !is.null(model))
  
  
  # fit test data
  fit_test <- 
    test_data %>% 
    bind_cols(
      predict(model, new_data = test_data),
      predict(model, new_data = test_data, type = "prob"),
    ) %>% 
    mutate_if(is.numeric, round, digits= 3) %>% 
    mutate(
      decision = .pred_1 - .pred_0
    )
      
  # confusion matrix
  confusion_matrix <- conf_mat(fit_test, truth = g, estimate = .pred_class)
  confusion_matrix_plot <- autoplot(confusion_matrix, type = "heatmap")
  
  
  # Accuracy
  acc <- accuracy(fit_test, truth = g, estimate = .pred_class)
  
  # Roc curve
  roc_curve <- roc_curve(fit_test, truth = g, estimate = .pred_0) %>% 
    autoplot()
  
  auc_roc <- roc_auc(fit_test,
                     truth = g, 
                     .pred_0)
      
  
  results <- list(fit = fit_test, 
                  cf_matrix = confusion_matrix, 
                  cf_plot =  confusion_matrix_plot, 
                  acc = acc, 
                  roc_curve = roc_curve, 
                  auc_roc = auc_roc)
  
  return(results)
}























