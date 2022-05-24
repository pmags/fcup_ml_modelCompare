
# libraries ---------------------------------------------------------------

library(ggplot2)
#library(ggforce)
library(dplyr)
library(purrr)
library(mvtnorm)
library(tidyr)
library(magrittr)





# Dataset and bayes optimal boundary plot ---------------------------------

#' @name: dataset generator
#'
#' @field
#'  
#'  
#' @return 
#'  A list containing :
#'  1. Dataframe representing a dataset
#'  2. Plot representing the new bayes boundary
#' 
  
#'
#' @examples
#' 

dataset_gen <- function(size = 1000, nVar = 2, n_g = 2, class_fun = NULL, treshold = 0.5) 
  {
    
    # Verify if inputs are correct data types
    stopifnot("A numeric value needs to be provided for the size of the dataset" = is.numeric(size))
    stopifnot("A numeric value needs to be provided for the number of variables to be produced" = is.numeric(nVar))
    stopifnot("The classification function needs to be of the type function" = is.function(class_fun) & is.null(shape))
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
      #scale_color_manual(values=c("orangered2", "navyblue"), name = "") 
  
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
    
    grid <- cbind(grid,conditional_prb)
    
    dataset_plot_border <- dataset_plot +
      geom_contour(data = grid, aes(x = x1,y = x2, z = bayesborder), color = "black", linetype = "dashed", breaks = 0) 
    
    # return results
    results <- list(dataset_plot = dataset_plot, dataset = dataset, cond = grid, border_plot = dataset_plot_border)
    return(results)
  
  }

