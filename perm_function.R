#' Permutation
#'
#' Tool to do a two-sided permutation test on a dataset
#'
#' Given a dataset where column 1 specifies the groups and column 2 specifies the observed values for each group, the function makes a two-sided permuation test. 
#'
#' @param data dataset of at least 2 columns. Column 1 has to specify the two groups as factors and column 2 their observed values as integers.
#'
#' @param test_statistic One of 3 options: "mean" (Default), "median" or "my_method". If "my_method" is used, the user has to have defined a function called my_method() that calculates the test-statistic, with the only input being column 2 from the data.
#'
#' @param nPerm Number of permutations. Default is 10^5
#' 
#' @param plot If TRUE (Default) the function returns two plots together with the regular output. If FALSE no plots are returned.
#' 
#' @return list
#'
#'@examples
#'
#' permtation(data, test_statistic = "median", n_perm = 10^4, plot = FALSE)
#' 
#' @import tidyverse
#'
#' @export
#'

permutation <- function(data, test_statistic = "mean", nPerm = 10^5, plot=TRUE) {
  if (test_statistic == "mean" ) {
    test_statistic = "Difference in mean"
    fun = mean
  }
  else if (test_statistic == "median") {
    test_statistic = "Difference in median"
    fun = median
  }
  else if (test_statistic == "my_method") {
    test_statistic = "my_method"
    fun = my_method
  }
  else {
    return(print("Test-statistic method not known for function. 
                 See description for further help."))
  }
  
  test_data <- data %>% 
    group_by_at(1) %>% 
    summarise_at( .vars = names(.)[2] , fun)
  
  obs_test_stat <- diff(test_data[[2]])
  
  perm_test_values = rep(NA,nPerm)
  
  for (p in seq(1:nPerm)) {
    perm_data = 
      cbind.data.frame(Group = sample(c(data[[1]]), replace = FALSE),
                       Value= c(data[[2]]))
    
    perm_test_val <- perm_data %>% 
      group_by(Group) %>% 
      summarise(val = fun(Value)) %>%
      .$val %>% 
      diff() 
    
    perm_test_values[p] <- perm_test_val
  }
  
  p.value = (sum(abs(perm_test_values) >= obs_test_stat)/nPerm)*2
  
  if (plot == TRUE) {
    plot1 = ggplot(data) +
      geom_histogram(aes_string(x = names(data)[2], fill=names(data)[1]), 
                     alpha = 0.6, bins = 50, color = "white") +
      geom_vline(data=test_data, 
                 aes_string(xintercept=names(test_data)[2], 
                            color=names(test_data)[1]), 
                 size = 0.8, linetype="dashed") +
      annotate(geom="text", x = Inf,y=Inf,
               label=paste(test_statistic,"test:",round(obs_test_stat,2)),
               size = 4, 
               vjust=2, hjust=1) +
      theme_bw(base_size = 15) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(title = "The observed values",
           y = "Counts")
    
    plot2 = ggplot() + 
      geom_histogram(aes(x = perm_test_values), 
                     bins = 50, 
                     color = "white",
                     fill = "blue",
                     alpha = 0.6) +
      geom_vline(aes(xintercept = obs_test_stat, color = "Observed\ntest-stat"), 
                 size = 0.8, linetype = "dashed") +
      scale_color_manual(values = c("Observed\ntest-stat" = "red")) +
      theme_bw(base_size = 15) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(title = "The null distribution for the test-statistic",
           x = "test_statistic",
           y = "Counts")
    return(list("Plot 1: The observed values" = plot1,
                "Plot 2: The null distribution for the test-statistic" = plot2,
                "Method" = test_statistic, 
                "Obs_Test_stat" = obs_test_stat,
                "perm_p_value" = p.value))
  }
  return(list("Method" = test_statistic, 
              "Obs_Test_stat" = obs_test_stat,
              "perm_p_value" = p.value))
}

## EXAMPLE OF USE
set.seed(0)

# Simulated data of blood pressure for two groups
data <- tibble(Group = c(rep("ctrl",100),rep("case",100)),
               Blood_pressure=(c(rnorm(100,mean=120,sd=20),
                                 rnorm(100,mean = 115,sd=15)))) 

# User specified function to calculate test-statistic
my_method <- function(x) {
  mean(x)
}

# Three possible values of the parameter "test_statistic"
permutation(data = data, test_statistic = "mean")
permutation(data = data, test_statistic = "median")
permutation(data = data, test_statistic = "my_method")





