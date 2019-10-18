#' Permutation
#'
#' Tool to do a two-sided permutation test on a dataset.
#'
#' Given a dataset where one column specifies the groups and another column specifies the observed values for each group, the function makes a two-sided permuation test. The teststatistic used for the test can either be mean, median or a user defined method.
#'
#' @param groups The column of the datasat containing the two groups. Has to be factors.
#'
#' @param observations The column of the dataset containing the observations for the two groups. Has to be numeric.
#'
#' @param test_statistic One of 3 options: "mean" (Default), "median" or "my_method". If "my_method", the user has to define a function called "my_method(observations)" that calculates the test-statistic, with the only input being the observations from the data grouped by group.
#'
#' @param nPerm Number of permutations. Default is 10^5
#'
#' @param plot If TRUE (Default) the function returns two plots together with the regular output. If FALSE no plots are returned.
#'
#' @return list
#'
#' @examples
#'
#' The test data provided in the package, BloodPressure, can be used as following:
#'
#' permutation(groups = BloodPressure$Groups, observations = BloodPressure$Blood_pressure, test_statistic = "median", n_perm = 10^4, plot = FALSE)
#'
#' @import tidyverse
#'
#' @export
#'

permutation <- function(groups, observations, test_statistic = "mean", nPerm = 10^5, plot=TRUE) {
  data = tibble("Group" = groups, "Observations" = observations)

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
    group_by(Group) %>%
    summarise(Mean = mean(Observations))

  obs_test_stat <- diff(test_data$Mean)

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
      geom_histogram(aes(x = Observations, fill=Group),
                     alpha = 0.6, bins = 50, color = "white") +
      geom_vline(data=test_data,
                 aes(xintercept=Mean,
                            color=Group),
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
    if (p.value == 0) {
      print("Warning: nPerm was to low to get any permuted test statistics equal to or more extreme than your observed")
    }
    return(list("Plot 1: The observed values" = plot1,
                "Plot 2: The null distribution for the test-statistic" = plot2,
                "Method" = test_statistic,
                "Obs_Test_stat" = obs_test_stat,
                "perm_p_value" = p.value))
  }
  if (p.value == 0) {
    print("Warning" = "nPerm was to low to get any permuted test statistics equal to or more extreme than your observed")
  }
  return(list("Method" = test_statistic,
              "Obs_Test_stat" = obs_test_stat,
              "perm_p_value" = p.value))
}




