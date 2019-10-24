#' Permutation
#'
#' Tool to do a two-sided permutation test on a dataset.
#'
#' Given a dataset where one column specifies the groups and another column specifies the observed values for each group, the function makes a two-sided permuation test. The teststatistic used for the test can either be mean, median or a user defined method. Summary or plot can be called on the output of class "permutation".
#'
#' @param groups The column of the datasat containing the two groups. Has to be factors.
#'
#' @param observations The column of the dataset containing the observations for the two groups. Has to be numeric.
#'
#' @param test_statistic One of 3 options: "mean" (Default), "median" or "my_method". If "my_method", the user has to define a function called "my_method(observations)" that calculates the test-statistic, with the only input being the observations from the data grouped by group.
#'
#' @param nPerm Number of permutations. Default is 10^5
#'
#' @return a list of class object "permutation".
#'
#' @examples
#'
#' The test data provided in the package, BloodPressure, can be used as following:
#'
#' permutation(groups = BloodPressure$Groups, observations = BloodPressure$Blood_pressure, test_statistic = "median", n_perm = 10^4)
#'
#' @import tidyverse
#'
#' @export
#'

permutation <- function(groups, observations, test_statistic = "mean", nPerm = 10^5) {
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
    return(output(Warnings = "Test-statistic method not known for function.
                 See description for further help."))
  }

  test_data <- data %>%
    group_by(Group) %>%
    summarise(Val = fun(Observations))

  obs_test_stat <- diff(test_data$Val)

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

  if (p.value == 0) {
    warn = "Warning" = "nPerm was to low to get any permuted test statistics equal to or more extreme than your observed"
    return(output(data = data,
                  Method = test_statistic,
                  fun = fun,
                  obs_Test_stat = obs_test_stat,
                  perm_test_values = perm_test_values,
                  perm_p_value = p.value,
                  Warnings = warn))
  }
  return(output(data = data,
                Method = test_statistic,
                fun = fun,
                obs_Test_stat = obs_test_stat,
                perm_test_values = perm_test_values,
                perm_p_value = p.value))
}






#' output
#'
#' @return a list of class object "permutation".
#'
#' @import tidyverse
#'
#' @export
#'

# Defining class Permutation
output <- function(data = tibble(),
                   Method = "Unknown",
                   fun = "mean",
                   obs_Test_stat = 0,
                   perm_test_values = 0,
                   perm_p_value = 0,
                   Warnings = "None") {
  value <- list(data = data,
                Method = Method,
                fun = fun,
                ObsTestStat = obs_Test_stat,
                PermTestValues = perm_test_values,
                PermPvalue = perm_p_value,
                Warnings = Warnings)
  attr(value,"class") <- "permutation"
  value
}




#' summary
#'
#' @return a summary of the class object "permutation".
#'
#' @import tidyverse
#'
#' @export
#'

# Calling the summary on permutation
summary.permutation <- function(permutation) {
  print(permutation$data %>% group_by(Group) %>% summarise(Method = permutation$fun(Observations)))
  print(paste("Method:", permutation$Method))
  print(paste("Observed test-statistic:", permutation$ObsTestStat))
  print(paste("Permuted p-value:", permutation$PermPvalue))
  if (permutation$Warnings != "None") {
    print(paste("Warnings!!",permutation$Warnings))
  }
}





#' plot
#'
#' @return a two plots of the class object "permutation".
#'
#' @import tidyverse
#'
#' @export
#'

# Calling plot on permutation
plot.permutation <- function(permutation) {
  test_data <- permutation$data %>%
    group_by(Group) %>%
    summarise(Val = permutation$fun(Observations))

  plot(ggplot(permutation$data) +
         geom_histogram(aes(x = Observations, fill=Group),
                        alpha = 0.6, bins = 50, color = "white") +
         geom_vline(data=test_data,
                    aes(xintercept=Val,
                        color=Group),
                    size = 0.8, linetype="dashed") +
         annotate(geom="text", x = Inf,y=Inf,
                  label=paste(permutation$Method,"test:",round(permutation$ObsTestStat,2)),
                  size = 4,
                  vjust=2, hjust=1) +
         theme_bw(base_size = 15) +
         theme(plot.title = element_text(hjust = 0.5)) +
         labs(title = "The observed values",
              y = "Counts"))

  plot(ggplot() +
         geom_histogram(aes(x = permutation$PermTestValues),
                        bins = 50,
                        color = "white",
                        fill = "blue",
                        alpha = 0.6) +
         geom_vline(aes(xintercept = permutation$ObsTestStat, color = "Observed\ntest-stat"),
                    size = 0.8, linetype = "dashed") +
         scale_color_manual(values = c("Observed\ntest-stat" = "red")) +
         theme_bw(base_size = 15) +
         theme(plot.title = element_text(hjust = 0.5)) +
         labs(title = "The null distribution for the test-statistic",
              x = "test_statistic",
              y = "Counts"))
}

