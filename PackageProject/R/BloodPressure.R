#' BloodPressure
#'
#' The bloodpressure is measured for each individual, either a control ("ctrl") or case ("case").
#'
#' The BloodPressure dataset is a testset for the permutation(). It has one column specifying the groups (case or control) and another column with the observations (bloodpressure) for each individual in each group.
#'
#' @docType data
#'
#' @usage data(BloodPressure)
#'
#' @param Group Specifying the group of an individual. ("ctrl" = control, "case" = case)
#'
#' @param Blood_pressure The measured bloodpressure of an individual.
#'
#' @format dataframe
#'

"BloodPressure"

#BloodPressure <- tibble(Group = c(rep("ctrl",100),rep("case",100)),
 #                       Blood_pressure=(c(rnorm(100,mean=120,sd=20),
  #                                        rnorm(100,mean = 115,sd=15))))

#save(BloodPressure, file="data/BloodPressure.RData")
