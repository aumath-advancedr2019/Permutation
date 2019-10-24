#' bootstrap
#'
#' Tool to do a bootstrap  on a dataset
#'
#' Given a dataset where column 1 specifies the groups and column 2 specifies the observed values for each group, the function makes a bootstrap, the output is a plot of the sample distribut, including confidence interval, mean of obs and sd.
#'
#' @param data dataset of at least 2 columns. Column 1 has to specify the two groups as factors and column 2 their observed values as integers.
#'
#' @param test_statistic One of 3 options: "mean" (Default(will be at least)), "median" or "my_method". If "my_method" is used, the user has to define the parameter selfchosenmethod as a function that calculates the test-statistic, with the only input being column 2 from the data.
#'
#' @param nr_iterations Number of permutations. Default(Default(will be at least is 10^5
#'
#' @param plot If TRUE (Default(Default(will be at least) the function returns two plots together with the regular output. If FALSE no plots are returned.
#'
#' @return list
#'
#' @examples
#'
#' bootstrap(PlantGrowth, PlantGrowth$group, PlantGrowth$weight, test_statistic=3, 10000, selfchosenmethod =mean)
#'
#' @import tidyverse
#'
#' @export

bootstrap<-function(data, x, y, test_statistic, nr_iterations, selfchosenmethod){
  methods<-c(2,3)
  medians<-c()
  means<-c()
  #output<-data.frame(SD=c(sd(y)))
  if (test_statistic!=methods){
    for (i in 1:nr_iterations){
      inputmedian<-median(y)
      sample <- sample(x = y, replace = T, size = length(y))
      medians[i]<-median(sample)


    }
    firstquantile<-((quantile(medians, 0.025, names= FALSE)[1]))
    lastquantile<-((quantile(medians, 0.975, names= FALSE)[1]))
    sd<-sd(medians)
    #output["SE"]<-sd(medians)
    #output["conf, interval"]<-paste(firstquantile, " : ", lastquantile)
    #output["obs. median"]<-inputmedian
    minus=(max(medians)-min(medians))/80
    top=max(data.frame(table(medians))$Freq)
    #print(output)
    ggplot(data.frame(medians) )+
      geom_histogram(aes(x=medians), fill="darkcyan", alpha=0.4)+
      theme_bw()+
      geom_vline(aes(xintercept=inputmedian), color="chocolate3")+
      geom_vline(aes(xintercept=c(firstquantile)), color="brown")+
      geom_vline(aes(xintercept=c(lastquantile)), color="brown")+
      annotate("text", x = (inputmedian-minus), y=top/2, label = "Obs. median", color= "chocolate4", angle=90)+
      annotate("text", x = (firstquantile-minus), y=top/2, label = "lower boundary of conf.interval", color= "brown4", angle=90)+
      annotate("text", x = (lastquantile-minus), y=top/2, label = "upper boundary of conf. interval", color= "brown4", angle=90)
  }
  else if(test_statistic==2){
    for (j in 1:nr_iterations){
      inputmean<-mean(y)
      sample <- sample(x = y, replace = T, size = length(y))
      means[j]<-mean(sample)
    }
    firstquantilemean<-((quantile(means, 0.025, names= FALSE)[1]))
    lastquantilemean<-((quantile(means, 0.975, names= FALSE)[1]))
    sd<-sd(means)
    minus2=(max(means)-min(means))/100
    top2=max(data.frame(table(means))$Freq)
    ggplot(data.frame(means) )+
      geom_histogram(aes(x=means), fill="darkcyan", alpha=0.4)+
      theme_bw()+
      geom_vline(aes(xintercept=inputmean), color="chocolate3")+
      geom_vline(aes(xintercept=c(firstquantilemean)), color="brown")+
      geom_vline(aes(xintercept=c(lastquantilemean)), color="brown")+
      geom_text(aes(label=paste("upper boundary of conf.interval:", firstquantilemean, "\n","lower boundary of conf.interval:", lastquantilemean )),x=Inf, y=Inf, vjust="top", hjust="right", color="brown4")+
      geom_text(aes(label=paste("Obs. mean:", inputmean)),x=-Inf, y=Inf, vjust="top", hjust="left", color="chocolate")
  }
  else if(test_statistic==3){
    for (j in 1:nr_iterations){
      inputmean<-selfchosenmethod(y)
      sample <- sample(x = y, replace = T, size = length(y))
      means[j]<-selfchosenmethod(sample)
    }
    firstquantilemean<-((quantile(means, 0.025, names= FALSE)[1]))
    lastquantilemean<-((quantile(means, 0.975, names= FALSE)[1]))
    sd<-sd(means)
    minus2=(max(means)-min(means))/100
    top2=max(data.frame(table(means))$Freq)
    ggplot(data.frame(means) )+
      geom_histogram(aes(x=means), fill="darkcyan", alpha=0.4)+
      theme_bw()+
      geom_vline(aes(xintercept=inputmean), color="chocolate3")+
      geom_vline(aes(xintercept=c(firstquantilemean)), color="brown")+
      geom_vline(aes(xintercept=c(lastquantilemean)), color="brown")+
      geom_text(aes(label=paste("upper boundary of conf.interval:", firstquantilemean, "\n","lower boundary of conf.interval:", lastquantilemean )),x=Inf, y=Inf, vjust="top", hjust="right", color="brown4")+
      geom_text(aes(label=paste("Obs.:", inputmean)),x=-Inf, y=Inf, vjust="top", hjust="left", color="chocolate")
  }
}

