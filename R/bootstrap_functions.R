#' Do a balanced bootstrap on a linear model.
#'
#' This function calculates a linear regression of y versus x. The data
#' are bootstrapped to generate confidence intervals around the slope and
#' intercept. Optionally, the r2 value may also be calculated. The bootstrapped
#' prediction interval around the fit may also be calculated.
#'
#' @param x x-axis values, a numeric vector
#' @param y y-axis values, a numeric vector
#' @param xsd (optional) Either a single value or a vector with length equal to x. Used to generate normal distributions around each x value with SD equal to xsd.
#' @param ysd (optional) Either a single value or a vector with length equal to y. Used to generate normal distributions around each y value with SD equal to ysd.
#' @param n Number of bootstrap replicates to perform.
#' @param ci.width Width of the confidence interval to use for hypothesis testing, a single numeric value between 1 and 100
#' @param method Regression method. Either 'ols" for Ordinary Least Squares, 'ma' for Major Axis, or 'rma' for Reduced Major Axis.
#' @param pred.band Compute the prediction interval? This is computationally intensive, especially for large datasets.
#' @param pred.limits Lower and upper limits of the prediction interval. By default, these are the limits of the observed x-axis data.
#' @param pred.steps # of steps to calculate the prediction interval at. Increase for higher resolution at cost of computation time.
#' @export
lm.bal <- function(x,
                   y,
                   xsd=0,
                   ysd=0,
                   n=10000,
                   ci.width=95,
                   method=c("ols","ma","rma"),
                   pred.band = T,
                   pred.limits = c(NA,NA),
                   pred.steps = 25){

  if(length(x) != length(y)){stop("x and y lengths not equal")}
  if(length(xsd) > 1 & length(xsd) != length(x)){stop("xsd must either be a single value or a vector of length equal to x")}
  if(length(ysd) > 1 & length(ysd) != length(y)){stop("ysd must either be a single value or a vector of length equal to y")}
  if(ci.width < 1 | ci.width > 100){stop("ci.width must be between 1 and 100")}
  if(length(x) < 7){warning("Data points with N < 7 results in all permutations being calculated. Interpet confidence intervals with caution.")}
  if(length(x) > 6 & length(x) < 9 & n == 10000){warning("Data points with N < 9 results in less than 10,000 possible permutations. Consider reducing bootstrap N to 1,000 and interpret confidence intervals with caution.")}
  if(!(method[1] %in% c("ols","ma","rma"))){stop("Specified method must be either 'ols', 'ma', or 'rma'. You have specified something else.")}

  lower = (100-ci.width)/2/100
  upper = 1-lower

  input = data.frame(x = x,
                     y = y,
                     xsd = xsd,
                     ysd = ysd)
  
  observed = input %>% 
    summarize(slope = ifelse(method[1]=="ols", cov(x,y)/var(x),NA),
              slope = ifelse(method[1]=="ma", 0.5*(var(y)/cov(x,y)-var(x)/cov(x,y)+sign(cov(x,y))*(4+(var(y)/cov(x,y)-var(x)/cov(x,y))^2)^0.5),slope),
              slope = ifelse(method[1]=="rma", sign(cov(x,y))*sd(y)/sd(x),slope),
              intercept = mean(y)-slope*mean(x),
              r2 = cor(x,y)^2)

  data = input %>%
    mutate(sample_index = 1:n(),
           rep = list(1:n)) %>%
    unnest(rep) %>%
    group_by(sample_index) %>%
    mutate(x = rnorm(n = n, mean = x, sd = xsd),
           y = rnorm(n = n, mean = y, sd = ysd)) %>%
    ungroup() %>%
    mutate(rep = sample(rep)) %>%
    group_by(rep) %>%
    summarize(slope = suppressWarnings(ifelse(method[1]=="ols", cov(x,y)/var(x),NA)),
              slope = suppressWarnings(ifelse(method[1]=="ma", 0.5*(var(y)/cov(x,y)-var(x)/cov(x,y)+sign(cov(x,y))*(4+(var(y)/cov(x,y)-var(x)/cov(x,y))^2)^0.5),slope)),
              slope = suppressWarnings(ifelse(method[1]=="rma", sign(cov(x,y))*sd(y)/sd(x),slope)),
              intercept = mean(y)-slope*mean(x),
              r2 = suppressWarnings(cor(x,y)^2),
              .groups="keep") %>%
    ungroup() %>%
    mutate(slope.observed = observed$slope,
           slope.lower = sort(slope)[lower*n],
           slope.upper = sort(slope)[upper*n],
           slope.sig = ifelse(sign(slope.lower) == sign(slope.upper),paste0("< ",lower*2),paste0("> ",lower*2)),
           intercept.observed = observed$intercept,
           intercept.lower = sort(intercept)[lower*n],
           intercept.upper = sort(intercept)[upper*n],
           intercept.sig = ifelse(sign(intercept.lower) == sign(intercept.upper),paste0("< ",lower*2),paste0("> ",lower*2)),
           r2.observed = observed$r2,
           r2.lower = sort(r2)[lower*n],
           r2.upper = sort(r2)[upper*n])
  
  if(pred.band){
    
    pred.lower = ifelse(is.na(pred.limits[1]),min(x),pred.limits[1])
    pred.upper = ifelse(is.na(pred.limits[2]),max(x),pred.limits[2])
    
    conf.band = data %>%
      mutate(x = list(seq(pred.lower,pred.upper,((pred.upper-pred.lower)/pred.steps)))) %>%
      unnest(x) %>%
      group_by(rep,x) %>%
      summarize(prediction = slope * x + intercept) %>%
      group_by(x) %>%
      summarize(prediction.observed = unique(observed$slope * x + observed$intercept),
                prediction.lower = sort(prediction)[lower*n],
                prediction.upper = sort(prediction)[upper*n])
  } else if(!pred.band){
    conf.band = data.frame(x = x) %>%
      mutate(prediction.observed = x * observed$slope + observed$intercept,
             prediction.lower = NA,
             prediction.upper = NA)
  }

  tidy.data <- data %>% select(-c(rep,slope,intercept,r2)) %>% distinct()

  list("tidy" = tidy.data, #just the summary statistics
       "data" = data, #the entire bootstrap results, including distributions
       "conf.band" = conf.band, #predicted values and (if selected) the bootstrapped prediction interval
       "input" = input) #the x and y input data
}





#' Perform a one-sample balanced bootstrap using standard deviation as the sampling statistic

#' @param a Input data, a numerical vector.
#' @param n Number of bootstrap replicates to perform.

#' @export
one.samp.bal.sd <- function(a, n=10000){
  if(length(a)>1){
    data.frame(a) %>%
      mutate(rep = list(1:(n))) %>%
      unnest(rep) %>%
      mutate(rep_shuffled = sample(rep)) %>%
      group_by(rep_shuffled) %>%
      summarize(sd = sd(a)) %>%
      ungroup() %>%
      summarize(sd_observed = mean(sd),
                ci2.5 = sort(sd)[0.025*length(sd)],
                ci97.5 = sort(sd)[0.975*length(sd)])
  }
  else if(length(a) < 2) {data.frame(mean = NA,ci2.5 = NA,ci97.5 = NA)}
}


#' Perform a one-sample balanced bootstrap using median as the sampling statistic
#'
#' @param a Input data, a numerical vector.
#' @param n Number of bootstrap replicates to perform.
#'
#' @export
one.samp.bal.median <- function(a, n=10000){
  if(length(a)>1){
    data.frame(a) %>%
      mutate(rep = list(1:(n))) %>%
      unnest() %>%
      mutate(rep_shuffled = sample(rep)) %>%
      group_by(rep_shuffled) %>%
      summarize(median = median(a)) %>%
      ungroup() %>%
      summarize(median_observed = median(median),
                ci2.5 = sort(median)[0.025*length(median)],
                ci97.5 = sort(median)[0.975*length(median)])
  }
  else if(length(a) < 2) {data.frame(median = NA,ci2.5 = NA,ci97.5 = NA)}
}


#' Perform a two-sample PAIRED balanced bootstrap using standard deviation as the sampling statistic
#'
#' @param a First dataset, a numerical vector.
#' @param b Second dataset, a numerical vector. Length must be equal to a.
#' @param n Number of bootstrap replicates to perform.
#'
#' @export
two.samp.bal.sd <- function(a,b,n=10000){
  data.frame(a,b) %>%
    mutate(rep = list(1:(n))) %>%
    unnest() %>%
    mutate(rep_shuffled = sample(rep)) %>%
    group_by(rep_shuffled) %>%
    summarize(sd_a = sd(a),
              sd_b = sd(b),
              diff = sd_a-sd_b) %>%
    ungroup() %>%
    summarize(sd_diff = mean(diff),
              ci2.5 = sort(diff)[0.025*length(diff)],
              ci97.5 = sort(diff)[0.975*length(diff)])
}


#' Perform a one-sample balanced bootstrap using mean as the sampling statistic
#'
#' @param a Dataset, a numerical vector.
#' @param n Number of bootstrap replicates to perform.
#' @param incdata Boolean to indicate if the complete bootstrapped distribution should be included in the results.
#'
#'
#' @export
one.samp.bal <- function(a,n=10000,incdata=F){
  if(length(a)>1){
    ran.a <- cbind(matrix(sample(rep(a,n-1)),nrow=length(a)),a)
    dist <- sort(colMeans(ran.a))
    mean_dist = mean(dist)
    ci2.5 = dist[0.025*length(dist)]
    ci97.5 = dist[0.975*length(dist)]
    p.value = ifelse(sign(ci2.5)==sign(ci97.5),"Significant (p < 0.05)","Non-significant (p > 0.05)")
    if(incdata==F){
      output <- as.data.frame(list("mean" = mean_dist,
                                   "ci2.5" = ci2.5,
                                   "ci97.5" = ci97.5,
                                   "p.value" = p.value))
    }
    if(incdata==T){
      temp <- list("mean" = mean_dist,
                   "ci2.5" = ci2.5,
                   "ci97.5" = ci97.5,
                   "p.value" = p.value,
                   "dist" = dist)
      output <- data.frame(temp)
    }}
  else if(length(a) < 2) {
    output <- data.frame(mean = NA,
                         ci2.5 = NA,
                         ci97.5 = NA,
                         p.value = NA)
  }
  output
}


#' Implementation of the balanced bootstrap for one or two samples
#' 
#' This function performs a balanced bootstrap on one or two samples If only one sample is provided, its bootstrapped distribution is compared against 
#' a defined value (null) to test for significance. If two samples are provided, then the first sample's summary statistic is subtracted from the second
#' sample's sampling statistic and the difference is compared against the defined value (null) to test for signifiance.
#' 
#' By default, this is a simple balanced bootstrap where the sample is replicates n times, shuffled, and then subset into groups equal to the original
#' sample size. However, if errors (asd or bsd) for either group are provided, then each bootstrap replicate for a value with an associated error is sampled from a 
#' normal distribution whose mean is the observed value and standard deviation is the user-submitted error. Thus, if errors are provided, this is more 
#' of a balanced Monte Carlo and carries an additional assumption that the true distribution surrounding individual values is normal.
#' 
#' The 'balanced' notion used here is taken from "Efficient bootstrap simulation" by Davison et al. 1986 Biometrika, Volume 73, Issue 3, December 1986, Pages 555–566, 
#' https://doi.org/10.1093/biomet/73.3.555
#' 
#' The 'difference' tested is b minus a, thus the directionality is such that positive differences indicate b > a and negative differences indicate b < a.
#' 
#' @section Output:
#' This function returns a named list as the result:
#' \itemize{
#' \item tidy.data - Summary statistics of the bootstrapped distribution
#' \item data - The complete bootstrap distribution(s) summarized at the replicate level using stat.function
#' \item parameters - The input parameters of the function
#' \item stat.function - The actual function used as the sampling statistic
#' \item input - The input data
#' }
#'
#' @param a First dataset, a numerical vector.
#' @param b Second dataset, a numerical vector. If paired=TRUE, then length must be equal to a.
#' @param asd (optional) First dataset's error as 1 standard deviation. If used, this must either be a single number (if error is uniform) or a vector with length equal to a.
#' @param bsd (optional) Second dataset's error as 1 standard deviation. If used, this must either be a single number (if error is uniform) or a vector with length equal to a.
#' @param n Number of bootstrap replicates to perform.
#' @param ci.width Width of the confidence interval to use for hypothesis testing, a single numeric value between 1 and 100
#' @param null Value representing the null hypothesis, default is 0.
#' @param stat.function Sampling statistic to use. Can use any function that takes a single vector and reports a single numerical value as its result. Default is the mean.
#' @param paired (optional) Boolean to indicate if the data are paired. Only relevant for two-sample situations.
#'
#' @export
bal.boot <- function(a,
                     b=NA,
                     asd=NA,
                     bsd=NA,
                     n=10000,
                     ci.width = 95,
                     null=0,
                     stat.function=mean,
                     paired=F){
  
  a <- na.omit(a)
  b <- na.omit(b)
  
  if (length(b) == 0){b = NA}
  
  if(paired & (length(a) != length(b))){stop("You indicated paired data but the length of a is not equal to b")}
  if(length(asd) > 1 & length(asd) != length(a)){stop("asd must either be a single value or a vector of length equal to a")}
  if(length(bsd) > 1 & length(bsd) != length(b)){stop("bsd must either be a single value or a vector of length equal to b")}
  if(ci.width < 1 | ci.width > 100){stop("ci.width must be between 1 and 100")}
  if(length(a) == 1){stop("a has a length of 1!")}
  if(!is.numeric(a)){stop("a is non-numeric!")}
  if(!anyNA(asd) & !is.numeric(asd)){stop("asd is non-numeric!")}
  if(!anyNA(b) & !is.numeric(b)){stop("b is non-numeric!")}
  if(!anyNA(bsd) & !is.numeric(bsd)){stop("bsd is non-numeric!")}
  
  if(anyNA(asd)){asd = 0}
  if(anyNA(bsd)){bsd = 0}
  
  ci.lower = (100-ci.width)/2/100
  ci.upper = 1-ci.lower
  
  if(anyNA(b)){
    data <- data.frame(a = a, asd = asd) %>% 
      rowwise() %>% 
      mutate(rep = list(data.frame(dist = rnorm(n=n,mean=a,sd=asd), rep = 1:n))) %>% 
      unnest(rep) %>% 
      mutate(rep = sample(rep)) %>% 
      group_by(rep) %>% 
      summarize(dist = stat.function(a),.groups="keep")
    
    tidy.data <- data %>% 
      ungroup() %>% 
      summarize(mean  = mean(dist),
                median = sort(dist)[0.5*n],
                sd = sd(dist),
                lower = sort(dist)[ci.lower*n],
                upper = sort(dist)[ci.upper*n],
                p.value = ifelse(sign(lower-null)==sign(upper-null),paste0("p < ",ci.lower*2),paste0("p > ",ci.lower*2)))
  }
  
  if(!anyNA(b) & paired){
    data = data.frame(a=a,asd=asd,b=b,bsd=bsd) %>% 
      rowwise() %>% 
      mutate(rep = list(data.frame(a.dist = rnorm(n=n,mean=a,sd=asd),
                                   b.dist = rnorm(n=n,mean=b,sd=bsd),
                                   rep = 1:n))) %>% 
      unnest(rep) %>% 
      mutate(rep = sample(rep)) %>% 
      group_by(rep) %>% 
      summarize(a = stat.function(a.dist),
                b = stat.function(b.dist),
                .groups="keep") %>% 
      mutate(diff = b - a)
    
    tidy.data <- data %>% 
      ungroup() %>% 
      summarize(mean = mean(diff),
                median = sort(diff)[0.5*n],
                sd = sd(diff),
                lower = sort(diff)[ci.lower*n],
                upper = sort(diff)[ci.upper*n],
                p.value = ifelse(sign(lower-null)==sign(upper-null),paste0("p < ",ci.lower*2),paste0("p > ",ci.lower*2)))
  }
  
  if(!anyNA(b) & !paired){
    data = rbind(data.frame(group = "a",values = a,sd = asd),
                 data.frame(group = "b",values = b,sd = bsd)) %>% 
      rowwise() %>% 
      mutate(rep = list(data.frame(dist = rnorm(n=n,mean=values,sd=sd),
                                   rep = 1:n))) %>% 
      unnest(rep) %>% 
      group_by(group) %>% 
      mutate(rep = sample(rep)) %>% 
      group_by(group,rep) %>% 
      summarize(dist = stat.function(dist),.groups="keep") %>% 
      spread(group,dist) %>%
      mutate(diff = b-a)
    tidy.data <- data %>% 
      ungroup() %>% 
      summarize(mean = mean(diff),
                median = sort(diff)[0.5*n],
                sd = sd(diff),
                lower = sort(diff)[ci.lower*n],
                upper = sort(diff)[ci.upper*n],
                p.value = ifelse(sign(lower-null)==sign(upper-null),paste0("p < ",ci.lower*2),paste0("p > ",ci.lower*2)))
    
  }
  
  if(anyNA(b)){input <- data.frame(group = "a",values = a,sd = asd)}
  if(!anyNA(b)){input <- rbind(data.frame(index = 1:length(a),group = "a",values = a,sd = asd),
                               data.frame(index = 1:length(b),group = "b",values = b,sd = bsd))}
  
  list("tidy" = tidy.data, #just the summary statistics
       "data" = data, #the entire bootstrap results, including distributions
       "parameters" = data.frame(ci.width,n,null,paired), #Input parameters
       "stat.function" = stat.function, # Summary statistic function
       "input" = input) #the a and b input data
}