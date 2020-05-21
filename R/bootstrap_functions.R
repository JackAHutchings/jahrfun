
#' @export
lm.bal <- function(x,
                   y,
                   xsd=0,
                   ysd=0,
                   n=10000,
                   ci.width=95,
                   method=c("ols","rma"),
                   pred.band = T,
                   pred.steps = 25){

  # x: x-axis values, a numeric vector
  # y: y-axis values, a numeric vector
  # xsd: (optional) Either a single value or a vector with length equal to x. Used to generate normal distributions around each x value with SD equal to xsd.
  # ysd: (optional) Either a single value or a vector with length equal to y. Used to generate normal distributions around each y value with SD equal to ysd.
  # n: Number of bootstrap replicates to perform.
  # ci.width: Width of the confidence interval to use for hypothesis testing, a single numeric value between 1 and 100
  # method: regression method. either 'ols" for Ordinary Least Squares or 'rma' for Reduced Major Axis.
  # pred.band: Compute the prediction interval? This is computationally intensive, especially for large datasets.
  # pred.steps: # of steps to calculate the 95% prediction interval at. Increase for higher resolution at cost of computation time.

  if(length(x) != length(y)){stop("x and y lengths not equal")}
  if(length(xsd) > 1 & length(xsd) != length(x)){stop("xsd must either be a single value or a vector of length equal to x")}
  if(length(ysd) > 1 & length(ysd) != length(y)){stop("ysd must either be a single value or a vector of length equal to y")}
  if(ci.width < 1 | ci.width > 100){stop("ci.width must be between 1 and 100")}

  lower = (100-ci.width)/2/100
  upper = 1-lower

  input = data.frame(x = x,
                     y = y,
                     xsd = xsd,
                     ysd = ysd)

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
    summarize(slope = ifelse(method[1]=="ols", cov(x,y)/var(x),NA),
              slope = ifelse(method[1]=="rma", sign(cov(x,y))*sd(y)/sd(x),slope),
              intercept = mean(y)-slope*mean(x),
              r2 = cor(x,y)^2) %>%
    ungroup() %>%
    mutate(slope.observed = mean(slope),
           slope.lower = sort(slope)[lower*n],
           slope.upper = sort(slope)[upper*n],
           slope.sig = ifelse(sign(slope.lower) == sign(slope.upper),paste0("< ",lower*2),paste0("> ",lower*2)),
           intercept.observed = mean(intercept),
           intercept.lower = sort(intercept)[lower*n],
           intercept.upper = sort(intercept)[upper*n],
           intercept.sig = ifelse(sign(intercept.lower) == sign(intercept.upper),paste0("< ",lower*2),paste0("> ",lower*2)),
           r2.observed = mean(r2),
           r2.lower = sort(r2)[lower*n],
           r2.upper = sort(r2)[upper*n])

  if(pred.band){
    conf.band = data %>%
      mutate(x = list(seq(min(x),max(x),((max(x)-min(x))/pred.steps)))) %>%
      unnest(x) %>%
      group_by(rep,x) %>%
      summarize(prediction = slope * x + intercept) %>%
      group_by(x) %>%
      summarize(prediction.observed = mean(prediction),
                prediction.lower = sort(prediction)[lower*n],
                prediction.upper = sort(prediction)[upper*n])
  } else if(!pred.band){
    conf.band = data.frame(x = x) %>%
      mutate(prediction.observed = x * data$slope.observed[1] + data$intercept.observed[1],
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
#' @export
one.samp.bal.sd <- function(a, n=10000){
  if(length(a)>1){
    data.frame(a) %>%
      mutate(rep = list(1:(n))) %>%
      unnest() %>%
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


#' Perform a two-sample balanced bootstrap using mean as the sampling statistic, defaults to unpaired
#' @export
two.samp.bal <- function(a,b,n=10000,paired=F,dist=F){
  a <- na.omit(a)
  b <- na.omit(b)
  diff.observed <- mean(b) - mean(a)
  if (paired==F) {
    shuffled <- rep(c(a,b),n)
    shuffled <- sample(shuffled,length(shuffled))
    a.random <- colMeans(matrix(shuffled[1:(length(a)*n)],nrow=length(a),ncol=n))
    b.random <- colMeans(matrix(shuffled[(length(a)*n+1):length(shuffled)],nrow=length(b),ncol=n))
    diff.random <- c(scale(b.random-a.random,scale=F),diff.observed)
    p.value <- round(sum(abs(diff.random) >= abs(diff.observed)) / (n+1), floor(log10((n+1))))
    #Output
    if (dist==F){data.frame(diff.observed,p.value)}
    else if (dist==T){
      list("dist" = diff.random,
           "diff.observed" = diff.observed,
           "p.value" = p.value)
    }
  }
  else if (paired==T) {
    diff.sample <- b - a
    shuffled <- rep(diff.sample,(n-1))
    shuffled <- sample(shuffled,length(shuffled))
    diff.random <- c(colMeans(matrix(shuffled,nrow=length(a),ncol=(n-1))),diff.observed)
    diff.95ci <- c(sort(diff.random)[(n*0.025)],sort(diff.random)[(n*0.975)])
    p.value <- ifelse((sign(diff.95ci[1])==sign(diff.95ci[2]))==T,"< 0.05","> 0.05")
    #Output
    if (dist==F){
      as.data.frame(list(
        "diff.observed" = diff.observed,
        "ci.2.5" = diff.95ci[1],
        "ci.97.5" = diff.95ci[2],
        "p.value" = p.value))
    }
    else if (dist==T) {
      list("dist" = diff.random, #bootstrapped distribution
           "obs" = diff.observed, #observed difference in means
           "ci.2.5" = diff.95ci[1], # 2.5 percentile
           "ci.97.5" = diff.95ci[2], # 97.5 percentile
           "p" = p.value) #two-tailed p value
    }
  }
}
