#' @export
GeomLMBoot <- ggproto("GeomLMBoot",Geom,
                      required_aes = c("x","y"),
                      optional_aes = c("xsd","ysd"),
                      default_aes = aes(colour = "black", linetype = 1, alpha = NA),
                      draw_panel = function(data,panel_params,coord,
                                            method, # Regression method, see the lm.bal.internal function below
                                            location, # Plot region, accepts four text-based options: "tl","tr","bl","br" for top-left, top-right, bottom-left, bottom-right
                                            n, # Bootstrap N, defaults to 10000
                                            show.fit, # Should the regression fit be shown?
                                            show.label, # Should the regression summary be shown?
                                            show.pred.band, # Compute a bootstrapped prediction interval?
                                            linesize, # Size of fit line.
                                            ci.width, # Confidence interval width (%)
                                            fill, # Color of the prediction interval
                                            pred.alpha, # Alpha (opacity) of the prediction interval
                                            pred.steps, # of steps to calculate the 95% prediction interval at. Increase for higher resolution at cost of computation time.
                                            m, # Should the slope be printed?
                                            m.95, # Should the slope 95% CI be printed?
                                            int, # Should the intercept by printed?
                                            int.95, # Should the intercept 95% CI be printed?
                                            r2, # Should the r2 be printed?
                                            r2.95, # Should the r2 95% CI be printed?
                                            p, # Should the p-value be printed? This only prints if it is above or below 0.05
                                            fontfamily, # Font family as a string. Defaults to "Times" aka Times New Roman, but other OS-specific calls should work
                                            fontsize, # Font size in points
                                            coef.digits, # Number of digits for the coefficients
                                            nudge_x, # X-axis adjustment of the text
                                            nudge_y # Y-axis adjustment of the text
                      ){

                        #Generate CI for slope and r2 using the balanced bootstrap approach

                        plot_data <- coord$transform(data,panel_params)

                        if(is.null(data$xsd)){data$xsd=0}
                        if(is.null(data$ysd)){data$ysd=0}

                        full_model <- lm.bal(x = data$x,
                                             y = data$y,
                                             xsd = data$xsd,
                                             ysd = data$ysd,
                                             n = n,
                                             method = method,
                                             ci.width= ci.width,
                                             pred.band = show.pred.band,
                                             pred.steps = pred.steps)

                        model <- full_model$tidy
                        regression <- full_model$conf.band
                        
                        # print(regression)


                        if(m.95==T){
                          eq.slope = paste("Slope = ",round(model$slope.observed,coef.digits),
                                           " (",round(model$slope.lower, digits=coef.digits),", ",
                                           round(model$slope.upper, digits=coef.digits),")",sep="")}
                        if(m.95==F){eq.slope = paste("Slope = ",round(model$slope.observed, digits =coef.digits),sep="")}

                        if(int.95==T){
                          eq.int = paste("Intercept = ",round(model$intercept.observed,digits=coef.digits),
                                         " (",round(model$intercept.lower, digits=coef.digits),", ",
                                         round(model$intercept.upper, digits=coef.digits),")",sep="")}
                        if(int.95==F){eq.int = paste("Intercept = ",round(model$intercept.observed, digits =coef.digits),sep="")}

                        if(r2.95==T){
                          eq.r2 = paste("r\u00B2 = ",round(model$r2.observed,digits=coef.digits),
                                        " (",round(model$r2.lower, digits=coef.digits),", ",
                                        round(model$r2.upper, digits=coef.digits),")",sep="")}
                        if(r2.95==F){eq.r2 = paste("r\u00B2 = ",round(model$r2.observed, digits =coef.digits),sep="")}

                        eq.p = paste("p ",model$slope.sig)

                        eq = list(m = eq.slope,
                                  int = eq.int,
                                  r2 = eq.r2,
                                  p = eq.p)

                        if(m==F) {eq$m<-NA}
                        if(int==F) {eq$int<-NA}
                        if(r2==F) {eq$r2<-NA}
                        if(p==F) {eq$p<-NA}

                        eq = eq[!is.na(eq)]
                        eq = paste(eq,collapse="\n")

                        if(location=="tl"){xloc = 0.025 ; yloc = 0.975 ; vjust = 1}
                        if(location=="tr"){xloc = 0.600 ; yloc = 0.975 ; vjust = 1}
                        if(location=="bl"){xloc = 0.025 ; yloc = 0.025 ; vjust = 0}
                        if(location=="br"){xloc = 0.600 ; yloc = 0.025 ; vjust = 0}

                        if(show.label){
                          label_grob <- textGrob(label = eq,
                                                 x= xloc + nudge_x,
                                                 y= yloc + nudge_y,
                                                 default.units = "native",
                                                 hjust = 0,
                                                 vjust = vjust,
                                                 gp = gpar(fontsize = fontsize * .pt,
                                                           fontfamily = fontfamily))
                        }else if(!show.label){label_grob <- NULL}


                        # print(plot_data)
                        # print(data)

                        segmentdata_x <- plot_data %>% 
                          select(x) %>% 
                          arrange(x) %>% 
                          filter(x == min(x) | x == max(x)) %>% 
                          distinct()
                        
                        
                        segmentdata_y <- plot_data %>% 
                          select(y) %>% 
                          arrange(y) %>% 
                          filter(y == min(y) | y == max(y)) %>% 
                          distinct()

                        
                        realdata_x <- data %>%
                          select(x) %>% 
                          arrange(x) %>% 
                          filter(x == min(x) | x == max(x)) %>% 
                          distinct()
                        
                        realdata_y <- data %>%
                          select(y) %>% 
                          arrange(y) %>% 
                          filter(y == min(y) | y == max(y)) %>% 
                          distinct()
                        
                        scale_x_slope = (segmentdata_x$x[2] - segmentdata_x$x[1]) / (realdata_x$x[2] - realdata_x$x[1])
                        scale_x_intercept = mean(segmentdata_x$x) - scale_x_slope*mean(realdata_x$x)
                        scale_y_slope = (segmentdata_y$y[2] - segmentdata_y$y[1]) / (realdata_y$y[2] - realdata_y$y[1])
                        scale_y_intercept = mean(segmentdata_y$y) - scale_y_slope*mean(realdata_y$y)
                        
                        # print(scale_x_slope)
                        # print(scale_x_intercept)

                        if(show.fit){
                          regressiondata <- regression %>%
                            filter(x == min(x) | x == max(x)) %>%
                            distinct() %>%
                            rename(y = prediction.observed) %>%
                            mutate(x = x * scale_x_slope + scale_x_intercept,
                                   y = y * scale_y_slope + scale_y_intercept)
                          
                          # print(regressiondata)

                          fit_grob <- segmentsGrob(
                            x0=regressiondata$x[1],
                            y0=regressiondata$y[1],
                            x1=regressiondata$x[2],
                            y1=regressiondata$y[2],
                            default.units = "native",
                            arrow=NULL,
                            gp = gpar(
                              col = alpha(plot_data$colour,plot_data$alpha),
                              fill = alpha(plot_data$colour,plot_data$alpha),
                              lwd = linesize * .pt,
                              lty = plot_data$linetype[1],
                              lineend = "butt",
                              linejoin = "round",
                              linemitre = 10)
                          )
                        } else if(!show.fit){fit_grob <- NULL}

                        if(show.pred.band){

                          pred_data <- regression %>%
                            select(x, prediction.lower,prediction.upper) %>%
                            gather(interval,y,prediction.lower,prediction.upper) %>%
                            mutate(x = x * scale_x_slope + scale_x_intercept,
                                   y = y * scale_y_slope + scale_y_intercept) %>%
                            group_by(interval) %>%
                            mutate(order = ifelse(interval == "prediction.lower",(1:n()),(n()+n()):(n()+1))) %>%
                            ungroup() %>% arrange(order)

                          pred_grob <- polygonGrob(
                            x = pred_data$x,
                            y = pred_data$y,
                            default.units = "native",
                            gp = gpar(
                              fill = alpha(fill, pred.alpha),
                              col = NA,
                              lwd = 1,
                              lty = "solid"
                            )
                          )


                        }else if(!show.pred.band){pred_grob <- NULL}

                        grobTree(
                          label_grob,
                          fit_grob,
                          pred_grob
                        )

                      })



#' Do a balanced bootstrap on a linear model.
#'
#' This function calculates a linear regression of y versus x. The data
#' are bootstrapped to generate confidence intervals around the slope and
#' intercept. Optionally, the r2 value may also be calculated. By default,
#' the geom prints the text of the results in the upper-left corner of the
#' plot and also plots the fit line. The bootstrapped prediction interval
#' around the fit may also be plotted.
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#'   or \code{\link[ggplot2]{aes_}}.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer,
#'   as a string.
#' @param method Regression model of the slope. Defaults to `ols` for Ordinary Least Squares, but may also be `rma` to perform a Reduced Major Axis (i.e., Standard Major Axis - not Ranged)
#' @param location Location to print fit results. Defaults to `tl` (top-left), but may be `tr`, `bl`, or `br`.
#' @param n Bootstrap N, defaults to 10000
#' @param show.fit Should the regression fit line be shown? Boolean, defaults to TRUE.
#' @param show.label Should the regression fit summary be shown? Boolean, defaults to TRUE.
#' @param show.pred.band Should the prediction interval be calculated and shown? Boolean. This is computationally intensive because it is also bootstrapped.
#' @param linesize The size of the fit line (in points).
#' @param ci.width The width of the confidence interval to use (in percents).
#' @param fill Color of the prediction interval.
#' @param pred.alpha Alpha of the prediction interval.
#' @param pred.steps Number of steps to calculate the prediction interval at. Increase for higher resolution at cost of computation time.
#' @param m Should the slope coefficient be shown? Boolean, defaults to TRUE.
#' @param m.95 Should the slope's confidence interval be shown? Boolean, defaults to TRUE.
#' @param int Should the intercept be shown? Boolean, defaults to TRUE.
#' @param int.95 Should the intercept's confidence interval be shown? Boolean, defaults to TRUE.
#' @param r2 Should Pearson's r-squared be shown? Boolean, defaults to TRUE.
#' @param r2.95 Should the r-squared confidence interval be shown? Boolean, defaults to FALSE. This can be misleading for poor fits because the calculated r value may be either negative or positive and is then squared. Use with caution.
#' @param p Should the slope's p-value be printed? This only prints if the hypothetical 'p' is above or below the significance level chosen. I.e., does the slope's confidence interval overlap with zero?
#' @param fontfamily Font family as a string to use for label plotting.
#' @param fontsize Font size as an integer (in pointS). Used for lable plotting.
#' @param coef.digits Number of digits to round to in the fit label.
#' @param nudge_x Manual X-axis adjustment of the text. In plotting units (where plotting area ranges from 0 to 1).
#' @param nudge_y Manual Y-axis adjustment of the text. In plotting units (where plotting area ranges from 0 to 1).
#' @section Aesthetics:
#' The geom understands the following aesthetics (required are in bold):
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#' \item xsd (Vector with length 1 or equal to X. If present, used as the SD term of an rnorm call for each X value during bootstrapping)
#' \item ysd (Vector with length 1 or equal to Y. If present, used as the SD term of an rnorm call for each Y value during bootstrapping)
#' }
#' @export
geom_lmboot <- function(mapping = NULL,
                        data = NULL,
                        stat = "identity",
                        show.legend = NA,
                        position = "identity",
                        ...,
                        n = 10000,
                        show.fit=T,
                        show.label=T,
                        linesize=1,
                        location = "tl",
                        method="ols",
                        m=T,
                        m.95=T,
                        int=T,
                        int.95=T,
                        r2=T,
                        r2.95=F,
                        p=T,
                        fontfamily="serif",
                        ci.width=95,
                        show.pred.band=F,
                        fill="blue",
                        pred.alpha=0.25,
                        pred.steps=25,
                        fontsize=6,
                        coef.digits=2,
                        nudge_x = 0,
                        nudge_y = 0,
                        inherit.aes = T
) {
  layer(geom = GeomLMBoot, stat = stat, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(location = location,
                      method = method,
                      n=n,
                      show.fit=show.fit,
                      show.label = show.label,
                      show.pred.band=show.pred.band,
                      linesize=linesize,
                      m=m,
                      m.95=m.95,
                      int=int,
                      int.95=int.95,
                      r2=r2,
                      r2.95=r2.95,
                      p=p,
                      fontfamily=fontfamily,
                      ci.width=ci.width,
                      fill=fill,
                      pred.alpha=pred.alpha,
                      pred.steps=pred.steps,
                      fontsize=fontsize*5/14,
                      coef.digits=coef.digits,
                      nudge_x=nudge_x,
                      nudge_y=nudge_y,
                      ...)
  )
}


#' Perform a one-sample balanced bootstrap using mean as the sampling statistic - output designed for stat_summary in ggplot
#'
#' @param x Dataset, a numerical vector.
#' @param n Number of bootstrap replicates to perform.
#' @param ci Width of the confidence interval in percents. Defaults to 95.
#'
#' @export
mean_ci_bal <- function(x,n=10000,ci=95){
  mean_observed = mean(x)
  data = data.frame(dist = rep(x,(n-1)),
                    index = sample(rep(seq(1,n-1),length(x)),length(x)*(n-1))) %>%
    group_by(index) %>% summarize(mean = mean(dist)) %>%
    select(mean)
  dist = c(data$mean,mean_observed)
  data.frame(y = mean(dist),
             ymin = sort(dist)[(100-ci)/2/100*length(dist)],
             ymax = sort(dist)[(100+ci)/2/100*length(dist)])
}


#' Calculate the mean and confidence intervals of a distribution - output designed for stat_summary in ggplot
#'
#' @param x Dataset, a numerical vector.
#' @param ci Width of the confidence interval in percents. Defaults to 95.
#'
#' @export
mean_ci_dist <- function(x,ci=95){
  data.frame(y = mean(x),
             ymin = sort(x)[(100-ci)/2/100*length(x)],
             ymax = sort(x)[(100+ci)/2/100*length(x)])
}

#' A simple linear model geom.
#'
#' A simplified edit of geom_smooth that just draws a y~x OLS regression line.
#' @export
geom_lm <- function(...,formula=y~x,method=lm,se=F){ggplot2::geom_smooth(...,formula=formula,method=method,se=se)}

#' Editted ggsave2 from cowplot function.
#'
#' Edit of ggplot save (actually the cowplot implementation ggsave2) that includes some sensible defaults, defaults units to mm,
#' and uses ImageMagick to trim whitespace from an exported png.
#' @export
ggsave <- function(
    filename, plot = ggplot2::last_plot(), device = NULL,
    path = NULL, scale = 1, width = NA, height = NA,
    units = c("mm"), dpi = 600, limitsize = TRUE, border=50, ...){

    if(is.na(width)){width = 1.25*height}
    if(is.na(height)){height = 1.25*width}

    cowplot::ggsave2(filename=filename,
                     plot=plot,
                     device=device,
                     path=path,
                     scale=scale,
                     width=width,
                     height=height,
                     units=units,
                     dpi=dpi,
                     limitsize=limitsize,
                     ...)
    if(grepl(".png",filename)){system(paste0("magick convert \"",filename,"\" -trim -bordercolor white -border ",border," \"",filename,"\""))}
}






#' @export
GeomLMFreq <- ggproto("GeomLMFreq",Geom,
                      required_aes = c("x","y"),
                      default_aes = aes(colour = "black", linetype = 1, alpha = NA),
                      draw_panel = function(data,panel_params,coord,
                                            location, # Plot region, accepts four text-based options: "tl","tr","bl","br" for top-left, top-right, bottom-left, bottom-right
                                            n, # Bootstrap N, defaults to 10000
                                            show.fit, # Should the regression fit be shown?
                                            show.label, # Should the regression summary be shown?
                                            linesize, # Size of fit line.
                                            m, # Should the slope be printed?
                                            m.err, # Should the slope standard error be printed?
                                            int, # Should the intercept by printed?
                                            int.err, # Should the intercept standard error be printed?
                                            r2, # Should the r2 be printed?
                                            p, # Should the p-value be printed?
                                            fontfamily, # Font family as a string. Defaults to "Times" aka Times New Roman, but other OS-specific calls should work
                                            fontsize, # Font size in points
                                            coef.digits, # Number of digits for the coefficients
                                            significance.level, # Significance level for the slope
                                            nudge_x, # X-axis adjustment of the text
                                            nudge_y # Y-axis adjustment of the text
                      ){
                        
                        #Generate CI for slope and r2 using the balanced bootstrap approach
                        
                        plot_data <- coord$transform(data,panel_params)
                        

                        full_model <- summary(lm(y ~ x,data=data))
                        
                        if(m.err==T){
                          eq.slope = paste("Slope = ",round(full_model$coefficients[2,1],coef.digits),
                                           " \u00b1 ",round(full_model$coefficients[2,2], digits=coef.digits),sep="")}
                        if(m.err==F){eq.slope = paste("Slope = ",round(full_model$coefficients[2,1], digits =coef.digits),sep="")}
                        
                        if(int.err==T){
                          eq.int = paste("Intercept = ",round(full_model$coefficients[1,1],digits=coef.digits),
                                         " \u00b1 ",round(full_model$coefficients[1,2], digits=coef.digits),sep="")}
                        if(int.err==F){eq.int = paste("Intercept = ",round(full_model$coefficients[1,1], digits =coef.digits),sep="")}
                        
                        eq.r2 = paste("r\u00B2 = ",round(full_model$r.squared, digits =coef.digits),sep="")
                        
                        
                        eq.p = paste("p ",ifelse(full_model$coefficients[2,4]<significance.level,paste("<",significance.level),paste(">",significance.level)))
                        
                        eq = list(m = eq.slope,
                                  int = eq.int,
                                  r2 = eq.r2,
                                  p = eq.p)
                        
                        if(m==F) {eq$m<-NA}
                        if(int==F) {eq$int<-NA}
                        if(r2==F) {eq$r2<-NA}
                        if(p==F) {eq$p<-NA}
                        
                        eq = eq[!is.na(eq)]
                        eq = paste(eq,collapse="\n")
                        
                        if(location=="tl"){xloc = 0.025 ; yloc = 0.975 ; vjust = 1}
                        if(location=="tr"){xloc = 0.600 ; yloc = 0.975 ; vjust = 1}
                        if(location=="bl"){xloc = 0.025 ; yloc = 0.025 ; vjust = 0}
                        if(location=="br"){xloc = 0.600 ; yloc = 0.025 ; vjust = 0}
                        
                        if(show.label){
                          label_grob <- textGrob(label = eq,
                                                 x= xloc + nudge_x,
                                                 y= yloc + nudge_y,
                                                 default.units = "native",
                                                 hjust = 0,
                                                 vjust = vjust,
                                                 gp = gpar(fontsize = fontsize * .pt,
                                                           fontfamily = fontfamily))
                        }else if(!show.label){label_grob <- NULL}

                        segmentdata_x <- plot_data %>% 
                          select(x) %>% 
                          arrange(x) %>% 
                          filter(x == min(x) | x == max(x)) %>% 
                          distinct()
                        
                        
                        segmentdata_y <- plot_data %>% 
                          select(y) %>% 
                          arrange(y) %>% 
                          filter(y == min(y) | y == max(y)) %>% 
                          distinct()
                        
                        
                        realdata_x <- data %>%
                          select(x) %>% 
                          arrange(x) %>% 
                          filter(x == min(x) | x == max(x)) %>% 
                          distinct()
                        
                        realdata_y <- data %>%
                          select(y) %>% 
                          arrange(y) %>% 
                          filter(y == min(y) | y == max(y)) %>% 
                          distinct()
                        
                        scale_x_slope = (segmentdata_x$x[2] - segmentdata_x$x[1]) / (realdata_x$x[2] - realdata_x$x[1])
                        scale_x_intercept = mean(segmentdata_x$x) - scale_x_slope*mean(realdata_x$x)
                        scale_y_slope = (segmentdata_y$y[2] - segmentdata_y$y[1]) / (realdata_y$y[2] - realdata_y$y[1])
                        scale_y_intercept = mean(segmentdata_y$y) - scale_y_slope*mean(realdata_y$y)
                        
                        
                        if(show.fit){
                          regressiondata <- realdata_x %>%
                            mutate(y = x * full_model$coefficients[2,1] + full_model$coefficients[1,1]) %>% 
                            mutate(x = x * scale_x_slope + scale_x_intercept,
                                   y = y * scale_y_slope + scale_y_intercept)
                          
                          fit_grob <- segmentsGrob(
                            x0=regressiondata$x[1],
                            y0=regressiondata$y[1],
                            x1=regressiondata$x[2],
                            y1=regressiondata$y[2],
                            default.units = "native",
                            arrow=NULL,
                            gp = gpar(
                              col = alpha(plot_data$colour,plot_data$alpha),
                              fill = alpha(plot_data$colour,plot_data$alpha),
                              lwd = linesize * .pt,
                              lty = plot_data$linetype[1],
                              lineend = "butt",
                              linejoin = "round",
                              linemitre = 10)
                          )
                        } else if(!show.fit){fit_grob <- NULL}
                        

                        
                        grobTree(
                          label_grob,
                          fit_grob
                        )
                        
                      })



#' Perform a linear regression using the lm command and output the results..
#'
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#'   or \code{\link[ggplot2]{aes_}}.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer,
#'   as a string.
#' @param location Location to print fit results. Defaults to `tl` (top-left), but may be `tr`, `bl`, or `br`.
#' @param show.fit Should the regression fit line be shown? Boolean, defaults to TRUE.
#' @param show.label Should the regression fit summary be shown? Boolean, defaults to TRUE.
#' @param linesize The size of the fit line (in points).
#' @param m Should the slope coefficient be shown? Boolean, defaults to TRUE.
#' @param m.err Should the slope's standard error be shown? Boolean, defaults to TRUE.
#' @param int Should the intercept be shown? Boolean, defaults to TRUE.
#' @param int.err Should the intercept's standard error be shown? Boolean, defaults to TRUE.
#' @param r2 Should Pearson's r-squared be shown? Boolean, defaults to TRUE.
#' @param p Should the slope's significance be shown?
#' @param signifiance.level Significance level to use for hypothesis testing. Default is 0.05
#' @param fontfamily Font family as a string to use for label plotting.
#' @param fontsize Font size as an integer (in pointS). Used for lable plotting.
#' @param coef.digits Number of digits to round to in the fit label.
#' @param nudge_x Manual X-axis adjustment of the text. In plotting units (where plotting area ranges from 0 to 1).
#' @param nudge_y Manual Y-axis adjustment of the text. In plotting units (where plotting area ranges from 0 to 1).
#' @section Aesthetics:
#' The geom understands the following aesthetics (required are in bold):
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#' }
#' @export
geom_lmfreq <- function(mapping = NULL,
                        data = NULL,
                        stat = "identity",
                        show.legend = NA,
                        position = "identity",
                        ...,
                        show.fit=T,
                        show.label=T,
                        linesize=1,
                        location = "tl",
                        m=T,
                        m.err=T,
                        int=T,
                        int.err=T,
                        r2=T,
                        p=T,
                        significance.level=0.05,
                        fontfamily="serif",
                        fontsize=6,
                        coef.digits=2,
                        nudge_x = 0,
                        nudge_y = 0,
                        inherit.aes = T
) {
  layer(geom = GeomLMFreq, stat = stat, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(location = location,
                      show.fit=show.fit,
                      show.label = show.label,
                      linesize=linesize,
                      m=m,
                      m.err=m.err,
                      int=int,
                      int.err=int.err,
                      r2=r2,
                      p=p,
                      significance.level=significance.level,
                      fontfamily=fontfamily,
                      fontsize=fontsize*5/14,
                      coef.digits=coef.digits,
                      nudge_x=nudge_x,
                      nudge_y=nudge_y,
                      ...)
  )
}
