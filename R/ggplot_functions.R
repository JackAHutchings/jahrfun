#' @export
GeomLMBoot <- ggproto("GeomLMBoot",Geom,
                      required_aes = c("x","y"),
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
                                                 y= yloc + nudge_x,
                                                 default.units = "native",
                                                 hjust = 0,
                                                 vjust = vjust,
                                                 gp = gpar(fontsize = fontsize * .pt,
                                                           fontfamily = fontfamily))
                        }else if(!show.label){label_grob <- NULL}


                        # print(plot_data)
                        # print(data)

                        segmentdata <- plot_data %>%
                          mutate(index = 1:n()) %>%
                          filter(index == min(index) | index == max(index))

                        realdata <- data %>%
                          mutate(index = 1:n()) %>%
                          filter(index == min(index) | index == max(index))

                        scale_x_slope = (segmentdata$x[2] - segmentdata$x[1]) / (realdata$x[2] - realdata$x[1])
                        scale_x_intercept = mean(segmentdata$x) - scale_x_slope*mean(realdata$x)
                        scale_y_slope = (segmentdata$y[2] - segmentdata$y[1]) / (realdata$y[2] - realdata$y[1])
                        scale_y_intercept = mean(segmentdata$y) - scale_y_slope*mean(realdata$y)

                        if(show.fit){
                          regressiondata <- regression %>%
                            filter(x == min(x) | x == max(x)) %>%
                            distinct() %>%
                            rename(y = prediction.observed) %>%
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
                        fontfamily="Times",
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
#' @export
mean_ci_dist <- function(x,ci=95){
  data.frame(y = mean(x),
             ymin = sort(x)[(100-ci)/2/100*length(x)],
             ymax = sort(x)[(100+ci)/2/100*length(x)])
}


#jah_ggplot_theme
{
  base_font_size = 8 # Default Font size in points.
  base_font_family = "Times" # Default font family.
  unit <- function(...,units="mm"){grid::unit(...,units=units)} #Set default units to mm
  margin <- function(...,unit="mm"){ggplot2::margin(...,unit=unit)} #Same as above, but for the margin comment.

  theme_set(theme(line = element_line(color="black",size=0.25,linetype=1,lineend="butt",arrow=F,inherit.blank=T),
                  rect = element_rect(fill=NA,color="black",size=0.25,linetype=1,inherit.blank=T),
                  text = element_text(family=base_font_family,face="plain",color="black",size=base_font_size,hjust=0.5,vjust=0.5,
                                      angle=0,lineheight=0.9,margin=margin(0,0,0,0),debug=F),
                  axis.title.x = element_text(margin=margin(2.75,0,0,0),inherit.blank = T),
                  axis.title.x.top = element_text(margin=margin(0,0,2.75,0),inherit.blank = T),
                  axis.title.y = element_text(vjust = 1,angle=90,margin=margin(0,2.75,0,0),inherit.blank = T),
                  axis.title.y.right = element_text(vjust = 0,angle=-90,margin=margin(0,0,0,2.75),inherit.blank = T),
                  axis.text = element_text(inherit.blank = T),
                  axis.text.x = element_text(margin=margin(0.75,0,0,0),inherit.blank = T),
                  axis.text.x.top = element_text(vjust=0,margin=margin(0,0,0.75,0),inherit.blank = T),
                  axis.text.y = element_text(hjust=1,margin=margin(0,0.75,0,0),inherit.blank = T),
                  axis.text.y.right = element_text(hjust=0,margin=margin(0,0,0,0.75),inherit.blank = T),
                  axis.ticks = element_line(size=0.2,inherit.blank = T),
                  axis.ticks.x = element_line(size=0.2,inherit.blank = T),
                  axis.ticks.y = element_line(size=0.2,inherit.blank = T),
                  axis.ticks.length = unit(1),
                  axis.line = element_line(inherit.blank = T),
                  axis.line.x = element_line(inherit.blank = T),
                  axis.line.y = element_line(inherit.blank = T),
                  legend.background = element_blank(), #default is element_rect()
                  legend.margin = margin(2,2,2,2),
                  legend.spacing = unit(4),
                  legend.spacing.x = unit(4),
                  legend.spacing.y = unit(4),
                  legend.key = element_blank(), #default is element_rect()
                  legend.key.size = unit(2.5),
                  legend.key.height = unit(2.5),
                  legend.key.width = unit(2.5),
                  legend.text = element_text(inherit.blank = T),
                  legend.text.align = 0,
                  legend.title = element_text(hjust=0,inherit.blank = T),
                  legend.title.align = 0.5, # alignment of legend title (number from 0 (left) to 1 (right))
                  legend.position = "top", # the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
                  legend.direction = "horizontal", # layout of items in legends ("horizontal" or "vertical")
                  legend.justification = "center", #anchor point for positioning legend inside plot, default is "center"
                  legend.box = "horizontal", #arrangment of multiple legends ("horizontal" or "vertical")
                  legend.box.just = "left", # justification of each legend within the overall bounding box, when there are multiple legends ("top", "bottom", "left", or "right")
                  legend.box.margin = margin(0,0,0,0),
                  legend.box.background = element_blank(), #background of box around multiple legends
                  legend.box.spacing = unit(4),
                  panel.background = element_rect(fill="white",color=NA,inherit.blank = T),
                  panel.border = element_rect(fill=NA,color="black",inherit.blank = T),
                  panel.spacing = unit(2), #Space between facets
                  panel.spacing.x = unit(2), #Space between facets
                  panel.spacing.y = unit(2), #Space between facets
                  panel.grid = element_blank(),
                  panel.grid.major = element_line(inherit.blank = T),
                  panel.grid.minor = element_line(inherit.blank = T),
                  panel.grid.major.x = element_line(inherit.blank = T),
                  panel.grid.major.y = element_line(inherit.blank = T),
                  panel.grid.minor.x = element_line(inherit.blank = T),
                  panel.grid.minor.y = element_line(inherit.blank = T),
                  panel.ontop = F, #Place the panel on top of the plot? Not sure why this is here.
                  plot.background = element_rect(color=NA),
                  plot.title = element_text(size=base_font_size+2,hjust=0,vjust=1,margin=margin(0,0,2.75,0),inherit.blank = T),
                  plot.subtitle = element_text(hjust=0,vjust=1,margin=margin(0,0,1.5,0),inherit.blank = T),
                  plot.caption = element_text(hjust=1,vjust=1,margin=margin(1.5,0,0,0),inherit.blank = T),
                  plot.margin = margin(1.5,1.5,1,1),
                  strip.background = element_rect(color=NA,inherit.blank = T),
                  strip.placement = "outside", #'inside' or 'outside' relative to the axis ticks/text
                  strip.text = element_text(inherit.blank = T),
                  strip.text.x = element_text(margin=margin(2,0,2,0),inherit.blank = T),
                  strip.text.y = element_text(margin=margin(0,2,0,2),angle=-90,inherit.blank = T),
                  strip.switch.pad.grid = unit(1),
                  strip.switch.pad.wrap = unit(1),
                  plot.tag = element_text(inherit.blank = T),
                  plot.tag.position = "topleft",
                  axis.ticks.length.x.bottom = unit(1),
                  axis.ticks.length.x.top = unit(1),
                  axis.ticks.length.y.left = unit(1),
                  axis.ticks.length.y.right = unit(1),
                  axis.ticks.length.x = unit(1),
                  axis.ticks.length.y = unit(1),
                  aspect.ratio = 1,
                  complete = T,
                  validate = T))
}





#' @export
geom_lm <- function(...,formula=y~x,method=lm,se=F){ggplot2::geom_smooth(...,formula=formula,method=method,se=se)}
#' @export
ggsave <- function(
    filename, plot = ggplot2::last_plot(), device = NULL,
    path = NULL, scale = 1, width = NA, height = NA,
    units = c("mm"), dpi = 600, limitsize = TRUE, border=50, ...){

    if(is.na(width)){width = 2*height}
    if(is.na(height)){height = 2*width}

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
    system(paste0("magick convert \"",filename,"\" -trim -bordercolor white -border ",border," \"",filename,"\""))
  }
