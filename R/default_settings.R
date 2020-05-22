#' Define a Windows font, set scipen options, and define JAH's default ggplot theme.
#'
#' @export
jah_settings <- function(){
  windowsFonts(Times=windowsFont(family="Times New Roman"))
  options(scipen=999)


  base_font_size = 8 # Default Font size in points.
  base_font_family = "Times" # Default font family.

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

#' Change the default unit to mm
#' @export
unit <- function(...,units="mm"){grid::unit(units=units,...)}

#' Change the default unit to mm
#' @export
margin <- function(...,unit="mm"){ggplot2::margin(unit=unit,...)}


