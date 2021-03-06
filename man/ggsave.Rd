% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot_functions.R
\name{ggsave}
\alias{ggsave}
\title{Editted ggsave2 from cowplot function.}
\usage{
ggsave(
  filename,
  plot = ggplot2::last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("mm"),
  dpi = 600,
  limitsize = TRUE,
  border = 50,
  ...
)
}
\description{
Edit of ggplot save (actually the cowplot implementation ggsave2) that includes some sensible defaults, defaults units to mm,
and uses ImageMagick to trim whitespace from an exported png.
}
