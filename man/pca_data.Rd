% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca_functions.R
\name{pca_data}
\alias{pca_data}
\title{Extract PCA data and grouping for further analysis or publication-quality plots.}
\usage{
pca_data(data, vars, samples = "rowname", scale = T, var_scaling = 5)
}
\arguments{
\item{data}{Input dataset.}

\item{vars}{Selection of columns from input dataset to perform the PCA on. All data within these columns be
numerical. Any rows with missing values will be dropped. Accepts the following input types:
\itemize{
\item A numerical vector specifying the columns to use (i.e., c(2,3,8) uses columns 2, 3 and 8).
\item A character vector specifyiny the names of the columns to use.
\item A character vector of length 2. This will select the two named columns and all columns between them.
}}

\item{samples}{Optional column to provide unique sample identifier. Otherwise, rownames are used.}

\item{scale}{Boolean. If TRUE, then correlation PCA. if FALSE, then covariance PCA.}

\item{var_scaling}{Multiplier for raw variable loadings. Helps scale them to similar values as sample loadings most of the time. Set to FALSE for raw output.}
}
\value{
This returns a named list of data frames:
\itemize{
\item \strong{pervar} contains the percent variance explained by each principal component.
\item \strong{vars} contains the variable loadings on each principal component. These may have been scaled by var_scaling for easier plotting.
\item \strong{samples} contains the sample loadings on each principal component.
}
}
\description{
Extract PCA data and grouping for further analysis or publication-quality plots.
}
