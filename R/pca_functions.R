#' Extract PCA data and grouping for further analysis or publication-quality plots.
#'
#' @param data Input dataset.
#' @param cols Selection of columns from input dataset to perform the PCA on. All data within these columns be
#' numerical. Any rows with missing values will be dropped. Accepts the following input types:
#' \itemize{
#' \item A numerical vector specifying the columns to use (i.e., c(2,3,8) uses columns 2, 3 and 8).
#' \item A character vector specifyiny the names of the columns to use.
#' \item A character vector of length 2. This will select the two named columns and all columns between them.
#' }
#' @param samples Optional column to provide unique sample identifier. Otherwise, rownames are used.
#' @param scale Boolean. If TRUE, then correlation PCA. if FALSE, then covariance PCA.
#' @param var_scaling Multiplier for raw variable loadings. Helps scale them to similar values as sample loadings most of the time.
#' 
#' @return This returns a named list of data frames:
#' \itemize{
#' \item \strong{pervar} contains the percent variance explained by each principal component.
#' \item \strong{vars} contains the variable loadings on each principal component. These may have been scaled by var_scaling for easier plotting.
#' \item \strong{samples} contains the sample loadings on each principal component.
#' }
#'
#' @export
pca_data <- function(data,cols,samples="rowname",scale=T,var_scaling=5){

  if (is.numeric(cols)) {index = cols  }
  if (is.character(cols) & length(cols) == 2) {index = match(cols[1], names(data)):match(cols[2], names(data))}
  if (is.character(cols) & length(cols) > 2) {index = match(cols, names(data))}
  if (samples == "rowname") {data <- data %>% mutate(rowname = rownames(.))}
  
  aes_index <- match(samples, names(data))
  plot_data <- na.omit(data[, c(index)])
  pca <- prcomp(plot_data[, (length(aes_index) + 1):length(plot_data)], 
                scale. = scale)
  pervar <- data.frame(percent_variance_explained = round(pca$sdev^2/sum(pca$sdev^2) * 100, 2)) %>% 
    mutate(PC = paste0("PC",row.names(.))) %>% 
    select(PC,percent_variance_explained)
  vars <- as.data.frame(pca$rotation) %>% rownames_to_column() %>% 
    gather(pc,loading,-rowname) %>% 
    full_join(data.frame(sdev = pca$sdev) %>% mutate(pc = paste0("PC",row.names(.))),by="pc") %>% 
    rowwise() %>% 
    mutate(loading = loading*sdev*var_scaling) %>% 
    select(-sdev) %>% 
    spread(pc, loading) %>% 
    rename(variable = rowname)
  sample_ids = plot_data[, match(samples, names(plot_data))]
  sample_data = cbind.data.frame(sample_ids, pca$x)
  output <- list(pervar = pervar, vars = vars, samples = sample_data)
  output
}

#' 'Clean' looking PCA plotting function using ggplot2
#' @param data Input dataset.
#' @param cols Selection of columns from input dataset to perform the PCA on. These must all be
#' @param color Optional. Select a column for color aesthetic mapping.
#' @param shape Optional. Select a column for shape aesthetic mapping.
#' @param label Optional. Select a column for sample label aesthetic mapping.
#' @param scale Boolean. If TRUE, then correlation PCA. if FALSE, then covariance PCA.
#' @param var_scaling Multiplier for raw variable loadings. Helps scale them to similar values as sample loadings most of the time.
#' @param text_size Font size of all printed labels in points.
#' @param legend_position Legend position. Accepts the same input as legend.position in ggplot's theme function
#' @param font_family Font family to use for all labels.
#' @param axis_alpha Alpha of the plotting x and y axes.
#' @param geom_type Either "text" or "label" to use either geom_text or geom_label, respectively.
#' @param point_size Point size in mm of plotted points.
#' @param repel_variables Boolean. Should plotted variables be repelled?
#' @param repel_samples Boolean. Should plotted samples be repelled?
#' @param point_outline Boolean. Should sample points have a black border?

#' @export

pca_plot <- function(data,cols,color=NA,shape=NA,label=NA,scale=T,var_scaling=5,text_size=8,legend_position="top",font_family="serif",
                     axis_alpha = 0.5, geom_type = "text",point_size = 4,repel_variables=F,repel_samples=F,point_outline=F){
  #Select data to be used in PCA
  if(is.numeric(cols)){index = cols}
  if(is.character(cols) & length(cols)==2){index = match(cols[1],names(data)) : match(cols[2],names(data))}
  if(is.character(cols) & length(cols)>2){index = match(cols,names(data))}

  aes_index <- match(na.omit(c(color,shape,label)),names(data))
  plot_data <- na.omit(data[,c(aes_index,index)])
  #Generate PCA with some creative subsetting of plot_data
  pca <- prcomp(plot_data[,(length(aes_index)+1):length(plot_data)],scale.=scale)
  #Percent variance explained by PCs
  pervar <- round(pca$sdev^2 / sum(pca$sdev^2) * 100,2)
  #Plot values and limits
  vars <- as.data.frame(pca$rotation[,1:2]) %>%
    rownames_to_column() %>%
    gather(pc,value,PC1,PC2) %>%
    full_join(data.frame(pc = c("PC1","PC2"),sdev = pca$sdev[1:2]) %>% mutate(pc = as.character(pc)),by="pc") %>%
    mutate(loading = value * sdev * var_scaling) %>%
    select(rowname,pc,loading) %>%
    spread(pc,loading)

  samples <- as.data.frame(pca$x[,1:2])
  #coloring
  if(!is.na(color)){col_group = as.factor(as.data.frame(plot_data[,match(color,names(plot_data))])[,1])
  samples = cbind(samples,col_group)}
  #shaping
  if(!is.na(shape)){sha_group = as.factor(as.data.frame(plot_data[,match(shape,names(plot_data))])[,1])
  samples = cbind(samples,sha_group)}
  #labeling
  if(!is.na(label)){lab_group = as.factor(as.data.frame(plot_data[,match(label,names(plot_data))])[,1])
  samples = cbind(samples,lab_group)}

  ggplot(samples,aes(PC1,PC2)) +
    geom_hline(yintercept=0,alpha=axis_alpha) +
    geom_vline(xintercept=0,alpha=axis_alpha) +

    {if(!is.na(shape) & !is.na(color)) {geom_point(aes(shape = sha_group, color = col_group),size=point_size)}} +
    {if(!is.na(shape) &  is.na(color)) {geom_point(aes(shape = sha_group),size=point_size)}} +
    {if( is.na(shape) & !is.na(color)) {geom_point(aes(color = col_group),size=point_size)}} +
    {if( is.na(shape) &  is.na(color)) {geom_point(size=point_size)}} +

    {if(!is.na(label) & repel_samples == F){geom_text(data = samples,aes(label = lab_group),size = text_size*0.352777778,family=font_family)}} +
    {if(!is.na(label) & repel_samples == T){geom_text_repel(data = samples,aes(label = lab_group),size = text_size*0.352777778,family=font_family)}} +

    {if(point_outline == T & !is.na(shape)){go <- geom_point(data=samples,aes(PC1,PC2),size=point_size,color="black",shape=21)}} +
    {if(point_outline == T & is.na(shape)){go <- geom_point(data=samples,aes(PC1,PC2,shape=sha_group),size=point_size,color="black")}} +

    {if( repel_variables == T & geom_type == "text") {geom_text_repel(data=vars,aes(PC1,PC2,label=rowname),family=font_family,size=text_size*0.352777778)}} +
    {if( repel_variables == T & geom_type == "label"){geom_label_repel(data=vars,aes(PC1,PC2,label=rowname),family=font_family,size=text_size*0.352777778)}} +
    {if( repel_variables == F & geom_type == "text") {geom_text(data=vars,aes(PC1,PC2,label=rowname),family=font_family,size=text_size*0.352777778)}} +
    {if( repel_variables == F & geom_type == "label"){geom_label(data=vars,aes(PC1,PC2,label=rowname),family=font_family,size=text_size*0.352777778)}} +

    theme(legend.title = element_blank(),
          legend.position = legend_position,
          text = element_text(size=text_size),
          legend.text = element_text(size=text_size)) +
    labs(x = paste("PC1 [",pervar[1],"% ]"),
         y = paste("PC2 [",pervar[2],"% ]"))
}
