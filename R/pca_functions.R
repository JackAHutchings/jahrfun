#' Extract PCA data and grouping for further analysis or publication-quality plots.
#' @export
pca_data <- function(data,cols,groups,scale=T,var_scaling=5){
  #Select data to be used in PCA
  if(is.numeric(cols)){index = cols}
  if(is.character(cols) & length(cols)==2){index = match(cols[1],names(data)) : match(cols[2],names(data))}
  if(is.character(cols) & length(cols)>2){index = match(cols,names(data))}

  aes_index <- match(groups,names(data))
  plot_data <- na.omit(data[,c(aes_index,index)])
  #Generate PCA with some creative subsetting of plot_data
  pca <- prcomp(plot_data[,(length(aes_index)+1):length(plot_data)],scale.=scale)
  #Percent variance explained by PCs
  pervar <- round(pca$sdev^2 / sum(pca$sdev^2) * 100,2)
  #Plot values and limits
  require(dplyr)
  vars <- as.data.frame(pca$rotation[,1:2]) %>%
    rownames_to_column() %>%
    gather(pc,value,PC1,PC2) %>%
    full_join(data.frame(pc = c("PC1","PC2"),sdev = pca$sdev[1:2]) %>% mutate(pc = as.character(pc)),by="pc") %>%
    mutate(loading = value * sdev * var_scaling) %>%
    select(rowname,pc,loading) %>%
    spread(pc,loading)
  samples <- pca$x[,1:2]

  #sample grouping
  sample_groups = plot_data[,match(groups,names(plot_data))]
  sample_pc1_pc2 = cbind.data.frame(sample_groups,samples)

  output <- list(pervar=pervar,vars=vars,samples=sample_pc1_pc2)
  output
}

#' 'Clean' looking PCA plotting function.
#' @export

pca_plot <- function(data,cols,color=NA,shape=NA,label=NA,scale=T,var_scaling=5,text_size=8,legend_position="top",font_family="Times",
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
