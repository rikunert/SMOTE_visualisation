#This script will try to explain the SMOTE function of the R package smotefamily

# Richard Kunert November 2017 (rikunert@gmail.com)

##########################################################################
# LOAD LIBRARIES

if(!require(ggplot2)){install.packages('ggplot2')} #plotting
library(ggplot2)

if(!require(ggthemes)){install.packages('ggthemes')} #plotting
library(ggthemes)

if(!require(ggforce)){install.packages('ggforce')} #plotting
library(ggforce)

if(!require(smotefamily)){install.packages('smotefamily')} #Synthetic Minority Oversampling TEchnique
if(!require(FNN)){install.packages('FNN')} #if not installed we get an error
library(smotefamily)

##########################################################################
# CUSTOM FUNCTIONS

fun_plot = function(dat_plot_majority = dat_plot$orig_N,
                    dat_plot_minority = dat_plot$orig_P,
                    dat_plot_synthetic = dat_plot$syn_data,
                    show_edges = T,
                    show_synth = T,
                    point_size = 5){
  
  #first get the plot style going
  dat_plot_all = rbind(dat_plot_majority, dat_plot_minority, dat_plot_synthetic)
  A = ggplot(data = dat_plot_all,
             aes(x = Sepal.Length, y = Sepal.Width)) +
    theme_tufte() +
    geom_rangeframe() +
    scale_x_continuous(breaks = extended_range_breaks()(dat_plot_all$Sepal.Length)) +
    scale_y_continuous(breaks = extended_range_breaks()(dat_plot_all$Sepal.Width)) +
    labs(caption = '@rikunert', x = 'Sepal length', y = 'Sepal width') +
    theme(plot.caption = element_text(size = 10, color = 'grey', face= 'italic'))
    
  
  if(show_edges){
    for(i in 1:nrow(dat_plot_minority)){
      for(j in 1:nrow(dat_plot_minority)){
        if(i != j){
          A = A + geom_segment(data = data.frame(x = dat_plot_minority$Sepal.Length[i],
                                                 y = dat_plot_minority$Sepal.Width[i],
                                                 xend = dat_plot_minority$Sepal.Length[j],
                                                 yend = dat_plot_minority$Sepal.Width[j]),
                               aes(x = x, y = y, xend = xend, yend = yend), size = 1.5)
        }}}}
  
  A = A +
    geom_point(data = dat_plot_majority[,1:2], aes(x = Sepal.Length, y = Sepal.Width),
               shape = 21, fill = 'grey', color = 'darkgreen', 
               size = point_size, stroke = point_size/1.5) +
    geom_point(data = dat_plot_minority[,1:2], aes(x = Sepal.Length, y = Sepal.Width),
               shape = 21, fill = 'grey', color = 'darkred',
               size = point_size, stroke = point_size/1.5) +
    annotate("text", x = 6, y = 2.5, label='Majority class', size = 5, color = 'darkgreen') +
    annotate("text", x = 4.8, y = 4.1, label='Minority class', size = 5, color = 'darkred')
  
  if(show_synth){
    A = A +
      geom_point(data = dat_plot_synthetic[,1:2], aes(x = Sepal.Length, y = Sepal.Width),
                 shape = 21, fill = 'grey', color = 'red',
                 size = point_size/2, stroke = point_size/2) +
      annotate("rect", xmin = 4.6, xmax = 5, ymin = 3.23, ymax = 3.35, fill = 'white', colour = 'white') +
      annotate("text", x = 4.8, y = 3.3, label='Synthetic minority instances', size = 3, color = 'red')
      
  }
  
  A
}

dat = iris[c(2, 9, 16, 23, 51:63),
           c(1, 2, 5)]

dat_plot = SMOTE(dat[,1:2],
                 as.numeric(dat[,3]),
                 K = 3, dup_size = 0)

fun_plot(show_edges = F, show_synth = T) + ggtitle('A typical machine learning problem (class imbalance) solved through SMOTE')
ggsave('SMOTE_R_visualisation_0.png',width = 12.9, height = 5.42, scale = 0.9, dpi = 1000)

fun_plot(show_edges = F, show_synth = F) + ggtitle('A typical machine learning problem: class imbalance')
ggsave('SMOTE_R_visualisation_1.png',width = 12.9, height = 5.42, scale = 0.9, dpi = 1000)

fun_plot(show_edges = T, show_synth = F) + ggtitle('Addressing class imbalance problems of ML via SMOTE: connecting the dots')
ggsave('SMOTE_R_visualisation_2.png',width = 12.9, height = 5.42, scale = 0.9, dpi = 1000)

fun_plot(show_edges = T, show_synth = T) + ggtitle('Addressing class imbalance problems of ML via SMOTE: synthesising new dots between existing dots')
ggsave('SMOTE_R_visualisation_3.png',width = 12.9, height = 5.42, scale = 0.9, dpi = 1000)

fun_plot(show_edges = F, show_synth = F) + 
  geom_segment(data = data.frame(x = 4.9, y = 3, xend = 4.6, yend = 3.6),
               aes(x = x, y = y, xend = xend, yend = yend), size = 1.5, alpha = 0.5) +
  geom_point(data = data.frame(Sepal.Length = 4.9, Sepal.Width = 3.0), aes(x = Sepal.Length, y = Sepal.Width),
             shape = 21, color = 'darkred', 
             size = 97, stroke = 2, alpha = 0.5) +
  ggtitle('Addressing class imbalance problems of ML via SMOTE: instance neighbourhoods and the K parameter')
ggsave('SMOTE_R_visualisation_4.png',width = 12.9, height = 5.42, scale = 0.9, dpi = 1000)

fun_plot(show_edges = F, show_synth = F) + 
  geom_segment(data = data.frame(x = 4.9, y = 3, xend = 4.6, yend = 3.6),
               aes(x = x, y = y, xend = xend, yend = yend), size = 1.5, alpha = 0.5) +
  geom_segment(data = data.frame(x = 4.9, y = 3, xend = 4.4, yend = 2.9),
               aes(x = x, y = y, xend = xend, yend = yend), size = 1.5, alpha = 0.5) +
  geom_point(data = data.frame(Sepal.Length = 4.9, Sepal.Width = 3.0), aes(x = Sepal.Length, y = Sepal.Width),
             shape = 21, color = 'darkred', 
             size = 97, stroke = 2, alpha = 0.5) +
  geom_point(data = data.frame(Sepal.Length = 4.9, Sepal.Width = 3.0), aes(x = Sepal.Length, y = Sepal.Width),
             shape = 21, color = 'darkred', 
             size = 130, stroke = 2, alpha = 0.5) +
    ggtitle('Addressing class imbalance problems of ML via SMOTE: instance neighbourhoods and the K parameter')
ggsave('SMOTE_R_visualisation_5.png',width = 12.9, height = 5.42, scale = 0.9, dpi = 1000)

dat_plot = SMOTE(dat[,1:2],
                 as.numeric(dat[,3]),
                 K = 3, dup_size = 200)

fun_plot(show_edges = T, show_synth = T) + 
  ggtitle('Addressing class imbalance problems of ML via SMOTE (dup_size = 200)')
ggsave('SMOTE_R_visualisation_6.png',width = 12.9, height = 5.42, scale = 0.9, dpi = 1000)

dat_plot = SMOTE(dat[,1:2],
                 as.numeric(dat[,3]),
                 K = 3, dup_size = 200)

fun_plot(show_edges = T, show_synth = T) + 
  ggtitle('Addressing class imbalance problems of ML via SMOTE (dup_size = 200)')
ggsave('SMOTE_R_visualisation_6.png',width = 12.9, height = 5.42, scale = 0.9, dpi = 1000)