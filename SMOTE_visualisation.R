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

#fun_plot(show_edges = F, show_synth = F) + ggtitle('A typical machine learning problem: class imbalance')
#ggsave('SMOTE_R_visualisation_1.png',width = 12.9, height = 5.42, scale = 0.9, dpi = 1000)

#fun_plot(show_edges = T, show_synth = F) + ggtitle('Addressing class imbalance problems of ML via SMOTE: connecting the dots')
#ggsave('SMOTE_R_visualisation_2.png',width = 12.9, height = 5.42, scale = 0.9, dpi = 1000)

# fun_plot(show_edges = T, show_synth = T) + ggtitle('Addressing class imbalance problems of ML via SMOTE: synthesising new dots between existing dots')
# ggsave('SMOTE_R_visualisation_3.png',width = 12.9, height = 5.42, scale = 0.9, dpi = 1000)

fun_plot(show_edges = T, show_synth = F) + 
  geom_point(data = data.frame(Sepal.Length = 4.9, Sepal.Width = 3.0), aes(x = Sepal.Length, y = Sepal.Width),
             shape = 21, color = 'darkred', 
             size = 110, stroke = 2, alpha = 0.5) +
  ggtitle('Addressing class imbalance problems of ML via SMOTE: instance neighbourhoods and the K parameter')
ggsave('SMOTE_R_visualisation_4.png',width = 12.9, height = 5.42, scale = 0.9, dpi = 1000)

fun_plot(show_edges = T, show_synth = F) + 
  geom_point(data = data.frame(Sepal.Length = 4.9, Sepal.Width = 3.0), aes(x = Sepal.Length, y = Sepal.Width),
             shape = 21, color = 'darkred', 
             size = 110, stroke = 2, alpha = 0.5) +
  geom_point(data = data.frame(Sepal.Length = 4.9, Sepal.Width = 3.0), aes(x = Sepal.Length, y = Sepal.Width),
             shape = 21, color = 'darkred', 
             size = 145, stroke = 2, alpha = 0.5) +
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

# dat_plot = SMOTE(dat_plot[,1:2],
#                  as.numeric(dat_plot[,3]),
#                  K = 3, dup_size = 0)

#dup_size regulates the re-use of data points from which to draw synthetic cases
#careful, 0 is a special case which indicates duplication until (nearly) balanced cases
#K regulates which points the algorithm draws to, just the nearest one? (K = 1) or more?

# > SMOTE
# function (X, target, K = 5, dup_size = 0) 
# {
#   ncD = ncol(X)#check how many dimensions there are to the data
#   n_target = table(target)#check how many exemplars of each class there are
#   classP = names(which.min(n_target))#check the name of the minority class
#   P_set = subset(X, target == names(which.min(n_target)))[sample(min(n_target)),]#the minority data, in shuffled order
#   N_set = subset(X, target != names(which.min(n_target)))#the majority data
#   P_class = rep(names(which.min(n_target)), nrow(P_set))#class labels for exemplars in minority
#   N_class = target[target != names(which.min(n_target))]#class labels for exemplars in majority
#   sizeP = nrow(P_set)#the minority class sample size
#   sizeN = nrow(N_set)#the majority class sample size
#   knear = knearest(P_set, P_set, K)#smotefamily function
# matrix specifying who is the nearest neighbour of whom, second nearest neighbour, etc
# rows are minority class exemplars, columns are nearest neighbours in descending order, number is the index of the neighbour
#   sum_dup = n_dup_max(sizeP + sizeN, sizeP, sizeN, dup_size)#smotefamily function
# this function simply spits out how many times a data point will get resused to draw a new synthetic sample
# if you enter a dup_size unlike zero it just returns that number
# if you enter a dup_size of zero it uses a clever little formula:
# floor((2 * size_N - size_input)/size_P)
# this formula determines the ideal sample suze of the balanced sample (2*size_N),
# how many samples we are away from it (2*size_N - size_input)
# and divides the number of samples we are away from the ideal, i.e. the number of synthetic samples, over the number of minority samples we have (/P_set)
# using the floor() function ensures that the minority class size never exceeds the majority class size, at best they match
#   syn_dat = NULL#initialise the variable holding the synthetic data, no idea why this was not properly pre-allocated
#   for (i in 1:sizeP) {#for each each minority exemplar
#     if (is.matrix(knear)) {if K > 1, i.e. if each minority sample has K nearest neighbours
#       pair_idx = knear[i, ceiling(runif(sum_dup) * K)]#recall that knear holds the indices of K nearest neighbours, let's randomly sample from that with replacement 
#     }
#     else {#if each minority sample only has one closest neighbour
#       pair_idx = rep(knear[i], sum_dup)#get the nearest neighbour's index sum_dup number of times
#     }
#     g = runif(sum_dup)#draw a random number between 0 and 1 sum_dup number of times, i.e. one number for each synthetic exemplar a minority data point will 'produce'
#     P_i = matrix(unlist(P_set[i, ]), sum_dup, ncD, byrow = TRUE)#repeat the data of this minority sample a sum_dup number of times, i.e. once for eahc to be synthesised data point
#     Q_i = as.matrix(P_set[pair_idx, ])#get the data of the neighbours of this minority sample, recall that neighbours are randomly chosen via pair_idx
#     syn_i = P_i + g * (Q_i - P_i)#create sum_dup samples between P_i, i.e. the coordinate of the real sample, and Q_i, i.e. the neighbours
# each row will be one new synthetic sample
# Q_i - P_i is the distance between the minority sample and its neighbours on each dimension
# this distance gets multiplied by g, a random number between 0 and 1, and added to the coordinate of the minority sample
# thereby the new point is somewhere (defined by g) along the line between the minority sample and a neighbour
#     syn_dat = rbind(syn_dat, syn_i)#add the synthetic samples to the variable holding the totally of synthetic samples
#   }
#   P_set[, ncD + 1] = P_class
#   colnames(P_set) = c(colnames(X), "class")
#   N_set[, ncD + 1] = N_class
#   colnames(N_set) = c(colnames(X), "class")
#   rownames(syn_dat) = NULL
#   syn_dat = data.frame(syn_dat)
#   syn_dat[, ncD + 1] = rep(names(which.min(n_target)), nrow(syn_dat))
#   colnames(syn_dat) = c(colnames(X), "class")
#   NewD = rbind(P_set, syn_dat, N_set)
#   rownames(NewD) = NULL
#   D_result = list(data = NewD, syn_data = syn_dat, orig_N = N_set, 
#                   orig_P = P_set, K = K, K_all = NULL, dup_size = sum_dup, 
#                   outcast = NULL, eps = NULL, method = "SMOTE")
#   class(D_result) = "gen_data"
#   return(D_result)
# }