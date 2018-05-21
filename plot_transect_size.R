libs <- c("sf", "tidyverse")
lapply(libs, require, character.only = TRUE)

# create circle to represent a plot
point <- st_point(x=c(0, 0), dim="XYZ") %>%
  st_sfc(crs=3310) 

plot_radius <- 16  #1/10 acre: 11.3m   1/5 acre: 16

circle <- st_buffer(point, dist=plot_radius)  #only estimate number of trees for the real plot size

#create transect rectangle 
transect <- st_polygon(list(cbind(c(-1,1,1,-1, -1),c(plot_radius, plot_radius, -plot_radius, -plot_radius, plot_radius)))) %>%
  st_sfc(crs=3310) 


#function to test fraction of plot trees that fall within transect
transect_vs_census <- function(num_trees) {
  
  trees <- st_sample(circle, num_trees)
  
  trees_in_transect <- st_intersection(trees, transect)
  count_trees_in_transect <- length(trees_in_transect)
  
  fraction_sampled <- count_trees_in_transect/num_trees  
  
  results <- data.frame("num_trees"=num_trees, "transect_trees" = count_trees_in_transect, "fraction_sampled" = fraction_sampled)
  
  return(results)
  
}

#run function for various tree amounts, 500x each

num_trees_options <- seq(5, 100, 5) %>%
  rep(each=500)

results_multirun <- lapply(num_trees_options, transect_vs_census) %>%
  bind_rows()

 
#plot results
true_fraction=(plot_radius*2*2)/st_area(circle) %>%
  as.numeric()

results_multirun$num_trees <- as.factor(results_multirun$num_trees)

ggplot(results_multirun, aes(x=num_trees, y=fraction_sampled)) + 
  geom_boxplot() + 
  geom_hline(yintercept=true_fraction, colour="red") 
  


