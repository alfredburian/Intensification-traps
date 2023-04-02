library(lattice);library(colorspace);library(RColorBrewer); library(latticeExtra); library(ggplot2);library(ggpubr); library(eulerr); library(gridExtra); library("inlmisc")
source("function_complex_model_20200902.R"); source("function_create_model_constants_casestudies.R");
source("function_plot_relationships.R"); source("function_plot_model_output.R")

# create colour scheme
cbPalette6 <- c( "#D0B541",  "#7EB875", "#57A2AC", "#4E78C4",  "#CE2220","#E67F33"); col<-brewer.pal(11,"RdYlGn")

# 1. Step: load in file with model constants and create them in R
constants.specific<-read.csv('constants_input_casestudy.csv',header=T, dec=".",sep=",",na.strings="NA")

create.casestudy.constants()

# 2. define model grid size
E <- seq(0,1, by= 0.01); WL<- seq(0,1, by= 0.01) #the smaller the number after "by", the higher the grid resolution

# 3. Step: Plot all relationships in the conceptual relationship (A-E) in conceptual Figure 1

plot.input.relationships() # press on zoom to see output graphs properly! 1000*800

# 4. The command for the actual model
output<-complex.model() # if you get a warning message, just run again!!

#4. Plot output of the model (3 sets of plots) (1000*300)
plot.model.results() # Three level plots - need time to load; press on zoom to see output graphs properly!
# trade-off plot between biodiversity and total production

plot.tradeoff # trade-off plot between biodiversity and total production: this plot depicts the outcome of all possible scenarios
# (outcome = B and P); the colour of the surface indicates the effort (dark blue) is low effort, light blue is high effort. The red 
# line represents optimal scenarios (maximise B at a certain P value - represents the opportunity costs between P and B) (490*450)
plot.venn # biodiversity in WL and in non-WL at the optimum production scenario
# If the error below occurs, the label in the function (plot model outputs need to be turned off)
# Error in grid.Call(C_convert, x, as.integer(whatfrom), as.integer(whatto),  : 
#                      Viewport has zero dimension(s)
