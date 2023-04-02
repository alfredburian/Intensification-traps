library(lattice);library(colorspace);library(RColorBrewer); library(latticeExtra); library(ggplot2); library("ellipse")

# load in required function
source("function_complex_model_20200902.R")
source("function_create_model_constants.R")
source("function_output_evaluation.R")

# create colour scheme
cbPalette6 <- c( "#D0B541",  "#7EB875", "#57A2AC", "#4E78C4",  "#CE2220","#E67F33")
col<- rev(c("#7B3014", "#D04A07","#F98C40", rgb(255,242,204,maxColorValue =255), rgb(90,165,205,maxColorValue =255), rgb(35,108,167,maxColorValue =255), rgb(38,69,110,maxColorValue =255)))

#### A) Create artificial landscapes
# 1) create required vectors for artificial landscape creation
#define grid size for input variables (Intensification effort is E and Working land is WL)
E <- seq(0,1, by= 0.01); WL<- seq(0,1, by= 0.01)

n<- 10000 #number of runs in the sensitivity analysis (implemented in a loop)
constants.meta<-data.frame(); opp.cost.meta<-list(); constants.meta<-data.frame(); eval.output.meta<-c()

# 2) randomly create model constant based on attributed mean and sd
constants.mean<-read.csv('constants_input.csv',header=T, dec=".",sep=",",na.strings="NA")

counter<-0 # counter for the loop of the creation process

#here starts the loop for the sensitivity analysis
repeat{ 
# implement function that creates the model constants
create.model.constants()

# 3) define habitat preferences (will vary for each run)
# this is based on the assumption that the max WL.cont depends on the NWL.cont (see SI for explanation)
NWL.cont<-sample(seq(30,90,by=1),1); WL.cont<-sample(seq(10, round((100-NWL.cont)),by=1),1) 
habitat.pref.prop<-c(WL.cont,NWL.cont,100-WL.cont-NWL.cont) #proportions of species living in WL, NWL and in both
# habitat types
habitat.preferences<- c(rep("WL",habitat.pref.prop[1]),rep("NWL",habitat.pref.prop[2]),rep("both",habitat.pref.prop[3])) 

# 4) run the model function
output<-complex.model() #reminder - last object defined in the function code will be saved here. 

# 5) extract the meta-data from the output for the sensitivity analysis
evaluation()

# 6) save evaluation output
constants.meta<-rbind(constants.meta, model.constants)  # all constants from all simulations
opp.cost.meta<-append(opp.cost.meta, list(opport.costs[,1:2] ))
eval.output.meta<- rbind(eval.output.meta, eval.output)

counter<-counter+1
if(counter==n){break}
} # end of the loop

# name output of the loop
colnames(constants.meta)<-c("neg.spill","TE.sd","TE.mean", "Ymax","Cost.E","Cost.baseline","min.Cost.WL", "slope.Cost.WL","min.Y.E", "slope.Y.E", "min.Y.B", "slope.Y.B","mean.WL.Y", "sd.WL.Y", "habitat.WL", "habitat.NWL", "habitat.both", "min.Bio.E", "slope.Bio.E")
eval.output.meta<-as.data.frame(eval.output.meta)
colnames(eval.output.meta)<-c("biodiv.at.max.production", "effort.at.max.production","WL.at.max.production", "edge.distance","prod.at.max.man.int", "biodiv.at.max.man.int", "risk", "intensity")

eval.output.meta$intensity [eval.output.meta$intensity == -Inf]<- 0