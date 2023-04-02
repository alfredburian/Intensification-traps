library(lattice);library(colorspace);library(RColorBrewer); library(latticeExtra); library(ggplot2); library("ellipse")
# load in functions
source("function_complex_model_20200902.R")
source("function_create_model_constants.R")
source("function_output_evaluation.R")

# create colour scheme
cbPalette6 <- c( "#D0B541",  "#7EB875", "#57A2AC", "#4E78C4",  "#CE2220","#E67F33")
col<- rev(c("#7B3014", "#D04A07","#F98C40", rgb(255,242,204,maxColorValue =255), rgb(90,165,205,maxColorValue =255), rgb(35,108,167,maxColorValue =255),rgb(38,69,110,maxColorValue =255)))

#### I) Systematic sensitivity analysis - change the parameters of equation A-D ####
sens.ranges<-read.csv('sensitivity_ranges.csv',header=T, dec=".",sep=",",na.strings="NA")
source("function_create_model_constants_casestudies.R")

constants.meta<-data.frame(); opp.cost.meta<-list(); constants.meta<-data.frame(); eval.output.meta<-c()
resolution<-30 # how many scenarios are computer between upper and lower range of the constant that is systematically changed
for (i in 1:4){ # first create the grid for model constants that are changed systematically
  relationship<-which(sens.ranges$relationship==unique(sens.ranges$relationship)[i])
  a.range<-seq(sens.ranges$range.lower[relationship[1]] ,sens.ranges$range.upper[relationship[1]], length = resolution)
  b.range<-seq(sens.ranges$range.lower[relationship[2]] ,sens.ranges$range.upper[relationship[2]], length = resolution)
  grid<-c()
  for(g in 1:length(a.range)){a<-rep(a.range[g],length(a.range)); gird.piece<-data.frame(a = a, b = b.range)
  grid<-rbind(grid,gird.piece)}
  # next step is to overwrite the respective input variables, run the model and save the output
  
  constants.specific<-read.csv('constants_input_means.csv',header=T, dec=".",sep=",",na.strings="NA")
  create.casestudy.constants() # make sure the case-study file has the mean values of literature search.
  for(g in 1:nrow(grid)){ 
    assign (as.character(sens.ranges$parameter[relationship[1]]),grid$a[g])
    assign (as.character(sens.ranges$parameter[relationship[2]]),grid$b[g])
    scale.Bio.E<- 1-min.Bio.E; scale.Y.E<- 1-min.Y.E; scale.Y.B<<- 1-min.Y.B
    
    # A) define habitat preferences
    habitat.pref.prop<-c(30,60,10) #proportions of species in WL - lives in WL, NWL - lives in non WL, both - lives in both
    habitat.preferences<- c(rep("WL",habitat.pref.prop[1]),rep("NWL",habitat.pref.prop[2]),rep("both",habitat.pref.prop[3])) 
    E <- seq(0,1, by= 0.01); WL<- seq(0,1, by= 0.01)
    
    # B) run the model function
    output<-complex.model()
    
    # C) extract the meta-data from the output for the sensitivity analysis
    evaluation()
    
    # D) get the non-linearity (the concave nature) of trade-off curves
    slope<-(max(opport.costs$P)-min(opport.costs$P))/ (max(opport.costs$B)-min(opport.costs$B))
    intercept<- max(opport.costs$P) + slope * min(opport.costs$B); prediction<- intercept + slope * opport.costs$B * (-1)
    eval.output<-c(eval.output,mean(opport.costs$P-prediction))
    
    # E) store and name everything
    constants.meta<-rbind(constants.meta, model.constants)  # all constants from all simulations
    opp.cost.meta<-append(opp.cost.meta, list(opport.costs[,1:2] ))
    eval.output.meta<- rbind(eval.output.meta, eval.output)
    colnames(constants.meta)<-c("neg.spill","TE.sd","TE.mean", "Ymax","Cost.E","Cost.baseline","min.Cost.WL", "slope.Cost.WL","min.Y.E", "slope.Y.E", "min.Y.B", "slope.Y.B", "mean.WL.Y", "sd.WL.Y", "habitat.WL", "habitat.NWL", "habitat.both", "min.Bio.E", "slope.Bio.E")
    eval.output.meta<-as.data.frame(eval.output.meta)
    colnames(eval.output.meta)<-c("biodiv.at.max.production", "effort.at.max.production","WL.at.max.production", "edge.distance", "convex.nature")}
  
  #F) Store everything under relationship-specific names (A-D)
  assign(paste0("rel.",sens.ranges$relationship[i*2],".const"),constants.meta)
  assign(paste0("rel.",sens.ranges$relationship[i*2],".opp.cost"),opp.cost.meta)
  assign(paste0("rel.",sens.ranges$relationship[i*2],".eval.output"),eval.output.meta)
  
  #empty the objects
  constants.meta<-data.frame(); opp.cost.meta<-list(); constants.meta<-data.frame(); eval.output.meta<-c()}

#F plot the model output
rel.A.eval.output<-cbind(rel.A.eval.output, rel.A.const[,c(13,14)]); rel.B.eval.output<-cbind(rel.B.eval.output, rel.B.const[,c(9,10)])
rel.C.eval.output<-cbind(rel.C.eval.output, rel.C.const[,c(11,12)]); rel.D.eval.output<-cbind(rel.D.eval.output, rel.D.const[,c(18,19)])

# calculate biodiversity at 90% production 
b90A<-c(); b90B<-c(); b90C<-c(); b90D<-c()
for (i in 1:length(rel.A.opp.cost)){
  b90A<-c(b90A, rel.A.opp.cost[[i]]$B[which(abs(0.9-rel.A.opp.cost[[i]]$P)==min(abs(0.9-rel.A.opp.cost[[i]]$P)))])
  b90B<-c(b90B, rel.B.opp.cost[[i]]$B[which(abs(0.9-rel.B.opp.cost[[i]]$P)==min(abs(0.9-rel.B.opp.cost[[i]]$P)))])
  b90C<-c(b90C, rel.C.opp.cost[[i]]$B[which(abs(0.9-rel.C.opp.cost[[i]]$P)==min(abs(0.9-rel.C.opp.cost[[i]]$P)))])
  b90D<-c(b90D, rel.D.opp.cost[[i]]$B[which(abs(0.9-rel.D.opp.cost[[i]]$P)==min(abs(0.9-rel.D.opp.cost[[i]]$P)))])  }

rel.A.eval.output$b90<-b90A ; rel.B.eval.output$b90<-b90B; rel.C.eval.output$b90<-b90C; rel.D.eval.output$b90<-b90D   

# biodiv 90 + edge distance 0.01
min(rel.A.eval.output$b90); min(rel.B.eval.output$b90); min(rel.C.eval.output$b90); min(rel.D.eval.output$b90)
max(rel.A.eval.output$b90); max(rel.B.eval.output$b90); max(rel.C.eval.output$b90); max(rel.D.eval.output$b90)

# colour definition
col<- rev(c("#7B3014", "#D04A07", rgb(255,242,204,maxColorValue =255), rgb(90,165,205,maxColorValue =255), rgb(35,108,167,maxColorValue =255), rgb(38,69,110,maxColorValue =255))) # we can use this one...
rbPal <- colorRampPalette(col)
n<- 30; col <- rev(rbPal(n))

# 420*360
my.at=seq(0.2, 1., length.out=n)
levelplot(b90 ~  mean.WL.Y * sd.WL.Y, data=rel.A.eval.output, main = "Relationship A", at=my.at,
          col.regions = col, cuts=n, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0))) +
  levelplot(edge.distance ~  mean.WL.Y * sd.WL.Y, data=rel.A.eval.output, main = "Relationship A", lsz = 2.1,
          col.regions = NA, cuts=1,  at = c(-5,0.01,5), contour=T,lty = 2, panel=panel.2dsmoother, args=list(span=0.15), scales = list(tck = c(0.8,0)))

levelplot(b90 ~  (1-min.Y.E) * slope.Y.E, data=rel.B.eval.output, main = "Relationship B", at=my.at,
          col.regions = col, cuts=n, panel=panel.2dsmoother, args=list(span=0.1), scales = list(tck = c(0.8,0)))+
  
  levelplot(edge.distance ~  (1-min.Y.E) * slope.Y.E, data=rel.B.eval.output, main = "Relationship B",
          col.regions = NA, cuts=1,  at = c(-5,0.01,5), contour=T,lty = 2, panel=panel.2dsmoother, args=list(span=0.15), scales = list(tck = c(0.8,0)))

levelplot(b90 ~  (1-min.Y.B) * slope.Y.B, data=rel.C.eval.output, main = "Relationship C", at=my.at,
          col.regions = col, cuts=n, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0)))+
  levelplot(edge.distance ~  (1-min.Y.B) * slope.Y.B, data=rel.C.eval.output, main = "Relationship C",
          col.regions = NA, cuts=1,  at = c(-5,0.01,5), contour=T,lty = 2, panel=panel.2dsmoother, args=list(span=0.15), scales = list(tck = c(0.8,0)))

levelplot(b90 ~  (1-min.Bio.E) * (slope.Bio.E*-1), data=rel.D.eval.output, main = "Relationship D", at=my.at,
          col.regions = col, cuts=n, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0)))+
  levelplot(edge.distance ~  (1-min.Bio.E) * (slope.Bio.E*-1), data=rel.D.eval.output, main = "Relationship D",
          col.regions = NA, cuts=1,  at = c(-5,0.01,5), contour=T,lty = 2, panel=panel.2dsmoother, args=list(span=0.1), scales = list(tck = c(0.8,0)))

colnames(rel.C.eval.output)
table(rel.C.eval.output$slope.Y.B)
table(rel.B.eval.output$slope.Y.E)

# biodiv.at.max.production
levelplot(biodiv.at.max.production ~  mean.WL.Y * sd.WL.Y, data=rel.A.eval.output, main = "Relationship A",
          col.regions = col, cuts=10, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0)))
levelplot(biodiv.at.max.production ~  min.Y.E * slope.Y.E, data=rel.B.eval.output, main = "Relationship B",
          col.regions = col, cuts=10, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0)))
levelplot(biodiv.at.max.production ~  min.Y.B * slope.Y.B, data=rel.C.eval.output, main = "Relationship C",
          col.regions = col, cuts=10, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0)))
levelplot(biodiv.at.max.production ~  (1-min.Bio.E) * slope.Bio.E, data=rel.D.eval.output, main = "Relationship D",
          col.regions = col, cuts=10, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0)))

# edge.distance
levelplot(edge.distance ~  mean.WL.Y * sd.WL.Y, data=rel.A.eval.output, main = "Relationship A",
          col.regions = col, cuts=10, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0)))
levelplot(edge.distance ~  min.Y.E * slope.Y.E, data=rel.B.eval.output, main = "Relationship B",
          col.regions = col, cuts=10, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0)))
levelplot(edge.distance ~  min.Y.B * slope.Y.B, data=rel.C.eval.output, main = "Relationship C",
          col.regions = col, cuts=10, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0)))
levelplot(edge.distance ~  (1-min.Bio.E) * slope.Bio.E, data=rel.D.eval.output, main = "Relationship D",
          col.regions = col, cuts=10, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0)))

plot(rel.B.eval.output$edge.distance ~ rel.B.eval.output$biodiv.at.max.production)
plot(rel.A.eval.output$edge.distance ~ rel.A.eval.output$biodiv.at.max.production)

# convex.nature
levelplot(convex.nature ~  mean.WL.Y * sd.WL.Y, data=rel.A.eval.output, main = "Relationship A",
          col.regions = col, cuts=10, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0)))
levelplot(convex.nature ~  min.Y.E * slope.Y.E, data=rel.B.eval.output, main = "Relationship B",
          col.regions = col, cuts=10, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0)))
levelplot(convex.nature ~  min.Y.B * slope.Y.B, data=rel.C.eval.output, main = "Relationship C",
          col.regions = col, cuts=10, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0)))
levelplot(convex.nature ~  (1-min.Bio.E) * slope.Bio.E, data=rel.D.eval.output, main = "Relationship D",
          col.regions = col, cuts=10, panel=panel.2dsmoother, args=list(span=0.20), scales = list(tck = c(0.8,0)))

plot(rel.B.eval.output$edge.distance ~ rel.B.eval.output$biodiv.at.max.production)
plot(rel.A.eval.output$edge.distance ~ rel.A.eval.output$biodiv.at.max.production)

length(which(eval.output.meta$edge.distance==0))/nrow(eval.output.meta)

##### II) Systematic sensitivity analysis - change habitat requirements #####

# A) define grid for the habitat requirements.
x<-c();y<-c();z<-c(); resolution<-70 #determine resolution
res<-seq(1,98, length = resolution)
for (i in res){for (k in res[which(res<(99-i))]){x<-c(x,i);  y<-c(y,k);   z<-c(z,(100-i-k) )}}
habitat.grid<-data.frame(WL = x, NWL = y, both = z)
# define borders of habitat

habitat.grid<-habitat.grid[-which(habitat.grid$WL<5|habitat.grid$WL>80|habitat.grid$NWL<20|habitat.grid$NWL>90|habitat.grid$both<5| habitat.grid$both>80),]

# create other model constants
source("function_create_model_constants_casestudies.R")
constants.specific<-read.csv('constants_input_means.csv',header=T, dec=".",sep=",",na.strings="NA")
create.casestudy.constants() # make sure the case-study file has the mean values of literature search.
# Watch out again, earlier output is LOST!!!!!
constants.meta<-data.frame(); opp.cost.meta<-list(); constants.meta<-data.frame(); eval.output.meta<-c()

# B) loop where for each combination of grid variables the model output is computed
for(i in 1:nrow(habitat.grid)){
  # i) set habitat preferences
  habitat.pref.prop<-c(habitat.grid$WL[i],habitat.grid$NWL[i],habitat.grid$both[i])
  habitat.preferences<- c(rep("WL",habitat.grid$WL[i]),rep("NWL",habitat.grid$NWL[i]),rep("both",habitat.grid$both[i])) 
  E <- seq(0,1, by= 0.01); WL<- seq(0,1, by= 0.01)
  
  # ii) run the model function
  output<-complex.model()
  
  # iii) extract the meta-data from the output for the sensitivity analysis
  evaluation()

  # iv) extract the meta-data from the output for the sensitivity analysis
  slope<-(max(opport.costs$P)-min(opport.costs$P))/ (max(opport.costs$B)-min(opport.costs$B))
  intercept<- max(opport.costs$P) + slope * min(opport.costs$B); prediction<- intercept + slope * opport.costs$B * (-1)
  eval.output<-c(eval.output,mean(opport.costs$P-prediction))
  
  # V) store and name everything
  constants.meta<-rbind(constants.meta, model.constants)  # all constants from all simulations
  opp.cost.meta<-append(opp.cost.meta, list(opport.costs[,1:2] ))
  eval.output.meta<- rbind(eval.output.meta, eval.output)}

colnames(constants.meta)<-c("neg.spill","TE.sd","TE.mean", "Ymax","Cost.E","Cost.baseline","min.Cost.WL", "slope.Cost.WL","min.Y.E","slope.Y.E", "min.Y.B", "slope.Y.B","mean.WL.Y", "sd.WL.Y", "habitat.WL", "habitat.NWL", "habitat.both", "min.Bio.E", "slope.Bio.E")
eval.output.meta<-as.data.frame(eval.output.meta)
colnames(eval.output.meta)<-c("biodiv.at.max.production", "effort.at.max.production","WL.at.max.production", "edge.distance",
                              "max.eff.production", "max.eff.biod","max.P", "curverture.of.opp.costs")

b90<-c()
for (i in 1:length(opp.cost.meta)){
  b90<-c(b90, opp.cost.meta[[i]]$B[which(abs(0.9-opp.cost.meta[[i]]$P)==min(abs(0.9-opp.cost.meta[[i]]$P)))])}
eval.output.meta$b90<-b90

# C) Ternary contour 
library(ggtern)
# colour definition
col<- rev(c("#7B3014", "#D04A07", rgb(255,242,204,maxColorValue =255), rgb(90,165,205,maxColorValue =255), rgb(35,108,167,maxColorValue =255),  rgb(38,69,110,maxColorValue =255))) # we can use this one...
rbPal <- colorRampPalette(col)
n<- 30; col <- rev(rbPal(n))

plot.dat<- data.frame(WL = constants.meta$habitat.WL, NWL = constants.meta$habitat.NWL, both = constants.meta$habitat.both, 
                      eval.output.meta[,10], trap = eval.output.meta$max.eff.production); 
plot.dat$trap[which(plot.dat$trap==1)]<-0
colnames(plot.dat)[4]<-colnames(eval.output.meta)[10] # modify in eval.output.meta[,1] - also 4 and 5
hist(plot.dat$b90)

#700*500 - change to trap if you want to have the contour line 
ggtern(data = plot.dat, aes(x = WL,y = NWL,z = both, colour= b90))+ 
  geom_point(size = 5, alpha = 0.9) + theme_bw(base_size = 12, base_family = "") + 
  scale_colour_gradientn(colours = col)+
  scale_T_continuous(limits=c(0.15,1.0), breaks=seq(0,1,by=0.2)) + 
  scale_L_continuous(limits=c(0.0,0.85), breaks=seq(0,1,by=0.2)) +
  scale_R_continuous(limits=c(0.0,0.85),breaks=seq(0,1,by=0.2)) +
  geom_Tline(Tintercept = 0.2, size = 0.9, color = "black", linetype = "dashed", alpha = 0.5)+
  geom_Tline(Tintercept = 0.4, size = 0.9, color = "black", linetype = "dashed", alpha = 0.5)+
  geom_Tline(Tintercept = 0.6, size = 0.9, color = "black", linetype = "dashed", alpha = 0.5)+
  geom_Tline(Tintercept = 0.8, size = 0.9, color = "black", linetype = "dashed", alpha = 0.5)+
  geom_Lline(Lintercept = 0.2, size = 0.9, color = "black", linetype = "dashed", alpha = 0.5)+
  geom_Lline(Lintercept = 0.4, size = 0.9, color = "black", linetype = "dashed", alpha = 0.5)+
  geom_Lline(Lintercept = 0.6, size = 0.9, color = "black", linetype = "dashed", alpha = 0.5)+
  geom_Lline(Lintercept = 0.8, size = 0.9, color = "black", linetype = "dashed", alpha = 0.5)+
  geom_Rline(Rintercept = 0.2, size = 0.9, color = "black", linetype = "dashed", alpha = 0.5)+
  geom_Rline(Rintercept = 0.4, size = 0.9, color = "black", linetype = "dashed", alpha = 0.5)+
  geom_Rline(Rintercept = 0.6, size = 0.9, color = "black", linetype = "dashed", alpha = 0.5)+
  geom_Rline(Rintercept = 0.8, size = 0.9, color = "black", linetype = "dashed", alpha = 0.5)
