create.casestudy.constants <- function(output){
  
# these are fixed parameters
slope.Cost.WL<<-0.3
min.Cost.WL<<-0.35
Cost.baseline<<-0.4
Cost.E<<-0.8
Ymax<<-1
scale.Cost.WL<<- (1-min.Cost.WL)/ (1/(1+slope.Cost.WL))
neg.spill<<-0.9

# these are the parameters that are loaded in for the 5 relationships that need to defined

# 1) Bio.E
slope.input.Bio.E<<-constants.specific$value[which(constants.specific$constant=="slope.input.Bio.E")]
min.Bio.E<<-constants.specific$value[which(constants.specific$constant=="min.Bio.E")]
scale.Bio.E<<- 1-min.Bio.E 

# 2) Y.WL
mean.Y.WL<<-constants.specific$value[which(constants.specific$constant=="mean.Y.WL")]
var.Y.WL<<-constants.specific$value[which(constants.specific$constant=="var.Y.WL")]

# 3) Y.E
slope.input.Y.E<<-constants.specific$value[which(constants.specific$constant=="slope.input.Y.E")]
min.Y.E<<-constants.specific$value[which(constants.specific$constant=="min.Y.E")]
scale.Y.E<<- 1-min.Y.E

# 4) B.WL
TE.mean<<-constants.specific$value[which(constants.specific$constant=="TE.mean")]
TE.sd<<-constants.specific$value[which(constants.specific$constant=="TE.sd")]
#neg.spill<<-constants.specific$value[which(constants.specific$constant=="neg.spill")]

# 5) Y.B
slope.input.Y.B<<-constants.specific$value[which(constants.specific$constant=="slope.input.Y.B")]
min.Y.B<<-constants.specific$value[which(constants.specific$constant=="min.Y.B")]
scale.Y.B<<- 1-min.Y.B

# 6) distribution of species across habitat types
habitat.pref.prop<-c(constants.specific$value[which(constants.specific$constant=="WL.habitat")],
                     constants.specific$value[which(constants.specific$constant=="NWL.habitat")],
                     constants.specific$value[which(constants.specific$constant=="both.habitat")])
habitat.preferences<<- c(rep("WL",habitat.pref.prop[1]),rep("NWL",habitat.pref.prop[2]),rep("both",habitat.pref.prop[3])) 


}
