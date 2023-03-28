complex.model <- function(output){

############################################################################################################################
# (i) compute biodiversity
# Biodiv is computed in a stepwise procedure. First, the species pool is create and each species gets a randomly attributed TE (thres-
# hold of exitance value) and habitat requirements. Random attribution is based on drawing values from a predefined distribution (for
# TE this is TE.mean andTE.sd, which define a normal distribution; for habitat preference, HP, this are the ratios of species occurence
# between habitat types).
# The second step is to compute for a grid of E(intensification effort) and WL (fraction of working land used) combination B in WL and
# non-WL. There are two substeps to implement this step. First, only habitat availability is considered. For a given WL-non WL ratio in
# the landscape, the number of species, which can exist only in non-WL (requirement is only non-WL and threshold value of existence is
# below the actual fraction of non-WL in the landscape) is assessed. Analogous, the species that only can survive in WL are assessed
# for each WL-nonWL ratio. Afterwards the species, which can survive in both are distributed between the two habitat types according to
# the ratio of WL-nonWL.
# The second sub-step is to incorporate the negative effect of E on B in WL and nonWL. (see below for further details)

# (I) create species pool
TE<- rnorm(10000,TE.mean,TE.sd);  TE<-TE[which(TE<=1 & TE>=0)] # attribute species from the sp. pool to different habitats 
HP<-sample (habitat.preferences, size = length(TE), replace = T) #HP stands for habitat preferences

spec.pool<-data.frame(TE = TE, HP = HP)

B<-c(); WL.B<- c(); E.B<-c(); x<-c(); species.n<-c(); species.WL <- c(); species.NWL<- c(); species.both<-c()
for (i in 1:length(WL)){NWL.sp<- length(which(spec.pool$HP=="NWL" & spec.pool$TE<(1-WL[i]))) #species specialised on NWL
                        WL.sp<- length(which(spec.pool$HP=="WL" & spec.pool$TE<WL[i])) #species specialised on  WL
                        both.sp<- length(which(spec.pool$HP=="both")) #species living in both habitats
                        species.n<-c(species.n, rep(c(both.sp+WL.sp+NWL.sp),length(E))) #total species richness
                        species.WL<-c(species.WL, rep(c(WL.sp+both.sp*WL[i]),length(E))) # species in WL - species in both are counted as 
#indifferent and species counts are distributed between WL and NWL according to the proportion of these land uses in the total area
                        species.NWL<-c(species.NWL, rep(c(NWL.sp+both.sp*(1-WL[i])),length(E))) # species in NWL
                        species.both<-c(species.both, rep(both.sp,length(E)))
                        WL.B<- c(WL.B, rep(WL[i], length(E))); E.B<-c(E.B, E); species.n}
output<- data.frame(species.n, species.WL, species.NWL,species.both, WL = WL.B, E = E.B)

# (II) normalise species number to values between 1 and 0
output$B<- species.n/max(species.n)
output$species.WL<- species.WL/max(species.n)
output$species.NWL<- species.NWL/max(species.n)
output$species.both<- species.both/max(species.n)
# calculates how many species in WL/ NWL are also able to survive in the other habitat...
output$fraction.WL.both<-(output$species.both*output$WL) /(output$species.WL + output$species.both*output$WL)
output$fraction.NWL.both<-(output$species.both*(1-output$WL)) / (output$species.NWL+ output$species.both*(1-output$WL))
output$fraction.NWL.both[which(output$fraction.NWL.both==Inf)]<-0; output$fraction.WL.both[which(output$fraction.WL.both==Inf)]<-0
output$fraction.NWL.both[is.nan(output$fraction.NWL.both)]<-0; output$fraction.WL.both[is.nan(output$fraction.WL.both)]<-0

# (III) compute number of species in different landscapes factoring in land use intensification
# The effect of E on B in WL is evaluated based on the relationship D in Fig. 1. The negative impact of E on B is thereby determined
# by a constant that defines the impact size (e.g. the B that is left when E is 1, i.e. highest; defined as min.Bio.E) and a slope
# parameter. The slope parameter (slope.Bio.E) ranges between -1 and 1, whereas -1 denotes a strongly concave relationship, 0 denotes a 
# linear relationship and 1 denotes a convex relationship (see Fig. S1). This relationship determines the reduction of B in WL at a 
# given effort E. The negative effect of E on nonWL (negative spill-over effects) depends on the proportion of WL to the whole landscape
# -> based on formula in line 54. 

# the code is based on a switch function (concave vs. convex relationships) and is mathematically a bit more tricky.  
if(slope.input.Bio.E>=0){ #convex
slope.Bio.E <- 1+slope.input.Bio.E + slope.input.Bio.E^3*6 # scaling the slope

x<-rev(1-output$E^slope.Bio.E*scale.Bio.E)
output$species.WL <- (min.Bio.E + (1-x)) * output$species.WL # accounts for E in WL
x<-rev(1-output$E^slope.Bio.E*scale.Bio.E)* output$WL^neg.spill 
x<-(min.Bio.E*output$WL^neg.spill  + (1-x))
output$species.NWL<- x * output$species.NWL} else { #concave relationship
# second line accounts fo E in NWL, whereas the spillover effect is defined by size of WL and a scaling constant
slope.Bio.E <- 1+slope.input.Bio.E *(-1)+abs( slope.input.Bio.E)^3*6 # scaling the slope
output$species.WL <- (1-output$E^slope.Bio.E*scale.Bio.E) * output$species.WL # accounts for E in WL
output$species.NWL<- (1-output$E^slope.Bio.E*scale.Bio.E * output$WL^neg.spill)* output$species.NWL}
# the first part of the if statement is mathematically a bit more tricky. The output$WL^neg.spill makes the effect of E on B in NWL 
# dependent on the amount of WL [%] in the landscape. It needs to be twice incorporated when the slope is inverted. A bit trickier.
# NOTE: if neg.spill is below 1 then it increases the effect of WL on B in NWL (WL-multiplicator becomes higher), if it is above 1, 
#       the effect of WL on B in NWL becomes lower (WL-multiplicator becomes lower through the effect of neg.spill)
output$B<-output$species.WL+output$species.NWL

############################################################################################################################
# (ii) compute the relationship between yield and WL based on heterogenity in land fertility (achievable Y)
# First a distribution of the production potential (Y) is defined by it's mean and it's variance. Numbers in the distribution are restricted between 0  
# and 1 (fraction of optimum Y that can be achieved). In mathematical terms a beta distribution is used. Note that the variance of the distribution is
# constrained - e.g. a mean of 0.1 can only have a certain degree of variance attributed when the precondition that all numbers need to between 0 and 1
# is still fullfilled. Mean Y in the agricultrually used landscape is then computed by using WL as fraction in the probabi

# A) transform var.Y.WL (fraction of the maximal possible var at a given mean) into real var
var<- mean.Y.WL*(1-mean.Y.WL)*var.Y.WL
# B) compute alpha and beta of the beta distribution
if (var >= mean.Y.WL*(1-mean.Y.WL)) {stop("error: variance in Y across the landscape is larger than allowed for the chosen mean")}
alpha <- ((1 - mean.Y.WL) / var - 1 / mean.Y.WL) * mean.Y.WL ^ 2
beta <- alpha * (1 / mean.Y.WL - 1)
# C) compute mean Y for the fraction of land in use based on a statistical approximation (to avoid a very complex integral...)
dist<-rbeta(10^5, alpha, beta)
borders<- data.frame(border = qbeta(unique(output$WL), alpha, beta, lower.tail = F), # defines the border at which all larger values are included
                        WL = unique(output$WL))
border<-as.data.frame(borders$border)
borders$means<-apply(border,1, function(x) {mean(dist[which(dist>x)])})
borders$means[which(is.nan(borders$means)==T)]<-1
output$Y<- borders$means[match(output$WL, borders$WL)]
  
############################################################################################################################
#(iii) compute yield for each E-WL combination and add to the output data-frame
# Yield will be computed in a three step procedure. First relationship B (effect of E on Y) will be parameterised based on a shape and 
# an impact size constant, analogeous to the effect of E on B assuming a concave curve shape. Second, the effect of biodiversity on 
# Y will be accounted for. Again, the relationship is parametrisied (based on impact size and slope), but in this case again a switch 
# function is implemented in the code to account for changes from convex to concave shapes. The third and final step is then the 
# integration of decreasing mean Y with increasing WL. This is based on the distribution of the production potential (see above).  

# Effort based reduction of Ymax (only convex)
slope.Y.E <- 1+slope.input.Y.E*(-1) + abs(slope.input.Y.E)^3*6 # scales the slope to an appropriate scale
Y<- rev(output$E^slope.Y.E* scale.Y.E + min.Y.E)  # Effort term
Y<- rep(Ymax,nrow(output)) * (min.Y.E +(1-Y))

# Biodiv based reduction of Ymax (convex or concave)
if(slope.input.Y.B>=0){ #convex relationships
  slope.Y.B <- 1+slope.input.Y.B + slope.input.Y.B^3*6 # range is from 1 (linear) to 8 (highly non-linear)
  x <- output$B^slope.Y.B* scale.Y.B + min.Y.B; Y<-Y*x}

if(slope.input.Y.B<0){  #concave relationship
    slope.Y.B <- 1+slope.input.Y.B*(-1)+abs(slope.input.Y.B)^3*6 # range is from 1 (linear) to 8 (highly non-linear)
    x <-   rev(output$B^slope.Y.B * scale.Y.B + min.Y.B)
    x <- (min.Y.B + (1-x)); Y<-Y*x}

output$Y<-output$Y*Y

t<- rep(1, nrow(output)) # time is incorporated, but can be neglected for now - I set it to one, which means we are looking at annual P
output$P<- output$Y * output$WL * t
output$P<- output$P/max(output$P)# scale so that max production is one

# Cost functions - right now this is not considered - please disregard. 
# Second, account for intensification effort
output$Cost.unscaled<-Cost.baseline + output$E * Cost.E # reflects the area-unscaled production costs per WL area

# Calculate real total costs
output$Cost.real<- rep(NA,nrow(output))
for (i in 1:nrow(output)){
  output$Cost.real[i]<-(1 - output$WL[i]/(output$WL[i] + slope.Cost.WL )*scale.Cost.WL) *  output$Cost.unscaled[i] * output$WL[i]}

# (ii) compute profite based on production and costs
price <- rep(1,nrow(output)) # product price per production of one - just for simplisisty set to 1
output$S<- price *  output$P - output$Cost.real 

output<-output}

######################################################################################################