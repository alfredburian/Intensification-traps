plot.input.relationships <- function(){

# 1) RELATIONSHIP BETWEEN B AND WL
# (I) create species pool
TE<- rnorm(10000,TE.mean,TE.sd);  TE<-TE[which(TE<=1 & TE>=0)]
# attribute species from the sp. pool to different habtitats 
HP<-sample (habitat.preferences, size = length(TE), replace = T) #HP stands for habiat preferences

spec.pool<-data.frame(TE = TE, HP = HP)

B<-c(); WL.B<- c(); E.B<-c(); x<-c(); species.n<-c(); species.WL <- c(); species.NWL<- c(); species.both<-c()
for (i in 1:length(WL)){NWL.sp<- length(which(spec.pool$HP=="NWL" & spec.pool$TE<(1-WL[i]))) #species specialised on NWL
WL.sp<- length(which(spec.pool$HP=="WL" & spec.pool$TE<WL[i])) #species specialised on  WL
both.sp<- length(which(spec.pool$HP=="both")) #species living in both habitats
species.n<-c(species.n, rep(c(both.sp+WL.sp+NWL.sp),length(E))) #total species richness
species.WL<-c(species.WL, rep(c(WL.sp+both.sp*WL[i]),length(E))) # species in WL - species in both are counted as 
#indifferent and species counts are distributed between WL and NWL according to the proportion of these landuese in the total area
species.NWL<-c(species.NWL, rep(c(NWL.sp+both.sp*(1-WL[i])),length(E))) # species in NWL
species.both<-c(species.both, rep(both.sp,length(E)))
WL.B<- c(WL.B, rep(WL[i], length(E))); E.B<-c(E.B, E); species.n}
output<- data.frame(species.n, species.WL, species.NWL,species.both, WL = WL.B, E = E.B)

# (II) normalise species number to values between 1 and 0
output$B<- species.n/max(species.n)
output$species.WL<- species.WL/max(species.n)
output$species.NWL<- species.NWL/max(species.n)
output$species.both<- species.both/max(species.n)

colors <- c("Total B" = cbPalette6[1], "B in WL" = cbPalette6[2], "B in non-WL" = cbPalette6[3], "B in both" = cbPalette6[4])
outputE<-output
Rel.E<-ggplot(data=outputE, aes(x=WL)) +  theme_bw()+geom_line(aes(y = B, color = "Total B"), size = 1.1)+
  geom_line(aes(y = species.WL-species.both*WL, color = "B in WL"), size = 1.1)+
  geom_line(aes(y = species.NWL-species.both*(1-WL), color = "B in non-WL"), size = 1.1)+
  geom_line(aes(y = species.both, color = "B in both" ), size = 1.1)+ scale_color_manual(values = colors)+
  labs(x = "WL [proportion]", y = "Biodiv", color = "Legend")+ggtitle("Relationship E")


# 3) RELATIONSHIP BETWEEN E AND Y
slope.Y.E <- 1+slope.input.Y.E*(-1) + abs(slope.input.Y.E)^3*6 # scales the slope to an appropriate scale
Y<- rev(output$E^slope.Y.E* scale.Y.E + min.Y.E)  # Effort trem
Y<- rep(Ymax,nrow(output)) * (min.Y.E +(1-Y))
output$Y<-Y

Rel.B<-ggplot(data=output, aes(x=E, y = Y)) +  theme_bw()+geom_line(size = 1.1, color = cbPalette6[1])+ggtitle("Relationship B")+
  ylim(0, 1)

# 4) RELATIONSHIP BETWEEN Y AND B

Y<- rep(Ymax,nrow(output))
output$B<-seq(0,1,length.out=nrow(output))

# Biodiv based reduction of Ymax (convex or concave)
if(slope.input.Y.B>=0){ #convex relationships
  slope.Y.B <- 1+slope.input.Y.B + slope.input.Y.B^3*6 # range is from 1 (linear) to 8 (highly non-linear)
  x <- Y * output$B^slope.Y.B* scale.Y.B + min.Y.B; Y<-Y*x} 
if(slope.input.Y.B<0){  #concave relationship
  slope.Y.B <- 1+slope.input.Y.B*(-1)+abs(slope.input.Y.B)^3*(6) # range is from 1 (linear) to 8 (highly non-linear)
  x <-   rev(output$B^slope.Y.B * scale.Y.B + min.Y.B)
  x <- (min.Y.B + (1-x)); Y<-Y*x}
output$Y<-Y

Rel.C<-ggplot(data=output, aes(x=B, y = Y)) +  theme_bw()+geom_line(size = 1.1, color = cbPalette6[2])+
  ggtitle("Relationship C")+ylim(0, 1)

# 5) Relationship between E AND B

if(slope.input.Bio.E>=0){ #concave
  slope.Bio.E <- 1+slope.input.Bio.E + slope.input.Bio.E^3*6 # scaling the slope
  output$B <- (1-output$E^slope.Bio.E*scale.Bio.E) # accounts for E in WL
  x<- (1-output$E^slope.Bio.E*scale.Bio.E * output$WL^neg.spill)} else { #convex relationship
    # second line accounts fo E in NWL, whereas the spillover effect is defined by size of WL and a scaling constant
    slope.Bio.E <- 1+slope.input.Bio.E *(-1)+abs( slope.input.Bio.E)^3*6 # scaling the slope
    x<-rev(1-output$E^slope.Bio.E*scale.Bio.E)
    output$B <- (min.Bio.E + (1-x))  # accounts for E in WL
    x<-rev(1-output$E^slope.Bio.E*scale.Bio.E)* output$WL^neg.spill 
    x<-(min.Bio.E*output$WL^neg.spill  + (1-x))}
# the second part of the if statement is mathematically a bit more tricky. The output$WL^neg.spill makes the effect of E on B in NWL 
# dependent on the amount of WL [%] in the landscape. It needs to be twice incorporated when the slope is inverted. A bit trickier.
# NOTE: if neg.spill is below 1 then it increases the effect of WL on B in NWL (WL-multiplicator becomes higher), if it is above 1, 
#       the effect of WL on B in NWL becomes lower (WL-multiplicator becomes lower through the effect of neg.spill)
output$x<-x

Rel.D.WL<-ggplot(data=output, aes(x=E, y = B)) +  theme_bw()+geom_line(size = 1.1, color = cbPalette6[3])+ggtitle("Relationship D")+
  ylim(0, 1)+ ylab("B in WL")

Rel.D.NWL<<-levelplot(x ~ WL * E, data=output,main = "Reduction of B in non-WL due to E",
          col.regions = col, cuts=10, panel=panel.2dsmoother, args=list(span=0.15))

# 2) WL - Bio
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

WL.Y.dist<<-data.frame(WL = borders$WL , Y = borders$means)

Rel.A<<-ggplot(data=WL.Y.dist, aes(x=WL, y = Y)) +  theme_bw()+geom_line(size = 1.1, color = "darkred")+ylim(0, 1)+
  ggtitle("Relationship A")

#ylim(0, 1)
disA<-data.frame(dist=dist)
Rel.A.hist<-ggplot(disA, aes(x=dist)) + geom_histogram(color="black", fill="darkred", binwidth = 0.01, alpha = 0.4)+ theme_bw() +
  ggtitle("Histogram of grid cell fertility")

ggarrange( Rel.A, Rel.A.hist, Rel.B, Rel.C, Rel.D.WL,Rel.E ,
          labels = c("A", "B", "C","D","E","F"),
          ncol = 3, nrow = 2)
}