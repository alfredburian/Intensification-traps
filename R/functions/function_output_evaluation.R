evaluation <- function(){
  
  # 1) biodiversity, effort and WL maintained with maximal production scenario
  biodiv.at.max.production<- max(output$B[which(output$P==max(output$P))])
  effort.at.max.production<- output$E[which(output$P==max(output$P))][1] # effort that results in max. production
  WL.at.max.production<-output$WL[which(output$P==max(output$P))][1] # fraction of WL that results in max. production
  max.eff.production<- output$P[which(output$E==1 & output$WL==1)] # production attained at maximal effort
  max.eff.biod<- output$B[which(output$E==1 & output$WL==1)] # biodiversity attained at maximal effort 
  
  # calculate risk of intensification traps
  risk<-length(which(output$B < max(output$B[output$P == max(output$P)]) & output$P < max(output$P)))/nrow(output)
  
  # intensity of intensification traps
  intensity<- max(output$P) - min(output$P[which(output$B < max(output$B[output$P == max(output$P)]) & output$P < max(output$P))])
  
  # 2) Proxy of the relative importance of biodiversity -> city block distance between max. production and the 
  # upper right edge of the working-land - effort space (=operational space for management). This distance is 0 if production is
  # independent of biodiversity and can be used as proxy for the strength of biodiversity-production trade-off
  edge.distance<- (1-effort.at.max.production)+(1-WL.at.max.production) 
  
  # 3) calculation of opportunity costs between B and P
  # (i) order all B that are higher than biodiv.at.max.production
  ordered.B<-order(output$B[which(output$B>=biodiv.at.max.production)], output$P[which(output$B>=biodiv.at.max.production)]*(-1))
  data<- data.frame(B= output$B[which(output$B>=biodiv.at.max.production)][ordered.B], 
                    P = output$P[which(output$B>=biodiv.at.max.production)][ordered.B])
  
  # (ii) some identical B values will be the result of different production scenarios - make sure you only consider the ones 
  # with highest P value
  unique.B<-unique(output$B[which(output$B>=biodiv.at.max.production)]) #get unique B, which is higher than biodiv.at.max.prod.
  unique.B<-unique.B[order(unique.B)] # order unique B
  data<-data[match(unique.B, data$B),]
  # (iii) get rid of all rows where B is lower than in a row with higher P
  repeat{
    data$order.P<-order(data$P*(-1))
    data$order.B<-order(data$B)
    data$order.P2<-match(data$order.B,data$order.P)
    data$diff<-data$order.B-data$order.P2
    if(  is.element(F,unique(data$order.P2==data$order.B))==F ){break}
    data<-data[which(data$diff>=0),]}
  
  # 4) prepare output 
  opport.costs<<-  data
  eval.output<<-  c(biodiv.at.max.production, effort.at.max.production, WL.at.max.production, edge.distance, max.eff.production, max.eff.biod,risk, 
                    intensity)
  model.constants<<-c(neg.spill,TE.sd,TE.mean, Ymax, Cost.E, Cost.baseline, min.Cost.WL, slope.Cost.WL, min.Y.E, slope.input.Y.E, 
                      min.Y.B, slope.input.Y.B, mean.Y.WL,var.Y.WL, habitat.pref.prop, min.Bio.E, slope.input.Bio.E)
}