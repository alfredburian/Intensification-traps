plot.model.results <- function(){

  
  # plot biodiversity and its dependence on management (E and WL)
  # 350*325
  n<-20
  col<- rev(c("#7B3014", "#D04A07","#F98C40", rgb(255,242,204,maxColorValue =255), rgb(90,165,205,maxColorValue =255), rgb(35,108,167,maxColorValue =255),  
              rgb(38,69,110,maxColorValue =255)))
  rbPal <<- colorRampPalette(col)
  col <- rbPal(n)
  
plot.Bio<-levelplot(B ~ WL * E, data=output, main = "Total Biodiveristy",at=seq(0, 1, length.out=n), 
            col.regions = col, cuts=n, panel=panel.2dsmoother, args=list(span=0.15), scales = list(tck = c(0.8,0)))
  
  # plot productivity and its dependence on management (E and WL)
plot.Y<-levelplot(Y ~ WL * E, data=output,main = "Yield",at=seq(0, 1, length.out=n), 
            col.regions = col, cuts=n, panel=panel.2dsmoother, args=list(span=0.15), scales = list(tck = c(0.8,0)))
  
  # plot total production and its dependence on management (E and WL)
plot.P<-levelplot(P ~ WL * E, data=output,main = "Production",at=seq(0, 1, length.out=n), 
            col.regions = col, cuts=n, panel=panel.2dsmoother, args=list(span=0.05), scales = list(tck = c(0.8,0)))

# create trade-off curve and then plot it with ggplot

# 3) calculation of opportunity costs between B and P
# (i) order all B that are higher than biodiv.at.max.production
biodiv.at.max.production<- max(output$B[which(output$P==max(output$P))])
effort.at.max.production<- output$E[which(output$P==max(output$P))][1] # effort that results in max. production
WL.at.max.production<-output$WL[which(output$P==max(output$P))][1] # fraction of WL that results in max. production
ordered.B<-order(output$B[which(output$B>=biodiv.at.max.production)], output$P[which(output$B>=biodiv.at.max.production)]*(-1))
data<- data.frame(B= output$B[which(output$B>=biodiv.at.max.production)][ordered.B], 
                  P = output$P[which(output$B>=biodiv.at.max.production)][ordered.B])
# (ii) some identical B values will be the result of different production scenarios - make sure you only consider the ones with highest
# P value
unique.B<-unique(output$B[which(output$B>=biodiv.at.max.production)]) #get unique B, which is higher than biodiv.at.max.production
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

#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c(rgb(20,46,71, maxColorValue=255),rgb(86,176,245, maxColorValue=255)))
rbPal <- colorRampPalette(rev(brewer.pal(9, "Greens")[5:9])) #YlGn
#rbPal <- colorRampPalette(cbPalette6[c(1,4)])
rbPal <-colorRampPalette(c(rgb(234,163,73, maxColorValue =255),rgb(98,129,157, maxColorValue =255)))

col <- rbPal(200)[as.numeric(cut(output$E,breaks = 200))]

# 260x 235 ## innen: doppelt
plot.tradeoff<<-ggplot(data=opport.costs, aes(x = B, y=P))+ geom_point(data=output, aes(x=B, y=P), colour = col, size = 1.1)+ 
  theme_bw()  + geom_line(colour=cbPalette6[5], size = 1.8) + scale_x_continuous(limits = c(0, 1))

venn.colour<-c(
  rgb(98,129,157, maxColorValue =255), rgb(234,163,73, maxColorValue =255),
  rgb(98, 103, 106, maxColorValue =255), rgb(234,163,73, maxColorValue =255),
  rgb(244, 153, 38, maxColorValue =255), rgb(234,163,73, maxColorValue =255))

output[which(output$P==1),]
biodiv.frac<- as.vector(round(as.numeric(output[which(output$P==1),c(2:4,8,9)]),2))
completely_contained <- euler(c("Total"=1*100, "WL" = 0, "N-WL" = 0,"Total&WL" = (biodiv.frac[1] * (1-biodiv.frac[4])+0.001)*100, 
                                "Total&N-WL" = (biodiv.frac[2]* (1-biodiv.frac[5])+0.001)*100, 
                                "Total&WL&N-WL" = (biodiv.frac[1]*biodiv.frac[4]+biodiv.frac[2]*biodiv.frac[5] +0.001)*100))

plot.venn<<-plot(completely_contained,quantities = TRUE,
     labels = list(font = 3),fills = list(fill = c("white",venn.colour[1],venn.colour[2], "", "", "",venn.colour[3])),
     legend = list(labels = c("Maximal realised B","Biodiversity in WL", "Biodiversity in N-WL")), edges = list(lty = c(2,1,1,1)))

plot.venn2<<-plot(completely_contained,quantities = F,
                 labels = NA,fills = list(fill = c("white",venn.colour[1],venn.colour[2], "", "", "",venn.colour[3])),
                 legend = list(labels = c("Maximal realised B","Biodiversity in WL", "Biodiversity in N-WL")), edges = list(lty = c(2,1,1,1)))

library(gridExtra)
dev.off()
grid.arrange(plot.Bio, plot.Y,plot.P,  ncol=3, nrow=1)
  
}
