create.model.constants <- function(output){

# 1) Bio.E
repeat{slope.input.Bio.E<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="slope.input.Bio.E")], 
                          sd = constants.mean$sd[which(constants.mean$constant=="slope.input.Bio.E")], n = 1)
                          if(slope.input.Bio.E>=c(-1)& slope.input.Bio.E<=c(1)) {break}}
repeat{min.Bio.E<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="min.Bio.E")], 
                        sd = constants.mean$sd[which(constants.mean$constant=="min.Bio.E")], n = 1)
                        if(min.Bio.E>=c(0) & min.Bio.E<= 1){break}}
scale.Bio.E<<- 1-min.Bio.E 

# 2) Y.WL
repeat{var.Y.WL<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="var.Y.WL")], 
                           sd = constants.mean$sd[which(constants.mean$constant=="var.Y.WL")], n = 1); 
                           if(var.Y.WL>=0 & var.Y.WL<=1){break}}
repeat{mean.Y.WL<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="mean.Y.WL")], 
                           sd = constants.mean$sd[which(constants.mean$constant=="mean.Y.WL")], n = 1); 
                           if(mean.Y.WL>=0 & mean.Y.WL<=1){break}}

# 3) Y.E
repeat{slope.input.Y.E<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="slope.input.Y.E")], 
                        sd = constants.mean$sd[which(constants.mean$constant=="slope.input.Y.E")], n = 1)
                        if(slope.input.Y.E>=c(-1)& slope.input.Y.E<=c(0)) {break}}
repeat{min.Y.E<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="min.Y.E")], 
                      sd = constants.mean$sd[which(constants.mean$constant=="min.Y.E")], n = 1);
                      if(min.Y.E>=0& min.Y.E<=1){break}}
scale.Y.E<<- 1-min.Y.E

# 4) B.WL
repeat{TE.mean<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="TE.mean")], 
                       sd = constants.mean$sd[which(constants.mean$constant=="TE.mean")], n = 1); if(TE.mean>=0){break}}
repeat{TE.sd<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="TE.sd")], 
                     sd = constants.mean$sd[which(constants.mean$constant=="TE.sd")], n = 1); if(TE.sd>=0){break}}
repeat{neg.spill<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="neg.spill")], 
                         sd = constants.mean$sd[which(constants.mean$constant=="neg.spill")], n = 1); if(neg.spill>=0){break}}

# 5) Y.B
repeat{slope.input.Y.B<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="slope.input.Y.B")],
                        sd = constants.mean$sd[which(constants.mean$constant=="slope.input.Y.B")], n = 1)
                        if(slope.input.Y.B>=c(-1)& slope.input.Y.B<=c(1)) {break}}
repeat{min.Y.B<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="min.Y.B")],
                      sd = constants.mean$sd[which(constants.mean$constant=="min.Y.B")], n = 1);
                      if(min.Y.B>=0 & min.Y.B<=1){break}}
scale.Y.B<<- 1-min.Y.B

# 6) Cost
repeat{slope.Cost.WL<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="slope.Cost.WL")], 
                            sd = constants.mean$sd[which(constants.mean$constant=="slope.Cost.WL")], n = 1); if(slope.Cost.WL>=0){break}}
repeat{min.Cost.WL<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="min.Cost.WL")], 
                          sd = constants.mean$sd[which(constants.mean$constant=="min.Cost.WL")], n = 1); 
                          if(min.Cost.WL>=0 & min.Cost.WL<=1){break}}
scale.Cost.WL<<- (1-min.Cost.WL)/ (1/(1+slope.Cost.WL))

repeat{Cost.baseline<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="Cost.baseline")], 
                            sd = constants.mean$sd[which(constants.mean$constant=="Cost.baseline")], n = 1); if(Cost.baseline>=0){break}}
repeat{Cost.E<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="Cost.E")], 
                     sd = constants.mean$sd[which(constants.mean$constant=="Cost.E")], n = 1); if(Cost.E>=0){break}}
Ymax <<- 1 


## B.WL
# repeat{slope.input.Bio.WL<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="slope.input.Bio.WL")], 
#                          sd=constants.mean$sd[which(constants.mean$constant=="slope.input.Bio.WL")], n = 1); 
#                          if(slope.input.Bio.WL>=c(-1)& slope.input.Bio.WL<=c(1)) {break}}
# repeat{min.Bio.WL<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="min.Bio.WL")], 
#                          sd = constants.mean$sd[which(constants.mean$constant=="min.Bio.WL")], n = 1); 
#                          if(min.Bio.WL>=0& min.Bio.WL<= 1){break}}
# scale.Bio.WL<<- 1-min # analogous to above

# # Y.WL
# repeat{slope.input.Y.B<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="slope.input.Y.B")], 
#                         sd = constants.mean$sd[which(constants.mean$constant=="slope.input.Y.B")], n = 1)
#                         if(slope.input.Y.B>=c(-1)& slope.input.Y.B<=c(1)) {break}}
# repeat{min.Y.B<<-rnorm(mean = constants.mean$mean[which(constants.mean$constant=="min.Y.B")], 
#                       sd = constants.mean$sd[which(constants.mean$constant=="min.Y.B")], n = 1); 
#                       if(min.Y.B>=0 & min.Y.B<=1){break}}
# scale.Y.B<<- 1-min 
}

