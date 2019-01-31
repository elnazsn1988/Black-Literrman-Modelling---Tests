

library("quadprog")
library(BLCOP) 
library("MASS")
library(xlsx)
library(dplyr)
library(portfolio)
library(ggplot2)
install.packages("PerformanceAnalytics")
install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
library(IntroCompFinR)



setwd("C:\\Program Files\\R\\R-3.4.1")

RF<- mean(New_RF)
#Re<-read.xlsx("Input_Rtr.xlsx",1)
#ex<-read.xlsx("excess_Rtr.xlsx",1)
#RF<-read.xlsx("Riskfree.xlsx",1)
#MC<-read.xlsx("Market_cap.xlsx",1)
#N_I=length(Re)
#K=views
#K=3
#Re_dat <- data.frame(Re[,3:N_I]*12) ##--The data is multiplied by 12 to denote annual returns, if 3 is used, citi is removed.
#rownames(Re_dat)<- Re$Date
#Mc_dat<-data.frame(MC[,3:N_I]) ##as the CAPM are independent of dates, the Market cap isnt multiplied.
#rownames(Mc_dat)<- MC$Date
#Rf_dat<- data.frame(RF[,1]*12)  ###also muliplied by 12
#rownames(Rf_dat)<- Re$Date

RA_FTS4G["Risky_Asset"]<- NULL ###--Change from 5 to "Risky_assets" to generalize
RA_FTS4G_mv["Risky_Asset"]<- NULL###--Change from 5 to "Risky_assets" to generalize




###------Get ALL Combinations (2+)
New_Re_DS_GRN["Date"]<-NULL   
SP_tr["Date"]<-NULL  
New_Re["Date"]<-NULL  
New_Re["Risky_Asset"]<-NULL
 colMeans(New_Re)

DT_G<- as.data.table((New_Re_DS_GRN), keep.rownames=TRUE )
DT_C<- as.data.table((New_Re), keep.rownames=TRUE )
DT_SP1<- as.data.table((S1_tr), keep.rownames=TRUE )
DT_SP2<- as.data.table((S2_tr), keep.rownames=TRUE )
DT_SP3<- as.data.table((S3_tr), keep.rownames=TRUE )


colnames(S1_tr)<- colnames(SP_tr)[1]
colnames(S2_tr)<- colnames(SP_tr)[2]
colnames(S3_tr)<- colnames(SP_tr)[3]


R_G<-  colnames(New_Re_DS_GRN)
R_S1<- colnames(S1_tr)
R_S2<- colnames(S2_tr)
R_S3<- colnames(S3_tr)
R_c<-  colnames(New_Re)




All_n<- c(R_c,R_G, R_S1, R_S2, R_S3)
ALl_n_C<- list(New_Re,New_Re_DS_GRN,S1_tr,S2_tr,S3_tr)

MC_SP<- data.frame(203585.58, 41823.80, 39313.00)
colnames(MC_SP)<- colnames(SP_tr)

MC_GR<- data.frame(22261.60,	3130.90,	650.5,	100.1)
colnames(MC_GR)<- c( R_G[3], R_G[4], R_G[1], R_G[5])

MC_C<- data.frame(t(colMeans(market_value[1:(ncol(market_value)-1)])))
MC_ALL<- cbind(MC_C, MC_GR, MC_SP)


MC_ALL[1]<- 22278160
MC_ALL[2]<- 61021040
MC_ALL[3]<- 8308703
MC_ALL[4]<- 1472156
MC_ALL[8]<- 100100
MC_ALL[7]<- 650500
MC_ALL[9]<- 203600
MC_ALL[5]<- 2226160
MC_ALL[6]<- 3130910
MC_ALL[10]<-4182380
MC_ALL[11]<-3931300

ncol(MC_ALL)
CHECK<- unlist(ALl_n_C, recursive=FALSE)
length(CHECK)

###-----------------------------------------------
##-----------------------------------------------

R_AG<- c(R_G, R_S1, R_S2, R_S3)




####_____Creating Power Func for BL

All_BL<- intersect(All_n, names(MC_ALL))

##SP_m <- c(match( c(colnames(MC_SP)), All_n))



##PowerSet_BL<- function(ALl_n_C,All_BL, MC_ALL){
###PowerSet_BL(ALl_n_C,All_BL, MC_ALL)


Ccols<- list()
N_G<- list()
for (i in 2:length(All_BL)){
cols <- combn( (All_BL), i)

Ccols[[i]]<- cols
N_AG<- list()
  for (j in 1:ncol(Ccols[[i]])){
  N_C<-length(Ccols[[i]][,j][Ccols[[i]][,j] %in% R_AG])
  N_AG[[j]]<- N_C
  }
 N_G[[i]]<- N_AG
}

library(compiler)

YY<- list()
AJI<- list()
VayVay<- list()
Data_IJ<- list()
YEY<- list()
DT<- list()
for (i in 1:length(ALl_n_C)){
 for (j in 1:ncol(ALl_n_C[[i]])){
assign(paste0(colnames(ALl_n_C[[i]][j])), as.data.table((ALl_n_C[[i]][j]), keep.rownames=TRUE ))
}}

removeC<- function(DF){
DF["rn"]<-NULL
return(DF)}

Karim<- function(X,Y){
IJ<- list()
  IJ<- lapply(seq(from=1,to=(length(Ccols[[X]][,Y]))), function(i) get(Ccols[[X]][,Y][i]))
return(IJ)
          }
my_Karim<- cmpfun(Karim)



Asghar<- function(X,Y){
keke<- list()
common<- list()
Data_IJ<- list()
common <- Reduce(intersect, Map("[[", my_Karim(X,Y), "rn"))
New_RF[common]
keke<- lapply(my_Karim(X,Y), function(x) x[x$rn %in% common, ])
Data_IJ<- do.call(cbind, keke)
return(na.omit(Data_IJ))
}
my_Asghar <- cmpfun(Asghar)

Kokab<- function(X){
VayVay<- list()
 VayVay<- lapply(seq(from=1,to=(ncol(Ccols[[X]]))), function(i) my_Asghar(X,i))
return(VayVay)}
my_Kokab <- cmpfun(Kokab)

YEY<- lapply(seq(from=2,to=(length(Ccols))), function(i) my_Kokab(i)) 

lastf<- function(X,Y){
covar<- list()
estret<- list()
YEY_D<- list()
YEY_DF<- list()
YEY_df<- list()
MOFO_list<- list()

YEY_df<- data.frame(YEY[[X]][[Y]])
rownames(YEY_df)<- as.matrix(YEY_df["rn"])
YEY_DF<- YEY_df
Nullify<- names(YEY_DF %>% select(contains("rn")))
YEY_DF[Nullify]<- NULL
YEY_D<- YEY_DF
covar<- (cov(YEY_D))
estret<- colMeans(YEY_D)
corel<- (cor(YEY_D))

MOFO_list<- list(covar, estret, corel)

return(MOFO_list)
return(YEY_DF)
}


Moji<- function(X){
KGB<- list()
KGB<- lapply(seq(from=1,to=(length(YEY[[X]]))), function(i) lastf(X,i))
return(KGB)}

UNIVERSE_BL<- list()
UNIVERSE_BL<- lapply(seq(from=1,to=(length(YEY))), function(i) Moji(i))
##return(UNIVERSE_BL[[5]][[4]] )
##}

Simon_L<- c("WORLD.DS.Market" ,  "EMERGING.MARKETS.DS" , "JPM.GBI.GLOBAL" , "WORLD.DS.REITs" , "MSCI.GLOBAL.ENVIRONMENT"  , "S.P.Green" )
Jawab<- list()
for (i in 1:ncol(Ccols[[length(Simon_L)]])){

if ( setequal(Simon_L , Ccols[[length(Simon_L)]][,i])==TRUE){
Jawab<- i}
}

Jawab1<- list()
Kar<- list()
for (i in 1:length(UNIVERSE_BL[[length(Simon_L)]])){
if (setequal(Simon_L , c(rownames(UNIVERSE_BL[[length(Simon_L)-1]][[i]][[1]])) )==TRUE){
Jawab1<- i}
Kar[[i]]<- Jawab1
}

class(c(colnames(UNIVERSE_BL[[length(Simon_L)]][[i]][[1]])) )
class(Simon_L)

Variation_N_UBL<- Jawab1
Ccols[[6]][,4]
##UNIVERSE_BL[[5]][[4]][[2]]
###=======================================================
####========================GET RESULTS
###=======================================================

#cov.mat <- UNIVERSE_BL[[10]][[4]][[1]]
#er<- UNIVERSE_BL[[6]][[4]][[2]]
#G_i<- colnames(UNIVERSE_BL[[5]][[2]][[1]])
#Mc_dat<- MC_ALL[G_i]
#Var_m<- (diag(cov.mat))
Rf_mean<- mean(New_RF)
New_RF1<- New_RF[1:length(New_RF)]
New_RF1<- data.frame(New_RF1)
rownames(New_RF1) <-  (rownames(New_Re))

Gmin.list<- function(X){
Gmin.list<- list()
Gmin.list<- lapply(seq(from=1,to=length(UNIVERSE_BL[[X]])), function(i) globalMin.portfolio(UNIVERSE_BL[[X]][[i]][[2]], UNIVERSE_BL[[X]][[i]][[1]], shorts=FALSE) )
}
Gmin.all<- list()
Gmin.all<- lapply(seq(from=1,to=(length(UNIVERSE_BL))), function(i) Gmin.list(i))

GetMins<- function(X,i){
Min_Returns<- list()
Min_R<- list()
Min_Returns<- Gmin.all[[X]][[i]]$er
Min_sd<- Gmin.all[[X]][[i]]$sd
Min_W<-Gmin.all[[X]][[i]]$weights
NG_min<- names(Min_W)
WG<- sum(Min_W[NG_min[NG_min %in% R_AG]])

Min_R<- list(Min_Returns,Min_sd,WG)
return(list(Min_R))
}

Get_all_mins<- function(X){
Min_R<- list()
Min_Returns<- lapply(seq(from=1,to=(length(Gmin.all[[X]]) ) ) ,function(i) GetMins(X,i) )
}

GMR<- list()
GMR<- lapply(seq(from=1,to=(length(Gmin.all) ) ) ,function(i) Get_all_mins(i) )


SNOOPY<- function(X){
KAFKA<- list()
KAFKA<-lapply(seq(from=1,to=(length(GMR[[X]]) ) ), function(i) data.frame( X+1,i, N_G[[X+1]][[i]], GMR[[X]][[i]][[1]][[2]],GMR[[X]][[i]][[1]][[1]],GMR[[X]][[i]][[1]][[3]] ) )
}


BIGBANG<- lapply(seq(from=1,to=(length(GMR) ) ), function(i) SNOOPY(i))
BABANG<- (unlist(BIGBANG, recursive=FALSE))

library(ggthemes)

Min_Ret_Mat<- data.frame(do.call(rbind, BABANG))
colnames(Min_Ret_Mat)<- c("Asset_No", "Variation_No", "Gr_No", "sd", "er", "GW")
Rf_mean<- rep(mean(New_RF), nrow(Min_Ret_Mat))
RF<- mean(New_RF)


# create the subset
g1<- subset(Min_Ret_Mat , Asset_No==length(Simon_L)  & Variation_No==Jawab)

###------------------------------------------------------------------
###------------------------------------------------------------------
###------------------------------------------------------------------
###------------------------------------------------------------------
###------------------------------------------------------------------

SimonDocB<- docx()

#df<- melt(Min_Ret_Mat, Min_Ret_Mat$Asset_No)
PP<-  ggplot(Min_Ret_Mat, aes(x = Min_Ret_Mat$Asset_No, y=Min_Ret_Mat$er)) +
  geom_point(aes(colour = Min_Ret_Mat$Gr_No))+ scale_colour_gradient(low = "red", high = "green")+
  geom_hline(aes(yintercept=RF)) +xlab("Number of Assets in Portfolio")+geom_text(aes(0,RF,label = "Risk Free Rate", vjust = -1, hjust= -2.5))+
 ylab("GMVP Returns")+ 
ggtitle("Global Minimum Variance Portfolio(GMVP) Returns Vs Total Number of Assets in Portfolio" , subtitle = NULL)+ 
labs(colour = "Green Assets No (GMVP)")+
geom_point(data=g1, aes(x=as.numeric(g1$Asset_No), y=as.numeric(g1$er))) +
geom_text(data=g1, aes( as.numeric( as.character(g1$Asset_No)), as.numeric(as.character(g1$er)) ,label = "Simon's Portfolio", vjust =0.1, hjust= -0.1))

g1<- subset(Min_Ret_Mat , Asset_No==length(Simon_L)  & Variation_No==Jawab)


SimonDocB<- addTitle( SimonDocB, value=paste("Calculating Global Minimum Variance Portfolio (GMVP) Attributes " ), level = 2 ) 


#df<- melt(Min_Ret_Mat, Min_Ret_Mat$Asset_No)
PP1<-  ggplot(Min_Ret_Mat, aes(x = Min_Ret_Mat$Asset_No, y=Min_Ret_Mat$er)) +
  geom_point(aes(colour = Min_Ret_Mat$GW))+ scale_colour_gradient(low = "red", high = "green")+
  geom_hline(aes(yintercept=RF)) +xlab("Number of Portfolios")+geom_text(aes(0,RF,label = "Risk Free Rate", vjust = -1, hjust= -2.5))+
 ylab("GMVP Returns")+ 
ggtitle("Global Minimum Variance Portfolio(GMVP) Returns Vs Total Number of Assets in Portfolio" , subtitle = NULL)+ 
labs(colour = "Weight of Green Assets")+
geom_point(data=g1, aes(x=as.numeric(g1$Asset_No), y=as.numeric(g1$er))) +
geom_text(data=g1, aes( as.numeric( as.character(g1$Asset_No)), as.numeric(as.character(g1$er)) ,label = "Simon's Portfolio", vjust =0.1, hjust= -0.1))

SimonDocB<- addTitle( SimonDocB, value=paste("Calculating Global Minimum Variance Portfolio (GMVP) Attributes " ), level = 2 ) 

SimonDocB<- addParagraph(SimonDocB, value= "Caluclating all possible Portfolio's from the combination of the Assets, the global 
minimum Portfolio is drawn to compare to the Risk Free Rate. While we would expect that the returns of the porfolios 
always be more than the risk free rate,it is not always the case (as can be seen). This may be due to including portfolios 
that are only made of 2 or 3 assets,which are are all green/ bonds. To investigate this further, the return and standard deviations are 
plotted vs total number of assets and total green weight of portfolio (see appendix A). The risk free rate is then illustrated via additiong of a 
plane with a zintercept (horizontal plane) equal to the risk free rate. The Specific Portfolio chosen by Simon is shown by the black spot (does not reflect its green weight). It must be noted that the Markowitz optimization here does 
not allow shorting, to allow for better examination of green weight distribution across portfolios. " , stylename = "Normal" )
SimonDocB<- addPlot( SimonDocB , fun=print, x=PP)  
SimonDocB<- addPlot( SimonDocB , fun=print, x=PP1)  


x= Min_Ret_Mat$sd
y= Min_Ret_Mat$Asset_No
z= Min_Ret_Mat$er

library("plot3D")

# save plotting parameters
  pm <- par("mfrow")
## 
## imag3D> ## =======================================================================
## imag3D> ## images in x, y, z plane
## imag3D> ## =======================================================================
## imag3D> 

p44a.pryr %<a-% {


scatter3D(x = Min_Ret_Mat$sd, y = Min_Ret_Mat$Asset_No, z = Min_Ret_Mat$er, colvar = Min_Ret_Mat$Gr_No, 
      pch = 16, cex = 1.5, xlab = "Standard Deviation", ylab = "Number Of Assets", 
     zlab = "Return", clab = c("Number of","Green Assets"),
     main = "GMVP Return vs SD and Green Number of Assets", ticktype = "detailed", 
       theta = 10, d = 2, 
      colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))}

SimonDocB<- addTitle( SimonDocB, value=paste("GMVP return vs Standard Deviation and Total number of assets" ), level = 3 )   
SimonDocB<- addPlot( SimonDocB , fun = function() p44a.pryr)

p44b.pryr %<a-% {
scatter3D(x = Min_Ret_Mat$sd, y = Min_Ret_Mat$Asset_No, z = Min_Ret_Mat$er, colvar = Min_Ret_Mat$Gr_No, 
      pch = 16, cex = 1.5, xlab = "Standard Deviation", ylab = "Number Of Assets", 
     zlab = "Return", clab = c("Number of","Green Assets"),
     main = "GMVP Return vs SD and Green Number of Assets", ticktype = "detailed", 
       theta = 10, d = 2, 
      colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))
 image3D(x = seq(min(x), max(x), (max(x)-min(x))/10),y = seq(min(y), max(y), (max(y)-min(y))/10), z = RF, 
  col = "purple",alpha= 0.1, xlim = c(0,1), colkey = list(plot = FALSE), add=TRUE)}

SimonDocB<- addTitle( SimonDocB, value=paste("GMVP return and Risk Free Rate vs Standard Deviation and Total number of assets" ), level = 3 )   
SimonDocB<- addPlot( SimonDocB , fun = function() p44b.pryr)

p44c.pryr %<a-% {
scatter3D(x = Min_Ret_Mat$sd, y = Min_Ret_Mat$Asset_No, z = Min_Ret_Mat$er, colvar = Min_Ret_Mat$GW, col = ramp.col(c("red", "green")),
      pch = 16, cex = 1.5, xlab = "Standard Deviation", ylab = "Total Weight of Green Assets in Portfolio", 
     zlab = "Return", clab = c("Number of","Green Assets"),
     main = "GMVP Return vs SD and Total Green Weight of Portfolio", ticktype = "detailed", 
       theta = 10, d = 2, 
      colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))
 image3D(x = seq(min(x), max(x), (max(x)-min(x))/10),y = seq(min(y), max(y), (max(y)-min(y))/10), z = RF, 
  col = "purple", alpha= 0.1, xlim = c(0,1), colkey = list(plot = FALSE), add=TRUE)}

SimonDocB<- addPlot( SimonDocB , fun = function() p44c.pryr)

SimonDocB<- addParagraph(SimonDocB, value= "Based on the above, for the remainder of the calculations, only the portfolios with a 
GMVP return higher than the risk free rate are considered and used as inputs to the Black Litterman later on. " , stylename = "Normal" )
###======================================================================================================
###----SUbset for RF


#writeDoc(SimonDocB, file = 'k1511.docx')
#browseURL("k1511.docx")
#########=====choosing those with higher than RF rate Returns to input into tanget port

Min_Ret_Mat_1<- Min_Ret_Mat[(Min_Ret_Mat["er"]>RF),]
nrow(Min_Ret_Mat_1)

L_t<- list()
L_i<- list()
for (i in 1:length(Gmin.all)){
List_c<- list()
for (j in 1:length(Gmin.all[[i]])){

if (Gmin.all[[i]][[j]]$er >= RF){
List_c<- c(i,j)
}
L_i[[j]]<- List_c
}
L_t[[i]]<- L_i
}

Rf_App1<- do.call( rbind, unlist(L_t, recursive=FALSE) )

Rf_App<- as.matrix(Rf_App1[!duplicated(Rf_App1),])
##(Rf_App)[510,1]

tan.list<- list()
tan.list<- lapply(seq(from=1,to=nrow(Rf_App)), function(i) list(as.numeric(as.character(Rf_App[i,1])),as.numeric(as.character(Rf_App[i,2])),tangency.portfolio(UNIVERSE_BL[[as.numeric(as.character(Rf_App[i,1]))]][[as.numeric(as.character(Rf_App[i,2]))]][[2]], UNIVERSE_BL[[as.numeric(as.character(Rf_App[i,1]))]][[as.numeric(as.character(Rf_App[i,2]))]][[1]],RF,  shorts=FALSE)) )


###===========================================================================================================
##REPLICATING GMIN FOR TANGENCY PORTFOLIO

Gettans<- function(Y){
Min_Returns<- list()
Tan_R<- list()
X<- as.numeric(as.character(tan.list[[Y]][[1]]))
i<- as.numeric(as.character(tan.list[[Y]][[2]]))
Min_Returns<- (tan.list[[Y]][[3]])$er
Min_sd<- (tan.list[[Y]][[3]])$sd
Min_W<-(tan.list[[Y]][[3]])$weights
NG_min<- names(Min_W)
WG<- sum(Min_W[NG_min[NG_min %in% R_AG]])

Tan_R<- list(Min_Returns,Min_sd,WG, X, i)
return((Tan_R))
}

Tan_RR<- list()
Tan_RR<- lapply(seq(from=1,to=(length(tan.list) ) ) ,function(i) Gettans(i) )

###er<- Tan_RR[[Y]][[1]]
###sd<- Tan_RR[[Y]][[2]]
###T_WG<- Tan_RR[[Y]][[3]]
###T_X<- Tan_RR[[Y]][[4]]
###T_i<- Tan_RR[[Y]][[5]]



KAFKA<- list()
KAFKA<-lapply(seq(from=1,to=(length(Tan_RR) ) ), function(Y) data.frame( Tan_RR[[Y]][[4]]+1,as.numeric(as.character(Tan_RR[[Y]][[5]])),  N_G[[ as.numeric(as.character(Tan_RR[[Y]][[4]]))+1 ]][[as.numeric(as.character(Tan_RR[[Y]][[5]])) ]], Tan_RR[[Y]][[2]],Tan_RR[[Y]][[1]],Tan_RR[[Y]][[3]] ) )

#writeDoc(SimonDocB, file = '11.docx')
#browseURL("11.docx")

##BABANG<- (unlist(KAFKA, recursive=FALSE))

library(ggthemes)

Min_Ret_Mat_t<- data.frame(do.call(rbind, KAFKA))
colnames(Min_Ret_Mat_t)<- c("Asset_No","Variation_Number",  "t_Gr_No", "t_sd", "t_er", "t_GW")
head(Min_Ret_Mat_t)
Rf_mean<- rep(mean(New_RF), nrow(Min_Ret_Mat_t))
RF<- mean(New_RF)

#####SimonDocB<- docx()
g1<- subset(Min_Ret_Mat_t , Asset_No==length(Simon_L)  & Variation_Number==Jawab)

#df<- melt(Min_Ret_Mat_t, Min_Ret_Mat_t$Asset_No)
p<-  ggplot(Min_Ret_Mat_t, aes(x = Min_Ret_Mat_t$Asset_No, y=Min_Ret_Mat_t$t_er)) +
  geom_point(aes(colour = Min_Ret_Mat_t$t_Gr_No))+ scale_colour_gradient(low = "red", high = "green")+
  geom_hline(aes(yintercept=RF)) +xlab("Number of Portfolios")+geom_text(aes(0,RF,label = "Risk Free Rate", vjust = -1, hjust= -2.5))+
 ylab("GMVP Returns")+ 
ggtitle("Tangency Portfolio (TP) Returns Vs Total Number of Assets in Portfolio" , subtitle = NULL)+ 
labs(colour = "Green Assets No (TP)")+
geom_point(data=g1, aes(x=as.numeric(as.character(g1$Asset_No)), y=as.numeric(as.character(g1$t_er)))) +
geom_text(data=g1, aes( as.numeric( as.character(g1$Asset_No)), as.numeric(as.character(g1$t_er)) ,label = "Simon's Portfolio", vjust =0.1, hjust= -0.1))



p1<-  ggplot(Min_Ret_Mat_t, aes(x = Min_Ret_Mat_t$Asset_No, y=Min_Ret_Mat_t$t_er)) +
  geom_point(aes(colour = Min_Ret_Mat_t$t_GW))+ scale_colour_gradient(low = "red", high = "green")+
  geom_hline(aes(yintercept=RF)) +xlab("Number of Portfolios")+geom_text(aes(0,RF,label = "Risk Free Rate", vjust = -1, hjust= -2.5))+
 ylab("GMVP Returns")+ 
ggtitle("Tangency Portfolio (TP) Returns Vs Total Number of Assets in Portfolio" , subtitle = NULL)+ 
labs(colour = "Weight of green assets")+
geom_point(data=g1, aes(x=as.numeric(as.character(g1$Asset_No)), y=as.numeric(as.character(g1$t_er)))) +
geom_text(data=g1, aes( as.numeric( as.character(g1$Asset_No)), as.numeric(as.character(g1$t_er)) ,label = "Simon's Portfolio", vjust =0.1, hjust= -0.1))



SimonDocB<- addTitle( SimonDocB, value=paste("Calculating Tangency Portfolio (TP)) Attributes " ), level = 2 )   
SimonDocB<- addPlot( SimonDocB , fun=print, x=p) 
SimonDocB<- addPlot( SimonDocB , fun=print, x=p1) 

x= Min_Ret_Mat_t$t_sd
y= Min_Ret_Mat_t$Asset_No
z= Min_Ret_Mat_t$t_er

library("plot3D")

# save plotting parameters
  pm <- par("mfrow")
## 
## imag3D> ## =======================================================================
## imag3D> ## images in x, y, z plane
## imag3D> ## =======================================================================
## imag3D> 

p4a.pryr %<a-% {


scatter3D(x = Min_Ret_Mat_t$t_sd, y = Min_Ret_Mat_t$Asset_No, z = Min_Ret_Mat_t$t_er, colvar = Min_Ret_Mat_t$t_Gr_No, 
      pch = 16, cex = 1.5, xlab = "Standard Deviation", ylab = "Number Of Assets", 
     zlab = "Return", clab = c("Number of","Green Assets"),
     main = "Tangent Portfolio (TP) Return vs Standarard deviation and Green Number of Assets", ticktype = "detailed", 
       theta = 10, d = 2, 
      colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))}

SimonDocB<- addTitle( SimonDocB, value=paste("Tangent Portfolio (TP) return vs Standard Deviation and Total number of assets" ), level = 3 )   
SimonDocB<- addPlot( SimonDocB , fun = function() p4a.pryr)

p4b.pryr %<a-% {
scatter3D(x = Min_Ret_Mat_t$t_sd, y = Min_Ret_Mat_t$Asset_No, z = Min_Ret_Mat_t$t_er, colvar = Min_Ret_Mat_t$t_Gr_No, 
      pch = 16, cex = 1.5, xlab = "Standard Deviation", ylab = "Number Of Assets", 
     zlab = "Return", clab = c("Number of","Green Assets"),
     main = "Tangent Portfolio (TP) Return vs Total and Green Number of Assets", ticktype = "detailed", 
       theta = 10, d = 2,
      colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))
 image3D(x = seq(min(x), max(x), (max(x)-min(x))/10),y = seq(min(y), max(y), (max(y)-min(y))/10), z = RF, 
  col = "purple", xlim = c(0,1),alpha=0.1, colkey = list(plot = FALSE), add=TRUE)}

SimonDocB<- addTitle( SimonDocB, value=paste("Tangent Portfolio (TP) return and Risk Free Rate vs Standard Deviation and Total number of assets" ), level = 3 )   
SimonDocB<- addPlot( SimonDocB , fun = function() p4b.pryr)

p4c.pryr %<a-% {
scatter3D(x = Min_Ret_Mat_t$t_sd, y = Min_Ret_Mat_t$Asset_No, z = Min_Ret_Mat_t$t_er, col = ramp.col(c("red", "green")),
colvar = Min_Ret_Mat_t$t_GW, 
      pch = 16, cex = 1.5, xlab = "Standard Deviation", ylab = "Total Weight of Green Assets in Portfolio", 
     zlab = "Return", clab = c("Weight of","Green Assets"),
     main = "Tangent Portfolio (TP) Return vs Standard Deviation and Total Weight of Green Assets", ticktype = "detailed", 
       theta = 10, d = 2, 
      colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))
 image3D(x = seq(min(x), max(x), (max(x)-min(x))/10),y = seq(min(y), max(y), (max(y)-min(y))/10), z = RF, 
  col = "purple", xlim = c(0,1),alpha=0.1, colkey = list(plot = FALSE), add=TRUE)}

SimonDocB<- addPlot( SimonDocB , fun = function() p4c.pryr)


###==============================================
###Find Subset of Universe_Bl applicable to be BL
BL_U<- list()
BL_Select<- function(i){
Lala<- names(UNIVERSE_BL[[as.numeric(as.character(Rf_App[i,1]))]][[as.numeric(as.character(Rf_App[i,2]))]][[2]])
BL_U[[i]]<- list(UNIVERSE_BL[[as.numeric(as.character(Rf_App[i,1]))]][[as.numeric(as.character(Rf_App[i,2]))]], MC_ALL[Lala])
}
BL_Inp<- list()
BL_Inp<- lapply(seq(from=1, to=(nrow(Rf_App))), function(i) BL_Select(i))

####========APPENDIX

library(BLCOP)
Simon_Ri<- which(Rf_App[,1]==length(Simon_L)-1 & Rf_App[,2]== Jawab)

i=Simon_Ri

UNIVERSE_BL[[5]][[4]]

##BABA<- BL_Elnaz(509, Chngr, q1, conf1)

install.packages("OpenMx")
library(OpenMx)
M_w<- data.frame()
w_mrkt<- matrix()
M_w<- data.frame(BL_Inp[[i]][[2]]/rowSums(BL_Inp[[i]][[2]]))
w_mrkt<- (as.matrix(M_w)) #---MARKET WEIGHTS	

Tab1<- rbind(BL_Inp[[i]][[1]][[2]], BL_Inp[[i]][[2]], w_mrkt, t(sqrt(diag2vec(BL_Inp[[i]][[1]][[1]]))))
rownames(Tab1)<- c("Historic Returns","Market Value", "Market Weights", "Standard Deviations")


SimonDocB<- addTitle( SimonDocB, value=paste("Annual vs Monthly Inputs into the Black Litterman Optimization" ), level = 2 ) 


SimonDocB<- addTitle( SimonDocB, value=paste("Monthly: Historical Returns , Market Weights, Standard Deviations of Simon's Portoflio" ), level = 3 ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(Tab1,6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))
Tab2<- UNIVERSE_BL[[length(Simon_L)-1]][[Jawab]][[1]]

SimonDocB<- addTitle( SimonDocB, value=paste("Monthly: Covariance" ), level = 3 ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(Tab2,6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


Tab11<- rbind(BL_Inp[[i]][[1]][[2]], BL_Inp[[i]][[2]], w_mrkt, t(sqrt(diag2vec(BL_Inp[[i]][[1]][[1]]*12))))
rownames(Tab11)<- c("Historic Returns","Market Value", "Market Weights", "Standard Deviations")
Tab22<- BL_Inp[[i]][[1]][[1]]*12

SimonDocB<- addTitle( SimonDocB, value=paste("Annual: Historical Returns , Market Weights, Standard Deviations of Simon's Portoflio" ), level = 3 ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(Tab11,6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))
SimonDocB<- addTitle( SimonDocB, value=paste("Annual: Covariance" ), level = 3 ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(Tab22,6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


Tab23<- BL_Inp[[i]][[1]][[3]]

SimonDocB<- addTitle( SimonDocB, value=paste("Annual & Monthly: Correlations" ), level = 3 ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(Tab23,6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


V1BL<- BL_Inp[[i]][[1]][[2]][2]+BL_Inp[[i]][[1]][[2]][3]-BL_Inp[[i]][[1]][[2]][5]
V2BL<- BL_Inp[[i]][[1]][[2]][6]-BL_Inp[[i]][[1]][[2]][1]

Qmatt1<- data.frame(rbind(V1BL*12, V2BL*12))
rownames(Qmatt1)<- c(paste( "View 1=", colnames(BL_Inp[[i]][[1]][[2]])[2],"+", colnames(BL_Inp[[i]][[1]][[2]])[3],"-",colnames(BL_Inp[[i]][[1]][[2]])[5]), 
                      paste( "View 2=",colnames(BL_Inp[[i]][[1]][[2]])[6], "-", colnames(BL_Inp[[i]][[1]][[2]])[1]) )

colnames(Qmatt1)<- c("Historical Q Input, used as base for Black Litterman Views 1 and 2")


SimonDocB<- addTitle( SimonDocB, value=paste("View Base  Q Inputs" ), level = 3 ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(Qmatt1,6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


#-------------------------------------------------------------------------
#----Calculating Risk aversion from original Math to compare with variance
#-------------------------------------------------------------------------
Ex_R<- data.frame()

Rf_dat<- RF

##Ex_R <- BL_Inp[[i]][[1]][[2]]-(Rf_dat) #---Needed to Calculate 
# Cov of excess returns

UNIVERSE_BL[[length(Simon_L)-1]][[Jawab]][[2]]

cov<- (BL_Inp[[i]][[1]][[1]])

Er_cov.mat<- (BL_Inp[[i]][[1]][[1]])*12
cov.mat<- (BL_Inp[[i]][[1]][[1]])*12

er<- BL_Inp[[i]][[1]][[2]]
Er_mkt<- er%*%w_mrkt  ##===Market return, no equilibirum return, different


Ex_R_m <- er - RF ###-----Excess Return of each asset
Ex_R_mp <- Er_mkt - RF ###-----Excess Return of Market Portfolio

R_av<- 2.5
Re_Eq<- (as.double(R_av) * (Er_cov.mat %*% as.vector(w_mrkt))) #+  mean(Rf_dat[,1])) #--Equilbirum market returns

SimonDocB<- addTitle( SimonDocB, value=paste("Equilibrium Returns" ), level = 3 ) 

SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(Re_Eq),4)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))




#-----------------------------
#-----------------------------
Chngr_v1<- matrix()
Chngr_v2<-  matrix()
Chngr<-  list()


conf1<- matrix()
conf2<- matrix()
conf<- list()


q1<- matrix()
q2<- matrix()
q_a<-list()


BL_Elnaz<- function(i, Chngr, q, confidences, R_av, tau, w_mrkt){

M1<- match(unlist(Chngr[1][1]), rownames(cov.mat))
M2<- match(unlist(Chngr[2][1]), rownames(cov.mat))

P.mat_v1<-  c(0, -0.88, -0.12,0,1,0)
#####matrix(0L, nrow = 1  , ncol=nrow(cov.mat)) 
P.mat_v2<- c(-1,0,0,0,0,1)


P.mat<- (rbind(P.mat_v1, P.mat_v2))
tau<- 0.05

Pi<- R_av*Er_cov.mat%*%t(w_mrkt)
Er_BL_nv<- Pi ####+Rf_mean
 w_mrkt<- (w_mrkt)
SimonDocB<- addTitle( SimonDocB, value=paste("BL, User Conf, View_1", q, confidences ), level = 1 ) 
view_1<- BLViews(t(as.matrix(P.mat_v1)), q, confidences, assetNames=colnames(cov.mat))

Views_doc<- capture.output(view_1)

mypot <- pot( paste(Views_doc, collapse = "\n"), 
      format = textProperties(font.family = "Courier New", font.size = 9) )
SimonDocB <- addParagraph( SimonDocB, mypot,  par.properties = parLeft() )


Post_pi_v1<- posteriorEst(views=view_1, sigma=as.matrix(Er_cov.mat), mu= as.vector(Pi), tau, kappa=0)

        postMean1 <- data.frame(Post_pi_v1@posteriorMean)
        priorMean1 <- data.frame(Post_pi_v1@priorMean)
        postStDev1 <- data.frame(sqrt(diag(Post_pi_v1@posteriorCovar )))
        priorStDev1<- data.frame(sqrt(diag(Post_pi_v1@priorCovar)))
        Post_cov<- data.frame(Post_pi_v1@posteriorCovar)
        Prior_cov<- data.frame(Post_pi_v1@priorCovar)


        W_BL<- t(postMean1)%*%inv(R_av* Post_cov)
        W_BLT_SH<- tangency.portfolio(as.matrix(postMean1),as.matrix(Post_cov), RF, shorts=TRUE )
        W_BLT_L<- tangency.portfolio(as.matrix(postMean1),as.matrix(Post_cov), RF, shorts=FALSE )

####SIT_Post_pi_v1<- bl.compute.posterior(Pi, Er_cov.mat, pmat= t(as.matrix(P.mat_v1)), q, tau, confidences=conf)
SIT_Post_pi_NULL <- bl.compute.posterior(Pi, Er_cov.mat, NULL, NULL, tau, confidences=NULL)

Null_er<- SIT_Post_pi_NULL$expected.return
Null_cov<- SIT_Post_pi_NULL$cov

SIT_Post_pi_Omega1 <- bl.compute.posterior(Pi,Er_cov.mat, t(P.mat_v1), q, tau, confidences=NULL)
LOL<- list( postMean1,priorMean1, postStDev1,priorStDev1,Post_cov,Prior_cov)
LOL_NULL<- list(Null_er, Null_cov)
LOL_OM<- list(SIT_Post_pi_Omega1$expected.return, SIT_Post_pi_Omega1$cov)

SimonDocB<- addTitle( SimonDocB, value=paste("Posterior Mean"), level = 4  ) 

SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(postMean1),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

SimonDocB<- addTitle( SimonDocB, value=paste("Prior Mean"), level = 4  ) 

SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(priorMean1),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

SimonDocB<- addTitle( SimonDocB, value=paste("Posterior Covariance"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(Post_cov),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

SimonDocB<- addTitle( SimonDocB, value=paste("Prior Covariance"), level = 4  ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(Prior_cov),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


SimonDocB<- addTitle( SimonDocB, value=paste("BL Weights"), level = 4  ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(W_BL),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


SimonDocB<- addTitle( SimonDocB, value=paste("BL Weights, Short=TRUE"), level = 4  ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(W_BLT_SH$weights),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


SimonDocB<- addTitle( SimonDocB, value=paste("BL Weights, Short=FALSE"), level = 4  ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(W_BLT_L$weights),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))



SimonDocB<- addTitle( SimonDocB, value=paste("No Views" ), level = 3 ) 
SimonDocB<- addTitle( SimonDocB, value=paste("Null Covariance"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(Null_cov),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

SimonDocB<- addTitle( SimonDocB, value=paste("Null Estimated Returns"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(Null_er),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))





SimonDocB<- addTitle( SimonDocB, value=paste("Omega BL" ), level = 3 ) 

SimonDocB<- addTitle( SimonDocB, value=paste("Omega Covariance"), level = 4  ) 

SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(SIT_Post_pi_Omega1$cov),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


SimonDocB<- addTitle( SimonDocB, value=paste("Omega Returns"), level = 4  ) 

SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(SIT_Post_pi_Omega1$expected.return),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))



###-----------------VIEW 2

view_2<- BLViews(t(as.matrix(P.mat_v2)), q, confidences, assetNames=colnames(cov.mat))
Post_pi_v2<- posteriorEst(views=view_2, sigma=as.matrix(Er_cov.mat), mu= as.vector(Pi), tau, kappa=0)

SimonDocB<- addTitle( SimonDocB, value=paste("BL, User Conf, View_2", q, confidences ), level = 2 ) 
Views_doc2<- capture.output(view_2)

mypot2 <- pot( paste(Views_doc2, collapse = "\n"), 
      format = textProperties(font.family = "Courier New", font.size = 9) )
SimonDocB <- addParagraph( SimonDocB, mypot2,  par.properties = parLeft() )

        postMean1 <- data.frame(Post_pi_v2@posteriorMean)
        priorMean1 <- data.frame(Post_pi_v2@priorMean)
        postStDev1 <- data.frame(sqrt(diag(Post_pi_v2@posteriorCovar )))
        priorStDev1<- data.frame(sqrt(diag(Post_pi_v2@priorCovar)))
        Post_cov<- data.frame(Post_pi_v2@posteriorCovar)
        Prior_cov<- data.frame(Post_pi_v2@priorCovar)

        W_BL<- t(postMean1) %*%inv(R_av* Post_cov)
        W_BLT_SH<- tangency.portfolio(as.matrix(postMean1),as.matrix(Post_cov), RF, shorts=TRUE )
        W_BLT_L<- tangency.portfolio(as.matrix(postMean1),as.matrix(Post_cov), RF, shorts=FALSE )



####SIT_Post_pi_v1<- bl.compute.posterior(Pi, Er_cov.mat, pmat= t(as.matrix(P.mat_v2)), qmat=q_a, tau, confidences=conf)
SIT_Post_pi_NULL2 <- bl.compute.posterior(Pi, Er_cov.mat, NULL, NULL, tau, confidences=NULL)

Null2_er<- SIT_Post_pi_NULL2$expected.return
Null2_cov<- SIT_Post_pi_NULL2$cov

SIT_Post_pi_Omega2 <- bl.compute.posterior(Pi,Er_cov.mat, t(P.mat_v2), q=unlist(q_a)[[1]], tau, confidences=NULL)



LOL2<- list( postMean1,priorMean1, postStDev1,priorStDev1,Post_cov,Prior_cov)
LOL_NULL2<- list(Null2_er, Null2_cov)
LOL_OM2<- list(SIT_Post_pi_Omega2$expected.return, SIT_Post_pi_Omega2$cov)

SimonDocB<- addTitle( SimonDocB, value=paste("Posterior Mean"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(postMean1),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

SimonDocB<- addTitle( SimonDocB, value=paste("Prior Mean"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(priorMean1),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

SimonDocB<- addTitle( SimonDocB, value=paste("Posterior Covariance"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(Post_cov),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


SimonDocB<- addTitle( SimonDocB, value=paste("Prior Covariance"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(Prior_cov),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


SimonDocB<- addTitle( SimonDocB, value=paste("BL Weights"), level = 4  ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(W_BL),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


SimonDocB<- addTitle( SimonDocB, value=paste("BL Weights, Short=TRUE"), level = 4  ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(W_BLT_SH$weights),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


SimonDocB<- addTitle( SimonDocB, value=paste("BL Weights, Short=FALSE"), level = 4  ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(W_BLT_L$weights),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))




SimonDocB<- addTitle( SimonDocB, value=paste("No Views" ), level = 3 ) 

SimonDocB<- addTitle( SimonDocB, value=paste("Null Covariance"), level = 4  ) 

SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(Null2_cov),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

SimonDocB<- addTitle( SimonDocB, value=paste("Null returns"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(Null2_er),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))





SimonDocB<- addTitle( SimonDocB, value=paste("Omega BL View_2" ), level = 3 ) 
SimonDocB<- addTitle( SimonDocB, value=paste("Omega Covariance"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(SIT_Post_pi_Omega2$cov),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


SimonDocB<- addTitle( SimonDocB, value=paste("Omega Returns"), level = 4  ) 

SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(SIT_Post_pi_Omega2$expected.return),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))}






BL_Elnaz2<- function(i, Chngr, q, confidences, R_av, tau, w_mrkt){

M1<- match(unlist(Chngr[1][1]), rownames(cov.mat))
M2<- match(unlist(Chngr[2][1]), rownames(cov.mat))

P.mat_v1<-  c(0, -0.88, -0.12,0,1,0)
#####matrix(0L, nrow = 1  , ncol=nrow(cov.mat)) 
P.mat_v2<- c(-1,0,0,0,0,1)


P.mat<- (rbind(P.mat_v1, P.mat_v2))
tau<- 0.05

Pi<- R_av*Er_cov.mat%*%t(w_mrkt)
Er_BL_nv<- Pi ####+Rf_mean
 w_mrkt<- (w_mrkt)



###---------------------VIEW 3 





#view_3<- BLViews((as.matrix(rbind(P.mat_v1,P.mat_v2))), c(q,q), c(confidences,confidences), assetNames=colnames(cov.mat))
view_3<- BLViews((as.matrix(rbind(P.mat_v1,P.mat_v2))), q, confidences, assetNames=colnames(cov.mat))

##view_3<- BLViews((as.matrix(rbind(P.mat_v1,P.mat_v2))), as.numeric(q_2v[1,]), as.numeric(conf_2v[1,]), assetNames=colnames(cov.mat))

##tau= 0.05
Post_pi_v3<- posteriorEst(views=view_3, sigma=as.matrix(Er_cov.mat), mu= as.vector(Pi), tau, kappa=0)

SimonDocB<- addTitle( SimonDocB, value=paste("BL, User Conf, View_3: Expected Returns", c(q)[1], c(q)[2], "Confidences:", c(confidences)[1] ,c(confidences)[2]  ), level = 2 ) 
Views_doc3<- capture.output(view_3)

mypot3 <- pot( paste(Views_doc3, collapse = "\n"), 
      format = textProperties(font.family = "Courier New", font.size = 9) )
SimonDocB <- addParagraph( SimonDocB, mypot3,  par.properties = parLeft() )

        postMean1 <- data.frame(Post_pi_v3@posteriorMean)
        priorMean1 <- data.frame(Post_pi_v3@priorMean)
        postStDev1 <- data.frame(sqrt(diag(Post_pi_v3@posteriorCovar )))
        priorStDev1<- data.frame(sqrt(diag(Post_pi_v3@priorCovar)))
        Post_cov<- data.frame(Post_pi_v3@posteriorCovar)
        Prior_cov<- data.frame(Post_pi_v3@priorCovar)

        W_BL<- t(postMean1) %*% inv(R_av* Post_cov)
        W_BLT_SH<- tangency.portfolio(as.matrix(postMean1),as.matrix(Post_cov), RF, shorts=TRUE )
        W_BLT_L<- tangency.portfolio(as.matrix(postMean1),as.matrix(Post_cov), RF, shorts=FALSE )



####SIT_Post_pi_v1<- bl.compute.posterior(Pi, Er_cov.mat, pmat= t(as.matrix(P.mat_v2)), qmat=q_a, tau, confidences=conf)
SIT_Post_pi_NULL3 <- bl.compute.posterior(Pi, Er_cov.mat, NULL, NULL, tau, confidences=NULL)

Null3_er<- SIT_Post_pi_NULL3$expected.return
Null3_cov<- SIT_Post_pi_NULL3$cov

SIT_Post_pi_Omega3 <- bl.compute.posterior(Pi,Er_cov.mat, as.matrix(rbind(P.mat_v1,P.mat_v2)), q, tau, confidences=NULL)



LOL3<- list( postMean1,priorMean1, postStDev1,priorStDev1,Post_cov,Prior_cov)
LOL_NULL3<- list(Null3_er, Null3_cov)
LOL_OM3<- list(SIT_Post_pi_Omega3$expected.return, SIT_Post_pi_Omega3$cov)

SimonDocB<- addTitle( SimonDocB, value=paste("Posterior Mean"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(postMean1),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

SimonDocB<- addTitle( SimonDocB, value=paste("Prior Mean"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(priorMean1),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

SimonDocB<- addTitle( SimonDocB, value=paste("Posterior Covariance"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(Post_cov),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

SimonDocB<- addTitle( SimonDocB, value=paste("Prior Covariance"), level = 4  ) 



SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(Prior_cov),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


SimonDocB<- addTitle( SimonDocB, value=paste("BL Weights"), level = 4  ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(W_BL),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


SimonDocB<- addTitle( SimonDocB, value=paste("BL Weights, Short=TRUE"), level = 4  ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(W_BLT_SH$weights),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


SimonDocB<- addTitle( SimonDocB, value=paste("BL Weights, Short=FALSE"), level = 4  ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(W_BLT_L$weights),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))



SimonDocB<- addTitle( SimonDocB, value=paste("No Views" ), level = 3 ) 

SimonDocB<- addTitle( SimonDocB, value=paste("Null Covariance"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(Null3_cov),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

SimonDocB<- addTitle( SimonDocB, value=paste("Null Returns"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(Null3_er),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))



SimonDocB<- addTitle( SimonDocB, value=paste("Omega BL View_3" ), level = 3 ) 

SimonDocB<- addTitle( SimonDocB, value=paste("Omega Covariance"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(SIT_Post_pi_Omega3$cov),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

SimonDocB<- addTitle( SimonDocB, value=paste("Omega Returns"), level = 4  ) 


SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( data.frame(round(t(SIT_Post_pi_Omega3$expected.return),6)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))




return(list(LOL3, LOL_NULL3, LOL_OM3))} ###

###

Chngr_v1<- c("WORLD.DS.Market", "EMERGING.MARKETS.DS","MSCI.GLOBAL.ENVIRONMENT")
Chngr_v2<- c("JPM.GBI.GLOBAL", "S.P.Green")
Chngr<- list(Chngr_v1, Chngr_v2)

conf1<- c(0.25, 0.50, 0.75)
conf2<- c(0.25, 0.50, 0.75)
conf<- list(conf1, conf2)

q1<- c(0.0015, 0.0025, 0.0035)
q2<- c(0.0022, 0.0032, 0.0042)
q_a<- list(q1, q2)


q11<- c(0.0015+max(V1BL, V2BL), 0.0025+max(V1BL, V2BL), 0.0035+max(V1BL, V2BL))
q21<- c(0.0022+max(V1BL, V2BL), 0.0032+max(V1BL, V2BL), 0.0042+max(V1BL, V2BL))
q_a1<- list(q11, q21)


###q_2v<- combn(unlist(q_a), 2) 
###conf_2v<- combn(unlist(conf), 2) 


q_22v<-expand.grid(unlist(q_a), unlist(q_a))
q_2v<- cbind(q_22v[,1]+V1BL,q_22v[,2]+V2BL)


conf_2v<-expand.grid(unlist(conf), unlist(conf))


I_var<- function(X,Y){
HEHE<- list()
HEHE<- BL_Elnaz(Simon_Ri, Chngr, unlist(q_a1)[X], conf[2][[1]][[Y]], R_av=2.5, tau=0.05, w_mrkt)}


#I_var2<- function(X,Y){
I_var2<- function(X){
HEHE_2v<- list
#HEHE_2v<- BL_Elnaz2(Simon_Ri, Chngr, as.numeric(q_2v[X,]), as.numeric(conf_2v[Y,]) , R_av=2.5, tau=0.05, w_mrkt)}
HEHE_2v1<- BL_Elnaz2(Simon_Ri, Chngr, as.numeric(q_2v[X,]), as.numeric(conf_2v[1,]) , R_av=2.5, tau=0.05, w_mrkt)
HEHE_2v11<- BL_Elnaz2(Simon_Ri, Chngr, as.numeric(q_2v[X,]), as.numeric(conf_2v[11,]) , R_av=2.5, tau=0.05, w_mrkt)
HEHE_2v36<- BL_Elnaz2(Simon_Ri, Chngr, as.numeric(q_2v[X,]), as.numeric(conf_2v[36,]) , R_av=2.5, tau=0.05, w_mrkt)

}


##view_3<- BLViews((as.matrix(rbind(P.mat_v1,P.mat_v2))), as.numeric(q_2v[1,]), as.numeric(conf_2v[1,]), assetNames=colnames(cov.mat))

Q_Var<- function(Y){
Qvar_List<- list()
Qvar_List<- lapply(seq(from=1, to=length(unlist(q_a))), function(i) I_var(i, Y))} 



#Q_Var2<- function(Y){
#Qvar_List2<- list()



Conf_List<- list()
#Conf_List2<- list()

Conf_List<- lapply(seq(from=1, to=length(conf[2][[1]])), function(i) Q_Var(i))
#Conf_List2<- lapply(seq(from=1, to=nrow(conf_2v)), function(i) Q_Var2(i))
Qvar_List2<- lapply(seq(from=1, to=nrow(q_2v)), function(i) I_var2(i))#} 

####--------------------------------------------------------------------------------------------
####--------------------------------------------------------------------------------------------
####--------------------------------------------------------------------------------------------
####--------------------------------------------------------------------------------------------
####--------------------------------------------------------------------------------------------
####--------------------------------------------------------------------------------------------

writeDoc(SimonDocB, file = "BL654.docx")
browseURL("BL654.docx")



#####-------selecting portfolios with the two changers included for response checking
####all(x %in% y) should do what you want:

BL_C<- list()
for (i in 1:nrow(Rf_App)){
y<- Ccols[[as.numeric(as.character(Rf_App[i,1]))+1]][ ,as.numeric(as.character(Rf_App[i,2]))]

if( (all(Chngr %in% y))== TRUE ){
 Cmprs<- c(as.numeric(as.character(Rf_App[i,1]))+1, Rf_App[i,2])
}
else if ((all(Chngr %in% y))== FALSE) {
  Cmprs<- c()
}
BL_C[[i]]<- Cmprs
}

Simon_Dat<- data.frame(do.call( rbind, BL_C))

Counter<- data.frame(Min_Ret_Mat_t[,1:2], rownames(Min_Ret_Mat_t[,1:2]))
colnames(Counter)<- c("No_Assets", "No_Variation", "Portfolio_Number")
colnames(Simon_Dat)<- c("No_Assets", "No_Variation")

Finally<- as.numeric(as.character(merge(Simon_Dat, Counter, by=c("No_Assets","No_Variation"))[,3]))
Delta_i<- function(X,Y,Z,W){
Diffa<- list()
Diffa<- (Conf_List[[X]][[Y]][[ Finally[Z] ]][[W]][[1]]["FTSE4GOOD.GLOBAL.",1]- Conf_List[[X]][[Y]][[Finally[Z]]][[W]][[1]]["WORLD.DS.Market",1])
return(Diffa)
}
#Delta_BL<- list()
#Delta_BL<- list(mapply(Delta_i, conf_all, q_all, Finally,c(1,2,3) ))

f1<- function(X,Y,Z){
Delta_BL1<- lapply(seq(from=1, to=3), function(i) Delta_i(X,Y,Z,i))}

f2<- function(X,Y){
Delta_BL2<- lapply(seq(from=1, to=length(Finally)), function(i) f1(X,Y,i))}


f3<- function(X){
Delta_BL3<- lapply(seq(from=1, to=length(q_all)), function(i) f2(X,i))}


Delta_BL<-  lapply(seq(from=1, to=length(conf_all)), function(i) f3(i))

babynd<- function(X,Y,Z,W){
VAVOM<- list()
kibi<- data.frame(X, Y, Z, W, Delta_BL[[X ]][[Y ]][[Z ]][[W ]],Finally[[Z]] )
colnames(kibi)<- c("Conf", "Q", "Sol_Number", "BL_type", "Delta","Portfolio_No")
rownames(kibi)<- c("Results")
VAVOM<- kibi}

Twix1<- list()
f11<- function(X,Y,Z){
Twix1<- lapply(seq(from=1, to=3), function(i) babynd(X,Y,Z,i))}

Twix2<- list()
f12<- function(X,Y){
Twix2<- lapply(seq(from=1, to=length(Finally)), function(i) f11(X,Y,i))}

Twix3<- list()
f13<- function(X){
Twix3<- lapply(seq(from=1, to=length(q_all)), function(i) f12(X,i))}

Twix<- list()
Twix<-  lapply(seq(from=1, to=length(conf_all)), function(i) f13(i))

T11<- list()
T11<- unlist(Twix,recursive=FALSE)

list <- unlist(T11, recursive = FALSE)
list2 <- unlist(list, recursive = FALSE)
Mozard_1<- data.frame()
Mozart_1 <- do.call("rbind", list2)

colnames(Mozart_1)<- c("Conf", "Q", "Sol_Number", "BL_type", "Delta", "Portfolio_No")
SimonDocB<-  addParagraph(SimonDocB, value= "Having obtained the range of portfolios inputted into the Black Litterman Model, our inputted views are defined as 
FTSE4GOOD.GlOBAL outperforming WORLD.DS.Market by a set defined Delta. The Black litterman is applied in 3 ways, User defined confidences, Omega 
caluclated, and no views applied. The range of Deltas and Confidences are defined as",stylename = "Normal")

SimonDocB<- addTitle( SimonDocB, value=paste("Range of Deltas" ), level = 3 ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( round( as.matrix(t(q_all)),4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ,
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) )))

SimonDocB<- addTitle( SimonDocB, value=paste("Range of Confidences" ), level = 3 ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( round( as.matrix(t(conf_all)) ,4) , header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ,
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) )))

SimonDocB<- addTitle( SimonDocB, value=paste("Methodology" ), level = 3 ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable(  as.matrix(c("User Defined", "Omega", "NULL views")), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

SimonDocB<-  addParagraph( SimonDocB , value= "The Variations are inputted into the Black Litteman function and posterior mean and covariances calculated.
 This will then be inputted back into the Tangent portfolio as defined previously, but first the differences in the 
returns of FTS4GOOD and WORLD.DS.Market are looked at prior to optimization of the portfolios. 
The first few rows of the resulting dataframe of Combinations and Posterior Delta is as follows", stylename = "Normal" )

SimonDocB<- addTitle( SimonDocB, value=paste("Delta List: Black Litterman Posterior Delta" ), level = 3 )     
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( round(head(Mozart_1),4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))



#Baghlava <- data.frame(cbind(Twix, Simon_Dat, Finally))
#colnames(Baghlava)<- c("Conf", "Q", "Sol_Number", "BL_type", "Delta","No_Assets","No_Variation", "Portfolio_Number")

x=as.numeric(as.character(Mozart_1$Conf))
y=as.numeric(as.character(Mozart_1$Q))
z=as.numeric(as.character(Mozart_1$Delta))
k= as.numeric(as.character(unlist(Mozart_1$BL_type)))

p444c.pryr %<a-% {with(Mozart_1, scatter3D(x = as.numeric(as.character(Mozart_1$Conf)),
 y = as.numeric(as.character(Mozart_1$Q)), 
   z = as.numeric(as.character(Mozart_1$Delta)), 
  col = c("blue", "purple", "red"), plotdev(lighting = TRUE, alpha = 0.2, theta = 0), pch = 16, cex = 1,  
 ticktype = "detailed", phi = 20,
   xlab = "Confidence x 25%", ylab = "Input Return Difference 0.x% ", 
   zlab = "Output Return Difference",  main = "Comparison of {FTS4GOOD-MSCI} Across Solutions",
    colkey = list(at = c(1.33, 2, 2.66), side = 4, 
    addlines = TRUE, length = 1, width = 1,
    labels = c("User-Def", "Omega", "NULL") )))}

#image3D(x = seq(min(x), max(x), (max(x)-min(x))/10),y = seq(min(y), max(y), (max(y)-min(y))/10), z = 0.001, 
 # col = gg.col(length(100) + 1, alpha = 0.1), colkey = list(plot = FALSE), add=TRUE)
#image3D(x = seq(min(x), max(x), (max(x)-min(x))/10),y = seq(min(y), max(y), (max(y)-min(y))/10), z = 0.002, 
  #col = gg.col(length(200) + 1, alpha = 0.1), colkey = list(plot = FALSE), add=TRUE)
#image3D(x = seq(min(x), max(x), (max(x)-min(x))/10),y = seq(min(y), max(y), (max(y)-min(y))/10), z = 0.003, 
  #col = gg.col(length(300) + 1, alpha = 0.1), colkey = list(plot = FALSE), add=TRUE)

SimonDocB<- addTitle( SimonDocB, value=paste("Black Litterman Posterior Delta" ), level = 3 )   
SimonDocB<- addParagraph(SimonDocB, "The Variation of the Delta across Confidences, methodology, and input Delta Ranges can be examined
via plotting the results in a 3 dimensional space of x= Confidences, y= input delta range, and z= output delta range. The Points represent the
Black Litterman Solution of the portfolios, the total number of which after excluding the portfolios with a GMVP under the risk free rate and 
those not including the two FTS4GOOD and WORLD.DS.Market indices, is 132. The number of BL solutions with the variations aboveis 3563, the results
of which are shown bellow: ")

  SimonDocB<- addPlot( SimonDocB , fun = function() p444c.pryr)
SimonDocB<- addTitle( SimonDocB, value=paste("Appendix A: Global Minimum Variance Portfolio Table" ), level = 2 ) 
SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( round(Min_Ret_Mat_t,4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

SimonDocB<- addFlexTable(  SimonDocB,
              (FlexTable( round(Min_Ret_Mat,4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))
writeDoc(SimonDocB, file = 'tetas3.docx')
browseURL("tetas3.docx")






xlim = c(0,1), 

library("plot3Drgl")
plotrgl() 

Finally_T1<- Counter[Counter$Portfolio_Number %in% Finally,]
Min_Ret_Mat_t$Portfolio_Number<- rownames(Min_Ret_Mat_t)
Finally_T2<- Min_Ret_Mat_t[Min_Ret_Mat_t$Portfolio_Number %in% Finally,]

DD<- do.call(cbind, Delta_BL)
nrow(DD)
Delta_D<- data.frame(cbind(Simon_Dat, Finally, as.numeric(DD[,1])))
colnames(Delta_D)<- c("No_Assets", "Variation_No", "Portfolio_No", "Index_Delta")

p<-  ggplot(Delta_D, 
aes(x = Delta_D$Variation_No, y=Delta_D$Index_Delta)) +
  geom_point(aes(colour = Delta_D$No_Assets))+ 
  geom_hline(aes(yintercept=0.001)) +
geom_hline(aes(yintercept=0.002)) +
geom_hline(aes(yintercept=0.003)) 



+geom_text(aes(0,RF,label = "Risk Free Rate", vjust = -1, hjust= -2.5))+
 ylab("GMVP Returns")+ 
ggtitle("Tangency Portfolio (TP) Returns Vs Total Number of Assets in Portfolio" , subtitle = NULL)+ 
labs(colour = "Number of Green Assets in Tangency Portfolio (TP)")+
theme_tufte()



tan.list<- list()
tan.list<- lapply(seq(from=1,to=nrow(Rf_App)), function(i) list(as.numeric(as.character(Rf_App[i,1])),as.numeric(as.character(Rf_App[i,2])),tangency.portfolio(UNIVERSE_BL[[as.numeric(as.character(Rf_App[i,1]))]][[as.numeric(as.character(Rf_App[i,2]))]][[2]], UNIVERSE_BL[[as.numeric(as.character(Rf_App[i,1]))]][[as.numeric(as.character(Rf_App[i,2]))]][[1]],RF,  shorts=FALSE)) )





##-------------------------------------------------------------------
##-------------------------------------------------------------------

##-----------------------------------------------------------------------------------------------------------------
###-----COMPARISON OF PI AND MU AS INPUT TO PRIOR FOR INDIVIDUAL VIEWS: using prior calculated from average returns
##-----------------------------------------------------------------------------------------------------------------


SimonDoc22 <- docx( ) #%>% 

for (i in 1:nrow(q.mat)) {
    for (j in 1:nrow(conf.mat)){

view_1<- BLViews(P=P.mat1, q=q.mat[i], confidences=conf.mat[j], assetNames=colnames(cov.mat))
Post_pi_v1<- posteriorEst(views=view_1, sigma=as.matrix(Er_cov.mat), mu= as.vector(Pi), tau=0.025, kappa=conf.mat[j])

        postMean1 <- data.frame(Post_pi_v1@posteriorMean)
        priorMean1 <- data.frame(Post_pi_v1@priorMean)
        postStDev1 <- data.frame(sqrt(diag(Post_pi_v1@posteriorCovar )))
        priorStDev1<- data.frame(sqrt(diag(Post_pi_v1@priorCovar)))
        Post_cov<- data.frame(Post_pi_v1@posteriorCovar)
        Prior_cov<- data.frame(Post_pi_v1@priorCovar)


##Tab_all <- capture.output(print(Post_pi_v1))
addTitle( SimonDoc22, value=paste("Prior vs Posterior Mean Distribution: WOLRD.DS.Market Underperfroms FTS4GOOD by", q.mat[i], "Return under" , conf.mat[j], "Confidence" ), level = 1 )%>%  
addTitle( SimonDoc22, value=paste("Posterior Returns" ), level = 2 )%>%  

addFlexTable(  postMean1 %>%
               FlexTable( header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textBold( color = "white" ),
                            add.rownames = TRUE ) %>%
              setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%


addTitle( SimonDoc22, value=paste("Prior Returns" ), level = 2 )%>%  
addFlexTable(    priorMean1 %>%
               FlexTable( header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textBold( color = "white" ),
                            add.rownames = TRUE ) %>%
              setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) 

addTitle( SimonDoc22, value=paste("Posterior Standard deviation" ), level = 2 )%>%  
addFlexTable(  postStDev1 %>%
               FlexTable( header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textBold( color = "white" ),
                            add.rownames = TRUE ) %>%
              setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) 

addTitle( SimonDoc22, value=paste("Prior Standard deviation" ), level = 2 )%>%  
addFlexTable(  priorStDev1 %>%
               FlexTable( header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textBold( color = "white" ),
                            add.rownames = TRUE ) %>%
              setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%

addTitle( SimonDoc22, value=paste("Prior Covariance" ), level = 2 )%>%  
addFlexTable(  priorStDev1 %>%
               FlexTable( header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textBold( color = "white" ),
                            add.rownames = TRUE ) %>%
              setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) 


addTitle( SimonDoc22, value=paste("Posterior Covariance" ), level = 2 )%>%  
addFlexTable(  Post_cov %>%
               FlexTable( header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textBold( color = "white" ),
                            add.rownames = TRUE ) %>%
              setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) 


}
}

writeDoc(SimonDoc22, file = 'Prior-and-Posterior.docx')
# open the Word document
browseURL("Prior-and-Posterior.docx")


###----for all DAT


Post_pi_v1<- posteriorEst(views=view_1, sigma=as.matrix(Er_cov.mat), mu= as.vector(Pi), tau=0.025, kappa=conf.mat[1])
#Post_CAPM_v1<- posteriorEst(views=view_1, sigma=as.matrix(Er_cov.mat), mu= as.vector(Re_Eq), tau=0.025, kappa=conf.mat[1])
Post_Er_v1<- posteriorEst(views=view_1, sigma=as.matrix(Er_cov.mat), mu= as.vector(er), tau=0.025, kappa=conf.mat[1])

tau<- 0.025

SIT_Post_pi_v1<- bl.compute.posterior(mu= as.vector(Pi), cov=Er_cov.mat, pmat= P.mat1, q.mat[i], tau, confidences=conf.mat[1])



par(mfrow=c(2,2)) 
par(oma=c(0,0,2,0))
densityPlots(Post_pi_v1, "WORLD.DS.Market" , main="Equilibrium returns as prior")
#densityPlots(Post_CAPM_v1, "WOLRD.DS.Market", main="CAPM as Prior")
densityPlots(Post_Er_v1, "WORLD.DS.Market", main="Historical Returns as Prior")

densityPlots(Post_pi_v1, "FTSE4GOOD.GLOBAL.", main="Equilibrium returns as prior")
#densityPlots(Post_CAPM_v1, "FTSE4GOOD.GLOBAL.", main="CAPM as Prior")
densityPlots(Post_Er_v1, "FTSE4GOOD.GLOBAL.", main="Historical Returns as Prior")

title("Prior vs Posterior Mean Distribution: WOLRD.DS.Market Underperfroms FTS4GOOD by 0.1% with 25% Confidence", outer=TRUE, size=1, cex.main=1)

##----------------ALL NOT NEEDED
#Post_pi_v2<- posteriorEst(views=view_2, sigma=as.matrix(Er_cov.mat), mu= as.vector(Pi), tau=0.025, kappa=conf.mat[2])
#Post_CAPM_v2<- posteriorEst(views=view_2, sigma=as.matrix(Er_cov.mat), mu= as.vector(Re_Eq), tau=0.025, kappa=conf.mat[2])
#Post_Er_v2<- posteriorEst(views=view_2, sigma=as.matrix(Er_cov.mat), mu= as.vector(er), tau=0.025, kappa=conf.mat[2])

##par(mfrow=c(2,1)) 

###densityPlots(Post_pi_v2, "Goldman.Sachs.Commodity", main="Equilibrium returns as prior")
###densityPlots(Post_CAPM_v2, "Goldman.Sachs.Commodity", main="CAPM as Prior")
###densityPlots(Post_Er_v2, "Goldman.Sachs.Commodity", main="Historical Returns as Prior")
###title("Prior vs Posterior Black-Litterman Mean Distribution: GS Commodity decrease by 1%", outer=T RUE, cex.main=1)


####Post_pi_v3<- posteriorEst(views=view_3, sigma=as.matrix(Er_cov.mat), mu= as.vector(Pi), tau=0.025, kappa=conf.mat[3])
####Post_CAPM_v3<- posteriorEst(views=view_3, sigma=as.matrix(Er_cov.mat), mu= as.vector(Re_Eq), tau=0.025, kappa=conf.mat[3])
####Post_Er_v3<- posteriorEst(views=view_3, sigma=as.matrix(Er_cov.mat), mu= as.vector(er), tau=0.025, kappa=conf.mat[3])

####par(mfrow=c(2,1)) 

####densityPlots(Post_pi_v3, "MSCI.World.Real.Estate", main="Equilibrium returns as prior")
####densityPlots(Post_CAPM_v3, "MSCI.World.Real.Estate", main="CAPM as Prior")
####densityPlots(Post_Er_v3, "MSCI.World.Real.Estate", main="Historical Returns as Prior")
#####title("Prior vs Posterior Black-Litterman Mean Distribution: MSCI World increase by 1%", outer=TRUE)


#####Post_pi_v4<- posteriorEst(views=view_4, sigma=as.matrix(Er_cov.mat), mu= as.vector(Pi), tau=0.025, kappa=conf.mat[4])
#####Post_CAPM_v4<- posteriorEst(views=view_4, sigma=as.matrix(Er_cov.mat), mu= as.vector(Re_Eq), tau=0.025, kappa=conf.mat[4])
#####Post_Er_v4<- posteriorEst(views=view_4, sigma=as.matrix(Er_cov.mat), mu= as.vector(er), tau=0.025, kappa=conf.mat[4])

#####par(mfrow=c(2,1)) 
#####densityPlots(Post_pi_v4, "MSCI.EM", main="Equilibrium returns as prior")
#####densityPlots(Post_CAPM_v4, "MSCI.EM", main="CAPM as Prior")
#####densityPlots(Post_Er_v4, "MSCI.EM", main="Historical Returns as Prior")
#####title("Prior vs Posterior Black-Litterman Mean Distribution: MSCI.EM outperforms MSCI.World by 3%", outer=TRUE)


#####Post_pi_v5<- posteriorEst(views=view_5, sigma=as.matrix(Er_cov.mat), mu= as.vector(Pi), tau=0.025, kappa=conf.mat[5])
#####Post_CAPM_v5<- posteriorEst(views=view_5, sigma=as.matrix(Er_cov.mat), mu= as.vector(Re_Eq), tau=0.025, kappa=conf.mat[5])
#####Post_Er_v5<- posteriorEst(views=view_5, sigma=as.matrix(Er_cov.mat), mu= as.vector(er), tau=0.025, kappa=conf.mat[5])

#####par(mfrow=c(2,1)) 
#####densityPlots(Post_pi_v5, "JP.Morgan.Global.Aggregate.Bonds", main="Equilibrium returns as prior")
#####densityPlots(Post_CAPM_v5, "JP.Morgan.Global.Aggregate.Bonds", main="CAPM as Prior")
#####densityPlots(Post_Er_v5, "JP.Morgan.Global.Aggregate.Bonds", main="Historical Returns as Prior")
#####title("Prior vs Posterior Black-Litterman Mean Distribution: MSCI.World.Real.Estate outperforms JP.Morgan.Global.Aggregate.Bonds by 1%", outer=TRUE)

##-----------------------------------------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------------------------------------

##-----------------------------------------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------------------------------------
###-----Plot of tau Comparison for view 1, DENSITY PLOTS FOR DIFFERENT TAUS AT CONFIDENCE FOR VIEW 1:
##-----------------------------------------------------------------------------------------------------------------

Post_pi_v1_t1<- posteriorEst(views=view_1, sigma=as.matrix(Er_cov.mat), mu= as.vector(Pi), tau=0.25, kappa=conf.mat[1])
Post_pi_v1_t2<- posteriorEst(views=view_1, sigma=as.matrix(Er_cov.mat), mu= as.vector(Pi), tau=0.55, kappa=conf.mat[1])
Post_pi_v1_t3<- posteriorEst(views=view_1, sigma=as.matrix(Er_cov.mat), mu= as.vector(Pi), tau=0.75, kappa=conf.mat[1])
Post_pi_v1_t4<- posteriorEst(views=view_1, sigma=as.matrix(Er_cov.mat), mu= as.vector(Pi), tau=1, kappa=conf.mat[1])


#densityPlots(Post_pi_v3, "JP.Morgan.Global.Aggregate.Bonds", main="Equilibrium returns as prior")
#par(new=TRUE)
#densityPlots(Post_pi_v2, "JP.Morgan.Global.Aggregate.Bonds", main="Equilibrium returns as prior")
#par(new=TRUE)
#densityPlots(Post_pi_v1, "JP.Morgan.Global.Aggregate.Bonds", main="Equilibrium returns as prior")

assetsSel = seq(along = Post_pi_v1_t1@views@assets)
      postMean1 <- Post_pi_v1_t1@posteriorMean[assetsSel[5]] 
        priorMean1 <- Post_pi_v1_t1@priorMean[assetsSel[5]]
        postStDev1 <- sqrt(Post_pi_v1_t1@posteriorCovar[assetsSel[5],assetsSel[5]] )
        priorStDev1<- sqrt(Post_pi_v1_t1@priorCovar[assetsSel[5],assetsSel[5]])
      
        plotDispersion1 <- max(postStDev1, priorStDev1)
        x1 <- seq(from = min(priorMean1,postMean1) - 2.5 * abs(plotDispersion1), to = max(priorMean1,postMean1) + 2.5 * abs(plotDispersion1), length = 200)
        xLabel1 <- if(is.character(assetsSel)) assetsSel[5] else Post_pi_v1_t1@views@assets[5]
        
      postMean2 <- Post_pi_v1_t2@posteriorMean[assetsSel[5]] 
        priorMean2 <- Post_pi_v1_t2@priorMean[assetsSel[5]]
        postStDev2 <- sqrt(Post_pi_v1_t2@posteriorCovar[assetsSel[5],assetsSel[5]] )
        priorStDev2<- sqrt(Post_pi_v1_t2@priorCovar[assetsSel[5],assetsSel[5]])
      
        plotDispersion2 <- max(postStDev2, priorStDev2)
        x2 <- seq(from = min(priorMean2,postMean2) - 2.5 * abs(plotDispersion2), to = max(priorMean2,postMean2) + 2.5 * abs(plotDispersion2), length = 200)
        xLabel2 <- if(is.character(assetsSel)) assetsSel[5] else Post_pi_v1_t1@views@assets[5]


      postMean3 <- Post_pi_v1_t3@posteriorMean[assetsSel[5]] 
        priorMean3 <- Post_pi_v1_t3@priorMean[assetsSel[5]]
        postStDev3 <- sqrt(Post_pi_v1_t3@posteriorCovar[assetsSel[5],assetsSel[5]] )
        priorStDev3<- sqrt(Post_pi_v1_t3@priorCovar[assetsSel[5],assetsSel[5]])
      
        plotDispersion3 <- max(postStDev3, priorStDev3)
        x3 <- seq(from = min(priorMean3,postMean3) - 2.5 * abs(plotDispersion3), to = max(priorMean3,postMean3) + 2.5 * abs(plotDispersion3), length = 200)
        xLabel3 <- if(is.character(assetsSel)) assetsSel[5] else Post_pi_v1_t1@views@assets[5]



      postMean4 <- Post_pi_v1_t4@posteriorMean[assetsSel[5]] 
        priorMean4 <- Post_pi_v1_t4@priorMean[assetsSel[5]]
        postStDev4 <- sqrt(Post_pi_v1_t4@posteriorCovar[assetsSel[5],assetsSel[5]] )
        priorStDev4 <- sqrt(Post_pi_v1_t4@priorCovar[assetsSel[5],assetsSel[5]])
      
        plotDispersion4 <- max(postStDev4, priorStDev4)
        x4 <- seq(from = min(priorMean4,postMean4) - 2.5 * abs(plotDispersion4), to = max(priorMean4,postMean4) + 2.5 * abs(plotDispersion4), length = 200)
        xLabel4 <- if(is.character(assetsSel)) assetsSel[5] else Post_pi_v1_t1@views@assets[5]

ND_1<- dnorm(x1, mean = postMean1, sd = postStDev1)
ND_2<- dnorm(x2, mean = postMean2, sd = postStDev2)
ND_3<- dnorm(x3, mean = postMean3, sd = postStDev3)
ND_4<- dnorm(x4, mean = postMean4, sd = postStDev4)
ND_P<- dnorm(x1, mean = priorMean1,sd = priorStDev1)

DGMS <- data.frame(ND_1,ND_2,ND_3, ND_4, ND_P)

plot(x1, dnorm(x1, mean = priorMean1, sd = priorStDev1), col = "black", type = "l", ylab = "Density", xlab = "xLabel")

            abline(v = priorMean1, lty = 2, col = "black")
            lines(x1, dnorm(x1, mean = postMean1, sd = postStDev1), col = "blue", type = "l")
            abline(v = postMean1, lty = 2, col = "blue")
            lines(x2, dnorm(x2, mean = postMean2, sd = postStDev2), col = "pink", type = "l")
            abline(v = postMean2, lty = 2, col = "pink")
            lines(x3, dnorm(x3, mean = postMean3, sd = postStDev3), col = "green", type = "l")
            abline(v = postMean3, lty = 2, col = "green")
             lines(x4, dnorm(x4, mean = postMean4, sd = postStDev4), col = "purple", type = "l")
            abline(v = postMean4, lty = 2, col = "purple")
            legend("topright", legend = c("Equilibrium Mean", "tau=0.25", "tau=0.55", "tau=0.75", "tau=1"), lty = c(1,1), col = c("black", "blue", "pink", "green", "purple"))
title("Distribution of MSCI World subject to view A at Varied Tau")

data<- melt(DGMS)
ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.25)
ggplot(data,aes(x=value, fill=variable)) + geom_histogram(alpha=0.25)
ggplot(data,aes(x=variable, y=value, fill=variable)) + geom_boxplot()

##-----------------------------------------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------------------------------------
###---------------------------------------------------------------------
###----Adding on more views, checking whether nonlinearity exists or not:
###---------------------------------------------------------------------
##-----------------------------------------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------------------------------------

 
views_2 <- addBLViews(P.mat2, q=qmat[2], conf.mat[2] , view_1)
views_3 <- addBLViews(P.mat3, q=qmat[3], conf.mat[3] , views_2)
views_4 <- addBLViews(P.mat4, q=qmat[4], conf.mat[4] , views_3)
views_5 <- addBLViews(P.mat5, q=qmat[5], conf.mat[5] , views_4)

Post_pi_vAB<- posteriorEst(views_2, sigma=as.matrix(cov.mat), mu= as.vector(Pi), tau=0.025)
Post_pi_vABC<- posteriorEst(views_3, sigma=as.matrix(cov.mat), mu= as.vector(Pi), tau=0.025)
Post_pi_vABCD<- posteriorEst(views_4, sigma=as.matrix(cov.mat), mu= as.vector(Pi), tau=0.025)
Post_pi_vABCDE<- posteriorEst(views_5, sigma=as.matrix(cov.mat), mu= as.vector(Pi), tau=0.025)

###-----------Plot Desnity Distributions
par(mfrow=c(2,4))
par(oma=c(1,1,1,1))
densityPlots(Post_pi_v2, "MSCI.World", main="View B")
densityPlots(Post_pi_v3, "MSCI.World", main="View C")
densityPlots(Post_pi_v4, "MSCI.World", main="View D")
densityPlots(Post_pi_v5, "MSCI.World", main="ViewE")
densityPlots(Post_pi_vAB, "MSCI.World", main="View A,B ")
densityPlots(Post_pi_vABC,"MSCI.World", main="View A,B,C")
densityPlots(Post_pi_vABCD, "MSCI.World", main="View A,B,C,D")
densityPlots(Post_pi_vABCDE, "MSCI.World", main="View A,B,C,D,E")

title( "Comparison of View Implementation Sequential vs Cumulative", outer=TRUE)
###----------------------------------------------------------------------------------------------------
###---------------------------------------------------------------------------------------------------

monthlyReturns<- Re_dat
priorMeans<- er
priorVarcov<-cov.mat

####----------------GET TANGENCY PORTFOLIO (ERIC VERSION)


#-----------------------------------------------------------
 library(fportfolio)
setClass("fPFOLIOSPEC",
 representation(
  model = "list",
  portfolio = "list",
  optim = "list") )




 portfolioSpec(
   model = list(
     type = "MV", optimize = "minRisk",
     estimator = "covEstimator", tailRisk = list(),
     params = list(alpha = 0.05, a = 1)),
   portfolio = list(
     weights = NULL, targetReturn = NULL,
     targetRisk = NULL, riskFreeRate = Rf_mean, nFrontierPoints = 200,
     status = NA),
     optim = list(
     solver = "solveRquadprog",
     objective = c("portfolioObjective", "portfolioReturn", "portfolioRisk"),
     options = list(meq = 2), control = list(), trace = FALSE),
   messages = list(
     messages = FALSE, note = ""),
   ampl = list(
     ampl = FALSE, project = "ampl", solver = "ipopt",
     protocol = FALSE, trace = FALSE)
   )





optimalPortfolios.fPort(Market.Posterior_view1,optimizer="tangencyPortfolio")
$priorOptimPortfolio



###-----ALL before this is NOPE
###------------------------------------------------------------------------------------
#####-------- SIT version: Compute Posterior estimates of returns and covariance
###------------------------------------------------------------------------------------

# He & Litterman: The intuition Behind Black- Litterman Model Portfolios
# formulas (8), (9), (10)
# compute the posterior estimate of the returns and covariance
#' @export 
bl.compute.posterior <- function
(
	mu, 		# Equilibrium returns
	cov, 		# Covariance matrix
	pmat, 	# Views pick matrix
	qmat, 	# View mean vector
	tau, 	# Measure of uncertainty of the prior estimate of the mean returns
	confidences  # Confidence of each view
)
{
	out = list()	

	if( !is.null(pmat) ) {
		if( is.null(confidences) ) {
		# The Black-Litterman Model In Detail by Jay Walters
		# Assume that the variance of the views will be proportional to the variance of the asset
		# returns, just as the variance of the prior distribution is. He and Litterman (1999)
		# This specification of the variance, or uncertainty, of the views essentially equally weights the investor's
		# views and the market equilibrium weights. By including tau in the expression, the final solution becomes
		# independent of tau as well.
				
		# contactenate 1 and remove first row, col ([-1,-1]) to work properly with single view
		omega = diag(c(1,diag(tau * pmat %*% cov %*% t(pmat))))[-1,-1]
		} else {
		omega = diag(c(1,confidences))[-1,-1]
		}
		
		temp = solve(solve(tau * cov) + t(pmat) %*% solve(omega) %*% pmat)
	
		out$cov = cov + temp
	
		out$expected.return = temp %*% (solve(tau * cov) %*% mu + t(pmat) %*% solve(omega) %*% qmat)
	} else {	# no views	
		temp = tau * cov
	
		out$cov = cov + temp
	
		out$expected.return = temp %*% (solve(tau * cov) %*% mu )
	
	}
	return(out)
}

#T1<- ((tau * cov) + t(pmat) %*%(omega) %*% pmat)
#T2<- inv(inv(tau * cov) + t(pmat) %*% inv(omega) %*% pmat)
#T3<- solve(solve(tau * cov) + t(pmat) %*% solve(omega) %*% pmat)

cov<- Er_cov.mat
mu<- Pi
tau=0.025
pmat=matrix()
SIT_Post_pi_v1<- bl.compute.posterior(mu, cov, pmat= P.mat1, qmat[1], tau, confidences=conf.mat[1])
SIT_Post_pi_v2<- bl.compute.posterior(mu, cov, pmat= P.mat[1:2,], qmat[1:2], tau, confidences=conf.mat[1:2])
SIT_Post_pi_v3<- bl.compute.posterior(mu, cov, pmat= P.mat[1:3,], qmat[1:3], tau, confidences=conf.mat[1:3])
SIT_Post_pi_v4<- bl.compute.posterior(mu, cov, pmat= P.mat[1:4,], qmat[1:4], tau, confidences=conf.mat[1:4])
SIT_Post_pi_v5<- bl.compute.posterior(mu, cov, pmat= P.mat[1:5,], qmat[1:5], tau, confidences=conf.mat[1:5])

SIT_Post_pi_Omega1 <- bl.compute.posterior(mu, cov, P.mat1, qmat[1], tau, confidences=NULL)
SIT_Post_pi_Omega2 <- bl.compute.posterior(mu, cov, P.mat[1:2,], qmat[1:2], tau, confidences=NULL)
SIT_Post_pi_Omega3 <- bl.compute.posterior(mu, cov, P.mat[1:3,], qmat[1:3], tau, confidences=NULL)
SIT_Post_pi_Omega4 <- bl.compute.posterior(mu, cov, P.mat[1:4,], qmat[1:4], tau, confidences=NULL)
SIT_Post_pi_Omega5 <- bl.compute.posterior(mu, cov, P.mat[1:5,], qmat[1:5], tau, confidences=NULL)

SIT_Post_pi_NULL <- bl.compute.posterior(mu, cov, NULL, NULL, tau, confidences=NULL)

Re_SIT_pi_v1<- sum(SIT_Post_pi_v1$expected.return) ###-----expected returns of Black Litterman
Re_SIT_pi_v2<- sum(SIT_Post_pi_v2$expected.return)  ###-----expected returns of Black Litterman
Re_SIT_pi_v3<- sum(SIT_Post_pi_v3$expected.return)  ###-----expected returns of Black Litterman
Re_SIT_pi_v4<- sum(SIT_Post_pi_v4$expected.return)  ###-----expected returns of Black Litterman
Re_SIT_pi_v5<- sum(SIT_Post_pi_v5$expected.return)  ###-----expected returns of Black Litterman

Ret_conf<- c(Re_SIT_pi_v1,Re_SIT_pi_v2,Re_SIT_pi_v3,Re_SIT_pi_v4,Re_SIT_pi_v5)

Re_SIT_v1_Omega1<- sum(SIT_Post_pi_Omega1$expected.return)
Re_SIT_v2_Omega2<- sum(SIT_Post_pi_Omega2$expected.return)
Re_SIT_v3_Omega3<- sum(SIT_Post_pi_Omega3$expected.return)
Re_SIT_v4_Omega4<- sum(SIT_Post_pi_Omega4$expected.return)
Re_SIT_v5_Omega5<- sum(SIT_Post_pi_Omega5$expected.return)

Col_Omega<- matrix(rep("Omega",6), nrow=1, ncol=6) 
Col_conf<- matrix(rep("User Confidence",6), nrow=1, ncol=6) 
Col_Null<- matrix(rep("NULL",6), nrow=1, ncol=6) 

Ret_Omega_all<- cbind(rownames(cov), t(Col_Omega),SIT_Post_pi_Omega1$expected.return,SIT_Post_pi_Omega2$expected.return,SIT_Post_pi_Omega3$expected.return,SIT_Post_pi_Omega4$expected.return,SIT_Post_pi_Omega5$expected.return)
colnames(Ret_Omega_all)<-(c("Asset", "Confidence Method", "View A", "View B", "View C", "View D", "View E"))

Ret_conf_all<- cbind( rownames(cov), t(Col_conf), SIT_Post_pi_v1$expected.return,SIT_Post_pi_v2$expected.return,SIT_Post_pi_v3$expected.return,SIT_Post_pi_v4$expected.return,SIT_Post_pi_v5$expected.return)
colnames(Ret_conf_all)<-(c("Asset", "Confidence Method", "View A", "View B", "View C", "View D", "View E"))

Ret_Null_all<-cbind(rownames(cov), t(Col_Null), SIT_Post_pi_NULL$expected.return,SIT_Post_pi_NULL$expected.return,SIT_Post_pi_NULL$expected.return,SIT_Post_pi_NULL$expected.return,SIT_Post_pi_NULL$expected.return)
colnames(Ret_Null_all)<-(c("Asset", "Confidence Method", "View A", "View B", "View C", "View D", "View E"))

Ret_all<- rbind(Ret_Null_all, Ret_Omega_all, Ret_conf_all)
colnames(Ret_all)<-(c("Asset", "Confidence Method", "View A", "View B", "View C", "View D", "View E"))

###------------------------------------------------------------------------------------------------------
###------------------------------------------------------------------------------------------------------
###----Final ret matrix of different views and tau impact
Rets<- data.frame(Ret_all)


Rets2 <- melt(Rets, id.vars=1:2)
Ret_MSCI<- Rets2[Rets2$Asset=="MSCI.World",]
##---methods from:http://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2/###-------

ggplot(Ret_MSCI, aes( y=value, x=variable, color=variable, fill=variable)) + 
    geom_bar( stat="identity") +    
    facet_wrap(~Confidence.Method)+
    geom_hline(yintercept= 11, linetype="dashed", color="green", size=1)#+
    #geom_hline(yintercept= SIT_Post_pi_NULL$expected.return[5]+ 0.03518501, linetype="dashed", color="yellow", size=2)

Rets.long<- melt(Rets, id="Confidence.Method")
p<- ggplot(Ret.long, aes(Views,value))
p +geom_bar(stat = "identity", aes(fill = variable), position = "dodge")+scale_fill_brewer(palette="Blues")+
ggtitle("Comparison of Total Black Litterman Returns for User Confidences vs Omega")



###------------------------------------------------------------------------------------------------------
###------------------------------------------------------------------------------------------------------
###----------------------------------------------------------------
###----------------------------------------------------------------
###----------------------------------------------------------------
###----------------------------------------------------------------
###----------------------------------------------------------------



risk.aversion<- R_av
cap.weight<- w_mrkt
risk.free<- Rf_mean


iterations = 100
variables = 2

output <- matrix(ncol=variables, nrow=iterations)


###-----Populating Returns based on confidences
Date<- as.list(seq(1,100,0.01))

n=41 ###---confidence variation counts with i
m=41###--tau variation counts with j
n_o<- length(colnames(cov))
Views.m<- c("View A", "View B", "View C", "View D", "View E")

K_U<- data.frame(matrix(0, nrow=n, ncol=6))
K_Om<- data.frame(matrix(0, nrow=n, ncol=6))
K_N<- data.frame(matrix(0, nrow=n, ncol=6))
       colnames(K_U)[6]<- "Asset"
colnames(K_U)[1]<- "View"  
colnames(K_U)[2]<- "Confidence Method"       
colnames(K_U)[3]<-  "tau" 
colnames(K_U)[4]<- "conf"
colnames(K_U)[5]<- "Post.Return"

      colnames(K_Om)[6]<- "Asset"
colnames(K_Om)[1]<- "View" 
colnames(K_Om)[2]<- "Confidence Method"       
colnames(K_Om)[3]<-  "tau" 
colnames(K_Om)[4]<- "conf"
colnames(K_Om)[5]<- "Post.Return"

      colnames(K_N)[6]<- "Asset"
colnames(K_N)[1]<- "View" 
colnames(K_N)[2]<- "Confidence Method"       
colnames(K_N)[3]<-  "tau" 
colnames(K_N)[4]<- "conf"
colnames(K_N)[5]<- "Post.Return"


conf.m<- c('User Defined', 'omega', 'null')
Reslist<- list()
Reslist2<- list()
Reslist3<- list()
Res2list<- list()
Data_FML<- list()
Data_V<- list()

for (ii in 1:length(Views.m)){
for( o in 1:n_o){
    for (i in 1:n){
     for (j in 1:m){
        
         for (k in 1:3){

         K_U[i:n,1]<- Views.m[ii]
         K_U[i:n,2]<- conf.m[1]
         K_U[,3]<- (j-0.999)/(m-1)
         K_U[i,4]<- (i-0.999)/(n-1)

         K_Om[i:n,1]<- Views.m[ii]
         K_Om[i:n,2]<- conf.m[2]
         K_Om[,3]<- (j-0.999)/(m-1)
         K_Om[i,4]<- (i-0.999)/(n-1)

         K_N[i:n,1]<- Views.m[ii]
         K_N[i:n,2]<- conf.m[3]
         K_N[,3]<- (j-0.999)/(m-1)
         K_N[i,4]<- (i-0.999)/(n-1)


            if (k==1){
               K_U[i:n, 2] <- conf.m[1]
               K_U[i,5] <- bl.compute.posterior(mu, cov, t(as.matrix(P.mat[ii,])), qmat[ii], tau=K_U[i,3], confidences=K_U[i,4])$expected.return[o]
               K_U[ ,6]=colnames(cov)[o]
             }   
  
            if (k==2){
               K_Om[i:n, 2] <- conf.m[2]
               K_Om[i, 5] <- bl.compute.posterior(mu, cov, t(as.matrix(P.mat[ii,])), qmat[ii], tau=K_Om[i,3], confidences=NULL)$expected.return[o]
               K_Om[, 6]=colnames(cov)[o]
             }

            if (k==3){
               K_N[i:n, 2] <- conf.m[3]
               K_N[i, 5] <- bl.compute.posterior(mu, cov, NULL, NULL, tau=K_N[i,3], NULL)$expected.return[o]
               K_N[, 6]=colnames(cov)[o]
             }
K_j<- (rbind(K_U, K_Om, K_N))

Reslist[[j]] <- K_j
         }
      }
   MOFO<-do.call(rbind,Reslist)
   Data_FML[[o]]<- MOFO
   }
  big_data <- do.call(rbind, Data_FML)
  Data_V[[ii]]<- big_data
  }

 Data_Frame<- do.call(rbind, Data_V)
}


DF<- data.frame(Data_Frame)
Name<- DF[,6]
DF<- cbind(Name, DF)
colnames(DF[1])<- "Asset"
DF<- DF[,1:6]
head(DF)
#DF[DF=='null'] <- NA
#DF2<- DF[,3:10]
DF.view<- DF$View
DF.type<- DF$Asset
colnames(t(DF))
log.DF <- log(DF[, 4:6])
DF.pca <- prcomp(DF[, 4:6], scale = TRUE)

DF.pca$eig 
summary(DF.pca)
plot(DF.pca, type = "b")
library(ggbiplot)
biplot(DF.pca , scale. = 0)
###--------Create PCA From Rbloggers

#load library
install.packages("dummies")
library(dummies)
install.packages("factoextra")
##----------------------------------------------------------------------------------------------
##----------------------------------------------------------------------------------------------
#create a dummy data frame---HERE, SIZE OF MATRIX IS AN ISSUE, RISES TO MB WHICH IS NOT ALLOWED
##----------------------------------------------------------------------------------------------
##----------------------------------------------------------------------------------------------

new_my_data <- dummy.data.frame(DF, names = c("Name", "View", "Confidence.Method", "tau","conf", "Post.Return"))
#check the data set
str(new_my_data)

DF_n.pca <- prcomp(new_my_data, scale. = T)
names(DF_n.pca)
prin_comp<- DF_n.pca

#outputs the mean of variables
prin_comp$center
prin_comp$rotation[1:5,1:4]
prin_comp$rotation

biplot(prin_comp, scale = 0)

#outputs the standard deviation of variables
prin_comp$scale

std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
             ylab = "Proportion of Variance Explained",
             type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
              ylab = "Cumulative Proportion of Variance Explained",
              type = "b")


##---------using Decathlon example:

decathlon2.active<- DF[1:(nrow(DF)-0.2*(nrow(DF))), 4:6]
res.pca <- prcomp(decathlon2.active, scale = TRUE)
names(res.pca)
head(res.pca$sdev)
head(unclass(res.pca$rotation)[, 1:3])


# Eigenvalues
eig <- (res.pca$sdev)^2
# Variances in percentage
variance <- eig*100/sum(eig)
# Cumulative variances
cumvar <- cumsum(variance)
eig.decathlon2.active <- data.frame(eig = eig, variance = variance,
                     cumvariance = cumvar)
head(eig.decathlon2.active)

summary(res.pca)

library("factoextra")
eig.val <- get_eigenvalue(res.pca)
head(eig.val)

barplot(eig.decathlon2.active[, 2], names.arg=1:nrow(eig.decathlon2.active), 
       main = "Variances",
       xlab = "Principal Components",
       ylab = "Percentage of variances",
       col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.decathlon2.active), 
      eig.decathlon2.active[, 2], 
      type="b", pch=19, col = "red")


fviz_screeplot(res.pca, ncp=10)
fviz_screeplot(res.pca, ncp=10, choice="eigenvalue")

var <- get_pca_var(res.pca)
var$coord[, 1:3]

# Helper function : 
# Correlation between variables and principal components
var_cor_func <- function(var.loadings, comp.sdev){
  var.loadings*comp.sdev
  }
# Variable correlation/coordinates
loadings <- res.pca$rotation
sdev <- res.pca$sdev
var.coord <- var.cor <- t(apply(loadings, 1, var_cor_func, sdev))
head(var.coord[, 1:3])

# Plot the correlation circle
a <- seq(0, 2*pi, length = 100)
plot( cos(a), sin(a), type = 'l', col="gray",
      xlab = "PC1",  ylab = "PC2")
abline(h = 0, v = 0, lty = 2)
# Add active variables
arrows(0, 0, var.coord[, 1], var.coord[, 2], 
      length = 0.1, angle = 15, code = 2)
# Add labels
text(var.coord, labels=rownames(var.coord), cex = 1, adj=1)
fviz_pca_var(res.pca)



fviz_pca_var(res.pca, col.var="contrib")+
scale_color_gradient2(low="white", mid="blue", 
      high="red", midpoint=55) + theme_minimal()


fviz_pca_var(res.pca, col.var="contrib") +
scale_color_gradient2(low="white", mid="blue", 
      high="red", midpoint=50) + theme_minimal()

ind.coord <- res.pca$x
head(ind.coord[, 1:3])

center <- res.pca$center
scale<- res.pca$scale
getdistance <- function(ind_row, center, scale){
  return(sum(((ind_row-center)/scale)^2))
  }
d2 <- apply(decathlon2.active,1,getdistance, center, scale)
# Compute the cos2
cos2 <- function(ind.coord, d2){return(ind.coord^2/d2)}
ind.cos2 <- apply(ind.coord, 2, cos2, d2)
head(ind.cos2[, 1:3])



# Contributions of individuals
contrib <- function(ind.coord, comp.sdev, n.ind){
  100*(1/n.ind)*ind.coord^2/comp.sdev^2
}
ind.contrib <- t(apply(ind.coord,1, contrib, 
                       res.pca$sdev, nrow(ind.coord)))
head(ind.contrib[, 1:3])


plot(ind.coord[,1], ind.coord[,2], pch = 19,  
     xlab="PC1 - 41.2%",ylab="PC2 - 18.4%")
abline(h=0, v=0, lty = 2)
text(ind.coord[,1], ind.coord[,2], labels=rownames(ind.coord),
        cex=0.7, pos = 3)


g <- ggbiplot(DF.pca, obs.scale = 0, var.scale = 0, 
              groups = DF$Confidence.Method, ellipse = TRUE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

####------18-09-2017

###------Dataframe CREATED, HALLELUJAH



install.packages("plotly")
library(plotly)
install.packages("devtools") 
install.packages("plot3D")
library(plot3D)
library(devtools)

Unvrs<- DF[ (DF$Name=="MSCI.World"), ]
#Unvrs<- Unvrs[ (Unvrs$Asset=="Goldman.Sachs.Commodity"), ]
Unvrs_A <-Unvrs[ Unvrs$View=="View A", ]
Unvrs_B <-Unvrs[ (Unvrs$View=="View B"), ]
Unvrs_C <-Unvrs[ (Unvrs$View=="View C"), ]
Unvrs_D <-Unvrs[ (Unvrs$View=="View D"), ]
Unvrs_E <-Unvrs[ (Unvrs$View=="View E"), ]

Unvrs_UD<- Unvrs[ (Unvrs$Confidence.Method=="User Defined"), ]
Unvrs_Om<- Unvrs[ (Unvrs$Confidence.Method=="omega"), ]
Unvrs_NULL<- Unvrs[ (Unvrs$Confidence.Method=="null"), ]

#x<- Unvrs$tau
#y<- Unvrs$conf
#z<- Unvrs$Post.Return

x<- Unvrs_UD$tau
y<- Unvrs_UD$conf
z<- Unvrs_UD$Post.Return

colnames(Unvrs)

scatter3D(x, y, z, clab = c("MSCI World, View 1", "USD"))
scatter3D(x, y, z, colvar = NULL, col = "blue",
          pch = 19, cex = 0.5)
# full box
scatter3D(x, y, z, bty = "f", colkey = FALSE, main ="bty= 'f'")
# back panels and grid lines are visible
scatter3D(x, y, z, bty = "b2", colkey = FALSE, main ="bty= 'b2'" )

# grey background with white grid lines
scatter3D(x, y, z, bty = "g", colkey = FALSE, main ="bty= 'g'")
# User defined
scatter3D(x, y, z, pch = 18, bty = "u", colkey = FALSE, 
   main ="bty= 'u'", col.panel ="steelblue", expand =0.4, 
   col.grid = "darkblue")

# gg.col: ggplot2 like color
scatter3D(x, y, z, bty = "g", pch = 18, col = gg.col(100))
# ramp.col: custom palettes
scatter3D(x, y, z, bty = "g", pch = 18,
          col = Unvrs_UD$pcolor, xlab="tau", ylab="Confidence/Omega", zlab="MSCI World Return", main="MSCI World Return Subject to different views and different methods" )


library(scatterplot3d)
Unvrs_UD$pcolor[Unvrs_UD$View== "View A"] <- "red"
Unvrs_UD$pcolor[Unvrs_UD$View== "View B"] <- "orange"
Unvrs_UD$pcolor[Unvrs_UD$View== "View C"] <- "darkblue"
Unvrs_UD$pcolor[Unvrs_UD$View== "View D"] <- "pink"
Unvrs_UD$pcolor[Unvrs_UD$View== "View E"] <- "purple"
with(Unvrs_UD, {
  s3d <- scatterplot3d(conf,   # x axis
                 tau,     # y axis
                 Post.Return,    # z axis
                 color=pcolor, pch=19,  
                 main="Black Litterman Posterior, MSCI World, All Views",
                 scale.y=.75,  
 
                 xlab="Confidence",
                 ylab="Tau",
                 zlab="Posterior Return")

                 legend("topleft", inset=.05,      # location and inset
                 bty="n", cex=.5,              # suppress legend box, shrink text 50%
                 title="Views",
                 c("View A", "View B", "View C", "View D", "View E"), fill=c("red","orange"
                 ,"darkblue","pink","purple"  ))

})
 

hist3D_fancy(x, y, z)
# Make the rgl version
library("plot3Drgl")
plotrgl() 



library(ReporteRs)
library(magrittr)

SimonDoc <- docx( ) %>% 
  addParagraph(  value="Green and Conventional Statistical Study", stylename = "TitleDoc" )%>%
  addTitle(  "Green Data", level = 1 )%>%
  addParagraph( value="tables of standard deviation and expected returns for Green Data excluding Market values", stylename = "DocDefaults" )%>%
  
  addFlexTable( Green_STDV %>%
                  FlexTable( header.cell.props = cellProperties( background.color =  "#003366" ),
                             header.text.props = textBold( color = "white" ),
                             add.rownames = TRUE ) %>%
                  setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%

  addTitle( "Conventional Data", level = 1 )%>%
  addParagraph( value="tables of standard deviation and expected returns for Conventional Data excluding Market values", stylename = "DocDefaults" )%>%

  addFlexTable( Conv_STDV %>%
                  FlexTable( header.cell.props = cellProperties( background.color =  "#003366" ),
                             header.text.props = textBold( color = "white" ),
                             add.rownames = TRUE ) %>%
                  setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ))  %>%
 addTitle( "Covariance Matrix: Green Data", level = 1 )%>%
 addParagraph( value=" For Covariances, Only the Complete data was used (no NA), as the asset vectors must be same size")%>%
 addFlexTable( Cov_GR %>%
                  FlexTable( header.cell.props = cellProperties( background.color =  "#003366" ),
                             header.text.props = textBold( color = "white" ),
                             add.rownames = TRUE ) %>%
                  setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ))  %>%

 addTitle( "Covariance Matrix: Conventional Data", level = 1 )%>%
 addFlexTable( Cov_Conv %>%
                  FlexTable( header.cell.props = cellProperties( background.color =  "#003366" ),
                             header.text.props = textBold( color = "white" ),
                             add.rownames = TRUE ) %>%
                  setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ))  %>%

 
writeDoc(file = 'Report.docx')
# open the Word document
browseURL("Report.docx")







Unvrs<- DF[ (DF$Asset=="MSCI.World"), ]
Unvrs_A <-Unvrs[ (Unvrs$View=="View A"), ]
Unvrs_A_UD<- Unvrs_A[ (Unvrs_A$Confidence.Method=="User Defined"), ]
Unvrs_A_NULL<- Unvrs_A[ (Unvrs_A$Confidence.Method=="null"), ]
Unvrs_NULL<- DF[ (DF$Confidence.Method=="null"), ]


Uni_msci.w<- Unvrs_A$MSCI.World
Unvrs_A <-DF[ (DF$View=="View A"), ]

UA_m<- melt(Unvrs_A, idvarys="Confidence.Method")
Unvrs_A_UD<- Unvrs_A[ (Unvrs_A$Confidence.Method=="User Defined"), ]
Unvrs_A_NULL<- Unvrs_A[ (Unvrs_A$Confidence.Method=="null"), ]
Unvrs_A_Om<- Unvrs_A[ (Unvrs_A$Confidence.Method=="omega"), ]
Unvrs_NULL<- DF[ (DF$Confidence.Method=="null"), ]

p1<- ggplot(Unvrs_A_UD, aes(x = conf, y = Post.Return,colour= tau))+ geom_point()
p2<- ggplot(Unvrs_A_NULL, aes(x = conf, y = Post.Return,colour= tau))+ geom_point()
p3<- ggplot(Unvrs_A_Om, aes(x = conf, y = Post.Return,colour= tau)) + geom_point()

multiplot(p1, p2, p3, cols=3)
#> `geom_smooth()` using method = 'loess'

p12<-  ggplot(data= Unvrs_A_NULL, aes(x = conf, y = Post.Return,colour= "MSCI Null",
 label=format(round(mean(Unvrs_A_NULL$tau)/Unvrs_A_NULL$conf, 2), nsmall = 2)))+ geom_point()+ 
geom_text(aes(label=format(round(mean(Unvrs_A_NULL$tau)/Unvrs_A_NULL$conf, 2), nsmall = 2)),position = position_nudge(y = -0.01))
format(round(mean(Unvrs_A_NULL$tau)/Unvrs_A_NULL$conf, 2), nsmall = 2)

p13<- ggplot(data= Unvrs_A_NULL, aes(x = conf, y = Post.Return + 0.035 ,
colour= "MSCI Null+0.35 (2%)",label=format(round(mean(Unvrs_A_NULL$tau)/Unvrs_A_NULL$conf, 2), nsmall = 2)))+geom_point()+
geom_text(aes(label=format(round(mean(Unvrs_A_NULL$tau)/Unvrs_A_NULL$conf, 2), nsmall = 2)),position = position_nudge(y = -0.01))
format(round(mean(Unvrs_A_NULL$tau)/Unvrs_A_NULL$conf, 2), nsmall = 2)

p14<- ggplot(data= Unvrs_A_UD, aes(x = conf, y = Post.Return),
colour= "MSCI with 2% view A",label=format(round(mean(Unvrs_A_UD$tau)/Unvrs_A_UD$conf, 2), nsmall = 2)))+geom_point()+
geom_text(aes(label=format(round(mean(Unvrs_A_UD$tau)/Unvrs_A_UD$conf, 2), nsmall = 2)),position = position_nudge(y = -0.01))
format(round(mean(Unvrs_A_UD$tau)/Unvrs_A_UD$conf, 2), nsmall = 2)

multiplot(p12, p13, p14, cols=3)


+ geom_text(aes(label=format(round(mean(tau)/conf, 2), nsmall = 2)),position = position_nudge(y = -0.1))
format(round(mean(tau)/conf, 2), nsmall = 2)



ggplot((data=Unvrs_A_NULL, aes(x = conf, y = MSCI.World, colour= "MSCI NULL view A")))+ geom_point()+
        + geom_point(data=Unvrs_A_NULL, aes(x = conf, y = MSCI.World ,colour= "MSCI NULL view A + 2%" ))
Unvrs_A_NULL[1:10, 1:11]
library(akima)
library(rgl)
library(plot3D)

s=interp(x,y,z)

theme_new <- theme_bw() +
  theme(plot.background = element_rect(size = 1, color = "blue", fill = "black"),
        text=element_text(size = 12, family = "Serif", color = "ivory"),
        axis.text.y = element_text(colour = "purple"),
        axis.text.x = element_text(colour = "red"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "orange"))

library("ggthemes")
library("scales")

 ggplot(big_data, aes(x = conf, y = MSCI.World)) + geom_point() + facet_wrap(~ tau)+ 
geom_hline(yintercept= SIT_Post_pi_NULL$expected.return[5], linetype="dashed", color="green", size=1)+
geom_hline(yintercept= SIT_Post_pi_NULL$expected.return[5]+ 0.03518501, linetype="dashed", color="yellow", size=2)+ 
#geom_hline(yintercept= er[5], linetype="dashed", color="yellow", size=1)+
#geom_hline(yintercept= Er_BL_nv[5], linetype="dashed", color="purple", size=1)+
#geom_hline(yintercept= (Er_BL_nv[5]+0.035), linetype="dashed", color="red", size=1)+
#geom_hline(yintercept= Pi[5], linetype="dashed", color="pink", size=1)+ theme_stata() + 
 labs(title = "Confidence Study for Tau Variation\n", x = "User Confidences" , y= "MSCI.World Return", color = "Legend Title\n") +
  scale_color_manual(labels = c("Equilibrium return+ 2%", "Black Litterman Return, View A"), values = c("pink", "black")) 
#geom_hline(yintercept= ExpRet[5], linetype="dashed", color="black", size=1)

  ggplot(big_data, aes(x = conf, y = MSCI.World))+ geom_line(aes(color = tau))



#cat("confidence is" j*0.01)
Noview<- bl.compute.posterior(mu, cov, NULL, NULL, tau,NULL)
Noview$expected.return
K[[i]]
K[[i,j]] <- (Res)
pd[,i]<- K[[i]]$expected.return


colnames(pd[,i])<- (i*0.01)
rownames(pd)<- rownames(cov)
}
}

Ret<- data.frame()
Noview<- bl.compute.posterior(mu, cov, NULL, NULL, tau,NULL)
C1<- (t(cbind(Noview$expected.return, pd)))
Ret<- data.frame(C1)
colnames(Ret[,1])<- 0 
rownames(Ret)<-seq(0,1,0.01)
library(ggplot2)
hist(Ret[,"MSCI.World"])
Conf<- rownames(Ret)
library(ggplot2)
p5<- ggplot(Ret, aes(x= Conf, y = MSCI.World)) 
p5 + geom_point(aes(color = MSCI.World)) +
theme(legend.position="top",axis.text=element_text(size = 6))+
#geom_hline(yintercept= er[5], linetype="dashed", color="yellow", size=1)+
#geom_hline(yintercept= Er_BL_nv[5], linetype="dashed", color="purple", size=1)+
#geom_hline(yintercept= (Er_BL_nv[5]+0.035), linetype="dashed", color="red", size=1)+
geom_hline(yintercept= Pi[5], linetype="dashed", color="red", size=2)+
#geom_hline(yintercept= ExpRet[5], linetype="dashed", color="red", size=1)

xlab("confidence")

  geom_line(aes(y = pred.SC))




  # create Black-Litterman input assumptions with Views   
  #  ia.bl.view = mu
      #  ia.bl.view$expected.return = SIT_Post_pi_v1$expected.return
        #ia.bl.view$cov = SIT_Post_pi_v1$cov
      #  ia.bl.view$risk = sqrt(diag(ia.bl.view$cov))

   # create efficient frontier(s)
    #ef.risk.bl.view = portopt(ia.bl.view, constraints, 50, 'Black-Litterman + View(s)', equally.spaced.risk = T)    
 
BL_ret<- data.frame(SIT_Post_pi_v1$expected.return, SIT_Post_pi_v2$expected.return, SIT_Post_pi_v3$expected.return, SIT_Post_pi_v4$expected.return, SIT_Post_pi_v5$expected.return)

  

par(mfcol = c(5, 1))

weightsPie(optPorts_SIT_1$posteriorOptimPortfolio)
title("MV Tangency Portfolio Opt,1 view, Posterior") 


weightsPie(optPorts_SIT_2$posteriorOptimPortfolio)
title("MV Tangency Portfolio Opt,2 views, Posterior") 

weightsPie(optPorts_SIT_3$posteriorOptimPortfolio)
title("MV Tangency Portfolio Opt,3 views, Posterior") 

weightsPie(optPorts_SIT_4$posteriorOptimPortfolio)
title("MV Tangency Portfolio Opt,4 views, Posterior") 

weightsPie(optPorts_SIT_5$posteriorOptimPortfolio)
title("MV Tangency Portfolio Opt,5 views, Posterior") 






##----calculating EQ returns with SIT means adding on Rf_mean to the reverse optim
bl.compute.eqret <- function
(
	risk.aversion, 	# Risk Aversion
	cov, 			# Covariance matrix
	cap.weight, 	# Market Capitalization Weights
	risk.free 	# Rsik Free Interest Rate
)
{
	return( risk.aversion * cov %*% as.vector(cap.weight) +  as.double(risk.free))	
}

SIT_Re_Eq<- bl.compute.eqret(R_av, cov.mat, w_mrkt, Rf_mean)

###----------Computing optimal portfolio weights for the for BL:

# He & Litterman: The intuition Behind Black- Litterman Model Portfolios
# formulas (13)
#
# T. Idzorek: A STEP-BY-STEP GUIDE TO THE BLACK-LITTERMAN MODEL
# formulas (2)
#
# compute the portfolio weights for the optimal portfolio on the unconstrained efficient frontier
#' @export 


cov<- as.matrix(cov.mat)

	omega = diag(c(1,diag(tau * P.mat1 %*% cov %*% t(P.mat1))))[-1,-1]

		
	    PostCov = solve(solve(tau * cov) + t(P.mat1) %*% solve(omega) %*% P.mat1)##--becareful to not use 
        #####Omega instead of omega

	SigmaP = cov + PostCov
      EqRiskPrem = R_av * cov %*% w_mrkt

	ExpRet = PostCov %*% (solve(tau * cov) %*% EqRiskPrem + t(P.mat1) %*% solve(omega) %*% qmat[1])
##------Optimal weights 

bl.compute.optimal <- function(risk.aversion, ExpRet, cov)
{
	return( (1/risk.aversion) * solve(cov) %*% ExpRet )
}
SIT_Re_OW<- bl.compute.optimal(R_av, ExpRet, as.matrix(cov.mat))




###-----------------------------------------------------------------------------------------------------------
ia = list()
ia$symbols = names(Re_dat)
ia$symbol.names = names(Re_dat)
ia$hist.returns = Re_dat
ia$risk = apply(ia$hist.returns, 2, sd, na.rm = T)
ia$annual.factor = 1
ia$arithmetic.return = apply(ia$hist.returns, 2, mean, na.rm = T)
ia$arithmetic.return = (1 + ia$arithmetic.return)^ia$annual.factor - 1
ia$geometric.return = apply(ia$hist.returns, 2, function(x) prod(1+x)^(1/length(x))-1 )
ia$geometric.return = (1 + ia$geometric.return)^ia$annual.factor - 1
ia$correlation = cor(ia$hist.returns, use = 'complete.obs', method = 'pearson')
ia$cov = cov(ia$hist.returns, use = 'complete.obs', method = 'pearson')
ia$expected.return = ia$arithmetic.return

ia.bl<- Re_dat
ia$n = nrow(Re_dat)
risk.aversion<- R_av
risk.free<- Rf_mean
cap.weight<- t(w_mrkt)
expected.return<- bl.compute.eqret(risk.aversion, cov, cap.weight, Rf_mean)

   
n = ia$n
# -1 <= x.i <= 1
constraints = new.constraints(n, lb = 0, ub = 1)
 
# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)    

efficient.frontier(SIT_Post_pi$expected.return, SIT_Post_pi$cov, nport=200, alpha.min=-1, alpha.max=1)
ia.bl$expected.return = bl.compute.eqret( risk.aversion, ia$cov, cap.weight , Rf_mean)
ef.risk.bl = portopt(ia.bl, constraints, 50, 'Black-Litterman', equally.spaced.risk = T)  


ef.risk = portopt(ia, constraints, 50, 'Historical', equally.spaced.risk = T)       
ef.risk.bl = portopt(ia.bl, constraints, 50, 'Black-Litterman', equally.spaced.risk = T)    
 