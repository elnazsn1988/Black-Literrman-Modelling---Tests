#----if you need SIT, use these-----------------------------------
#con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
 #   source(con)
#close(con)


#devtools::install_github('systematicinvestor/SIT.date')

#library(curl)
#curl_download('https://github.com/systematicinvestor/SIT/raw/master/SIT.tar.gz', 'sit',mode = 'wb',quiet=T)
#install.packages('sit', repos = NULL, type='source')


#aa.test.hist.capitalization() 


##rm(list=ls())
library("quadprog")
library(BLCOP) 
library("MASS")
library(xlsx)
library(dplyr)
library(portfolio)
library(ggplot2)

setwd("C:\\Program Files\\R\\R-3.4.1")

Re<-read.xlsx("Input_Rtr.xlsx",1)
ex<-read.xlsx("excess_Rtr.xlsx",1)
RF<-read.xlsx("Riskfree.xlsx",1)
MC<-read.xlsx("Market_cap.xlsx",1)
N_I=length(Re)
#K=views
#K=3
Re_dat <- data.frame(Re[,3:N_I]*12) ##--The data is multiplied by 12 to denote annual returns, if 3 is used, citi is removed.
rownames(Re_dat)<- Re$Date
Mc_dat<-data.frame(MC[,3:N_I]) ##as the CAPM are independent of dates, the Market cap isnt multiplied.
rownames(Mc_dat)<- MC$Date
Rf_dat<- data.frame(RF[,1]*12)  ###also muliplied by 12
rownames(Rf_dat)<- Re$Date


Var_m <- data.frame() 
Cor_m <- data.frame()
Cov_m <- data.frame()
ER_m  <- data.frame()



for (i in 1:N)
{
Var_m[1,i]=var(Re_dat[,i], y = NULL, na.rm = FALSE)
ER_m[1,i]=mean(Re_dat[,i])
}

colnames(Var_m)<-colnames(Re_dat)
N=length(Re_dat)
colnames(ER_m) <-colnames(Re_dat)
rownames(ER_m) <- "Expected returns"
#----calculating mean risk free
numdata<- Rf_dat[sapply(Rf_dat, is.numeric)]  
Rf_mean<- sapply(numdata, mean, na.rm = T)  # Returns a vector



#-------calculate cov and cor------------------------

Cov_m <- cov(Re_dat, y = NULL, use = "everything",
    method = c("pearson"))

Cor_m <- cor(Re_dat, y = NULL, use = "everything",
    method = c("pearson"))


er<-ER_m

er<- as.numeric(ER_m[1,])
cov.mat<-Cov_m
#efficient<-efficient.frontier()
 #
  #------------check for valid inputs
  #
 asset.names <- names(er)
  er <- as.vector(er)
  cov.mat <- as.matrix(cov.mat)
  if(length(er) != nrow(cov.mat))
    stop("invalid inputs")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")
  #
  #-----------------create portfolio names
  #
nport=1
library(portfolio)  
port.names <- rep("port",nport)
  ns <- seq(1,nport)
  port.names <- paste(port.names,ns)



#----------------compute global minimum variance portfolio

#-----------------compute global minimum variance portfolio

#----------------compute global minimum variance portfolio

#-----------------compute global minimum variance portfolio

#-----------------compute global minimum variance portfolio

#-----------------compute global minimum variance portfolio

#-----------------compute global minimum variance portfolio

  #
mu.vec=unlist(ER_m)
sigma.mat=cov.mat
top.mat=cbind(2*cov.mat, rep(1, nrow(cov.mat)))
 bot.vec = c(rep(1, nrow(cov.mat)), 0) 
 Am.mat = rbind(top.mat, bot.vec)
b.vec = c(rep(0, nrow(cov.mat)), 1)
 z.m.mat = solve(Am.mat)%*%b.vec
 m.vec = z.m.mat[1:nrow(cov.mat),1]


 sig2.gmin = as.numeric(t(m.vec)%*%sigma.mat%*%m.vec) #--Global Minimum Variance Portfolio Variance and STdv
 sig.gmin = sqrt(sig2.gmin)
mu.gmin = as.numeric(crossprod(m.vec, mu.vec)) 

 top.mat = cbind(2*sigma.mat, mu.vec, rep(1, nrow(cov.mat))) #--calculating efficient portfolio for max return
 mid.vec = c(mu.vec, 0, 0)
 bot.vec = c(rep(1, nrow(cov.mat)), 0, 0)
 A.mat = rbind(top.mat, mid.vec, bot.vec)
 bmsft.vec = c(rep(0, nrow(cov.mat)), max(mu.vec), 1)
#-----solve for x using (1-18)

z.mat = solve(A.mat)%*%bmsft.vec
x.vec = z.mat[1:nrow(cov.mat),] #---weights of efficient portfolio with target return
mu.px = as.numeric(crossprod(x.vec, mu.vec)) ###-return of efficient portfolio with target return


sig2.px = as.numeric(t(x.vec)%*%sigma.mat%*%x.vec)#--variance and std of efficient portfolio
sig.px = sqrt(sig2.px)
Ax.mat<-A.mat
bsbux.vec = c(rep(0, nrow(cov.mat)), min(mu.vec), 1) #---create min return portfolio

z.mat = solve(Ax.mat)%*%bsbux.vec
y.vec = z.mat[1:nrow(cov.mat),]
mu.py = as.numeric(crossprod(y.vec, mu.vec))

sig2.py = as.numeric(t(y.vec)%*%sigma.mat%*%y.vec)
sig.py = sqrt(sig2.py)

sigma.xy = as.numeric(t(x.vec)%*%sigma.mat%*%y.vec)
rho.xy = sigma.xy/(sig.px*sig.py)
sigma.xy                       #----calculating covar and corel between Rx and Ry

a = 0.5                        #----new frontier portfolio
z.vec = a*x.vec + (1-a)*y.vec
mu.pz = as.numeric(crossprod(z.vec, mu.vec)) #-getting efficient frontier----------
sig2.pz = as.numeric(t(z.vec)%*%sigma.mat%*%z.vec)
sig.pz = sqrt(sig2.pz)

#----plot efficient frontier-------CHECKED ALL GOOD

a = seq(from=-1, to=1, by=0.1)
n.a = length(a)
z.mat = matrix(0, n.a, nrow(cov.mat))
mu.z = rep(0, n.a)
sig2.z = rep(0, n.a)
m=y.vec
sig.mx = t(y.vec)%*%sigma.mat%*%x.vec
sig.gx = t(m.vec)%*%sigma.mat%*%x.vec

for (i in 1:n.a) {
    z.mat[i, ] = a[i]*m.vec + (1-a[i])*x.vec
    mu.z[i] = a[i]*mu.gmin + (1-a[i])*mu.px
    sig2.z[i] = a[i]^2 * sig2.gmin + (1-a[i])^2 * sig2.px +
    2*a[i]*(1-a[i])*sig.gx
}

plot(sqrt(sig2.z), mu.z, type="b", pch=16, col="green", ylab=expression(mu[p]), xlab=expression(sigma[p]))

for (i in 1:n.a) {
    z.mat[i, ] = a[i]*y.vec + (1-a[i])*x.vec
    mu.z[i] = a[i]*mu.py + (1-a[i])*mu.px
    sig2.z[i] = a[i]^2 * sig2.py + (1-a[i])^2 * sig2.px +
    2*a[i]*(1-a[i])*sig.mx
}



points(sqrt(sig2.z), mu.z, type="b", pch=16, col="blue", ylab=expression(mu[p]), xlab=expression(sigma[p]))





plot(sqrt(sig2.z), mu.z, type="b", pch=16, col="blue", ylab=expression(mu[p]), xlab=expression(sigma[p]))
text(sig.gmin, mu.gmin, labels="Global min", pos=4)


rf = Rf_mean
sigma.inv.mat = solve(sigma.mat)
one.vec = rep(1, nrow(cov.mat))
mu.minus.rf = mu.vec - rf*one.vec   ##---excess returns
top.mat = sigma.inv.mat%*%mu.minus.rf    #-----tan portfolio
bot.val = as.numeric(t(one.vec)%*%top.mat)
t.vec = top.mat[,1]/bot.val#--tangency portfolio weights
mu.t = as.numeric(crossprod(t.vec, mu.vec)) ###---tangency return
sig2.t = as.numeric(t(t.vec)%*%sigma.mat%*%t.vec)
sig.t = sqrt(sig2.t)#---tangency portfolio return

x.t.02 = 0.02/sig.t           #-----assuming risk aversion of 0.02 and using 1.46 
x.t.02*t.vec
mu.t.02 = x.t.02*mu.t + (1-x.t.02)*rf
sig.t.02 = x.t.02*sig.t



Rf_p<- cbind(0,Rf_mean)
Tang_P<- data.frame(cbind(sig.t, mu.t))
s2point<- cbind(sig.t.02,mu.t.02)
ef_front<- data.frame(cbind(sqrt(sig2.z),mu.z))
colnames(Rf_p) <- colnames(s2point) 
colnames(Tang_P)  <- colnames(s2point)
Tan_line<- rbind(s2point, Rf_p, Tang_P)

#---TO PLOT THE TANGENT AND MARKOWITZ, FIRST PLOT TAN_LINE, AND THEN POINTS(MARKOWITZ)

plot(sig.t ,mu.t , xlim=range(min(min(Tan_line[,1]),min(sqrt(sig2.z))),max(max(sqrt(sig2.z)),max(Tan_line[,1]))), ylim=range(min(min(Tan_line[,2]),min(mu.z)),max(max(mu.z),max(Tan_line[,2]))), type="b", pch=16, col="red")
points(Tan_line, type="b", pch=16, col="red", ylab=expression(mu[p]), xlab=expression(sigma[p]))
points(sqrt(sig2.z), mu.z, type="b", pch=24, col="blue", ylab=expression(mu[p]), xlab=expression(sigma[p]))
text(sig.gmin, mu.gmin, labels="Global min", pos=4)
text(sig.t, mu.t, labels="Tangency Portfolio", pos=4)
points(Tang_P)



#----expected return on global minimum portfolio:
mu.gmin = as.numeric(crossprod(m.vec, mu.vec))






#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------


M_w<- data.frame(Mc_dat/rowSums(Mc_dat))
M_mean<- (Mc_dat[1,])
M_w<- Mc_dat[1,]/sum(Mc_dat[1,])
library(reshape2)
M_wn<- data.frame()
M_wn<- M_w[1,]
print("having checked, the first line of the M_w corresponds to the first line of the formula setup")
w_mrkt<- t(as.matrix(M_wn)) #---MARKET WEIGHTS	

##M_w$Date<-row.names(Mc_dat)
###---------------------------------------
###--------------------Plot Risk Aversions
###---------------------------------------
#layout( matrix(c(1,1,2,3), nrow=2, byrow=T) )
    #pie((M_wn), paste(colnames(M_wn), round(100*M_wn), '%'), 
       # main = paste(' Market Capitalization Weights for', format(index(M_wn),'%b %Y')))
     
   # plot.ia(ia.bl, T)

##------------------------------------------------------------


##xy=melt(M_w, id.Vars=M_w$Date )
##library(ggplot2)
##ggplot(xy, aes(Date , y= value, col=variable))+ geom_point() + 
  ##stat_smooth() + facet_wrap(~variable)

#-------------------------------------------------------------------------
#----Calculating Risk aversion from original Math to compare with variance
#-------------------------------------------------------------------------
Ex_R<- data.frame()
Ex_R <- Re_dat[1:ncol(Re_dat)]-(Rf_dat[,1]) #---NOT TRUE IN CURRENT APPLICATION
Er_mkt<- er%*%w_mrkt


Ex_R_m <- er - mean(Rf_dat[,1]) ###-----Excess Return of each asset
Ex_R_mp <- Er_mkt - mean(Rf_dat[,1]) ###-----Excess Return of Market Portfolio

#---------------------------------
#-----Variance Functions
#---------------------------------
#covmat <- var(x = x, na.rm = na.rm)
weights<- w_mrkt
  utc <- upper.tri(cov.mat)
  wt.var <- sum(diag(cov.mat) * weights^2)
  wt.cov <- sum(weights[row(cov.mat)[utc]] *
                weights[col(cov.mat)[utc]] *
                cov.mat[utc])
variance <- wt.var + 2 * wt.cov###-----same as denomiator of above method
st_dev<- sqrt(variance)

#------------------------------------------
#---------Calculating different returns-----
#------------------------------------------
#var_1<- as.matrix(cov(Ex_R))%*%w_mrkt#---calculating based on each asset
#var_2<- t(var_1)%*%(var_1)
#R_av<- t(var_1)%*% Ex_R_m/var_2 
###------Risk Aversion from Market returns

R_av_1<- ((Er_mkt)-mean(Rf_dat[,1]))/variance
Re_Eq_1<- (as.double(R_av) * (cov.mat %*% as.vector(w_mrkt))) #+  mean(Rf_dat[,1])) #--Equilbirum market returns

#rownames(M_Re[1,])<- "Risk Aversion"



#beta<-(cov.mat%*%w_mrkt)/((t(w_mrkt))%*%cov.mat%*%w_mrkt)##----this doesnt work, need to check why later
beta<-(Ex_R_mp)/((t(w_mrkt))%*%cov.mat%*%w_mrkt) 
#---Question is, which covariance? actual market returns for each asset, or 
#covariance of excess returns?the variance and excess are for market portfolio. 

#----Compute Equilibrium Returns from reverse optimization:



print("substituting above beta for extracted from Literature as 0.5")
SR<- 0.5
R_av_2<- SR/st_dev

Re_Eq_2<- (as.double(R_av_2) * (cov.mat %*% as.vector(w_mrkt))) #+  mean(Rf_dat[,1])) #--Equilbirum market returns

R_av<- 2.5

Re_Eq<- (as.double(R_av) * (cov.mat %*% as.vector(w_mrkt))) #+  mean(Rf_dat[,1])) #--Equilbirum market returns
 
M_Er<- data.frame(t((M_mean)), w_mrkt, er, Re_Eq_1, Re_Eq_2, Re_Eq,  Er_BL_nv)
rownames(M_Er)<- rownames(Re_Eq_1)
colnames(M_Er)<- c("Market Caps USD", "Market Weights", "Historial Est Re", "CAPM with Lamba Calc= 0.7", "CAPM with Lambda from (SR=0.5)=0.9", "CAPM with Lamba from Lit=2.5","BL No View Re with Lambda=2.5")
#-----------------------------
#-----------------------------
K.vec=rep(0, 3)

K.vec[1]<- 0.3
K.vec[length(K.vec)]<- 0.3
K.vec[ceiling((1+length(K.vec))/2)]<- -0.6

P.mat<- matrix(0L, nrow = 5  , ncol=nrow(cov.mat))
#for (i in 1:length(K.vec)){

  #if (K.vec[i]!= 0)
 #  P.mat[i,i]<- 1
#}
###View 1: The investor expects asset 1 to outperform asset 2 by 5% in expected return with a
###confidence of 20%.
###View 2: The investor expects asset 3 to outperform asset 1 by 4% in expected return with
###confidence of 25%.

P.mat1= matrix(c(  0 ,0,  0, 0,  1, 0), nrow=1)
P.mat2= matrix(c(  0 ,0, -1, 0,  0, 0), nrow=1)
P.mat3= matrix(c(  0 ,1,  0, 0,  0, 0), nrow=1)
P.mat4= matrix(c(  0 ,0,  0, 0, -1, 1), nrow=1)
P.mat5= matrix(c( -1 ,0,  0, 0,  1, 0), nrow=1)


P.mat[1,]<- P.mat1
P.mat[2,]<- P.mat2
P.mat[3,]<- P.mat3
P.mat[4,]<- P.mat4
P.mat[5,]<- P.mat5


#------------------------------------------------
#----Creating P (views) matrix pmat3
#------------------------------------------------
if (nrow(cov.mat)>nrow(P.mat)){

N_1<- (nrow(cov.mat))-(nrow(P.mat))


pmat<-matrix(0L, nrow=N_1, ncol=nrow(cov.mat))
pmat3<- rbind(P.mat, pmat)
}
if (nrow(pmat)>nrow(cov.mat)){
C_1<- (-nrow(cov.mat))+(nrow(P.mat))
C_2<- nrow(P.mat)
pmat<- matrix(0L, nrow=C_2 ,ncol= C_1)
pmat3<- cbind(P.mat, pmat)
}

qmat<- c((1+0.02)*er[5], (1-0.01)*er[3] ,(1+0.01)*er[2] ,0.03,0.01) 
conf.mat<- c(0.5, 0.75, 0.25, 0.35, 0.6)

#------------------------------------------------

tau<- 0.025
Omega_notDiagonal<- tau* pmat%*% cov.mat %*% t(pmat)  #----uncertainty
##----checking if omega is singular

install.packages("matrixcalc")
library(matrixcalc)
Kaboom<- is.singular.matrix(pmat3)
if(Kaboom=TRUE):
print("The Pmatrix is singular, we shall use other methods of calculating confidence")

#####-----------------------------------------------------
##Conf<- inv(Omega) ##---confidence, IS SINGULAR IN THIS CASE
install.packages("BLCOP")
library(BLCOP)
colnames(P.mat1)<- colnames(cov.mat)
colnames(P.mat2)<- colnames(cov.mat)

Pi<- R_av*cov.mat%*%w_mrkt
Er_BL_nv<- Pi+Rf_mean

view_1<- BLViews(P=P.mat1, q=qmat[1], confidences=conf.mat[1], assetNames=colnames(cov.mat))
view_2<- BLViews(P=P.mat2, q=qmat[2], confidences=conf.mat[2], assetNames=colnames(cov.mat))
view_3<- BLViews(P=P.mat3, q=qmat[3], confidences=conf.mat[3], assetNames=colnames(cov.mat))
view_4<- BLViews(P=P.mat4, q=qmat[4], confidences=conf.mat[4], assetNames=colnames(cov.mat))
view_5<- BLViews(P=P.mat5, q=qmat[5], confidences=conf.mat[5], assetNames=colnames(cov.mat))

M_Er<- data.frame(t((M_mean)), w_mrkt, er, Re_Eq_1, Re_Eq_2,Re_Eq,  Er_BL_nv)
rownames(M_Er)<- rownames(Re_Eq_1)
colnames(M_Er)<- c("Market Caps USD", "Market Weights", "Historial Est Re", "CAPM with Lamba Calc= 0.7", "CAPM with Lambda from (SR=0.5)=0.9", "CAPM with Lamba from Lit=2.5", "BL No View Re with Lambda=2.5")
K=data.frame()

iterations = 100
variables = 2

output <- matrix(ncol=variables, nrow=iterations)
K=list()
for (i in 1:iterations)
{K[i]<- BLViews(P=P.mat1, q=qmat[1], confidences=i*0.01, assetNames=colnames(cov.mat))
output[i]<- posteriorEst(views=K[i], sigma=as.matrix(cov.mat), mu= as.vector(Pi), tau=0.25, kappa=i*0.01)
}

###-----using prior calculated from average returns
Post_pi_v1<- posteriorEst(views=view_1, sigma=as.matrix(cov.mat), mu= as.vector(Pi), tau=0.25, kappa=conf.mat[1])
Post_CAPM_v1<- posteriorEst(views=view_1, sigma=as.matrix(cov.mat), mu= as.vector(Re_Eq), tau=0.25, kappa=conf.mat[1])
Post_Er_v1<- posteriorEst(views=view_1, sigma=as.matrix(cov.mat), mu= as.vector(er), tau=0.25, kappa=conf.mat[1])

par(mfrow=c(3,1)) 
densityPlots(Post_pi_v1, "MSCI.World", main="Equilibrium returns as prior")
densityPlots(Post_CAPM_v1, "MSCI.World", main="CAPM as Prior")
densityPlots(Post_Er_v1, "MSCI.World", main="Historical Returns as Prior")

###---------------------------------------------------------------------
###----Adding on more views, checking whether nonlinearity exists or not:
###---------------------------------------------------------------------

 
views_2 <- addBLViews(P.mat2, q=qmat[2], conf.mat[2] , view_1)
views_3 <- addBLViews(P.mat3, q=qmat[3], conf.mat[3] , views_2)
views_4 <- addBLViews(P.mat4, q=qmat[4], conf.mat[4] , views_3)
views_5 <- addBLViews(P.mat5, q=qmat[5], conf.mat[5] , views_4)

Post_pi_v2<- posteriorEst(views_2, sigma=as.matrix(cov.mat), mu= as.vector(Pi), tau=0.25)
Post_pi_v3<- posteriorEst(views_3, sigma=as.matrix(cov.mat), mu= as.vector(Pi), tau=0.25)
Post_pi_v4<- posteriorEst(views_4, sigma=as.matrix(cov.mat), mu= as.vector(Pi), tau=0.25)
Post_pi_v5<- posteriorEst(views_5, sigma=as.matrix(cov.mat), mu= as.vector(Pi), tau=0.25)

###-----------Plot Desnity Distributions
par(mfrow=c(2,2))

densityPlots(Post_pi_v1, "MSCI.World", main="Equilibrium returns as prior, View1")
densityPlots(Post_pi_v1, "Goldman.Sachs.Commodity", main="Equilibrium returns as prior,View1")
densityPlots(Post_pi_v2, "MSCI.World", main="Equilibrium returns as prior,View2")
densityPlots(Post_pi_v2, "Goldman.Sachs.Commodity", main="Equilibrium returns as prior,View2")

##---------------------------------------------------------------------------------------------------------
#------bellow is used if the market cap weights are not calculated manually but assuming to know some other market index as using to calculate market cap weight
#-----------------------------------------------------------------------------------------------------------
#marketPosterior<- BLPosterior(as.matrix(Re_dat),views=views, tau=0.5, marketIndex= as.matrix(Re_dat[6]), riskFree= as.matrix(Rf_dat))
#CAPMList(Re_dat, marketIndex = as.matrix(Re_dat[6]), riskFree = Rf_mean, regFunc = "rlm")

optPorts_TP <- optimalPortfolios.fPort(Post_pi_v1, optimizer = "tangencyPortfolio")
optPorts_MV <- optimalPortfolios.fPort(Post_pi_v1)
optPorts_MV_minRisk <- optimalPortfolios.fPort(Post_pi_v1, optimizer= "minriskPortfolio")


par(mfcol = c(2, 3))

weightsPie(w_mrkt)
title("MV Tangency Portfolio Opt, Prior") 

weightsPie(optPorts_TP$posteriorOptimPortfolio)
title(""MV Tangency Portfolio Opt, Posterior") 

weightsPie(optPorts_TP$priorOptimPortfolio)
title("MV Opt Portfolio, Prior")
weightsPie(optPorts_TP$posteriorOptimPortfolio)
title("MV Opt Portfolio, Posterior")

weightsPie(optPorts_MV_minRisk$priorOptimPortfolio)
title("MV min Risk Opt Portfolio, Prior")
weightsPie(optPorts_MV_minRisk$posteriorOptimPortfolio)
title("MV min Risk Opt Portfolio, Posterior")



title("Portfolio Weights Comparison for View 1", outer=TRUE)

##-----------adding on views and calculating weights
optPorts_TP_1 <- optimalPortfolios.fPort(Post_pi_v1, optimizer = "tangencyPortfolio")
optPorts_TP_2 <- optimalPortfolios.fPort(Post_pi_v2, optimizer = "tangencyPortfolio")
optPorts_TP_3 <- optimalPortfolios.fPort(Post_pi_v3, optimizer = "tangencyPortfolio")
optPorts_TP_4 <- optimalPortfolios.fPort(Post_pi_v4, optimizer = "tangencyPortfolio")
optPorts_TP_5 <- optimalPortfolios.fPort(Post_pi_v5, optimizer = "tangencyPortfolio")

par(mfcol = c(5, 1))

weightsPie(optPorts_TP_1$posteriorOptimPortfolio)
title("MV Tangency Portfolio Opt,1 view, Posterior") 

weightsPie(optPorts_TP_2$posteriorOptimPortfolio)
title("MV Tangency Portfolio Opt,2 views, Posterior") 

weightsPie(optPorts_TP_3$posteriorOptimPortfolio)
title("MV Tangency Portfolio Opt,3 views, Posterior") 

weightsPie(optPorts_TP_4$posteriorOptimPortfolio)
title("MV Tangency Portfolio Opt,4 views, Posterior") 

weightsPie(optPorts_TP_5$posteriorOptimPortfolio)
title("MV Tangency Portfolio Opt,5 views, Posterior") 

optPorts_TP_5$posteriorOptimPortfolio
optimalPortfolios.fPort()$posteriorReturns

###----------------------------------------------------------------------------------------------------
#####--------------Calculating the Optimal portfolios under prior and posterior distribution
###----------------------------------------------------------------------------------------------------


library(BLCOP)
library(timeSeries)
library(RUnit)
library(fBasics)
library(fMultivar)
library(fPortfolio)

Opt_BL<- optimalPortfolios(Post_pi, doPlot = TRUE)##---for some reason is total crap(strange, Inv later)


###---------------------------------------------------------------------------------------------------

monthlyReturns<- Re_dat
priorMeans<- er
priorVarcov<-cov.mat

####----------------GET TANGENCY PORTFOLIO (ERIC VERSION)
tangency.portfolio <- 
function(er,cov.mat,risk.free)
{
  # compute tangency portfolio
  #
  # inputs:
  # er				   N x 1 vector of expected returns
  # cov.mat		   N x N return covariance matrix
  # risk.free		 scalar, risk-free rate
  #
  # output is portfolio object with the following elements
  # call			  captures function call
  # er				  portfolio expected return
  # sd				  portfolio standard deviation
  # weights		 N x 1 vector of portfolio weights
  call <- match.call()

  #
  # check for valid inputs
  #
names(er)<- colnames(cov.mat)
risk.free<- Rf_mean
  asset.names <- names(er)
  if(risk.free < 0)
    stop("Risk-free rate must be positive")
  er <- as.vector(er)
  cov.mat <- as.matrix(cov.mat)
  if(length(er) != nrow(cov.mat))
    stop("invalid inputs")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")
  # remark: could use generalized inverse if cov.mat is positive semi-definite

  #
  # compute global minimum variance portfolio
  #
  gmin.port <- globalMin.portfolio(er,cov.mat)
  if(gmin.port$er < risk.free)
    stop("Risk-free rate greater than avg return on global minimum variance portfolio")

  # 
  # compute tangency portfolio
  #
  cov.mat.inv <- solve(cov.mat)
  w.t <- cov.mat.inv %*% (er - risk.free) # tangency portfolio
  w.t <- as.vector(w.t/sum(w.t))	# normalize weights
  names(w.t) <- asset.names
  er.t <- crossprod(w.t,er)
  sd.t <- sqrt(t(w.t) %*% cov.mat %*% w.t)
  tan.port <- list("call" = call,
		   "er" = as.vector(er.t),
		   "sd" = as.vector(sd.t),
		   "weights" = w.t)
  class(tan.port) <- "portfolio"
  tan.port
}

###--------------------------------------------------
###-------GET EFFICIENT FRONTIER---------------------
efficient.frontier <- 
function(er, cov.mat, nport=200, alpha.min=-1, alpha.max=1)
{
  # Compute efficient frontier with no short-sales constraints
  #
  # inputs:
  # er			  N x 1 vector of expected returns
  # cov.mat	  N x N return covariance matrix
  # nport		  scalar, number of efficient portfolios to compute
  #
  # output is a Markowitz object with the following elements
  # call		  captures function call
  # er			  nport x 1 vector of expected returns on efficient porfolios
  # sd			  nport x 1 vector of std deviations on efficient portfolios
  # weights 	nport x N matrix of weights on efficient portfolios 
  call <- match.call()

  #
  # check for valid inputs
  #
  asset.names <- names(er)
  er <- as.vector(er)
  cov.mat <- as.matrix(cov.mat)
  if(length(er) != nrow(cov.mat))
    stop("invalid inputs")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")

  #
  # create portfolio names
  #


  #
  # compute global minimum variance portfolio
  #
  cov.mat.inv <- solve(cov.mat)
  one.vec <- rep(1,length(er))
  port.gmin <- globalMin.portfolio(er,cov.mat)
  w.gmin <- port.gmin$weights

  #
  # compute efficient frontier as convex combinations of two efficient portfolios
  # 1st efficient port: global min var portfolio
  # 2nd efficient port: min var port with ER = max of ER for all assets
  #
  er.max <- max(er)
  port.max <- efficient.portfolio(er,cov.mat,er.max)
  w.max <- port.max$weights    
alpha.min<- -1
alpha.max<- 1
nport<- 200

  port.names <- rep("port",nport)
  ns <- seq(1,nport)
  port.names <- paste(port.names,ns)

  a <- seq(from=alpha.min,to=alpha.max,length=nport)			# convex combinations
  we.mat <- a %o% w.gmin + (1-a) %o% w.max
  cat(" checked and the sum of the row is", sum(we.mat[1,]))	# rows are efficient portfolios
  er.e <- we.mat %*% er							# expected returns of efficient portfolios
  er.e <- as.vector(er.e)
  names(er.e) <- port.names
  cov.e <- we.mat %*% cov.mat %*% t(we.mat) # cov mat of efficient portfolios
  sd.e <- sqrt(diag(cov.e))					# std devs of efficient portfolios
  sd.e <- as.vector(sd.e)
  names(sd.e) <- port.names
  dimnames(we.mat) <- list(port.names,asset.names)

  # 
  # summarize results
  #
  ans <- list("call" = call,
	      "er" = er.e,
	      "sd" = sd.e,
	      "weights" = we.mat)
  class(ans) <- "Markowitz"
  ans
}

#
# print method for portfolio object
print.portfolio <- function(x, ...)
{
  cat("Call:\n")
  print(x$call, ...)
  cat("\nPortfolio expected return:    ", format(x$er, ...), "\n")
  cat("Portfolio standard deviation: ", format(x$sd, ...), "\n")
  cat("Portfolio weights:\n")
  print(round(x$weights,4), ...)
  invisible(x)
}

#
# summary method for portfolio object
summary.portfolio <- function(object, risk.free=NULL, ...)
# risk.free			risk-free rate. If not null then
#				compute and print Sharpe ratio
# 
{
  cat("Call:\n")
  print(object$call)
  cat("\nPortfolio expected return:    ", format(object$er, ...), "\n")
  cat(  "Portfolio standard deviation: ", format(object$sd, ...), "\n")
  if(!is.null(risk.free)) {
    SharpeRatio <- (object$er - risk.free)/object$sd
    cat("Portfolio Sharpe Ratio:       ", format(SharpeRatio), "\n")
  }
  cat("Portfolio weights:\n")
  print(round(object$weights,4), ...)
  invisible(object)
}
# hard-coded 4 digits; prefer to let user control, via ... or other argument

#
# plot method for portfolio object
plot.portfolio <- function(object, ...)
{
  asset.names <- names(object$weights)
  barplot(object$weights, names=asset.names,
	  xlab="Assets", ylab="Weight", main="Portfolio Weights", ...)
  invisible()
}

#
# print method for Markowitz object
print.Markowitz <- function(x, ...)
{
  cat("Call:\n")
  print(x$call)
  xx <- rbind(x$er,x$sd)
  dimnames(xx)[[1]] <- c("ER","SD")
  cat("\nFrontier portfolios' expected returns and standard deviations\n")
  print(round(xx,4), ...)
  invisible(x)
}
# hard-coded 4, should let user control

#
# summary method for Markowitz object
summary.Markowitz <- function(object, risk.free=NULL)
{
  call <- object$call
  asset.names <- colnames(object$weights)
  port.names <- rownames(object$weights)
  if(!is.null(risk.free)) {
    # compute efficient portfolios with a risk-free asset
    nport <- length(object$er)
    sd.max <- object$sd[1]
    sd.e <- seq(from=0,to=sd.max,length=nport)	
    names(sd.e) <- port.names

    #
    # get original er and cov.mat data from call 
    er <- eval(object$call$er)
    cov.mat <- eval(object$call$cov.mat)

    #
    # compute tangency portfolio
    tan.port <- tangency.portfolio(er,cov.mat,risk.free)
    x.t <- sd.e/tan.port$sd		# weights in tangency port
    rf <- 1 - x.t			# weights in t-bills
    er.e <- risk.free + x.t*(tan.port$er - risk.free)
    names(er.e) <- port.names
    we.mat <- x.t %o% tan.port$weights	# rows are efficient portfolios
    dimnames(we.mat) <- list(port.names, asset.names)
    we.mat <- cbind(rf,we.mat) 
  }
  else {
    er.e <- object$er
    sd.e <- object$sd
    we.mat <- object$weights
  }
  ans <- list("call" = call,
	      "er"=er.e,
	      "sd"=sd.e,
	      "weights"=we.mat)
  class(ans) <- "summary.Markowitz"	
  ans
}

print.summary.Markowitz <- function(x, ...)
{
	xx <- rbind(x$er,x$sd)
	port.names <- names(x$er)
	asset.names <- colnames(x$weights)
	dimnames(xx)[[1]] <- c("ER","SD")
	cat("Frontier portfolios' expected returns and standard deviations\n")
	print(round(xx,4), ...)
	cat("\nPortfolio weights:\n")
	print(round(x$weights,4), ...)
	invisible(x)
}
# hard-coded 4, should let user control

#
# plot efficient frontier
#
# things to add: plot original assets with names
# tangency portfolio
# global min portfolio
# risk free asset and line connecting rf to tangency portfolio
#
plot.Markowitz <- function(object, plot.assets=FALSE, ...)
# plot.assets		logical. If true then plot asset sd and er
{
  if (!plot.assets) {
     y.lim=c(0,max(object$er))
     x.lim=c(0,max(object$sd))
     plot(object$sd,object$er,type="b",xlim=x.lim, ylim=y.lim,
          xlab="Portfolio SD", ylab="Portfolio ER", 
          main="Efficient Frontier", ...)
     }
  else {
	  call = object$call
	  mu.vals = eval(call$er)
	  sd.vals = sqrt( diag( eval(call$cov.mat) ) )
	  y.lim = range(c(0,mu.vals,object$er))
	  x.lim = range(c(0,sd.vals,object$sd))
	  plot(object$sd,object$er,type="b", xlim=x.lim, ylim=y.lim,
          xlab="Portfolio SD", ylab="Portfolio ER", 
          main="Efficient Frontier", ...)
        text(sd.vals, mu.vals, labels=names(mu.vals))
  }
  invisible()
}
P1<- efficient.frontier(er, cov.mat, alpha.min=-20, alpha.max=+20, nport=10000)

plot.Markowitz(P1)
###--------------------------------------------------
###----------Function GETPORTFOLIO
getPortfolio <-
function(er, cov.mat, weights)
{
	# contruct portfolio object
	#
	# inputs:
	# er				   N x 1 vector of expected returns
	# cov.mat  		 N x N covariance matrix of returns
	# weights			 N x 1 vector of portfolio weights
	#
	# output is portfolio object with the following elements
	# call				original function call
	# er				  portfolio expected return
	# sd				  portfolio standard deviation
	# weights			N x 1 vector of portfolio weights
	#
	call <- match.call()
	
	#
	# check for valid inputs
	#
	asset.names <- names(er)
	weights <- as.vector(weights)
	names(weights) = names(er)
  	er <- as.vector(er)					# assign names if none exist
	if(length(er) != length(weights))
		stop("dimensions of er and weights do not match")
 	cov.mat <- as.matrix(cov.mat)
	if(length(er) != nrow(cov.mat))
		stop("dimensions of er and cov.mat do not match")
	if(any(diag(chol(cov.mat)) <= 0))
		stop("Covariance matrix not positive definite")
		
	#
	# create portfolio
	#
	er.port <- crossprod(er,weights)
	sd.port <- sqrt(weights %*% cov.mat %*% weights)
	ans <- list("call" = call,
	      "er" = as.vector(er.port),
	      "sd" = as.vector(sd.port),
	      "weights" = weights) 
	class(ans) <- "portfolio"
	ans
}

###--------------------------------------------------------------








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




###------------------------------------------------------------------------------------
#####-------- SIT version: Compute Posterior estimates of returns and covariance
###------------------------------------------------------------------------------------

# He & Litterman: The intuition Behind Black- Litterman Model Portfolios
# formulas (8), (9), (10)
# compute the posterior estimate of the returns and covariance
#' @export 

bl.compute.posterior <- function
	(mu <- Re_Eq ,		# Equilibrium returns
	cov<- cov.mat	,	# Covariance matrix
	pmat=P.mat1 ,	# Views pick matrix
	qmat[1] ,	# View mean vector
	tau=0.25	,# Measure of uncertainty of the prior estimate of the mean returns
	confidences=conf.mat[1]  # Confidence of each view
)
#qmat<- t(matrix(c(0.25, 0.45, rep(0 , 5))))
#colnames(qmat[1])<- colnames(cov.mat)

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
	
		out$expected.return = temp %*% (solve(tau * cov) %*% mu + t(pmat) %*% solve(omega) %*% qmat[1])
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

SIT_Post_pi<- bl.compute.posterior(mu, cov, P.mat1, qmat[1], tau, confidences)
SIT_Post_pi_Omega <- bl.compute.posterior(mu, cov, P.mat1, qmat[1], tau, confidences=NULL)
SIT_Post_pi_NULL <- bl.compute.posterior(mu, cov, NULL, NULL, tau, confidences=NULL)

Re_SIT_v1<- SIT_Post_pi$expected.return  ###-----expected returns of Black Litterman
Re_SIT_v1_Omega<- SIT_Post_pi_Omega$expected.return
Re_SIT_NULL<- SIT_Post_pi_NULL$expected.return
1:
risk.aversion<- R_av
cap.weight<- w_mrkt
risk.free<- Rf_mean

SIT_Post_pi_v1<- bl.compute.posterior(mu, cov, P.mat1, qmat[1], tau, conf.mat[1])
SIT_Post_pi_v2<- bl.compute.posterior(mu, cov, P.mat[1:2,], qmat[1:2], tau, conf.mat[1:2])
SIT_Post_pi_v3<- bl.compute.posterior(mu, cov, P.mat[1:3,], qmat[1:3], tau, conf.mat[1:3])
SIT_Post_pi_v4<- bl.compute.posterior(mu, cov, P.mat[1:4,], qmat[1:4], tau, conf.mat[1:4])
SIT_Post_pi_v5<- bl.compute.posterior(mu, cov, P.mat[1:5,], qmat[1:5], tau, conf.mat[1:5])

iterations = 100
variables = 2

output <- matrix(ncol=variables, nrow=iterations)


###-----Populating Returns based on confidences
K<- data.frame()
K <- data.frame(matrix(0, nrow=4, ncol=4))
pd <- data.frame(matrix(0, nrow=4, ncol=4))
Date<- as.list(seq(1,100,0.01))
m=1
n=1
for (i in 1:m){
  for (j in 1:n){
       #K[(6*(i-1)+1):(6*i) , 1] <-(i*0.01)



#K[1:6 , 1] <- (1*0.1)

       Res<- bl.compute.posterior(mu, cov, P.mat1, qmat[1], tau=i*0.01, j*0.01)
       K[(6*(i-1)+1):(6*i) , j] <- Res$expected.return 
       #pd[(6*(i-1)+1):(6*i) , j]<- $expected.return 

  
#colnames(K[,j])<- (j*0.01)
#rownames(K[(6*(i-1)+1):(6*i)],) <- rownames(cov)
rownames(K[(6*(i-1)+1):(6*i),])<- seq(1,6)

print(rownames(K[(6*(i-1)+1):(6*i),]))
rownames(K[(6*(i-1)+1):(6*i),]) <- as.list(rownames(cov))
print(rownames(K[(6*(i-1)+1):(6*i),]))
print(K[(6*(i-1)+1):(6*i),])
}

}
colnames(K)<- seq(1,4)


p5 <- ggplot(K, aes(x = Date, y = Home.Value))
p5 + geom_line(aes(color = State))


#cat("confidence is" j*0.01)
Noview<- bl.compute.posterior(mu, cov, NULL, NULL, tau,NULL)
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
 