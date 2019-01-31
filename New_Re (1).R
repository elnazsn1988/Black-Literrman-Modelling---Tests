library("quadprog")
library(BLCOP) 
library("MASS")
library(xlsx)

library(portfolio)
install.packages("ggplot2")
library(ggplot2)


library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

install.packages("XLConnect")
install.packages("data.table")
library(XLConnect)
library(data.table)
install.packages("dplyr")
library(dplyr)

install.packages("htmlTable")
library("htmlTable")
install.packages("ReporteRs")
library(ReporteRs)

library(magrittr)
library(lubridate)
install.packages("matrixStats")
library(matrixStats)

install.packages("microbenchmark")
library("microbenchmark")

library("factoextra")
install.packages("pryr")
library(pryr)
install.packages("caret")
install.packages("e1071")
library(e1071)
library(caret)

require(gridExtra)
library(ReporteRs)

install.packages("prodlim")
library(prodlim)

require(reshape2)
library(gridExtra)

install.packages("compare")
library(compare)
install.packages("tseries")
library(tseries)
install.packages("roll")
library("roll")

install.packages("stargazer")
library(stargazer)



Bind_Down<- function(X, Y){
###Defining function of X and Y, where X<<<Y

New_Re_Green_Complete_Y<-  (year(rownames(X)))
New_Re_Green_Complete_M<- month(rownames(X))

New_Re_Green_Complete_dates<- data.frame(cbind(New_Re_Green_Complete_M, New_Re_Green_Complete_Y))
colnames(New_Re_Green_Complete_dates)<- c("M" , "Y")

New_Re_Y<- (year(rownames(Y)))
New_Re_M<- month(rownames(Y))

New_Re_dates<- data.frame(cbind(New_Re_M, New_Re_Y))
colnames(New_Re_dates)<- c("M" , "Y")

New_Re_SameD<- inner_join(New_Re_dates, New_Re_Green_Complete_dates)

 if (nrow(New_Re_SameD)!=length(New_Re_dates)){
cat("The Green and Conventional Matrixes were not the same size, Conv was ",(nrow(New_Re_Green_Complete_dates)-nrow(New_Re_dates)), "more than green, so matched dates extracted")
}

Y$Date<- rownames(Y)
Comp_Y<- data.frame(matrix(, nrow=nrow(New_Re_SameD), ncol= ncol(Y)))
Comp_X<- data.frame(matrix(, nrow=nrow(New_Re_SameD), ncol= ncol(X)))

 for (i in 1:nrow(New_Re_SameD)){
Comp_Y[i,]<- subset(Y, ( (month(rownames(Y)))==New_Re_SameD[i,1] & year(rownames(Y))==New_Re_SameD[i,2]))
}

 for (i in 1:nrow(New_Re_SameD)){
Comp_X[i,]<- subset(X, ( (month(rownames(X)))==New_Re_SameD[i,1] & year(rownames(X))==New_Re_SameD[i,2]))
}

rownames(Comp_Y)<- Comp_Y[, ncol( Comp_Y)]
colnames(Comp_Y)<- colnames(Y)
Comp_Y["Date"]<- NULL

rownames(Comp_X)<- Comp_X[, ncol( Comp_X)]
colnames(Comp_X)<- colnames(X)
Comp_X["Date"]<- NULL

if(nrow(Comp_Y)!=nrow(Comp_X)){
cat("Nope, The Green and Conventional Matrixes are not matched correctly, Conv has ",(nrow(Comp_Y)-nrow(Comp_X)), "more than green, fix it")
}

return(data.frame(cbind(Comp_Y,Comp_X)))

}


setwd("C:\\Program Files\\R\\R-3.4.1")

Re_DS_con1<- read.xlsx("New_1.xlsx", sheetName ="DS_Conventional")
Re_DS_Green1<-read.xlsx("New_1.xlsx",sheetName ="DS_GreenEq")

###-------------------------
###------Cleaning up sheet 1
###-------------------------

N1<- ncol(Re_DS_con1)
Re_DS_con<- Re_DS_con1[,2:N1 ]
rownames(Re_DS_con)<- as.Date(Re_DS_con1[,1])
colnames(Re_DS_con1)

N<- ncol(Re_DS_con)

###in case later needed to extract all words from column names:
###----MAKE SURE DPLYR IS INSTALLED CORRECTLY, IS BIT MOODY
MK_names<- names(Re_DS_con %>% select(contains("MARKET.VALUE")))
market_value1<- Re_DS_con[names(Re_DS_con %>% select(contains("MARKET.VALUE")))]

New_Re1<-Re_DS_con[ , !(names(Re_DS_con) %in% MK_names)]
head(New_Re1)

New_Re2<- data.frame()
for (i in 2:nrow(New_Re1)){
    for (j in 1:ncol(New_Re1)){
New_Re2[i-1,j]<- (((New_Re1[i,j]-New_Re1[(i-1),j])/New_Re1[(i-1),j]))
}
}
colMeans(New_Re2)
New_RF<- New_Re2[,1]
nrow(New_Re2)
rownames(New_Re2)<- as.Date(rownames(New_Re1[2:nrow(New_Re1),]))
colnames(New_Re2)<- c(colnames(New_Re1[,1:ncol(New_Re1)]))

New_Re2<- New_Re2[,2:ncol(New_Re2)]

#New_Re<- data.frame(New_Re2[,2],New_Re2[,5],New_Re2[,6],New_Re2[,11])
New_Re<- New_Re2[complete.cases(data.frame(New_Re2)),]
#rownames(New_Re)<- as.Date(rownames(New_Re2))
#colnames(New_Re)<- c(colnames(New_Re2)[2], colnames(New_Re2)[5], colnames(New_Re2)[6], colnames(New_Re2)[11])
colnames(New_Re)<- c(colnames(New_Re2))

market_value<- data.frame()
###selecting columns we want
##market_value1<- cbind(market_value1[,1],market_value1[,2],market_value1[,3],market_value1[,5])
market_value<- data.frame(market_value1[-1,1],market_value1[-1,2],market_value1[-1,3],market_value1[-1,5])
colnames(market_value)<- c(colnames(market_value1)[1], colnames(market_value1)[2], colnames(market_value1)[3], colnames(market_value1)[5])
rownames(market_value)<- as.Date(rownames(market_value1[-1,]))

head(market_value1)
head(market_value)

###----getting shortenned names 

R_names<- list()
Data_D<- list()
N<- ncol(market_value)
for (i in 1:N){

R_names=  (paste(sapply(strsplit(as.character(colnames(market_value)[i]),"\\."), `[`, 1:3), collapse= " "))

Data_D[[i]]<- R_names
All_dat<- do.call(rbind,Data_D)
}
colnames(market_value)<- All_dat

R_names5<- list()
Data_D5<- list()
N5<- ncol(New_Re)
for (i in 1:N5){

R_names5=  (paste(sapply(strsplit(as.character(colnames(New_Re)[i]),"\\."), `[`, 1:3), collapse= " "))

Data_D5[[i]]<- R_names5
All_dat5<- do.call(rbind,Data_D5)
}
colnames(New_Re)<- NULL
colnames(New_Re)<- All_dat5

head(New_Re)
###----DOCUMENT 

mydocA <- docx( )

mydocA<- addTitle( mydocA, value=paste("All Conventional and Green indices, Statistical Study" ), level = 1 )   
mydocA<- addParagraph(mydocA, value= "As discussed, the selected data to study are listed bellow, along with abreviated names, and returns calculated annually from total returns listed in Excel Sheet", stylename = "DocDefaults")
mydocA<- addFlexTable(  mydocA,
              (FlexTable( as.matrix(colnames(New_Re)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

             


###----Getting Density Plots for CONVENTIONAL	

New_Re<- data.frame(New_Re)
mtlong <- reshape2::melt(New_Re)

P_New_Hist<- ggplot(mtlong, aes(value)) + facet_wrap(~variable, scales = 'free_x') +
  geom_density()
geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))

mydocA<- addTitle( mydocA, value=paste("Density Plots of Conventional Data Used" ), level = 1 )   
mydocA<- addPlot( mydocA , fun=print, x=P_New_Hist)   
#mydocA<- addPlot( mydocA , fun=print, x=ggplot(New_Re))   

#writeDoc(mydocA, file = 'Testing.docx')
#browseURL("Testing.docx")

###-------------------------
###------Cleaning up sheet 2
###-------------------------

N1<- ncol(Re_DS_Green1)
Re_DS_Green2<- Re_DS_Green1[,2:N1 ]
rownames(Re_DS_Green2)<- as.Date(Re_DS_Green1[[1]])

N<- ncol(Re_DS_Green2)

###in case later needed to extract all words from column names:

MK_names_DS_GRN<- names(Re_DS_Green2 %>% select(contains("MARKET.VALUE")))
market_value_DS_GRN<- Re_DS_Green2[names(Re_DS_Green2 %>% select(contains("MARKET.VALUE")))]

head(market_value_DS_GRN)

New_Re_DS_GRN1<- data.frame()

New_Re_DS_GRN1<-(Re_DS_Green2[ , !(names(Re_DS_Green2) %in% MK_names_DS_GRN)])
New_Re_DS_GRN <- data.frame()


for (i in 2:nrow(New_Re_DS_GRN1)){
    for (j in 1:ncol(New_Re_DS_GRN1))
New_Re_DS_GRN[i-1,j]<- (((New_Re_DS_GRN1[i,j]-New_Re_DS_GRN1[(i-1),j])/New_Re_DS_GRN1[(i-1),j]))
}

colnames(New_Re_DS_GRN)<- colnames(New_Re_DS_GRN1)

rownames(New_Re_DS_GRN)<- as.Date(rownames(Re_DS_Green2[-1,]))

New_Re_DS_GRN<- New_Re_DS_GRN[,1:5]

##-----08-10-2017 Dealing with S&P Green data

Re_SP_Green<-data.frame(read.xlsx("New_1.xlsx", sheetName= "Sheet1", detectDate=TRUE))
Re_SP_Green_mv<-data.frame(read.xlsx("New_1.xlsx", sheetName= "Sheet3"))

SP_Gr_I<- Re_SP_Green[,1:2]
SP_CE_TR<- Re_SP_Green[,3:4]
SP_FF_TR<- Re_SP_Green[,7:8]

SP_Gr_I<- SP_Gr_I[complete.cases(SP_Gr_I), ]
colnames(SP_Gr_I)<- c("Dates", "GR_I")

SP_CE_TR<- SP_CE_TR[complete.cases(SP_CE_TR), ]
colnames(SP_CE_TR)<- c("Dates", "SP_CE_TR")

SP_FF_TR<- SP_FF_TR[complete.cases(SP_FF_TR), ]
colnames(SP_FF_TR)<- c("Dates", "SP_FF_TR")


Dates<- SP_Gr_I[,1]

D1<- SP_Gr_I %>% 
  group_by(strftime(Dates, "%Y-%m")) %>% #Groups by the yearmonths
  filter(Dates == max(Dates)) %>%        #Take the last date of each group
  .$Dates                              #Returns the filtered dates as a vector

D2<- SP_CE_TR %>% 
  group_by(strftime(Dates, "%Y-%m")) %>% #Groups by the yearmonths
  filter(Dates == max(Dates)) %>%        #Take the last date of each group
  .$Dates                              #Returns the filtered dates as a vector

D3<- SP_FF_TR %>% 
  group_by(strftime(Dates, "%Y-%m")) %>% #Groups by the yearmonths
  filter(Dates == max(Dates)) %>%        #Take the last date of each group
  .$Dates                              #Returns the filtered dates as a vector



SP_Gr_I<- SP_Gr_I[SP_Gr_I$Dates %in% as.Date(D1),]
rownames(SP_Gr_I)<- format( SP_Gr_I$Dates, format="%Y-%m")
SP_Gr_I<- data.frame(SP_Gr_I)
SP1<- SP_Gr_I[,2]
S1<- data.frame(SP1)
rownames(S1) <- format( SP_Gr_I$Dates, format="%Y-%m")


SP_CE_TR<- SP_CE_TR[SP_CE_TR$Dates %in% as.Date(D2),]
rownames(SP_CE_TR)<- format( SP_CE_TR$Dates, format="%Y-%m")
SP_CE_TR<- data.frame(SP_CE_TR)
SP2<- SP_CE_TR[,2]
S2<- data.frame(SP2)
rownames(S2) <- format( SP_CE_TR$Dates, format="%Y-%m")

SP_FF_TR<- SP_FF_TR[SP_FF_TR$Dates %in% as.Date(D3),]
rownames(SP_FF_TR)<- format( SP_FF_TR$Dates, format="%Y-%m")
SP_FF_TR<- data.frame(SP_FF_TR)
SP3<- SP_FF_TR[,2]
S3<- data.frame(SP3)
rownames(S3) <- format( SP_FF_TR$Dates, format="%Y-%m")
 
S_a<- data.frame(merge(S1, S2, by="row.names"))

#S_a<- cbind(S2, S1[2:nrow(S1),])
S3$Row.names<- rownames(S3)
SP<-  data.frame(merge(S_a, S3, by="Row.names"))
rownames(SP)<-SP$Row.names
SP$Row.names<- NULL
S3$Row.names<- NULL
SP<- SP[c(2,3,1)]

colnames(SP)<- colnames(Re_SP_Green_mv)

###for list later:

colnames(S1)<- ("S.P.Green.Bond.Index")
colnames(S2)<- ("S.P.Global.1200.Carbon.Efficient.Index.TR")
colnames(S3)<- ("S.P.Global.1200.Fossil.Fuel.Free.Index.TR")



S1_tr<- data.frame()

S2_tr<- data.frame()

S3_tr<- data.frame()


for (i in 1:nrow(S1)){
    for (j in 1:ncol(S1))
S1_tr[i-1,j]<- (((S1[i,j]-S1[(i-1),j])/S1[(i-1),j]))
}

rownames(S1_tr)<- rownames(S1) [2:length(rownames(S1))]

for (i in 1:nrow(S2)){
    for (j in 1:ncol(S2))
S2_tr[i-1,j]<- (((S2[i,j]-S2[(i-1),j])/S2[(i-1),j]))
}
rownames(S2_tr)<- rownames(S2) [2:length(rownames(S2))]


for (i in 1:nrow(S3)){
    for (j in 1:ncol(S3))
S3_tr[i-1,j]<- (((S3[i,j]-S3[(i-1),j])/S3[(i-1),j]))
}
rownames(S3_tr)<- rownames(S3) [2:length(rownames(S3))]



SP_tr<- data.frame()

for (i in 1:nrow(SP)){
    for (j in 1:ncol(SP))
SP_tr[i-1,j]<- (((SP[i,j]-SP[(i-1),j])/SP[(i-1),j]))
}

colnames(SP_tr)<- colnames(SP)
rownames(SP_tr)<- rownames(SP[-1,])


###----------------------------
###----getting shortenned names 
###----------------------------


R_names<- list()
Data_D<- list()
N<- ncol(market_value_DS_GRN)
for (i in 1:N){

R_names=  (paste(sapply(strsplit(as.character(colnames(market_value_DS_GRN)[i]),"\\."), `[`, 1:3), collapse= " "))

Data_D[[i]]<- R_names
All_dat<- do.call(rbind,Data_D)
}
colnames(market_value_DS_GRN)<- NULL
colnames(market_value_DS_GRN)<- All_dat

R_names2<- list()
Data_D2<- list()
N2<- ncol(New_Re_DS_GRN)
for (i in 1:N2){

R_names2=  (paste(sapply(strsplit(as.character(colnames(New_Re_DS_GRN)[i]),"\\."), `[`, 1:3), collapse= " "))

Data_D2[[i]]<- R_names2
All_dat2<- do.call(rbind,Data_D2)
}
colnames(New_Re_DS_GRN)<- NULL
colnames(New_Re_DS_GRN)<- All_dat2


R_names3<- list()
Data_D3<- list()
N3<- ncol(SP_tr)
for (i in 1:N3){

R_names3<-  (paste(sapply(strsplit(as.character(colnames(SP_tr)[i]),"\\."), `[`, 1:3), collapse= " "))

Data_D3[[i]]<- R_names3
All_dat3<- do.call(rbind,Data_D3)
}
colnames(SP_tr)<- NULL
colnames(SP_tr)<- All_dat3






mydocA<- addTitle( mydocA, value=paste("Selected Green Data to Use" ), level = 1 ) 
mydocA<- addParagraph(mydocA, value= "Not all the green data used is complete, many inlcude NA values. Two approaches are taken, firstly, using each index's complete value (excluding all NANS) for calculations of Standard deviations and means, and second, using the minimum available complete section of green indices for calculation of covariances, which require vectors of equal lengths.
The first few columns of each dataset are shown here as an example. For the S&P data, no NANS exist, and density plots are drawn for the dataset.", stylename = "DocDefaults")
mydocA<- addFlexTable(  mydocA,
              (FlexTable( head(New_Re_DS_GRN), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))



###----Getting GREEN Density Plots

New_Re_DS_GRN<- data.frame(New_Re_DS_GRN)
mtlong <- reshape2::melt(New_Re_DS_GRN)

P_Green_Density<- ggplot(mtlong, aes(value)) + facet_wrap(~variable, scales = 'free_x') +
  geom_density()

New_Re_Green_Complete<- New_Re_DS_GRN[complete.cases(New_Re_DS_GRN), ]

market_value_Green_Complete<- New_Re_DS_GRN[complete.cases(New_Re_DS_GRN), ]

mydocA<- addTitle( mydocA, value=paste("Green Data Density Plots" ), level = 1 ) 
mydocA<- addPlot( mydocA , fun=print, x=P_Green_Density)  
mydocA<- addTitle( mydocA, value=paste("Green Data, Complete Dataset" ), level = 1 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( head(New_Re_Green_Complete), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


###----Getting S&P Density Plots

SP_tr<- data.frame(SP_tr)
mtlong <- reshape2::melt(SP_tr)

P_SP_Density<- ggplot(mtlong, aes(value)) + facet_wrap(~variable, scales = 'free_x') +
  geom_density()

mydocA<- addTitle( mydocA, value=paste("S&P Green Density Plots" ), level = 1 ) 
mydocA<- addPlot( mydocA , fun=print, x=P_Green_Density)  
mydocA<- addTitle( mydocA, value=paste("S&P Green Data, Complete Dataset" ), level = 1 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( head(SP_tr), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))






###------------------------------------------------------------------
###------------------------------------------------------------------
###----ALL STAT WORK FOR GREEN AND CONV EXCLUDING SHEET 3 (S&P)
###------------------------------------------------------------------
###------------------------------------------------------------------


Green_ER<- colMeans(New_Re_Green_Complete)
Green_ALL_ER<- colMeans(New_Re_DS_GRN, na.rm = TRUE)
Conv_ER<- colMeans(New_Re)
SP_ER<-colMeans(SP_tr)

Cov_Conv<- cov(New_Re)
Cor_Conv<- cor(New_Re)

Cov_SP<- cov(SP_tr)
Cor_SP<- cor(SP_tr)

Cov_GR<- cov(New_Re_Green_Complete)
rownames(Cov_GR)<- All_dat2
colnames(Cov_GR)<- All_dat2

Cor_GR<- cor(New_Re_Green_Complete)
rownames(Cor_GR)<- All_dat2
colnames(Cor_GR)<- All_dat2

mydocA<- addTitle( mydocA, value=paste("Covariance, Correlations and Expected Returns" ), level = 1 ) 

mydocA<- addTitle( mydocA, value=paste("Expected Returns of Conventional data" ), level = 2 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( data.frame(round(Conv_ER,4)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


mydocA<- addTitle( mydocA, value=paste("Expected Returns of Completed Green data" ), level = 2 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( data.frame(round(Green_ER,4)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


mydocA<- addTitle( mydocA, value=paste("Expected Returns of All Green data" ), level = 2 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( data.frame(round(Green_ALL_ER,4)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))



mydocA<- addTitle( mydocA, value=paste("Expected Returns of All S&P Data" ), level = 2 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( data.frame(round(SP_ER,4)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))





mydocA<- addTitle( mydocA, value=paste("Green Covariance" ), level = 2 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( round(Cov_GR,4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

mydocA<- addTitle( mydocA, value=paste("Green Corelation" ), level = 2 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( round(Cor_GR,4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


mydocA<- addTitle( mydocA, value=paste("Conventional Covariance" ), level = 2 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( round(Cov_Conv,4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

mydocA<- addTitle( mydocA, value=paste("Conventional Corelation" ), level = 2 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( round(Cor_Conv,4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


mydocA<- addTitle( mydocA, value=paste("S&P Covariance" ), level = 2 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( round(cov(SP_tr),4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

mydocA<- addTitle( mydocA, value=paste("S&P Corelation" ), level = 2 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( round(cor(SP_tr),4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))




##-------PRINCIPAL COMPONENT ANALYSIS
fit_C <- princomp(Cor_Conv, cor=TRUE)
summary(fit_C) # print variance accounted for 
loadings(fit_C) # pc loadings 
p1_Conv<- plot(fit_C,type="lines") # scree plot 
mydocA<- addTitle( mydocA, value=paste("Principal Component Analysis"), level = 1 ) 
#mydocA<- addTitle( mydocA, value=paste("Conventional PCA, Scree Plot" ), level = 2 ) 
#mydocA<- addPlot( mydocA , fun=print, x=plot(fit_C,type="lines"))  

fit_C$scores # the principal components
p2_Conv<- biplot(fit_C)
#mydocA<- addTitle( mydocA, value=paste("Conventional PCA, Biplot" ), level = 2 ) 
#mydocA<- addPlot( mydocA , fun=print, x=biplot(fit_C))  


fit_G <- princomp(Cor_GR, cor=TRUE)
summary(fit_G) # print variance accounted for 
loadings(fit_G) # pc loadings 
plot(fit_G,type="lines") # scree plot 
#mydocA<- addTitle( mydocA, value=paste("Green PCA, Scree Plot" ), level = 2 ) 
#mydocA<- addPlot( mydocA , fun=print, x=plot(fit_G,type="lines"))  

fit_G$scores # the principal components
biplot(fit_G)
#mydocA<- addTitle( mydocA, value=paste("Green PCA, Biplot" ), level = 2 ) 
#mydocA<- addPlot( mydocA , fun=print, x=biplot(fit_G))  



#cbind.fill(New_Re, New_Re_Green_Complete)
#head( New_Re_Green_Complete)

p333.pryr %<a-% { plot(New_Re)}
p444.pryr %<a-% {plot(New_Re_Green_Complete)}
p555.pryr %<a-% {plot(SP_tr)}

mydocA<- addTitle( mydocA, value=paste("Conventional, Green and S&P Indices illustrated corrleations" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=function() p333.pryr) 
mydocA<- addPlot( mydocA , fun=function() p444.pryr) 
mydocA<- addPlot( mydocA , fun=function() p555.pryr) 





Conv_pca <- prcomp(New_Re , scale = TRUE)
GR_pca <- prcomp(New_Re_Green_Complete , scale = TRUE)
SP_pca <- prcomp(SP_tr , scale = TRUE)

T1<- (summary(Conv_pca))
mydocA<- addTitle( mydocA, value=paste("Conventional PCA, Summary" ), level = 2 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( data.frame(round(T1$importance,4)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))


T2<- (summary(GR_pca))

mydocA<- addTitle( mydocA, value=paste("Green PCA, Summary" ), level = 2 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( data.frame(round(T2$importance,4)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))



T3<- (summary(SP_pca))

mydocA<- addTitle( mydocA, value=paste("S&P PCA, Summary" ), level = 2 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( data.frame(round(T3$importance,4)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))



p3b.pryr %<a-% { biplot(Conv_pca, scale.=0)}
#mydocA<- addTitle( mydocA, value=paste("Conventional Biplot" ), level = 2 ) 
#mydocA<- addPlot( mydocA , fun=print, x=biplot(Conv_pca, scale.=0))  

p4b.pryr %<a-% {biplot(GR_pca, scale.=0)}
#mydocA<- addTitle( mydocA, value=paste("Green Biplot" ), level = 2 ) }
#mydocA<- addPlot( mydocA , fun=print, x=biplot(GR_pca, scale.=0))  

p5b.pryr %<a-% { biplot(SP_pca, scale.=0)}
#writeDoc(mydocA, file = 'Testing.docx')
#browseURL("Testing.docx")



mydocA<- addTitle( mydocA, value=paste("PCA Analysis: Outlying/ Main Affecting Date Identification" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=function() p3b.pryr) 
mydocA<- addPlot( mydocA , fun=function() p4b.pryr) 
mydocA<- addPlot( mydocA , fun=function() p5b.pryr) 



###-----------------------------------------------------------
###-------Using binding function for here and later use:
###-----------------------------------------------------------
#install.packages("rje")
#library(rje)



#R_c<-  colnames(New_Re)

#New_Re_DS_GRN<- New_Re_DS_GRN[,-(ncol(New_Re_DS_GRN))]
#SP_tr<- SP_tr

#R_G<-  colnames(New_Re_DS_GRN)
#R_SP<- colnames(SP_tr)


#All_n<- c(R_c,R_G,R_SP)

#Ccols<- list()

#for (i in 2:length(All_n)){
#cols <- combn( (All_n), i)
#Ccols[[i]]<- cols
#}



#GG1<- rbind(as.matrix(colnames(New_Re_DS_GRN)),as.matrix(colnames(New_Re)),as.matrix(colnames(SP_tr)))
#colnames(SP_tr)
#colnames(New_Re)



#GG<- colnames(New_Re)
#Var<- list()
#for (i in 1:ncol(New_Re)){
#Var[i]<- paste0(GG[i])
#K<- data.frame( New_Re[,i] )

#DD<- as.Date(rownames(New_Re))
#DD_C<- format( DD, "%Y-%m")

#rownames(K)<- DD_C
#colnames(K)<- GG[i]
#assign(Var[[i]], K)

#}



#GG<- colnames(New_Re_DS_GRN)
#Var_G<- list()
#for (i in 1:ncol(New_Re_DS_GRN)){
#Var_G[i]<- paste0(GG[i])
#K<- data.frame( New_Re_DS_GRN[,i] )

#DD<- as.Date(rownames(New_Re_DS_GRN))
#DD_G<- format( DD, format="%Y-%m")

#rownames(K)<- DD_G
#colnames(K)<- GG[i]
#assign(Var_G[[i]], K)

#}

#GG<- colnames(SP_tr)
#Var_SP<- list()
#for (i in 1:ncol(SP_tr)){
#Var_SP[i]<- paste0(GG[i])
#K<- data.frame( SP_tr[,i] )
#DD<- as.Date(rownames(SP_tr))
#DD_SP<- format( DD, format="%Y-%m")

#rownames(K)<- DD_SP
#colnames(K)<- GG[i]
#assign(Var_SP[[i]], K)

#}

##powerSet(GG, rev = FALSE)###-----WORKS, BUT TOO BIG

#do.call()

#cbind(t, z[match(rownames(t), rownames(z),rownames(g))])


#New__Comp<- Bind_Down(New_Re_Green_Complete, New_Re)

DD1<- (rownames(SP_tr))
#DD1<- format( DD, format="%Y-%m")
rownames(SP_tr)<- DD1


DD2<- as.Date(rownames(New_Re_Green_Complete))
DD22<- format( DD2, "%Y-%m")
rownames(New_Re_Green_Complete)<- DD22


DD3<- as.Date(rownames(New_Re))
DD13<- format( DD3, format="%Y-%m")
rownames(New_Re)<- DD13
New_Re$Date<- NULL
New_Re_Green_Complete$Date<- NULL
SP_tr$Date<- NULL


Alldat<- list(New_Re,New_Re_Green_Complete,SP_tr)

New__Comp<- data.frame()

New__Comp<- transform(Reduce(merge, lapply(Alldat, function(x) data.frame(x, rn = row.names(x)))), row.names=rn, rn=NULL)


Cov_ALL<- cov(New__Comp)
Cor_ALL<- cor(New__Comp)
all.pca<- prcomp(New__Comp , scale = TRUE)

ALL_pca <- summary(prcomp(New__Comp , scale = TRUE))

mydocA<- addTitle( mydocA, value=paste("Binding of Complete Green and Conventional Data" ), level = 2 ) 
mydocA<- addParagraph(mydocA, value= "Following the explanation of complete green data, in order to bind the two datasets (complete green and conventional) the corresponsing
  time frame between the two must be defined. The dates of the two data sets are compared, and rows with dates with the same Years and Month are selected, insuring that the 
data is monthly even if the exact day of the month does not correspond. Later to compare each green index to all the conventional ones, the same operation if performed, 
without the need to only include the Complete green dataset, the comparison of dates will automatically create the range of conventional data that corresponds to
the available time range of the green index. The Covariance is shown bellow. ", stylename = "DocDefaults")
mydocA<- addFlexTable(  mydocA,
              (FlexTable( round(Cov_ALL,4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

mydocA<- addTitle( mydocA, value=paste("All Data PCA, Summary" ), level = 2 ) 
mydocA<- addFlexTable(  mydocA,
              (FlexTable( data.frame(round(ALL_pca$importance,4)), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))
###------------------------------------------------------------------------------------------------------------------------------

##---------using Decathlon example:

#decathlon2.active<- DF[1:(nrow(DF)-0.2*(nrow(DF))), 4:6]
res.pca <- Conv_pca
resG.pca<- GR_pca
names(res.pca)
head(res.pca$sdev)
head(unclass(res.pca$rotation))


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


eig.val <- get_eigenvalue(res.pca)
head(eig.val)

v_GR<- barplot(eig.decathlon2.active[, 2], names.arg=1:nrow(eig.decathlon2.active), 
       main = "Variances",
       xlab = "Principal Components",
       ylab = "Percentage of variances",
       col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.decathlon2.active), 
      eig.decathlon2.active[, 2], 
      type="b", pch=19, col = "red")


fv_scr_Conv<- fviz_screeplot(res.pca, ncp=10)

mydocA<- addTitle( mydocA, value=paste("Conventional PCA, Scree Plot" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=print, x=fv_scr_Conv) 

fv_scr_Gr<- fviz_screeplot(GR_pca, ncp=10)
mydocA<- addTitle( mydocA, value=paste("Green PCA, Scree Plot" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=print, x=fv_scr_Gr) 

fv_scr_ALL<- fviz_screeplot(all.pca, ncp=10)
mydocA<- addTitle( mydocA, value=paste("ALL Data PCA, Scree Plot" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=print, x=fv_scr_ALL) 

fviz_screeplot(res.pca, ncp=10, choice="eigenvalue")

var_conv <- get_pca_var(res.pca)
var_Gr<- get_pca_var(GR_pca)

Contrb_Gr<- var_Gr$contrib
Contrb_Conv<- var_conv$contrib
cor_Gr<- var_Gr$cor
cor_Conv<- var_conv$cor

coord_Gr<- var_Gr$coord
coord_Conv<- var_conv$coord

Cos2_Gr<- var_Gr$Cos2
Cos2_Conv<- var_conv$Cos2


# Helper function : 
# Correlation between variables and principal components
var_cor_func <- function(var.loadings, comp.sdev){
  var.loadings*comp.sdev
  }
# Variable correlation/coordinates
loadings <- res.pca$rotation
sdev <- res.pca$sdev
var.coord <- var.cor <- t(apply(loadings, 1, var_cor_func, sdev))
head(var.coord)

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



pca_conv<- fviz_pca_var(res.pca, col.var="contrib")+
scale_color_gradient2(low="blue",  mid="yellow", 
      high="red", midpoint=55) + theme_minimal()
mydocA<- addTitle( mydocA, value=paste("Conventional PCA, Biplot" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=print, x=pca_conv) 


pca_Gr<- fviz_pca_var(resG.pca, col.var="contrib") +
scale_color_gradient2(low="blue",  mid="yellow", 
      high="red", midpoint=55) + theme_minimal()
mydocA<- addTitle( mydocA, value=paste("Green PCA, Biplot" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=print, x=pca_Gr) 



pca_SP<- fviz_pca_var(SP_pca, col.var="contrib") +
scale_color_gradient2(low="blue",  mid="yellow", 
      high="red", midpoint=55) + theme_minimal()
mydocA<- addTitle( mydocA, value=paste("S&P PCA, Biplot" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=print, x=pca_SP) 






pca_ALL<- fviz_pca_var(all.pca, col.var="contrib") +
scale_color_gradient2(low="blue",  mid="yellow", 
      high="red", midpoint=55) + theme_minimal()
mydocA<- addTitle( mydocA, value=paste("All PCA, Biplot" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=print, x=pca_ALL) 

###---------------ALL DATES PCA


mydocA<- addTitle( mydocA, value=paste("Timeseries Analysis of Indices" ), level = 2 ) 
mydocA<- addParagraph(mydocA, value= " Having completed the PCA of the Indices, it would be interesting to see how the addition of green indices
to a conventional dataframe alters the effects of variations through time. Although the timespans will differ due to different available data ranges, it may be 
possible that inclusion of indices minimize the weight of events significantly altering the trends of the conventional data, and vice versa, changing the time dependency",
 stylename = "DocDefaults")





ind.coord <- all.pca$x
head(ind.coord)

center <- all.pca$center
scale<- all.pca$scale
getdistance <- function(ind_row, center, scale){
  return(sum(((ind_row-center)/scale)^2))
  }
#d2 <- apply(decathlon2.active,1,getdistance, center, scale)
# Compute the cos2
#cos2 <- function(ind.coord, d2){return(ind.coord^2/d2)}
#ind.cos2 <- apply(ind.coord, 2, cos2, d2)
#head(ind.cos2)



# Contributions of individuals
contrib <- function(ind.coord, comp.sdev, n.ind){
  100*(1/n.ind)*ind.coord^2/comp.sdev^2
}
ind.contrib <- t(apply(ind.coord,1, contrib, 
                       all.pca$sdev, nrow(ind.coord)))
head(ind.contrib[, 1:3])



p3.pryr %<a-% {plot(ind.coord[,1], ind.coord[,2], pch = 19,  
     xlab="PC1 - 41.2%",ylab="PC2 - 18.4%")
abline(h=0, v=0, lty = 2)
text(ind.coord[,1], ind.coord[,2], labels=rownames(ind.coord),
        cex=0.7, pos = 3)}


mydocA<- addTitle( mydocA, value=paste("All binded Data(Green and Conventional): Date Contribution" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=function() p3.pryr) 


###---------------CONV DATES PCA


ind.coord <- res.pca$x
head(ind.coord)

center <- res.pca$center
scale<- res.pca$scale
getdistance <- function(ind_row, center, scale){
  return(sum(((ind_row-center)/scale)^2))
  }
#d2 <- apply(decathlon2.active,1,getdistance, center, scale)
# Compute the cos2
#cos2 <- function(ind.coord, d2){return(ind.coord^2/d2)}
#ind.cos2 <- apply(ind.coord, 2, cos2, d2)
#head(ind.cos2)



# Contributions of individuals
contrib <- function(ind.coord, comp.sdev, n.ind){
  100*(1/n.ind)*ind.coord^2/comp.sdev^2
}
ind.contrib <- t(apply(ind.coord,1, contrib, 
                       res.pca$sdev, nrow(ind.coord)))
head(ind.contrib[, 1:3])




p1.pryr %<a-% { plot(ind.coord[,1], ind.coord[,2], pch = 19,  
     xlab="PC1 - 41.2%",ylab="PC2 - 18.4%")
abline(h=0, v=0, lty = 2)
text(ind.coord[,1], ind.coord[,2], labels=rownames(ind.coord),
        cex=0.7, pos = 3)
}

mydocA<- addTitle( mydocA, value=paste("Conventional Data: Date Contribution" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=function() p1.pryr) 

###---------------Green DATES PCA

ind.coord <- resG.pca$x
head(ind.coord)

center <- resG.pca$center
scale<- resG.pca$scale
getdistance <- function(ind_row, center, scale){
  return(sum(((ind_row-center)/scale)^2))
  }
#d2 <- apply(decathlon2.active,1,getdistance, center, scale)
# Compute the cos2
#cos2 <- function(ind.coord, d2){return(ind.coord^2/d2)}
#ind.cos2 <- apply(ind.coord, 2, cos2, d2)
#head(ind.cos2)



# Contributions of individuals
contrib <- function(ind.coord, comp.sdev, n.ind){
  100*(1/n.ind)*ind.coord^2/comp.sdev^2
}
ind.contrib <- t(apply(ind.coord,1, contrib, 
                       resG.pca$sdev, nrow(ind.coord)))
head(ind.contrib[, 1:3])


p2.pryr %<a-% {
plot(ind.coord[,1], ind.coord[,2], pch = 19,  
     xlab="PC1 - 41.2%",ylab="PC2 - 18.4%")
abline(h=0, v=0, lty = 2)
text(ind.coord[,1], ind.coord[,2], labels=rownames(ind.coord),
        cex=0.7, pos = 3)
}
mydocA<- addTitle( mydocA, value=paste("Green Data: Date Contribution" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=function() p2.pryr) 


##---------correcting for skewness etc:
require(caret)
trans = preProcess(New_Re[,1:ncol(New_Re)], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))

PC = predict(trans, New_Re)
model <- train(New_Re[,1:ncol(New_Re)], method='lda', preProcess='pca')
trans$rotation
biplot(x=x1, y=PC, scale.=0)
###----------------------------------------------------------------------------------------------------------------------------
###--------------------------Normalizing and attempting to add to word, FAILED, SAVED TO PDF-----------------------------------
###----------------------------------------------------------------------------------------------------------------------------


colnames(New_Re)
set.seed(1)
#predictors = data.frame(New_Re[,1:11])
New_Re$Date<- (rownames(New_Re))

plot_list = list()
#mydoc2 = docx()
mydocA<- addTitle( mydocA, value=paste("Correcting for Skewness via Box Cox Power Function" ), level = 1 ) 


for (i in 1:(length(colnames(New_Re))-1)){

predictors= data.frame(x1 = New_Re$Date, 
                        x2 = New_Re[colnames(New_Re)[i]])
colnames(predictors)<- c("x1", "x2")
p1<- ggplot(predictors, aes(x = x1, y = x2)) + geom_point()

temp <- melt(predictors, measured = c("x1", "x2"))
p2<- ggplot(temp) + geom_histogram(aes(x=value)) + 
  facet_grid(. ~ variable, scales = "free_x")

grid.arrange(p1, p2)
trans <- preProcess(predictors, c("BoxCox", "center", "scale"))
predictors_trans <- data.frame(trans = predict(trans, predictors))

p3<- ggplot(predictors_trans) + geom_point(aes(x = trans.x1, y = trans.x2))
temp <- melt(predictors_trans, measured = c("trans.x1", "trans.x2"))
p4<- ggplot(temp) + geom_histogram(aes(x=value), data = temp) + 
  facet_grid(. ~ variable, scales = "free_x")
grid.arrange(p3, p4)
#title(cat("Normalization of Index", colnames(New_Re)[i]))

#pdf("filename.pdf", width = 8, height = 12) 
plot_list[[i]] =  (grid.arrange(p1, p2, p3, p4))

p4.pryr %<a-% {(grid.arrange(p1, p2, p3, p4))}
g1<- do.call("grid.arrange", plot_list[[i]]) 
#ggsave("sgcirNIR.jpg")

mydocA<- addTitle( mydocA, value=paste("Correcting for Skewness via Box Cox Power Function", colnames(New_Re)[i] ), level = 2 ) 
mydocA<- addPlot( mydocA , fun = function() p4.pryr)#, 
 #vector.graphic = TRUE, par.properties = parCenter(), width = 6, heigth = 7 )


     }


#writeDoc(mydocA, file = 'testa.docx')
#browseURL("testa.docx")


pdf("plots1234.pdf", onefile = TRUE)
for (i in 1:length(plot_list)){
#:length(colnames(New_Re))-1)
  do.call("grid.arrange", plot_list[[i]])  

}
dev.off()

g2<- grid.draw(g1)

#mydoc = docx()

#base_legend = "My first plot"

#for( index in 1:3 ){
  #mylegend = pot(base_legend, textBoldItalic() )

  #if( index > 1 ) mylegend = paste0(base_legend, " (cont'd)" )
  #else mylegend = base_legend

  #mydoc = addPlot( doc = mydoc, fun = print, x = grid.arrange(plot_list[[index]])  , 
    #vector.graphic = TRUE, par.properties = parCenter(), width = 6, heigth = 7 )

 # mydoc = addParagraph( mydoc, value = mylegend, stylename = "rPlotLegend")
#}
#writeDoc( mydoc, "why2.docx")
#browseURL( "why2.docx" )
##---------------------------------------------------------

#g <- ggbiplot(DF.pca, obs.scale = 1, var.scale = 1, 
 #             groups = DF$Asset, ellipse = TRUE, 
  #            circle = TRUE)


g <- ggbiplot(all.pca, obs.scale = 1, var.scale = 1, 
              groups = NULL, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')

g<- ggbiplot(Conv_pca, choices = 1:2, scale = 1, pc.biplot = TRUE, obs.scale = 1 - scale, var.scale = scale, groups = NULL, ellipse = TRUE, ellipse.prob = 0.68, labels =FALSE, labels.size = 3, alpha = 1, var.axes = TRUE, circle  = TRUE, circle.prob = 0.69, varname.size = 3, varname.adjust = 1.5, varname.abbrev = FALSE)

print(g)

###------------------------------------------------
##----------Plotting all data----------------------
###------------------------------------------------

mtlong <- reshape2::melt(New_Re_Green_Complete)

ggplot(mtlong, aes(value)) + facet_wrap(~variable, scales = 'free_x') +
  geom_density()

df <- melt(New_Re ,  id.vars = 'Date', variable.name = 'series')
all_Cov1<- ggplot(df, aes(Date,value, group = 1)) + geom_line(aes(colour = series))

mydocA<- addTitle( mydocA, value=paste("Total Returns" ), level = 1 ) 

mydocA<- addTitle( mydocA, value=paste("Total Conventional Returns" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=print, x=all_Cov1)  
all_Cov2<- ggplot(df, aes(Date,value, group = 1)) + geom_line() + facet_grid(series ~ .)
mydocA<- addPlot( mydocA , fun=print, x=all_Cov2)  

New_Re_Green_Complete$Date<- rownames(New_Re_Green_Complete)
df <- melt(New_Re_Green_Complete ,  id.vars = 'Date', variable.name = 'series')
all_Green1<- ggplot(df, aes(Date,value, group = series)) + geom_line(aes(colour = series))
mydocA<- addTitle( mydocA, value=paste("Total Green Returns" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=print, x=all_Green1)  

all_Green2<- ggplot(df, aes(Date,value, group = series)) + geom_line() + facet_grid(series ~ .)
mydocA<- addPlot( mydocA , fun=print, x=all_Green2)  


SP_tr$Date<- rownames(SP_tr)
df <- melt(SP_tr ,  id.vars = 'Date', variable.name = 'series')
all_SP_tr<- ggplot(df, aes(Date,value, group = series)) + geom_line(aes(colour = series))
mydocA<- addTitle( mydocA, value=paste("Total S&P Returns" ), level = 2 ) 
mydocA<- addPlot( mydocA , fun=print, x=all_SP_tr)  

all_SP_tr2<- ggplot(df, aes(Date,value, group = series)) + geom_line() + facet_grid(series ~ .)
mydocA<- addPlot( mydocA , fun=print, x=all_SP_tr2)  




####-----------------------------------------------------------------------------------------------------------------------------------------
####--------------------Comparison of Green and conventional, creating matching numbers based on year and month,02-10-2017-------------------
####-----------------------------------------------------------------------------------------------------------------------------------------


FTS4G<- New_Re_DS_GRN[3]
#X<- FTS4G
#Y<- New_Re

#New_Re_Green_Complete_Y<-  (year(rownames(X)))
#New_Re_Green_Complete_M<- month(rownames(X))

#New_Re_Green_Complete_dates<- data.frame(cbind(New_Re_Green_Complete_M, New_Re_Green_Complete_Y))
#colnames(New_Re_Green_Complete_dates)<- c("M" , "Y")

#New_Re_Y<- (year(rownames(Y)))
#New_Re_M<- month(rownames(Y))

#New_Re_dates<- data.frame(cbind(New_Re_M, New_Re_Y))
#colnames(New_Re_dates)<- c("M" , "Y")
#New_Re_CMN_D<- inner_join(New_Re_dates, New_Re_Green_Complete_dates)

#if (length(New_Re_CMN_D)!=length(New_Re_dates)){
#cat("The Green and Conventional Matrixes were not the same size, Conv was ",(length(New_Re_CMN_D)-length(New_Re_dates)), "more than green, so matched dates extracted")
#}


#Y$Date<- rownames(Y)
#Comp_Y<- data.frame(matrix(, nrow=nrow(New_Re_CMN_D), ncol= ncol(Y)))
#Comp_X<- data.frame(matrix(, nrow=nrow(New_Re_CMN_D), ncol= ncol(X)))

#for (i in 1:nrow(New_Re_CMN_D)){
#Comp_Y[i,]<- subset(Y, ( (month(rownames(Y)))==New_Re_CMN_D[i,1] & year(rownames(Y))==New_Re_CMN_D[i,2]))
#}

#for (i in 1:nrow(New_Re_CMN_D)){
#Comp_X[i,]<- subset(X, ( (month(rownames(X)))==New_Re_CMN_D[i,1] & year(rownames(X))==New_Re_CMN_D[i,2]))
#}

#rownames(Comp_Y)<- Comp_Y[, ncol( Comp_Y)]
#colnames(Comp_Y)<- colnames(Y)
#Comp_Y["Date"]<- NULL

#rownames(Comp_X)<- Comp_X[, ncol( Comp_X)]
#colnames(Comp_X)<- colnames(X)
#Comp_X["Date"]<- NULL


#FTS4G<- Comp_X
#New_Re<- Comp_Y

if(nrow(FTS4G)!=nrow(New_Re)){
cat("Nope, The Green and Conventional Matrixes are not matched correctly, Conv has ",(nrow(FTS4G)-nrow(New_Re)), "more than green, fix it")
}###-------!!!!!!!!

###-----------------------RUN TESTS

PP.test(New_Re[,1])
kpss.test(New_Re[,1])
adf.test(New_Re[,1])


#mydoc <- docx( ) #%>% 

for (i in 1:ncol(New__Comp)) {

Tab1<- (PP.test(New__Comp[,i]))
Tab2<- kpss.test(New__Comp[,i])
Tab3<- adf.test(New__Comp[,i])

Tab_all <- capture.output(print(Tab1), print(Tab2), print(Tab3))
##Tab4<- rbind(Tab1, Tab2, Tab3)

addTitle( mydocA, value=paste("Test Results for", colnames(New__Comp)[i] ), level = 1 )%>%  
addParagraph( value=Tab_all, stylename = "DocDefaults" )# %>%

#addFlexTable(mydoc,  Tab1 %>%
                #  FlexTable( header.cell.props = cellProperties( background.color =  "#003366" ),
                            # header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                            # add.rownames = TRUE ) %>%
                 # setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) 
}
 
#writeDoc(mydocA, file = '08-10.docx')
# open the Word document
#browseURL("08-10.docx")


###---bind green and conv together for simon------
mydocA<- addTitle( mydocA, value=paste("Binding Green Indices to Conventional for Comparison" ), level = 1 ) 
mydocA<- addParagraph(mydocA, 
         value= "To help determine which indices should be added to which conventional portfolios for the Black Litterman Analysis, each green index
is added to the risky asset portfolio, and the overall covariances are compared. The difference between these covariances and the previous All Data 
covariance lies in that for each index, the conventional range is set to the range of dates available for the individual green index being added,
as opposed to the length of the complete green indices. One column of Risky assets is also added to the indices, the value of which is the sum of
the conventional asset returns at any date, the same rule is applied to its market value. To accuratley measure the covariance
throughout the timeseries, the covariance is measured through rolling windows of length time series divided by 20. The windows of covariance roll over
the data to insure connectivity and avoid misrepresentation of jumps associated with window transaction with that of covariance volitlity. 
The same applies to standard deviation", 
         stylename = "DocDefaults")
New_Re<- New_Re[,1:(ncol(New_Re)-1)]
New_Re$Risky_Asset<- rowSums(New_Re)
#New_Re$Date<- as.Date(rownames(New_Re))

#New_Re_Green_Complete$Date<- as.Date(rownames(New_Re_Green_Complete))
#Re_dat$Date<- as.Date(rownames(Re_dat))

merge(FTS4G, New_Re,by=0)###--all data


DD<- as.Date(rownames(FTS4G))
DD1<- format( DD, format="%Y-%m")
rownames(FTS4G)<- DD1

Kb1<- list(New_Re, FTS4G)

RA_FTS4G<- transform(Reduce(merge, lapply(Kb1, function(x) data.frame(x, rn = row.names(x)))), row.names=rn, rn=NULL)

width<- floor(nrow(New_Re)/20)
#width<- 2
Cov_roll_New_Re<- roll_cov(as.matrix(New_Re), width, weights = rep(1, width))#, center = TRUE,
Cov.dat_New_Re<- cov(as.matrix(New_Re))

#Cov_roll_SP<- roll_cov(as.matrix(SP_tr), width, weights = rep(1, width))#, center = TRUE,
#Cov.dat_SP<- cov(as.matrix(SP_tr))


Cor_roll_New_Re<- roll_cor(as.matrix(New_Re), width, weights = rep(1, width))# , center = TRUE,
#scale = FALSE, min_obs = width, complete_obs = TRUE,
#na_restore = FALSE, parallel_for = c("rows", "cols"))

Cor_roll_RA_FT<- roll_cor(as.matrix(RA_FTS4G), width, weights = rep(1, width))# , center = TRUE,
Cov_roll_RA_FT<- roll_cov(as.matrix(RA_FTS4G), width, weights = rep(1, width))#, center = TRUE,
Sd_roll_RA_FT<- roll_sd(as.matrix(RA_FTS4G), width, weights = rep(1, width))


Sd_roll_New_Re<- roll_sd(as.matrix(New_Re), width, weights = rep(1, width))
DF_Cor<- data.frame(Cor_roll_RA_FT)



###-------------Get Elements of Cor and Cov Rolled Matrixes (Upper Traingular)

N_m<- dim(Cov_roll_RA_FT)[1]
Windows<- dim(Cov_roll_RA_FT)[3]
rows <- N_m 


x <- rev(abs(sequence(seq.int(rows - 1)) - rows) + 1)
y <- rep.int(seq.int(rows - 1), rev(seq.int(rows - 1)))

idx <- cbind(y, x)

Nms<- vector()

Cov_Dat<- data.frame(matrix(,  nrow= Windows, ncol= nrow(idx)))
colnames(Cov_Dat)<- t(c(apply(format(idx), 1, paste, collapse=",")))

##---remove dates from dat Frames and bind FTSE$GOOD AND RISKY ASSETS:

G_N<- colnames(New_Re_DS_GRN)
New_Re_DS_GRN$Date<- rownames(New_Re_DS_GRN) 



idkk<-data.frame()

for (i in 1:nrow(idx) ){
idkk[i,1]<- colnames(RA_FTS4G)[idx[i,1]]
idkk[i,2]<- colnames(RA_FTS4G)[idx[i,2]]
}

colnames(Cov_Dat)<- t(c(apply(format(idkk), 1, paste, collapse=",")))

for (i in 1:nrow(idx) ){
Cov_Dat[,i]<- Cov_roll_RA_FT[idx[i,1], idx[i,2], ]
}

Cov.dat<- cov(RA_FTS4G)
Cov_Dat<- Cov_Dat[complete.cases(Cov_Dat), ]
Cov_Dat$D<- rownames(Cov_Dat)
df <- melt(Cov_Dat ,  id.vars = 'D', variable.name = 'series')

#P_Cor_Dat<- ggplot(df, aes(D,value, group=series)) +  geom_line(aes(colour = series))


P_Cov_Dat<- ggplot(df, aes(D,value,, group=series)) + geom_line(aes(colour = series))
(aes(colour = series))#+stat_smooth()


####---bind market values:

market_value_DS_GRN<- Re_DS_Green2[names(Re_DS_Green2 %>% select(contains("MARKET.VALUE")))]

market_value_DS_GRN<- market_value_DS_GRN[-1,1:2]
market_value$Risky_assets<- rowSums(market_value)

FTS4G_mv<- data.frame(market_value_DS_GRN[,1])
rownames(FTS4G_mv)<- rownames(market_value_DS_GRN)


 duplicated(rownames(FTS4E_mv))

FTS4E_mv<- data.frame(market_value_DS_GRN[,2])
rownames(FTS4E_mv)<- rownames(market_value_DS_GRN)

RA_FTS4G_mv<- cbind(market_value, FTS4G_mv)
RA_FTS4E_mv<- cbind(market_value,FTS4E_mv)


RA_FTS4G_mv<- RA_FTS4G_mv[complete.cases(RA_FTS4G_mv),]
RA_FTS4E_mv<- RA_FTS4E_mv[complete.cases(RA_FTS4E_mv),]

#Green<- cbind(market_value, market_value_DS_GRN[-1,1] )###--all data


###--------------------------------------
##----bind each in loop------------------
###--------------------------------------
Vars<- list()
for (i in 1:(ncol(New_Re_DS_GRN)-1)) {


#FTS4G<- New_Re_DS_GRN[3]

G_S<- data.frame()


DD<- as.Date(rownames(New_Re_DS_GRN))
DD1<- format( DD, format="%Y-%m")
rownames(New_Re_DS_GRN)<- DD1

New_Re_DS_GRN$Date<- DD1

G_S<- data.frame(New_Re_DS_GRN[complete.cases(New_Re_DS_GRN[,i]),i])
New_Re_DS_GRN$Date[complete.cases(New_Re_DS_GRN[,i])]
rownames(G_S)<- New_Re_DS_GRN$Date[complete.cases(New_Re_DS_GRN[,i])]
colnames(G_S)<- G_N[i]
G_Bind1<- list(G_S, New_Re)
G_Bind<- transform(Reduce(merge, lapply(G_Bind1, function(x) data.frame(x, rn = row.names(x)))), row.names=rn, rn=NULL)






assign(paste0("DF_Risky_A_", G_N[i]), data.frame( G_Bind ))
mydocA<- addTitle( mydocA, value=paste0("All Assets and ", G_N[i]), level = 2 ) 
mydocA<- addParagraph(mydocA, value= paste("Common Dates between the Risky assets and " ,G_N[i]), stylename = "DocDefaults")
Tab55<- data.frame(c(rownames(G_Bind)[1],rownames(G_Bind)[nrow(G_Bind)],nrow(G_Bind)))
colnames(Tab55)<- paste("Risky assets and " ,G_N[i])
rownames(Tab55)<- c("Start Date", "End date", "Size")


mydocA<- addFlexTable(  mydocA,
              (FlexTable( Tab55, header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

##mydocA<- addFlexTable(  mydocA,
              ##(FlexTable( stargazer(data.frame(G_Bind)))))



Vars[i]<- paste0("DF_Risky_A_", G_N[i])

mydocA<- addTitle( mydocA, value=paste0("Portfolio of Risky Assets and ", G_N[i]), level = 2 ) 


assign(Vars[[i]], data.frame( G_Bind ))
it<- get(Vars[[i]])

width<- floor(nrow(it)/20)
#width<- 2
it["Date"]<- NULL

Cor_roll_it<- roll_cor(as.matrix(it), width, weights = rep(1, width))
Cov_roll_it<- roll_cov(as.matrix(it), width, weights = rep(1, width))
Sd_roll_it<- roll_sd(as.matrix(it), width, weights = rep(1, width))

Sd_roll_it<- data.frame(Sd_roll_it)

N_m<- dim(Cov_roll_it)[1]
Windows<- dim(Cov_roll_it)[3]
rows <- N_m 


x <- rev(abs(sequence(seq.int(rows - 1)) - rows) + 1)
y <- rep.int(seq.int(rows - 1), rev(seq.int(rows - 1)))

idx <- cbind(y, x)

Nms<- vector()

Cov_Dat<- data.frame(matrix(,  nrow= Windows, ncol= nrow(idx)))
colnames(Cov_Dat)<- t(c(apply(format(idx), 1, paste, collapse=",")))

Cor_Dat<- data.frame(matrix(,  nrow= Windows, ncol= nrow(idx)))
colnames(Cor_Dat)<- t(c(apply(format(idx), 1, paste, collapse=",")))

#G_N<- colnames(New_Re_DS_GRN)
#New_Re_DS_GRN$Date<- rownames(New_Re_DS_GRN) 



idkk<-data.frame()

for (ii in 1:nrow(idx) ){
idkk[ii,1]<- colnames(it)[idx[ii,1]]
idkk[ii,2]<- colnames(it)[idx[ii,2]]
}

colnames(Cov_Dat)<- t(c(apply(format(idkk), 1, paste, collapse=",")))
colnames(Cor_Dat)<- t(c(apply(format(idkk), 1, paste, collapse=",")))


for (iii in 1:nrow(idx) ){
Cov_Dat[,iii]<- Cov_roll_it[idx[iii,1], idx[iii,2], ]
Cor_Dat[,iii]<- Cor_roll_it[idx[iii,1], idx[iii,2], ]
}


###----creating rolling data

Cov_Dat<- Cov_Dat[complete.cases(Cov_Dat), ]
Cov_Dat$D<- rownames(Cov_Dat)


Cor_Dat<- Cor_Dat[complete.cases(Cor_Dat), ]
Cor_Dat$D<- rownames(Cor_Dat)

Sd_roll_it$D <- rownames(Sd_roll_it)
Sd_roll_it<- Sd_roll_it[complete.cases(Sd_roll_it),]# 1:(ncol(Sd_roll_it)-1) ]

df <- melt(Cov_Dat ,  id.vars = 'D', variable.name = 'series')
df1 <- melt(Cor_Dat ,  id.vars = 'D', variable.name = 'series')
df2<- melt(Sd_roll_it,  id.vars = 'D', variable.name = 'series')

##------COV
mydocA<- addTitle( mydocA, value="Manual Covariance", level = 3 ) 
Cov.dat<- cov(it)
mydocA<- addFlexTable(  mydocA,
              (FlexTable( round(Cov.dat,4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

P_Cov_Dat<- ggplot(df, aes(D,value, group=series)) + geom_line(aes(colour = series))+ theme(legend.text=element_text(size=7), legend.position="bottom")

(aes(colour = series))
mydocA<- addPlot( mydocA , fun=print, x=P_Cov_Dat)  


##-----COR

mydocA<- addTitle( mydocA, value="Manual Correlation", level = 3 ) 
Cor.dat<- cor(it)
mydocA<- addFlexTable(  mydocA,
              (FlexTable( round(Cor.dat,4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))
P_Cor_Dat<- ggplot(df1, aes(D,value, group=series)) + geom_line(aes(colour = series))+ theme(legend.text=element_text(size=7), legend.position="bottom")
(aes(colour = series))
mydocA<- addPlot( mydocA , fun=print, x=P_Cor_Dat)  

#-----STDV
mydocA<- addTitle( mydocA, value="Manual Standard Deviation", level = 3 ) 
stds.dat<- colSds(as.matrix(it))
stds.dat<- data.frame(t(stds.dat))
colnames(stds.dat)<- colnames(it)
mydocA<- addFlexTable(  mydocA,
              (FlexTable( round(stds.dat,4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

P_stdv_Dat<- ggplot(df2, aes(D,value, group=series)) + geom_line(aes(colour = series))+ theme(legend.text=element_text(size=7), legend.position="bottom")
(aes(colour = series))
mydocA<- addPlot(  mydocA , fun=print, x=P_stdv_Dat)  



}


###--------------------------------------------
###-------------BINDING TO S&P DATA------------
###--------------------------------------------
Vars2<- list()
for (i in 1:(ncol(SP_tr))) {

G_N<- colnames(SP_tr)
#FTS4G<- New_Re_DS_GRN[3]

G_S<- data.frame()

G_S<- data.frame(SP_tr[complete.cases(SP_tr[,i]),i])
SP_tr$Date<- rownames(SP_tr)

rownames(G_S)<- SP_tr$Date[complete.cases(SP_tr[,i])]
colnames(G_S)<- G_N[i]
G_Bind1<- list(G_S, New_Re)
G_Bind<- transform(Reduce(merge, lapply(G_Bind1, function(x) data.frame(x, rn = row.names(x)))), row.names=rn, rn=NULL)


  assign(paste0("DF_Risky_A_", G_N[i]), data.frame( G_Bind ))

Vars2[i]<- paste0("DF_Risky_A_", G_N[i])
mydocA<- addTitle( mydocA, value=paste0("All Assets and ", G_N[i]), level = 2 ) 
mydocA<- addParagraph(mydocA, value= paste("Common Dates between the Risky assets and " ,G_N[i]), stylename = "DocDefaults")
Tab55<- data.frame(c(rownames(G_Bind)[1],rownames(G_Bind)[nrow(G_Bind)],nrow(G_Bind)))
colnames(Tab55)<- paste("Risky assets and " ,G_N[i])
rownames(Tab55)<- c("Start Date", "End date", "Size")

mydocA<- addFlexTable(  mydocA,
              (FlexTable( Tab55, header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

##mydocA<- addFlexTable(  mydocA,
              ##(FlexTable( stargazer(data.frame(G_Bind)))))


assign(Vars2[[i]], data.frame( G_Bind ))
it<- get(Vars2[[i]])
it["Date"]<- NULL
width<- floor(nrow(it)/20)
#width<- 2


Cor_roll_it<- roll_cor(as.matrix(it), width, weights = rep(1, width))
Cov_roll_it<- roll_cov(as.matrix(it), width, weights = rep(1, width))
Sd_roll_it<- roll_sd(as.matrix(it), width, weights = rep(1, width))

Sd_roll_it<- data.frame(Sd_roll_it)

N_m<- dim(Cov_roll_it)[1]
Windows<- dim(Cov_roll_it)[3]
rows <- N_m 


x <- rev(abs(sequence(seq.int(rows - 1)) - rows) + 1)
y <- rep.int(seq.int(rows - 1), rev(seq.int(rows - 1)))

idx <- cbind(y, x)

Nms<- vector()

Cov_Dat<- data.frame(matrix(,  nrow= Windows, ncol= nrow(idx)))
colnames(Cov_Dat)<- t(c(apply(format(idx), 1, paste, collapse=",")))

Cor_Dat<- data.frame(matrix(,  nrow= Windows, ncol= nrow(idx)))
colnames(Cor_Dat)<- t(c(apply(format(idx), 1, paste, collapse=",")))

#G_N<- colnames(New_Re_DS_GRN)
#New_Re_DS_GRN$Date<- rownames(New_Re_DS_GRN) 



idkk<-data.frame()

for (ii in 1:nrow(idx) ){
idkk[ii,1]<- colnames(it)[idx[ii,1]]
idkk[ii,2]<- colnames(it)[idx[ii,2]]
}

colnames(Cov_Dat)<- t(c(apply(format(idkk), 1, paste, collapse=",")))
colnames(Cor_Dat)<- t(c(apply(format(idkk), 1, paste, collapse=",")))


for (iii in 1:nrow(idx) ){
Cov_Dat[,iii]<- Cov_roll_it[idx[iii,1], idx[iii,2], ]
Cor_Dat[,iii]<- Cor_roll_it[idx[iii,1], idx[iii,2], ]
}



###----creating rolling data

Cov_Dat<- Cov_Dat[complete.cases(Cov_Dat), ]
Cov_Dat$D<- rownames(Cov_Dat)


Cor_Dat<- Cor_Dat[complete.cases(Cor_Dat), ]
Cor_Dat$D<- rownames(Cor_Dat)

Sd_roll_it$D <- rownames(Sd_roll_it)
Sd_roll_it<- Sd_roll_it[complete.cases(Sd_roll_it),]# 1:(ncol(Sd_roll_it)-1) ]

df <- melt(Cov_Dat ,  id.vars = 'D', variable.name = 'series')
df1 <- melt(Cor_Dat ,  id.vars = 'D', variable.name = 'series')
df2<- melt(Sd_roll_it,  id.vars = 'D', variable.name = 'series')

##------COV
mydocA<- addTitle( mydocA, value="Manual Covariance", level = 3 ) 
Cov.dat<- cov(it)
mydocA<- addFlexTable(  mydocA,
              (FlexTable( round(Cov.dat,4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

P_Cov_Dat<- ggplot(df, aes(D,value, group=series)) + geom_line(aes(colour = series))+ theme(legend.text=element_text(size=7), legend.position="bottom")

(aes(colour = series))
mydocA<- addPlot( mydocA , fun=print, x=P_Cov_Dat)  


##-----COR

mydocA<- addTitle( mydocA, value="Manual Correlation", level = 3 ) 
Cor.dat<- cor(it)
mydocA<- addFlexTable(  mydocA,
              (FlexTable( round(Cor.dat,4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))
P_Cor_Dat<- ggplot(df1, aes(D,value, group=series)) + geom_line(aes(colour = series))+ theme(legend.text=element_text(size=7), legend.position="bottom")

(aes(colour = series))
mydocA<- addPlot( mydocA , fun=print, x=P_Cor_Dat)  

#-----STDV
mydocA<- addTitle( mydocA, value="Manual Standard Deviation", level = 3 ) 
stds.dat<- colSds(as.matrix(it))
stds.dat<- data.frame(t(stds.dat))
colnames(stds.dat)<- colnames(it)
mydocA<- addFlexTable(  mydocA,
              (FlexTable( round(stds.dat,4), header.cell.props = cellProperties( background.color =  "#003366" ),
                           header.text.props = textProperties( color = "white", font.size = 9, font.weight = "bold" ),
                           body.text.props = textProperties( font.size = 7 ),
                           add.rownames = TRUE ) %>%
                           setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ))

P_stdv_Dat<- ggplot(df2, aes(D,value, group=series)) + geom_line(aes(colour = series))+ theme(legend.text=element_text(size=7), legend.position="bottom")

(aes(colour = series))
mydocA<- addPlot(  mydocA , fun=print, x=P_stdv_Dat)  




}


writeDoc(mydocA, file = 'k16.docx')
browseURL("k16.docx")

