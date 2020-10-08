
## 2. Loading Data & Generating Features
rm(list = ls()) # Clears global environment
library(pacman)
p_load(DataExplorer, ggplot2, ggcorrplot, dplyr, corrplot, jtools, ggstance, broom.mixed)

data = read.csv("AmesHousing.csv", stringsAsFactors = TRUE) # reads in data set

  #2. A. correctly code variables
data$Bsmt.Full.Bath=factor(data$Bsmt.Full.Bath)
data$Bsmt.Half.Bath =factor(data$Bsmt.Half.Bath)
data$Full.Bath =factor(data$Full.Bath)
data$Half.Bath =factor(data$Half.Bath)
data$Bedroom.AbvGr = factor(data$Bedroom.AbvGr)
data$Kitchen.AbvGr = factor(data$Kitchen.AbvGr)


dim(data) # gives the number of variables and observations in the data set
head(data) # gives a glimse of the head of the data
plot_missing(data, group = list(Good = 0.05, OK = 0.3, Bad = 0.5, Remove = 1), missing_only = TRUE, geom_label_args = list(), title = NULL, ggtheme = theme_gray(), theme_config = list(legend.position = c("bottom"))) # plots percent missing by column in the data set

  # 2. B. Creating a correlation plot for only highly correlated variables
corr_simple <- function(data= data,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
df_cor <- data %>% mutate_if(is.character, as.factor)
df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
corr[corr == 1] <- NA 
  #turn into a 3-column table
corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
corr <- na.omit(corr) 
  #select significant values  
corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
corr <- corr[order(-abs(corr$Freq)),] 
  #print table
print(corr)
  #turn corr back into matrix in order to plot with corrplot
mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}
corr_simple(data) # creates a correlation plot including only highly correlated variables 


## 3. Details on Data Preprocessing

  # 3. A. remove any variables that had atleast 45% missing data

data<-select(data, -Fireplace.Qu)
data<-select(data, -Fence)
data<-select(data, -Alley)
data<-select(data, -Misc.Feature)
data<-select(data, -Pool.QC)

  # 3. B. impute any variables that had atleast 5% missing data and was not removed
      # Creating Mode function

getmode <- function(data) {
  uniqv <- unique(data)
  uniqv[which.max(tabulate(match(data, uniqv)))]
}

#vet.data$M_DemMedIncome<-as.factor(ifelse(is.na(vet.data$DemMedIncome), 1, 0))
#summary(vet.data$M_DemMedIncome)
#vet.data$DemMedIncome[is.na(vet.data$DemMedIncome)]<-median(vet.data$DemMedIncome, na.rm=TRUE)
#summary(vet.data$DemMedIncome)

      #Garage.Finish
data$M_Garage.Finish<-as.factor(ifelse(is.na(data$Garage.Finish), 1, 0))
data$Garage.Finish[is.na(data$Garage.Finish)]<-getmode(data$Garage.Finish)
      #Garage.Type
data$M_Garage.Type<-as.factor(ifelse(is.na(data$Garage.Type), 1, 0))
data$Garage.Type[is.na(data$Garage.Type)]<-getmode(data$Garage.Type)
      #Garage.Cond
data$M_Garage.Cond<-as.factor(ifelse(is.na(data$Garage.Cond), 1, 0))
data$Garage.Cond[is.na(data$Garage.Cond)]<-getmode(data$Garage.Cond)
      #Garage.Qual
data$M_Garage.Qual<-as.factor(ifelse(is.na(data$Garage.Qual), 1, 0))
data$Garage.Qual[is.na(data$Garage.Qual)]<-getmode(data$Garage.Qual)
      #Garage.Yr.Blt
data$M_Garage.Yr.Blt<-as.factor(ifelse(is.na(data$Garage.Yr.Blt), 1, 0))
data$Garage.Yr.Blt[is.na(data$Garage.Yr.Blt)]<-median(data$Garage.Yr.Blt, na.rm = TRUE)
      #Lot.Frontage
data$M_Lot.Frontage<-as.factor(ifelse(is.na(data$Lot.Frontage), 1, 0))
data$Lot.Frontage[is.na(data$Lot.Frontage)]<-median(data$Lot.Frontage, na.rm = TRUE)
    #SalePrice
data$M_SalePrice<-as.numeric(ifelse(is.na(data$SalePrice), 1, 0))
data$SalePrice[is.na(data$SalePrice)]<-median(data$SalePrice, na.rm = TRUE)

# remove the newly created factors from the data
data<-select(data, -M_SalePrice)
data<-select(data, -M_Lot.Frontage)
data<-select(data, -M_Garage.Yr.Blt)
data<-select(data, -M_Garage.Cond)
data<-select(data, -M_Garage.Type) 
data<-select(data, -M_Garage.Finish)
data<-select(data, -M_Garage.Qual)

  
  #3.C. Drop missing data so we can run a stepwise regression
dim(data) # Check to see # of observations before omitting missing data
data=na.omit(data) #remove missing data
dim(data) # Check number of observations after omitting data
                 
# 4 Final Analysis in small pieces

  # 4. A. Creating the first stepwise regression on training data 

full<-lm(SalePrice~., data = data) # creating full linear regression

null<-lm(SalePrice~1, data=data) # Creating null linear regression

lm.step1<-step(null, scope=list(lower=null, upper=full), direction="both", trace=0) 


  #4. B. Create new model removing highly correlated variables

lm = lm(SalePrice ~ Overall.Qual + BsmtFin.SF.1 + Roof.Matl + MS.SubClass + Bsmt.Exposure + Condition.2 + Sale.Condition + Garage.Area + Overall.Cond + Bsmt.Qual + Total.Bsmt.SF + Lot.Area + Pool.Area + Screen.Porch + Functional + Exterior.1st + Land.Contour + Bsmt.Full.Bath + Mas.Vnr.Area + Condition.1 + Fireplaces + Garage.Yr.Blt + Mas.Vnr.Type + BsmtFin.SF.2 + Wood.Deck.SF + Garage.Finish + BsmtFin.Type.1 + MS.Zoning + Land.Slope + Garage.Cars + Kitchen.AbvGr + Low.Qual.Fin.SF + Utilities, data = data)
summary(lm)
lm$coefficients

lm2 = abs(lm$coefficients)
lm2
sort(lm2, decreasing = TRUE)
  #4. C. Create a graph to show the coefficient values

lm2 = (head(sort(lm2, decreasing = TRUE), 6))
barplot(lm2,
        main = "Top 6 Coefficients (ABS)",
        horiz = TRUE,
        names.arg = c("Shngl", "Roll", "Shake", "CompShg", "Membran", "Metal"))



  #4. E. Create a graph to show the prediction ability of the model 


ggplot(data=data, aes(lm$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

