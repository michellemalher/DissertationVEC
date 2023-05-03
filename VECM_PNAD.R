#############################################
## VEC model for Price and Wage inflation ### 
#############################################

options(scipen=999)

# Load packages
library(readxl)
library(tidyverse)
library(seasonal)
library(mFilter)
library(vars)
library(tsDyn)
library(tseries)
library(gridExtra)

### Load dataset ###
Base <- read_excel("./Base.xlsx")
Base = Base %>% filter(data > "2003-12-01", data < "2022-01-01")

str(Base)
head(Base)

# Declare time series variables
IpcaGeral <- ts(Base$IpcaGeral, start = c(2004,1), frequency = 12) %>%  log()
dIpcaGeral <- diff(IpcaGeral, differences = 1, lag = 12)

IcGeral <- ts(Base$IcGeral, start = c(2004,1), frequency = 12) %>%  log()
dIcGeral <- diff(IcGeral, differences = 1, lag = 12)

RemTrabTotal <- ts(Base$RemTrabTotal, start = c(2004,1), frequency = 12) %>%  log()
dRemTrabTotal <- diff(RemTrabTotal, differences = 1, lag = 12)

PibReal <- ts(Base$PibReal, start = c(2004,1), frequency = 12) %>%  log()
dPibReal <- diff(PibReal, differences = 1, lag = 12)

# Seasonally adjust data
PibRealAjuste <- seas(PibReal)
PibRealAjuste <- final(PibRealAjuste)

# Real GDP hiatus
hp = hpfilter(PibReal, freq = 14400, type = 'lambda', drift = FALSE)
hpCycle = data.frame(hp$cycle)
HiatoPib = ts(hpCycle, start = c(2004,1), frequency = 12)

# Real GDP hiatus seasonally adjusted
hp = hpfilter(PibRealAjuste, freq = 14400, type = 'lambda', drift = FALSE)
hpCycle = data.frame(hp$cycle)
HiatoPibAjuste = ts(hpCycle, start = c(2004,1), frequency = 12)

rm(hpCycle, hp)

# Include new variables
Base <- Base %>% dplyr::select(1)
Base <-  Base %>% 
  mutate(IpcaGeral = as.numeric(IpcaGeral),
         IcGeral = as.numeric(IcGeral),
         HiatoPib = as.numeric(HiatoPib),
         HiatoPibAjuste = as.numeric(HiatoPibAjuste),
         RemTrabTotal = as.numeric(RemTrabTotal),)

Base <- as.data.frame(Base)

# Lagged variables
IcGeral_lag1 <- stats::lag(ts(Base$IcGeral, start = c(2004,1), frequency = 12), -1) %>% window(start = c(2012,3), end = c(2022,1))
dIcGeral_lag1 <- diff(IcGeral_lag1, differences = 1, lag = 12) %>% window(start = c(2013,3), end = c(2022,1))

plot(cbind(dIcGeral, dIcGeral_lag1))

# Base
Base = Base %>% 
  filter(data > "2012-02-01",
         data < "2022-01-01") %>%
  mutate(dummy_RemCovid = case_when((data >= "2020-02-01") & 
                                      (data < "2020-07-01") ~ 1,
                                    TRUE ~ 0))

tail(Base%>%dplyr::select(data, dummy_RemCovid), 25)

head(Base)
tail(Base)

# Declare time series variables
IpcaGeral <- ts(Base$IpcaGeral, start = c(2012,3), frequency = 12)
dIpcaGeral <- diff(IpcaGeral, differences = 1, lag = 12)

IcGeral <- ts(Base$IcGeral, start = c(2012,3), frequency = 12)
dIcGeral <- diff(IcGeral, differences = 1, lag = 12)

RemTrabTotal <- ts(Base$RemTrabTotal, start = c(2012,3), frequency = 12)
dRemTrabTotal <- diff(RemTrabTotal, differences = 1, lag = 12)

# Hiatus
HiatoPib <- ts(Base$HiatoPib, start = c(2012,3), frequency = 12) %>% window(start = c(2013,3))
HiatoPibAjuste <- ts(Base$HiatoPibAjuste, start = c(2012,3), frequency = 12) %>% window(start = c(2013,3))

plot(cbind(HiatoPib, HiatoPibAjuste))

# Dummy
dummy_RemCovid <- ts(Base$dummy_RemCovid, start = c(2012,3), frequency = 12) %>% window(start = c(2013,3))

plot(cbind(dRemTrabTotal, dummy_RemCovid))

# Plots
plot(cbind(IpcaGeral, RemTrabTotal, IcGeral, IcGeral_lag1, HiatoPib, HiatoPibAjuste),
     main="Series em nivel - I")

plot(cbind(dIpcaGeral, dRemTrabTotal, dIcGeral, dIcGeral_lag1, HiatoPib, HiatoPibAjuste),
     main="Series em primeira diferença sazonal - I")


### TESTE ADF ###
# Level
VARselect(IpcaGeral)
ur.df(IpcaGeral, type = "none", lags = 2) %>% summary()

VARselect(RemTrabTotal)
ur.df(RemTrabTotal, type = "none", lags = 4) %>% summary()

VARselect(IcGeral)
ur.df(IcGeral, type = "none", lags = 5) %>% summary()

VARselect(HiatoPib)
ur.df(HiatoPib, type = "none", lags = 10) %>% summary() #estacionario

VARselect(HiatoPibAjuste)
ur.df(HiatoPib, type = "none", lags = 1) %>% summary() #estacionario

#Diff
VARselect(dIpcaGeral)
ur.df(dIpcaGeral, type = "none", lags = 10) %>% summary()
ur.df(dIpcaGeral, type = "none", lags = 2) %>% summary()

VARselect(dRemTrabTotal)
ur.df(dRemTrabTotal, type = "none", lags = 3) %>% summary()

VARselect(dIcGeral)
ur.df(dIcGeral, type = "none", lags = 6) %>% summary() 
ur.df(dIcGeral, type = "none", lags = 3) %>% summary()

#KPSS test
#Level
kpss.test(IpcaGeral)
kpss.test(RemTrabTotal)
kpss.test(IcGeral)
kpss.test(HiatoPib) #estacionario
kpss.test(HiatoPibAjuste) #estacionario
#Diff
kpss.test(dIpcaGeral)
kpss.test(dRemTrabTotal)
kpss.test(dIcGeral) #estacionario

# dIpca, dRemTrabTotal and dIcGeral are not stationary
# Perform cointegration test 


#################
### VEC Model ###
#################

# Bind
endog <- cbind(dIpcaGeral, dRemTrabTotal) 
exogen <- cbind(dIcGeral_lag1, HiatoPibAjuste, dummy_RemCovid)

# Lag Selection Criteria
lagselect <- VARselect(endog, lag.max = 12, type = "const", exogen = exogen, season = NULL)
lagselect$selection

k = 3 # k is the lag order of the series (levels) in the VAR


# Cointegration test

# Johansen Testing (Trace)
ca.jo1 <- ca.jo(endog, type = "trace", ecdet = "const", K = 3, spec = "transitory", dumvar = exogen, season = NULL)
summary(ca.jo1)# r=1

# Johansen Testing (MaxEigen)
ca.jo2 <- ca.jo(endog, type = "eigen", ecdet = "const", K = 3, spec = "transitory", dumvar = exogen, season = NULL)
summary(ca.jo2) # inconclusivo

r = 1 # r=1


# VECM Model
vecm <- VECM(endog, (k-1), r=r, estim=("ML"), include = "none", LRinclude = "const", exogen = exogen) # VECM we use k-1
summary(vecm) # Resultado dos coeficientes do VECM é esse

# VEC to VAR
vec2var <- vec2var(ca.jo1, r=r) 
vec2var #Transformamos em VAR só para calcular graficos IRF

# Autocorrelation test
serial.test(vec2var, lags.bg = 12, type = "BG") # tem aurocorrel serial
serial.test(vec2var, lags.bg = 12, type = "PT.asymptotic") # nao tem autocorrel serial (a 5%)

#Estimating IRFs
#InfLivres -> InfLivres
irf_1 <- irf(vec2var, 
             impulse = "dIpcaGeral", 
             response = "dIpcaGeral", 
             n.ahead = 15, 
             boot = TRUE,
             ortho = TRUE)
plot(irf_1, ylab="dIpcaGeral", main = "Resposta de dIpcaGeral ao impulso de dIpcaGeral")
irf_1

irf_2 <- irf(vec2var, 
             impulse = "dRemTrabTotal", 
             response = "dRemTrabTotal", 
             n.ahead = 15, 
             boot = TRUE,
             ortho = TRUE)
plot(irf_2, ylab="dRemTrabTotal", main = "Resposta de dRemTrabTotal ao impulso de dRemTrabTotal")
irf_2

irf_3 <- irf(vec2var, 
             impulse = "dRemTrabTotal", 
             response = "dIpcaGeral", 
             n.ahead = 15, 
             boot = TRUE,
             ortho = TRUE)
plot(irf_3, ylab="dRemTrabTotal", main = "Resposta de dIpcaGeral ao impulso de dRemTrabTotal")
irf_3

irf_4 <- irf(vec2var, 
             impulse = "dIpcaGeral", 
             response = "dRemTrabTotal", 
             n.ahead = 15, 
             boot = TRUE,
             ortho = TRUE)
plot(irf_4, ylab="dRemTrabTotal", main = "Resposta de dRemTrabTotal ao impulso de dIpcaGeral")
irf_4

#####################
### IRF function ###

irfPlots_display_vec <- function(endog, model){
  
  endog <- endog
  var <- model
  
  # Create list ggplots
  plot_list = list()
  n <- ncol(endog)
  lags_irf = 25
  
  for (i in 1:n){
    for (j in 1:n){
      var_plot = irf(var, impulse = paste(colnames(var$y)[i]), response=paste(colnames(var$y)[j]),
                     n.ahead = 25, boot = TRUE, ortho = TRUE, ci=0.95)
      
      df <- as.data.frame(cbind(unlist(var_plot$irf), unlist(var_plot$Lower), unlist(var_plot$Upper))) %>%
        mutate(n=c(0:lags_irf)) 
      names(df) <- c("irf", "lower", "upper", "period")
      
      p <- df %>%
        ggplot(aes(x=period, y=irf, ymin=lower, ymax=upper)) +
        geom_hline(yintercept = 0, color="red") +
        geom_ribbon(fill="grey", alpha=0.2) +
        geom_line() +
        theme_light() +
        ggtitle(paste("Imp. from", colnames(var$y)[i], sep = " "))+
        ylab(colnames(var$y)[j])+
        xlab("") +
        theme(plot.title = element_text(size = 11, hjust=0.5),
              axis.title.y = element_text(size=11))
      plot_list[[paste(i,j,sep="")]]<-p
    }
  }
  # Display
  grid.arrange(grobs = plot_list, ncol = n)
}

# Plot IRFs
irfPlots_display_vec(endog = endog, model = vec2var)