#'
#' ---
#' title: "Analýza časových radov projekt"
#' author: "Matúš Varhaník"
#' date: "18.2.2021"
#' output:
#'    prettydoc::html_pretty:
#'      toc: true    
#'      theme: cayman
#' ---

#' # Načítanie časového radu a základný popis

#'načítanie knižníc 
library(readxl)
library(prettydoc)
library(magrittr)
#library(knitr)
#library(dplyr)
#library(date)
#library(lubridate)
library(randtests)
#library(car)
#library(dynlm)
#library(forecast)
#library(aTSA)
`%>%` <- magrittr::`%>%`  # pipe operator

#' Načítanie dát, pričom dáta sa preškálujú, aby sa pracovalo s menšími hodnotami
D <- read_excel("E:/Semester6/casove_rady/download.xlsx")
D$emp<-D$emp/1000

#' ### Popis časového radu
#' časový rad popisuje vývoj počtu zamestnancov vo Švajčiarsku  
#' v rokoch 1991-2020 za kvartálne obdobia
#'

#' ### Graf a základné vlastnosti
plot(D$time,D$emp,xlab="time", ylab="emp",
     type="l", main="Absolútny počet zamestancov, preškálované")

rbind(maximum = max(D$emp),minimum=min(D$emp),priemer=mean(D$emp),
      med=median(D$emp),odchylka=sd(D$emp) ) 

#' # Dekompozícia časového radu: trend 

#' rozdelenie na trénovaciu a vyhodnocovaciu zložku
dat <-split(D,D$time>= as.POSIXct("2014-04-01"))%>% setNames(c("train","eval"))

#' vykreslenie rozdeleného časového radu
plot(emp ~ time, do.call(rbind, dat), type="n") 
lines(emp ~ time, dat$train)
lines(emp ~ time, dat$eval, col="red")
legend("bottomright", legend=c("trénovacia zložka", "evaluačná zložka"),
       lty=1, col=c(1,2))

#' list pre modely
model <- list()

#' formátovanie času na roky
dat$train$year <- format(as.Date(dat$train$time, format="%Y-%m-%d"),"%Y")
dat$train <- dplyr::mutate(dat$train,year = lubridate::decimal_date(time),
                           quart= lubridate::quarter(time),
                           quart = factor(quart, ordered=F))
dat$train$year <- as.vector(dat$train$year)  

dat$eval$year <- format(as.Date(dat$eval$time, format="%Y-%m-%d"),"%Y")
dat$eval <- dplyr::mutate(dat$eval,year = lubridate::decimal_date(time),
                          quart= lubridate::quarter(time),
                          quart = factor(quart, ordered=F))
dat$eval$year <- as.vector(dat$eval$year)  

#' ## lineárny trend
#' vytvorenie trendu
model$Tlin <- lm(emp ~ year, dat$train)
#' uloženie premenných
dat$train$Tlin <- model$Tlin$fitted.values
#' sumárna štatistika a intervaly spoľahlivosti
summary(model$Tlin)
confint(model$Tlin)

#' ## po častiach lineárny trend
#' výber uzlových hodnôt
knots <- dat$train$year[c(24,55)]
dat$train$dummy1 <- pmax(dat$train$year - knots[1], 0)
dat$train$dummy2 <- pmax(dat$train$year - knots[2], 0)
#vytvorenie modelu
model$TpwlC <- lm(emp ~ year + dummy1 + dummy2, dat$train,x=T) 
#' uloženie premenných
dat$train$TpwlC <- model$TpwlC$fitted.values
# sumárna štatistika a intervaly spoľahlivosti
summary(model$TpwlC)
confint(model$TpwlC)

#' ## kvadratický trend
#' vytvorenie trendu
model$Tqua <- lm(emp ~ year + I(year^2), dat$train)
#' uloženie premenných
dat$train$Tqua <- model$Tqua$fitted.values 
#' sumárna štatistika a intervaly spoľahlivosti
summary(model$Tqua)
confint(model$Tqua)

#' ## exponenciálny trend
#' vytvorenie trendu
model$Texp <- lm(log(emp) ~ year, dat$train)
#' uloženie premenných
dat$train$Texp <- exp(model$Texp$fitted.values)
#' sumárna štatistika a intervaly spoľahlivosti
summary(model$Texp)
confint(model$Texp)

#' ## spoločný graf pre všetky trendy na porovnanie
with(dat$train, {
  plot(emp ~ year, main="Trend",type="l", col="grey")
  lines(year, Tlin, col="red")
  lines(year, Tqua, col="blue")
  lines(year, Texp, col="green")
  lines(year, TpwlC, col="brown")
  legend("topleft", legend=c("Lineárny", "Kvadratický", "Exponenciálny",
   "Po častiach lineárny"), pch=c(1,NA,NA,NA,NA), col=c("red","blue","green","brown"), 
  lty=c(1,1,1,1))
}
)

#' ## Výber vhodného trendu
#' Najvhodnejším trendom pre náš model bude po častiach lineárny trend. Z grafu 
#' prichádzal do úvahy aj kvadratický trend, ale zvolil som po častiach lineárny 
#' na základe koeficientu determinácie.
#'
#' uloženie a vykreslenie reziduí
dat$train$RT <- model$TpwlC$residuals
plot(RT ~ time, dat$train, type="l", main="Reziduum")

#' # Dekompozícia časového radu: sezónna zložka 
#' autokorelačná funkcia
acf(dat$train$RT)
#' na autokorelačnej funkcií nevidíme výraznú sezónnosť časového radu

#' ## model sezónnej zložky pomocou goniometrických funkcií
#' vytvorenie modelu
model$Scos <- lm(RT ~ 
                   cos(2*pi*year/1) + sin(2*pi*year/1) +
                   cos(2*pi*year/1*2) + sin(2*pi*year/1*2)
                 ,dat$train)
#' sumárna štatistika modelu
summary(model$Scos)
#' uloženie odhadnutých parametrov
dat$train$Scos <- model$Scos$fitted.values
#' graf sezónnej zložky pomocou goniometrických funkcií spolu s reziduami
plot(RT ~ year, dat$train, main="Sezónna zložka", type="l")
lines(Scos ~ year, dat$train, col="red", lwd=2)

#' ## model sezónnej zložky pomocou dummy variables
#' príprava dummy variables
dummyS <- as.data.frame(
  sapply(
    'names<-'(2:4, paste0("Q", 2:4)), 
    function(x) (round(( dat$train$year-floor( dat$train$year))*4)+1) %in% x
  ) * 1  
)
dat$train <- cbind( dat$train, dummyS)
#' vytvorenie modelu pomocou dummy variables
model$Smkp <- lm(RT ~ Q2 + Q3 + Q4, dat$train)
#' sumárna štatistika modelu
summary(model$Smkp)
#' uloženie odhadnutých parametrov
dat$train$Smkp <- model$Smkp$fitted.values
#' graf sezónnej zložky pomocou dummy variables spolu s reziduami
plot(RT ~ year, dat$train, main="Sezónna zložka", type="l")
lines(Smkp ~ year, dat$train, col="red", lwd=2)

#' ## výber vhodného modelu sezónnej zložky
#' Oba modely sezónnej zložky sú prakticky totožné, pre jednoduchosť teda 
#' vyberiem modelovanie pomocou dummy variables, nakoľko kvartálne dáta 
#' potrebujú len 3 kvalitatívne premenné.
#' 
#' uloženie a vykreslenie reziduí
dat$train$RTS <- model$Smkp$residuals
plot(RTS ~ year, dat$train, type="l", main="Reziduum")

#' vykreslenie dát spolu so sezónnou zložkou a a trendom. Vidíme, že sezónna 
#' zložka je podľa očakávania nevýrazná
plot(emp~year,dat$train, col="black",type="l",
     main="dáta spolu s trendom a sezónnou zložkou")
lines(TpwlC~year,dat$train,col="red")
lines(I(TpwlC+Smkp)~ year, dat$train,type="l", col="blue")
legend("topleft", legend=c("trend", "sezónna zložka","dáta"), 
       col=c("red","blue","black"), lty=c(1,1,1))

#' # Dekompozícia časového radu: cyklická zložka

#' ## diskrétna Fourierova transformácia, periodogram
dat.FFT <- dat$train$RTS %>% 
  fft() %>%  
  abs() %>% '^'(2) %>% '/'(2*pi*length(.)) %>%   
  head(floor(length(.)/2+1))

omega <- 2*pi*seq(0, along.with = dat.FFT)/nrow(dat$train)  
period <- 2*pi/omega   
plot(omega, dat.FFT, ylab="power density", type="h")
plot(period, dat.FFT, ylab="power density", xlab="period (quartals)", type="h", log="x")

#' ## Výber významných frekvencií
spektrum <- data.frame(f = dat.FFT, omega=omega, period=period)
#' zoradenie od najväčšej po najmenšiu 
spektrum <- spektrum[order(spektrum$f, decreasing = TRUE), ]  
#' test významnosti, ktorý odstráni bezvýznamné periódy
vyznamne <- cbind(spektrum[0,], 
                  data.frame(test.stat = numeric(0), crit.val = numeric(0)))
spek <- spektrum
repeat {
  test <- data.frame(test.stat = spek$f[1]/sum(spek$f),
                     crit.val = 1 - (0.05/nrow(spek))^(1/(nrow(spek)-1))
  ) 
  if(test$test.stat > test$crit.val) {
    vyznamne <- rbind(vyznamne, cbind(spek[1,],test)); 
    spek <- spek[-1,]  
  } else break
}
rm(spek, test)  
#' výpis významných periód
vyznamne 
#' našli sme viacero významných frekvencií, dalej ich spresníme pomocou spojitého
#' výberového spektra 

#' # Spresnenie významných periód
#' vytvorenie spojitej spektrálnej hustoty

SpecDens <- Vectorize(  
  FUN = function(omega, acov=NULL, data=NULL) {
    if(is.null(acov)) {
      if(is.null(data)) {
        stop("Please provide either vector of autocovariance function values or time series data.")
      }
      else acov <- acf(data, type="covariance", plot=FALSE)
    }
    k <- seq(to=length(acov)-1)
    ( acov[1] + 2*sum(acov[-1]*cos(k*omega)) ) / (2*pi)
  },
  vectorize.args = "omega"
)

#' spresnenie významných frekvencií
dat.ACF <- as.numeric( acf(dat$train$RTS, lag.max=nrow(dat$train)/1.2, 
                           type="covariance", plot=FALSE)$acf )   
spresnene <- sapply(
  vyznamne$omega,
  function(x) optimize(SpecDens, 
                       interval=x+c(1,-1)*pi/length(dat.FFT), 
                       acov=dat.ACF, 
                       maximum=T
  )$maximum
)
vyznamne <- cbind(vyznamne, data.frame(omega_sm=spresnene, 
                                       period_sm=2*pi/spresnene
)
)
#' výpis spresnených významných hodnôt, odstránenie prebytočného poľa
vyznamne
rm(spresnene)
#' najvýznamnejšia perióda po zaokrúhlení ostáva rovnaká, nepodarilo sa ju 
#' dalej spresniť. Na model cyklickej zložky som vybral 2 najvýznamnejšie 
#' periódy, jedna bola nedostatočná

#' vytvorenie modelu cyklickej zložky.
model$C <- lm(RTS ~ cos(2*pi*year/(22.6/4)) + sin(2*pi*year/(22.6/4))+
                cos(2*pi*year/(34.6/4)) + sin(2*pi*year/(34.6/4))+
                cos(2*pi*year/(13.5/4)) + sin(2*pi*year/(13.5/4)),
              dat$train,x=T
)

summary(model$C)$sigma
#' 
 
#' cos(2*pi*year/(23/4)) + sin(2*pi*year/(23/4))+
#' uloženie odhadnutých hodnôt,
dat$train$C <- model$C$fitted.values
#' vykreslenie cyklickej zložky spolu s reziduami
plot(RTS ~ year, dat$train, main="Cyklická zložka",type="l")
lines(C ~ year, dat$train, col="red", lwd=2)


#' # Spoločný model pre všetky systematické zložky
model$TSC <- lm(emp ~ year +TpwlC + # trend
                  Smkp + # sezónna zložka
                  C, # cyklická zložka,
                dat$train)
#' sumárna štatistika modelu
summary(model$TSC)
#' koeficient determinácie naznačuje, že model dobre zachytáva časový rad 

#' uloženie celkového modelu a vykreslenie zostávajúcich reziduí
dat$train$TSC <- model$TSC$fitted.values
dat$train$RTSC <- model$TSC$residuals

#' vykreslenie spoločného modelu spolu s pôvodnými dátami 
plot(emp ~ year, dat$train, main="Spoločný model")
lines(TSC ~ year, dat$train, col="red", lwd=2)

#' # Testy náhodnosti reziduí
#' vykreslenie zostávajúcich reziduí
plot(RTSC ~ year, dat$train, main="", type="l")

#' ## -watson test
car::durbinWatsonTest(model$TSC)
#' test naznačuje prítomnosť autokolerácie
#'
#' ## Test nulovosti autokorelácií
which(abs(acf(dat$train$RTSC)$acf[-1]) > 2/sqrt(length(dat$train$RTSC)))
#' vyzera to, že v reziduálnej zložke sa stále nachádzajú významné kolerácie,
#' najviac pre posunutie 1 ale aj iné posunutia
#'
#' ## Znamienkový test
randtests::difference.sign.test(dat$train$RTSC) 
#' znamienkový test naznačuje, že v reziduálnej zložke sa už nenachádza 
#' lineárny trend
#'
#' ## Test pomocou Kendallovho korelačného koeficientu
randtests::rank.test(dat$train$RTSC) 
#' test pomocou korelačného koeficientu zamieta, že v reziduálnej zložke
#' sa nachádza lineárny trend. 
#'
#' ## Test podľa bodov zvratu
randtests::turning.point.test(dat$train$RTSC)
#' tento test naznačuje, že reziduálna zložka nie je náhodná
#'
#' ## Wald-Wolfowitz mediánový test
randtests::runs.test(dat$train$RTSC)
#' mediánový test taktiež naznačuje závyslosť , viac hodnôt sa nachádza nad
#' mediánom
#' 
#' ## vyhodnotenie testov
#' 
#' Reziduálna zložka neobsahuje trend, ale môže obsahovať zvyšky sezónnej alebo 
#' cyklickej zložky.Kedže nie je realizáciou bieleho šumu, budeme ju ďalej
#' modelovať pomocou ARMA modelu
#' 
#' # Modelovanie reziduálnej zložky pomocou modelu ARMA 
#' reziduálnu zložku konvertujeme na objekt typu ts
dat$train$RTSC <- ts(dat$train$RTSC, frequency = 4
)
#' ## Autokorelačná a parciálna autokorelačná funkcia na odhad parametrov 
acf(dat$train$RTSC, ylab="(P)ACF", main="", lwd=2)
tmp <- pacf(dat$train$RTSC, plot=F)
points(tmp$lag+0.2/4, tmp$acf, col="red", type = "h", lwd=2)
legend("topright", legend=c("ACF","PACF"), col = c("black","red"), lty=c(1,1), lwd=2)
#' podľa PACF funkcie by sa mohlo jednať o AR(4) alebo MA(6), ale nevieme to s presnosťou určiť

#' odhad maximálneho stupňa modelu ARMA pre HR procedúru
pmax <- 5
qmax <- 7
kmax <- 10
#' ## Odhad najlepšieho ARMA modelu podľa BIC kritéria
pqBIC <- c()
for(p in 0:pmax) {
  for(q in 0:qmax) {
    if(p==0 & q==0) next    # ARMA(0,0) neodhadne
    fit <- arima(dat$train$RTSC, order=c(p,0,q),method="ML")
    pqBIC <- rbind(pqBIC, c(p,q,BIC(fit)))
  }
}

# zoradenie odhadov podľa BIC
pqBIC <- pqBIC[order(pqBIC[,3]),]
head(pqBIC)

#' vytvoríme 5 najlepších modelov a vykreslíme ich spolu s reziduálnou zložkou
model$Rarma <- list()
for(i in 1:5) {
  p <- pqBIC[i,1]
  q <- pqBIC[i,2]
  id <- paste0("ARMA(", p, ",", q, ")")
  fit <- model$Rarma[[id]] <- arima(dat$train$RTSC, order=c(p,0,q))  
  # je potrebne použiť dvojité zátvorky [[]]
  plot(dat$train$RTSC, main=paste("model", id) )   # zobraz data 
  lines(fitted(fit), col="blue")  # a na nich modelové hodnoty
  plot(fit$residuals, main=paste("rezidua", id, "    sigma =", round(sqrt(fit$sigma2),4)))  
  # vykresli rezíduá
}
#' ## Zostrojenie celkového modelu
#' ako najvhodnejší model sa na základe odchýlky bude pravdepodobne
#'  model ARMA(1,7). Presný model určíme pomocou diagnostiky odhadnutých modelov
#'
#' zobrazenie možného finálneho modelu spolu s pôvodnými dátami
with(dat$train, {
  fit <- TSC + (RTSC - model$Rarma$`ARMA(1,2)`$residuals)
  emp <- 'tsp<-'(ts(emp), tsp(fit))
  plot(emp, type="l")
  lines(fit, col="red")
  legend("bottomright", legend=c("data","model"), lty=1, pch=c(NA,NA), col=c("black","red"))
})

#' ## Diagnostika odhadnutého modelu
#' 
#' 

#' test stacionarity modelu
old <- par(mfrow=c(2,3))
for(i in names(model$Rarma)) {
  forecast:::plot.Arima(model$Rarma[[i]], type='ar', main=paste("ARMA(",i,") inverse AR roots"))
}
par(old)
#' Všetky existujúce korene AR polynómu ležia v jednotkovom kruhu, modely sú stacionárne
#' 
#' ## Test nekorelovanosti reziduí
sapply(1:kmax, function(x) Box.test(model$Rarma[[1]]$residuals, lag=x)$p.value) %>% 
  plot(type="b", ylim=c(0,1.2), xlab="maximálny posun", ylab="p-value", main="Box-Pierce test")
abline(h=0.05, lty="dotted")

for(i in 2:length(model$Rarma)) {
  sapply(1:kmax, function(x) Box.test(model$Rarma[[i]]$residuals, lag=x)$p.value) %>% 
    lines(type="b", col=i)
}
legend("topright", legend=paste0(names(model$Rarma)), col=1:5, pch=1, lty=1, bg = "white")

#' Testu vzhovujú všetky modely okrem MA(1)
#'
#' ## Test normality reziduí
#' 
#' zobrazenie jednotlivých histogramov
for(i in 1:5){
  with(model$Rarma[[i]], {
    sdev <- sd(residuals)
    xrange <- seq(from=-3*sdev, to=3*sdev, by=0.1)
    hist(residuals, breaks=20, freq=F)
    lines(density(residuals)) 
    lines(x = xrange, y = dnorm(xrange, mean=mean(residuals), sd=sdev), col="red")
  })
}

#' Test normality reziduí
names(model$Rarma) %>% 
  sapply(function(x) tseries::jarque.bera.test(model$Rarma[[x]]$residuals)$p.value) %>% 
  round(5)
#' reziduá všetkých ARMA modelov sú normálne rozdelené
#' 
#' Test podmienenej homoskedasticity rezíduí
model$Rarma %>% 
  sapply(function(x) setNames(aTSA::arch.test(x, output=F)[3,5], NULL)) %>% 
  round(5)
#' všetky modely vyhovujú tomuto testu
#' 
#' do úvahy prichádzajú modely ARMA(1,2), ARMA(0,2), ARMA(2,0),ARMA(1,1). 
#' Vyberieme model ARMA(1,2)
model$Rarma <- model$Rarma[[1]]
#' # Predpovede

#' konverzia časového radu na objekt ts a príprava na predpovede
TSP <- list(train = tsp(dat$train$RTSC)) 
dat$train <- lapply(dat$train, function(x) 'tsp<-'(ts(x), value=TSP$train))
dat$train$time <- as.vector(dat$train$time) 
dat$eval$emp <- with(dat$train, ts(dat$eval$emp, start=tsp(emp)[2] + deltat(emp),                             frequency=frequency(emp)))
TSP$eval <- tsp(dat$eval$emp)
dat$all$emp <- ts(c(dat$train$emp, dat$eval$emp), start=TSP$train[1], freq=TSP$train[3])
TSP$all <- tsp(dat$all$emp)


#' vytvorenie regresnej matice pre trénovaciu zložku
regX <- list()   # vytvorime kontajner, bude treba aj na predpovede
auxC <- model$C$x[,(2:7),drop=FALSE]%>%`colnames<-`(paste0("C",1:6))# z cyklickej zložky nechcem prvý stlpec
regX$train <- cbind(model$TpwlC$x)
regX$train <- cbind(regX$train ,dummyS)
regX$train <- cbind(regX$train,auxC)  # a pripoji za trendovu regresnu maticu

#' vytvorenie modelu na predpovedanie
model$TSR <- arima(dat$train$emp, order=c(1,0,2), xreg=regX$train, include.mean=F )

#' regresná matica pre evaluačnú zložku
regX$eval <- data.frame(
  Abs = 1,  # vytvor df so stĺpcom pre absolútny člen
  year = dat$eval$year  # a lin.trend
) %>%   
  transform(dummy1 = pmax(year - knots[1], 0),  # pridaj kval.prem. na zalomenie trendu
            dummy2 = pmax(year - knots[2], 0)
  ) %>% 
  as.matrix() %>%  # konvertuj do matice
  cbind(sapply(2:4, function(x) dat$eval$quart %in% x) %>% 
          `colnames<-`(paste0("Q",2:4)))  %>% 
  cbind(data.frame(
    C1 = cos(2*pi*dat$eval$year/(22.6/4)),C2 = sin(2*pi*dat$eval$year/(22.6/4)))
   ,C3=cos(2*pi*dat$eval$year/(34.6/4)),C4 = sin(2*pi*dat$eval$year/(34.6/4)),
   C5=cos(2*pi*dat$eval$year/(13.5/4)),C6 = sin(2*pi*dat$eval$year/(13.5/4))) %>% as.matrix()
colnames(regX$eval)[1] <- '(Intercept)'


#' regresná matica pre celý časový rad
regX$all <- rbind(regX$train,regX$eval) %>%   as.matrix()

#' viackrokové predpovede
dat$eval <- model$TSR %>% 
  predict(n.ahead=nrow(dat$eval), newxreg=regX$eval, se.fit=TRUE) %>% 
  as.data.frame() %>%
  setNames(c("fit", "se")) %>% 
  dplyr::mutate(lwr = fit + qnorm(0.025)*se, 
                upr = fit + qnorm(0.975)*se,
                se = NULL) %>% 
  data.frame(TSR=.) %>%   # '.' je zastupny znak pre vstup
  cbind(dat$eval, .) 
head(dat$eval,2)

#' graf viackrokových predpovedí
with(dat$eval, {
  plot(emp, type="b")
  lines(TSR.fit, col="blue")
  year <- time(emp)
  polygon(x = c(year, rev(year)), y=c(TSR.lwr, rev(TSR.upr)), 
          col=adjustcolor("blue",alpha.f=0.2), border=F)
})

#' jednokrokové predpovede
dat$eval$TSR1.fit <- 
  (tmp <- forecast::Arima(dat$all$emp, xreg=regX$all, model=model$TSR)) %>% 
  fitted() %>% 
  window(start=start(dat$eval$emp))
dat$eval$TSR1.lwr <- dat$eval$TSR1.fit + qnorm(0.025)*sqrt(tmp$sigma2)
dat$eval$TSR1.upr <- dat$eval$TSR1.fit + qnorm(0.975)*sqrt(tmp$sigma2)

#' graf viacktrokových predpovedí
with(dat$eval, {
  plot(emp, type="b")
  lines(TSR1.fit, col="cyan4")
  year <- time(emp)
  polygon(x = c(year, rev(year)), y=c(TSR1.lwr, rev(TSR1.upr)), 
          col=adjustcolor("cyan",alpha.f=0.2), border=F)
})

dat$train$RTss <- forecast::ma(dat$train$RT, order=4) # seasonal smoothing
ts.plot(cbind(dat$train$RT, dat$train$RTss, 0), col=c(1,2,1), lty=c(1,1,2)) 

dat$train$RTss %>% na.omit() %>% { list(
  adf = tseries::adf.test(., k=ar(diff(.))$order),
  pp = tseries::pp.test(.),
  kpss = tseries::kpss.test(., null="Level")
)} %>% 
  sapply(getElement, name="p.value") %>% round(4)

m<-list()
( m$auto.arima <- forecast::auto.arima(dat$train$emp, xreg=model$Tpwlc$x) )

m$auto.arima$residuals %>% acf(lag.max = 3*4, main="ACF:  RT", ann=F)
m$auto.arima$residuals %>% pacf(lag.max = 3*4, main="PACF:  RT", ann=F)

m <- with(dat$train, {
  list(
    forecast::Arima(emp, order=c(3,1,3), seasonal=list(order=c(2,1,3), period=4)),
    forecast::Arima(emp, order=c(2,1,2), seasonal=list(order=c(2,1,1), period=4)),
    forecast::Arima(emp, order=c(3,1,3), seasonal=list(order=c(1,1,1), period=4)),
    forecast::Arima(emp, order=c(3,1,3), seasonal=list(order=c(2,1,1), period=4)),
    forecast::Arima(emp, order=c(4,1,2), seasonal=list(order=c(1,1,1), period=4)),
    forecast::Arima(emp, order=c(4,1,3), seasonal=list(order=c(1,1,1), period=4)),
    forecast::Arima(emp, order=c(4,1,3), seasonal=list(order=c(2,1,1), period=4)),
    forecast::Arima(emp, order=c(4,1,4), seasonal=list(order=c(1,2,3), period=4))
    
  )
})
names(m) <- sapply(m, function(x) {
  o <- forecast::arimaorder(x)
  paste0("S(", paste0(o[1:3],collapse=","), ")(", paste0(o[4:6],collapse=","),")",o[7])
})
tmp <- sapply(m, BIC)
sort(tmp)[1:5]

m$T <- arima( dat$train$emp, order = c(4,1,4),seasonal=list(order=c(1,2,3), period=4),
  include.mean=FALSE) %>%  print()
m$T$residuals %>% acf(lag.max = 3*4, main="ACF:  RTsarma", ann=F)
m$T$residuals %>% pacf(lag.max = 3*4, main="PACF:  RTsarma", ann=F)
m$TSC <- arima(dat$train$emp, xreg=regX$train, include.mean=FALSE) %>% print()

dat$eval$TSC <- predict(m$TSC, n.ahead=25, newxreg=regX$eval)$pred
dat$eval$TSsarma <- predict(m$T, n.ahead=25, xreg=regX$eval[,1:4])$pred

plot(dat$eval$emp,type="b", main="Viackrokové predpovede",ylab="emp")
lines(dat$eval$TSC, col=2)

dat$all$Tpwlc <- model$TpwlC %>% predict.lm(newdata = as.data.frame(regX$all)) %>% 
  ts()%>% `tsp<-`(tsp(dat$all$emp))

dat$all$TSC1 <- forecast::Arima(dat$all$emp, model=m$TSC, xreg=regX$all) %>% fitted() 
dat$eval$TSC1 <- window(dat$all$TSC1, start(dat$eval$emp))  # urezanie validačnej časti
dat$all$TSsarma1 <- forecast::Arima(dat$all$emp, model=m$T, newxreg=regX$all[,1:4]) %>% fitted()
dat$eval$TSsarma1 <- window(dat$all$TSsarma1, start(dat$eval$emp)) 
plot(dat$eval$emp,type="b", main="Jednokrokové predpovede",ylab="emp")
lines(dat$all$TSsarma1,col=2)

model <- list()
model$HW_aM <- HoltWinters(dat$train$emp, seasonal="additive"); 
model$HW_aM
# model$HW_aM$fitted # výstupom sú okamžité odhady všetkých troch zložiek
#' Vizualizácia sklonu trendu
plot(model$HW_aM$fitted[,"trend"], ylab="sklon trendu") 
plot(model$HW_aM)  # zobrazenie vyrovnaných hodnôt
plot(residuals(model$HW_aM))  # zobrazenie rezíduí
#' ### Predpovede
#' Funkcia hw() z balíku forecast okrem odhadu modelu vypočíta aj predpovede.
model$hw_tlaM <- forecast::hw(dat$train$emp, h=nrow(dat$eval), exponential=FALSE, seasonal="additive")
model$hw_tlaM$model %>% summary()
#' Vizualizácia predpovedí
plot(model$hw_tlaM,main = "Predpovede pomocou exponenciálneho vyrovnávania")  # predpovede
plot(model$hw_tlaM$model) # dekompozícia graficky
dat$train$hw_tlaM <- fitted(model$hw_tlaM$model) # uloží vyrovnané hodnoty
dat$eval$hw_tlaM <- model$hw_tlaM$mean  # ulož predpovede

