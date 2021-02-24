# LIBEK ----
library(dplyr)
library(plotly)
library(ggplot2)
library(hrbrthemes)  
library(tidyr)  
library(viridis)     
library(gmodels)
library(datasets)
library(caret)
library(e1071)


# ADATOK BEOLVASÁSA ----
sgc_data <- read.table("https://raw.githubusercontent.com/bbalint642/South_German_Credit_Dataset/main/SouthGermanCredit.asc", header = TRUE)

# ADATOK FELDOLGOZÁSA ----

# a numerikus valtozok kivetelevel minden valtozo faktor
for (i in setdiff(1:21, c(2,4,5,13)))
  sgc_data[[i]] <- factor(sgc_data[[i]])

# hitel celja -> faktor
sgc_data[[4]] <- factor(sgc_data[[4]], levels=as.character(0:10))


# szintek megadasa

#hitel kockazat
levels(sgc_data$credit_risk) <- c("rossz", "jó")

#fizetes
levels(sgc_data$status) = c("nincs folyószámla",
                            " < 0 DM",
                            "0<=  < 200 DM",
                            ">= 200 DM / min. 1 éve")

#hiteltorténet
levels(sgc_data$credit_history) <- c(
  "késedelmes fizetés",
  "kritikus / hitelek máshol",
  "nincs hitel / minden visszafizetve",
  "a hiteleket visszafizették",
  "összes hitel visszafizetve")

#hitelek celjai
levels(sgc_data$purpose) <- c(
  "egyéb",
  "autó (új)",
  "autó (használt)",
  "bútor/berendezések",
  "rádió/televízió",
  "háztartási készülékek",
  "javítások",
  "oktatás", 
  "üdülés",
  "átképzés",
  "üzleti")

#megtakaritasok
levels(sgc_data$savings) <- c("ismeretlen/nincs megtakarítási számla",
                              "... <  100 DM", 
                              "100 <= ... <  500 DM",
                              "500 <= ... < 1000 DM", 
                              "... >= 1000 DM")
#alkalmazott
levels(sgc_data$employment_duration) <- 
  c(  "munkanélküli", 
      "< 1 év", 
      "1 <= ... < 4 év",
      "4 <= ... < 7 év", 
      ">= 7 év")

#torleszto reszlet
sgc_data$installment_rate <- ordered(sgc_data$installment_rate)
levels(sgc_data$installment_rate) <- c(">= 35", 
                                       "25 <= ... < 35",
                                       "20 <= ... < 25", 
                                       "< 20")
#egyeb adosok
levels(sgc_data$other_debtors) <- c(
  "nincs",
  "társ-pályázó",
  "kezes"
)

#nemek
# megozvegyult noket nem emlitett az ertelmezo tabla
levels(sgc_data$personal_status_sex) <- c(
  "férfi : elvált",
  "nő : nem egyedülálló vagy férfi : egyedülálló",
  "férfi : házas/özvegy",
  "nő : egyedülálló")


#lakhely
sgc_data$present_residence <- ordered(sgc_data$present_residence)
levels(sgc_data$present_residence) <- c("< 1 év", 
                                        "1 <= ... < 4 év", 
                                        "4 <= ... < 7 év", 
                                        ">= 7 év")

#legertekesebb tulajdonok
levels(sgc_data$property) <- c(
  "ismeretlen / nincs tulajdon",
  "autó vagy egyéb",
  "építő társaságok megtakarítási megállapodása/életbiztosítás", 
  "ingatlan")

#egyeb reszletfizetesi tervek
levels(sgc_data$other_installment_plans) <- c(
  "bank",
  "boltok",
  "nincs")

#lakhatas modja
levels(sgc_data$housing) <- c("ingyenesen", "bérlés", "saját tulajdon")

#hitelek
sgc_data$number_credits <- ordered(sgc_data$number_credits)
levels(sgc_data$number_credits) <- c("1", "2-3", "4-5", ">= 6")

#munka tipusa
levels(sgc_data$job) <- c(
  "munkanélküli / szakképzetlen - nem rezidens",
  "szakképzetlen - rezidens",
  "képzett alkalmazott / tisztviselő",
  "menedzser / önálló alkalmazású / magasan képzett munkavállaló")

#anyagilag fuggok szama
levels(sgc_data$people_liable) <- c("3 vagy tobb", "0 - 2")

#vezetekes telefon
levels(sgc_data$telephone) <- c("nincs", "van (ügyfél neve alatt)")

#kulfoldi munkavallalo
levels(sgc_data$foreign_worker) <- c("igen", "nem")


# ELEMZÉS ----
## ADATOK MEGTEKINTÉSE MAGASABB SZINTEN 
str(sgc_data)
summary(sgc_data)
class(sgc_data)

View(sgc_data)
head(sgc_data)

## KATEGÓRIÁS vs. FOLYTONOS VÁLTOZÓK

### életkor vs egyes változók:

by(sgc_data$age, sgc_data$status, summary)
by(sgc_data$age, sgc_data$credit_history, summary)

by(sgc_data$age, sgc_data$purpose, summary)
# láthatjuk, hogy oktatásra senki nem vett fel hitelt

by(sgc_data$age, sgc_data$savings, summary)
by(sgc_data$age, sgc_data$employment_duration, summary)
# a kevesebb mint 1 éve dolgozók átlagéletkora jelentősen alacsonyabb a többi csoporténál
by(sgc_data$age, sgc_data$installment_rate, summary)
by(sgc_data$age, sgc_data$personal_status_sex, summary)
by(sgc_data$age, sgc_data$other_debtors, summary)
by(sgc_data$age, sgc_data$present_residence, summary)
by(sgc_data$age, sgc_data$property, summary)
# az ingatlan birtoklók átlagéletkora kiemelkedik a többi csoport közül
by(sgc_data$age, sgc_data$other_installment_plans, summary)
by(sgc_data$age, sgc_data$housing, summary)
# jellemzően a legfiatalabbaknak alakul ingyenesen a lakhatása, valamint az idősebbeknek van saját tulajdonú lakása
by(sgc_data$age, sgc_data$number_credits, summary)
# a sok hitelt felveők között 4-5 vagy több magasabb az átlagéletkor
by(sgc_data$age, sgc_data$job, summary)
by(sgc_data$age, sgc_data$people_liable, summary)
by(sgc_data$age, sgc_data$telephone, summary)
by(sgc_data$age, sgc_data$foreign_worker, summary)
by(sgc_data$age, sgc_data$credit_risk, summary)


## vizualizációk

boxplot(sgc_data$age~sgc_data$property, notch=TRUE, col=c("lightgreen","darkgreen","gold","orange"), main="Életkor vs legértékesebb tulajdon", xlab ="tulajdon", ylab="életkor" )


# sűrűség plot
density_age_property <- ggplot(sgc_data, aes(x=age, group=property, fill=property, alpha=.5))+
  geom_density(adjust=1)+ 
  labs(title="Életkor és legértékesebb tulajdon")+
  theme_ipsum()
density_age_property


# sűrűség plot
density_age_prop2 <- ggplot(sgc_data, aes(x=age, group=property, fill=property, alpha=.5))+
  geom_density(adjust=1)+ 
  facet_wrap(~sgc_data$property, ncol = 4)+
  labs(title="Életkor és legértékesebb tulajdon")+
  theme_ipsum()
density_age_prop2



##  KATEGÓRIÁS VS KATEGÓRIÁS VÁLTOZÓK ----

##   Cross Tabulation (xtabs) / Kontingencia táblák

xtab_history_vs_risk <- xtabs(~credit_risk+credit_history, sgc_data)
xtab_history_vs_risk

plot(xtabs(~credit_risk+credit_history, sgc_data), col=c("seagreen","lightgreen"), main="hitelkockázat és hiteltörténet", xlab="kockázat", ylab="hiteltörténet")






# ÁSSUNK MÉLYEBBRE: Pearson-féle Khi négyzet próba ----

#   a következőkben khi négyzetösszegekkel meg fogjuk határozni a p-értéket, hogy megtudjuk az egyes változók szignifikancia szintjét
#   ha a p-értéket > 0.05 akkor nem szignifikáns 
#   figyeljünk arra, hogy az Y változó a függő változó (tehát jelen esetben a függvény második paramétere: credit_risk)

# P < 0,05 --- ITT SORAKOZNAK A SZIGNIFIKÁNS VÁLTOZÓK

CrossTable(sgc_data$status, sgc_data$credit_risk, chisq = TRUE, prop.t = F)
#   a p-érték nagyon kicsi (jelentősen kisebb, mint 0.05) tehát a status fontos változó

CrossTable(sgc_data$credit_history, sgc_data$credit_risk, chisq = TRUE, prop.t = F)
#   a p-érték nagyon kicsi (jelentősen kisebb, mint 0.05) tehát a credit_history

CrossTable(sgc_data$purpose, sgc_data$credit_risk, chisq = TRUE, prop.t = F)
#   a p-érték nagyon kicsi (jelentősen kisebb, mint 0.05) tehát a purpose változó szignifikáns hatással van a credit_risk kimenetére

CrossTable(sgc_data$savings, sgc_data$credit_risk, chisq = TRUE, prop.t = F)
# a p-érték nagyon kicsi (jelentősen kisebb, mint 0.05) tehát a savings változó szignifikáns hatással van a credit_risk kimenetére

CrossTable(sgc_data$employment_duration, sgc_data$credit_risk, chisq = TRUE, prop.t = F)
# a p-érték nagyon kicsi (jelentősen kisebb, mint 0.05) tehát az employment_duration szignifikánsnak tűnik

CrossTable(sgc_data$property, sgc_data$credit_risk, chisq = TRUE, prop.t = F)
# a p-érték nagyon kicsi, fontos változó

CrossTable(sgc_data$other_installment_plans, sgc_data$credit_risk, chisq = TRUE, prop.t = F)
# kicsi p-érték

CrossTable(sgc_data$housing, sgc_data$credit_risk, chisq = TRUE, prop.t = F)
# nagyon nagyon kicsi P

# ------------------------------------------------------------------------------------------------------


## P > 0,05 --- ITT SORAKOZNAK A NEM SZIGNIFIKÁNS VÁLTOZÓK

CrossTable(sgc_data$personal_status_sex, sgc_data$credit_risk, chisq = TRUE, prop.t = F)
#   a p-érték nagyobb, mint 0.05 tehát a jelek szerint a nem és státusz nem szignifikáns változó

CrossTable(sgc_data$installment_rate, sgc_data$credit_risk, chisq = TRUE, prop.t = F)

CrossTable(sgc_data$other_debtors, sgc_data$credit_risk, chisq = TRUE, prop.t = F)

CrossTable(sgc_data$present_residence, sgc_data$credit_risk, chisq = TRUE, prop.t = F)

CrossTable(sgc_data$number_credits, sgc_data$credit_risk, chisq = TRUE, prop.t = F)

CrossTable(sgc_data$job, sgc_data$credit_risk, chisq = TRUE, prop.t = F)

CrossTable(sgc_data$people_liable, sgc_data$credit_risk, chisq = TRUE, prop.t = F)

CrossTable(sgc_data$telephone, sgc_data$credit_risk, chisq = TRUE, prop.t = F)


## EZ ITT EGY SPECIÁLISABB ESETNEK TŰNIK ----
CrossTable(sgc_data$foreign_worker, sgc_data$credit_risk, chisq = TRUE, prop.t = F)
# Statistics for All Table Factors


# Pearson's Chi-squared test 
# ------------------------------------------------------------
# Chi^2 =  6.737044     d.f. =  1     p =  0.009443096 

# Pearson's Chi-squared test with Yates' continuity correction 
# ------------------------------------------------------------
# Chi^2 =  5.821576     d.f. =  1     p =  0.01583075 


##  FOLYTONOS VS FOLYTONOS VÁLTOZÓK ----

#   keressünk összefüggéseket az életkor és felvett hitel mértéke között (van-e egyáltalán? ha igen, mi lehet az?)

scatter.smooth(sgc_data$age, sgc_data$amount, main="Kor és felvett hitel mértéke", xlab="kor", ylab="felvett hitel")



# INTERAKTÍV PLOTOK : ----

# histogrammok
plot_ly(x = ~sgc_data$age, type = "histogram")
plot_ly(x = ~sgc_data$amount, type = "histogram")
plot_ly(x = ~sgc_data$duration, type = "histogram")

# státusz és kockázat
figure1 <- sgc_data
figure1 <- figure1 %>% count(status, credit_risk)
figure1 <- figure1 %>% plot_ly(x = ~status, y = ~n, color = ~credit_risk) %>%
  layout(title="Státusz és kockázat")

figure1


#kördiagram: 

apply(sgc_data[c("status", "credit_risk", "employment_duration")], 2, table)


labels = c('munkanélküli', '< 1 év', '1 <= ... < 4 év', '4 <= ... < 7 év', '>= 7 év')
values = c(62, 172, 339, 174, 253)

pie_chart1 <- plot_ly(type='pie', labels=labels, values=values, 
                      textinfo='label+percent',
                      insidetextorientation='radial') %>%
  layout(title="Alkalmazásban eltöltött idő")
pie_chart1

# státuszok
factor(sgc_data$status)
status_to_table = table(sgc_data$status)
table_status = as.data.frame(status_to_table)
names(table_status)[1] = 'status'
table_status

statuszok <- plot_ly(
  x = c("nincs folyószámla", "< 0 DM", "0<=  < 200 DM", ">= 200 DM / min. 1 éve"),
  y = c(274, 269, 63, 394),
  name = "status",
  type = "bar"
) %>%
  layout(title="Folyószámla státusza / havi bevétel")
statuszok



# MODELLEK: ----

## készítsünk listát az adathalmaz 80%-ról a traininghez
inTraining <- createDataPartition(sgc_data$housing, p=0.80, list = FALSE)

## használjuk fel az előkészített 80%-ot
training <- sgc_data[inTraining,]

## használjuk a maradék 20%-ot validáláshoz
validation <- sgc_data[-inTraining,]

#--------------------------------------------------------
## futtassunk algoritmust 10x-es cross validationhoz
control <- trainControl(method="cv", number = 10)
metric <- "Accuracy"

### a) lineáris algoritmusok
set.seed(7)

fit.lda <- train(housing~., data=sgc_data, method="lda", metric=metric, trControl=control)
predictions <- predict(fit.lda, validation)
predictions
confusionMatrix(predictions, validation$credit_risk)


## Variable importance: ----

vmiImp <- varImp(validation, scale = FALSE)




# VÁLTOZÓK FONTOSSÁGA: ----

importance <- varImp(fit.lda)
plot(importance)