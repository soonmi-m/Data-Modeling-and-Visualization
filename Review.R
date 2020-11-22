library(mosaicData)
library(dplyr) #functions like arrange
library(tidyverse) #for ggplot
library(ggplot2) #for ggplot
library(knitr) #for kable
library(ISLR) #for linear modeling
library(broom) #for tidy
library(mosaic) #for ntiles
library(regclass) #for stat_smooth

data(package = "mosaicData")
data(SAT)

#basics of data
SAT[1:5, ]
attach(SAT)
names(SAT)
dim(SAT)
summary(SAT)

#correlation
cor(math, verbal)
cor(SAT[,2:8])

#continuous into groups
salary_split <- matrix(0, nrow=nrow(SAT), ncol=1)
for(i in 1: nrow(SAT)) {
  if(salary[i]>=38.55){salary_split[i] <-1}
  else if((salary[i]<38.55) & (salary[i]>=34.83)){salary_split[i] <-2}
  else if((salary[i]<34.83) & (salary[i]>=33.29)){salary_split[i] <-3}
  else if((salary[i]<33.29) & (salary[i]>=30.98)){salary_split[i] <-4}
  else {salary_split[i] <-5}
}

salary_split <-as.factor(salary_split)
table(salary_split)

#adding new column
SAT <- cbind(SAT, salary_split)
#SAT <- drop(salary_split)

SAT[1:10,]
SAT_bysal <- arrange(SAT, desc(salary))

SAT_bysal[1:10,]

#boxplots without ggplot
par(mfrow=c(1,2))
boxplot(math~salary_split, xlab = "Salary Groups", ylab = "Math Score")
boxplot(verbal~salary_split, xlab = "Salary Groups", ylab = "Verbal Score")

#ggplot exploration 
ggplot(data=SAT, mapping = aes(x=expend, y=math)) + 
  geom_boxplot(mapping = aes(group = cut_number(expend,5))) +
  labs(title = "Math Scores by Expenditure per Student", subtitle = "SAT Scores")
ggplot(data=SAT) + geom_point(mapping = aes(x=expend, y=verbal, color = salary))
ggplot(data=SAT) + geom_point(mapping = aes(x=expend, y=verbal)) + facet_wrap(~salary_split, nrow=2)
ggplot(data=SAT) + geom_bar(mapping = aes(x=salary_split, color=salary_split, fill=salary_split))

#making another column
ratio_split <- matrix(0, nrow=nrow(SAT), ncol=1)
med_ratio <- median(ratio)
for(i in 1: nrow(SAT)){
  if (ratio[i] >= med_ratio){ratio_split[i] <- 1}
  else {ratio_split[i] <- 0}
}
SAT <- cbind(SAT, ratio_split)

#more ggplot
ggplot(data=SAT) + geom_histogram(mapping = aes(x=salary), bins=10) + facet_wrap(~ratio_split, ncol=2) +
  ylab("student ratio")
ggplot(data = SAT, mapping = aes(x=salary_split, y=sat)) + geom_boxplot() + coord_flip()


##switching over to SaratogaHouses data in mosaicData 

summarize(SaratogaHouses, mean_bedrooms = mean(bedrooms))

#using pipe operator
by_NC <- SaratogaHouses %>% group_by(newConstruction) %>%
  summarize(mean=mean(bedrooms))
by_NC
as.data.frame(by_NC)

SaratogaHouses %>% ggplot(aes(x=newConstruction, fill=newConstruction)) +geom_bar()+
  ylab("Count") + scale_fill_brewer(palette="Set1")

#filter search
with_fpwf <- SaratogaHouses %>%
  filter(fireplaces==1, waterfront =="Yes")
dim(with_fpwf)
with_fpwf

#categorical variables
price_fuel_heat <- SaratogaHouses %>% group_by(fuel, heating) %>%
  summarize(mean_p=mean(price), freq=n(), mean_a=mean(age))
kable(price_fuel_heat)

SaratogaHouses %>% ggplot(aes(x=fuel, y=price, fill=fuel)) + geom_boxplot() +
  scale_fill_brewer(palette="Accent")

#data details
SaratogaHouses %>% head(5)
SaratogaHouses %>% glimpse
SaratogaHouses %>% str
SaratogaHouses %>% nrow
SaratogaHouses %>% names

#change zeros to 1 to make price/age a valid variable
SaratogaHouses$age[SaratogaHouses$age == 0] <- 1

#create new variables in a new data set
SaratogaHouses2 <- SaratogaHouses %>% mutate(price/age)
SaratogaHouses2 %>% head(5)

SaratogaHouses2 %>% ggplot(aes(x=price/age)) + geom_histogram(bins=10) + facet_wrap(~centralAir)
SaratogaHouses2 %>% ggplot(mapping=aes(x=factor(rooms), y=price)) + geom_boxplot()

SaratogaHouses2 %>% arrange(price) %>% head(5)

#summary stats
price_summary <- SaratogaHouses %>%
  summarize(
    min= min(price, na.rm=TRUE),
    q1= quantile(price, .25, na.rm=TRUE),
    median = quantile(price, .5, na.rm=TRUE),
    q3 = quantile(price, .75, na.rm=TRUE),
    max = max(price, na.rm=TRUE),
    mean = mean(price, na.rm=TRUE),
    sd = sd(price, na.rm=TRUE),
    missing = sum(is.na(price))
  )
kable(price_summary)

#select and reorder values
s_houses <- SaratogaHouses %>% select(rooms, bathrooms, heating:sewer, everything())
s_houses %>% head(5)

#select columns that start with. similar to 'ends_with' and 'contains'
s_houses_fuelgas <- SaratogaHouses %>%
  select(starts_with("p")) 
s_houses_fuelgas %>% head(5)

#rename columns
SaratogaHouses2 <- SaratogaHouses %>%
  select(contains(c('college','price','age'))) %>%
  rename(gradCollege = pctCollege)
names(SaratogaHouses2)

#ordering
avg_price_heating <- SaratogaHouses %>%
  group_by(heating) %>%
  summarize(avg_price = mean(price)) %>%
  arrange(desc(avg_price))
kable(avg_price_heating)

#top n
top_pctCollege <- SaratogaHouses %>%
  group_by(pctCollege) %>%
  summarize(newConstruction = n()) %>%
  arrange(desc(newConstruction)) %>%
  top_n(n=10)
top_pctCollege

#5th expensive house
SaratogaHouses %>% filter(rank(desc(price))==5)

#age vs price linear model
SaratogaHouses %>% ggplot(aes(x=age, y=price)) + geom_point() + geom_smooth(method='lm', se=FALSE, color='red')
SaratogaHouses %>% summarize(correl=cor(age,price))
model <- lm(formula = price~age, data=SaratogaHouses)
tidy(model) %>% kable()

#check residuals
new_price <- data_frame(age = c(50, 100, 150))
model %>% augment(newdata=new_price) %>% kable()
model_points <- augment(model) %>% select(price, age, .fitted, .resid)
model_points %>% head() %>% kable()

#check expected values
tail(SaratogaHouses)
model_points$.fitted[(nrow(SaratogaHouses)-5):nrow(SaratogaHouses)]
SaratogaHouses$price[(nrow(SaratogaHouses)-5):nrow(SaratogaHouses)]

#check conditions - can clearly tell a linear model is not appropriate.
ggplot(data=model_points, mapping = aes(x=.resid))+ geom_histogram()
ggplot(data=model_points, mapping = aes(x=price, y=.resid)) + geom_point() + geom_abline(intercept=0, slope =0, color ="blue")
ggplot(data=model_points, mapping = aes(sample=.resid))+ stat_qq()

#density dist of price by rooms
ggplot(data = SaratogaHouses, mapping = aes(x=price)) + geom_density(fill = 'gray')
ggplot(data = SaratogaHouses, mapping = aes(x=price)) + geom_density(mapping = aes(color=newConstruction), fill='gray', alpha=.5)
ggplot(data = SaratogaHouses, mapping = aes(x=price)) + geom_density(mapping = aes(color=as.factor(rooms)), fill='gray', alpha=.5)

#create intervals for age and show density of price by rooms
SaratogaHouses <- SaratogaHouses %>% mutate(ageGroup = ntiles(age, n=4, format ='interval'))
ggplot(SaratogaHouses, mapping = aes(x=price))+ geom_density(mapping=aes(color =as.factor(rooms)), fill='gray', alpha=0.5) +
  facet_wrap(~ageGroup)

#boxplot with confidence interval notch
SaratogaHouses$rooms = as.factor(SaratogaHouses$rooms)
ggplot(SaratogaHouses, mapping = aes(x=(rooms), y=price))+
  geom_boxplot(mapping = aes(color = rooms, fill=rooms), notch=TRUE) + facet_wrap(~ageGroup)

#dotplot price vs age with colors by fuel and interval prediction
SaratogaHouses %>% ggplot() + geom_point(aes(x=age, y=price, color=fuel))
SaratogaHouses %>% ggplot(aes(x=age, y=price, color=fuel)) + stat_smooth(se=TRUE, method ='loess')+geom_point()
SaratogaHouses %>% ggplot(aes(x=age, y=price, color=fuel)) + stat_smooth(se=TRUE, method ='gam')+geom_point()
SaratogaHouses %>% ggplot(aes(x=age, y=price, color=fuel)) + stat_smooth(se=TRUE, method ='lm')+geom_point()

#dotplot price vs age split by centralAir
SaratogaHouses %>% ggplot(aes(x=age, y=price))+ geom_point() + geom_smooth(data = subset(SaratogaHouses, centralAir="a")) +
    facet_wrap(~centralAir)

#bivariate densities
SaratogaHouses %>% ggplot(aes(x=age, y=price)) +geom_point()+geom_density2d(color='red')+facet_wrap(~heating, nrow=3)
