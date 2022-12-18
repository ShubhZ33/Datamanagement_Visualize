#setup
install.packages("hot.deck")
library(readxl)
library(tidyverse)
library(hot.deck)
install.packages("mice")
library(mice)
# read data
df <-  read_excel('~/Documents/TCD/7141 visaliaztion/group report/new.data.xlsx')

df[df=='NA']=NA

data=df[,-2]
# convert into numeric
sapply(data,class)
data <- data %>% 
  mutate_all(as.numeric) %>% 
  data.frame(.)

#multiple imputation
imp <- mice(data,m=5)
a=complete(imp,action=1)
b=complete(imp,action=2)
c=complete(imp,action=3)
d=complete(imp,action=4)
e=complete(imp,action=5)
new.df <- (a+b+c+d+e)/5

new.df$Company=df$Company

#write csv
write.csv(new.df,file='~/Desktop/twitte+facebook.csv')


# hot deck
new.df <- hot.deck(df, m = 1)$data[[1]]
new.df$Company = data$Company
new.df$Twitter_Username = data$Twitter_Username

#write csv
write.csv(new.df,file='~/Desktop/twitter.csv')
#_______________________________________________________________
library(tidyverse)
library(ggplot2)
library(knitr)
library(gt)
library(tidyverse,warn.conflicts = F)
library(Hmisc)
library(car)
library(caret)
# read new dataset
mydata <-  read_excel('~/Desktop/Twitter_fb_combined_final.xlsx')
# as.factor
mydata$Year= as.factor(mydata$Year)


final.data <- mydata[, -c(1,2,20)]

#corelation

df_cor <- Hmisc::rcorr(as.matrix(final.data))
data.frame(df_cor$r) %>% head %>% gt()

cors <- function(y){
  M <- Hmisc::rcorr(as.matrix(y))
  Mdf <- map(M,~data.frame(.x))
  return(Mdf)}

cors(final.data) %>% map(~rownames_to_column(.x,var='measure1')) %>% 
  first() %>% head() %>% gt()
#________________________________________________________
cors(final.data) %>% 
  map(~rownames_to_column(.x, var="measure1")) %>%
  map(~pivot_longer(.x, -measure1, "measure2")) %>%
  first() %>%
  head() %>%
  gt()
#____________________________________________________________

cors(final.data) %>%
  map(~rownames_to_column(.x, var="measure1")) %>%
  map(~pivot_longer(.x, -measure1, "measure2")) %>%
  bind_rows(.id = "id") %>%
  head() %>%
  gt()
#_____________________________________________________________
cors(final.data) %>%
  map(~rownames_to_column(.x, var="measure1")) %>%
  map(~pivot_longer(.x, -measure1, "measure2")) %>%
  bind_rows(.id = "id") %>%
  pivot_wider(names_from = id, values_from = value) %>%
  head() %>%
  gt()
#_______________________________________________________________
cors(final.data) %>%
  map(~rownames_to_column(.x, var="measure1")) %>%
  map(~pivot_longer(.x, -measure1, "measure2")) %>%
  bind_rows(.id = "id") %>%
  pivot_wider(names_from = id, values_from = value) %>%
  rename(p = P) %>%
  mutate(sig_p = ifelse(p < .05, T, F),
         p_if_sig = ifelse(sig_p, p, NA),
         r_if_sig = ifelse(sig_p, r, NA)) %>%
  head() %>% 
  gt()
#____________________________________________________________
formatted_cors <- function(final.data){
  cors(final.data) %>%
    map(~rownames_to_column(.x, var="measure1")) %>%
    map(~pivot_longer(.x, -measure1, "measure2")) %>%
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = value) %>%
    rename(p = P) %>%
    mutate(sig_p = ifelse(p < .05, T, F),
           p_if_sig = ifelse(sig_p, p, NA),
           r_if_sig = ifelse(sig_p, r, NA)) 
}
formatted_cors(final.data) %>% head() %>% gt()
#______________________________________________________________
formatted_cors(final.data) %>%
  ggplot(aes(x = measure1, y = measure2, fill = r)) +
  geom_tile()
#_____________________________________________________________
#_____________________________________________________________
formatted_cors(final.data) %>%
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill ="" , title="",
       subtitle="") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  geom_text(size=1.5) +
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(axis.text=element_text(size=5,face = "bold"),axis.text.x=element_text(size=5,angle=90,hjust=1))
#———————————————————————————————————
final.data = final.data[,-c(2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,20,22,27)]

#model Fit
model1 <- lm(PCT_WOMEN_ON_BOARD~Tweets+ Images +followers_count+Shares +
               `FB Images` +`FB Posts`, data=final.data)
summary(model1)

model2 <-  nls(PCT_WOMEN_ON_BOARD~thetal/(1+exp(Tweets+ Images +followers_count+Shares +
                 `FB Images` +`FB Posts`))), startdata=final.data)










model2 <- lm(ROBECOSAM_TOTAL_STBLY_RANK ~. , data=final.data)
summary(model2)
 
model3 <- lm(TOT_GHG_CO2_EM_INTENS_PER_SALES~.,data=final.data)
summary(model3)

model4 <- lm(PCT_WOMEN_EMPLOYEES~., data=final.data)
summary(model4)

model4 <- lm(TOT_GHG_CO2_EM_INTENS_PER_SALES ~ `FB Images` + `FB Proportion relevant` ,
             data=final.data)
summary(model4)
plot(model4)

model5 <- lm(ESG_DISCLOSURE_SCORE ~ followers_count+Tweets +`FB Posts`,
             data=final.data)
summary(model5)


model7 <- lm(PCT_WOMEN_ON_BOARD ~ Images + `FB Images`,data=final.data)
print(model7)
summary(model7)
plot(model7)

#
p <- par(mfrow=c(2,2))

plot(model7)
#
install.packages('gvlma')
library(gvlma)
gvlma(model7)
influencePlot(model7,id=list(method='identify'),main='influence plot')

#___________________________________________________________________
twitter.df <- read.csv("~/Desktop/twitter.csv")
twitter.df=twitter.df[,c(9,20,23,24,25,26,30,31,32,33)]

model8 <- lm(ESG_DISCLOSURE_SCORE~., data=twitter.df)
summary(model8)









