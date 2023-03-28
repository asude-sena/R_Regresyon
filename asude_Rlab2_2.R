install.packages("tidyverse")
install.packages("moderndive")
install.packages("skimr")
library(tidyverse)
library(moderndive)
library(skimr)
data(evals)
asude1_puanlar<-evals%>%
  select(ID,score,bty_avg,age)
glimpse(asude1_puanlar)
asude1_puanlar%>%
  sample_n(size = 7)
asude1_puanlar%>%
  summarize(guzellik_ort = mean(bty_avg),puan_ort = mean(score),
            guzellik_ortanca = median(bty_avg),puan_ortanca = median(score))
asude1_puanlar%>% select(score,bty_avg)%>%skim()
asude1_puanlar%>%
  summarize(korelasyon = cor(score,bty_avg))

ggplot(asude1_puanlar,aes(x = bty_avg , y = score))+
  geom_point() +
  labs(x = "Güzellik Skoru",
       y = "Değerlendirme Puanı",
       title = "Güzellik skoru ve puan ilişkisini gösteren saçılım grafiği")

ggplot(asude1_puanlar,aes(x = bty_avg , y = score))+
  geom_jitter() +
  labs(x = "Beauty Score",
       y = "Teaching Score",
       title = "Scatterplot of relationship of teaching and beauty scores")  

ggplot(asude1_puanlar,aes(x = bty_avg , y = score))+
  geom_point() +
  labs(x = "Güzellik Skoru",
       y = "Değerlendirme Puanı",
       title = "Güzellik skoru ve puan ilişkisini gösteren saçılım grafiği")+
  geom_smooth(method = "lm", se = FALSE)

asude2_model<-lm(score ~ bty_avg , data = asude1_puanlar)

get_regression_table(asude2_model)
asude3_artıklar<-get_regression_points(asude2_model)
