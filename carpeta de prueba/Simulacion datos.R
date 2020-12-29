
pacman::p_load(tidyverse, assertr, fastDummies,rebus)


datos=function(x){
  data.frame(cbind(NSE=sample(c("bajo","medio","alto"),size=x, replace = T),
                   sexo=sample(c("h","m"),size=x, replace = T),
                   zona=sample(c("norte","centro","sur"),size=x,replace = T)))
}


base=datos(10000)



base_dum=fastDummies::dummy_cols(base)

str_c(colnames(base_dum[,c(4:11)]), c("*0.3","*0.8","*0.5","*0.2","*0.5","*0.5","*0.8","*0.3"),collapse = "+")



base_dum=base_dum %>% mutate(depresion = NSE_alto*0.3+NSE_bajo*0.8+NSE_medio*0.5+sexo_h*0.2+sexo_m*0.5+zona_centro*0.5+zona_norte*0.8+zona_sur*0.3)
base_dum=base_dum %>%
  mutate(depresion2 = rnorm(nrow(base_dum),NSE_alto*0.3+NSE_bajo*0.8+NSE_medio*0.5+
                              sexo_h*0.2+sexo_m*0.5+zona_centro*0.5+zona_norte*0.8+zona_sur*0.3,1))


base_dum=base_dum %>% mutate(ansiedad = rnorm(nrow(base_dum), depresion2+runif(nrow(base_dum),-1,1),2))

plot(base_dum$depresion2, base_dum$ansiedad)
cor(base_dum$depresion2, base_dum$ansiedad)


psicometricos=base_dum[,!str_detect(string=colnames(base_dum),pattern = one_or_more(char_class("_")))] %>%
  mutate(across(.cols=where(is.numeric), scale)) %>% select(depresion=depresion2,ansiedad)

base=cbind(base,psicometricos)





