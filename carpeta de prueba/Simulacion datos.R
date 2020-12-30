
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

base$depresion=base$depresion[,1]
base$ansiedad=base$ansiedad[,1]
### Esto fue un pull y un push, para probar a ver si funcionaban #################
############ Acá voy a agregar alguna lesera en Github y veamos cómo ocurre el Merge cuando hago Pull:
## veamos si ahora hago push

cor(base$depresion, base$ansiedad) ## Esto es para echar a perder el código.


###################################################################3
glimpse(base)
base$zona=factor(base$zona,levels=c("sur","centro","norte"))
base$NSE=factor(base$NSE,levels=c("bajo","medio","alto"))

# Podría interesarnos saber cómo se vincula la depresión con las variables de NSE y 
# Zona geográfica

# Hagamos gráficos simples:

base %>% ggplot(aes(x=zona, y=depresion)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult=2), geom = "errorbar", size=1, width=0.5)+
  stat_summary(fun.y = "mean", geom = "point", size=2, aes(color=NSE, group=NSE)) +
  stat_summary(fun.y = "mean", geom = "line", size=1, aes(color=NSE, group=NSE), alpha=.5) +
  theme_bw() + geom_point(alpha=.05, position=position_jitter(.3),size=.5) +
  theme(legend.position = "top", legend.title = element_text(size=5))







