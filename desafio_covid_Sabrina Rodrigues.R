### Desafio Coronav√≠rus
#chamando library

library(esquisse)
library(ggplot2)
library(dplyr)
library(animation)

rm(list = ls())
dev.off(dev.list()["RStudioGD"])
cat("\014")

### Lendo aquivos csv
caso_full <- read.csv("C:/Users/sabri/OneDrive/Documentos/UNIFOR/3.IntroduÁ„o ao R/caso_full.csv",colClasses = c(date = "Date"))
obito_cartorio <- read.csv("C:/Users/sabri/OneDrive/Documentos/UNIFOR/3.IntroduÁ„o ao R/obito_cartorio.csv",colClasses = c(date = "Date"))

### Retirando linhas com falta de observa√ß√µes
sum(complete.cases(caso_full))
s_caso_full2 <- caso_full[complete.cases(caso_full), ]

### Juntando sele√ß√µes
j_casos2 <- merge(x = s_caso_full2, y = obito_cartorio, by = "date", all.x = TRUE)

##GR¡FICO 01: utilizando da dataframe completo e tambÈm do library "esquisse" que auxilia a plotar gr·ficos com tÈcnica arrasta e solta. O que serviu 
#para o entendimento do funcionamento das regras do ggplot
##esquisser(j_casos2)
j_casos2 %>%
  filter(place_type %in% "state") %>%
  filter(state.x %in% c("PE", "CE", "PI", "RN", "BA", "MA", 
                        "AL", "PB", "SE")) %>%
  filter(state.y %in% c("PE", "PI", "RN", "SE", "AL", "BA", "CE", "MA", "PB")) %>%
  ggplot() +
  aes(x = date, y = new_confirmed, colour = state.x) +
  geom_line(size = 0.9) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Data",
    y = "N∫ de novos casos",
    title = "Novos casos de covid por estado (NE)",
    subtitle = "Nordeste",
    color = "Estados"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(vars(state.x))


##GRAFICO 02: realizado sem o library mencionado anteriormente e utilizando-se do filtro de estado "CE"

### Selecionando observa√ß√µes
s_caso_full <- caso_full[which(caso_full$state == 'CE'), ]
s_obito_cartorio <- obito_cartorio[which(obito_cartorio$state == 'CE'), ]

### Retirando linhas com falta de observa√ß√µes
sum(complete.cases(s_caso_full))
s_caso_full <- s_caso_full[complete.cases(s_caso_full), ]

### Juntando sele√ß√µes
j_casos <- merge(x = s_caso_full, y = s_obito_cartorio, by = "date", all.x = TRUE)

View(j_casos)

#plotando gr·fico 
ggplot(j_casos ,aes(x=date,y= new_deaths_covid19))+
  geom_line(aes(y = new_deaths_pneumonia_2020,col="Pneumonia(2020)"))+
  geom_line(aes(y = new_deaths_pneumonia_2019,col="Pneumonia (2019)"))+
  geom_line(aes(col="Covid19(2020)"))+
  theme_bw()+
  labs(x = "Dia",
       y = "N∫ de novas mortes",
       color = NULL,
       title = "Comparativo de ”bitos di·rios registrados por pneumonia entre os anos de 2019 e 2020 com a covid19",
       subtitle = "Comparativo entre o ano de 2019 e 2020")+
  theme(legend.position = 'top')

#Gr·fico 03: Abordado o n˙mero de mortos por covid no Cear· por dia

saveGIF(movie.name = "C:/Users/sabri/OneDrive/Documentos/UNIFOR/3.IntroduÁ„o ao R/minha_animacao3.gif",
        interval=.1,ani.height=422,ani.width=622,ani.res=120,{
  for (i in 1:nrow(j_casos)) {
    print(i)
    plot(j_casos$date[1:i],j_casos$deaths_covid19[1:i],
         type = "l",
         col = "red",
         xlim = range(j_casos$date),
         ylim = range(j_casos$deaths_covid19),
         xlab = "Data",
         ylab = "N∫ de mortes",
         main = "”bitos acumulados por covid-19 no Cear·")
  }
}
)

