##Gr?fica Casos coronavirus M?xico


##Se cargan las librer?as necesarias


if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, gganimate,
               gifski, dplyr,lubridate, scales,ggimage)


##Se importa la base de datos

mx<-read.csv("D:/Documentos/GitHub/covid19/mxcovid19.csv",
             encoding="latin",header=TRUE,check.names=FALSE)



##Se crea la tasa de letalidad
mx<-mx%>%
  mutate(let=acummuertes/Acumulado*100)

##Se modifica de wide
mx<-mx %>% gather(tipo, value, -day,-month,-year)

##Se agrega liga de imagen
mx$imagen <- "https://cdn.clipart.email/09787f9fc843340d7e18c16ba98395ad_coronavirus-icon-of-line-style-available-in-svg-png-eps-ai-_256-256.png#.XnB6RftZaZc.link"

##Se crea variable de fecha

mx$fecha<-with(mx, ymd(paste(year, month, day,sep= ' ')))

##Se crea etiqueta de valor



mx$etiqueta<-ifelse(mx$tipo=="let",paste(format(round(mx$value,1),nsmall=0,big.mark = ",")),
                    paste(format(round(mx$value,0),nsmall=0,big.mark = ",")))


##Se crea paleta de colores
covid_colors <- c("#1c9099", "#feb24c")

##Se crea el gr?fico plano
curva<-mx%>%filter(tipo=="Nuevos casos"|tipo=="Acumulado")
coronavirus<-ggplot(curva, aes(fecha, value)) +
  geom_line(aes(color = tipo, linetype = "solid"),size=2) +
  geom_point(aes(group = seq_along(curva$fecha),color=tipo),size=3)+
  geom_vline(xintercept = ymd("2020-03-23"), linetype="dashed", 
             color = "blue", size=1.5)+
  geom_vline(xintercept = ymd("2020-05-30"), linetype="dashed", 
             color = "red", size=1.5)+
  scale_color_manual(values = covid_colors)+
  geom_text(aes(label=ifelse(curva$fecha =='2021-08-09',
                             curva$etiqueta,"")),
            hjust=0.5,vjust=2,size=5,fontface="bold")+
  scale_y_continuous(labels=comma)+
  theme_bw() +
  labs(
    title = "México. Nuevos casos confirmados y acumulados de COVID19",
      subtitle = "27 de febrero de 2020 al 9 de agosto de 2021",
    y = "Casos",
    x="Fecha",
    caption = "Nota: La línea vertical azul indica el inicio de la Jornada Nacional de Sana Distancia (23 de marzo),
mientras que la línea vertical roja indica la conclusión de ésta (30 de mayo).
Fuente: Elaboración propia con datos de la Secretaría de Salud."
  )+
  theme(plot.title = element_text(hjust = 0, size=20,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
        plot.caption = element_text(hjust = 0,size=10),
        legend.position = "none",
        text=element_text(size=20))+
  facet_wrap(~ tipo, ncol=1, scale = "free_y")

coronavirus
ggsave("curvaepid.png", height=20, width=20, units='in', dpi=300)
animcorona<-coronavirus+
  transition_reveal(curva$fecha)




animcorona %>% animate(fps = 15,
                       nframes = 100,
                       duration=5,
                       end_pause = 25,
                       width = 800, height = 800)

anim_save("mxcovid.gif")



##Muertes

##Se crea paleta de colores
covid_colors <- c("#7fcdbb", "#43a2ca","#2c7fb8")

##Se crea el gr?fico plano
def<-mx%>%filter(tipo=="acummuertes"|tipo=="let"|tipo=="diarias")%>%
  mutate(tipo=ifelse(tipo=="acummuertes",
                     "Acumulado",
                     ifelse(tipo=="let","Tasa de letalidad (%)",
                            ifelse(tipo=="diarias","Defunciones diarias reportadas",""))))

ggplot(def, aes(fecha, value)) +
  geom_line(aes(color = tipo),size=3) +
  geom_vline(xintercept = ymd("2020-03-23"), linetype="dashed", 
             color = "blue", size=1.5)+
  geom_vline(xintercept = ymd("2020-05-30"), linetype="dashed", 
             color = "red", size=1.5)+
  scale_color_manual(values = covid_colors)+
  geom_text(aes(label=ifelse(def$fecha =='2021-08-09',
                             def$etiqueta,"")),
            hjust=0.5,vjust=1.5,size=5,fontface="bold")+
  scale_y_continuous(labels=comma)+
  theme_bw() +
  labs(
    title = "México. Defunciones por COVID19 y tasa de letalidad",
    subtitle = "27 de febrero de 2020 al 9 de agosto de 2021",
    y = "",
    x="Fecha",
    caption = "Nota: La tasa de letalidad es la relación entre el número de defunciones y los casos existentes por 100. 
La línea vertical azul indica el inicio de la Jornada Nacional de Sana Distancia (23 de marzo),
mientras que la línea vertical roja indica la conclusión de ésta (30 de mayo).
Fuente: Elaboración propia con datos de la Secretaría de Salud."
  )+
  theme(plot.title = element_text(hjust = 0, size=20,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
        plot.caption = element_text(hjust = 0,size=10),
        legend.position = "none",
        text=element_text(size=20))+
  facet_wrap(~ tipo, ncol=2, scale = "free_y")

ggsave("defuncovid.png", height=10, width=20, units='in', dpi=300)