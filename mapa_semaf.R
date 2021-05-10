#Mapa semáforo COVID19

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, sf,ggspatial, lubridate,scales)

memory.limit(size=999999999999999)
#Liga de Github
semaf<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/covid19/master/semaforo_covidmx.csv",
               encoding="latin",header=TRUE,check.names=FALSE)

###Se cargan los polígonos
pol<-st_read("C:/Users/ALIENWARE/Documents/marcogeoest/mg_sep2019_integrado/conjunto_de_datos/00ent.shp")


##Trabajar con base para poder mapear
semaf<-semaf%>%
  #Fromato de fecha
  mutate(fecha_publica=dmy(fecha_publica,
                           locale=Sys.getlocale("LC_TIME")))%>%
  
#Clave de la entidad a caracter
mutate(cve_ent=ifelse(nchar(cve_ent)==1,
                      paste0("0",cve_ent),cve_ent))%>%
  mutate(color_sem=factor(color_sem))%>%
#Dejar las variables que se requerirán
    select(cve_ent, fecha_publica,color_sem)



#Unir la base con la cartografía

pol<-pol%>%
  left_join(semaf,
            by=c("CVE_ENT"="cve_ent"))
  

#Paleta de colores del semáforo
colores <- c("#fec44f","#fd8d3c","#de2d26","#41ab5d") 

#Mapa
pol%>%
  ggplot() +
  # cambia el color y el grosor de las líneas
  geom_sf(data=pol,aes(fill = color_sem),colour = "white", size = 0.07) +
  # agrega títulos
  scale_fill_manual(values = colores)+
  labs(title = "Semáforo epidemiológico COVID-19 por fecha de publicación",
       subtitle = "Del 1° de junio de 2020 al 7 de mayo 2021",
       caption = "Fuente: @claudiodanielpc con información de la Secretaría de Salud") +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        legend.position = "none",
        #Quitar ejes y marcas
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=20))+
  #Quitar líneas del mapa
  coord_sf(ndiscr = 0) +
  #Formato a la fecha 
  facet_wrap(~fecha_publica,ncol=5)

##salvar la gráfica
ggsave("evolsemaf.png",height = 10,width = 20, units="in",dpi=300)
