


library(tidyverse)
library(readxl)
library(gt)
library(robservable)
library(RColorBrewer)
library(gganimate)
library(gifski)
library(Cairo)
library(reactable)
library(ggplot2)
library(plotly)
library(sf)


datos_earnings <- read_excel("data/datos_earnings.xlsx")

mas_premios <- datos_earnings %>% slice_max(TotalMoney, n=1)

imagen_dota2<- "https://estnn.com/wp-content/uploads/2019/09/dota-2-header-800x450.jpg"

df_graf <- mas_premios %>% add_column(imagen_dota2)

df_graf <- df_graf %>% select(GameName, TotalMoney, TournamentNo, imagen_dota2)








Tabla_dota <- df_graf %>% gt()
Tabla_dota <- Tabla_dota %>% tab_header(title = md("**El juego con más premios repartidos**"),subtitle = md("A fecha: 1/12/2020"))

Tabla_dota <- Tabla_dota %>% tab_options(heading.background.color = "green") %>% tab_options(heading.title.font.size = 15, heading.subtitle.font.size = 13,  column_labels.font.weight =  "bold")


Tabla_dota <- Tabla_dota  %>%
  gt::text_transform(locations = cells_body(columns = vars(imagen_dota2)), fn = function(x) {gt::web_image(x, height = 50)}) %>%  cols_align(align = "center")

Tabla_dota


#Pais con mas earnings totales

aa <- datos_earnings %>%  group_by(Top_Country) %>% summarise(N = n())

bb <- aa  %>% slice_max(N, n= 1)

imagen_EEUU<- "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a4/Flag_of_the_United_States.svg/300px-Flag_of_the_United_States.svg.png"

df_graf2 <- bb %>% add_column(imagen_EEUU)
Tabla_pais <- df_graf2 %>% gt()

Tabla_pais <- Tabla_pais %>%
                   tab_header(title = md("**El país con mayor diversidad de earnings**"),subtitle = md("A fecha: 1/12/2020"))

Tabla_pais <- Tabla_pais %>%  tab_options(heading.background.color = "green") %>% tab_options(heading.title.font.size = 15, heading.subtitle.font.size = 13,  column_labels.font.weight =  "bold")


Tabla_pais <- Tabla_pais  %>%
  gt::text_transform(locations = cells_body(columns = vars(imagen_EEUU)), fn = function(x) {gt::web_image(x, height = 50)}) %>%  cols_align(align = "center")

Tabla_pais


#probamos si funciona esto he cogido el codigo de un gráfico sobre el mercado de la aviación y lo he convertido para que funcionara con mis datos. Primero cambio los nombres que quedan muy mal los nombres completos

datos_earnings$GameName<- recode(datos_earnings$GameName, "Counter-Strike: Global Offensive" = "CSGO","PLAYERUNKNOWN'S BATTLEGROUNDS" = "PUBG")

grafico_ <-datos_earnings %>%
  mutate(GameName = fct_lump_n(f = GameName,n = 10,w = TotalMoney,other_level = "Otros")) %>%
  group_by(GameName)  %>%
  summarise(Premios = sum(TotalMoney)) %>%
  mutate(Proportion = Premios/sum(Premios),
  GameName = fct_reorder(.f = GameName, .x = Proportion,.fun = max, .desc = TRUE),
 GameName = fct_relevel(.f = GameName, "Otros", after = Inf))


 grafico_total<- grafico_ %>%  ggplot(aes(x = "",y = Proportion,fill = GameName)) +
  geom_col(width = 1,color = "black",alpha = 1) +
  theme(legend.position = "right", legend.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold")) +
  labs(title = "Cantidad de dinero repartida", y = element_blank(), x = element_blank(),
       fill = "Nombre del juego") +
  geom_text(aes(label = ifelse(Proportion >0.08,paste0(round(Proportion,3)*100,"%"),"")),position = position_stack(vjust = 0.5))

grafico_total



#Un nuevo gráfico dinámico de años de lanzamiento


grafico_3 <- datos_earnings %>% select(GameName, Releaseyear, Genre) %>%
 group_by(Releaseyear) %>%
  arrange(Releaseyear, GameName) %>%
  mutate(ranking = row_number())%>% filter(ranking <= 11)


cosas <- datos_earnings %>% group_by(Genre, Releaseyear) %>% summarise(N = n())
cosas2 <- cosas %>%  rename(id=Genre) %>% rename(date=Releaseyear) %>%  rename(value=N) %>% filter(date!= "11") %>%  arrange(date)
#install.packages("robservable")




#d <- cosas2 %>%select(-`Province/State`, -Lat, -Long) %>%rename(id = `Country/Region`) %>%group_by(id) %>%summarise(across(everything(), sum)) %>%pivot_longer(-id, names_to = "date") %>%mutate(date = as.character(lubridate::mdy(date))) robservable( "https://observablehq.com/@juba/bar-chart-race",include = c("viewof date", "chart", "draw", "styles"),hide = "draw",input = list(data = cosas2,title = "COVID-19 deaths",subtitle = "Cumulative number of COVID-19 deaths by country",source = "Source : Johns Hopkins University"))

juegos2 <- datos_earnings %>% select(Genre, Releaseyear, GameName)

juegos2 <- juegos2 %>% group_by(Releaseyear, Genre) %>% summarise(N = n())

juegos2a <- juegos2 %>%
  group_by(Releaseyear) %>%
  arrange(Releaseyear, desc(N)) %>%
  mutate(ranking = row_number())%>% filter(ranking <= 10)


nb.cols <- 11
mycolors <- colorRampPalette(brewer.pal(11, "Paired"))(nb.cols)


grafico_final <-   ggplot(juegos2a)+
  geom_col(aes(ranking,N,fill=Genre))+
  scale_fill_manual(values=mycolors)+
  geom_text(aes(ranking,N,label=as.factor(N)),hjust=-0.2,size=5)+
  geom_text(aes(ranking, y=0 , label = Genre), hjust=1.1,size=5) +
  geom_text(aes(x=10, y=max(N) , label = as.factor(Releaseyear)), vjust = 0, hjust=0.5, alpha = 0.1,  col = "black", size = 20)+
  labs(title = "Evolución de la temática de los juegos",
       x=NULL,
       y=NULL)+
  coord_flip(clip = "off")+
  scale_x_reverse()+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0, size=20,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=12, face="italic"),
        plot.margin = margin(1, 4, 1, 3, "cm"))+
  transition_states(Releaseyear,transition_length = 1,state_length = 0,wrap = FALSE)
#install.packages("gifski")

#install.packages("Cairo")



 animate (grafico_final,
        nframes = 1000,
        fps = 30,
        end_pause = 150,
        width = 1000,
        height = 600,
        type = "cairo")

# Gráfico quie complementa el gráfico anterior done se muestra la evolución de las temáticas de los videojuegos según el año en el que se presentaron. En este gráfico que complementa el anterior queria comprovar si la temática de los videojuegos ha evolucionado
datos_earnings$Genre<- recode(datos_earnings$Genre, "First-Person Shooter" = "FPS","Multiplayer Online Battle Arena" = "MOBA")

codigo_grafico_complementario <- datos_earnings %>%  mutate(ReleaseGroup = ifelse(Releaseyear <= 2010,"Antes del 2010", ifelse(Releaseyear <= 2015,"2011 - 2015","2016-Actualidad")))

  grafico_complementario <- codigo_grafico_complementario %>%  ggplot(aes(x = factor(Genre,levels = c("Strategy","Fighting Game","Racing","Sports","FPS","MOBA","Battle Royale")),
           fill = factor(ReleaseGroup,levels = c("Antes del 2010","2011 - 2015","2016-Actualidad")))) +
  geom_bar(position = "fill",color = "black",alpha = 0.75) +
  labs(fill = "Período de lanzamiento", x = element_blank(), y = element_blank()) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Como ha cambiado el genero de los juegos lanzados",caption ="*No he seleccionado todas las variables porque hay algunas que son poco relevantes")

grafico_complementario

#Analisis de sagas tnato Call of Dutty Como FIFA.

Call_of_Duty <- datos_earnings %>% filter(GameName %in% c("Call of Duty: Advanced Warfare", "Call of Duty: Black Ops", "Call of Duty: Black Ops 2", "Call of Duty: Black Ops 4", "Call of Duty: Black Ops III","Call of Duty: Ghosts", "Call of Duty: Infinite Warfare", "Call of Duty: Modern Warfare", "Call of Duty: Modern Warfare 2", "Call of Duty: Modern Warfare 3", "Call of Duty: World War II"
))
Call_of_Duty$GameName<- recode(Call_of_Duty$GameName, "Call of Duty: Advanced Warfare" = "Advanced Warfare","Call of Duty: Black Ops"="Black Ops", "Call of Duty: Black Ops 2"="Black Ops 2", "Call of Duty: Black Ops 4"="Black Ops 4", "Call of Duty: Black Ops III"="Black Ops III","Call of Duty: Ghosts"="Ghosts", "Call of Duty: Infinite Warfare"= "Infinite Warfare", "Call of Duty: Modern Warfare"="Modern Warfare", "Call of Duty: Modern Warfare 2"="Modern Warfare 2", "Call of Duty: Modern Warfare 3"="Modern Warfare 3", "Call of Duty: World War II"="World War II")

fifa <- datos_earnings %>% filter(GameName %in% c( "FIFA 10", "FIFA 11", "FIFA 12", "FIFA 13", "FIFA 14", "FIFA 15", "FIFA 16", "FIFA 17", "FIFA 18", "FIFA 19", "FIFA 20"))

Call <- Call_of_Duty %>% select(Releaseyear, GameName,TotalMoney) %>% arrange(Releaseyear)

#ggplot del fifa y el call of duty




a <- ggplot(data =Call_of_Duty , aes(Releaseyear ,TotalMoney, group=GameName,color =GameName )) + geom_line() + geom_point(size=5) + theme_bw()  + labs(y= "", y = "", caption = "") + theme(plot.title = element_text(hjust = 3)) + scale_x_continuous(breaks = seq(1, 11, 1))  + ggrepel::geom_label_repel(aes(label=GameName),
    fill = "white",
    color = "black", size= 4.5,
    box.padding = unit(1.1, "lines")) +scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) + theme(legend.position = "none")

a


b <-  ggplot(data =fifa , aes(Releaseyear ,TotalMoney, group=GameName,color =GameName )) + geom_line() + geom_point(size=5) + theme_bw()  + labs(y= "", y = "", caption = "") + theme(plot.title = element_text(hjust = 3)) + scale_x_continuous(breaks = seq(1, 11, 1))  + ggrepel::geom_label_repel(aes(label=GameName),
    fill = "white",
    color = "black", size= 4.5,
    box.padding = unit(1.1, "lines")) +scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) + theme(legend.position = "none")
b



#La tabla de buscar y encontrar


datos_buscar <- datos_earnings %>% filter(TotalMoney>500000)
datos_buscar <- datos_buscar %>%  select(GameName,Genre,TournamentNo,TotalMoney) %>% arrange(Genre,TotalMoney)
names(datos_buscar)= c("Dinero Total", "Nombre del Juego", "Género", "Número de Torneos")

#install.packages("reactable")


reactable(datos_buscar, defaultPageSize =  10,  paginationType = "jump", showPageSizeOptions =  TRUE , pageSizeOptions =  c ( 10 , 50 , 100 ),defaultColDef = colDef(
    align = "center",
    minWidth = 70,
    headerStyle = list(background = "lightgreen"),
    filterable = TRUE),  highlight = TRUE, outlined = TRUE,
    columns = list(`Juegos para buscar`
   = colDef(style = function(value) {
    if (value > 0) {
      color <- "#e00000"}
      else {
      color <- "#008000"
    }
    list(color = color, fontWeight = "bold")
  })))




#mapa





aaa <- datos_earnings %>% group_by(Top_Country) %>% count() %>% filter(Top_Country != "United States")

#MAPITA EUROPA


aa <- aaa %>% filter(Top_Country != "Azerbaijan")  %>% filter(Top_Country != "Brazil") %>% filter(Top_Country != "Canada") %>% filter(Top_Country != "China") %>% filter(Top_Country != "Japan")  %>% filter(Top_Country != "Korea, Republic of")  %>% filter(Top_Country != "Japan")  %>% filter(Top_Country != "Malaysia") %>% filter(Top_Country != "Japan")  %>% filter(Top_Country != "None") %>% filter(Top_Country != "Taiwan, Republic of China") %>% filter(Top_Country != "Thailand") %>% filter(Top_Country != "Russian Federation")  %>% filter(Top_Country != "Mexico") %>% filter(Top_Country != "Saudi Arabia")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
df_inner <- inner_join(world, aa, by= c("sovereignt" = "Top_Country"))

europa <-ggplot() + geom_sf(data = world, color = "black") +
    geom_sf(data = df_inner, aes(geometry = geometry, fill = n))  + coord_sf(xlim = c(-13.00, 36.00), ylim = c(35, 67.44), expand = FALSE) +
  theme(panel.background = element_rect(fill = "azure"))  + scale_fill_viridis_c(direction = -1, option = "plasma") + theme_bw() + labs(title = "VICTORIAS PAÍSES EUROPEOS")
ggplotly(europa)
 #mapita asia


aaa$Top_Country <- recode(aaa$Top_Country, "Russian Federation"="Russia")
Asia <- aaa %>% filter(Top_Country %in% c("China", "Korea, Republic of","Malaysia","Russia","Singapore","Viet Nam", "Japan", "Azerbaijan", "Taiwan, Republic of China"))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
df_inner <- inner_join(world, Asia, by= c("sovereignt" = "Top_Country"))

asia <-ggplot() + geom_sf(data = world, color = "black") +
    geom_sf(data = df_inner, aes(geometry = geometry, fill = n))  + coord_sf(xlim = c(31.00, 180.00), ylim = c(-10, 80), expand = FALSE) +
  theme(panel.background = element_rect(fill = "azure"))  + scale_fill_viridis_c(direction = -1, option = "plasma") + theme_bw() + labs(title = "VICTORIAS PAÍSES ASIATICOS")
ggplotly(asia)

