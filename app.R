library(data.table)
library(sf)
library(ggplot2)
library(leaflet)
library(dplyr)
library(glue)
library(ggsci)


c('Total Population'='A00002_1',
'Population Density (Per Sq. Mile)'='A00002_2',
'Median Household Income'='A14006_1',
'Housing Units'='A10001_1')->vs



pub<-fread('Final data/Public_Facilities.csv')
pub<-pub[,.(type=POPL_TYPE,name=POPL_NAME,
           lat=Lat,long=Lon,
           addr=gsub('(.*?)\\n.*','\\1',POPL_ADDR1 ))]
pub[,label:=glue_data(.SD,'Name:{name}<br>Type:{type}<br>Address:{addr}')]

pub[,Icon:=fcase(
        type == 'FIRE STATION' , 'fire-extinguisher',
        type == 'LIBRARY', 'book',
        type == 'POLICE STATION', 'user',
        default='tags'
)]


park<-fread('Final data/Parks_Locations_and_Features (1).csv')
park<-park[,.(type=Park_Type,Park_Name,
           lat=Lat,long=Lon,
           addr=Address)]
		   
parkSF<-st_as_sf(park,
                 coords = c('long','lat'),
                 agr = "constant",
                 crs = 4326,
                 stringsAsFactors = FALSE,
                 remove = TRUE)

school<-st_read('Final data/School_Boundaries/School_Boundaries.shp')
fw<-st_read('Final data/Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp')
dis<-st_read('Final data/City_Council_Districts/City_Council_Districts.shp')
cen<-st_read('Final data/2020 census Data/2020_CensusData.shp')
cen<-st_transform(cen,st_crs(dis))
comm<-st_join(cen, dis, join = st_intersects) %>% 
      filter(!is.na(Num)) %>%
      select(A14006_1, Num, Council_Me) %>%
	  st_drop_geometry() %>%
	  `row.names<-`(NULL) %>%
	  rename(income=A14006_1) %>%
	  as.data.table() 
comm[,CouncilDistrict:=paste0(Num,'(',Council_Me,')')]


###leaflet maps          
pal <- colorFactor(c('#E42217','#2B65EC'), domain =c('Private','Public'))

mapO<-leaflet() %>%
addProviderTiles(providers$CartoDB.Positron,  group = "CartoDB" ) %>%
addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo") %>%
addTiles(group = "Basic")  %>%
addMarkers(data=parkSF,clusterOptions=markerClusterOptions(),group='Park',popup=~Park_Name) %>%
addPolygons(data=dis,color = "blue", 
                     weight = 4, 
                     opacity = 0.5, 
                     fillOpacity = 0.5,group='Council Districts') %>%
addPolygons(data=school,group='School',
  label = ~School,
  color = pal(school$SchoolType),
  weight = 2,
  opacity = 1,
  dashArray = "3",
  fillOpacity = 0.7,
  highlightOptions = highlightOptions(
    weight = 2,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE)) %>%
addLegend(pal = pal, values = c('Private','Public'), opacity = 0.7,
          position = "bottomright",title='School')
		  
map2<-mapO %>%
      addAwesomeMarkers(
      data=pub,
      lng=~long,
      lat=~lat,
      group='Public Facilities',
      icon = ~awesomeIcons(library = 'fa', 
                                     icon = Icon,
      							   squareMarker =TRUE,
      							   markerColor ='lightblue',
                                     iconColor = 'red'),
      label=lapply(pub$label,function(x) htmltools:::HTML(x))) 


map3<-map2 %>%
       addPolygons(data = fw, 
                   fillColor = "black",
                   weight = 1,
                   color = "black",
                   fillOpacity = 0.7,
                   group = 'Abandoned Properties') %>%
       addLayersControl(
           baseGroups = c("CartoDB","Nat Geo","Basic"),
           overlayGroups= c('Council Districts',"School","Park",'Public Facilities','Abandoned Properties'),
           options = layersControlOptions(collapsed = FALSE)
         ) %>%
       hideGroup('Park')	   
###########################

library(shiny)
library(shinyWidgets)
library(bslib)


ui<-page_navbar(
theme=bs_theme(version='5',bootswatch='darkly'),
title='Revitalization for South Bend’s Redevelopment Initiative',

nav_panel('Map of Facilities',
leafletOutput('mapOut')
),

nav_panel('Census distribution',
pickerInput('whatV','what index?',choices=vs),
plotOutput('cenMAP')
),

nav_panel('Statistic Summary',
layout_columns(
card(card_header(
      "Income by City Council District"
    ),
	card_body(
	selectInput('whichDisTab2','Which District?',as.character(1:6),multiple=F,selected=as.character(3)),
	plotOutput('statPlot2')
	)),
card(card_header(
      "Redevelopment Corridors"
    ),
	card_body(
	selectInput('whichDisTab3','Which District?',as.character(1:6),multiple=T,selected=as.character(1:3)),
	plotOutput('statPlot3')
	))
)
)
)


server<-function(input,output,session)
{

output$mapOut<-renderLeaflet({
map3
})

output$cenMAP<-renderPlot({
interv<-input$whatV
cen2<-cen %>% select(all_of(interv))
names(cen2)[1]<-'v'

ggplot(cen2)+
   geom_sf(aes(fill=v))+
   theme_bw()+
   labs(fill=names(interv))+
   scale_fill_material(palette='green')+
   theme(legend.position='bottom',
         legend.justification='center',
        legend.key.width=unit(2,'cm'))
   
   
})


output$statPlot2<-renderPlot({
dat2<-fw %>% 
    filter(Outcome_St == "Deconstructed" | Outcome_St == "Demolished") %>% 
    filter(!is.na(Council_Di)) %>%
	st_drop_geometry() %>%
	as.data.table() 

dat2<-dat2[,.(Street_Nam,Zip_Code,Outcome_St,Council_Di)]	  
	 
	
dat2[Council_Di==input$whichDisTab2]->inter.dat2
inter.dat2<-inter.dat2[,.(count=.N),.(Outcome_St,Street_Nam,Zip_Code)]
inter.dat2[,Zip_Code:=as.character(Zip_Code)]
interys<-inter.dat2[,.(n=sum(count)),.(Street_Nam)][order(n)][,Street_Nam]
inter.dat2[,Street_Nam:=factor(Street_Nam,interys)]

ggplot(inter.dat2,aes(y=Street_Nam,x=count,fill=Zip_Code))+
  geom_bar(stat='identity',position='stack')+
  theme_bw()+
  scale_x_continuous(expand=expansion(c(0,0.06)))+
  #labs(title=paste(paste(input$whichOutcome,collapse="&"),"@ District",input$whichDisTab2))+
  scale_fill_hue()
  

})

output$statPlot3<-renderPlot({
ggplot(comm[Num %in% input$whichDisTab3],aes(x=CouncilDistrict,y=income))+
   geom_boxplot(aes(colour=CouncilDistrict,fill = after_scale(alpha(colour, 0.3))),outlier.shape=NA)+
   geom_point(position=position_jitter(width=0.1))+
   theme_minimal(14)+
   guides(x=guide_axis(angle=60))+
   theme(legend.position='none')+
   scale_colour_jama()
})


}


shinyApp(ui,server)
