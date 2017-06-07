library(data.table)
library(chron)
library(geosphere)
library(ggmap)


dane1="Poniatowski.csv"

palmaLat=52.231 
palmaLon=21.0208

waszyngtonLat=52.2386 
waszyngtonLon=21.0523


zaladuj_dane<-function(plik=dane1, sep=';') {
  tabela <- fread(plik, sep, colClasses = 'character', encoding = "UTF-8", header = FALSE)
  nazwy<-names(tabela)
  nowe_nazwy<-c("Lat", "Lon", "Timestamp", "Line", "Group")
  setnames(tabela, nazwy, nowe_nazwy)
  cols<-c(1,2) 
  tabela[,(cols):=lapply(.SD, as.numeric),.SDcols=cols]
  cols<-c(4,5) 
  tabela[,(cols):=lapply(.SD, as.factor),.SDcols=cols] 
  tabela[,Timestamp := as.POSIXct(Timestamp, tz="Europe/Berlin", format="%H:%M:%S")]
  setorder(tabela, "Line", "Group", "Timestamp")
  tabela
}

sprzatanie_danych<-function(tabela) {
  print(nrow(tabela))
  setkey(tabela,NULL)
  tabela<-unique(tabela)
  print(nrow(tabela))
  tabela
}

obliczenia<-function(tabela_raw) {
  tabela<-sprzatanie_danych(tabela_raw)
  tabela[,prevLat:=ifelse(Group==shift(Group), shift(Lat), NA)]
  tabela[,prevLon:=ifelse(Group==shift(Group), shift(Lon), NA)]
  #tabela[,AngularDistance:=sqrt((Lat-prevX)**2+(Y-prevY)**2)]
  #tabela[,Distance:=111*AngularDistance]
  tabela[,DistanceGeosphere:= distm(x=c(Lon, Lat), y=c(prevLon, prevLat), fun = distHaversine), by=list(Lon,Lat,prevLon,prevLat)] #lon first
  tabela[,Interval:=as.numeric(Timestamp-shift(Timestamp))]
  tabela[,Speed:=3.6*DistanceGeosphere/Interval]
  tabela
}

most<-function(tabela_big) {
  tabela<-tabela_big[(((Lon >=palmaLon) & (Lon<=waszyngtonLon))&((Lat>=palmaLat) & (Lat<=waszyngtonLat))),]
  print(nrow(tabela))
  #tabela<-tabela_big[((Lat >=palmaLat) & (Lat<=waszyngtonLat)),]
  tabela[,DistancePalma:=distm(x=c(Lat, Lon), y=c(palmaLat, palmaLon), fun = distHaversine), by=list(Lat,Lon)]
  tabela[,AvSpeed:=(Speed+shift(Speed, type="lag")+shift(Speed, type="lead"))/3]
  tabela[,Direction:=ifelse(shift(Group, n=2, type="lead")==shift(Group, n=2, type="lag"),
                            ifelse(shift(Lon, n=2, type="lag")-shift(Lon, n=2, type="lead")>0, "zachod", "wschod"),NA)]
  tabela[,Direction:=as.factor(Direction)]
  tabela
}


#map_waw <- get_map(location = c(lon=21.03, lat=52.23), zoom = 13)

# mapa<-ggmap(map_waw, darken = 0.3) +
#   geom_count(data=dane_most, aes(Lon, Lat), size=1, color="red") +
#   scale_alpha(range = c(0.05, 0.7)) +
#   theme_void() +
#   theme(legend.position = "none")

#print(mapa)

speed_plot<-function() {
  gg<-ggplot(data=dane_most[AvSpeed<100], aes(x=DistancePalma, y=AvSpeed, colour=Group, linetype=Direction)
            ) + geom_line(size=0.75
            ) + facet_wrap(~Line
            ) + theme_gray(base_size = 14
            #)+theme(legend.position = "none"
            ) + ylab("Srednia predkosc z 30 sekund") +xlab("Odleglosc od palmy"
            ) +scale_y_continuous(minor_breaks = seq(0 , 100, 5), breaks = seq(0, 100, 25))+ scale_colour_brewer(palette = "Paired")
  print(gg)
  gg
}