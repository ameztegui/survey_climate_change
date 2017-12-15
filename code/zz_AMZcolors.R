# Color palette of 15 elements based on colors of graphic design


AMZ_brickred<- "#d1323a"
AMZ_green <- "#138470"
AMZ_lightblue <- "3c86ab"
AMZ_violet <- "#a985a4"
AMZ_orange<-"#de9416"
AMZ_blue<-"#106ab4"
AMZ_mustard <- "#ffd92f" # "#fde05e"
AMZ_blue2 <- "#104577"
AMZ_maroon <- "#ab2323"
AMZ_pistachio <- "#bcd74a"
AMZ_beige <-"#f8e0c8"
AMZ_darkblue <-"#0f3e5a"
AMZ_purple<- "#b54295"
AMZ_red<- "#ee1822"
AMZ_lightblue2<-"#31a5dd"



AMZcolors=function(nvar) {
     
        if(nvar==1)  {AMZ=c(AMZ_orange)}
        else if (nvar==2) {AMZ=c(AMZ_maroon,AMZ_orange)}
        else if (nvar==3) {AMZ=c(AMZ_maroon,AMZ_mustard,AMZ_green)}
        else if (nvar==4) {AMZ=c(AMZ_maroon,AMZ_green, AMZ_mustard,AMZ_orange)} 
        else if (nvar==5) {AMZ=c(AMZ_maroon,AMZ_green, AMZ_mustard, AMZ_orange, AMZ_violet)}
        else if (nvar==6) {AMZ=c(AMZ_maroon,AMZ_green, AMZ_mustard, AMZ_orange, AMZ_violet,AMZ_lightblue)}
        else if (nvar==7) {AMZ=c(AMZ_maroon,AMZ_green, AMZ_mustard, AMZ_orange, AMZ_violet, AMZ_lightblue,AMZ_pistachio)}
        else if (nvar==8) {AMZ=c(AMZ_maroon,AMZ_green, AMZ_mustard, AMZ_orange, AMZ_violet,  AMZ_lightblue,AMZ_pistachio, AMZ_beige )}
        else if (nvar==9) {AMZ=c(AMZ_maroon,AMZ_green, AMZ_mustard, AMZ_orange, AMZ_violet,  AMZ_lightblue,AMZ_pistachio, AMZ_beige,AMZ_darkblue)}
return(AMZ)
}
AMZcolors(7)


brewercolors=function(nvar) {
     
     if(nvar==1)  {brew=c("#018571")}
     else if (nvar==2) {brew=c("#dfc27d","#018571")}
     else if (nvar==3) {brew=c("#a6611a","#80cdc1","#018571")}
     else if (nvar==4) {brew=c("#a6611a","#dfc27d","#80cdc1","#018571" )} 
     else if (nvar==5) {brew=c("#a6611a","#dfc27d","#f6e8c3","#80cdc1","#018571" )}
     else if (nvar==6) {brew=c("#8c510a","#d8b365","#f6e8c3","#c7eae5","#5ab4ac","#01665e" )}
     else if (nvar==7) {brew=c("#8c510a","#d8b365","#f6e8c3","#ffffbf","#c7eae5","#5ab4ac","#01665e")}
     else if (nvar==8) {brew=c("#8c510a","#bf812d","#dfc27d","#f6e8c3","#c7eae5","#80cdc1","#35978f","#01665e")}
     return(brew)
}




# Segunda version ---------------------------------------------------------

turquesa <- "#5F978E"
violet <- "#5B4689"
magenta <- "#A14063"
magenta2 <-"#9D2D5D"
naranja <- "#CB8128"
erc <- "#DBBD5D"
podemos <- "#83366C"
rojo_psc <- "#C83C3B"
AMZ_blue <- "#104577"
AMZ_maroon <- "#ab2323"
AMZ_lightblue <- "#7cb5d2"
AMZ_violet <- "#bc94b7"
AMZ_green <- "#18a68c"
AMZ_pistachio <- "#bcd74a"
AMZ_mustard <- "#fde05e"
AMZ_lightpink <-"#f8e0c8"
AMZ_darkblue <-"#0f3e5a"
AMZ_brickred<- "#ec6645"
AMZ_orange<-"#f7a519"
AMZ_purple<- "#b54295"
AMZ_red<- "#ee1822"
AMZ_blue2<-"#106ab4"
AMZ_lightblue2<-"#31a5dd"

AMZcolors=c(magenta, magenta2,AMZ_maroon,rojo_psc, AMZ_red,AMZ_brickred,
            AMZ_orange, erc,AMZ_mustard, AMZ_pistachio, AMZ_green,
            turquesa,AMZ_lightblue, AMZ_lightblue2, AMZ_blue2,AMZ_blue, AMZ_darkblue,
            violet,podemos, AMZ_purple,AMZ_violet ,
             AMZ_lightpink)


# n=seq(1:16)
# barplot(n, col=rev(colors16))


ndata<- 5 # number of datasets to represent
colors<-sample(AMZcolors, ndata, replace=T) # subsample to get just ndata colors



