setwd("~/Documents/GitHub/Caso-2/Calidad de Hospitales - data")
tabla<-(read.csv(file="outcome-of-care-measures.csv"))

mejor <- function(estado, resultado) {
    if (!(estado %in% unique(tabla[,7]))) {stop("Favor de introducir un Estado valido")}
    if ( !resultado=="Heart attack"){
        if (!resultado=="Heart failure"){
            if (!resultado=="Pneumonia") 
            {stop("Esa enfermedad es invalida")}}}
    
    if (resultado == "Heart attack"){col<-11}; if (resultado=="Heart failure"){col<-17}
    if (resultado== "Pneumonia"){col<-23} 
    
    enfermedad<- as.vector(tabla[,col]); estados<- as.vector(tabla[,7]); hospital<- as.vector(tabla[,2])
    suppressWarnings(class(enfermedad)<- "numeric")
 
    tablageneral<- data.frame(hospital,estado,enfermedad)
    colnames(tablageneral)<- c("Nombre","Estado", "Resultado")
    final<-subset.data.frame(tablageneral,tablageneral[,"Estado"]==estado)
    final<-subset.data.frame(final,is.na(final[,"Resultado"])==FALSE )
    #print(final)
    final[order(final$Resultado),]
    #print(finalfinal[,"Resultado"])
    minimo<- min(final[,"Resultado"]);#print(minimo)
    numero<- length(final[,"Resultado"]);#print(numero)
    x<-which(final[,"Resultado"] == minimo)
    y<-length(final[x,]);#print(y)
    x<-(final[x,])
    # print(x)
    if (y>1){x[order(x$Nombre),]}
    else if(y==1){final[x,]}
}

