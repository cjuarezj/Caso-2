setwd("~/Documents/GitHub/Caso-2/Calidad de Hospitales - data")
tabla<-data.fram(read.csv(file="outcome-of-care-measures.csv"))

rankhospital<- function(estado,resultado,num){
    if (!(estado %in% unique(tabla[,7]))) {stop("Favor de introducir un Estado valido")}
    if ( !resultado=="Heart attack"){
        if (!resultado=="Heart failure"){
            if (!resultado=="Pneumonia") 
            {stop("Esa enfermedad es invalida")}}}
    
    if (resultado == "Heart attack"){resultado<-11}; if (resultado=="Heart failure"){resultado<-17}; if (resultado== "Pneumonia"){resultado<-23} 
    
    resultados<- as.vector(tabla[,resultado])
    estados<- as.vector(tabla[,7])
    hospital<- as.vector(tabla[,2])
    suppressWarnings(class(resultados)<- "numeric")
    
    tablageneral<- data.frame(hospital,estados,resultados)
    colnames(tablageneral)<- c("Nombres","Estado", "Resultado")
    final<-subset.data.frame(tablageneral,tablageneral[,"Estado"]==estado)
    final<-subset.data.frame(final,is.na(final[,"Resultado"])==FALSE )
    final<-final[order(final$Resultado),]
    
    x<-length(final[,"Resultado"]); Ranking<- c(1:x);
    final<- data.frame(final,Ranking)
    
    minimo<- min(final[,"Resultado"])
    maximo<- max(final[,"Resultado"])
    total<- x
    
    if (!is.numeric(num)==FALSE && num> total) {
        print(N1A)
    } else if (!is.character(num)==FALSE && num== "mejor") {
        print(final[1,])
    }else if (!is.character(num)==FALSE && num== "peor"){
        print(final[x,])
    }else if (!is.numeric(num)==FALSE && num< total){
        final<-final[1:num,]
        final<-final[order(final$Nombre),]; final[,"Ranking"]= c(1:num);
        final<-final[order(final$Resultado),]; final[,"Ranking"]= c(1:num); print(final[num,])
    }
}
rankhospital("MD","Heart attack",num ="peor")

