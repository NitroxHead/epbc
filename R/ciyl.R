ciyl = function(df,fhof,shof,spoint,ftop){

  options(warn=0)
  sa=df
  rownames(sa) <- sa$Name
  sa$Name=NULL
  
  loabp = t(colnames(sa[,2:spoint]))
  lospp = t(colnames(sa[(spoint+1):length(sa)]))

  loabpff = knitr::combine_words(loabp, sep ="+",and = "")
  finalmodel2=list()

  for(h in 1:length(lospp)){
    x = lospp[h]

    allModelsList <- paste(x,"~+1" )

    first.model = paste("model.full =",fhof, x,"~+1,data=sa",shof)

    eval(parse(text=first.model))

    lmAIC<-step(model.full, scope = as.character(paste("~",loabpff)))

    storebaba2=drop1(lmAIC,test = "Chisq")
    storebaba=storebaba2[5]
    storebaba = na.omit(storebaba)
    storebaba6 = t(storebaba)
    storebaba=as.numeric(storebaba6)
    names(storebaba) = colnames(storebaba6)
    if(length(storebaba)>1){

      albaba=NULL
      sayac = 1
      for(g in 1:length(storebaba)){
        deneme = storebaba[g]
        if(deneme<ftop){

          albaba[sayac] = g
          sayac = sayac +1
        }
      }
      if(length(albaba)>0){
        pfor = "~"
        for(h in 1:length(albaba)){
          pfor= paste(names(storebaba[albaba]))
        }
        naform= paste(pfor, collapse = '+')
        formdayiz = paste(x,"~",naform)}
      else{formdayiz = paste(x,"~+1")# if a variable doesnt have high accurate variable it gives +1 instead
      
      }
    }else{
      formdayiz = paste(x,"~+1")
      
    }
    #reconstract the formula

    ourformula = paste("finalmodel2[[",h,"]]=",fhof,"as.formula(formdayiz),data=sa",shof)
    eval(parse(text=ourformula))

  }
  finalmodel2[lengths(finalmodel2) == 0] = NULL

  return(finalmodel2)
}

