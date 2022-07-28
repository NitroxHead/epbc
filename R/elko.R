elko = function(df,spoint,ftop){

  options(warn=0)
  sa=df
  rownames(sa) <- sa$Name
  sa$Name=NULL

  loabp = t(colnames(sa[,2:spoint]))
  lospp = t(colnames(sa[(spoint+1):length(sa)]))

  loabpff = knitr::combine_words(loabp, sep ="+",and = "")
  finalmodel=list()
  for(h in 1:length(lospp)){
    x = lospp[h]

    allModelsList <- paste(x,"~+1" )

    model.full = glm(allModelsList, data = sa, family = negative.binomial(theta=1))

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
      #print("şşşşşşş")
      }
    }else{
      formdayiz = paste(x,"~+1")
      #print("ĞĞĞĞĞĞ")
    }
    #reconstract the formula


    finalmodel[[h]] = glm(as.formula(formdayiz), data = sa, family = negative.binomial(theta=1))
  }
  finalmodel[lengths(finalmodel) == 0] = NULL

  return(finalmodel)
}

