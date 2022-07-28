croyer = function(df,fhof,shof,spoint,ftop,vout,vtype,vtest){
  library(splitstackshape)
  options(warn=0)
  sa=df
  rownames(sa) <- sa$Name
  sa$Name=NULL
  
  loabp = t(colnames(sa[,1:spoint]))
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
  # for(i in 1:length(finalmodel2)){
  #   
  #   which(finalmodel2[[x]][["formula"]][[3]] = c("+1","+ 1"))
  # }
  if(vout > 0){
    
    theoutput = list()
    
    allModelsAIC=lapply(1:length(finalmodel2), function(x) AIC(finalmodel2[[x]])) 
    UsedVariablesFactor = lapply(1:length(finalmodel2), function(x) finalmodel2[[x]][["formula"]][[3]])
    UsedVariablesFactor = data.frame(rbind(UsedVariablesFactor))
    newspp = array()
    for(x in 1:length(finalmodel2)){newspp[x] = as.character(finalmodel2[[x]][["formula"]][[2]])}
    
    UsedVariablesFactor=apply(UsedVariablesFactor,2,as.character)
    
    trlistdf = data.frame(UsedVariablesFactor)
    trlistdfparsed =cSplit(trlistdf,1:ncol(trlistdf),sep = "+",stripWhite = T,type.convert = F)
    rownames(trlistdfparsed) = newspp
    
    dddi = dim(trlistdfparsed)
    hnnat=1#how many non numeric at the table
    CU = matrix(0,dddi[1],length(loabp)+1) #creating dataframe for counting
    colnames(CU) = append(loabp)#creating dataframe for counting
    CU = data.frame(CU)#creating dataframe for counting
    rownames(CU)= rownames(trlistdfparsed)#creating dataframe for counting
    
    for(i in 1:length(lospp)){
      if(isTRUE(as.logical(grep("1",trlistdfparsed[i,2])))){CU[i,1:length(loabp)]=CU[i,1:length(loabp)]+0#check if it is 1 and if it is adding 0 to each column of that row
      } else{
        for(y in 1:length(trlistdfparsed[i,])){
          
          if(!is.na(trlistdfparsed[i,y,with=FALSE])==TRUE){TMPV=trlistdfparsed[i,y,with=FALSE]
          qq = drop1(finalmodel2[[i]],test = vtest)
          if(qq[y+1,5]>0){
            if(vtype > 0){
            CU[i,as.character(TMPV)]=CU[i,as.character(TMPV)]+qq[y+1,5]
            }else{CU[i,as.character(TMPV)]=CU[i,as.character(TMPV)]+1}
          }
          }
        }
      }
    }
    
    CU = CU[apply(CU[,-1], 1, function(x) !all(x==0)),]
    theoutput[[1]] = finalmodel2
    theoutput[[2]] = CU
    return(theoutput)
    
  
  
  }else{
    return(finalmodel2)
  }
}

