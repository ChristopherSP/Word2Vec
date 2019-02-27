library(data.table)
library(stringi)
library(tm)
library(lsa)
library(doParallel)
library(parallel)
library(Rcpp)

stopword = stopwords("portuguese")
myLetters = c(letters,"ç","á","é","í","ó","ú","à","ü")

WordToVec = function(df,col = "", preProcess = c("scale","pca"), train = T){
  
  preProcess = match.arg(preProcess)
  
  dt = copy(df)
  m = ncol(dt)
  
  eval(parse(text = paste0("dt[,cleantitulo := gsub(' ','_',stri_trim_both(gsub('[[:punct:]]','',tolower(",col,"))))]")))
  
  countLetters = lapply(dt$cleantitulo, function(name){
    word = strsplit(name, "_")[[1]]
    word = word[!word %in% stopword]
    chars = tolower(strsplit(paste(word,collapse = ' '), "")[[1]])
    letter <- chars[chars %in% myLetters]
    tab = table(letter)
    dt = as.data.table(t(c(as.vector(tab), length(word))))
    names(dt) = c(names(tab),"ntokens")
    dt
  })
  
  countLetters = rbindlist(countLetters,fill = T)
  missingCols = myLetters[!(myLetters %in% names(countLetters))]
 
  if(length(missingCols)!=0){
    dtMissing = as.data.table(matrix(0, nrow = nrow(dt), ncol = length(missingCols)))
    names(dtMissing) = missingCols
    countLetters = cbind(countLetters,dtMissing)
  }
  
  setcolorder(countLetters, c("ntokens",myLetters))
  
  countLetters[is.na(countLetters)] = 0
  countLetters[, lettersPerToken := Reduce(`+`, .SD)/ntokens]
  countLetters[is.nan(lettersPerToken), lettersPerToken := 0]
  
  switch (preProcess,
    scaled = {
      countLetters[,names(countLetters[,which(colSums(.SD) ==0)]) := NULL]
      if(train == T){
        scaled = scale(countLetters)
        scaledMean <<- attr(scaled,which = "scaled:center",T)
        scaledSD <<- attr(scaled,which = "scaled:scale",T)
        countLetters = as.data.table(scaled)
      }else{
        (countLetters - scaledMean)/scaledSD
      }
    },
    pca = {
      if(train == T){
        countLetters[,names(countLetters[,which(colSums(.SD) ==0)]) := NULL]
        pca <<- prcomp(countLetters,center = T,scale. = T)
        sumpca = summary(pca)
        cumProp = sumpca$importance[which(rownames(sumpca$importance) == "Cumulative Proportion"),]
        dimension <<- which(cumProp>=0.9)[1]
        countLetters = as.data.table(pca$x)[,.SD, .SDcols = 1:dimension]
      }else{
        countLetters = as.data.table(predict(pca,countLetters))[,.SD, .SDcols = 1:dimension]
      }
    },{
      countLetters
    }
  )
  dtWord2Vec = cbind(dt,countLetters)
  gc()
  return(dtWord2Vec)
}

cboWordToVec = WordToVec(cbo2002,"titulos","scale",T)
encWordToVec = WordToVec(data,"cargofuncao","scale",F)

columns =  if(any(startsWith(names(cboWordToVec),"PC"))){
  names(cboWordToVec)[startsWith(names(cboWordToVec),"PC")]
}else{
  c("ntokens", myLetters, "lettersPerToken")
}

cboWordToVecList = split(cboWordToVec[,.SD,.SDcols = c("cbo_2002","titulos",columns)], by=c("cbo_2002","titulos"))
cboWordToVecList = lapply(cboWordToVecList, function(dt){
  dt[,titulos := NULL]
  dt[,cbo_2002 := NULL]
  as.numeric(dt)
})

encWordToVecList = split(encWordToVec[,.SD,.SDcols = c("cargofuncao",columns)], seq(1,nrow(encWordToVec)))
encWordToVecList = lapply(encWordToVecList, function(dt){
  dt[,cargofuncao := NULL]
  as.numeric(dt)
})

sourceCpp("~/Downloads/MyCos.cpp")

similarityMatrix = applyCos(encWordToVecList,cboWordToVecList)
similarityMatrix = as.data.table(similarityMatrix)
names(similarityMatrix) = c("matchIdx","similarity")

similarityMatrix$matchLabel = names(cboWordToVecList[ifelse(similarityMatrix$matchIdx<1,1,similarityMatrix$matchIdx)])
encWordToVec = cbind(encWordToVec,similarityMatrix)
encWordToVec[matchIdx == -2, matchLabel := ""]
View(head(unique(encWordToVec[!cargofuncao%in%c("TERCEIRO",""),.(cargofuncao,similarity,matchLabel)], by = "cargofuncao"),5000)[order(-similarity)])
