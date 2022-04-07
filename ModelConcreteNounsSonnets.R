convertSonnet<-function(og.filename, output.folder=NULL){
  library(XML)
  raw.text<-scan(og.filename, what='character', sep="\n", quiet=T)
  raw.text<-paste(raw.text, collapse="\n")
  xml.text<-paste('<?xml version="1.0" encoding="UTF-8" ?>', raw.text, sep="\n")
  xml.ingest<-htmlTreeParse(xml.text, useInternalNodes=T)
  sonnet.lines<-xpathSApply(xml.ingest, "//*/div[@type='line']", xmlValue)
  epigraph<-xpathSApply(xml.ingest, "//*/div[@type='epigraph']/div[@type='line']", xmlValue)
  if(length(epigraph)>0){
    sonnet.lines<-sonnet.lines[-which(sonnet.lines %in% epigraph)]
  }
  sonnet.lines<-gsub("&indent;", "", sonnet.lines)
  sonnet.lines<-gsub("&amp;indent;", "", sonnet.lines)
  author.fn<-xpathSApply(xml.ingest,"//*/fname", xmlValue)
  author.ln<-xpathSApply(xml.ingest, "//*/lname", xmlValue)
  author.name<-paste(author.fn[1], author.ln[1], sep=" ")
  title.main<-xpathSApply(xml.ingest, "//*/title/main", xmlValue)
  title.sub<-xpathSApply(xml.ingest, "//*/title/sub", xmlValue)
  title.edition<-xpathSApply(xml.ingest, "//*/title/edition", xmlValue)
  if(length(title.sub)>0){
    title<-paste(title.main, ": ",title.sub, "\nFrom: ", title.edition, sep="")
  } else {
    title<-paste(title.main, "\nFrom: ", title.edition, sep="")
  }
  
  title<-gsub("&wblank;", "", title)
  #return(title)
  if(is.null(output.folder)){
    return(sonnet.lines)
  } else {
    sonnet.lines<-paste(sonnet.lines, collapse="\n")
    #sonnet.lines<-paste(title, author.name, "\n", sonnet.lines, sep="\n")
    base.fn<-unlist(strsplit(og.filename, ".tml"))
    base.fn<-unlist(strsplit(base.fn, "/"))
    base.fn<-base.fn[length(base.fn)]
    new.fn<-paste(output.folder, "/", base.fn, ".txt", sep="")
    write(sonnet.lines, new.fn)
    #return(sonnet.lines)
  }
}

testWords<-function(raw.filename){
  sonnet.lines<-convertSonnet(raw.filename)
  print(sonnet.lines)
  sonnet.lines<-unlist(lapply(sonnet.lines, function(x) gsub("'", " ", x)))
  sonnet.lines<-unlist(lapply(sonnet.lines, function(x) gsub("—", " ", x)))
  sonnet.lines<-unlist(lapply(sonnet.lines, function(x) gsub("  ", " ", x)))
  sonnet.lines<-unlist(lapply(sonnet.lines, function(x) gsub("&sblank;", "", x)))
  for(i in 1:length(sonnet.lines)){
    curr.line<-unlist(strsplit(sonnet.lines[i], " "))
    bad.lines<-which(curr.line=="")
    if(length(bad.lines)>0){
      curr.line<-curr.line[-bad.lines]
    }
    curr.line<-paste(curr.line, collapse=" ")
    sonnet.lines[i]<-curr.line
  }
  sonnet.words<-unlist(lapply(sonnet.lines, function(x) unlist(strsplit(x, " "))))
  return(sonnet.words)
}


parseSonnet<-function(raw.filename, output.folder){
  sonnet.lines<-convertSonnet(raw.filename)
  sonnet.lines<-paste(sonnet.lines, collapse=" | ")
  sonnet.parse<-udpipe(sonnet.lines, "english")
  line.endings<-which(sonnet.parse$token=="|")
  line.starts<-c(1, line.endings+1)
  line.endings<-c(line.endings, nrow(sonnet.parse))
  line.lengths<-(line.endings-line.starts)+1
  line.assignments<-unlist(mapply(function(x,y) rep(x,y), seq(1,length(line.lengths), by=1), line.lengths))
  sonnet.parse$Line<-line.assignments
  #sonnet.lines<-unlist(lapply(sonnet.lines, function(x) gsub("'", " ", x)))
  #sonnet.lines<-unlist(lapply(sonnet.lines, function(x) gsub("  ", " ", x)))
  #sonnet.lines<-unlist(lapply(sonnet.lines, function(x) gsub("—", " ", x)))
  #sonnet.lines<-unlist(lapply(sonnet.lines, function(x) gsub("&sblank;", "", x)))
  #for(i in 1:length(sonnet.lines)){
  #  curr.line<-unlist(strsplit(sonnet.lines[i], " "))
  #  bad.lines<-which(curr.line=="")
  #  if(length(bad.lines)>0){
  #    curr.line<-curr.line[-bad.lines]
  #  }
  #  curr.line<-paste(curr.line, collapse=" ")
  #  sonnet.lines[i]<-curr.line
  #}
  #line.lengths<-unlist(lapply(sonnet.lines, function(x) length(unlist(strsplit(x, " ")))))
  #sonnet.collapse<-paste(sonnet.lines, collapse=" ")
  #sonnet.parse<-udpipe(sonnet.collapse, "english")
  #sonnet.parse<-sonnet.parse[-which(sonnet.parse$token %in% c(".", ",", ":", ";", "'", '"', "(", ")", "!", "?", "-", "", " ", ":—", "—")),]
  #line.assignments<-unlist(mapply(function(x,y) rep(x,y), seq(1, length(line.lengths), by=1), line.lengths))
  #if(length(line.assignments)!=nrow(sonnet.parse)){
  #  all.parse<-lapply(sonnet.lines, function(x) udpipe(x, "english"))
  #  line.assign<-mapply(function(x,y) rep(x,nrow(y)), seq(1, length(all.parse), by=1), all.parse)
  #  all.parse<-do.call('rbind', all.parse)
  #  all.parse$Line<-unlist(line.assign)
  #  all.parse<-all.parse[-which(all.parse$token %in% c(".", ",", ":", ";", "'", '"', "(", ")", "!", "?", "-", "", " ", ":—", "—")),]
  #  if(nrow(all.parse)==nrow(sonnet.parse)){
  #    line.assignments<-all.parse$Line
  #  } else {
  #    line.assignments<-all.parse$Line
  #    if(line.assignments>nrow(sonnet.parse)){
  #      line.assignments<-line.assignments[1:nrow(sonnet.parse)]
  #    } else {
  #      line.assignments<-c(line.assignments, rep(line.assignments[length(line.assignments)], nrow(sonnet.parse)-length(line.assignments)))
  #    }
  #  }
  #}
  #sonnet.parse$Line<-line.assignments
  output.filename<-unlist(strsplit(raw.filename, "/"))
  output.filename<-paste(output.folder, output.filename[length(output.filename)], sep="/")
  output.filename<-gsub(".tml", ".csv", output.filename)
  write.csv(sonnet.parse, output.filename, row.names=F)
}



#************************************************
#code copied from the entire corpus version but changed to be run on sonnets
#note, the code above to parse the poems cleans the punctuation out of the text before saving it, so cleaning (except tolower, has been removed)

cleanSonnet<-function(raw.table){
  indent.lines<-grep("&", raw.table$token)
  if(length(indent.lines)>0){
    indent.plus<-raw.table$token[indent.lines+1]
    bad.lines<-which(indent.plus==";")
    remove.lines<-indent.lines[bad.lines]
    remove.lines<-c(remove.lines, remove.lines+1)
    raw.table<-raw.table[-remove.lines,]
  }
  return(raw.table)
}

makeBins<-function(clean.text, num.bins=100){
  text.length<-length(clean.text)
  if(text.length>=num.bins){
    bin.size<-text.length%/%num.bins
    bin.lengths<-rep(bin.size, num.bins)
    bin.remainder<-text.length-sum(bin.lengths)
    #selected.bins<-sample(num.bins, bin.remainder)
    if(bin.remainder>0){
      selected.bins<-seq(1, bin.remainder, by=1)
      bin.lengths[selected.bins]<-bin.size+1
    }
    bin.starts<-1
    for(i in 2:length(bin.lengths)){
      bin.starts<-c(bin.starts, bin.starts[i-1]+bin.lengths[i-1])
    }
    bin.ends<-bin.starts+(bin.lengths-1)
    binned.text<-mapply(function(x,y) clean.text[x:y], bin.starts, bin.ends, SIMPLIFY=F)
    return(binned.text)
  } else {
    return(NA)
  }
}

discreteSonnetBins<-function(clean.table){
  unique.lines<-unique(clean.table$Line)
  binned.tokens<-lapply(unique.lines, function(x) clean.table$token[which(clean.table$Line==x)])
  binned.pos<-lapply(unique.lines, function(x) clean.table$xpos[which(clean.table$Line==x)])
  binned.list<-list(binned.tokens, binned.pos)
  return(binned.list)
}



makeSlidingSizeBins<-function(raw.text, bin.size=100){
  bin.advance<-bin.size%/%2
  bin.starts<-seq(1, length(raw.text), by=bin.size)
  bad.bins<-which(bin.starts>=length(raw.text))
  if(length(bad.bins)>0){
    bin.starts<-bin.starts[-bad.bins]
  }
  bin.ends<-bin.starts+bin.size
  bad.bins<-which(bin.ends>length(raw.text))
  if(length(bad.bins)>0){
    bin.ends[bad.bins]<-length(raw.text)
  }
  bin.lengths<-bin.ends-bin.starts
  bad.bins<-which(bin.lengths<bin.size)
  if(length(bad.bins)>0){
    bin.starts<-bin.starts[-bad.bins]
    bin.ends<-bin.ends[-bad.bins]
  }
  binned.text<-mapply(function(x,y) raw.text[x:y], bin.starts, bin.ends, SIMPLIFY=T)
  return(binned.text)
}

textStats<-function(raw.filename, object.terms, concept.terms){
  text.table<-read.csv(raw.filename)
  noun.index<-which(text.table$xpos %in% c("NN", "NNS", "NNP", "NNPS"))
  text.table$token<-tolower(text.table$token)
  total.words<-nrow(text.table)
  poem.nouns<-text.table$token[noun.index]
  poem.object.nouns<-table(poem.nouns[which(poem.nouns %in% object.terms)])
  poem.objects<-poem.object.nouns
  poem.concept.nouns<-table(poem.nouns[which(poem.nouns %in% concept.terms)])
  poem.concepts<-poem.concept.nouns
  total.objects<-sum(poem.objects)
  total.concepts<-sum(poem.concepts)
  raw.poem.words<-text.table$token
  raw.noun.index<-which(text.table$xpos %in% c("NN", "NNS", "NNP", "NNPS"))
  punct.hits<-which(text.table$token %in% c(".", ",", ":", ";", "'", '"', "(", ")", "!", "?", "-"))
  if(length(punct.hits)>0){
    poem.words<-raw.poem.words[-which(raw.poem.words %in% c(".", ",", ":", ";", "'", '"', "(", ")", "!", "?", "-"))]
    culled.poem.pos<-text.table$xpos[-which(raw.poem.words %in% c(".", ",", ":", ";", "'", '"', "(", ")", "!", "?", "-"))]
    culled.noun.index<-which(culled.poem.pos %in% c("NN", "NNS", "NNP", "NNPS"))
    clean.table<-text.table[-which(text.table$token %in% c(".", ",", ":", ";", "'", '"', "(", ")", "!", "?", "-")),]
  } else {
    poem.owrds<-raw.poem.words
    culled.poem.pos<-text.table$xpos
    culled.noun.index<-which(culled.poem.pos %in% c("NN", "NNS", "NNP", "NNPS"))
    clean.table<-text.table
  }
  raw.poem.nouns<-raw.poem.words[raw.noun.index]
  #print(dim(clean.table))
  poem.distribution.list<-discreteSonnetBins(clean.table)
  poem.distribution.words<-poem.distribution.list[[1]]
  poem.distribution.pos<-poem.distribution.list[[2]]
  distribution.noun.index<-lapply(poem.distribution.pos, function(x) which(x %in% c("NN", "NNS", "NNP", "NNPS")))
  poem.distribution<-mapply(function(x,y) x[y], poem.distribution.words, distribution.noun.index, SIMPLIFY=F)
  object.distribution<-unlist(lapply(poem.distribution, function(x) length(which(x %in% object.terms))))
  concept.distribution<-unlist(lapply(poem.distribution, function(x) length(which(x %in% concept.terms))))
  #print(object.distribution)
  #print(concept.distribution)
  di.object<-var(object.distribution)/mean(object.distribution)
  di.concept<-var(concept.distribution)/mean(concept.distribution)

  sentence.tokens<-paste(as.character(text.table$sentence_id), as.character(text.table$token_id), sep="_")
  object.hits<-which(raw.poem.words %in% object.terms)
  object.hits<-object.hits[which(object.hits %in% raw.noun.index)]
  if(length(object.hits)>0){
    object.deps<-table(text.table$dep_rel[object.hits])
    object.heads<-paste(as.character(text.table$sentence_id[object.hits]), as.character(text.table$head_token[object.hits]), sep="_")
    object.head.pos<-text.table$upos[which(sentence.tokens %in% object.heads)]
    object.heads<-text.table$lemma[which(sentence.tokens %in% object.heads)]
    object.heads<-paste(object.heads, object.head.pos, sep="_")
    object.heads<-table(object.heads)
  } else {
    object.deps<-NA
    object.heads<-NA
  }
  concept.hits<-which(raw.poem.words %in% concept.terms)
  concept.hits<-concept.hits[which(concept.hits %in% raw.noun.index)]
  if(length(concept.hits)>0){
    concept.deps<-table(text.table$dep_rel[concept.hits])
    concept.heads<-paste(as.character(text.table$sentence_id[concept.hits]), as.character(text.table$head_token[concept.hits]), sep="_")
    concept.head.pos<-text.table$upos[which(sentence.tokens %in% concept.heads)]
    concept.heads<-tolower(text.table$lemma[which(sentence.tokens %in% concept.heads)])
    concept.heads<-paste(concept.heads, concept.head.pos, sep="_")
    concept.heads<-table(concept.heads)
  } else {
    concept.deps<-NA
    concept.heads<-NA
  }
  freq.objects<-total.objects/total.words
  freq.concepts<-total.concepts/total.words
  base.stats<-c(total.objects, total.concepts, freq.objects, freq.concepts, total.words, di.object, di.concept)
  names(base.stats)<-c("Total_Objects", "Total_Concepts", "Freq_Objects", "Freq_concepts", "Total_Words", "DI_Objects", "DI_Concepts")
  text.list<-list(object.distribution, concept.distribution, poem.object.nouns, object.deps, object.heads, poem.concept.nouns, concept.deps, concept.heads, base.stats)
  return(text.list)
}


#output name should be the period as well as any path - ie. "results/Early Eighteenth-Century")
poemCorpusStats<-function(sub.meta.table, parsed.folder, object.terms, concept.terms, output.name){
  ptm<-proc.time()
  print(output.name)
  print(nrow(sub.meta.table))
  all.filenames<-sub.meta.table$ParsedFilename
  all.filenames<-paste(parsed.folder, all.filenames, sep="/")
  all.stats<-lapply(all.filenames, function(x) textStats(x, object.terms, concept.terms))
  base.stats<-lapply(all.stats, function(x) x[[9]])
  base.stats<-do.call("rbind", base.stats)
  total.object<-sum(base.stats[,1])
  total.concept<-sum(base.stats[,2])
  object.distributions<-lapply(all.stats, function(x) x[[1]])
  object.distributions<-do.call("rbind", object.distributions)
  write.csv(object.distributions, file=paste(output.name, "ObjectDistributions.csv", sep="_"), row.names=F)
  concept.distributions<-lapply(all.stats, function(x) x[[2]])
  concept.distributions<-do.call("rbind", concept.distributions)
  write.csv(concept.distributions, file=paste(output.name, "ConceptDistributions.csv", sep="_"), row.names=F)
  object.terms<-unlist(lapply(all.stats, function(x) x[[3]]))
  object.terms<-tapply(object.terms, names(object.terms), "sum")
  object.term.freq<-object.terms/total.object
  object.term.table<-data.frame(names(object.terms), object.terms, object.term.freq)
  colnames(object.term.table)<-c("Term", "RawCount", "Freq")
  object.term.table<-object.term.table[order(object.term.table$Freq, decreasing=T),]
  write.csv(object.term.table, file=paste(output.name, "ObjectTerms.csv", sep="_"), row.names=F)
  object.deps<-unlist(lapply(all.stats, function(x) x[[4]]))
  object.deps<-tapply(object.deps, names(object.deps), "sum")
  object.dep.freq<-object.deps/total.object
  object.dep.table<-data.frame(names(object.deps), object.deps, object.dep.freq)
  colnames(object.dep.table)<-c("Term", "RawCount", "Freq")
  objectdep.table<-object.dep.table[order(object.dep.table$Freq, decreasing=T),]
  write.csv(object.dep.table, file=paste(output.name, "ConcreteDependencies.csv", sep="_"), row.names=F)
  object.heads<-unlist(lapply(all.stats, function(x) x[[5]]))
  object.heads<-tapply(object.heads, names(object.heads), "sum")
  object.head.freq<-object.heads/total.object
  object.head.table<-data.frame(names(object.heads), object.heads, object.head.freq)
  colnames(object.head.table)<-c("Term", "RawCount", "Freq")
  object.head.table<-object.head.table[order(object.head.table$Freq, decreasing=T),]
  write.csv(object.head.table, file=paste(output.name, "ObjectHeadTokens.csv", sep="_"), row.names=F)
  concept.terms<-unlist(lapply(all.stats, function(x) x[[6]]))
  concept.terms<-tapply(concept.terms, names(concept.terms), "sum")
  concept.term.freq<-concept.terms/total.concept
  concept.term.table<-data.frame(names(concept.terms), concept.terms, concept.term.freq)
  colnames(concept.term.table)<-c("Term", "RawCount", "Freq")
  concept.term.table<-concept.term.table[order(concept.term.table$Freq, decreasing=T),]
  write.csv(concept.term.table, file=paste(output.name, "ConceptTerms.csv", sep="_"), row.names=F)
  concept.deps<-unlist(lapply(all.stats, function(x) x[[7]]))
  concept.deps<-tapply(concept.deps, names(concept.deps), "sum")
  concept.dep.freq<-concept.deps/total.concept
  concept.dep.table<-data.frame(names(concept.deps), concept.deps, concept.dep.freq)
  colnames(concept.dep.table)<-c("Term", "RawCount", "Freq")
  concept.dep.table<-concept.dep.table[order(concept.dep.table$Freq, decreasing=T),]
  write.csv(concept.dep.table, file=paste(output.name, "ConceptDependencies.csv", sep="_"), row.names=F)
  concept.heads<-unlist(lapply(all.stats, function(x) x[[8]]))
  concept.heads<-tapply(concept.heads, names(concept.heads), "sum")
  concept.head.freq<-concept.heads/total.concept
  concept.head.table<-data.frame(names(concept.heads), concept.heads, concept.head.freq)
  colnames(concept.head.table)<-c("Term", "RawCount", "Freq")
  concept.head.table<-concept.head.table[order(concept.head.table$Freq, decreasing=T),]
  write.csv(concept.head.table, file=paste(output.name, "ConceptHeadtokens.csv", sep="_"), row.names=F)
  enhanced.meta<-data.frame(sub.meta.table, base.stats, stringsAsFactors=F)
  write.csv(enhanced.meta, file=paste(output.name, "MetaTablewithStats.csv", sep="_"), row.names=F)
  print(proc.time()-ptm)
  jitter.index<-sample(10000, nrow(enhanced.meta))
  jitter.index<-jitter.index/10000
  enhanced.meta$Jitter<-jitter.index
  return(enhanced.meta)
}

allPeriodStats<-function(full.meta.table, parsed.folder, object.terms, concept.terms, output.folder){
  ptm<-proc.time()
  library(dplyr)
  periods<-unique(full.meta.table$meta_period)
  sub.tables<-lapply(periods, function(x) full.meta.table[which(full.meta.table$meta_period==x),])
  output.names<-paste(output.folder, periods, sep="/")
  all.periods.results<-mapply(function(x,y) poemCorpusStats(x, parsed.folder, object.terms, concept.terms, y), sub.tables, output.names, SIMPLIFY = F)
  full.enhanced.meta<-bind_rows(all.periods.results)
  write.csv(full.enhanced.meta, file=paste(output.folder, "FullMetadataAndStats.csv", sep="/"), row.names=F)
  print(proc.time()-ptm)
  return(full.enhanced.meta)
}
#******************
###########
#Questionable Code for guessing dates of poetry by dispersing them throughout the author's life
cleanDate<-function(char.date){
  char.date<-unlist(char.date)
  clean.date<-unlist(strsplit(char.date, ""))
  clean.date<-suppressWarnings(clean.date[which(!is.na(as.numeric(clean.date)))])
  clean.date<-paste(clean.date, collapse="")
  clean.date<-as.numeric(clean.date)
  return(clean.date)
}

assignPoemDateAuthor<-function(author.name, all.author.names, meta.table){
  sub.table<-meta.table[which(all.author.names %in% author.name),]
  num.poems<-nrow(sub.table)
  if(nrow(sub.table)>1){
    dob<-sub.table$author_dob[1]
    dod<-sub.table$author_dod[1]
  } else {
    dob<-sub.table[2]
    dod<-sub.table[4]
  }
  dob<-cleanDate(dob)
  dod<-cleanDate(dod)
  if(!is.na(dod)){
    end.date<-dod
    if(!is.na(dob)){
      start.date<-dob+15
      if(start.date>dod){
        start.date<-dod-30
      }
    } else {
      start.date<-(death-30)
    }
  } else {
    if(!is.na(dob)){
      start.date<-dob+15
      end.date<-dob+50
    } else {
      start.date<-1000
      end.date<-1050
    }
  }
  poem.seq<-seq(start.date, end.date, by=1)
  poem.dates<-sample(poem.seq, nrow(sub.table), replace=T)
  return(poem.dates)
}

makeDatesCorpus<-function(meta.table){
  author.bound<-paste(meta.table$author_fname, meta.table$author_lname, meta.table$author_dod, sep="_")
  author.bound.unique<-unique(author.bound)
  author.indicies<-lapply(author.bound.unique, function(x) which(author.bound==x))
  author.date.seq<-lapply(author.bound.unique, function(x) assignPoemDateAuthor(x, author.bound, meta.table))
  poem.dates<-rep(0, nrow(meta.table))
  for(i in 1:length(author.bound.unique)){
    poem.dates[author.indicies[[i]]]<-author.date.seq[[i]]
  }
  return(poem.dates)
}

######################
#code to sumarize distributions
summarizeDistribution<-function(dist.matrix, meta.table, period.name){
  total.words<-meta.table$Total_Words[which(meta.table$meta_period %in% period.name)]
  bad.dists<-which(is.na(rowSums(dist.matrix)))
  if(length(bad.dists)>0){
    dist.matrix<-dist.matrix[-bad.dists,]
    total.words<-total.words[-bad.dists]
  }
  total.dist<-colSums(dist.matrix)
  total.words<-sum(total.words)
  bin.size<-total.words%/%length(total.dist)
  bin.lengths<-rep(bin.size, length(total.dist))
  bin.remainder<-total.words-sum(bin.lengths)
  if(bin.remainder>0){
    selected.bins<-seq(1, bin.remainder, by=1)
    bin.lengths[selected.bins]<-bin.size+1
  }
  bin.freq<-total.dist/bin.lengths
  return(bin.freq)
}

#####################3
#Functions for creating stacked tables with additional stats, and summarized distributions
#(automates what I keep doing manually)
stackStats<-function(og.stat.table, folder.name){
  require(dplyr)
  calculated.dates<-makeDatesCorpus(og.stat.table)
  base.stats<-og.stat.table[,c(1,2,4,6,7,8,9,10,14,18,19,20,21,26)]
  base.stats<-bind_rows(base.stats, base.stats)
  base.stats$CalculatedDates<-c(calculated.dates, calculated.dates)
  NounCount<-c(og.stat.table$Total_Concrete, og.stat.table$Total_Abstract)
  NounFreq<-c(og.stat.table$Freq_Concrete, og.stat.table$Freq_Abstract)
  DI_Perc<-c(og.stat.table$DI_Perc_Concrete, og.stat.table$DI_Perc_Abstract)
  DI_Size<-c(og.stat.table$DI_Size_Concrete, og.stat.table$DI_Size_Abstract)
  add.table<-data.frame(NounCount, NounFreq, DI_Perc, DI_Size)
  add.table[is.na(add.table)]<--1
  add.table$NounType<-c(rep("Object", nrow(og.stat.table)), rep("Concept", nrow(og.stat.table)))
  Jitter<-sample(100000, nrow(add.table), replace=T)
  add.table$Jitter<-Jitter
  DI_Diff<-og.stat.table$DI_Size_Concrete-og.stat.table$DI_Size_Abstract
  DI_Ratio<-og.stat.table$DI_Size_Concrete/og.stat.table$DI_Size_Abstract
  add.table$DI_Diff<-c(DI_Diff, DI_Diff)
  add.table$DI_Ratio<-c(DI_Ratio, DI_Ratio)
  final.table<-data.frame(base.stats, add.table)
  write.csv(final.table, file=paste(folder.name, "StackedStatisticsTable.csv", sep="/"), row.names=F)
  return(final.table)
}

collapseDistributions<-function(folder.name, og.stat.table, vocab.name){
  unique.periods<-unique(og.stat.table$meta_period)
  folder.files<-list.files(folder.name, full.names=T)
  distribution.files<-folder.files[grep("Distributions.csv", folder.files)]
  concrete.files<-distribution.files[grep("Object", distribution.files)]
  abstract.files<-distribution.files[grep("Concept", distribution.files)]
  early.concrete.dist<-read.csv(concrete.files[grep("Early Eighteenth", concrete.files)])
  mid.concrete.dist<-read.csv(concrete.files[grep("Later Eighteenth", concrete.files)])
  late.concrete.dist<-read.csv(concrete.files[grep("Early Nineteenth", concrete.files)])
  early.abstract.dist<-read.csv(abstract.files[grep("Early Eighteenth", abstract.files)])
  mid.abstract.dist<-read.csv(abstract.files[grep("Later Eighteenth", abstract.files)])
  late.abstract.dist<-read.csv(abstract.files[grep("Early Nineteenth", abstract.files)])
  early.concrete.freq<-summarizeDistribution(early.concrete.dist, og.stat.table, unique.periods[3])
  mid.concrete.freq<-summarizeDistribution(mid.concrete.dist, og.stat.table, unique.periods[1])
  late.concrete.freq<-summarizeDistribution(late.concrete.dist, og.stat.table, unique.periods[2])
  early.abstract.freq<-summarizeDistribution(early.abstract.dist, og.stat.table, unique.periods[3])
  mid.abstract.freq<-summarizeDistribution(mid.abstract.dist, og.stat.table, unique.periods[1])
  late.abstract.freq<-summarizeDistribution(late.abstract.dist, og.stat.table, unique.periods[2])
  bin.seq<-seq(1, 14, by=1)
  early.concrete.table<-data.frame(bin.seq, early.concrete.freq, rep(unique.periods[3], 14))
  mid.concrete.table<-data.frame(bin.seq, mid.concrete.freq, rep(unique.periods[1], 14))
  late.concrete.table<-data.frame(bin.seq, late.concrete.freq, rep(unique.periods[2], 14))
  early.abstract.table<-data.frame(bin.seq, early.abstract.freq, rep(unique.periods[3], 14))
  mid.abstract.table<-data.frame(bin.seq, mid.abstract.freq, rep(unique.periods[1], 14))
  late.abstract.table<-data.frame(bin.seq, late.abstract.freq, rep(unique.periods[2], 14))
  colnames(early.abstract.table)<-c("Bin", "NounFreq", "Period")
  colnames(mid.abstract.table)<-c("Bin", "NounFreq", "Period")
  colnames(late.abstract.table)<-c("Bin", "NounFreq", "Period")
  colnames(early.concrete.table)<-c("Bin", "NounFreq", "Period")
  colnames(mid.concrete.table)<-c("Bin", "NounFreq", "Period")
  colnames(late.concrete.table)<-c("Bin", "NounFreq", "Period")
  abstract.table<-bind_rows(early.abstract.table, mid.abstract.table, late.abstract.table)
  abstract.table$NounType<-rep("Concept", nrow(abstract.table))
  concrete.table<-bind_rows(early.concrete.table, mid.concrete.table, late.concrete.table)
  concrete.table$NounType<-rep("Object", nrow(concrete.table))
  full.dist.table<-bind_rows(concrete.table, abstract.table)
  full.dist.table$Vocab<-rep(vocab.name, nrow(full.dist.table))
  write.csv(full.dist.table, file=paste(folder.name, "DistributionFrequencies_AllTypes.csv", sep="/"), row.names=F)
  return(full.dist.table)
}

##############################3
#functions to smooth above table

smoothDist<-function(dist.values, smooth.type="rolling.average", smoothing.factor=3){
  if(smooth.type=="rolling.average"){
    center.pos<-seq(1, length(dist.values), by=1)
    smooth.values<-lapply(center.pos, function(x) seq((x-smoothing.factor), (x+smoothing.factor), by=1))
    for(i in 1:length(smooth.values)){
      current.smooth<-smooth.values[[i]]
      bad.starts<-which(current.smooth<1)
      if(length(bad.starts)>0){
        current.smooth<-current.smooth[-bad.starts]
        smooth.values[[i]]<-current.smooth
      }
      bad.ends<-which(current.smooth>length(dist.values))
      if(length(bad.ends)>0){
        current.smooth<-current.smooth[-bad.ends]
        smooth.values[[i]]<-current.smooth
      }
    }
    smooth.nums<-lapply(smooth.values, function(x) dist.values[x])
    smooth.nums<-unlist(lapply(smooth.nums, function(x) mean(x)))
    return(smooth.nums)
  }
}

smoothStackedTable<-function(stacked.table, smoothing.factor=3){
  for(i in 1:length(smoothing.factor)){
    curr.smoothing.factor<-smoothing.factor[i]
    pasted.ids<-paste(stacked.table$Period, stacked.table$NounType, stacked.table$Vocab, sep="_")
    unique.ids<-unique(pasted.ids)
    dists<-lapply(unique.ids, function(x) stacked.table$NounFreq[which(pasted.ids==x)])
    dists<-unlist(lapply(dists, function(x) smoothDist(x, smoothing.factor=curr.smoothing.factor)))
    new.col.name<-paste("NounFreqSmoothed", as.character(curr.smoothing.factor), sep="_")
    new.cols.names<-c(colnames(stacked.table), new.col.name)
    stacked.table<-data.frame(stacked.table, dists, stringsAsFactors=F)
    colnames(stacked.table)<-new.cols.names
  }
  return(stacked.table)
}
