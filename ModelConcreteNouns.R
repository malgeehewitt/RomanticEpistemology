countNouns<-function(parsed.file, dict){
  parsed.table<-read.csv(parsed.file)
  raw.nouns<-table(parsed.table$token[which(parsed.table$xpos %in% c("NN", "NNS", "NNP", "NNPS"))])
  raw.nouns<-raw.nouns[which(names(raw.nouns) %in% dict)]
  return(raw.nouns)
}

gatherNouns<-function(parsed.folder, dict){
  all.files<-list.files(parsed.folder, full.names=T)
  all.nouns<-lapply(all.files, function(x) countNouns(x, dict))
  all.nouns<-unlist(all.nouns)
  all.nouns<-tapply(all.nouns, names(all.nouns), "sum")
  return(all.nouns)
}

#count words in a corpus to compare to noun counts above
countWords<-function(text.filename, dict){
  raw.text<-scan(text.filename, what='character', sep="\n", quiet=T)
  raw.text<-paste(raw.text, collapse=" ")
  raw.text<-unlist(strsplit(raw.text, ""))
  raw.text<-tolower(raw.text)
  raw.text<-raw.text[which(raw.text %in% c(letters, LETTERS, " "))]
  raw.text<-paste(raw.text, collapse="")
  raw.text<-unlist(strsplit(raw.text, " "))
  raw.text<-raw.text[which(raw.text %in% dict)]
  text.counts<-table(raw.text)
  remove(raw.text)
  gc()
  return(text.counts)
}

corpusWordcount<-function(corpus.source, dict){
  library(parallel)
  all.files<-list.files(corpus.source, full.names = T)
  #n.core<-detectCores()-2
  #proc.clust<-makeCluster(n.core, type="FORK")
  all.counts<-lapply(all.files, function(x) countWords(x, dict))
  #stopCluster(proc.clust)
  all.counts<-unlist(all.counts)
  all.counts<-tapply(all.counts, names(all.counts), "sum")
  return(all.counts)
}


modelNouns<-function(concrete.words, abstract.words, embedding){
  Groups<-c(rep("concrete", length(concrete.words)), rep("abstract", length(abstract.words)))
  concrete.sample<-embedding[which(rownames(embedding) %in% concrete.words),]
  abstract.sample<-embedding[which(rownames(embedding) %in% abstract.words),]
  training.data<-rbind(concrete.sample, abstract.sample)
  Groups<-as.factor(Groups)
  training.data<-data.frame(training.data, Groups)
  return(training.data)
}


#########################################
#code to derive information on concrete nouns from parsed poetry corpus

#what I need to get:
#1. distribution of % nouns per 1% segment of poetry
#2. most frequent root verb of concrete nouns
#3. dependency position of concrete nouns
#4. "clumping" statistic?
#5. total abstract, concrete, words

cleanPoem<-function(raw.table){
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

textStats<-function(raw.filename, concrete.terms, abstract.terms){
  text.table<-read.csv(raw.filename)
  raw.poem.words<-text.table$token
  raw.poem.words<-tolower(raw.poem.words)
  poem.words<-raw.poem.words[-which(raw.poem.words %in% c(".", ",", ":", ";", "'", '"', "(", ")", "!", "?", "-"))]
  total.words<-length(poem.words)
  raw.noun.index<-which(text.table$xpos %in% c("NN", "NNS", "NNP", "NNPS"))
  raw.poem.nouns<-raw.poem.words[raw.noun.index]
  culled.poem.pos<-text.table$xpos[-which(raw.poem.words %in% c(".", ",", ":", ";", "'", '"', "(", ")", "!", "?", "-"))]
  culled.noun.index<-which(culled.poem.pos %in% c("NN", "NNS", "NNP", "NNPS"))
  poem.nouns<-poem.words[culled.noun.index]
  poem.concrete.nouns<-table(poem.nouns[which(poem.nouns %in% concrete.terms)])
  poem.abstract.nouns<-table(poem.nouns[which(poem.nouns %in% abstract.terms)])
  total.concrete<-sum(poem.concrete.nouns)
  total.abstract<-sum(poem.abstract.nouns)
  if(total.words>100){
    poem.distribution.words<-makeBins(poem.words, 50)
    poem.distribution.pos<-makeBins(culled.poem.pos, 50)
    distribution.noun.index<-lapply(poem.distribution.pos, function(x) which(x %in% c("NN", "NNS", "NNP", "NNPS")))
    poem.distribution<-mapply(function(x,y) x[y], poem.distribution.words, distribution.noun.index, SIMPLIFY=F)
    concrete.distribution<-unlist(lapply(poem.distribution, function(x) length(which(x %in% concrete.terms))))
    abstract.distribution<-unlist(lapply(poem.distribution, function(x) length(which(x %in% abstract.terms))))
    di.concrete<-var(concrete.distribution)/mean(concrete.distribution)
    di.abstract<-var(abstract.distribution)/mean(abstract.distribution)
  } else {
    concrete.distribution<-NA
    abstract.distribution<-NA
    di.concrete<-NA
    di.abstract<-NA
  }
  if(total.words>=500){
    poem.size.distribution.words<-makeSlidingSizeBins(poem.words, 250)
    poem.size.distribution.pos<-makeSlidingSizeBins(culled.poem.pos, 250)
    size.distribution.noun.index<-lapply(poem.size.distribution.pos, function(x) which(x %in% c("NN", "NNS", "NNP", "NNPS")))
    poem.size.distribution<-mapply(function(x,y) x[y], poem.size.distribution.words, size.distribution.noun.index, SIMPLIFY=F)
    concrete.size.distribution<-unlist(lapply(poem.size.distribution, function(x) length(which(x %in% concrete.terms))))
    abstract.size.distribution<-unlist(lapply(poem.size.distribution, function(x) length(which(x %in% abstract.terms))))
    di.size.concrete<-var(concrete.size.distribution)/mean(concrete.size.distribution)
    di.size.abstract<-var(abstract.size.distribution)/mean(abstract.size.distribution)
  } else {
    di.size.concrete<-NA
    di.size.abstract<-NA
  }
  sentence.tokens<-paste(as.character(text.table$sentence_id), as.character(text.table$token_id), sep="_")
  concrete.hits<-which(raw.poem.words %in% concrete.terms)
  concrete.hits<-concrete.hits[which(concrete.hits %in% raw.noun.index)]
  if(length(concrete.hits)>0){
    concrete.deps<-table(text.table$dep_rel[concrete.hits])
    concrete.heads<-paste(as.character(text.table$sentence_id[concrete.hits]), as.character(text.table$head_token[concrete.hits]), sep="_")
    concrete.head.pos<-text.table$upos[which(sentence.tokens %in% concrete.heads)]
    concrete.heads<-text.table$lemma[which(sentence.tokens %in% concrete.heads)]
    concrete.heads<-paste(concrete.heads, concrete.head.pos, sep="_")
    concrete.heads<-table(concrete.heads)
  } else {
    concrete.deps<-NA
    concrete.heads<-NA
  }
  abstract.hits<-which(raw.poem.words %in% abstract.terms)
  abstract.hits<-abstract.hits[which(abstract.hits %in% raw.noun.index)]
  if(length(abstract.hits)>0){
    abstract.deps<-table(text.table$dep_rel[abstract.hits])
    abstract.heads<-paste(as.character(text.table$sentence_id[abstract.hits]), as.character(text.table$head_token[abstract.hits]), sep="_")
    abstract.head.pos<-text.table$upos[which(sentence.tokens %in% abstract.heads)]
    abstract.heads<-tolower(text.table$lemma[which(sentence.tokens %in% abstract.heads)])
    abstract.heads<-paste(abstract.heads, abstract.head.pos, sep="_")
    abstract.heads<-table(abstract.heads)
  } else {
    abstract.deps<-NA
    abstract.heads<-NA
  }
  freq.concrete<-total.concrete/total.words
  freq.abstract<-total.abstract/total.words
  base.stats<-c(total.concrete, total.abstract, freq.concrete, freq.abstract, total.words, di.concrete, di.abstract, di.size.concrete, di.size.abstract)
  names(base.stats)<-c("Total_Concrete", "Total_Abstract", "Freq_Concrete", "Freq_Abstract", "Total_Words", "DI_Perc_Concrete", "DI_Perc_Abstract", "DI_Size_Concrete", "DI_Size_Abstract")
  text.list<-list(concrete.distribution, abstract.distribution, poem.concrete.nouns, concrete.deps, concrete.heads, poem.abstract.nouns, abstract.deps, abstract.heads, base.stats)
  return(text.list)
}


#output name should be the period as well as any path - ie. "results/Early Eighteenth-Century")
poemCorpusStats<-function(sub.meta.table, parsed.folder, concrete.terms, abstract.terms, output.name){
  ptm<-proc.time()
  print(output.name)
  all.filenames<-sub.meta.table$ParsedFilename
  all.filenames<-paste(parsed.folder, all.filenames, sep="/")
  all.stats<-lapply(all.filenames, function(x) textStats(x, concrete.terms, abstract.terms))
  base.stats<-lapply(all.stats, function(x) x[[9]])
  base.stats<-do.call("rbind", base.stats)
  total.concrete<-sum(base.stats[,1])
  total.abstract<-sum(base.stats[,2])
  concrete.distributions<-lapply(all.stats, function(x) x[[1]])
  concrete.distributions<-do.call("rbind", concrete.distributions)
  write.csv(concrete.distributions, file=paste(output.name, "ConcreteDistributions.csv", sep="_"), row.names=F)
  abstract.distributions<-lapply(all.stats, function(x) x[[2]])
  abstract.distributions<-do.call("rbind", abstract.distributions)
  write.csv(abstract.distributions, file=paste(output.name, "AbstractDistributions.csv", sep="_"), row.names=F)
  concrete.terms<-unlist(lapply(all.stats, function(x) x[[3]]))
  concrete.terms<-tapply(concrete.terms, names(concrete.terms), "sum")
  concrete.term.freq<-concrete.terms/total.concrete
  concrete.term.table<-data.frame(names(concrete.terms), concrete.terms, concrete.term.freq)
  colnames(concrete.term.table)<-c("Term", "RawCount", "Freq")
  concrete.term.table<-concrete.term.table[order(concrete.term.table$Freq, decreasing=T),]
  write.csv(concrete.term.table, file=paste(output.name, "ConcreteTerms.csv", sep="_"), row.names=F)
  concrete.deps<-unlist(lapply(all.stats, function(x) x[[4]]))
  concrete.deps<-tapply(concrete.deps, names(concrete.deps), "sum")
  concrete.dep.freq<-concrete.deps/total.concrete
  concrete.dep.table<-data.frame(names(concrete.deps), concrete.deps, concrete.dep.freq)
  colnames(concrete.dep.table)<-c("Term", "RawCount", "Freq")
  concrete.dep.table<-concrete.dep.table[order(concrete.dep.table$Freq, decreasing=T),]
  write.csv(concrete.dep.table, file=paste(output.name, "ConcreteDependencies.csv", sep="_"), row.names=F)
  concrete.heads<-unlist(lapply(all.stats, function(x) x[[5]]))
  concrete.heads<-tapply(concrete.heads, names(concrete.heads), "sum")
  concrete.head.freq<-concrete.heads/total.concrete
  concrete.head.table<-data.frame(names(concrete.heads), concrete.heads, concrete.head.freq)
  colnames(concrete.head.table)<-c("Term", "RawCount", "Freq")
  concrete.head.table<-concrete.head.table[order(concrete.head.table$Freq, decreasing=T),]
  write.csv(concrete.head.table, file=paste(output.name, "ConcreteHeadTokens.csv", sep="_"), row.names=F)
  abstract.terms<-unlist(lapply(all.stats, function(x) x[[6]]))
  abstract.terms<-tapply(abstract.terms, names(abstract.terms), "sum")
  abstract.term.freq<-abstract.terms/total.abstract
  abstract.term.table<-data.frame(names(abstract.terms), abstract.terms, abstract.term.freq)
  colnames(abstract.term.table)<-c("Term", "RawCount", "Freq")
  abstract.term.table<-abstract.term.table[order(abstract.term.table$Freq, decreasing=T),]
  write.csv(abstract.term.table, file=paste(output.name, "AbstractTerms.csv", sep="_"), row.names=F)
  abstract.deps<-unlist(lapply(all.stats, function(x) x[[7]]))
  abstract.deps<-tapply(abstract.deps, names(abstract.deps), "sum")
  abstract.dep.freq<-abstract.deps/total.abstract
  abstract.dep.table<-data.frame(names(abstract.deps), abstract.deps, abstract.dep.freq)
  colnames(abstract.dep.table)<-c("Term", "RawCount", "Freq")
  abstract.dep.table<-abstract.dep.table[order(abstract.dep.table$Freq, decreasing=T),]
  write.csv(abstract.dep.table, file=paste(output.name, "AbstractDependencies.csv", sep="_"), row.names=F)
  abstract.heads<-unlist(lapply(all.stats, function(x) x[[8]]))
  abstract.heads<-tapply(abstract.heads, names(abstract.heads), "sum")
  abstract.head.freq<-abstract.heads/total.abstract
  abstract.head.table<-data.frame(names(abstract.heads), abstract.heads, abstract.head.freq)
  colnames(abstract.head.table)<-c("Term", "RawCount", "Freq")
  abstract.head.table<-abstract.head.table[order(abstract.head.table$Freq, decreasing=T),]
  write.csv(abstract.head.table, file=paste(output.name, "AbstractHeadtokens.csv", sep="_"), row.names=F)
  enhanced.meta<-data.frame(sub.meta.table, base.stats, stringsAsFactors=F)
  write.csv(enhanced.meta, file=paste(output.name, "MetaTablewithStats.csv", sep="_"), row.names=F)
  print(proc.time()-ptm)
  return(enhanced.meta)
}

allPeriodStats<-function(full.meta.table, parsed.folder, concrete.terms, abstract.terms, output.folder){
  ptm<-proc.time()
  library(dplyr)
  periods<-unique(full.meta.table$meta_period)
  sub.tables<-lapply(periods, function(x) full.meta.table[which(full.meta.table$meta_period==x),])
  output.names<-paste(output.folder, periods, sep="/")
  all.periods.results<-mapply(function(x,y) poemCorpusStats(x, parsed.folder, concrete.terms, abstract.terms, y), sub.tables, output.names, SIMPLIFY = F)
  full.enhanced.meta<-bind_rows(all.periods.results)
  write.csv(full.enhanced.meta, file=paste(output.folder, "FullMetadataAndStats.csv", sep="/"), row.names=F)
  print(proc.time()-ptm)
  return(full.enhanced.meta)
}

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
  dist.matrix<-dist.matrix[-bad.dists,]
  total.words<-total.words[-bad.dists]
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
  concrete.files<-distribution.files[grep("Concrete", distribution.files)]
  abstract.files<-distribution.files[grep("Abstract", distribution.files)]
  early.concrete.dist<-read.csv(concrete.files[grep("Early Eighteenth", concrete.files)])
  mid.concrete.dist<-read.csv(concrete.files[grep("Later Eighteenth", concrete.files)])
  late.concrete.dist<-read.csv(concrete.files[grep("Early Nineteenth", concrete.files)])
  early.abstract.dist<-read.csv(abstract.files[grep("Early Eighteenth", abstract.files)])
  mid.abstract.dist<-read.csv(abstract.files[grep("Later Eighteenth", abstract.files)])
  late.abstract.dist<-read.csv(abstract.files[grep("Early Nineteenth", abstract.files)])
  early.concrete.freq<-summarizeDistribution(early.concrete.dist, full.stats, unique.periods[3])
  mid.concrete.freq<-summarizeDistribution(mid.concrete.dist, full.stats, unique.periods[1])
  late.concrete.freq<-summarizeDistribution(late.concrete.dist, full.stats, unique.periods[2])
  early.abstract.freq<-summarizeDistribution(early.abstract.dist, full.stats, unique.periods[3])
  mid.abstract.freq<-summarizeDistribution(mid.abstract.dist, full.stats, unique.periods[1])
  late.abstract.freq<-summarizeDistribution(late.abstract.dist, full.stats, unique.periods[2])
  bin.seq<-seq(1, 50, by=1)
  early.concrete.table<-data.frame(bin.seq, early.concrete.freq, rep(unique.periods[3], 50))
  mid.concrete.table<-data.frame(bin.seq, mid.concrete.freq, rep(unique.periods[1], 50))
  late.concrete.table<-data.frame(bin.seq, late.concrete.freq, rep(unique.periods[2], 50))
  early.abstract.table<-data.frame(bin.seq, early.abstract.freq, rep(unique.periods[3], 50))
  mid.abstract.table<-data.frame(bin.seq, mid.abstract.freq, rep(unique.periods[1], 50))
  late.abstract.table<-data.frame(bin.seq, late.abstract.freq, rep(unique.periods[2], 50))
  colnames(early.abstract.table)<-c("Bin", "NounFreq", "Period")
  colnames(mid.abstract.table)<-c("Bin", "NounFreq", "Period")
  colnames(late.abstract.table)<-c("Bin", "NounFreq", "Period")
  colnames(early.concrete.table)<-c("Bin", "NounFreq", "Period")
  colnames(mid.concrete.table)<-c("Bin", "NounFreq", "Period")
  colnames(late.concrete.table)<-c("Bin", "NounFreq", "Period")
  abstract.table<-bind_rows(early.abstract.table, mid.abstract.table, late.abstract.table)
  abstract.table$NounType<-rep("Abstract", nrow(abstract.table))
  concrete.table<-bind_rows(early.concrete.table, mid.concrete.table, late.concrete.table)
  concrete.table$NounType<-rep("Concrete", nrow(concrete.table))
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


###############################
#functions to summarize freq, head token, and dependency tables

checkTable<-function(raw.table, cull){
  freq.values<-raw.table$Freq
  bad.cells<-which(is.na(freq.values))
  if(length(bad.cells)>0){
    raw.table<-raw.table[-bad.cells,]
  }
  if(cull){
    all.terms<-raw.table$Term
    term.index<-grep("_VERB", all.terms)
    raw.table<-raw.table[term.index,]
  }
  return(raw.table)
}

fillMissingValues<-function(raw.table, all.terms, col.identifier){
  require(dplyr)
  missing.terms<-all.terms[which(!all.terms %in% raw.table$Term)]
  if(length(missing.terms)>0){
    add.rows<-data.frame(missing.terms, rep(0, length(missing.terms)), rep(0, length(missing.terms)))
    colnames(add.rows)<-colnames(raw.table)
    raw.table<-bind_rows(raw.table, add.rows)
  }
  rank<-seq(1,nrow(raw.table), by=1)
  og.column.names<-colnames(raw.table)
  raw.table<-data.frame(rank, raw.table)
  colnames(raw.table)<-c("Rank", og.column.names)
  og.column.names<-colnames(raw.table)
  new.cols<-paste(og.column.names, col.identifier, sep="_")
  colnames(raw.table)<-new.cols
  return(raw.table)
}

getWordStats<-function(composite.table, term){
  early.index<-which(composite.table[,2]==term)
  mid.index<-which(composite.table[,6]==term)
  late.index<-which(composite.table[,10]==term)
  ranks<-c(composite.table[early.index,1], composite.table[mid.index,5],composite.table[late.index,9])
  max.rank<-min(ranks)
  rank.diff<-(abs(ranks[2]-ranks[1])+abs(ranks[3]-ranks[2]))
  freqs<-c(composite.table[early.index,4], composite.table[mid.index,8], composite.table[early.index,12])
  freq.diff<-(abs(freqs[2]-freqs[1])+abs(freqs[3]-freqs[2]))
  adj.rank.diff<-rank.diff/max.rank
  adj.freq.diff<-freq.diff/max.rank
  term.stats<-c(ranks, rank.diff, freq.diff, adj.rank.diff, adj.freq.diff)
  return(term.stats)
}

compositeTable<-function(table.list, folder.name, table.type){
  verb<-F
  if(table.type %in% c("ConcreteHeadtokens", "AbstractHeadtokens")){
    verb<-T
  }
  table.list<-lapply(table.list, function(x) checkTable(x, cull=verb))
  all.terms<-unlist(lapply(table.list, function(x) x$Term))
  unique.terms<-unique(all.terms)
  periods<-c("Early18th", "Late18th", "Early19th")
  fixed.tables<-mapply(function(x,y) fillMissingValues(x, unique.terms, y), table.list, periods, SIMPLIFY = F)
  composite.table<-bind_cols(fixed.tables)
  all.table.stats<-lapply(unique.terms, function(x) getWordStats(composite.table, x))
  all.table.stats<-do.call("rbind", all.table.stats)
  all.table.stats<-data.frame(unique.terms, all.table.stats)
  colnames(all.table.stats)<-c("Term", "Early18Rank", "Late18Rank", "Early19Rank", "RankChange", "FreqChange", "RankChangeAdj", "FreqChangeAdj")
  direction<-all.table.stats$Early19Rank-all.table.stats$Early18Rank
  ChangeDirection<-rep("Decreasing", length(direction))
  ChangeDirection[which(direction<0)]<-"Increasing"
  if(length(which(direction==0))>0){
    ChangeDirection[which(direction==0)]<-"Stable"
  }
  all.table.stats$Direction<-ChangeDirection
  all.table.stats<-all.table.stats[order(all.table.stats$RankChangeAdj, decreasing=T),]
  filename.composite<-paste("Composite", table.type, ".csv", sep="")
  filename.composite<-paste(folder.name, filename.composite, sep="/")
  write.csv(composite.table, filename.composite, row.names=F)
  filename.stats<-paste(table.type, "Stats.csv", sep="")
  filename.stats<-paste(folder.name, filename.stats, sep="/")
  write.csv(all.table.stats, filename.stats, row.names=F)
}

summaryTables<-function(folder.name){
  require(dplyr)
  abstract.dependencies<-list.files(folder.name, full.name=T, pattern="_AbstractDependencies.csv")
  abstract.dependencies<-abstract.dependencies[order(c(1,3,2))]
  abstract.dependencies<-lapply(abstract.dependencies, function(x) read.csv(x))
  compositeTable(abstract.dependencies, folder.name, "AbstractDependencies")
  concrete.dependencies<-list.files(folder.name, full.name=T, pattern="_ConcreteDependencies.csv")
  concrete.dependencies<-concrete.dependencies[order(c(1,3,2))]
  concrete.dependencies<-lapply(concrete.dependencies, function(x) read.csv(x))
  compositeTable(concrete.dependencies, folder.name, "ConcreteDependencies")
  abstract.Headtokens<-list.files(folder.name, full.name=T, pattern="_AbstractHeadtokens.csv")
  abstract.Headtokens<-abstract.Headtokens[order(c(1,3,2))]
  abstract.Headtokens<-lapply(abstract.Headtokens, function(x) read.csv(x))
  compositeTable(abstract.Headtokens, folder.name, "AbstractHeadtokens")
  concrete.Headtokens<-list.files(folder.name, full.name=T, pattern="_ConcreteHeadTokens.csv")
  concrete.Headtokens<-concrete.Headtokens[order(c(1,3,2))]
  concrete.Headtokens<-lapply(concrete.Headtokens, function(x) read.csv(x))
  compositeTable(concrete.Headtokens, folder.name, "ConcreteHeadtokens")
  abstract.Terms<-list.files(folder.name, full.name=T, pattern="_AbstractTerms.csv")
  abstract.Terms<-abstract.Terms[order(c(1,3,2))]
  abstract.Terms<-lapply(abstract.Terms, function(x) read.csv(x))
  compositeTable(abstract.Terms, folder.name, "AbstractTerms")
  concrete.Terms<-list.files(folder.name, full.name=T, pattern="_ConcreteTerms.csv")
  concrete.Terms<-concrete.Terms[order(c(1,3,2))]
  concrete.Terms<-lapply(concrete.Terms, function(x) read.csv(x))
  compositeTable(concrete.Terms, folder.name, "ConcreteTerms")
}






