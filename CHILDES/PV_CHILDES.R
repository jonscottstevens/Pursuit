#Implementation of Trueswell et al. hypothesis testing model


library(plyr)


################
#preliminaries

#load data

#source("Training.r")
#source("Testing.r")


#initialize data structures

lexicon<-list()
words<-c()
objects<-c()
observed_words<-c()


#construct an empty matrix for storing probability scores

for(n in 1:length(uttered)){for(w in uttered[[n]]){if(is.element(w,words)==F){words<-c(words,w)}}}
for(n in 1:length(visible)){for(o in visible[[n]]){if(is.element(o,objects)==F){objects<-c(objects,o)}}}
objects<-setdiff(objects,"NULL")
values<-matrix(nrow=length(words),ncol=length(objects))
rownames(values)<-words
colnames(values)<-objects


# construct an empty vector for storing best hypotheses

hyps<-matrix(nrow=length(words),ncol=1)
rownames(hyps)<-words
hyps[,]<-"NULL"


################
#parameter values

alphazero<-0.26
alpha<-0.71
values[,]<-alphazero


################
#reward functions

#calculate the binary reward term for a mapping for a given instance

rewarded<-FALSE

reward<-function(w,o,x){
	if(is.element(o,visible[[x]])){
        rewarded<<-TRUE #<--tell the 'analyze' function that a reward has occurred
        values[w,o]<<-alpha
    }
	else{
        rewarded<<-FALSE #<--tell the 'analyze' function that no reward has occurred
    }
}


################
#function for introducing novel hypotheses

introduce<-function(x){
	for(w in uttered[[x]]){
		if(is.element(w,observed_words)==F){
			observed_words<<-c(observed_words,w) #<--log which words have been seen
		}
	}
}


################
#the word learning algorithm

#function for choosing a hypothesis

choose_meaning<-function(w){
	remember<-FALSE
	if(hyps[w,]!="NULL"){remember<-sample(c(TRUE,FALSE),1,prob=c(values[w,hyps[w,]],1-values[w,hyps[w,]]))}
	if(remember){return(hyps[w,])}
	else{return("NULL")}
}


#introduce new hypotheses, select a hypothesis, adjust probabilities

analyze<-function(x){
	introduce(x)
    for(w in uttered[[x]]){
        rewarded<<-FALSE
        choice<-choose_meaning(w)
        reward(w,choice,x)
        if(rewarded==FALSE){hyps[w,]<<-sample(visible[[x]],1)}
	}
}


#for each new situation, build a discrete lexicon based on a threshold function

build<-function(){
    lexicon<<-list()
	for(w in observed_words){
       	hypothesis<-as.vector(choose_meaning(w))
        if(hypothesis!="NULL"){
        	lexicon[[length(lexicon)+1]]<<-c(w,hypothesis)
		}
	}
	print(lexicon)
	print("****************")
}


################
#process the data incrementally, displaying all lexicons through time

simulate<-function(){
	for(n in 1:length(uttered)){
        analyze(n)
        print(n)
	}
	build()
    lexeval()
}


################
#evaluation functions

#the words of the lexicon

w_mapped<-function(){
	domain<-c()
	for(n in 1:length(lexicon)){
		domain<-c(domain,lexicon[[n]][1])
	}
	return(domain)
}


#the objects of the lexicon

o_mapped<-function(){
	domain<-c()
	for(n in 1:length(lexicon)){
		domain<-c(domain,lexicon[[n]][2])
	}
	return(domain)
}


#what a given word maps to in the lexicon

hypothesis<-function(w){
	indices<-which(w_mapped()==w)
    if(length(indices)>1){index<-sample(indices,1)}
    else{index<-indices}
	return(lexicon[[index]][2])
}


#functions to evaluate and aggregate

meta_evaluate<-function(lex){
	correct = 0
	if(length(lex)>0){
		for (n in 1:length(lex)){
			if(hypothesis(lex[[n]][1])==gold(lex[[n]][1])){correct = correct + 1}
		}
		return(c(correct/(length(lex)),correct/goldlength))
	}
	else{
		return(c(0,0))
	}
}


precisions<-c()
recalls<-c()
meta_aggregate<-function(num){
	precisions<<-c()
	recalls<<-c()
	for(n in 1:num){
		values[,]<<-alphazero
		observed_words<<-c()
		lexicon<<-list()
        hyps[,]<<-"NULL"
		for(m in 1:length(uttered)){
			analyze(m)
			print(c(n,m))
		}
        build()
		eval<-meta_evaluate(lexicon)
		precisions<<-c(precisions,eval[1])
		recalls<<-c(recalls,eval[2])
	}
	avp<-mean(precisions)
	avr<-mean(recalls)
	fscore<-2/((1/avp)+(1/avr))
	return(c(avp,avr,fscore))
}


#evaluate the current lexicon

lexeval<-function(){
	correct = 0
	if(length(lexicon)>0){
		for (n in 1:length(lexicon)){
			if(hypothesis(lexicon[[n]][1])==gold(lexicon[[n]][1])){correct = correct + 1}
		}
		precision<-correct/(length(lexicon))
		recall<-correct/goldlength
		fscore<-fscore<-2/((1/precision)+(1/recall))
		return(c(precision,recall,fscore))
	}
	else{
		return(c(0,0,0))
	}
}