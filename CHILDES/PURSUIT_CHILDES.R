#A REINFORCEMENT-BASED WORD LEARNING ALGORITHM
#CHILDES VERSION

library(plyr)


################
#preliminaries

#load data (also loads "gold" evaluation function and "goldlength" global variable)
#be sure to set your R working directory to the folder which contains the data files!

#source("Training.r")
#source("Testing.r")


#initialize data structures

lexicon<-list()
words<-c()
objects<-c()
observed_words<-c()
observed_objects<-c()


#construct an empty matrix for storing probability scores

for(n in 1:length(uttered)){for(w in uttered[[n]]){if(is.element(w,words)==F){words<-c(words,w)}}}
for(n in 1:length(visible)){for(o in visible[[n]]){if(is.element(o,objects)==F){objects<-c(objects,o)}}}
values<-matrix(nrow=length(words),ncol=length(objects))
rownames(values)<-words
colnames(values)<-objects
values[,]<-0


################
#parameter values

gamma<-0.02
tau<-0.79
lambda<-0.001


################
#reward functions

#return the reward amount (0 or 1) for a mapping for a given instance

rewarded<-FALSE

reward<-function(w,o,x){
	if(is.element(o,visible[[x]])){ #<-hypothesis confirmed?
        rewarded<<-TRUE
        return(1)
    }
	else{ #<-hypothesis refuted?
        rewarded<<-FALSE
        return(0)
    }
}


#incrementally adjust the association score

adjust<-function(w,o,x){
    if(sum(values[w,])>0){values[w,o]<<-values[w,o]+(gamma*(reward(w,o,x)-values[w,o]))}
}


################
#the word learning algorithm

#choose meaning(s) for INITIALIZATION based on mutual exclusivity

first_guess<-function(x){
    scene<-intersect(visible[[x]],visible[[x]])
    #scene<-visible[[x]]
	if(length(scene)==1){return(scene)}
	else{
		maxes<-apply(values[,scene],2,max)
		return(names(maxes[maxes==min(maxes)])) #<-"arg min max"
	}
}

#INITIALIZATION of an association vector for a brand new word

introduce<-function(x){
	for(o in visible[[x]][is.element(visible[[x]],observed_objects)==F]){
		observed_objects<<-c(observed_objects,o) #<--log which objects have been seen
	}
	for(w in uttered[[x]]){
		if(is.element(w,observed_words)==F){
			for(o in first_guess(x)){values[w,o]<<-gamma} #<--initialize to gamma
			observed_words<<-c(observed_words,w) #<--log which words have been seen
		}
	}
}

#choose a single hypothesis to test against the current scene

choose_meaning<-function(w){
	return(sample(names(values[w,][values[w,]==max(values[w,])]),1)) #<--pick a single mapping w/ the highest association score
}


#INITIALIZATION plus PURSUIT:  introduce any new words, select a hypothesis for each word, adjust associations

analyze<-function(x){
	introduce(x)
    for(w in uttered[[x]]){
        choice<-choose_meaning(w)
        if(choice!="NULL"){adjust(w,choice,x)}
        if(rewarded==FALSE){adjust(w,sample(visible[[x]],1),x)} #<--if failure has occurred, reward a visible meaning at random
	}
}


#for each new situation, build a discrete lexicon based on a threshold function (LEXICON)

build<-function(){
    lexicon<<-list()
    for(w in observed_words){
        for(o in setdiff(observed_objects,"NULL")){
            prob<-(values[w,o]+lambda)/(sum(values[w,])+(lambda*length(objects)))
            if(prob>tau){
                lexicon[[length(lexicon)+1]]<<-c(w,o)
            }
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
		values[,]<<-0
		observed_words<<-c()
		observed_objects<<-c()
		lexicon<<-list()
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
		fscore<-2/((1/precision)+(1/recall))
		return(c(precision,recall,fscore))
	}
	else{
		return(c(0,0,0))
	}
}
