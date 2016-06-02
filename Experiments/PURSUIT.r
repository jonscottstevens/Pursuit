#A REINFORCEMENT-BASED WORD LEARNING ALGORITHM

library(plyr)


################
#preliminaries

#MAKE SURE YOU HAVE GENERATED DATA BEFORE YOU LOAD THIS SCRIPT


#initialize data structures

words<-c()
objects<-c()
observed_words<-c()


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
	scene<-visible[[x]]
	if(length(scene)==1){return(scene)}
	else{
		maxes<-apply(values[,scene],2,max)
		return(names(maxes[maxes==min(maxes)])) #<-"arg min max"
	}
}

#INITIALIZATION of an association vector for a brand new word

introduce<-function(x){
	for(w in uttered[[x]]){
		if(is.element(w,observed_words)==F){
            for(o in first_guess(x)){values[w,o]<<-gamma} #<--initialize to gamma
			observed_words<<-c(observed_words,w) #<--log which words have been heard
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


#LEXICON step is unnecessary for experimental sims (subjs must make a guess even if there is no lexical entry)


################
#the experimental task

remember<-1

guess<-function(w,x){ #<-choose best hypothesis; fall back on random guess
    hypothesis<-choose_meaning(w)
    if(is.element(hypothesis,visible[[x]])==FALSE){hypothesis<-sample(visible[[x]],1)}
    if(sample(c(FALSE,TRUE),1,prob=c(remember,1-remember))){hypothesis<-sample(visible[[x]],1)} #the subject can forget their hypothesis!
    return(hypothesis)
}

evaluate<-function(w,x){ #<-did the learner guess the correct referent?
    best_guess<-guess(w,x)
    guess_list<<-c(guess_list,best_guess)
	if(best_guess==gold(w)){return(1)}
	else{return(0)}
}


#initialize results lists

guess_list<-c() #<-sequence of all guesses
results_master<-c() #<-sequence of all guess evaluations


#simulate an experimental run

simulate<-function(id){
	values[,]<<-0
    observed_words<<-c()
    for(n in 1:length(uttered)){
        analyze(n)
        if(gold(uttered[[n]])!="NULL"){ #<-don't record fillers
            eval<-evaluate(uttered[[n]],n)
            results_master<<-c(results_master,eval)
        }
        print(c(id,n))
    }
}


#aggregate guesses and evals from multiple experiments; guesses and evals are dumped into "guess_list" and "results_master", respectively

aggregate<-function(num){
    for(n in 1:num){
		values[,]<<-0
		observed_words<<-c()
		simulate(n)
	}
}


#aggregate simulation data and write the results to a CSV

runsims<-function(numsims,memory,filename){
    remember<<-memory
    
    results_master<<-c()
    guess_list<<-c()
    
    #import generic data frame with subject/item data, repeat data "numsims" times
	output<-do.call("rbind", replicate(numsims, blank_data, simplify = FALSE))
    Simulation<-c()
    for(n in 1:numsims){Simulation<-c(Simulation,rep(n,nrow(blank_data)))}
    output$Simulation<-Simulation
    
    #run simulations and replace "Correct" column with simulation output
	aggregate(numsims)
    output$Correct<-results_master
    
    #add the recorded guesses for each instance
    output$Guess<-guess_list
    
    #add a "model" column
    output$Model<-rep("PURSUIT",length(guess_list))
    
    #write data frame as a CSV to "filename" in the working directory
    write.csv(output,paste(filename,".csv",sep=""))
}