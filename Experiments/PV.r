#PROPOSE/VERIFY

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
        if(choice!="NULL"){reward(w,choice,x)}
        if(rewarded==FALSE){hyps[w,]<<-sample(visible[[x]],1)}
	}
}


#LEXICON step is unnecessary for experimental sims (subjs must make a guess even if there is no lexical entry)


################
#the experimental task

guess<-function(w){
    return(hyps[w,])
}

evaluate<-function(w){
    best_guess<-guess(w)
    guess_list<<-c(guess_list,best_guess)
	if(best_guess==gold(w)){return(1)}
	else{return(0)}
}


#initialize results lists

guess_list<-c() #<-sequence of all guesses
results_master<-c() #<-sequence of all guess evaluations


#simulate an experimental run

simulate<-function(id){
	values[,]<<-alphazero
    hyps[,]<<-"NULL"
    rewarded<<-FALSE
    observed_words<<-c()
    for(n in 1:length(uttered)){
        analyze(n)
        if(gold(uttered[[n]])!="NULL"){ #<-don't record fillers
            eval<-evaluate(uttered[[n]])
            results_master<<-c(results_master,eval)
        }
        print(c(id,n))
    }
}


#aggregate guesses and evals from multiple experiments; guesses and evals are dumped into "guess_list" and "results_master", respectively

aggregate<-function(num){
    for(n in 1:num){
		simulate(n)
	}
}


#aggregate simulation data and write the results to a CSV

runsims<-function(numsims,filename){

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
    output$Model<-rep("PV",length(guess_list))

    #write data frame as a CSV to "filename" in the working directory
    write.csv(output,paste(filename,".csv",sep=""))
}
