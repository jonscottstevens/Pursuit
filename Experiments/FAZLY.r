#FAZLY ET AL. 2010 IMPLEMENTATION

library(plyr)


################
#preliminaries

#MAKE SURE YOU HAVE GENERATED DATA BEFORE YOU LOAD THIS SCRIPT


#initialize data structures

words<-c()
objects<-c()
observed_words<-c()
observed_objects<-c()


#construct an empty matrix for storing associations

for(n in 1:length(uttered)){for(w in uttered[[n]]){if(is.element(w,words)==F){words<-c(words,w)}}}
for(n in 1:length(visible)){for(o in visible[[n]]){if(is.element(o,objects)==F){objects<-c(objects,o)}}}

associations<-matrix(nrow=length(words),ncol=length(objects))
rownames(associations)<-words
colnames(associations)<-objects
associations[,]<-0


#construct a matrix for storing meaning probabilities

probs<-matrix(nrow=length(words),ncol=length(objects))
rownames(probs)<-words
colnames(probs)<-objects
probs[,]<-0


#parameter values

beta<-100
lambda<-0.01
tau<-0.57


#introduce novel words and hypotheses

introduce<-function(n){
	for(o in visible[[n]]){
		if(is.element(o,observed_objects)==FALSE){
			observed_objects<<-c(observed_objects,o)
		}
	}
	for(w in uttered[[n]]){
		if(is.element(w,observed_words)==F){
			for(o in visible[[n]]){
				probs[w,o]<<-1/beta
			}
			observed_words<<-c(observed_words,w)
		}
		else{
			for(o in visible[[n]]){
				if(probs[w,o]==0){
					probs[w,o]<<-1/beta
				}
			}
		}
	}
}


#meat of the model

alignment<-function(w,o,n){
	numerator<-probs[w,o]
	pool<-c()
	for(word in uttered[[n]]){
		pool<-c(pool,probs[word,o])
	}
	denominator<-sum(pool)
	return(numerator/denominator)
}

probability<-function(w,o){
	numerator<-associations[w,o]+lambda
	denominator<-sum(associations[w,])+(beta*lambda)
	return(numerator/denominator)
}

update<-function(n){
	for(w in uttered[[n]]){
		for(o in visible[[n]]){
			associations[w,o]<<-associations[w,o]+alignment(w,o,n)
		}
		for(o in observed_objects){
			probs[w,o]<<-probability(w,o)
		}
	}
}

analyze<-function(x){
	introduce(x)
    update(x)
}


#LEXICON step is unnecessary for experimental sims (subjs must make a guess even if there is no lexical entry)


################
#the experimental task

remember<-1

guess<-function(w,x){ #<-choose best hypothesis; fall back on random guess
    hypothesis<-sample(names(associations[w,][associations[w,]==max(associations[w,])]),1)
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
	associations[,]<<-0
    probs[,]<<-0
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
    output$Model<-rep("FAZLY",length(guess_list))
    
    #write data frame as a CSV to "filename" in the working directory
    write.csv(output,paste(filename,".csv",sep=""))
}