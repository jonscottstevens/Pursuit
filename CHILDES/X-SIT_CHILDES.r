#CROSS-SIT VARIANT (FAZLY ET AL. MODEL W/ ALIGNMENTS DETERMINED BY WORD PROBABILITIES RATHER THAN MEANING PROBABILITIES)
#CHILDES VERSION

library(plyr)


################
#preliminaries

#load data (also loads "gold" evaluation function and "goldlength" global variable)
#be sure to set your R working directory to the folder which contains the "CHILDES" folder!

#source("Training.r")
#source("Testing.r")
for(n in 1:length(visible)){visible[[n]]<-c(visible[[n]],"d")}


#initialize data structures

lexicon<-list()
words<-c()
objects<-c()
observed_words<-c()


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
tau<-0.09


#introduce novel words and hypotheses

introduce<-function(n){
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
	for(object in visible[[n]]){
		pool<-c(pool,probs[w,object])
	}
	denominator<-sum(pool)
	return(numerator/denominator)
}

probability<-function(w,o){
	numerator<-associations[w,o]+lambda
	denominator<-sum(associations[,o])+(beta*lambda)
	return(numerator/denominator)
}

update<-function(n){
	for(o in visible[[n]]){
		for(w in uttered[[n]]){
			associations[w,o]<<-associations[w,o]+alignment(w,o,n)
		}
		for(w in observed_words){
			probs[w,o]<<-probability(w,o)
		}
	}
}

build<-function(){
    for(w in observed_words){
        for(o in setdiff(objects,"d")){
            meaningprob<-(probs[w,o]+lambda)/(sum(probs[w,])+(beta*lambda))
            if(meaningprob>tau){lexicon[[length(lexicon)+1]]<<-c(w,o)}
        }
    }
}


#evaluation

w_mapped<-function(){
	domain<-c()
	for(n in 1:length(lexicon)){
		domain<-c(domain,lexicon[[n]][1])
	}
	return(domain)
}

evaluate<-function(){
	correct = 0
	if(length(lexicon)>0){
		for (n in 1:length(lexicon)){
			if(lexicon[[n]][2]==gold(lexicon[[n]][1])){
				correct = correct + 1
			}
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


#run on all data

simulate<-function(){
	for(n in 1:length(uttered)){
		introduce(n)
		update(n)
		print(n)
	}
	build()
	print(lexicon)
	evaluate()
}

bestF<-0
besttau<-0

optimize<-function(){
    tau<<-0.01
    for(m in 1:20){
        print(m)
        lexicon<<-list()
        build()
        eval<-evaluate()
        newF<-eval[3]
        if(newF>bestF){
            bestF<<-newF
            besttau<<-tau
        }
        tau<<-tau+0.01
    }
}