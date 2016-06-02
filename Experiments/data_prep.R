library(plyr)
uttered<-list()
visible<-list()
blank_data<-data.frame()

generate_data<-function(filename){
    clicking_data<-read.csv(filename)
    
    guesses<-clicking_data[,grep("Item",colnames(clicking_data))]
    
    num_items<-nrow(clicking_data)
    num_exposures<-max(clicking_data$GroupingOrder)

    for(m in 1:num_items){
        appendix<-c()
        for(n in 1:length(guesses)){
            if(guesses[m,n]!=''){
                appendix<-c(appendix,toString(guesses[m,n]))
            }
        }
        visible[[length(visible)+1]]<<-appendix
    }

    uttered<<-c(uttered,as.character(clicking_data$SoundFile))
    
    #grab gold standard meaning from target "Item1" values
    
    clicking_data<-subset(clicking_data,Type=="Target")
    
    num_items<-nrow(clicking_data)

    goldmeaning<-function(x){
        if(is.element(x,clicking_data$SoundFile)){return(as.character(subset(clicking_data,SoundFile==x)$Item1)[1])}
        else{return("NULL")}
    }

    #create a "blank" data frame with subject/item data for a single simulation, to be filled in after simulations

    Word<-clicking_data$SoundFile
    Gold<-c()
    for(w in Word){Gold<-c(Gold,goldmeaning(w))}
    
    if(is.null(clicking_data$Subject)){Subject<-rep(1,num_items)}
    else{Subject<-clicking_data$Subject}
    
    Instance<-clicking_data$GroupingOrder
    Correct<-rep(0,num_items)
    Guess<-rep(0,num_items)
    Condition<-clicking_data$Condition

    blank_data<<-data.frame(Subject,Word,Condition,Instance,Guess,Gold,Correct)
}

#gold standard helper function to be used in simulations

gold<-function(x){
    if(is.element(x,blank_data$Word)){return(as.character(subset(blank_data,Word==x)$Gold[1]))}
    else{return("NULL")}
}