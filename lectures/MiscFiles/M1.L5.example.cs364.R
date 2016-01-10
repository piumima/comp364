subject<- c("I", "You", "John", "Francis", "Erica")
verb <- c("defined", "ate", "high-fived", "made", "threw")
count <- c(5,3,10,8,7)
object <- c("variables", "cheeseburgers", "friends", "sculptures", "punches")

sentence.pieces <- data.frame(Subject=subject, Verb=verb, Count=count, Object=object, stringsAsFactors=F)
rownames(sentence.pieces) <- letters[1:5]

complete.sentences <- vector("character",5)
names(complete.sentences) <- letters[1:5]

for(index.of.my.cool.variable in rownames(sentence.pieces)){
	complete.sentences[index.of.my.cool.variable] <- paste(sentence.pieces[index.of.my.cool.variable,"Subject"], sentence.pieces[index.of.my.cool.variable,"Verb"], sentence.pieces[index.of.my.cool.variable,"Count"], sentence.pieces[index.of.my.cool.variable,"Object"])
}


#Forget the for loop! R likes to do things on vectors
#paste(sentence.pieces[,"Subject"], sentence.pieces[,"Verb"], sentence.pieces[,"Count"], sentence.pieces[,"Object"])


sentence.match <- FALSE
loop.count <-0
while(!sentence.match){
	loop.count <- loop.count+1
	trial.sentence<-paste(sentence.pieces[sample(1:5,1),"Subject"],sentence.pieces[sample(1:5,1),"Verb"], sentence.pieces[sample(1:5,1),"Count"],sentence.pieces[sample(1:5,1),"Object"] )
	print(trial.sentence)
	sentence.match<-ifelse(is.element(trial.sentence, complete.sentences), TRUE, FALSE)
	#Could this be coded simpler?
	
}
loop.count
