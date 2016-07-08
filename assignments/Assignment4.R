## Piumi Abeynayaka ID: 260458806

## Question 1
N=20000
A.probes=100
B.probes=250
C.probes=750

probabilities <- matrix(nrow=3,
                        ncol=3,
                        dimnames=list(c("A red balls","B red balls","C red balls"),
                                      c("A draws","B draws","C draws")))

## Intersection of A and B
## B=red balls, draw A times
probabilities[2,1]<-binom.test(x=10, n=A.probes, p=B.probes/N)$p.value

## A=red balls, draw B times
probabilities[1,2]<-binom.test(x=10, n=B.probes, p=A.probes/N)$p.value

## Intersection of A and C
## C=red balls, draw A times
probabilities[3,1]<-binom.test(x=20, n=A.probes, p=C.probes/N)$p.value

## A=red balls, draw C times
probabilities[1,3]<-binom.test(x=20, n=C.probes, p=A.probes/N)$p.value

## Intersection of B and C
## C=red balls, draw B times
probabilities[3,2]<-binom.test(x=10, n=B.probes, p=C.probes/N)$p.value

## B=red balls, draw C times
probabilities[2,3]<-binom.test(x=10, n=C.probes, p=B.probes/N)$p.value

print(probabilities)

## Question 2

hyper.probs <- matrix(ncol=3,dimnames=list(NULL,c("A and B","A and C","B and C")))

## Intersection of A and B
## B=red balls, draw A times
hyper.probs[1,1]<-dhyper(x=10, m=B.probes, n=N-B.probes, k=A.probes)

## A=red balls, draw B times
## Answer is the same as above
## dhyper(x=10, m=A.probes, n=N-A.probes, k=B.probes)

## Intersection of A and C
## C=red balls, draw A times
hyper.probs[1,2]<-dhyper(x=20, m=C.probes, n=N-C.probes, k=A.probes)

## A=red balls, draw C times
## Answer is the same as above
## dhyper(x=20, m=A.probes, n=N-A.probes, k=C.probes)

## Intersection of B and C
## C=red balls, draw B times
hyper.probs[1,3]<-dhyper(x=10, m=C.probes, n=N-C.probes, k=B.probes)

## B=red balls, draw C times
## Answer is the same as above
## dhyper(x=10, m=B.probes, n=N-B.probes, k=C.probes)

print(hyper.probs)
