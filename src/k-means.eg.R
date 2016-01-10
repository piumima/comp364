source("~/T4LS/src/hucMini.R")


setwd('~/T4LS')
dataset.collections <- c("nki", "miniTCGA", "vanvliet", "curtis.discovery", "curtis.validation")
huc <- huc.load(dataSets = dataset.collections, dataDir = "data")
names(huc)

def.par <- par( no.readonly = TRUE )
par( mfrow = c(1,3) )
par( oma = c( 5, 0, 3, 0 ))

gene1.clust1 <- rnorm(100, mean = 5, sd = 4)
gene1.clust2 <- rnorm(100, mean = -5, sd = 4)
gene1.clust3 <- rnorm(100, mean = 0, sd = 1)

gene2.clust1 <- rnorm(100, mean = -6, sd = 8)
gene2.clust2 <- rnorm(100, mean = -2, sd = 5)
gene2.clust3 <- rnorm(100, mean = 8, sd = 3)

gene3.clust1 <- rnorm(100, mean = 6, sd = 2)
gene3.clust2 <- rnorm(100, mean = 1, sd = 12)
gene3.clust3 <- rnorm(100, mean = -7, sd = 3)

clusts <- c( rep(1, 100), rep(2,100), rep(3, 100) )

gene1 <- c(gene1.clust1, gene1.clust2, gene1.clust3)
gene2 <- c(gene2.clust1, gene2.clust2, gene2.clust3)
gene3 <- c(gene3.clust1, gene3.clust2, gene3.clust3)

my.exprs <- data.frame(  Expr.gene1 = gene1, Expr.gene2 = gene2, Expr.gene3 = gene3, 
                         Clusts = clusts)

head(my.exprs)
tail(my.exprs)

my.clusters <- kmeans( my.exprs[,1:3], 3 )
my.clusters

plot(my.exprs[,1], col=my.clusters$cluster, main = "Expression Gene 1", 
      xlab = "Patient #", ylab = "Expression level"
      )
abline( v = c(100,200 ))

plot(my.exprs[,2], col=my.clusters$cluster, main = "Expression Gene 2", 
     xlab = "Patient #", ylab = "Expression level"
)
abline( v = c(100,200 ))


plot(my.exprs[,3], col=my.clusters$cluster, main = "Expression Gene 3", 
     xlab = "Patient #", ylab = "Expression level"
)
abline( v = c(100,200 ))



title(main="Visualization", outer=TRUE)
par(xpd=NA)

legend( -800, -50, c("Cluster 1", "Cluster 2", "Cluster 3"), pch=1, col=1:3, horiz=TRUE)


par(def.par)


################### Hierarchical Clustering


source("~/T4LS/src/hucMini.R")


setwd('~/T4LS')
dataset.collections <- c("nki", "miniTCGA", "vanvliet", "curtis.discovery", "curtis.validation")
huc <- huc.load(dataSets = dataset.collections, dataDir = "data")
names(huc)

## order expression matrix and clinical info
xprs <- huc$vanvliet$exprs[1:10, 1:10, drop=FALSE]
rownames(xprs) <- huc$vanvliet$probe.info$gene.name[1:10]
clin <- huc$vanvliet$clinical[1:10, ]

## layout of the heatmap figure
lmat <- matrix(c("","key", "", "title","", "",
                 "","row.labels.rjust", "", "heatmap","", "",
                 "","", "", "","", "",
                 "","clinical.labels.rjust", "", "clinical","","",
                 "","", "", "", "",""), nrow=5, ncol=6, byrow=TRUE)
widths <- c(1,15,1,85,1)
heights <- c(4, nrow(xprs), 1, ncol(clin),1)

width <- 1024

height <- width/floor(ncol(xprs)/250)

heatmap.simple(xprs,
               lmat,
               row.labels=rownames(xprs),
               widths=widths,
               heights=heights,
               row.clust=FALSE,
               col.clust=FALSE,
               clinical = huc.color.clinical(clin), 
  
               color.scheme = heatmap.color.scheme(
                 low.breaks = seq(-2, 0, length.out = 21),
                 high.breaks = seq(0,2, length.out = 21)
               ),
               title=names("My Fancy Heatmap")
)
  

## layout of the heatmap figure
lmat <- matrix(c("","key", "", "","", "",
                 "","row.labels.rjust", "", "heatmap","", "",
                 "","", "", "","", "",
                 "","clinical.labels.rjust", "", "clinical","","",
                 "","", "", "", "",""), nrow=5, ncol=6, byrow=TRUE)
widths <- c(1,15,1,85,1)
heights <- c(4, nrow(xprs), 1, ncol(clin),1)

width <- 1024

height <- width/floor(ncol(xprs)/250)

heatmap.simple(xprs,
               lmat,
               row.labels=rownames(xprs),
               widths=widths,
               heights=heights,
               
               row.clust = TRUE,
               col.clust = TRUE,
 
               clinical = huc.color.clinical(clin), 
              
               color.scheme = heatmap.color.scheme(
                 low.breaks = seq(-2, 0, length.out = 21),
                 high.breaks = seq(0,2, length.out = 21)
               ),
               title=names("My Fancy Heatmap 2"),
)


# using hclust


xprs <- huc$vanvliet$exprs[1:10, 1:10, drop=FALSE]
rownames(xprs) <- huc$vanvliet$probe.info$gene.name[1:10]
d <- dist(xprs )  # computes the distance between the rows (genes)
hier.clustering <- hclust(d)

my.vars <- apply( huc$vanvliet$exprs, 1, var )
my.vars.sorted <- sort( my.vars, decreasing = TRUE, index.return = TRUE )

top.100 <- huc$vanvliet$exprs[my.vars.sorted[[2]][1:100],]
rownames(top.100) <- huc$vanvliet$probe.info$gene.name[my.vars.sorted[[2]][1:100]]

h.100 <- hclust(dist(top.100))
plot(h.100)

pat.h.100 <- hclust(dist( t(top.100) ) )
plot(pat.h.100)


top.20 <- huc$vanvliet$exprs[my.vars.sorted[[2]][1:20],]
rownames(top.20) <- huc$vanvliet$probe.info$gene.name[my.vars.sorted[[2]][1:20]]


top.250 <- huc$vanvliet$exprs[my.vars.sorted[[2]][1:250],]
rownames(top.250) <- huc$vanvliet$probe.info$gene.name[my.vars.sorted[[2]][1:250]]

top.500 <- huc$vanvliet$exprs[my.vars.sorted[[2]][1:500],]
rownames(top.500) <- huc$vanvliet$probe.info$gene.name[my.vars.sorted[[2]][1:500]]

top.1000 <- huc$vanvliet$exprs[my.vars.sorted[[2]][1:1000],]
rownames(top.1000) <- huc$vanvliet$probe.info$gene.name[my.vars.sorted[[2]][1:1000]]

top.all <- huc$vanvliet$exprs[my.vars.sorted[[2]]]
rownames(top.100) <- huc$vanvliet$probe.info$gene.name[my.vars.sorted[[2]][1:100]]

pat.h.250 <- hclust(dist( t(top.250)))
pat.h.500 <- hclust(dist( t(top.500)))
pat.h.1000 <- hclust(dist( t(top.1000)))
pat.h.all <- hclust(dist( t(my.vars.sorted[[2]])))


# not very clear above so let's create heatmaps to explore better

# we need the clinical vars for each

                               
top.current <- top.20
c.current <- huc$vanvliet$clinical
                               
## layout of the heatmap figure
lmat <- matrix(c("","key", "", "","", "",
                 "","row.labels.rjust", "", "heatmap","", "",
                 "","", "", "","", "",
                 "","clinical.labels.rjust", "", "clinical","","",
                 "","", "", "", "",""), nrow=5, ncol=6, byrow=TRUE)
widths <- c(1,15,1,85,1)
heights <- c(4, nrow(top.current), 1, ncol(c.current),1)

width <- 1024

height <- width/floor(ncol(top.current)/250)

heatmap.simple(top.current,
               lmat,
               row.labels=rownames(top.current),
               widths=widths,
               heights=heights,
               
               row.clust = TRUE,
               col.clust = TRUE,
               
               clinical = huc.color.clinical(c.current), 
               
               color.scheme = heatmap.color.scheme(
                 low.breaks = seq(-2, 0, length.out = 21),
                 high.breaks = seq(0,2, length.out = 21)
               ),
               title=names("top guys"),
)







