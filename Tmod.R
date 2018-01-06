library(limma)
library(tmod)
library(pca3d)
library(tagcloud)
library(XML)
data(Egambia)
design <- cbind(Intercept=rep(1, 30), TB=rep(c(0,1), each= 15)) 
E <- as.matrix(Egambia[,-c(1:3)])
fit <- eBayes( lmFit(E, design))
tt <- topTable(fit, coef=2, number=Inf, genelist=Egambia[,1:3] ) 
head(tt, 10)
group <- rep(c("CTRL", "TB"), each = 15)
showGene(E["20799",], group, main=Egambia["20799", "GENE_SYMBOL"])

fg <- tt$GENE_SYMBOL[tt$adj.P.Val < 0.05 & abs(tt$logFC) > 1 ]
res <- tmodHGtest(fg= fg, bg = tt$GENE_SYMBOL)
res
l  <- tt$GENE_SYMBOL
res2 <- tmodUtest(l)
head(res2)

l <- tt$GENE_SYMBOL
res2 <- tmodCERNOtest(l)
head(res2)
evidencePlot(l, "LI.M75")

res.l <- tmodLimmaTest(fit, Egambia$GENE_SYMBOL)
length(res.l)
head(res.l$TB)


plotCI <- function(x, ci.l, ci.r, title="") { n <- length(x)
plot(x,
     ylab="logFC", xlab="Index",
     pch=19, ylim=c( min(x-ci.l), max(x+ci.r)), main=title)
segments(1:n, ci.l, 1:n, ci.r, lwd=5, col="#33333333") }

par(mfrow=c(1,3))
x <- tmodLimmaTopTable(fit, coef="TB") 
print(head(x))

x <- x[ x$logFC.TB > 0, ] # only to simplify the output! 
x2 <- x[ order(abs(x$logFC.TB), decreasing=T),][1:50,] 
plotCI(x2$logFC.TB, x2$ciL.TB, x2$ciR.TB, "logFC")
x2 <- x[ order(x$qval.TB),][1:50,] 
plotCI(x2$logFC.TB, x2$ciL.TB, x2$ciR.TB, "q-value")
x2 <- x[ order(x$msd.TB, decreasing=T),][1:50,] 
plotCI(x2$logFC.TB, x2$ciL.TB, x2$ciR.TB, "MSD")

x <- tmodLimmaTopTable(fit, coef="TB", genelist = Egambia[,1:3])
x.lfc <- x[order(abs(x$logFC.TB), decreasing = T),]
x.qval <- x[ order(x$qval.TB, decreasing = T),]
x.msd <- x[ order(x$msd.TB, decreasing=T),]

head(tmodCERNOtest(x.lfc$GENE_SYMBOL))

head(tmodSummary(res.l), 5)
tmodPanelPlot(res.l, text.cex=0.6)

pie <- tmodLimmaDecideTests(fit, genes = Egambia$GENE_SYMBOL)
head(pie$TB[order(pie$TB[,"Up"], decreasing = T), ])
all(names(pie) %in% names(res.l))
tmodPanelPlot(res.l, pie = pie, text.cex = 0.6)


tmodPanelPlot(res.l,
              pie = pie, pie.style = "rug",
              grid = "between",
              text.cex = 0.6)

mypal <- c("#E69F00", "#56B4E9")
pca <- prcomp(t(E), scale. = TRUE)
par(mfrow=c(1, 2))
l <- pca2d(pca, group = group,
           palette = mypal)
cols <- as.character(l$colors)
legend("topleft", as.character(l$groups),
       pch=l$shapes,
       col=cols, bty="n")

l<-pca2d(pca, group=group, components=3:4,
         palette=mypal)
legend("topleft", as.character(l$groups),
       pch =l$shapes,
       col=cols, bty="n")

o <- order(abs(pca$rotation[, 4]), decreasing = TRUE)
l <- Egambia$GENE_SYMBOL[o]
res <- tmodUtest(l)
head(res)


# Calculate enrichment for each component
gs <- Egambia$GENE_SYMBOL
# function calculating the enrichment of a PC
gn.f <- function(r){
  tmodCERNOtest(gs[order(abs(r), decreasing = T)],
                qval = 0.01)
}

x <- apply(pca$rotation, 2, gn.f)
tmodSummary(x, filter.empty = TRUE)[1:5,]
tmodPanelPlot(x, text.cex = 0.8)


qfnc <- function(r) quantile(r, 0.75)
qqs <- apply(pca$rotation[,1:10], 2, qfnc)
pie <- tmodDecideTests(gs, lfc=pca$rotation[,1:10], lfc.thr=qqs) 
tmodPanelPlot(x[1:10], pie=pie, pie.style="rug", grid="between")

# PCA and tag clouds
w <- -log10(res$P.Value)
c <- smoothPalette(res$AUC, min=0.5) 
tags <- strmultline(res$Title) 
tagcloud(tags, weights=w, col=c)


par(mar=c(1,1,1,1))
o3 <- order(abs(pca$rotation[,3]), decreasing=TRUE) 
l3 <- Egambia$GENE_SYMBOL[o3]
res3 <- tmodUtest(l3) 
layout(matrix(c(3,1,0,2),2,2,byrow=TRUE),
       widths=c(1/3, 2/3), heights=c(2/3, 1/3)) 
# note -- PC4 is now x axis!! 
l<-pca2d(pca, group=group, components=4:3,
         palette=mypal, radius=1.8) 
cols <- as.character(l$colors) 
legend("topleft",
       as.character(l$groups), 
       pch=l$shapes,
       col=cols, bty="n")
tagcloud(tags, weights=w, col=c, fvert= 0) 
tagcloud(strmultline(res3$Title),
         weights=-log10(res3$P.Value), 
         col=smoothPalette(res3$AUC, min=0.5), 
         fvert=1)


plotf <- function(pca, components){
  id1 <- components[1]
  id2 <- components[2]
  print(id1)
  print(id2)
  plot(pca$x[,id1], pca$x[,id2])
}
ret <- tmodPCA(pca, genes = Egambia$GENE_SYMBOL,
               components = 4:3, plotfunc = plotf)

# Permutation tests
permset <- function(data, design){
  require(limma)
  data <- data[, sample(1:ncol(data))]
  fit <- eBayes(lmFit(data, design))
  tt <- topTable(fit, coef = 2, number = Inf, sort.by = "n")
  order(tt$P.Value)
}
# same design as before
design <- cbind(Intercept= rep(1, 30),
                TB=rep(c(0,1), each = 15))
E <- as.matrix(Egambia[, -c(1:3)])
N <- 250     # small number ofr the sake of example
set.seed(54321)
perms <- sapply(1:N, function(x) permset(E, design))
pauc <- tmodAUC(Egambia$GENE_SYMBOL, perms)
dim(perms)

fit <- eBayes(lmFit(E, design))
tt <- topTable(fit, coef = 2, number = Inf,
               genelist = Egambia[,1:3])
res <- tmodCERNOtest(tt$GENE_SYMBOL, qval = Inf, order.by = "n")
all(res$ID == rownames(perms))

fnsum <- function(m) sum(pauc[m,] >= res[m, "AUC"])
sums <- sapply(res$ID, fnsum)
res$perm.P.Val <- sums / N
res$perm.P.Val.adj <- p.adjust(res$perm.P.Val)
head(res[order(res$perm.P.Val),
         c("ID", "Title", "AUC", "adj.P.Val", "perm.P.Val.adj") ])


data(tmod)
res <- tmodUtest(tt$GENE_SYMBOL, qval = Inf)
gstest <- function(x){
  sel <- tt$GENE_SYMBOL %in% tmod$MODULES2GENES[[x]]
  geneSetTest(sel, tt$logFC)
}
gst <- sapply(res$ID, gstest)

plot(res$P.Value, gst,
     log="xy", pch=19, col="#33333366",
     xlab="P Values from tmod", 
     ylab="P Values from geneSetTest")
abline(0, 1) # the intercept and slope, single values.
abline(h=0.01, col="grey")
abline(v=0.01, col="grey")

mymset <- makeTmod(
  modules=data.frame(ID=c("A", "B"),
                     Title=c("A title",
                             "B title")),
  modules2genes = list(
    A = c("G1", "G2"),
    B=c("G3", "G4")
  )
)

msig <- tmodImportMSigDB("/Users/song/Downloads/msigdb_v5.0.xml")
res <- tmodCERNOtest(tt$GENE_SYMBOL, mset=msig)

sel <- msig$MODULES$Category == "H" 
tmodCERNOtest(tt$GENE_SYMBOL, mset=msig[sel] )

foo <- xmlParse("/Users/song/Downloads/msigdb_v5.0.xml")
foo2 <- xmlToList(foo)
path1 <- foo2[[1]]

orgs <- sapply(foo2, function(x) x["ORGANISM"])
unique(orgs)

foo3 <- foo2[orgs == "Homo sapiens"]
foo3 <- foo3[!sapply(foo3, is.null)]
modules <- t(sapply(foo3, function(x)
  x[ c("SYSTEMATIC_NAME", "STANDARD_NAME", "CATEGORY_CODE", "SUBCATEGORY_CODE") ]))
colnames(modules) <-  c( "ID", "Title", "Category", "Subcategory" )
modules <- data.frame(modules, stringsAsFactors = FALSE)
m2g <- lapply(foo3, 
              function(x) strsplit(x["MEMBERS_SYMBOLIZED"], ",")[[1]])
names(m2g) <- modules$ID
msig <- makeTmod(modules = modules, modules2genes = m2g)

#human <- tempfile()
#download.file(
 # "http://www.wikipathways.org//wpi/batchDownload.php?species=Homo%20sapiens&fileType=txt",
 # destfile=human, mode="wb")

#files <- unzip(human, list=T)
#files$ID <- gsub(".*_(WP[0-9]*)_.*", "\\1", files$Name)
#files$Title <- gsub("(.*)_WP[0-9]*_.*", "\\1", files$Name)

data(modmetabo) ## modules
data(tbmprof)
ids <- rownames(tbmprof)
tb <- factor(gsub("\\..*","", ids))
sex <- factor(gsub(".*\\.([MF])\\..*", "\\1", ids))
table(tb, sex)
wcx.tb <- apply(tbmprof, 2, function(x) wilcox.test(x~tb, conf.in=T))
wcx.tb <- t(sapply(wcx.tb, function(x) c(x$estimate, x$p.value)))
wcx.sex <- apply(tbmprof, 2, function(x) wilcox.test(x ~ sex, conf.int=T)) 
wcx.sex <- t(sapply(wcx.sex, function(x) c(x$estimate, x$p.value)))

wcx <- data.frame(ID=colnames(tbmprof),
                  E.tb = wcx.tb[,1], pval.tb = wcx.tb[,2],
                  E.sex = wcx.sex[,1], pval.sex=wcx.sex[,2],
                  row.names = colnames(tbmprof))
ids <- wcx$ID
res <- list()
res$tb <- tmodCERNOtest(ids[order(wcx$pval.tb)], mset = modmetabo)
res$sex <- tmodCERNOtest(ids[order(wcx$pval.sex)], mset=modmetabo)
modmetabo$MODULES[c("ME.107", "MP.2"),]
modmetabo$MODULES
tmodPanelPlot(res)

pie.data <- wcx[,c("E.sex", "E.tb")]
colnames(pie.data) <- c("sex", "tb")
# count the up- or down-regulated genes per module
# for each module in a set, calculate the number of 
# genes which are in that module and which are significantly
# up- or down-regulated.
# A gene is considered to be up-regulated if the associated p-value
# is smaller than pval.thr and the associated log flod change is 
# greater than lfc.thr. 
pie <- tmodDecideTests(wcx$ID, lfc=pie.data, lfc.thr = 0.2, mset = modmetabo)
# how many of the genes in a module are significantly up- or down-regulated
# pie, . The argument must be a list of length equal to the length of x.
tmodPanelPlot(res, pie = pie, pie.style = "rug", grid = "between",text.cex = 0.5)

wcx <- wcx[order(wcx$pval.sex),]
# select genes belonging to a module from a data frame
# showModule(x, genes, module, mset = "all", extra = TRUE)
showModule(wcx[,c("E.sex", "pval.sex")], wcx$ID, "MS.1", mset=modmetabo)[1:10,]
i <- "HMDB00493" 
modmetabo$GENES[i,]

par(mfrow=c(1,2))
# A combined beeswarm/boxplot
showGene(tbmprof[,i], sex, main=modmetabo$GENES[i, "Name"], ylab="Relative abundance")
# now for cortisol cluster
i <- "HMDB00063"
wcx <- wcx[order(wcx$pval.tb),]
showModule(wcx[, c("E.tb", "pval.tb")], wcx$ID, "ME.37", mset = modmetabo)[1:10,]
showGene(tbmprof[,i], tb, main=modmetabo$GENES[i, "Name"], ylab="Relative abundance")

pca <- prcomp(tbmprof, scale. = T)
# tmodPCA PCA plot annotated with tmod
ret <- tmodPCA(pca, genes = colnames(tbmprof), 
               mset=modmetabo, plot.params = list(group=tb, legend="topright"))
plot(1:length(unlist(pca[1])),as.numeric(unlist(pca[1])))
ret$plot.return
tmodPanelPlot(ret$enrichments)
pca <- prcomp(tbmprof, scale. = T)
ret <- tmodPCA(pca, genes = colnames(tbmprof), mset=modmetabo, 
               plot.params=list(group=tb, legend="topright"), mode="cross")
foo <- summary(lm(pca$x) ~ sex)
