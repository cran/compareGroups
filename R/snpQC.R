reorder.snp2 <- function (x, ref = "common", ...) 
{
    s <- x
    if (!inherits(s, "snp")) 
        stop("object must be of class 'snp'")
    type <- charmatch(ref, c("common", "minor"))
    if (is.na(type)) 
        stop("ref must be either 'common' or 'minor'")
    if (type == 1) {
        class(s) <- "factor"
        tt <- table(s)
        if (length(tt) == 3 & min(tt) > 0) {
            if (tt[1] < tt[3]) {
                s <- relevel(relevel(s, 2), 3)
            }
        }
        else {
            if (length(unique(unlist(strsplit(names(tt)[1], "/")))) == 
                2 & length(tt) > 1) {
                s <- relevel(s, 2)
            }
        }
    }
    else {
        class(s) <- "factor"
        tt <- table(s)
        if (length(tt) == 3 & min(tt) > 0) {
            if (tt[3] < tt[1]) {
                s <- relevel(relevel(s, 2), 3)
            }
        }
        else {
            if (length(unique(unlist(strsplit(names(tt)[1], "/")))) == 
                2) {
                s <- relevel(s, 2)
            }
        }
    }
    class(s) <- c("snp", "factor")
    s
}


snp2 <- function (x, sep = "/", name.genotypes, reorder = "common", 
          remove.spaces = TRUE, allow.partial.missing = FALSE) 
{
    # if (is.snp(x)) {
    #     object <- x
    # }
    # else {
        if (sum(is.na(x)) == length(x)) {
            object <- rep(NA, length(x))
            attr(object, "allele.names") <- NULL
            class(object) <- c("snp", "logical")
            return(object)
        }
        if (missing(name.genotypes)) {
            alleles <- NULL
            x.d <- dim(x)
            x <- as.character(x)
            dim(x) <- x.d
            x[is.na(x)] <- ""
            if (remove.spaces) {
                xdim <- dim(x)
                x <- gsub("[ \t]", "", x)
                dim(x) <- xdim
            }
            if (!is.null(dim(x)) && ncol(x) > 1) 
                parts <- x[, 1:2]
            else {
                if (sep == "") 
                    sep <- 1
                if (is.character(sep)) {
                    part.list <- strsplit(x, sep)
                    part.list[sapply(part.list, length) == 0] <- NA
                    half.empties <- lapply(part.list, length) == 
                        1
                    part.list[half.empties] <- lapply(part.list[half.empties], 
                                                      c, NA)
                    empties <- is.na(x) | lapply(part.list, length) == 
                        0
                    part.list[empties] <- list(c(NA, NA))
                    parts <- matrix(unlist(part.list), ncol = 2, 
                                    byrow = TRUE)
                }
                else if (is.numeric(sep)) 
                    parts <- cbind(substring(x, 1, sep), substring(x, 
                                                                   sep + 1, 9999))
                else stop(paste("I don't know how to handle sep=", 
                                sep))
            }
            mode(parts) <- "character"
            temp <- grep("^[ \t]*$", parts)
            parts[temp] <- NA
            if (!allow.partial.missing) 
                parts[is.na(parts[, 1]) | is.na(parts[, 2]), 
                ] <- c(NA, NA)
            alleles <- unique(c(na.omit(parts)))
            if (length(alleles) > 2) 
                stop("SNP must have only two alleles")
            tmp <- ifelse(is.na(parts[, 1]) & is.na(parts[, 2]), 
                          NA, apply(parts, 1, paste, collapse = "/"))
            object <- factor(tmp)
            ll <- levels(object) <- na.omit(levels(object))
            if (length(ll) == 4) {
                object[object == ll[3]] <- ll[2]
                object <- factor(object)
            }
            control <- paste(rep(alleles[1], 2), collapse = "/") %in% 
                ll
            if (sum(control) == 0 & length(ll) == 3) {
                object[object == ll[2]] <- ll[1]
                object <- factor(object)
            }
            control <- paste(rep(alleles[2], 2), collapse = "/") %in% 
                ll
            if (sum(control) == 0 & length(ll) == 3) {
                object[object == ll[3]] <- ll[2]
                object <- factor(object)
            }
            if (length(object) == sum(is.na(object))) 
                stop("choose the correct character separator to divide alleles")
            class(object) <- c("snp", "factor")
            object <- reorder.snp2(object, ref = reorder)
            attr(object, "allele.names") <- alleles
        }
        else {
            if (any(is.na(match(x[!is.na(x)], name.genotypes)))) 
                stop("'name.genotypes' must match with the observed genotypes")
            x[x == name.genotypes[1]] <- "A/A"
            x[x == name.genotypes[2]] <- "A/B"
            x[x == name.genotypes[3]] <- "B/B"
            object <- as.factor(x)
            attr(object, "allele.names") <- c("A", 
                                              "B")
            class(object) <- c("snp", "factor")
        }
    # }
    object
}



setupSNP2 <- function (data, colSNPs, sep){
    # dataSNPs <- mclapply(data[, colSNPs, drop = FALSE], snp, sep = sep)
    dataSNPs <- lapply(data[, colSNPs, drop = FALSE], snp2, sep = sep)
    dataSNPs <- data.frame(dataSNPs)
    datPhen <- data[, -colSNPs, drop = FALSE]
    ans <- cbind(datPhen, dataSNPs)
    label.SNPs <- names(dataSNPs)
    class(ans) <- c("setupSNP", "data.frame")
    attr(ans, "row.names") <- 1:length(ans[[1]])
    attr(ans, "label.SNPs") <- label.SNPs
    attr(ans, "colSNPs") <- c((length(ans) - length(label.SNPs) + 1):length(ans))
    ans
}

snpQC <- function(X,sep,verbose)
{

    # X<-try(SNPassoc::setupSNP(X,1:ncol(X),sep=sep))
    # if (inherits(X, "try-error")) stop("ha donat un error")
    # if (inherits(X, "try-error")) stop(X)
    # for (i in 1:ncol(X)) X[,i] <- as.character(X[,i])
    X<-setupSNP2(X,1:ncol(X),sep=sep)

    snps<-attr(X,"label.SNPs")
    snp.sum<-data.frame(SNP=snps,
                        Ntotal=NA,    # Total number of samples for which genotyping was attempted
                        Ntyped=NA,    # Number of genotypes called
                        Typed.p=NA,   # Percentage genotyped
                        Miss.ct=NA,   # Number of missing genotypes
                        Miss.p=NA,    # Proportion of missing genotypes
                        Minor=NA,     # Minor Allele
                        MAF=NA,       # Minor allele frequency
                        A1=NA,        # Allele 1
                        A2=NA,        # Allele 2
                        A1.ct=NA,     # Count Allele 1
                        A2.ct=NA,     # Count Allele 2
                        A1.p=NA,      # Frequency of Allele 1
                        A2.p=NA,      # Frequency of Allele 2
                        Hom1=NA,      # Allele 1 Homozygote
                        Het=NA,       # Heterozygote
                        Hom2=NA,      # Allele 2 Homozygote
                        Hom1.ct=NA,   # Allele 1 Homozygote count
                        Het.ct=NA,    # Heterozygote Count
                        Hom2.ct=NA,   # Allele 2 Homozygote count
                        Hom1.p=NA,    # Frequency of Allele 1 Homozygote
                        Het.p=NA,     # Heterozygote frequency
                        Hom2.p=NA,    # Frequency of Allele 2 Homozygote
                        HWE.p=NA,
                        row.names=snps,
                        stringsAsFactors=FALSE
                       )

    # Compute genotyping success statistics
    snp.sum[,"Ntotal"]  <- nrow(X) 
    snp.sum[,"Ntyped"]  <- apply(X[,snps,drop=FALSE], 2, function(i) sum(!is.na(i)))
    snp.sum[,"Typed.p"] <- round(snp.sum[,"Ntyped"]/snp.sum[,"Ntotal"],3)
    snp.sum[,"Miss.ct"] <- snp.sum[,"Ntotal"] - snp.sum[,"Ntyped"]
    snp.sum[,"Miss.p"]  <- round(snp.sum[,"Miss.ct"]/snp.sum[,"Ntotal"],3)

    # Create an object to store results
    tm <- rep(NA,ncol(snp.sum)); names(tm) <- colnames(snp.sum); tm <- tm[-(c(1:6,ncol(snp.sum)))]

    # loop over SNPs
    snp.sum[,names(tm)] <- t(sapply(snps, function(snp.i){
        if (all(is.na(X[,snp.i])))  # no data
          return(tm)
        sm<-summary(X[!is.na(X[,snp.i]),snp.i])
        if(length(sm$allele.names)>2){
          snp.sum[snp.i,] <-NA
        } else {
          # Alleles
          tm["Minor"] <- rownames(sm$allele.freq)[which.min(sm$allele.freq[,2])]
          tm["MAF"]   <- round(min(sm$allele.freq[,2]),1)/100
          alels<-sm$allele.names
          if(length(alels)==2){
            tm[c("A1","A2")]<-alels
            tm[c("A1.ct","A2.ct")]<-sm$allele.freq[alels,"frequency"]
            tm[c("A1.p","A2.p")]<-round(sm$allele.freq[alels,"percentage"],digits=1)/100
          }
          if(length(alels)==1){
            tm[c("A1")]<-alels
            tm[c("A1.ct")]<-sm$allele.freq[alels,"frequency"]
            tm[c("A1.p")]<-round(sm$allele.freq[alels,"percentage"],digits=1)/100
          }
          # Genotypes
          gts <- attr(sm$genotype.freq,"dimnames")[[1]]; gts <- c(gts,rep(NA,3-length(gts)))
          tm[c("Hom1","Het","Hom2")] <- gts
          tm[c("Hom1.ct","Het.ct","Hom2.ct")][!is.na(gts)] <- sm$genotype.freq[,"frequency"]
          tm[c("Hom1.p","Het.p","Hom2.p")][!is.na(gts)] <- round(sm$genotype.freq[,"percentage"],1)/100
        }
        return(tm)    
    }))
    
    # Hardy-Weinberg test
    #require(HardyWeinberg)    
    hw <- as.matrix(snp.sum[,c("Hom1.ct","Het.ct","Hom2.ct")])
    hw[is.na(hw)] <- 0; hw <- matrix(as.numeric(hw),ncol=ncol(hw))
    snp.sum$HWE.p[rowSums(hw)>0]<-HWChisqMat(hw[rowSums(hw)>0,,drop=FALSE], verbose=verbose)$pvalvec

    # Set classes and return results
    numvar <- c("Ntotal","Ntyped","Typed.p","Miss.ct","Miss.p","MAF","A1.ct","A2.ct","A1.p","A2.p","Hom1.ct","Het.ct","Hom2.ct","Hom1.p","Het.p","Hom2.p","HWE.p")
    for(i in numvar) snp.sum[,i] <- as.numeric(snp.sum[,i])
    return(snp.sum)
}





