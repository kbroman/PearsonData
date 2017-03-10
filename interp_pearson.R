# R function interp_pearson
#   - to read in a table from Pearson and Lee (1903)
#   - interpolate to get pairs of values
#   - randomize values somewhat
#   - write to a file (if 'output' is non-null)
#
# Note: The counts of pairs for the resulting file will differ somewhat
#       from the original table

# input = input file name
# output = output file name (optional)
# digits = number of digits to round (if NULL, no rounding)
# match_count = If TRUE, ensure that number of rows in output
#               matches the count in the input

interp_pearson <-
    function(input, output=NULL, digits=1, match_count=TRUE)
{
    x <- as.matrix(read.table(input, check.names=FALSE))

    comments <- readLines(input)
    comments <- grep("^#", comments, value=TRUE) # just save the comments
    col_name <- strsplit(grep("^#\\s+cols:", comments, value=TRUE), "\\s+")[[1]][3]
    row_name <- strsplit(grep("^#\\s+rows:", comments, value=TRUE), "\\s+")[[1]][3]

    y <- round(x)
    while(sum(x) != sum(y)) { # repeat until we get matching counts
        for(i in 1:nrow(x)) {
            for(j in 1:ncol(x)) {
                y[i,j] <- (x[i,j] %/% 1) + as.numeric(runif(1) < x[i,j] %% 1)
            }
        }
        if(!match_count) break
    }

    z <- NULL
    co <- as.numeric(colnames(y))
    ro <- as.numeric(rownames(y))
    for(i in 1:nrow(y)) {
        for(j in 1:ncol(y)) {
            if(y[i,j] > 0) {
                zz <- matrix(rep(c(co[j]+0.5,ro[i]+0.5),y[i,j]),byrow=TRUE,ncol=2)
                if(is.null(z)) z <- zz
                else z <- rbind(z,zz)
            }
        }
    }

    dimnames(z) <- list(NULL, c(col_name, row_name))

    z <- z+runif(length(z),-0.5,0.5)

    if(!is.null(digits))
        z <- round(z, digits)

    if(!is.null(output)) {
        comments <- comments[!grepl("^#\\s+cols:", comments)]
        comments <- comments[!grepl("^#\\s+rows:", comments)]

        cat(paste0(comments, "\n", collapse=""), file=output)
        write.table(z, output, row.names=FALSE, col.names=TRUE,
                quote=FALSE, sep=",", append=TRUE)
        return(invisible(z))
    }

    z
}
