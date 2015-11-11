

# This function gets a data frame with eye metrics for 10s windows
# in a session, and calculates the Load Index for each window in that session,
# returning the same dataset with added columns
# Since in some cases the field labels may vary, they can also be specified explicitly
calculateLoadIndexSession <- function(data, meanlabel="value.pupilMean", sdlabel="value.pupilSD", fixlabel="value.longFix", saclabel="value.sacSpd"){
    
    # We find out the median value for each parameter, and do the median cut
    meansessionmed <- median(data[,meanlabel], na.rm=T)
    data$Above.Mean <- as.numeric(data[,meanlabel] > meansessionmed)
    sdsessionmed <- median(data[,sdlabel], na.rm=T)
    data$Above.SD <- as.numeric(data[,sdlabel] > sdsessionmed)
    longsessionmed <- median(data[,fixlabel], na.rm=T)
    data$Above.Fix <- as.numeric(data[,fixlabel] > longsessionmed)
    sacsessionmed <- median(data[,saclabel], na.rm=T)
    data$Above.Sac <- as.numeric(data[,saclabel] > sacsessionmed)
    # We calculate the Load Index simply summing the different median cuts
    data$Load <- data$Above.Mean + data$Above.SD + data$Above.Fix + data$Above.Sac
    
    data
}


# This function gets a data frame with eye metrics for 10s windows
# in a session, normalizes each session data with that of the first episode
# in the session, and calculates the Load Index for each window in that session,
# using the median, not of that session, but of a "session of reference".
# Returns the same dataset with added columns
# Since in some cases the field labels may vary, they can also be specified explicitly
calculateLoadIndexOtherSessionNormalized <- function(data, refsession, meanlabel="value.pupilMean", sdlabel="value.pupilSD", fixlabel="value.longFix", saclabel="value.sacSpd"){

    # We normalize the data for each session with that of the first episode
    normdata <- data.frame()
    for(session in sessions){

        sessdata <- data[data$Session == session,]
        
        normsessdata <- sessdata
        normsessdata[,meanlabel] <- sessdata[,meanlabel]/sessdata[1,meanlabel]
        normsessdata[,sdlabel] <- sessdata[,sdlabel]/sessdata[1,sdlabel]
        normsessdata[,fixlabel] <- sessdata[,fixlabel]/sessdata[1,fixlabel]
        normsessdata[,saclabel] <- sessdata[,saclabel]/sessdata[1,saclabel]
        
        # We join the new data into a dataset with all sessions normalized data
        if(length(normdata)==0) normdata <- normsessdata
        else normdata <- rbind(normdata,normsessdata)
    }  
    
    # We calculate the session of reference's (normalized) medians
    refdata <- normdata[normdata$Session == refsession,]
    meansessionmed <- median(refdata[,meanlabel], na.rm=T)
    sdsessionmed <- median(refdata[,sdlabel], na.rm=T)
    longsessionmed <- median(refdata[,fixlabel], na.rm=T)
    sacsessionmed <- median(refdata[,saclabel], na.rm=T)
    
    
    # We calculate the load indices using the reference medians
    normdata$Above.Mean <- as.numeric(normdata[,meanlabel] > meansessionmed)
    normdata$Above.SD <- as.numeric(normdata[,sdlabel] > sdsessionmed)
    normdata$Above.Fix <- as.numeric(normdata[,fixlabel] > longsessionmed)
    normdata$Above.Sac <- as.numeric(normdata[,saclabel] > sacsessionmed)
    # We calculate the Load Index simply summing the different median cuts
    normdata$Load <- normdata$Above.Mean + normdata$Above.SD + normdata$Above.Fix + normdata$Above.Sac
    
    normdata
}

