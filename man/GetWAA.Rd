\name{GetWAA}
\alias{GetWAA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This is a title
}
\description{
Description
}
\usage{
GetWAA(YEAR, USERNAME, PASSWORD)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{YEAR}{
%%     ~~The year that is needed by query.  SCHOOL_YEAR format: e.g. 2012-13~~
}
  \item{USERNAME}{
%%     ~~Your LDS User Name~~
}
  \item{PASSWORD}{
%%     ~~Your LDS Password~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~No Refs ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (YEAR, USERNAME, PASSWORD) 
{
    library(RODBC)
    library(data.table)
    channel <- odbcConnect("LDSDB.world", uid = USERNAME, pwd = PASSWORD, 
        believeNRows = FALSE, rows_at_time = 1)
    WSAS <- sqlQuery(channel, paste("SELECT \n                 SCHOOL_YEAR, \n                 LDS_STUDENT_KEY, \n                 DIST_ACCTBL_AGENCY_KEY_WSAS,\n                 SCH_ACCTBL_AGENCY_KEY_WSAS,\n                 RDG_WAA_SWD_PROF_LVL,\n                 PRIMARY_DISAB_CODE_WSAS,\n                 MATH_WAA_SWD_PROF_LVL\n                 FROM P_LDS.VW_WSAS_OUTCOMES_NEW \n                 WHERE SCHOOL_YEAR='", 
        YEAR, "' AND\n                 (LDS_STATUS_CODE_WSAS <> 'DSK'\n                                    OR\n                                    LDS_STATUS_CODE_WSAS IS NULL)", 
        sep = ""))
    dist <- sqlQuery(channel, "SELECT \n                 AGENCY_KEY, AGENCY_NAME\n                 FROM P_LDS.LDS_AGENCY where AGENCY_KEY in (SELECT \n                 distinct DIST_ACCTBL_AGENCY_KEY_WSAS\n                 FROM P_LDS.WSAS_FACT)")
    dist <- data.table(dist)
    WSAS <- data.table(WSAS)
    WSAS$ct <- 1
    WSAS <- within(WSAS, {
        rdgwaa <- NA
        rdgwaa[RDG_WAA_SWD_PROF_LVL == 3 | RDG_WAA_SWD_PROF_LVL == 
            4] <- 1
        rdgwaa[RDG_WAA_SWD_PROF_LVL == 1 | RDG_WAA_SWD_PROF_LVL == 
            2] <- 0
        rdgwaa[is.na(rdgwaa)] <- 0
        matwaa <- NA
        matwaa[MATH_WAA_SWD_PROF_LVL == 3 | MATH_WAA_SWD_PROF_LVL == 
            4] <- 1
        matwaa[MATH_WAA_SWD_PROF_LVL == 1 | MATH_WAA_SWD_PROF_LVL == 
            2] <- 0
        matwaa[is.na(matwaa)] <- 0
    })
    DISABCT <- as.data.frame(WSAS[, list(disabct = sum(ct, na.rm = FALSE)), 
        by = list(PRIMARY_DISAB_CODE_WSAS)])
    DISABRDG <- as.data.frame(WSAS[, list(disabrdg = mean(rdgwaa, 
        na.rm = FALSE)), by = list(PRIMARY_DISAB_CODE_WSAS)])
    DISABMAT <- as.data.frame(WSAS[, list(disabmat = mean(matwaa, 
        na.rm = FALSE)), by = list(PRIMARY_DISAB_CODE_WSAS)])
    DISABRDG <- data.table(DISABRDG)
    DISABMAT <- data.table(DISABMAT)
    setkey(WSAS, PRIMARY_DISAB_CODE_WSAS)
    setkey(DISABRDG, PRIMARY_DISAB_CODE_WSAS)
    setkey(DISABMAT, PRIMARY_DISAB_CODE_WSAS)
    WSAS2 <- merge(WSAS, DISABRDG)
    WSAS2 <- merge(WSAS2, DISABMAT)
    ALL <- as.data.frame(WSAS[, list(total = sum(ct, na.rm = TRUE)), 
        by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    RDGCT <- as.data.frame(WSAS[, list(RDG_WAA_CT = sum(rdgwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    MATHCT <- as.data.frame(WSAS[, list(MATH_WAA_CT = sum(matwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    RDGPCT <- as.data.frame(WSAS[, list(RDG_WAA_PCT = mean(rdgwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    MATHPCT <- as.data.frame(WSAS[, list(MATH_WAA_PCT = mean(matwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    RDGexp <- as.data.frame(WSAS2[, list(EXP_RDG = mean(disabrdg, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    MATHexp <- as.data.frame(WSAS2[, list(EXP_MAT = mean(disabmat, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    CD <- WSAS[which(WSAS$PRIMARY_DISAB_CODE_WSAS == "CD"), ]
    CD_DIST <- as.data.frame(CD[, list(NUM_CD = sum(ct, na.rm = FALSE)), 
        by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    CD_MATHCT <- as.data.frame(CD[, list(CD_MATH_WAA_CT = sum(matwaa, 
        na.rm = FALSE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    CD_RDGCT <- as.data.frame(CD[, list(CD_RDG_WAA_CT = sum(rdgwaa, 
        na.rm = FALSE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    A <- WSAS[which(WSAS$PRIMARY_DISAB_CODE_WSAS == "A"), ]
    A_DIST <- as.data.frame(A[, list(NUM_A = sum(ct, na.rm = FALSE)), 
        by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    A_MATHCT <- as.data.frame(A[, list(A_MATH_WAA_CT = sum(matwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    A_RDGCT <- as.data.frame(A[, list(A_RDG_WAA_CT = sum(rdgwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    OHI <- WSAS[which(WSAS$PRIMARY_DISAB_CODE_WSAS == "OHI"), 
        ]
    OHI_DIST <- as.data.frame(OHI[, list(NUM_OHI = sum(ct, na.rm = FALSE)), 
        by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    OHI_MATHCT <- as.data.frame(OHI[, list(OHI_MATH_WAA_CT = sum(matwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    OHI_RDGCT <- as.data.frame(OHI[, list(OHI_RDG_WAA_CT = sum(rdgwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    EBD <- WSAS[which(WSAS$PRIMARY_DISAB_CODE_WSAS == "EBD"), 
        ]
    EBD_DIST <- as.data.frame(EBD[, list(NUM_EBD = sum(ct, na.rm = FALSE)), 
        by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    EBD_MATHCT <- as.data.frame(EBD[, list(EBD_MATH_WAA_CT = sum(matwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    EBD_RDGCT <- as.data.frame(EBD[, list(EBD_RDG_WAA_CT = sum(rdgwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    LD <- WSAS[which(WSAS$PRIMARY_DISAB_CODE_WSAS == "LD"), ]
    LD_DIST <- as.data.frame(LD[, list(NUM_LD = sum(ct, na.rm = FALSE)), 
        by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    LD_MATHCT <- as.data.frame(LD[, list(LD_MATH_WAA_CT = sum(matwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    LD_RDGCT <- as.data.frame(LD[, list(LD_RDG_WAA_CT = sum(rdgwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    SL <- WSAS[which(WSAS$PRIMARY_DISAB_CODE_WSAS == "SL"), ]
    SL_DIST <- as.data.frame(SL[, list(NUM_SL = sum(ct, na.rm = FALSE)), 
        by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    SL_MATHCT <- as.data.frame(SL[, list(SL_MATH_WAA_CT = sum(matwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    SL_RDGCT <- as.data.frame(SL[, list(SL_RDG_WAA_CT = sum(rdgwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    N <- WSAS[which(WSAS$PRIMARY_DISAB_CODE_WSAS == "N"), ]
    N_DIST <- as.data.frame(N[, list(NUM_N = sum(ct, na.rm = FALSE)), 
        by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    N_MATHCT <- as.data.frame(N[, list(N_MATH_WAA_CT = sum(matwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    N_RDGCT <- as.data.frame(N[, list(N_RDG_WAA_CT = sum(rdgwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    MISC <- WSAS[which(WSAS$PRIMARY_DISAB_CODE_WSAS == "H" | 
        WSAS$PRIMARY_DISAB_CODE_WSAS == "V" | WSAS$PRIMARY_DISAB_CODE_WSAS == 
        "OI" | WSAS$PRIMARY_DISAB_CODE_WSAS == "Y" | WSAS$PRIMARY_DISAB_CODE_WSAS == 
        "TBI"), ]
    MISC_DIST <- as.data.frame(MISC[, list(NUM_MISC = sum(ct, 
        na.rm = FALSE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    MISC_MATHCT <- as.data.frame(MISC[, list(MISC_MATH_WAA_CT = sum(matwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    MISC_RDGCT <- as.data.frame(MISC[, list(MISC_RDG_WAA_CT = sum(rdgwaa, 
        na.rm = TRUE)), by = list(DIST_ACCTBL_AGENCY_KEY_WSAS)])
    ALL <- data.table(ALL)
    MATHCT <- data.table(MATHCT)
    RDGCT <- data.table(RDGCT)
    MATHPCT <- data.table(MATHPCT)
    RDGPCT <- data.table(RDGPCT)
    MATHexp <- data.table(MATHexp)
    RDGexp <- data.table(RDGexp)
    CD_DIST <- data.table(CD_DIST)
    CD_MATHCT <- data.table(CD_MATHCT)
    CD_RDGCT <- data.table(CD_RDGCT)
    A_DIST <- data.table(A_DIST)
    A_MATHCT <- data.table(A_MATHCT)
    A_RDGCT <- data.table(A_RDGCT)
    OHI_DIST <- data.table(OHI_DIST)
    OHI_MATHCT <- data.table(OHI_MATHCT)
    OHI_RDGCT <- data.table(OHI_RDGCT)
    EBD_DIST <- data.table(EBD_DIST)
    EBD_MATHCT <- data.table(EBD_MATHCT)
    EBD_RDGCT <- data.table(EBD_RDGCT)
    LD_DIST <- data.table(LD_DIST)
    LD_MATHCT <- data.table(LD_MATHCT)
    LD_RDGCT <- data.table(LD_RDGCT)
    SL_DIST <- data.table(SL_DIST)
    SL_MATHCT <- data.table(SL_MATHCT)
    SL_RDGCT <- data.table(SL_RDGCT)
    N_DIST <- data.table(N_DIST)
    N_MATHCT <- data.table(N_MATHCT)
    N_RDGCT <- data.table(N_RDGCT)
    MISC_DIST <- data.table(MISC_DIST)
    MISC_MATHCT <- data.table(MISC_MATHCT)
    MISC_RDGCT <- data.table(MISC_RDGCT)
    setnames(dist, "AGENCY_KEY", "DIST_ACCTBL_AGENCY_KEY_WSAS")
    setkey(ALL, DIST_ACCTBL_AGENCY_KEY_WSAS)
    setkey(dist, DIST_ACCTBL_AGENCY_KEY_WSAS)
    setkey(MATHCT, DIST_ACCTBL_AGENCY_KEY_WSAS)
    setkey(RDGCT, DIST_ACCTBL_AGENCY_KEY_WSAS)
    setkey(MATHPCT, DIST_ACCTBL_AGENCY_KEY_WSAS)
    setkey(RDGPCT, DIST_ACCTBL_AGENCY_KEY_WSAS)
    setkey(RDGexp, DIST_ACCTBL_AGENCY_KEY_WSAS)
    setkey(MATHexp, DIST_ACCTBL_AGENCY_KEY_WSAS)
    setkey(CD_DIST, DIST_ACCTBL_AGENCY_KEY_WSAS)
    setkey(CD_MATHCT, DIST_ACCTBL_AGENCY_KEY_WSAS)
    setkey(CD_RDGCT, DIST_ACCTBL_AGENCY_KEY_WSAS)
    WAA <- merge(dist, ALL)
    WAA <- merge(WAA, MATHCT)
    WAA <- merge(WAA, RDGCT)
    WAA <- merge(WAA, MATHPCT)
    WAA <- merge(WAA, RDGPCT)
    WAA <- merge(WAA, MATHexp)
    WAA <- merge(WAA, RDGexp)
    WAA <- merge(WAA, A_DIST, all.x = TRUE)
    WAA <- merge(WAA, A_MATHCT, all.x = TRUE)
    WAA <- merge(WAA, A_RDGCT, all.x = TRUE)
    WAA <- merge(WAA, OHI_DIST, all.x = TRUE)
    WAA <- merge(WAA, OHI_MATHCT, all.x = TRUE)
    WAA <- merge(WAA, OHI_RDGCT, all.x = TRUE)
    WAA <- merge(WAA, EBD_DIST, all.x = TRUE)
    WAA <- merge(WAA, EBD_MATHCT, all.x = TRUE)
    WAA <- merge(WAA, EBD_RDGCT, all.x = TRUE)
    WAA <- merge(WAA, LD_DIST, all.x = TRUE)
    WAA <- merge(WAA, LD_MATHCT, all.x = TRUE)
    WAA <- merge(WAA, LD_RDGCT, all.x = TRUE)
    WAA <- merge(WAA, SL_DIST, all.x = TRUE)
    WAA <- merge(WAA, SL_MATHCT, all.x = TRUE)
    WAA <- merge(WAA, SL_RDGCT, all.x = TRUE)
    WAA <- merge(WAA, CD_DIST, all.x = TRUE)
    WAA <- merge(WAA, CD_MATHCT, all.x = TRUE)
    WAA <- merge(WAA, CD_RDGCT, all.x = TRUE)
    WAA <- merge(WAA, N_DIST, all.x = TRUE)
    WAA <- merge(WAA, N_MATHCT, all.x = TRUE)
    WAA <- merge(WAA, N_RDGCT, all.x = TRUE)
    WAA <- merge(WAA, MISC_DIST, all.x = TRUE)
    WAA <- merge(WAA, MISC_MATHCT, all.x = TRUE)
    WAA <- merge(WAA, MISC_RDGCT, all.x = TRUE)
    Z <- 1.115
    WAA$MAT_CI <- (WAA$total/(WAA$total + Z^2)) * (WAA$MATH_WAA_PCT + 
        (Z^2/(2 * WAA$total)) - Z * (WAA$MATH_WAA_PCT * ((1 - 
        WAA$MATH_WAA_PCT)/WAA$total) + ((Z^2)/(4 * WAA$total^2)))^0.5)
    WAA$RDG_CI <- (WAA$total/(WAA$total + Z^2)) * (WAA$RDG_WAA_PCT + 
        (Z^2/(2 * WAA$total)) - Z * (WAA$RDG_WAA_PCT * ((1 - 
        WAA$RDG_WAA_PCT)/WAA$total) + ((Z^2)/(4 * WAA$total^2)))^0.5)
    ReplaceNAs = function(DT) {
        for (i in names(DT)) DT[is.na(get(i)), `:=`(i, 0), with = FALSE]
    }
    ReplaceNAs(WAA)
    WAA
    return(list(fWAA = WAA, fDist = dist))
    rm(WSAS)
    rm(channel)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
