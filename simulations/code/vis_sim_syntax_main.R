################################################################################
##### Package Loading #######
################################################################################

  ## Package Loading  
    library(tidyverse)
    library(readxl)
    library(xlsx)
    library(haven)
    library(lubridate)
    library(gdata)
    library(labelled)
    library(sjlabelled)

################################################################################
##### Data Loading #######
################################################################################

  ## Load Cleaned Data
    dat <- read.csv() # Data File Removed
    dat <- datcleaned[,c(2:3027)]

################################################################################
##### Ensure Correct Variable Type #######
################################################################################   
    
    
    
    numeric.conv <- c(1,
                      ## T3
                      3,91:92,107:108,114:115,122:123,126:127,130:131,135:136,
                      148:149,154:157,200:207,218:219, 339:354, 355:365, 371:373,397:404, 
                      411:416, 417:418, 443:449,484:487, 490, 
                      ## T3b
                      530, 618:619,634:635, 641:642, 649:650, 653:654, 657:658,
                      662:663, 675:676, 681:684, 727:734, 745:746, 866:881, 882:892,
                      898:900, 924:931, 938:943, 944:945, 970:976, 1011:1014, 1017,
                      ## T4
                      1057, 1145:1146, 1161:1162, 1168:1169, 1176:1177, 1180:1181, 1184:1185,
                      1189:1190, 1202:1203, 1208:1211, 1254:1261, 1272:1273, 1393:1408, 
                      1409:1419, 1425:1427, 1451:1458, 1465:1470, 1471:1472, 1497:1503, 
                      1538:1541, 1544,
                      ## T5
                      1584, 1672:1673, 1688:1689, 1695:1696, 1703:1704, 1707:1708,
                      1711:1712, 1716:1717, 1729:1730, 1735:1738, 1781:1788, 1799:1800,
                      1920:1935, 1936:1946, 1952:1954, 1978:1985, 1992:1997, 1998:1999,
                      2024:2030, 2065:2068, 2071,
                      ## Self Report
                      2121, 2125, 2134:2137, 2138, 2238, 2261:2263, 2276, 2296, 2297, 
                      2299:2300, 
                      ## T2
                      2308, 2315:2316, 2335:2336, 2342:2343, 2350:2351, 2358:2359,
                      2363:2364, 2373, 2386:2387, 2391:2392, 2485:2488, 2505, 2506,
                      2903:2919, 2954:2960, 2995:3001, 3003, 3009)
    
    dat.conv <- c(2, 529, 1056, 1583, 2237, 2239:2240, 2248, 2280, 2307, 
                  2498)
    
    fact.conv <- c(
      ## T3
      4:83, 85:90, 93:106, 109:113,
      116:121, 124:125, 128, 132:134,
      137:147, 150:151, 152:153, 158:159, 
      160:181, 182:195, 196:199, 208:217, 
      220:231, 232:241, 242:251, 252:285, 
      286:311, 312:323, 324:338, 366:370, 
      374:388, 389:396, 405:410, 424:425,
      419:423, 426:442, 450:466, 467:483, 
      488:489, 491:507, 508:528,
      ## T3b
      531:610, 612:617, 620:633, 636:640,
      643:648, 651:652, 655, 659:661,
      664:674, 677:678, 679:680, 685:686,
      687:708, 709:722, 723:726, 735:744,
      747:758, 759:768, 769:778, 779:812,
      813:838, 839:850, 851:865, 893:897,
      901:915, 916:923, 932:937, 951:952, 
      946:950, 953:969, 977:993, 994:1010,
      1015:1016, 1018:1034, 1035:1055,
      ## T4
      1058:1137, 1139:1144, 1147:1160, 1163:1167,
      1170:1175, 1178:1179, 1182, 1186:1188,
      1191:1201, 1204:1205, 1206:1207, 1212:1213,
      1214:1235, 1236:1249, 1250:1253, 1262:1271,
      1274:1285, 1286:1295, 1296:1305, 1306:1339,
      1340:1365, 1366:1377, 1378:1392, 1420:1424,
      1428:1442, 1443:1450, 1459:1464, 1478:1479,
      1473:1477, 1480:1496, 1504:1520, 1521:1537, 
      1542:1543, 1545:1561, 1562:1582,
      ## T5
      1585:1664, 1666:1671, 1674:1687, 1690:1694,
      1697:1702, 1705:1706, 1709, 1713:1715,
      1718:1728, 1731:1732, 1733:1734, 1739:1740,
      1741:1762, 1763:1776, 1777:1780, 1789:1798,
      1801:1812, 1813:1822, 1823:1832, 1833:1866,
      1867:1892, 1893:1904, 1905:1919, 1947:1951,
      1955:1969, 1970:1977, 1986:1991, 2005:2006,
      2000:2004, 2007:2023, 2031:2047, 2048:2064,
      2069:2070, 2072:2088, 2089:2109,
      ## Self
      2110:2120, 2122, 2124, 2127:2132, 2139, 2142:2165,
      2166, 2168, 2171:2178, 2179:2180, 2182:2184, 2186, 2188:2193,
      2196:2218, 2219:2220, 2223:2231, 2233:2236, 2242, 
      2244:2247, 2249, 2250:2260, 2264, 2266, 2268, 2270, 2272:2273,
      2277:2278, 2282:2283, 2285, 2287, 2289:2295, 2298, 
      2301:2306, 2309:2314, 2317:2334, 2337:2341, 2344:2349, 
      2352:2353, 2356, 2360:2362, 2365:2372, 2374:2384, 
      2388:2390, 2393:2398, 2400:2474, 2476:2484, 2489:2497,
      2499:2504, 2510:2514, 2518:2521, 2526:2530, 2534:2536, 
      2540:2544, 2548:2552, 2557:2560, 2565:2568, 2572:2575,
      2581:2584, 2589:2592, 2596:2599, 2603:2606, 2611:2613,
      2618:2620, 2624:2626, 2631:2633, 2637:2639, 2645:2647, 
      2651:2653, 2657:2659, 2663:2665, 2670:2674, 2679:2684, 
      2687:2692, 2695:2698, 2699:2902, 2920:2953, 2961:2994, 
      3002, 3004:3006, 3008, 3010:3011, 3012:3026)
    
    str.conv <- c(84, 129, 2111, 2123, 2126, 2133, 2140, 2141, 2167, 2169:2170,
                  2181, 2185, 2187, 2194:2195, 2221:2222, 2232, 2241, 2243, 2265,
                  2267, 2269, 2271, 2274:2275, 2279, 2281, 2284, 2286, 2288, 
                  2354:2355, 2357, 2385, 2399, 2475, 2507:2509, 2515:2517, 2522:2525,
                  2531:2533, 2537:2539, 2545:2547, 2553:2556, 2561:2564, 2569:2571,
                  2576:2580, 2585:2588, 2593:2595, 2600:2602, 2607:2610, 2614:2617,
                  2621:2623, 2627:2630, 2634:2636, 2640:2644, 2648:2650, 2654:2656,
                  2660:2662, 2666:2669, 2675:2678, 2685:2686, 2693:2694, 3007)
    
    dat[,numeric.conv] <- sapply(
      dat[,numeric.conv], FUN = as.numeric)
    
    dat$T2.dat <- as.Date(dat$T2.dat)
    dat$T2.dat.2 <- as.Date(dat$T2.dat.2)
    dat$T2.dat.x <- as.Date(dat$T2.dat.x)
    dat$T3.dat <- as.Date(dat$T3.dat)
    dat$T3b.dat <- as.Date(dat$T3b.dat)
    dat$T4.dat <- as.Date(dat$T4.dat)
    dat$T5.dat <- as.Date(dat$T5.dat)
    dat$dat.in.hos <- as.Date(dat$dat.in.hos)
    dat$dat.out.hos <- as.Date(dat$dat.out.hos)
    dat$traumatic.event.date <- as.Date(dat$traumatic.event.date)
    
    ## Convert to factor 
    dat.factor <- dat[,c(1, fact.conv)]
    for (i in 2:length(dat.factor)){
      dat.factor[,i] <- as.factor(dat.factor[,i])
    }
    ## Rejoin 
    dat[,fact.conv] <- dat.factor[,2:2453]
    
    dat[,str.conv] <- sapply(
      dat[,str.conv], FUN = as.character)
    
    
    ##
    datdf <- as.data.frame(dat)
    wide.colnames <- colnames(datdf)
    wide.colnames <- as.data.frame(wide.colnames)
    wide.colnames$vartype <- NA
    wide.colnames$desc <- NA
    
    for (i in 1:3026) {
      print(i)
      wide.colnames[i,2] <- class(datdf[,i])
      if(class(datdf[,i]) == "factor"){
        if(is.null(levels(datdf[,i])) == TRUE){
          wide.colnames[i,3] <- 1
        } else {
        wide.colnames[i,3] <- levels(datdf[,i])
        }}
      if(class(datdf[,i]) == "Date"){
        wide.colnames[i,3] <- "Date"
      } 
      if(class(datdf[,i]) == "numeric"){
        wide.colnames[i,3] <- toString(range(datdf[,i], na.rm = TRUE))
      } 
    }

    
    
    
################################################################################
##### Proof of concept #######
################################################################################   

    ## All Variables will be simulated, except those classified as dates (dat.conv) or strings (str.conv)
    ## As these will not be able to be simulated effectively, and will not be used in the later analysis
    
    dat.sim <- dat[, c(numeric.conv, fact.conv)]
    dat.sim <- dat.sim[,-c(2186, 2187, 2188, 2189, 2280)]
    sim.var <- length(dat.sim)
    
    ## Assume that the number of simulations 
    sim.n <- 10000
    ## Extract Column names
    dat.sim.colnames <- colnames(dat.sim)
    ## Specific Columns 
    numeric.n <- length(numeric.conv)
    fact.n <- length(fact.conv)
    
    
    ## Create Empty matrix
    dat.matrix <- as.data.frame(matrix(data = NA, nrow = sim.n, ncol = sim.var))
    colnames(dat.matrix) <- dat.sim.colnames
    
    ## Convert empty variables to numeric
    dat.matrix[,c(1:sim.var)] <- sapply(
      dat.matrix[,c(1:sim.var)], FUN = as.numeric)
    
    ## Create index patnr variable
    dat.matrix$Patnr <- 1:sim.n
    
    ## For loop to determine how to simulate variables
    ## Step 1: For each variable, after PATNR
  for (i in 2:sim.var){
      ## Step 1b: Determine how many unique values exist in each column (including NA)
      unique.val <- unique(dat.sim[,i])
      ## Step 1c: Sort these, with NA at the end (this is for determining intergers)
      unique.val <- sort(unique.val, na.last = T)
      
      ## Step 1d: Determine class of column (i)
      col.class <- class(dat.sim[,i])
      
      
      ## Step 2: If statements 
      if(length(unique.val) == 1){
        dat.matrix[,i] <- unique.val
        
        ## Step 2b: If length is less than or equal to three, this indicates a dichomous variable
        ## Therefore simulate accordingly
      } else if(col.class == "numeric"){
        if(
          if(unique.val[1] == 0){
            if(is.na(unique.val[2]) == TRUE){
              floor(unique.val[1] == unique.val[1])
            } else {
            floor(unique.val[2]) == unique.val[2]
          }} else {
            floor(unique.val[1]) == unique.val[1]
          }){
            
          dat.matrix[,i] <- floor(rnorm(n = sim.n,
                                        mean = mean(dat.sim[,i], na.rm = TRUE),
                                        sd = sd(dat.sim[,i], na.rm = TRUE)))
          ## Step 2d-b: If this value is different, assume non-int status, so generate values accordingly 
        } else {
          dat.matrix[,i] <- rnorm(n = sim.n,
                                  mean = mean(dat.sim[,i], na.rm = TRUE),
                                  sd = sd(dat.sim[,i], na.rm = TRUE))
        }
      } else if(col.class == "factor") {
        print(i)
        col.summary <- summary(dat.sim[,i])
        col.summary.edit <- as.numeric(gsub(".*:","", col.summary))
        col.summary.edit <- as.data.frame(col.summary.edit) 
        col.summary.edit$rownames <- unique.val
        
        col.summary.edit$prob <- col.summary.edit[,1]/852
        
        sum.prob <- col.summary.edit$prob
        sum.rows <- as.character(col.summary.edit$rownames)
        
        dat.matrix[,i] <- sample(sum.rows, 
                                 size = sim.n,
                                 prob = sum.prob,
                                 replace = T)
        
        
      } 
    }
      
    for (i in numeric.n:length(dat.matrix)){
      dat.matrix[,i] <- as.factor(dat.matrix[,i])
    }
    for (i in 1:numeric.n){
      dat.matrix[,i] <- as.numeric(dat.matrix[,i])
    }
      
################################################################################
##### Application of Visual Simulation #######
################################################################################      

    ## Create function of previous code 
    vis.sim <- function(data, matrix.count = 100, sim.n = 100, numeric.limit){
      ## Define Parameters 
        ## Number of variables 
        dat.varlength <- length(data)
        ## Colnames of database
        dat.colnames <- colnames(data)
        ## Number of simulations to make 
        matrix.count <- matrix.count
        ## Size of each simulation 
        sim.n <- sim.n
        ## Where do numeric variables end? 
        numeric.limit = numeric.limit
        
      ## Create Empty list for storage 
        matrix.list.sim <- vector(mode = "list", length = matrix.count)
        
      ## Begin loop for simulating each matrix 
      for (j in 1:matrix.count){
        ## Set random seed 
        set.seed((1 + sim.n*(j-1)))
        ## generate temp matrix 
        temp.matrix <- as.data.frame(matrix(data = NA,
                                            nrow = sim.n,
                                            ncol = dat.varlength))
        colnames(temp.matrix) <- dat.colnames
        
        ## Convert all values to numeric 
        temp.matrix[, c(1:dat.varlength)] <- sapply(
          temp.matrix[, c(1:dat.varlength)], FUN = as.numeric)
        
        ## Assign Patient number to first value 
        temp.matrix$Patnr <- 1:sim.n
        
      ## Generate Values for Matrix 
        for (i in 2:dat.varlength){
          ## Step 1b: Determine how many unique values exist in each column (including NA)
          unique.val <- unique(data[,i])
          ## Step 1c: Sort these, with NA at the end (this is for determining intergers)
          unique.val <- sort(unique.val, na.last = T)
          
          ## Step 1d: Determine class of column (i)
          col.class <- class(data[,i])
          
          
          ## Step 2: If statements 
          if(length(unique.val) == 1){
            temp.matrix[,i] <- unique.val
            
            ## Step 2b: Determine if the value is numeric or factor based 
            ## If numeric, determine if it should be interger or non-int numbers
            ## Therefore simulate accordingly
          } else if(col.class == "numeric"){
            if(
              if(unique.val[1] == 0){
                if(is.na(unique.val[2]) == TRUE){
                  floor(unique.val[1] == unique.val[1])
                } else {
                  floor(unique.val[2]) == unique.val[2]
                }} else {
                  floor(unique.val[1]) == unique.val[1]
                }){
              ## Generate Int values
              temp.matrix[,i] <- floor(rnorm(n = sim.n,
                                            mean = mean(data[,i], na.rm = TRUE),
                                            sd = sd(data[,i], na.rm = TRUE)))
            } else {
              ## Generate Non-Int Values
              temp.matrix[,i] <- rnorm(n = sim.n,
                                      mean = mean(data[,i], na.rm = TRUE),
                                      sd = sd(data[,i], na.rm = TRUE))
            }
            ## Step 2b-b If factor, determine frequency and probability and generate accordingly
          } else if(col.class == "factor") {
            col.summary <- summary(data[,i])
            col.summary.edit <- as.numeric(gsub(".*:","", col.summary))
            col.summary.edit <- as.data.frame(col.summary.edit) 
            col.summary.edit$rownames <- unique.val
            
            col.summary.edit$prob <- col.summary.edit[,1]/852
            
            sum.prob <- col.summary.edit$prob
            sum.rows <- as.character(col.summary.edit$rownames)
            
            temp.matrix[,i] <- sample(sum.rows, 
                                     size = sim.n,
                                     prob = sum.prob,
                                     replace = T)
            
            
          }
          
        }
        ## Convert values to numeric/factor where required
        
        for (i in numeric.limit:dat.varlength){
          temp.matrix[,i] <- as.factor(temp.matrix[,i])
        }
        for (i in 1:numeric.limit){
          temp.matrix[,i] <- as.numeric(temp.matrix[,i])
        }
        
        
        ## Assign generated matrix to list
        print( paste("Storing matrix: ", j, "of ", matrix.count)) 
        matrix.list.sim[[j]] <- temp.matrix
      }
       return(matrix.list.sim) 
    }
    
################################################################################
##### Apply Simulation Function #######
################################################################################      
    
    ## Specify the data (again)
    dat.sim <- dat[, c(numeric.conv, fact.conv)]
    dat.sim <- dat.sim[,-c(2186, 2187, 2188, 2189, 2280)]
    
    ## Use function to generate visually similar synthetic data
    test.list <- vis.sim(data = dat.sim,
                         matrix.count = 100, 
                         sim.n = 852,
                         numeric.limit = 430)

################################################################################
##### Looped Simulation #######
################################################################################      
    
    ## Specify the data (again)
    dat.sim <- dat[, c(numeric.conv, fact.conv)]
    dat.sim <- dat.sim[,-c(2186, 2187, 2188, 2189, 2280)]
    
    ## Specify simulation sizes
    multi.sim.n <- c(50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800, 852, 
                     900, 1000, 1250, 1500, 2000, 3000, 4000, 5000)
    ## Generate Empty list of lists
    list.layered <- vector(mode = "list", length = length(multi.sim.n))
    
    ## Populate lists of lists with synthetic data
    for(k in 1:length(multi.sim.n)){
      list.layered[[k]] <- vis.sim(data = dat.sim,
                                  matrix.count = 100,
                                  sim.n = multi.sim.n[k],
                                  numeric.limit = 430)
      print( paste("Storing list of matrices: ", k, "of ", length(multi.sim.n))) 
    }
    
    ## Note: This proof of concept works,
    ## However this is likely to be more effective by chunking the method into smaller sets 
    ## As this will reduce the computational power and reduce the risk of time-out errors
    
    
    ## Specify simulated size groups 
    multi.sim.npa <- c(50, 100, 150, 200, 250, 300)
    multi.sim.npb <- c(400, 500, 600, 700, 800, 852)
    multi.sim.npc <- c(900, 1000, 1250)
    multi.sim.npd <- c(1500, 2000, 3000)
    multi.sim.npe <- c(4000, 5000)
    multi.sim.npf <- c(10000)
    
    ## Specify matrix counts
    matrix.count = 100
    
    ## Generate empty vectors for empty vectors 
    list.layered.1 <- vector(mode = "list", length = length(multi.sim.npa))
    list.layered.2 <- vector(mode = "list", length = length(multi.sim.npb))
    list.layered.3 <- vector(mode = "list", length = length(multi.sim.npc))
    list.layered.4 <- vector(mode = "list", length = length(multi.sim.npd))
    list.layered.5 <- vector(mode = "list", length = length(multi.sim.npe))
    list.layered.6 <- vector(mode = "list", length = length(multi.sim.npf))
    
    ## Loop NPA
      for(k in 1:length(multi.sim.npa)){
        list.layered.1[[k]] <- vis.sim(data = dat.sim,
                                     matrix.count = matrix.count,
                                     sim.n = multi.sim.npa[k],
                                     numeric.limit = 430)
        print( paste("Storing list of matrices: ", k, "of ", length(multi.sim.npa))) 
      }
    ## Loop Save to .csv
      for(p in 1:matrix.count){
        for(q in 1:length(multi.sim.npa)){
          write.csv(list.layered.1[[q]][[p]], 
                    file = paste("data/vis_sim/group1/vis_sim.fileno.", q, ".iteration.", p, ".npa.csv",
                                 sep = ""))
        }
      }
    
    
    ## Loop NPB
      for(k in 1:length(multi.sim.npb)){
        list.layered.2[[k]] <- vis.sim(data = dat.sim,
                                     matrix.count = matrix.count,
                                     sim.n = multi.sim.npb[k],
                                     numeric.limit = 430)
        print( paste("Storing list of matrices: ", k, "of ", length(multi.sim.npb))) 
      }
    ## Loop Save to .csv
    for(p in 1:matrix.count){
      for(q in 1:length(multi.sim.npb)){
        write.csv(list.layered.2[[q]][[p]], 
                  file = paste("data/vis_sim/group1/vis_sim.fileno.", q, ".iteration.", p, ".npb.csv",
                               sep = ""))
      }
    }
    
    
    ## Loop NPC
    for(k in 1:length(multi.sim.npc)){
      list.layered.3[[k]] <- vis.sim(data = dat.sim,
                                   matrix.count = matrix.count,
                                   sim.n = multi.sim.npc[k],
                                   numeric.limit = 430)
      print( paste("Storing list of matrices: ", k, "of ", length(multi.sim.npc))) 
    }
    ## Loop Save to .csv
    for(p in 1:matrix.count){
      for(q in 1:length(multi.sim.npc)){
        write.csv(list.layered.3[[q]][[p]], 
                  file = paste("data/vis_sim/group1/vis_sim.fileno.", q, ".iteration.", p, ".npc.csv",
                               sep = ""))
      }
    }
    
    
    ## Loop NPD
    for(k in 1:length(multi.sim.npd)){
      list.layered.4[[k]] <- vis.sim(data = dat.sim,
                                   matrix.count = matrix.count,
                                   sim.n = multi.sim.npd[k],
                                   numeric.limit = 430)
      print( paste("Storing list of matrices: ", k, "of ", length(multi.sim.npd))) 
    }
    ## Loop Save to .csv
    for(p in 1:matrix.count){
      for(q in 1:length(multi.sim.npd)){
        write.csv(list.layered.4[[q]][[p]], 
                  file = paste("data/vis_sim/group1/vis_sim.fileno.", q, ".iteration.", p, ".npd.csv",
                               sep = ""))
      }
    }
    
    
    ## Loop NPE
    for(k in 1:length(multi.sim.npe)){
      list.layered.5[[k]] <- vis.sim(data = dat.sim,
                                   matrix.count = matrix.count,
                                   sim.n = multi.sim.npe[k],
                                   numeric.limit = 430)
      print( paste("Storing list of matrices: ", k, "of ", length(multi.sim.npe))) 
    }
    ## Loop Save to .csv
    for(p in 1:matrix.count){
      for(q in 1:length(multi.sim.npe)){
        write.csv(list.layered.5[[q]][[p]], 
                  file = paste("data/vis_sim/group1/vis_sim.fileno.", q, ".iteration.", p, ".npe.csv",
                               sep = ""))
      }
    }

    
    ## Loop NPF
    for(k in 1:length(multi.sim.npf)){
      list.layered.6[[k]] <- vis.sim(data = dat.sim,
                                     matrix.count = matrix.count,
                                     sim.n = multi.sim.npf[k],
                                     numeric.limit = 430)
      print( paste("Storing list of matrices: ", k, "of ", length(multi.sim.npf))) 
    }
    ## Loop Save to .csv
    for(p in 1:matrix.count){
      for(q in 1:length(multi.sim.npf)){
        write.csv(list.layered.6[[q]][[p]], 
                  file = paste("data/vis_sim/group1/vis_sim.fileno.", q, ".iteration.", p, ".npf.csv",
                               sep = ""))
      }
    }
    
    
    
    
  
