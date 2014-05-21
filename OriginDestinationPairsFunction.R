## a function that takes a csv of origin coordinates and destination coordinates
## and returns a xlsx of the distances and durations for travelling between.
## origin start and origin stop parameters control where to start and stop
## among the origin csv values. Make sure to set working directory to file where you
## want to store resulting xlsx.


origin.destination.pairs("FarmLocations.csv", "LewistonMcCoy.csv", 301, 350)


origin.destination.pairs = function(originCSV, destinationCSV, origin.start, origin.stop){
  
  require(XML)
  require(RCurl)
  require(data.table)
  require(xlsx)
  
  ### this block reads in CSV
  
  origins.data = read.csv(originCSV)
  destinations.data = read.csv(destinationCSV)
  
  ### this block converts the destinations into API friendly format
  
  destinations.size = length(destinations.data[,1])
  data.vec = c()
  for (i in 1:destinations.size){
    data.vec[i] = paste(as.vector(destinations.data[i,])[1],as.vector(destinations.data[i,])[2], sep = ",")
  }
  destination = paste(as.list(data.vec), collapse = "|")
  
  ### this block breaks it up into chunks that the API can handle, cutting it off at the limit
  
  chunk.size = 100%/%destinations.size
  sample.size = (origin.stop+1)-origin.start
  n.chunk = sample.size%/%chunk.size
  chunks = list()
  for (i in 1:n.chunk){
    chunks[[i]] = origins.data[((i-1)*(chunk.size) + origin.start):(i*chunk.size + origin.start - 1),]
  }
  
  ### this loops over the chunks, pausing for 10 secs between each chunk
  m.list = list()
  
  for (c in 1:n.chunk){
    
    ### this block puts it into the format the API needs
    
    data.vec = c()
    for (j in 1:chunk.size){
      data.vec[j] = paste(as.vector(chunks[[c]][j,])[1],as.vector(chunks[[c]][j,])[2], sep = ",")
    }
    origin = paste(as.list(data.vec), collapse = "|")
    
    ### this block communicates with the API and get the request
    
    xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=driving&sensor=false')
    xmlfile <- xmlParse(getURL(xml.url))
    
    ### this block transforms the request into usable info
    
    dist.matrix = matrix(NA, nrow = chunk.size, ncol = destinations.size)
    dur.matrix = matrix(NA, nrow = chunk.size, ncol = destinations.size)
    final.dest.dist = c()
    final.dest.dur = c()
    for (l in 1:chunk.size){
      for (k in 1:destinations.size){
        dist.matrix[l,k] <- as.numeric(xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[(l-1)*destinations.size + k]])$value))
        dur.matrix[l,k] <- as.numeric(xmlValue(xmlChildren(xpathApply(xmlfile,"//duration")[[(l-1)*destinations.size + k]])$value))
      }
      final.dest.dist[l] = which(dist.matrix[l,]==min(dist.matrix[l,]))
      final.dest.dur[l] = which(dur.matrix[l,]==min(dur.matrix[l,]))
    }
    m.list[[c]] = data.table(dist.matrix, final.dest.dist, dur.matrix, final.dest.dur)
    
    Sys.sleep(10.1)
    
  }
  
  ### this block returns info as a data.table
  
  final.data = rbindlist(m.list)
  write.xlsx(final.data, file = paste("Origins",origin.start, "to", origin.stop,".xlsx", sep = ''))
  return(final.data)
}
