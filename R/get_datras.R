#' @title get_hh_data
#' 
#' @description Get DATRAS station data. The function is a 
#' wrapper around the ICES DATRAS webservice API 
#' \href{https://datras.ices.dk/WebServices/DATRASWebService.asmx?op=getHHdata}{getHHdata}
#' 
#' @export
#' 
#' @param survey Character, name of the survey
#' @param year Integer
#' @param quarter Integer, values 1 through 4

get_hh_data <- function(survey="NS-IBTS",year=1996,quarter=1)
{
  getproducts.xml <- 
    paste('
    <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
    <getHHdata xmlns="ices.dk.local/DATRAS">
    <survey>',survey,'</survey>
    <year>',year,'</year>
    <quarter>',quarter,'</quarter>
    </getHHdata>
    </soap:Body>
    </soap:Envelope>',sep = "")
  
  
  header.fields <- c('Content-Type' = "text/xml; charset=utf-8",
                     SOAPAction="")   
  reader <- basicTextGatherer()
  header <- basicTextGatherer()
  curlPerform(url = "https://datras.ices.dk/WebServices/DATRASWebService.asmx",
              httpheader = header.fields,
              postfields = getproducts.xml,
              writefunction = reader$update,
              verbose = FALSE)
  # Check the server is not down by insepcting the XML response for internal server error message.
  ##  This should be moved further up
  if(grepl("Internal Server Error", reader$value())) {
    stop("Web service failure: the server seems to be down, please try again later.")
  }
  res_character <- reader$value()
  i1 <- str_locate(res_character,"<soap:Body>")[,2] + 1
  i2 <- str_locate(res_character,"</soap:Body>")[,1] - 1
  
  res_character <- str_sub(res_character,i1,i2)
  
  res_character <- str_replace(res_character,' xmlns=\"ices.dk.local/DATRAS\"','')
  
  res_xml <- xmlRoot(xmlTreeParse(res_character))
  res <- xmlSApply(res_xml[[1]], 
                   function(x) xmlSApply(x,function(x) xmlSApply(x,xmlValue)))
  
  res <- data.frame(t(res),row.names=NULL)
  names(res) <- str_replace_all(names(res),".text","")
  
  res$Quarter <- as.integer(res$Quarter)
  res$SweepLngt <- as.integer(res$SweepLngt)
  res$HaulNo <- as.integer(res$HaulNo)
  res$Year <- as.integer(res$Year)
  res$month <- as.integer(res$month)
  res$Day <- as.integer(res$Day)
  res$TimeShot <- as.integer(res$TimeShot)
  res$Stratum <- as.integer(res$Stratum)
  res$HaulDur <- as.integer(res$HaulDur)
  res$ShootLat <- as.numeric(res$ShootLat)
  res$ShootLong <- as.numeric(res$ShootLong)
  res$HaulLat <- as.numeric(res$HaulLat)
  res$HaulLong <- as.numeric(res$HaulLong)
  res$Depth <- as.integer(res$Depth)
  res$Netopening <- as.numeric(res$Netopening)
  res$Tickler <- as.integer(res$Tickler)
  res$Distance <- as.integer(res$Distance)
  res$Warplngt <- as.integer(res$Warplngt)
  res$Warpdia <- as.integer(res$Warpdia)
  res$WarpDen <- as.integer(res$WarpDen)
  res$DoorSurface <- as.numeric(res$DoorSurface)
  res$DoorWgt <- as.integer(res$DoorWgt)
  res$DoorSpread <- as.integer(res$DoorSpread)
  res$WingSpread <- as.integer(res$WingSpread)
  res$Buoyancy <- as.integer(res$Buoyancy)
  res$KiteDim <- as.numeric(res$KiteDim)
  res$WgtGroundRope <- as.integer(res$WgtGroundRope)
  res$TowDir <- as.integer(res$TowDir)
  res$GroundSpeed <- as.numeric(res$GroundSpeed)
  res$SpeedWater <- as.numeric(res$SpeedWater)
  res$SurCurDir <- as.integer(res$SurCurDir)
  res$SurCurSpeed <- as.numeric(res$SurCurSpeed)
  res$BotCurDir <- as.integer(res$BotCurDir)
  res$BotCurSpeed <- as.numeric(res$BotCurSpeed)
  res$WindDir <- as.integer(res$WindDir)
  res$WindSpeed <- as.integer(res$WindSpeed)
  res$SwellDir <- as.integer(res$SwellDir)
  res$SwellHeight <- as.numeric(res$SwellHeight)
  res$SurTemp <- as.numeric(res$SurTemp)
  res$BotTemp <- as.numeric(res$BotTemp)
  res$SurSal <- as.numeric(res$SurSal)
  res$BotSal <- as.numeric(res$BotSal)
  res$ThClineDepth <- as.integer(res$ThClineDepth)
  res$DateofCalculation <- as.integer(res$DateofCalculation)
  
  return(res)
}


#' @title get_ca_data 
#' 
#' @description Get DATRAS xx data. The function is a 
#' wrapper around the ICES DATRAS webservice API 
#' \href{https://datras.ices.dk/WebServices/DATRASWebService.asmx?op=getCAdata}{getCAdata}
#' 
#' @export
#' 
#' @param survey Character, name of the survey
#' @param year Integer
#' @param quarter Integer, values 1 through 4

get_ca_data <- function(survey="NS-IBTS",year=1996,quarter=1)
{
  getproducts.xml <- 
    paste('
          <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
          <soap:Body>
          <getCAdata xmlns="ices.dk.local/DATRAS">
          <survey>',survey,'</survey>
          <year>',year,'</year>
          <quarter>',quarter,'</quarter>
          </getCAdata>
          </soap:Body>
          </soap:Envelope>',sep = "")
  
  
  header.fields <- c('Content-Type' = "text/xml; charset=utf-8",
                     SOAPAction="")   
  reader <- basicTextGatherer()
  header <- basicTextGatherer()
  curlPerform(url = "https://datras.ices.dk/WebServices/DATRASWebService.asmx",
              httpheader = header.fields,
              postfields = getproducts.xml,
              writefunction = reader$update,
              verbose = FALSE)
  # Check the server is not down by insepcting the XML response for internal server error message.
  ##  This should be moved further up
  if(grepl("Internal Server Error", reader$value())) {
    stop("Web service failure: the server seems to be down, please try again later.")
  }
  res <- reader$value()
  i1 <- str_locate(res,"<soap:Body>")[,2] + 1
  i2 <- str_locate(res,"</soap:Body>")[,1] - 1
  res <- str_sub(res,i1,i2)
  
  res <- str_replace(res,' xmlns=\"ices.dk.local/DATRAS\"','')
  
  res <- xmlRoot(xmlTreeParse(res))
  res <- xmlSApply(res[[1]], 
                   function(x) xmlSApply(x,function(x) xmlSApply(x,xmlValue)))
  
  res <- data.frame(t(res),row.names=NULL)
  names(res) <- str_replace_all(names(res),".text","")
  
  res$Quarter <- as.integer(res$Quarter)
  res$SweepLngt <- as.integer(res$SweepLngt)
  res$HaulNo <- as.integer(res$HaulNo)
  res$Year <- as.integer(res$Year)
  res$SpecCode <- as.integer(res$SpecCode)
  res$LngtClass <- as.integer(res$LngtClass)
  res$Age <- as.integer(res$Age)
  res$NoAtALK <- as.integer(res$NoAtALK)
  res$IndWgt <- as.numeric(res$IndWgt)
  res$DateofCalculation <- as.integer(res$DateofCalculation)
    
  return(res)
}


#' @title get_hl_data
#' 
#' @description Get DATRAS xx data. The function is a 
#' wrapper around the ICES DATRAS webservice API 
#' \href{https://datras.ices.dk/WebServices/DATRASWebService.asmx?op=getCAdata}{getHLdata}
#' 
#' @export
#' 
#' @param survey Character, name of the survey
#' @param year Integer
#' @param quarter Integer, values 1 through 4

get_hl_data <- function(survey="NS-IBTS",year=1996,quarter=1)
{
  getproducts.xml <- 
    paste('
          <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
          <soap:Body>
          <getHLdata xmlns="ices.dk.local/DATRAS">
          <survey>',survey,'</survey>
          <year>',year,'</year>
          <quarter>',quarter,'</quarter>
          </getHLdata>
          </soap:Body>
          </soap:Envelope>',sep = "")
  
  
  header.fields <- c('Content-Type' = "text/xml; charset=utf-8",
                     SOAPAction="")   
  reader <- basicTextGatherer()
  header <- basicTextGatherer()
  curlPerform(url = "https://datras.ices.dk/WebServices/DATRASWebService.asmx",
              httpheader = header.fields,
              postfields = getproducts.xml,
              writefunction = reader$update,
              verbose = FALSE)
  # Check the server is not down by insepcting the XML response for internal server error message.
  ##  This should be moved further up
  if(grepl("Internal Server Error", reader$value())) {
    stop("Web service failure: the server seems to be down, please try again later.")
  }
  r <- reader$value()
  
  # Lets do this brute force - it is faster
  i1 <- str_locate(r,"<getHLdataResult>")[,2] + 1
  i2 <- str_locate(r,"</getHLdataResult>")[,1] - 1
  r <- str_sub(r,i1,i2)
  
  r <- str_replace_all(r,"<Cls_DatrasExchange_HL>","")
  r <- str_replace_all(r,"</Cls_DatrasExchange_HL>","\n")
  
  cn <- c("Survey","Quarter","Ship","Gear","SweepLngt","GearExp","DoorType","StNo",
          "HaulNo","Year","SpecCodeType","SpecCode","SpecVal","Sex","TotalNo",
          "CatIdentifier","NoMeas","SubFactor","SubWgt","CatCatchWgt","LngtCode",
          "LngtClass","HLNoAtLngt","DateofCalculation","Valid_Aphia")
  for (i in 1:length(cn)) {
    r <- str_replace_all(r,paste("<",cn[i],">",sep=""),"")
    r <- str_replace_all(r,paste("</",cn[i],">",sep=""),"\t")
  }
  
  r <- str_replace_all(r," ","")
  r <- str_replace_all(r,"\t-9\t","\tNA\t")
  
  # find a way to write this a tempfile
  write(r,"tmp.dat")
  r <- read.table("tmp.dat",sep="\t",header=F,fill=T)
  names(r) <- cn
  r <- r[,cn]
  return(r)
  
}