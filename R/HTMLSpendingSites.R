#functions for generating files for various HTML/javascript apps HTML version

HTML.getWorksiteFiles <- function(worksites=CBFishR.BPA::WORKSITES,sf_polygons=CBFishR.BPA::HUC12Polys,hucs_pops=CBFishR.BPA::HUCsPOPs){
  #' Returns unique worksites for Work Site ID, Description, and Lat Lon
  #' Save result and import into QGIS to get HUC_12 attribute
  #' Also returns data frame connecting WSEs to Work Site ID
  #'
  #' @param worksites data frame
  #' @return list of two data frames
  #' \describe{
  #'   \item{Locs}{NA}
  #'   \item{WSE_Sites}{NA}
  #' }
  #' @export
  #' @examples HTML.getWorksiteFiles()
  #'
    require(sf)
    require(dplyr)

    Locs <- worksites %>%
      select(Work.Site.ID,Description,Latitude,Longitude) %>%
      unique() %>%
      filter(!is.na(Work.Site.ID)) %>%
      rename(ID=Work.Site.ID)

    points_sf <- st_as_sf(Locs, coords = c("Longitude", "Latitude"), crs = st_crs(sf_polygons))
    points_with_attributes <- st_join(points_sf, sf_polygons, left = FALSE) %>%
      merge(Locs) %>%
      rename(HUC_12=HUC12) %>%
      mutate(HUC_12=as.numeric(HUC_12))
      merge(hucs_pops) %>%
      rename(Pops=pops)


    wse_sites <- worksites %>%
      group_by(WSE.ID,Work.Site.ID) %>%
      summarise() %>%
      ungroup() %>%
      rename(WSE_ID=WSE.ID,SiteID=Work.Site.ID)

    locs <- data.frame(points_with_attributes) %>%
      select(-geometry)

    return(list(Locs=locs,WSE_Sites=wse_sites))
}

HTML.getWSEFile <- function(projects=CBFishR.BPA::PROJECTS,contracts=CBFishR.BPA::CONTRACTS,we=CBFishR.BPA::WORKELEMENTS,wse){
  #' Process WSE file to convert dollars and percents to numbers and such
  #'
  #'
  #' @param projects
  #' @param contracts
  #' @param we
  #' @param wse
  #' @return list of two data frames
  #' \describe{
  #'   \item{NA}{NA}
  #' }
  #' @export
  #' @examples HTML.getWSEFile()
  #'
  data <- projects %>% select(1,2,9,10) %>% rename(Project.Title=Title)
  data <- merge(data,contracts %>% select(Project.Number,Contract.Number,Contract.Status,Contract.Contractor,Organization.Type,FY.Started,Current.Contract.Value,Final.Contract.Value,Invoiced,De.Obligation.Amount)) %>% rename(Contract_Year=FY.Started,Status=Contract.Status)

  data <- merge(data,wse)

  data <- merge(data,we %>% select(2,1,3) %>% rename(WE.Name=Name,WE_Category=Work.Category))
  data$Current <- unMoney(data$Current.Contract.Value)
  data$Final <- unMoney(data$Final.Contract.Value)
  data$Invoiced <- unMoney(data$Invoiced)
  data$WSE_Budget <- unMoney(data$WSE.Effective.Budget)
  data$Planned.WE.Budget <- unMoney(data$Planned.WE.Budget)
  data$Budget_Frac <- unPct(data$X..of.Total.WSE.Effective.Budget)
  data$Area1 <- setUnspecified(data$Primary.RME.Focal.Area)
  data$Area2 <- setUnspecified(data$Secondary.RME.Focal.Area)
  data$Species1 <- setUnspecified(data$Primary.Focal.Species)
  data$Species2 <- setUnspecified(data$Secondary.Focal.Species)
  data$Type1 <- setUnspecified(data$Primary.RME.Type)
  data$Type2 <- setUnspecified(data$Secondary.RME.Type)

  data <- data %>% rename(Project_Number=Project.Number,Project_Title=Project.Title,Contract_Number=Contract.Number,Contractor=Contract.Contractor,Contractor_Type=Organization.Type,FY_Started=FY.Started,WE_ID=WE.ID,WSE_ID=WSE.ID,WSE_Title=Title,Protocol_Name=Protocol.Name,Protocol_URL=Protocol.Url,Repositories=Data.Repositories..Titles.,Plan=Study.Plan.Name.and.Version,Progress=WSE.Progress,Hatchery_Purpose=Hatchery.Purpose,WE_Name=WE.Name,WSE_Start=WSE.Start,WSE_End=WSE.End,WSE_Planned=Planned.WE.Budget)

  data <- data %>% select(Project_Number,Project_Title,Purpose,Emphasis,Contract_Number,Contractor,Contractor_Type,Contract_Year,Status,Current,Final,Invoiced,WE_Category,WE_ID,WE_Name,WSE_ID,WSE_Title,FY_Started,WSE_Start,WSE_End,Progress,WSE_Planned,WSE_Budget,Budget_Frac,Area1,Area2,Type1,Type2,Species1,Species2,Protocol_Name,Protocol_URL,Plan,Repositories,Hatchery_Purpose)

  map <- CBFishR.BPA::OldSpeciesMapping
  primary <- data.frame()
  secondary <- data.frame()
  for(i in 1:nrow(map)){
    primary <- rbind(primary,data.frame(WSE_ID=data$WSE_ID,Species1=ifelse(regexpr(map$original[i], data$Species1) > 0,map$newname[i],"")))
    secondary <- rbind(secondary,data.frame(WSE_ID=data$WSE_ID,Species2=ifelse(regexpr(map$original[i], data$Species2) > 0,map$newname[i],"")))
  }
  primary <- primary %>% filter(Species1 != "") %>% group_by(WSE_ID) %>% summarise(Species1=paste(Species1,collapse=","))
  secondary <- secondary %>% filter(Species2 != "") %>% group_by(WSE_ID) %>% summarise(Species2=paste(Species2,collapse=","))

  data$Species1 <- NULL
  data$Species2 <- NULL
  data <- merge(data,primary,all.x=T) %>% mutate(Species1=ifelse(Species1=="","Unspecified",Species1))
  data <- merge(data,secondary,all.x=T) %>% mutate(Species2=ifelse(Species2=="","Unspecified",Species2))
  return(data)
}

HTML.getRMESpendingFiles <- function(contracts=CBFishR.BPA::CONTRACTS,wse=CBFishR.BPA::WSE,worksite.files=CBFishR.BPA::HTML.getWorksiteFiles()){
  ovh <- wse %>%
    filter(WE.ID==119 | WE.ID==132 | WE.ID==165) %>%
    mutate(frac=unPct(X..of.Total.WSE.Effective.Budget))%>%
    merge(contracts %>% select(Contract.Number,Current.Contract.Value)) %>%
    group_by(Contract.Number,Current.Contract.Value) %>%
    summarise(overpct=sum(frac)) %>%
    mutate(overhead=overpct*unMoney(Current.Contract.Value)) %>%
    select(Contract.Number,overpct,overhead) %>%
    rename(Contract_Number=Contract.Number,Overhead_Frac=overpct,Overhead=overhead)


  df <- wse %>%
    merge(data.frame(WE.ID=c(70,156,157,158,159,160,161,162,182,183)))

  df <- HTML.getWSEFile(wse=df)

  df <- df %>% merge(ovh,all.x=T) %>%
    mutate(Overhead=ifelse(is.na(Overhead),0,Overhead),Overhead_Frac=ifelse(is.na(Overhead_Frac),0,Overhead_Frac),Overhead_Frac=(Budget_Frac/(1-Overhead_Frac)),Overhead=(Overhead_Frac*Overhead))

  wse_sites <- worksite.files$WSE_Sites %>%
    merge(df %>% select(WSE_ID) %>% unique())

  locs <- worksite.files$Locs %>%
    merge(wse_sites %>% select(SiteID) %>% rename(ID=SiteID)) %>%
    rename(Descriptio=Description)

  return(list(RME.WSE=df,RME.Locs=locs,RME.WSE_Sites=wse_sites))
}

HTML.getHatcherySpendingFiles <- function(contracts=CBFishR.BPA::CONTRACTS,wse=CBFishR.BPA::WSE,worksite.files=CBFishR.BPA::HTML.getWorksiteFiles(),imetrics=CBFishR.BPA::IMETRICS){
  wse.hatch <- wse %>% merge(data.frame(WE.ID=c(60,61,66,100,171,176,187,196)))
  wse.hatch <- HTML.getWSEFile(wse=wse.hatch)

  hatch.imetrics <- wse.hatch %>%
    select(WSE_ID) %>%
    merge(IMETRICS %>% rename(WSE_ID=WSE.ID))

  wse.hatch <- wse.hatch %>%
    merge(hatch.imetrics %>%
            filter(Metric.ID==1410) %>%
            mutate(WSE_Purpose1=ifelse(Actual=="",Planned,Actual)) %>%
            select(WSE_ID,WSE_Purpose1),all.x=T)

  wse.hatch <- wse.hatch %>%
    merge(hatch.imetrics %>% filter(Metric.ID==1489) %>%
            mutate(WSE_Purpose2=ifelse(Actual=="",Planned,Actual)) %>%
            select(WSE_ID,WSE_Purpose2),all.x=T)

  wse.hatch <- wse.hatch %>%
    merge(hatch.imetrics %>%
            filter(Metric.ID==1490) %>%
            mutate(BroodYear=ifelse(Actual=="",Planned,Actual)) %>%
            select(WSE_ID,BroodYear),all.x=T)

  wse.hatch <- wse.hatch %>%
    mutate(WSE_Purpose1=ifelse(is.na(WSE_Purpose1),"NA",WSE_Purpose1),
           WSE_Purpose2=ifelse(is.na(WSE_Purpose2),"NA",WSE_Purpose2),
           BroodYear=ifelse(is.na(BroodYear),"NA",BroodYear))

  hatch.imetrics <- hatch.imetrics %>%
    filter(Metric.ID != 1410,Metric.ID != 1489,Metric.ID != 1490) %>%
    select(WSE_ID,Metric.ID,Metric,Planned,Actual) %>%
    rename(Metric_ID=Metric.ID)

  sites <- worksite.files$WSE_Sites %>%
    merge(wse.hatch %>% select(WSE_ID)) %>%
    merge(worksite.files$Locs %>% rename(SiteID=ID,Descriptio=Description))

  return(list(HATCH.WSE=wse.hatch,HATCH.IMETRICS=hatch.imetrics,HATCH.WSE_Sites=sites))
}
