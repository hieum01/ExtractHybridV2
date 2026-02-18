#' Extract_Box
#'
#' @description
#' Extracting the hybrid V2 climate dataset within a bounding box
#'
#' @references
#' Eum, H.-I., Gupta, A., 2019. Hybrid climate datasets from a climate data evaluation system and their impacts on hydrologic simulations for the Athabasca River basin in Canada. Hydrology and Earth System Sciences 23, 5151–5173. https://doi.org/10.5194/hess-23-5151-2019
#'
#'
#' @param Box.Points Four points of Lat and Lon ex: c(-120,-110,47,51)
#' @param Basin.Name Basin (or Project) name
#' @param Path.Hybrid String of a folder path that hybrid V2 data (RData format) are available
#' @param Target.Met.Var Meteorological variables to be extracted, data(Met.Var.Name). Ex: c('pr', 'tmin', 'tmax', 'windspeed', 'relhumid')
#' @param Buffer Buffer in decimal degree (default=0.5)
#' @param Start.Year Start year to extract the hybrid climate dataset (default=1950)
#' @param End.Year Ending year to extract the hybrid climate dataset(default=2019)
#' @param Output.Path Output path to store outputs
#' @param File.Format File format of extracted data; 1:RDS (default) 2:RData 3: csv
#'
#' @return Location data (Lat, Lon and Elevation), Precipitation, (Min/Max) Temperature
#' @export
#'
#' @examples Extract_Hybrid_Box(c(-117.0,-110.0,49.0,53.0),"SSRB","Input/PRCP.RData","Input/Tmin.RData","Input/Tmax.RData",
#' Buffer=1.0,Start.Year=1950,End.Year=2019,Output.Path="Output")
Extract_HybridV2_Box<-function(Box.Points,Basin.Name,Path.Hybrid,Target.Met.Var,Buffer=0.5,Start.Year=1950,End.Year=2019,Output.Path,File.Format){

  library(raster)
  library(sp)
  data("Grid.Hybrid.V2")
  Output.folder<-paste0(Output.Path,'/',Basin.Name)
  if(!file.exists(Output.folder)) {dir.create(Output.folder)}
  #---------------------------------------------------------------
  # Hybrid grid information

  Hybrid.xy.list<-list(X=Grid.Hybrid.V2$Longitude ,Y=Grid.Hybrid.V2$Latitude)
  #-------------------------------------------------------------------------------------------
  # Boundary
  X.min<- Box.Points[1]-Buffer
  X.max<- Box.Points[2]+Buffer
  Y.min<-Box.Points[3]-Buffer
  Y.max<-Box.Points[4]+Buffer
  #---Identifiying grid points within a boundary
  x.grid<-which(Hybrid.xy.list$X >= X.min & Hybrid.xy.list$X <= X.max &
                 Hybrid.xy.list$Y>=Y.min & Hybrid.xy.list$Y<=Y.max)

  #-------------------------------------------------------------------
  # Read hybrid climate data

  bd.Hybrid<-as.Date("1950-01-01")
  ed.Hybrid<-as.Date("2019-12-31")
  Hybrid.Date<-as.Date((seq(bd.Hybrid,ed.Hybrid,by="1 day")))

  bd.Selected<-as.Date(paste(Start.Year,"-01-01",sep=""))
  ed.Selected<-as.Date(paste(End.Year,"-12-31",sep=""))
  Date.Selected<-as.Date((seq(bd.Selected,ed.Selected,by="1 day")))
  Order.Start<-which(Hybrid.Date==bd.Selected)
  Order.End<-which(Hybrid.Date==ed.Selected)

  for (iVar in Target.Met.Var) {
    Path.Hybrid.Var<-paste0(Path.Hybrid,'/Hybrid_V2_Daily_', iVar,'_1950-2019.RData')
    print(paste0("Reading data: ",iVar))
    Hybrid.Var<-get(load(Path.Hybrid.Var))
    print(paste0("Extracting data: ",iVar))

    Var.Target<-Hybrid.Var[(Order.Start:Order.End),x.grid]

    if(Var.name=='qair') {
      Var.Target<-round(Var.Target,6)
    } else {
      Var.Target<-round(Var.Target,3)
    }

    Hybrid.Var_target<-cbind.data.frame(Date.Selected,Var.Target)
    colnames(Hybrid.Var_target)<-c('Date',paste0('Grid_',c(1:length(x.grid))))

    print(paste0("Writing data: ", iVar))

    if (File.Format==1) {
      saveRDS(Hybrid.Var_target,file=paste0(Output.folder,'/',Basin.Name,'_Hybrid_V2_',iVar,'.rds'))
    } else if (File.Format==2){
      save(Hybrid.Var_target,file=paste0(Output.folder,'/',Basin.Name,'_Hybrid_V2_',iVar,'.RData'))
    } else {
      write.csv(Hybrid.Var_target,file=paste0(Output.folder,'/',Basin.Name,'_Hybrid_V2_',iVar,'.csv'),row.names = F)
    }

  }

  XY_Grid_Target<-cbind.data.frame(Grid.Hybrid.V2$Longitude[x.grid],Grid.Hybrid.V2$Latitude[x.grid],Grid.Hybrid.V2$Elevation[x.grid])
  colnames(XY_Grid_Target)<-c('Longitude','Latitude','Elevation')
  rownames(XY_Grid_Target)<-paste0('Grid_',c(1:length(x.grid)))
  write.csv(XY_Grid_Target,file=paste0(Output.folder,'/',Basin.Name,'_XY_Points.csv'),row.names = T)
}



