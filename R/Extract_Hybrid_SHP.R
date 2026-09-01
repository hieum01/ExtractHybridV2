#' Extract_SHP
#'
#' @description
#' Extracting the hybrid V2 climate dataset within a boundary of a shape file
#' The boundary information is obtained from a shape file
#'
#' @references
#' Eum, H.-I., Gupta, A., 2019. Hybrid climate datasets from a climate data evaluation system and their impacts on hydrologic simulations for the Athabasca River basin in Canada. Hydrology and Earth System Sciences 23, 5151–5173. https://doi.org/10.5194/hess-23-5151-2019
#'
#' @param Path.SHP Path and file name of a shape file
#' @param Basin.Name Basin (or Project) name
#' @param Path.Hybrid String of a folder path that hybrid V2 data (RData format) are available
#' @param Target.Met.Var Meteorological variables to be extracted, data(Met.Var.Name). Ex: c('pr', 'tmin', 'tmax', 'windspeed', 'relhumid')
#' @param Buffer Buffer in meter (default=5000)
#' @param Start.Year Start year to extract the hybrid climate dataset (default=1950)
#' @param End.Year Ending year to extract the hybrid climate dataset(default=2019)
#' @param Output.Path Output path to store outputs
#' @param File.Format File format of extracted data; 1:RDS (default) 2:RData 3: csv
#'
#' @return Location data (Lat, Lon and Elevation), Precipitation, (Min/Max) Temperature
#' @export
#'
#' @examples Extract_Hybrid_SHP("Input/SSRB.shp","SSRB","Input/PRCP.RData","Input/Tmin.RData","Input/Tmax.RData",
#' Buffer=1.0,Start.Year=1950,End.Year=2019,Output.Path="Output")
Extract_HybridV2_SHP<-function(Path.SHP,Basin.Name,Path.Hybrid,Target.Met.Var,Buffer=5000,Start.Year=1950,End.Year=2019,Output.Path,File.Format=1){

  library(raster)
  library(sp)
  #library(terra)
  data("Grid.Hybrid.V2")
  Output.folder<-paste0(Output.Path,'/',Basin.Name)
  if(!file.exists(Output.folder)) {dir.create(Output.folder)}
  #---------------------------------------------------------------
  # Hybrid grid information
  Hybrid_coord<-cbind.data.frame(Grid.Hybrid.V2$Longitude,Grid.Hybrid.V2$Latitude)
  colnames(Hybrid_coord)<-c("Lon","Lat")
  coordinates(Hybrid_coord) <- ~ Lon + Lat
  projection(Hybrid_coord) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  #-------------------------------------------------------------------------------------------
  # #Reprojection Boundary to Hybrid_coord
  Boundary_SHP<-shapefile(Path.SHP)  #require(raster)
  Boundary_lonlat <- spTransform(Boundary_SHP, crs(Hybrid_coord))
  #Boundary_SHP_st<-st_read(Path.SHP)  #require(raster)
  #Boundary_lonlat <- spTransform(as_Spatial(Boundary_SHP_st$geometry), crs(Hybrid_coord))
  #Hybrid_coord_Buffer<-terra::buffer(Boundary_lonlat,Buffer)
  #projection(Hybrid_coord_Buffer) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

  #---Check if Hybrid grid points are within a boundary
  Check.Boundary.Hybrid<-over(Hybrid_coord,Boundary_lonlat)  #NA: out of SHP boundary
  #A<-Check.Boundary.Hybrid[,1]
  x.grid<-which(!is.na(Check.Boundary.Hybrid))
  #-------------------------------------------------------------------
  # Read hybrid climate data

  bd.Hybrid<-as.Date("1950-01-01")
  ed.Hybrid<-as.Date("2024-12-31")
  Hybrid.Date<-as.Date((seq(bd.Hybrid,ed.Hybrid,by="1 day")))

  bd.Selected<-as.Date(paste(Start.Year,"-01-01",sep=""))
  ed.Selected<-as.Date(paste(End.Year,"-12-31",sep=""))
  Date.Selected<-as.Date((seq(bd.Selected,ed.Selected,by="1 day")))
  Order.Start<-which(Hybrid.Date==bd.Selected)
  Order.End<-which(Hybrid.Date==ed.Selected)

  for (iVar in Target.Met.Var) {
    Path.Hybrid.Var<-paste0(Path.Hybrid,'/Hybrid_V2_Daily_', iVar,'_1950-2024.RData')
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



