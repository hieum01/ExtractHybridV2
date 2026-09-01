#' Extract_Hybrid_V2_Points
#'
#' @description
#' Extracting Hybrid V2 at specific points (extracting from the nearest grid cells)
#'
#' @references
#' Eum, H.-I., Gupta, A., 2019. Hybrid climate datasets from a climate data evaluation system and their impacts on hydrologic simulations for the Athabasca River basin in Canada. Hydrology and Earth System Sciences 23, 5151–5173. https://doi.org/10.5194/hess-23-5151-2019
#'
#' Eum, H.-I., Fajard, B., Tang, T., Gupta, A., 2023. Potential changes in climate indices in Alberta under projected global warming of 1.5–5 °C. Journal of Hydrology: Regional Studies 47, 101390. https://doi.org/10.1016/j.ejrh.2023.101390
#'
#' @param List.Points A array or matrix of points:  Lon and Lat in degrees
#' @param Project.Name Basin (or Project) name
#' @param Path.Hybrid String of a folder path that hybrid V2 data (RData format) are available
#' @param Target.Met.Var Meteorological variables to be extracted, data(Met.Var.Name). Ex: c('pr', 'tmin', 'tmax', 'windspeed', 'relhumid')
#' @param Start.Year Start year to extract the hybrid climate dataset (default=1950)
#' @param End.Year Ending year to extract the hybrid climate dataset(default=2019)
#' @param Output.Path Output path to store outputs (e.g., Output)
#' @param File.Format File format of extracted data; 1:RDS (default) 2:RData 3: csv
#'
#' @return Location data (Lat, Lon and Elevation), Meteorological variables within target domain in rds format
#' @export
#'
#' @examples
#'
Extract_HybridV2_Points<-function(List.Points,Project.Name,Path.Hybrid,
                                       Target.Met.Var=c('pr', 'tasmin', 'tasmax', 'sfcWind', 'hurs','rsds','rlds'),
                                       Start.Year=1950,End.Year=2019,Output.Path,File.Format=1){
  library(raster)
  library(sp)
  library(geosphere)

  data(Grid.Hybrid.V2)
  data(CMIP6.GCMs.Info)
  NVar<-length(Target.Met.Var)

  Output.folder<-paste0(Output.Path,'/',Project.Name)
  if(!file.exists(Output.folder)) {dir.create(Output.folder)}
  #---------------------------------------------------------------
  # Hybrid grid information

  N.Points<-nrow(List.Points)
  Loc.Point.in.Hybrid<-array(NA,dim=c(N.Points))

  for(iP in 1:N.Points) {
    Lon.Lat.Target<-List.Points[iP,]
    Distance<-as.data.frame(as.numeric(distm(Grid.Hybrid,Lon.Lat.Target)))  # require "geosphere"
    Loc.Nearest.Grid<-which(Distance==min(Distance))
    Loc.Point.in.Hybrid[iP]<-Loc.Nearest.Grid
  }
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
  #===========================================================================
  for (iVar in Target.Met.Var) {
    Path.Hybrid.Var<-paste0(Path.Hybrid,'/Hybrid_V2_Daily_', iVar,'_1950-2024.RData')
    print(paste0("Reading data: ",iVar))
    Hybrid.Var<-get(load(Path.Hybrid.Var))
    Var.name<-iVar
    print(paste0("Extracting data: ",iVar))

    Var.Target<-Hybrid.Var[(Order.Start:Order.End),Loc.Point.in.Hybrid]

    if(Var.name=='qair') {
      Var.Target<-round(Var.Target,6)
    } else {
      Var.Target<-round(Var.Target,3)
    }

    Hybrid.Var_target<-cbind.data.frame(Date.Selected,Var.Target)
    colnames(Hybrid.Var_target)<-c('Date',paste0('Grid_',c(1:length(Loc.Point.in.Hybrid))))

    #------------writing CMIP6 climate scenarios ---------------------------
    if (File.Format==1) {
      saveRDS(Hybrid.Var_target,file=paste0(Output.folder,'/',Basin.Name,'_Hybrid_V2_',iVar,'.rds'))
    } else if (File.Format==2){
      save(Hybrid.Var_target,file=paste0(Output.folder,'/',Basin.Name,'_Hybrid_V2_',iVar,'.RData'))
    } else {
      write.csv(Hybrid.Var_target,file=paste0(Output.folder,'/',Basin.Name,'_Hybrid_V2_',iVar,'.csv'),row.names = F)
    }

  }
}



