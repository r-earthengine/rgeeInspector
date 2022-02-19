#' Draw object on Earth Engine
#'
#' Function to draw an Earth Engine object interactively from R
#'
#' @param name name of new object on R.
#' @param import_as type of Earth Engine object.
#' @param properties is a list with properties of EE object.
#' @param color is a color of geometry (hexadecimal).
#' @param zoom is the zoom of the visualization in the display.
#' @param ... optional arguments for Map object.
#'
#' @details import_as
#' \itemize{
#' \item \bold{"A":} An Earth Engine object of type Geometry.
#' \item \bold{"B":} An Earth Engine object of type Feature.
#' \item \bold{"C":} An Earth Engine object of type Feature collection.
#' }
#'
#' @return An Earth Engine object, depends of the import_as, if is of type:
#' \itemize{
#' \item \bold{"A":} return a geometry.
#' \item \bold{"B":} return a feature.
#' \item \bold{"C":} return a feature collection.
#' }
#' @importFrom  rgee sf_as_ee ee_as_sf
#' @importFrom  mapedit editMap
#' @export

ee_draw_geometry <- function(name, import_as = "A",properties = NULL ,color = "red", zoom = 5,...){
  if(import_as == "geo"){
    ee_object <- editMap() %>% sf_as_ee()
    Map$centerObject(ee_object,zoom = zoom)
    ee_output <- Map$addLayer(
      eeObject = ee_object, visParams = list(color = color, width = 8)
    )
  } else if(import_as =="fea"){
    ee_object <- editMap() %>% sf_as_ee()
    Map$centerObject(ee_object,zoom = zoom)
    ee_feature <- ee$Feature(ee_object,properties)
    ee_output <- Map$addLayer(
      eeObject = ee_object, visParams = list(color = color, width = 8)
    )

  } else{
    ee_object <- editMap() %>% sf_as_ee()
    Map$centerObject(ee_object,zoom = zoom)
    ee_feature <- ee$FeatureCollection(ee_object,properties)
    ee_output <- Map$addLayer(
      eeObject = ee_object, visParams = list(color = color, width = 8)
    )
  }

  assign(name,ee_object,envir = .GlobalEnv)
  return(ee_output)
}
