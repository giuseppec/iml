get.feature.type = function(feature.class){
  assertCharacter(feature.class)
  
  feature.types = c(
    'numeric'='numerical', 
    'integer'='numerical', 
    'character'='categorical',
    'factor'='categorical',
    'ordered'='categorical'
    )
  
  stopifnot(all(feature.class %in% names(feature.types)))
  feature.types[feature.class]
}



summary.Experiment = function(obj, ...){
  obj$summary()
}