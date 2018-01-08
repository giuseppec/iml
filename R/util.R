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


# returns TRUE if object has predict function
has.predict = function(object){
  classes = class(object)
  any(unlist(lapply(classes, function(x){
    'predict' %in% attr(methods(class = x), 'info')$generic
  })))
}

#test = randomForest(Species ~ ., data = iris)
