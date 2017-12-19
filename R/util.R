get.feature.type = function(feature){
  assertVector(feature)
  cl = class(feature)
  if(cl %in% c('numeric', 'integer')){
    'numerical'
  } else if (cl %in% c('character', 'factor', 'ordered')) {
    'categorical'
  } else {
    stop('feature of class %s not supported', cl)
  }
}