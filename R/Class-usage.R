

setClass("usage", contains="expression")
usageClass <- getClass('usage', where = topenv(environment()))
setAs("call", 'usage', function(from)new(as.expression(from), Class=usageClass))
setAs("name", 'usage', function(from)new(as.expression(from), Class=usageClass))
