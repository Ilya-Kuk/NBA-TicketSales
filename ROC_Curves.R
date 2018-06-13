require(ROCR)

rocplot_p = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)
}

rocplot_n = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tnr", "fnr")
  plot(perf,...)
}

fitted = predict(glm.prr,type='response')
rocplot(fitted, post_retail_response)
rocplot(fitted, na.omit(d)$post_retail_response) #must na.omit transform the data first - only 3271 entries

fitted1 = predict(glm.prr1,type='response')
rocplot(fitted1, na.omit(d)$post_retail_response) #must na.omit transform the data first - only 3271 entries

fitted2 = predict(glm.prr2,type='response')
rocplot(fitted2, na.omit(d)$post_retail_response) #must na.omit transform the data first - only 3271 entries

