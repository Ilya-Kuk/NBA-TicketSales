require(ROCR)

rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)
}

fitted = predict(glm.prf,type='response')
rocplot(fitted, post_retail_response)
