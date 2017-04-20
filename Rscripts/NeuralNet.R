library(neuralnet)
data(infert)
head(infert)

nn <- neuralnet(
   case~age+parity+induced+spontaneous,
   data=infert, hidden=2, err.fct="ce", act.fct="logistic",
   linear.output=FALSE)
nn

out <- cbind(nn$covariate,
              nn$net.result[[1]])
dimnames(out) <- list(NULL,
                         c("age","parity","induced",
                             "spontaneous","nn-output"))
head(out)

nn.bp <- neuralnet(
   case~age+parity+induced+spontaneous,
   data=infert, hidden=2, err.fct="ce",
   linear.output=FALSE,
   algorithm="backprop",
   learningrate=0.01)
nn.bp

head(nn$generalized.weights[[1]])
plot(nn)
plot(nn.bp)

## probability to be fertile or unfertile
nn$net.result
neuralnet

## variable influence plots
gwplot(nn, selected.covariate = "age", min = -2.5, max = 5)
gwplot(nn, selected.covariate = "parity", min = -2.5, max = 5)

## cofidence intervals for the weights of the neural network
ci <- confidence.interval(nn,  alpha = 0.05)
