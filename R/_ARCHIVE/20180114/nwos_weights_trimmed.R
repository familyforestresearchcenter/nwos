nwos.weights.trimmed <- function(x, method)
{

}

# w.quant <- quantile(SAMPLE$w_f)
# w.iqr <- w.quant[4] - w.quant[2]
# SAMPLE$w_t <- ifelse(SAMPLE$w_f>w.quant[3] + (1.5*w.iqr), w.quant[3] + (1.5*w.iqr),
#                      ifelse(SAMPLE$w_f<w.quant[3] - (1.5*w.iqr), w.quant[3] - (1.5*w.iqr), SAMPLE$w_f))
# Need to redistribute weights to sum properly
