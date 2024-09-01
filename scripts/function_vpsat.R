### 'temp' is temperature in Celsius
vpsat = function(temp){
  vpsat = exp(52.57633-6790.49851/(temp+273.15)-5.028081*log(temp+273.15) )
  return(vpsat)
}

