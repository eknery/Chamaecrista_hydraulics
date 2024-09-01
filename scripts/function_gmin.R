### w = weight (g)
### t =  time (min)
### mfvpd = vapor pressure deficit (kPa)
### area = moist leaf area (cm2)
gmin = function(w1, w0, t1, t0, mfvpd, area ){
  gmin = -((w1-w0)/18*1000)/((t1-t0)*60)/mfvpd/(area*2/10000)
  return(gmin)
}
