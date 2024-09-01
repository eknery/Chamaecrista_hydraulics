### w = weight (g)
### t =  time (min)
### mfvpd = vapor pressure deficit (kPa)
### area = moist leaf area (cm2)
gmin = function(dw, dt, mfvpd, area ){
  gmin = -(dw/18*1000)/(dt*60)/mfvpd/(area*2/10000)
  return(gmin)
}
