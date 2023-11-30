base_rate_med = 0.71284166 #maximum growth rate in 50th percentile curve
base_rate_lo = 0.64805765 #maximum growth rate in 1st percentile curve
base_rate_hi = 0.740034450 #maximum growth rate in 99th percentile curve

grt.med<- 0.75*base_rate_med #above 75% of max growth is considered possible bloom conditions
grt.lo <- 0.75*base_rate_lo #above 75% of max growth is considered possible bloom conditions
grt.hi <- 0.75*base_rate_hi #above 75% of max growth is considered possible bloom conditions

base_rate =0.7183369 #maximum growth rate observed from Gobler experiments

gr_lo <- array(c(-2.751602e+00,4.507835e-01,-2.886584e-02,9.347239e-04,-1.158503e-05))
gr_med <- array(c(1.497026e+00,-3.486953e-01,2.705047e-02,-7.434991e-04,6.659022e-06))
gr_hi <- array(c(-3.418779e-01,1.019944e-01,-4.722763e-03,1.347631e-04,-1.686465e-06))
grt<- 0.75*base_rate #above 75% of max growth is considered possible bloom conditions

temps = seq(0,40,1)
micro_lo<-gr_lo[1]+(gr_lo[2]*temps)+(gr_lo[3]*(temps^2))+(gr_lo[4]*(temps^3))+(gr_lo[5]*(temps^4))
micro_med<-gr_med[1]+(gr_med[2]*temps)+(gr_med[3]*(temps^2))+(gr_med[4]*(temps^3))+(gr_med[5]*(temps^4))
micro_hi<-gr_hi[1]+(gr_hi[2]*temps)+(gr_hi[3]*(temps^2))+(gr_hi[4]*(temps^3))+(gr_hi[5]*(temps^4))

med_rate_of_change <- gr_med[2]+(2*gr_med[3]*temps)+(3*gr_med[4]*(temps^2))+(4*gr_med[5]*(temps^3))

#diplay point at which 0.75% of max growth occurs
which.min(abs(micro_med-grt.med))
which.min(abs(micro_lo-grt.lo))
which.min(abs(micro_hi-grt.hi))

#specify growth rate at tempturure
plus.gr = gr_med[1]+(gr_med[2]*22)+(gr_med[3]*(22^2))+(gr_med[4]*(22^3))+(gr_med[5]*(22^4))
neg.gr = gr_med[1]+(gr_med[2]*19)+(gr_med[3]*(19^2))+(gr_med[4]*(19^3))+(gr_med[5]*(19^4))

plot(temps,micro_lo)
plot(temps,micro_hi)
plot(temps,micro_med)
plot(temps,med_rate_of_change,xlim=c(10,40),ylim=c(-0.07,0.07))