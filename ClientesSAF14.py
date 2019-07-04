import calcIBAN
import csv

r = csv.reader(open('clientesS14.csv'))
lines = [l for l in r]

for i in range(1,len(lines)):
  print i
  if(lines[i][12][0].isdigit()):
    lines[i][12] = calcIBAN.calcular(lines[i][12])
  
writer = csv.writer(open('clientesS14py.csv','wb'))
writer.writerows(lines)
