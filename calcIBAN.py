#coding: utf-8

"""
iban.py -  ProInf.net 2014-01-24

IBAN

Es una estándar de homogeneización bancaria,
creada por el Comité Europeo de Estándares Bancarios (ECSB).
Está regulado en las normas ISO 13616 y EBS204.
Su formato puede variar teniendo un máximo de 34 caracteres,
pudiendo ser tanto números como letras.
En España, está formado por 24 caracteres.

Su composición es la siguiente:
- Primeros dos dígitos: código del país según la norma ISO 3166-1
- 2 dígitos de control, calculados según la norma ISO 13616
- BBAN, es el número de cuenta bancaria básica.
En España, se corresponde con el CCC (Código Cuenta Cliente).

Ejemplos de uso:
IBAN.convertir("1234-5678-06-1234567890")      --> "ES68 1234 5678 0612 3456 7890"
IBAN.calcular("1234-5678-??-1234567890")       --> "ES6812345678061234567890" (68 y 06)
IBAN.validar("ES68 1234 5678 0612 3456 7890")  --> true (68)
IBAN.validar("1234-5678-06-1234567890")        --> true (06)
IBAN.formatear("12345678061234567890")         --> "1234-5678-06-1234567890" (guiones)
IBAN.formatear("ES6812345678061234567890")     --> "ES68 1234 5678 0612 3456 7890" (espacios)

Referencias:
http://queaprendemoshoy.com/como-se-interpretan-los-digitos-de-ccc-y-el-iban/
http://www.integrasistemas.es/blog/general/calculo-del-iban/
http://www.lawebdelprogramador.com/foros/Visual_Basic/1409866-Calculo_IBAN.html#i1409890
http://es.ibancalculator.com/bic_und_iban.html
http://www.cnb.cz/miranda2/export/sites/www.cnb.cz/cs/platebni_styk/iban/download/EBS204.pdf

"""

paisOmision = "es"


##################################################
# PUBLISHED

"""

El parámetro número puede ser un CCC o un IBAN
Si es un CCC retorna el IBAN correspondiente
Si es un IBAN lo formatea
Avisa si es un CCC incorrecto o un IBAN incorrecto

Ejemplo1: IBAN.convertir("12345") --> "Error: No es IBAN ni CCC"
Ejemplo2: IBAN.convertir("ES0012345678061234567890") --> "Error: IBAN incorrecto"
Ejemplo3: IBAN.convertir("ES5212345678001234567890") --> "Error: CCC incorrecto"
Ejemplo4: IBAN.convertir("ES6812345678061234567890") --> "ES68 1234 5678 0612 3456 7890"
Ejemplo5: IBAN.convertir("1234-5678-06-1234567890") --> "ES68 1234 5678 0612 3456 7890"

"""

def convertir(numero, pais="es"):
  numero = limpiar(numero)
  iban = numero[-24:]
  ccc = numero[-20:]
  if not esIBAN(numero) and not esCCC(numero):
    return "Error: No es IBAN ni CCC"
  elif esIBAN(numero) and not validarIBAN(iban):
    return "Error: IBAN incorrecto"
  elif not validarCCC(ccc):
    return "Error: CCC incorrecto"
  elif esIBAN(numero):
    return formatearIBAN(iban)
  else:
    return formatearIBAN(calcularIBAN(ccc, pais))


# Ejemplo: IBAN.calcular("1234-5678-??-1234567890") --> "ES6812345678061234567890" (68 y 06)

def calcular(numero, pais="es"):
  numero = limpiar(numero)
  if esCCC(numero):
    dc = numero[8:10]
    if not dc.isdigit():
      numero = calcularCCC(numero)
    return calcularIBAN(numero, pais)
  else:
    return numero


# Ejemplo1: IBAN.validar("ES68 1234 5678 0612 3456 7890") --> True (68)
# Ejemplo2: IBAN.validar("1234-5678-06-1234567890") --> True (06)
def validar(numero):
  numero = limpiar(numero)
  if esIBAN(numero):
    return validarIBAN(numero)
  elif esCCC(numero):
    return validarCCC(numero)
  else:
    return False

# Ejemplo: IBAN.formatear("12345678061234567890") --> "1234-5678-06-1234567890"
# Ejemplo: IBAN.formatear("ES6812345678061234567890") --> "ES68 1234 5678 0612 3456 7890"

def formatear(numero, separador=None):
  numero = limpiar(numero)
  if esIBAN(numero):
    return formatearIBAN(numero, separador)
  elif esCCC(numero):
    return formatearCCC(numero, separador)
  else:
    return ""

##################################################

# HIGH LEVEL


"""
Como se calcula los dígitos de control del IBAN
a) Se añade al final de la BBAN, el código del país
según la norma ISO 3166-1 y dos ceros.
b) Si en el BBAN hay letras, convierte estas letras en números del 10 al 35,
siguiendo el orden del abecedario A=10 y Z=35.
c) Divide el número por 97, y quédate con el resto.
d) Restale a 98 el resto que te quede
e) Ya tenemos los dígitos de control, si la diferencia es menor a 10,
añade un 0 a la izquierda.
"""


# Ejemplo: calcularIBAN("1234-5678-06-1234567890", "es") --> "ES6812345678061234567890"

def calcularIBAN(ccc, pais="es"):
  ccc = limpiar(ccc)
  pais = pais.upper()
  cifras = ccc + valorCifras(pais) + "00"
  resto = modulo(cifras, 97)
  return pais + cerosIzquierda(str(98 - resto), 2) + ccc

# Ejemplo1: validarIBAN("ES00 1234 5678 0612 3456 7890") --> False
# Ejemplo2: validarIBAN("ES68 1234 5678 0612 3456 7890") --> True

def validarIBAN(iban):
  iban = limpiar(iban)
  pais = iban[0:2]
  dc = iban[2:4]
  cifras = iban[4:] + valorCifras(pais) + dc
  resto = modulo(cifras, 97)
  return resto == 1


# Ejemplo1: validarCCC("1234-5678-00-1234567890") --> False
# Ejemplo2: validarCCC("1234-5678-06-1234567890") --> True

def validarCCC(ccc):
  ccc = limpiar(ccc)
  items = formatearCCC(ccc, " ").split()
  dc = str(modulo11(items[0] + items[1])) + str(modulo11(items[3]))
  return dc == items[2]


# Ejemplo: calcularCCC("1234-5678-??-1234567890") --> "12345678061234567890"

def calcularCCC(ccc):
  ccc = limpiar(ccc)
  return ccc[0:8] + calcularDC(ccc) + ccc[10:20]


# Ejemplo: calcularDC("1234-5678-??-1234567890") --> "06"

def calcularDC(ccc):
  ccc = limpiar(ccc)
  items = formatearCCC(ccc, " ").split()
  return str(modulo11(items[0] + items[1])) + str(modulo11(items[3]))


# Ejemplo: formatearCCC("12345678061234567890") --> "1234-5678-06-1234567890"

def formatearCCC(ccc, separador=None):
  ccc = limpiar(ccc)
  if separador == None: separador = "-"
  return ccc[0:4] + separador + ccc[4:8] + separador + ccc[8:10] + separador + ccc[10:20]


# Ejemplo: formatearIBAN("ES6812345678061234567890") --> "ES68 1234 5678 0612 3456 7890"

def formatearIBAN(iban, separador=None):
  iban = limpiar(iban)
  if separador == None: separador = " "
  items = []
  for i in range(6): items.append(iban[i*4: (i+1)*4])
  return separador.join(items)

##################################################

# LOW LEVEL

def esCCC(cifras):
  return len(cifras) == 20


def esIBAN(cifras):
  return len(cifras) == 24


# Ejemplo: limpiar("IBAN1234 5678-90") --> "1234567890"

def limpiar(numero):
  return numero \
  .replace("IBAN", "") \
  .replace(" ", "") \
  .replace("-", "")


# Ejemplo: modulo("12345678061234567890142800", 97) --> 30

def modulo(cifras, divisor):
  """
  El entero más grande en Python es 9.223.372.036.854.775.807 (2**63-1)
  que tiene 19 cifras, de las cuales las 18 últimas pueden tomar cualquier valor.
  El divisor y el resto tendrán 2 cifras. Por lo tanto CUENTA como tope
  puede ser de 16 cifras (18-2) y como mínimo de 1 cifra.
  
  """

  CUENTA, resto, i = 13, 0, 0
  while i < len(cifras):
    dividendo = str(resto) + cifras[i: i+CUENTA]
    resto = int(dividendo) % divisor
    i += CUENTA
  return resto



# Ejemplo1: modulo11("12345678") --> "0"
# Ejemplo2: modulo11("1234567890") --> "6"

def modulo11(cifras):
  modulos = [(2**x)%11 for x in range(10)]
  suma = 0
  cifras = cerosIzquierda(cifras, 10)
  for cifra, modulo in zip(cifras, modulos):
    suma += int(cifra) * modulo
  control = suma % 11
  return control if control < 2 else 11 - control


# Ejemplo: cerosIzquierda("7", 3) --> "007"

def cerosIzquierda(cifras, largo):
  cantidad = largo - len(cifras)
  ceros = "0"*cantidad
  return ceros + cifras


# Ejemplo: valorCifras("es") --> "1428"

def valorCifras(cifras):
  LETRAS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" # A=10, B=11, ... Z=35
  items = []
  for cifra in cifras:
    posicion = LETRAS.find(cifra)
    items.append(str(posicion) if posicion >= 0 else "-")
  return "".join(items)
