module Library where

import PdePreludat

data Ingrediente
  = Carne
  | Pan
  | Panceta
  | Cheddar
  | Pollo
  | Curry
  | QuesoDeAlmendras
  | Papas
  | PatiVegano
  | BaconDeTofu
  | PanIntegral
  deriving (Eq, Show)

precioIngrediente :: Ingrediente -> Number
precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo = 10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3

data Hamburguesa = Hamburguesa
  { precioBase :: Number,
    ingredientes :: [Ingrediente]
  }
  deriving (Eq, Show)

-- Parte I

precioIngredientes :: Hamburguesa -> Number
precioIngredientes = sum . map precioIngrediente . ingredientes

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa {precioBase = 20, ingredientes = [Pan, Carne, Cheddar, Pan]}

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa
  | (elem PatiVegano . ingredientes) hamburguesa = hamburguesa {ingredientes = PatiVegano : ingredientes hamburguesa}
  | (elem Carne . ingredientes) hamburguesa = hamburguesa {ingredientes = Carne : ingredientes hamburguesa}
  | (elem Pollo . ingredientes) hamburguesa = hamburguesa {ingredientes = Pollo : ingredientes hamburguesa}
  | otherwise = hamburguesa

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = hamburguesa {ingredientes = ingrediente : ingredientes hamburguesa}

calcularDescuento :: Number -> Number -> Number
calcularDescuento descuento precioBaseHamburguesa = (1 - descuento / 100) * precioBaseHamburguesa

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento descuentos hamburguesa = hamburguesa {precioBase = calcularDescuento descuentos (precioBase hamburguesa)}

calcularPrecio :: Hamburguesa -> Number
calcularPrecio hamburguesa = precioBase hamburguesa + precioIngredientes hamburguesa

pdepBurger :: Hamburguesa
pdepBurger = descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar $ cuartoDeLibra

precioPdeP :: Number
precioPdeP = calcularPrecio pdepBurger

-- Parte II

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Carne . agregarIngrediente Cheddar $ cuartoDeLibra

precioDobleCuarto :: Number
precioDobleCuarto = calcularPrecio dobleCuarto

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

precioBigPdep :: Number
precioBigPdep = calcularPrecio bigPdep

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = descuento 30 . agregarIngrediente Papas $ hamburguesa

precioDeldia :: Hamburguesa -> Number
precioDeldia hamburguesa = calcularPrecio (delDia hamburguesa)

-- Parte III
reemplazarVeg :: Ingrediente -> Ingrediente
reemplazarVeg Carne = PatiVegano
reemplazarVeg Pollo = PatiVegano
reemplazarVeg Cheddar = QuesoDeAlmendras
reemplazarVeg Panceta = BaconDeTofu
reemplazarVeg otro = otro

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa = hamburguesa {ingredientes = map reemplazarVeg (ingredientes hamburguesa)}

reemplazarPanIntegral :: Ingrediente -> Ingrediente
reemplazarPanIntegral Pan = PanIntegral
reemplazarPanIntegral otro = otro

reemplazoPan :: Hamburguesa -> Hamburguesa
reemplazoPan hamburguesa = hamburguesa {ingredientes = map reemplazarPanIntegral (ingredientes hamburguesa)}

dobleCuartoVegano :: Hamburguesa -> Hamburguesa
dobleCuartoVegano = reemplazoPan . hacerVeggie