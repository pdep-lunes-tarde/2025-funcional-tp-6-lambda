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
  | (elem Carne . ingredientes) hamburguesa = hamburguesa {ingredientes = Carne : ingredientes hamburguesa}
  | (elem Pollo . ingredientes) hamburguesa = hamburguesa {ingredientes = Pollo : ingredientes hamburguesa}

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = hamburguesa {ingredientes = ingrediente : ingredientes hamburguesa}

calcularDescuento :: Number -> Number -> Number
calcularDescuento descuento precioBaseHamburguesa = (1 - descuento / 100) * precioBaseHamburguesa

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento descuentos hamburguesa = hamburguesa {precioBase = calcularDescuento descuentos (precioBase hamburguesa)}

calcularPrecio :: Hamburguesa -> Number
calcularPrecio hamburguesa = precioBase hamburguesa + precioIngredientes hamburguesa

pdepBurger :: Number
pdepBurger = descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar $ cuartoDeLibra
-- pdepBurger = descuento 20 . calcularPrecio $ Hamburguesa{precioBase= precioBase cuartoDeLibra, ingredientes= agregarIngrediente Panceta . agrandar . agrandar $ ingredientes cuartoDeLibra}
-- Parte II
dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Carne . agregarIngrediente Cheddar $ Hamburguesa {precioBase = 34, ingredientes = [Pan, Carne, Cheddar, Pan]}

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia = descuento 30 . agregarIngrediente Papas