import Text.Show (Show)

-- Entrenamiento
data Lista a = Empty | Put a (Lista a) deriving (Show)

data Natural = Cero | Sucesor Natural deriving (Show)

type Cadena = Lista Char

type Posiciones = Lista Natural

type Caracter = Char

miFoldr f v Empty = v
miFoldr f v (Put x xs) = f x (miFoldr f v xs)

miLength Empty = Cero
miLength (Put x xs) = Sucesor (miLength xs)

buscar c cs = snd (miFoldr f (Cero, Empty) cs)
  where
    f c1 (pos, ps) = (Sucesor pos, if c == c1 then Put pos ps else ps)

buscar2 c cs = snd (foldr f (length cs - 1, []) cs)
  where
    f c1 (pos, ps) = (pos - 1, if c == c1 then pos : ps else ps)