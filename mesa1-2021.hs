import Language.Haskell.TH (Lit (StringL))
import Language.Haskell.TH.Syntax (Lit (StringL))

data Lista a = Vacia | Anadir a (Lista a) deriving (Show)

data Natural = Cero | Proximo Natural deriving (Show)

conv :: String -> Lista Char
conv ys = foldr f a ys
  where
    f b bs
      | b == 'a' || b == 'e' || b == 'i' || b == 'o' || b == 'u' = Anadir b (Anadir b bs)
      | otherwise = Anadir b bs
    a = Vacia

transforma Cero = ""
transforma (Proximo n) = '*' : transforma n

convierte :: [Natural] -> [String]
convierte = map transforma