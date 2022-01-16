miReverse xs = foldr f a xs
    where
        f x rs = rs ++ [x]
        a = []

map1 f as = foldr g [] as
    where
        g x bs = (f x) : bs

filter1 f as = foldr g [] as
    where
        g x bs = if (f x)
            then (x : bs)
            else bs
-- map, filter, length, ++, 
{- Usando foldr definir:
1. Una funcion que reciba una lista y devuelva el elemento mayor
2. Una funcion que ordene una lista ascendentemente
3. Una funcion que reste dos listas
4. Una funcion que reciba una lista y devuelva verdad, si la lista está ordenada ascendentemente.
-}

mayor xs = foldr f 0 xs
    where
        f x y
            |x > y = x
            |otherwise = y

ordenar [] = []
ordenar xs = (menor xs) : (ordenar (eliminar (menor xs) xs))
    where
        menor as = foldr f 999999 as
            where
                f x y
                    |x < y = x
                    |otherwise = y
        eliminar a bs = filter f bs
            where
                f x = (x > a) || (x < a)

restar xs ys = zipWith (-) xs ys

verificarOrden xs = xs == ordenar xs

f1 x y z u =
    if x (\a -> a > 2)
        then (\a -> \b -> a b)
        else y (z (*)) (u (>))

f2 x y =
    if (x (>2)) < 10
        then 1
        else y h (x (> 2))
                where
                    h z
                        |z < 10 = 'a'
                        |z == 10 = 'b'
                        |otherwise = 'c'

igualarMat xss yss = sum (map g [0..((length xss) - 1)])
    where
        g x = comp (xss !! x) (yss !! x)
        comp as bs = sum (zipWith f as bs)
        f x y = if x == y then 1 else 0

prodEscalar fs cs = foldr f 0 [0..(length cs) - 1]
    where
        f pos r = (fs !! pos) * (cs !! pos) + r

transp fss = foldr f [] [0..(length (fss !! 0)) - 1]
    where
        f pos yss = (doFilaTransp fss pos) : yss

doFilaTransp fss i = foldr f [] fss
    where
        f xs ys = (xs !! i) : ys

mulMat xss yss = foldr f [] xss
    where
        f xs rss = (doFilaMul xs yss) : rss

doFilaMul xs yss = foldr f [] [0..(length (yss !! 0)) - 1]
    where
        f cPos fila = (prodEscalar xs (doFilaTransp yss cPos)) : fila

--Practica

tercerElem xs = xs !! 2

segundoElem xs = xs !! 1

aplicarPrimera xss y = ((xss !! 0) !! 0) y

quintoDeLaTercera xss = (xss !! 2) !! 4

terceroDeLaCuartaDeLaSegunda xss = ((xss !! 2) !! 4) !! 3

verificarFOrden xs f = (f xs) == xs

compararListas xs ys = xs == ys

verificarMatriz xss = and (map f xss)
    where
        f xs = (length xs) == length (xss !! 0)

elementoN n xs = xs !! (n - 1)

myLength xs = foldr f 0 xs
    where
        f x y = y + 1

myFilter f xs = foldr g [] xs
    where
        g x ys =
            if f x then x : ys
            else ys

myZip xs ys = foldr f [] [0..(length xs) - 1]
    where
        f pos rs = (xs !! pos, ys !! pos) : rs

transpuesta xss = foldr f [] [0..(length (xss !! 0)) - 1]
    where
        f pos rs = (filaTranspuesta xss pos) : rs

filaTranspuesta xss idxC = foldr f [] xss
    where
        f xs rs = (xs !! idxC) : rs

multiplicarMatrices xss yss = foldr f [] xss
    where
        f xs rss = (construirFila xs) : rss
        construirFila xs = foldr g [] [0..(length (yss !! 0)) - 1]
            where
                g pos fr = productoEscalar xs (filaTranspuesta yss pos) : fr

productoEscalar v1 v2 = foldr f 0 [0..(length v1) - 1]
    where
        f pos r = ((v1 !! pos) * (v2 !! pos)) + r

multiplicarTresMat xss yss zss = multiplicarMatrices (multiplicarMatrices xss yss) zss

multiplicarCuatroMat xss yss zss wss = multiplicarMatrices (multiplicarTresMat xss yss zss) wss

verificarMatFOrden xss f = (f xss) == xss

soloPares xs = filter even xs

longitudes xss = map length xss

longitudesPares xss = filter even (longitudes xss)

borrarPares xss = foldr f [] xss
    where
        f xs rss = (filter odd xs) : rss

penultimos xss = foldr f [] xss
    where
        f xs rs = (xs !! ((length xs) - 2)) : rs

divisores num = foldr f [] [1..num]
    where
        f x rs = if (num `mod` x) == 0 then x : rs
        else rs

myZipWith f xs ys = map g (zip xs ys)
    where
        g (x,y) = f x y

--Fin Practica

restarMatrices xss yss = foldr f [] [0..(length xss) - 1]
    where
        f pos rss = restarVectores (xss !! pos) (yss !! pos) : rss

restarVectores v1 v2 = foldr f [] [0..(length v1) - 1]
    where
        f pos rs = (v1 !! pos) - (v2 !! pos) : rs

restarMatrices2 xss yss = foldr f [] (zip xss yss)
    where
        f (xs, ys) rss = foldr g [] (zip xs ys) : rss
            where
                g (x,y) rs = x - y : rs

repeticionesCadena ls = (\(xs,ys) -> zip xs ys) (foldr f ([],[]) ls)
    where
        f c (xs, ys) = if elem c xs then (xs,ys) else (c : xs, foldr g 0 ls : ys)
            where
                g xc total =
                    if xc == c then total + 1 else total

quitarRepetidos xs = foldr f [] xs
    where
        f x rs = if elem x rs then rs else x : rs

inc :: Float -> Float
inc x = x + 1.0
f :: Float -> Float -> Float
f x y = x + (4.0 * x )
