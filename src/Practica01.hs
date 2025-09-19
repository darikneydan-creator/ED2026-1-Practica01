module Practica01 where

--FUNCIONES
valorAbs :: Int -> Int
valorAbs 0 = 0
valorAbs x = if x < 0 then x*(-1) else x

esDivisor :: Int -> Int -> Bool
esDivisor x y = if y `mod` x == 0 then  True else False 

cuadratica :: Float -> Float -> Float -> Float -> Float
cuadratica a b c v =  a*(v*v) + b*v + c


sumaFracciones :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumaFracciones (w,x) (y,z) = if x == z then (w+y,x) else (x*y + w*z, x*z)

comparador :: Float -> Float -> Int
comparador n m = if  n > m then 1 else if m>n then -1 else 0

puntoMedio :: (Float, Float) -> (Float, Float) -> (Float, Float)
puntoMedio (x,z) (y,w)= ((x + y) / 2, (z+w) / 2)


--RELACIONES
--Funciones de apoyo extra
esMultiplo :: Int ->Int -> Bool
esMultiplo n m = n `mod` m == 0

esMenor ::Int->Int->Bool
esMenor a b = a<b

esParREL :: Int->Bool
esParREL x = (x `mod` 2)==0

esImpar :: Int->Bool
esImpar x = (x `mod` 2)/=0

esDiferente:: Int->Int->Bool
esDiferente x y = x/=y

mismaParidad :: Int->Int->Bool
mismaParidad x y = ((esParREL x==True) && (esParREL y==True)) || ((esImpar x==True) && (esImpar y==True))

--Funciones principales
type Rel a b = [(a, b)]

relacionDivisor :: Rel Int Int
relacionDivisor = [(x,y) | x <- [1..30], y <- [1..30], mismaParidad x y, esDivisor x y]

relacionSumaEspecial :: Rel  Int Int
relacionSumaEspecial = [(x,y) | x<-[1..30], y<-[1..30], esMultiplo (x+y) 5, esMenor x y]

relacionCongruentesModuloN :: Int -> Rel Int Int
relacionCongruentesModuloN n = [(x,y) | x<-[1..30], y<-[1..30], ((x `mod` n) == (y `mod` n)), esDiferente x y]


--NATURALES
-- Cero es natural, Suc Cero es natural, Suc Suc Cero es natural, etc.
data Natural = Cero | Suc Natural deriving (Show,Eq) --Esto es para que se muestre y que se puedan comparar

esPar :: Natural -> Bool
esPar Cero = True
esPar (Suc Cero) = False
esPar (Suc (Suc n)) = esPar n

iguales :: Natural -> Natural -> Bool
iguales Cero Cero = True
iguales _ Cero = False
iguales Cero _ = False
iguales (Suc n) (Suc m) = iguales n m

maximo :: Natural -> Natural -> Natural 
maximo Cero m = m
maximo n Cero = n
maximo (Suc n) (Suc m) = (Suc (maximo n m))

potencia :: Natural -> Natural -> Natural
potencia _ Cero = (Suc Cero)
potencia n (Suc m) = multiplicacion (potencia n m) n

multiplicacion :: Natural -> Natural -> Natural
multiplicacion _ Cero = Cero
multiplicacion n (Suc m) = suma (multiplicacion n m) n

suma :: Natural -> Natural -> Natural
suma n Cero = n
suma n (Suc m) = (Suc (suma n m))

