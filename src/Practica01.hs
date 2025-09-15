module Practica01 where

--FUNCIONES
valorAbs :: Int -> Int
valorAbs = undefined

esDivisor :: Int -> Int -> Bool
esDivisor = undefined 

cuadratica :: Float -> Float -> Float -> Float -> Float
cuadratica = undefined


sumaFracciones :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumaFracciones = undefined

comparador :: Float -> Float -> Int
comparador = undefined

puntoMedio :: (Float, Float) -> (Float, Float) -> (Float, Float)
puntoMedio = undefined


--RELACIONES
type Rel a b = [(a, b)]

relacionDivisor :: Rel Int Int
relacionDivisor = undefined

relacionSumaEspecial :: Rel Int Int
relacionSumaEspecial = undefined

relacionCongruentesModuloN :: Int -> Rel Int Int
relacionCongruentesModuloN = undefined


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
