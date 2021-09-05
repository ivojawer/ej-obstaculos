module Lib where

type Obstaculos = [Coordenada]
type Coordenada = (Int, Int)
x = fst
y = snd
data Eje = X | Y

entryPoint :: Int -> Int -> Int -> Int -> [Int] -> [Int] -> Bool
entryPoint x1 y1 x2 y2 obsX obsY = puedeLlegar (x1,y1) (x2,y2) (zip obsX obsY)

puedeLlegar :: Coordenada -> Coordenada -> Obstaculos -> Bool
puedeLlegar inicio target obstaculos 
    | inicio == target = True 
    | otherwise = any (== target) (todosLosDestinosDesde target 0 obstaculos inicio)

todosLosDestinosDesde :: Coordenada -> Int -> Obstaculos -> Coordenada -> [Coordenada]
todosLosDestinosDesde tope step obstaculos punto 
    | sePaso tope punto = []
    | otherwise = concat
        [
            destinosSinObstaculos, 
            concat.map (todosLosDestinosDesde tope proximoStep obstaculos) $ destinosSinObstaculos
        ]
    where 
        siguienteEnX = siguienteEn X punto step
        siguienteEnY = siguienteEn Y punto step
        proximoStep = step + 1
        destinosSinObstaculos = sinObstaculos [siguienteEnX, siguienteEnY] obstaculos

siguienteEn :: Eje -> Coordenada -> Int -> Coordenada
siguienteEn X coordenada step = (siguienteValor (x coordenada) step, y coordenada)
siguienteEn Y coordenada step = (x coordenada, siguienteValor (y coordenada) step)

siguienteValor valor step = valor + fibonacci step

sinObstaculos :: [Coordenada] -> Obstaculos -> [Coordenada]
sinObstaculos puntos obstaculos = filter (not.(flip elem obstaculos)) puntos

sePaso:: Coordenada -> Coordenada -> Bool
sePaso (xTarget, yTarget) (xActual, yActual) = xActual > xTarget || yActual > yTarget

fibonacci 0 = 1
fibonacci 1 = 1
fibonacci posicion = fibonacci (posicion - 1) + fibonacci (posicion - 2)