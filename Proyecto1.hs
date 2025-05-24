{-- 
    Colaboradores: 
    - Marcel Mejias 30514210 
    - Samantha Ramirez 31307714
--}
{-- 
    Diseñar un programa que permita determinar cuántos litros de cerveza deben agregarse entre los barriles para servir 
    exactamente n vasos de cerveza desde cualquiera de las salidas
    Vaso
    - 1 vaso = 1L cerveza
    - Se sirve desde cualquier barril que cumpla con la cantidad solicitada
    Barril
    - Diferentes capacidades
    - Tienen una salida para servir en el vaso
    - Transferencias entre ellos respetan sus capacidades máximas
    - Posibles transferencias A <-> B <-> C 
    - Posibles agregaciones desde A, C
--}
{--
    Funciones adicionales
--}
-- Devolver 0 si el número es negativo
notNegative :: Int -> Int
notNegative x = max x 0

validatedBarrel :: Barrel -> Barrel
validatedBarrel (maxCapacity, currentAmount) = (notNegative maxCapacity, notNegative currentAmount)

{-- 
    Parte 1: Inicialización de barriles 
    - Recibir los valores de cada barril 
    - Devolver una terna de Barrel
--}
type Barrel = (Int, Int) -- (capacidad máxima, cantidad actual)

initialBarrels :: Barrel -> Barrel -> Barrel -> (Barrel, Barrel, Barrel)
initialBarrels a b c = (validatedBarrel a, validatedBarrel b, validatedBarrel c)

{-- 
    Parte 2: Existe solución 
    - Recibir los tres barriles y el objetivo de vasos de cerveza 
    - Devolver un booleano verificando si el estado actual de los barriles garantiza una posible solución
--}

isSolution :: (Barrel, Barrel, Barrel) -> Int -> Bool
isSolution (a, b, c) objective = maxCurrent >= objective
    where 
        (_, currentA) = a -- Cantidad actual de A
        (_, currentB) = b -- Cantidad actual de B
        (_, currentC) = c -- Cantidad actual de C
        maxCurrent = max currentA (max currentB currentC) -- Máximo entre cantidades actuales

{-- 
Parte 3: Añadir cerveza 
--}

{-- 
Parte 4: Mejor solución 
--}