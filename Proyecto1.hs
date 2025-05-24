-- Colaboradores: 
-- Marcel Mejias 30514210 
-- Samantha Ramirez 31307714

-- Parte 1: Inicialización de barriles
type Barrel = (Int, Int) -- (capacidad máxima, cantidad actual)

-- Recibir los valores de cada barril y devolver una terna de Barrel
initialBarrels :: Barrel -> Barrel -> Barrel -> (Barrel, Barrel, Barrel)
initialBarrels a b c = (a, b, c)

-- Parte 2: Existe solución
-- Recibir los tres barriles, el objetivo de cervezas y devuelva un booleano indicando si hay solución
-- Verificar si el estado actual de los barriles garantiza una posible solución
isSolution :: (Barrel, Barrel, Barrel) -> Int -> Bool
isSolution (a, b, c) objective = maxCurrent >= objective
    where 
        (_, currentA) = a -- Cantidad actual de A
        (_, currentB) = b -- Cantidad actual de B
        (_, currentC) = c -- Cantidad actual de C
        maxCurrent = max currentA (max currentB currentC) -- Máximo entre cantidades actuales