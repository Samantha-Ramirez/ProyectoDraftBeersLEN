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
    - Diferentes capacidades máximas
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
initialBarrels a b c = (validatedBarrel a, validatedBarrel b, validatedBarrel c) -- Validación de negativo

{-- 
    Parte 2: Existe solución 
    - Recibir los tres barriles y el objetivo de vasos de cerveza 
    - Devolver un booleano verificando si el estado actual de los barriles garantiza una posible solución
--}
isSolution :: (Barrel, Barrel, Barrel) -> Int -> Bool
isSolution (a, b, c) objective = maxCurrent >= objective
    where 
        (_, currentAmountA) = a -- Cantidad actual de A
        (_, currentAmountB) = b -- Cantidad actual de B
        (_, currentAmountC) = c -- Cantidad actual de C
        maxCurrent = max currentAmountA (max currentAmountB currentAmountC) -- Máximo entre cantidades actuales

{-- 
    Parte 3: Añadir cerveza 
    - Recibir la cantidad de cerveza a añadir y el barril
    - Devolver el barril con la cantidad añadida y la cantidad de cerveza a transferir al barril vecino (si desborda/sobrepasa su capacidad máxima)
--}
addBeer :: Int -> Barrel -> (Barrel, Int)
addBeer amountToAdd (maxCapacity, currentAmount) = ((maxCapacity, realAmount), overflowAmount)
    where
        validatedAmountToAdd = notNegative amountToAdd -- Validación de negativo
        fakeAmount = currentAmount + validatedAmountToAdd -- Cantidad ideal a añadir (ignorando la capacidad máxima)
        realAmount = min fakeAmount maxCapacity -- Cantidad real que se permite añadir (usando la capacidad máxima)
        overflowAmount = fakeAmount - realAmount -- Desbordamiento al vecino (la cantidad de cerveza no pudo entrar) si lo hay
        
{-- 
    Parte 4: Mejor solución 
--}
addtoA :: (Barrel, Barrel, Barrel) -> (Barrel, Barrel, Barrel)
addtoA (a, b, c) = (afterPourA, afterPourB, c)
    where
        (afterPourA, overflowA) = addBeer 1 a
        (afterPourB, overflowB) = addBeer overflowA b
        
addtoC :: (Barrel, Barrel, Barrel) -> (Barrel, Barrel, Barrel)
addtoC (a, b, c) = (a, afterPourB, afterPourC)
    where
        (afterPourC, overflowC) = addBeer 1 c
        (afterPourB, overflowB) = addBeer overflowC b


recursiveA :: Int -> Int -> (Barrel, Barrel, Barrel) -> Int -> Maybe(Int, (Barrel, Barrel, Barrel))
recursiveA amountAdded goal (a, b, c) maxRecursive
    | isSolution (a, b, c) goal = Just (amountAdded, (a, b, c))
    | amountAdded >= maxRecursive = Just(0, (a, b, c))
    | otherwise =
        let
            (newA, newB, c)= addtoA (a, b, c)
            nextStep = recursiveA (amountAdded+1) goal (newA, newB, c) (maxRecursive - 1)
        in
            nextStep

recursiveC :: Int -> Int -> (Barrel, Barrel, Barrel) -> Int -> Maybe(Int, (Barrel, Barrel, Barrel))
recursiveC amountAdded goal (a, b, c) maxRecursive
    | isSolution (a, b, c) goal = Just (amountAdded, (a, b, c))
    | amountAdded >= maxRecursive = Just(0, (a, b, c))
    | otherwise =
        let
            (a, newB, newC)= addtoC (a, b, c)
            nextStep = recursiveC (amountAdded+1) goal (a, newB, newC) (maxRecursive - 1)
        in
            nextStep

amountCompare ::Maybe(Int, a) -> Int
amountCompare (Just (amountAdded, _)) = amountAdded

findBestSolution :: Int -> (Barrel, Barrel, Barrel) -> Maybe(Int, (Barrel, Barrel, Barrel))
findBestSolution goal (a, b, c) =
    let
        resultA = recursiveA 0 goal (a, b, c) 100
        resultC = recursiveC 0 goal (a, b, c) 100
        amountAddedA = amountCompare resultA 
        amountAddedC = amountCompare resultC 
    in
        if amountAddedA <= amountAddedC
            then resultA 
            else resultC 
