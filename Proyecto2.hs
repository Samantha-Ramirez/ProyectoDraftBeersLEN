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
    - Determinar la cantidad de cerveza óptima que debe agregarse en los barriles para alcanzar una cantidad específica de vasos de cerveza 
    en uno de los tres barriles
    - Reutilizar las funciones isSolution y addBeer
    - Si ocurre un desborde en el barril B, la cantidad de cerveza a transferir debe ir al barril vecino que tenga menor cantidad
    - Puede llegar a perderse cerveza si la cantidad total de cerveza excede la capacidad total de los barriles.
    - Si no hay solución se retorna (0, (estado inicial))
    - Si no hay mejor solución (no se agrega nada) se retorna (0, (estado inicial))
--}
totalCapacity :: (Barrel, Barrel, Barrel) -> Int
totalCapacity ((maxCapacityA, _), (maxCapacityB, _), (maxCapacityC, _)) = max maxCapacityA (max maxCapacityB maxCapacityC) -- Suma de las capacidades máximas

transferBOverflow :: Int -> Barrel -> Barrel -> (Barrel, Barrel)
transferBOverflow overflow (maxCapacityA, currentAmountA) (maxCapacityC, currentAmountC)
    -- Si la cantidad actual de A es menor o igual que la de C, A recibe el desborde
    | currentAmountA <= currentAmountC = 
        let ((newMaxA, newCurA), _) = addBeer overflow (maxCapacityA, currentAmountA)
        in ((newMaxA, newCurA), (maxCapacityC, currentAmountC))
    -- Si la cantidad actual de C es menor que la de A, C recibe el desborde
    | currentAmountC < currentAmountA = 
        let ((newMaxC, newCurC), _) = addBeer overflow (maxCapacityC, currentAmountC)
        in ((maxCapacityA, currentAmountA), (newMaxC, newCurC))

addAndTransferA :: Int -> (Barrel, Barrel, Barrel) -> (Barrel, Barrel, Barrel)
addAndTransferA amount (a, b, c) =
    let ((maxCapacityA, currentAmountAddedA), overflowA) = addBeer amount a
        ((maxCapacityB, curB_from_A), overflowB_from_A_to_Neighbors) = addBeer overflowA b
        (finalA_from_B_overflow, finalC_from_B_overflow) = transferBOverflow overflowB_from_A_to_Neighbors (maxCapacityA, currentAmountAddedA) c
    in (finalA_from_B_overflow, (maxCapacityB, curB_from_A), finalC_from_B_overflow)

addAndTransferB :: Int -> (Barrel, Barrel, Barrel) -> (Barrel, Barrel, Barrel)
addAndTransferB amount (a, b, c) =
    let ((maxCapacityB, currentAmountAddedB), overflowB_to_Neighbors) = addBeer amount b
        (finalA, finalC) = transferBOverflow overflowB_to_Neighbors a c
    in (finalA, (maxCapacityB, currentAmountAddedB), finalC)

addAndTransferC :: Int -> (Barrel, Barrel, Barrel) -> (Barrel, Barrel, Barrel)
addAndTransferC amount (a, b, c) =
    let ((maxCapacityC, currentAmountAddedC), overflowC) = addBeer amount c
        ((maxCapacityB, curB_from_C), overflowB_from_C_to_Neighbors) = addBeer overflowC b
        (finalA_from_B_overflow, finalC_from_B_overflow) = transferBOverflow overflowB_from_C_to_Neighbors a (maxCapacityC, currentAmountAddedC)
    in (finalA_from_B_overflow, (maxCapacityB, curB_from_C), finalC_from_B_overflow)

findBestSolutionRecursive :: Int -> (Barrel, Barrel, Barrel) -> Int -> (Int, (Barrel, Barrel, Barrel))
findBestSolutionRecursive n initialBarrels currentAmountToAdd
    | currentAmountToAdd > (n + totalCapacity initialBarrels) = (0, initialBarrels)
    | isSolution (addAndTransferA currentAmountToAdd initialBarrels) n = (currentAmountToAdd, addAndTransferA currentAmountToAdd initialBarrels)
    | isSolution (addAndTransferC currentAmountToAdd initialBarrels) n = (currentAmountToAdd, addAndTransferC currentAmountToAdd initialBarrels)
    | otherwise = findBestSolutionRecursive n initialBarrels (currentAmountToAdd + 1)

findBestSolution :: Int -> (Barrel, Barrel, Barrel) -> (Int, (Barrel, Barrel, Barrel))
findBestSolution n initialBarrels
    | n > totalCapacity initialBarrels = (0, initialBarrels)
    | otherwise = findBestSolutionRecursive n initialBarrels 0