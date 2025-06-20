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

-- Validación del barril
validatedBarrel :: Barrel -> Barrel
validatedBarrel (maxCapacity, currentAmount) =
    let validatedMaxCapacity = notNegative maxCapacity
        validatedCurrentAmount = notNegative currentAmount
        realCurrentAmount = min validatedCurrentAmount validatedMaxCapacity
    in (validatedMaxCapacity, realCurrentAmount)

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
-- Máximo entre las capacidades
totalCapacity :: (Barrel, Barrel, Barrel) -> Int
totalCapacity ((maxCapacityA, _), (maxCapacityB, _), (maxCapacityC, _)) = max maxCapacityA (max maxCapacityB maxCapacityC) 

-- Maneja el desbordamiento de B hacia A o C, dependiendo de cuál tenga menor cantidad actual
transferBOverflow :: Int -> Barrel -> Barrel -> (Barrel, Barrel)
transferBOverflow overflow (maxCapacityA, currentAmountA) (maxCapacityC, currentAmountC)
    -- Si la cantidad actual de A es menor o igual que la de C, A recibe el desborde
    | currentAmountA <= currentAmountC =
        let ((_, newCurrentAmountA), _) = addBeer overflow (maxCapacityA, currentAmountA) -- Ignorar la capacidad máxima devuelta
        in ((maxCapacityA, newCurrentAmountA), (maxCapacityC, currentAmountC)) 
    -- Si la cantidad actual de C es menor que la de A, C recibe el desborde
    | otherwise =
        let ((_, newCurrentAmountC), _) = addBeer overflow (maxCapacityC, currentAmountC) -- Ignorar la capacidad máxima devuelta
        in ((maxCapacityA, currentAmountA), (maxCapacityC, newCurrentAmountC))

addAndTransferA :: Int -> (Barrel, Barrel, Barrel) -> (Barrel, Barrel, Barrel)
addAndTransferA amount (a, b, c) =
    let ((maxCapacityA, currentAmountAddedA), overflowA) = addBeer amount a -- Rellena el barril A
        ((maxCapacityB, currentAmountBFromA), overflowBFromAToNeighbors) = addBeer overflowA b -- rellena el barril b
        (finalAFromBOverflow, finalCFromBOverflow) = transferBOverflow overflowBFromAToNeighbors (maxCapacityA, currentAmountAddedA) c -- rellena el barril C de ser necesario
    in (finalAFromBOverflow, (maxCapacityB, currentAmountBFromA), finalCFromBOverflow)

addAndTransferC :: Int -> (Barrel, Barrel, Barrel) -> (Barrel, Barrel, Barrel)
addAndTransferC amount (a, b, c) =
    let ((maxCapacityC, currentAmountAddedC), overflowC) = addBeer amount c -- rellena el barril C
        ((maxCapacityB, currentAmountBFromC), overflowBFromCToNeighbors) = addBeer overflowC b -- rellena el barril B
        (finalAFromBOverflow, finalCFromBOverflow) = transferBOverflow overflowBFromCToNeighbors a (maxCapacityC, currentAmountAddedC) -- Rellena el barril A de ser necesario
    in (finalAFromBOverflow, (maxCapacityB, currentAmountBFromC), finalCFromBOverflow)

findBestSolutionRecursive :: Int -> (Barrel, Barrel, Barrel) -> Int -> (Int, (Barrel, Barrel, Barrel))
findBestSolutionRecursive n initialBarrels currentAmountToAdd
    | currentAmountToAdd > (n + totalCapacity initialBarrels) = (0, initialBarrels) -- Verifica que sea posible tener una solución
    | isSolution (addAndTransferA currentAmountToAdd initialBarrels) n = (currentAmountToAdd, addAndTransferA currentAmountToAdd initialBarrels) -- Le agrega el currentAmount de litros de cerveza al barril A y chequea si es solución
    | isSolution (addAndTransferC currentAmountToAdd initialBarrels) n = (currentAmountToAdd, addAndTransferC currentAmountToAdd initialBarrels) -- Le agrega el currentAmount de litros de cerveza al barril C y chequea si es solución
    | otherwise = findBestSolutionRecursive n initialBarrels (currentAmountToAdd + 1) -- Al no encontrar solución prueba agregando otro litro más

findBestSolution :: Int -> (Barrel, Barrel, Barrel) -> (Int, (Barrel, Barrel, Barrel))
findBestSolution n initialBarrels
    | n > totalCapacity initialBarrels = (0, initialBarrels) -- Verifica que sea posible tener una solución
    | otherwise = findBestSolutionRecursive n initialBarrels 0 -- Si existe una solución llama a la función recursiva
