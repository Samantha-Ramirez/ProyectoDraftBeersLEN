type Barrel = (Int, Int) -- (capacidad máxima, cantidad actual)
-- Caso de prueba Parte 1
barrelA :: Barrel
barrelA = (10, 10)

barrelB :: Barrel
barrelB = (7, 0)

barrilC :: Barrel
barrilC = (3, 0)

testCase1 :: (Barrel, Barrel, Barrel)
testCase1 = initialBarrels barrelA barrelB barrilC
-- > testCase1
-- ((10,10),(7,0),(3,0))

-- Caso de prueba Parte 2
testCase21 :: Bool
testCase21 = isSolution ((10, 1), (7, 3), (2, 2)) 2
-- > testCase21
-- True

-- Caso de prueba Parte 2
testCase22 :: Bool
testCase22 = isSolution ((3, 3), (5, 2), (7, 1)) 5
-- > testCase22
-- False

-- Caso de prueba Parte 3
testCase31 :: (Barrel, Int)
testCase31 = addBeer 5 (20, 15)
-- > testCase31
-- ((20,20),0)

-- Caso de prueba Parte 3
testCase32 :: (Barrel, Int)
testCase32 = addBeer 5 (4, 3)
-- > testCase32
-- ((4,4),4)

-- Caso de prueba Parte 3
testCase33 :: (Barrel, Int)
testCase33 = addBeer (-10) (20, 5)
-- > testCase33
-- ((20,5),0)

-- Caso de prueba Parte 4
testCase41 :: (Int, (Barrel, Barrel, Barrel))
testCase41 = findBestSolution 10 ((10,5),(7,5),(4,100))
-- > testCase41
-- (5, ((10,10),(7,7),(4,4)))

-- Caso de prueba Parte 4
testCase42 :: (Int, (Barrel, Barrel, Barrel))
testCase42 = findBestSolution 4 ((10, 6), (7, 0), (4, 0))
-- > testCase42
-- (0, ((10, 6), (7, 0), (4, 0)))

-- Caso de prueba Parte 4
testCase43 :: (Int, (Barrel, Barrel, Barrel))
testCase43 = findBestSolution 5 ((10, 3), (7, 0), (4, 0))
-- > testCase43
-- (2, ((10, 5), (7, 0), (4, 0)))

-- Caso de prueba Parte 4
testCase44 :: (Int, (Barrel, Barrel, Barrel))
testCase44 = findBestSolution 6 ((10, 2), (7, 5), (4, 2))
-- > testCase44
-- (3, ((10, 2), (7, 6), (4, 4)))

-- Caso de prueba Parte 4
testCase45 :: (Int, (Barrel, Barrel, Barrel))
testCase45 = findBestSolution 6 ((1, 1), (7, 2), (4, 3))
-- > testCase45
-- (4, ((1, 1), (7, 6), (4, 3)))