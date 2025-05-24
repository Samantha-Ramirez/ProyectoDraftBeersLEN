-- Caso de prueba Parte 1
barrelA :: Barrel
barrelA = (10, 10)

barrelB :: Barrel
barrelB = (7, 0)

barrilC :: Barrel
barrilC = (3, 0)

testResult :: (Barrel, Barrel, Barrel)
testResult = initialBarrels barrelA barrelB barrilC
-- > testResult
-- ((10,10),(7,0),(3,0))

-- Caso de prueba Parte 2
testCase1 :: Bool
testCase1 = isSolution ((10, 1), (7, 3), (2, 2)) 2
-- > testCase1
-- True

-- Caso de prueba 2:
testCase2 :: Bool
testCase2 = isSolution ((3, 3), (5, 2), (7, 1)) 5
-- > testCase2
-- False