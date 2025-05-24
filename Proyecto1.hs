-- Parte 1: Inicialización de barriles

type Barrel = (Int, Int) -- (capacidad máxima, cantidad actual)

-- Recibir los valores de cada barril y devolver una terna de Barrel
initialBarrels :: Barrel -> Barrel -> Barrel -> (Barrel, Barrel, Barrel)
initialBarrels b1 b2 b3 = (b1, b2, b3)