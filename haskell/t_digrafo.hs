-- dev: f97gp1@gmail.com
-- 19*08*19
-- Haskell, Estructura de Datos, Digrafo
--

-- every method to be exported

module Digrafo (Digrafo(G),
                vertices,
                arcos,
                nVertices,
                nArcos,
                sucesores,
                antecesores,
                gradoSal,
                gradoEnt,
                depthFirstSearch,
                topologicalSort) where
          

data Digrafo v = G [v] (v -> [v])
-- debo definir algo acá?

vertices :: Digrafo v -> [v]
vertices v
    | v == 1 = [1,1,1,1]
    | otherwise = [0,0,0,0]

-- Section to be executed

grafo1 = (G [1..4] suc) where
    suc 1 = [2,3]       -- ¿este bicho llamada a 'vertices'?

