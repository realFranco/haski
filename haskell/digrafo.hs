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

-- insert, insertBy
-- lista de los verices del grafo
vertices :: Digrafo v -> [v]
vertices v = fst v
        
-- lista de tuplas, con los arcos del grafo
arcos :: Digrafo v -> [(v, v)]

nVertices :: Digrafo v -> Int

nArcos :: Digrafo v -> Int

sucesores :: Eq v => Digrafo v -> v -> [v]

antecesores :: Eq v => Digrafo v -> v -> [v]

gradoSal :: Eq v => Digrafo v -> v -> Int

gradoEnt :: Eq v => Digrafo v -> v -> Int

depthFirstSearch :: Eq v => Digrafo v -> v -> [v]

topologicalSort :: Eq v => Digrafo v -> [v]


-- execute

grafo1 = (G [1..4] suc) where
    suc 1 = [2,3]
    suc 2 = [4]
    suc 3 = [4]
    suc 4 = [] 
