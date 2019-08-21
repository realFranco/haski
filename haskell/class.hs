-- example of class

class Car c where
    car :: c -> String

instance Car Bool where
    car x = "To the speedway"

instance Car Char where
    car x = "Category " ++ show x


