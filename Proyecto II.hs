---- PROYECTO 2 ----

---Ejercicio 1]

--a)
data Carrera = Matematica | Fisica | Computacion | Astronomia | Profesorado
                deriving (Show,Eq)
--b)
titulo :: Carrera -> String

titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Ciencias de la Computacion"
titulo Astronomia = "Licenciatura en Astronomia"
titulo Profesorado = "Profesorado"

--c)
--No se puede definir por casos ya que la función carrera no lo admite--

---Ejercicio 2]

--a)
type Ingreso = Int

data Funcion = Teorico | Practico
               deriving (Show,Eq)

data Rol = Decanx
		  | Docente Funcion
		  | Estudiante Carrera Ingreso
		   deriving (Show,Eq)
--b)
--Docente es del tipo Docente :: Funcion -> Rol
--c)
cuantos_doc :: [Rol] -> Funcion -> Int
cuantos_doc [] _ = 0
cuantos_doc (x:xs) c | x == Docente c = 1 + cuantos_doc xs c
					 | otherwise = cuantos_doc xs c
--d)
cuantos_doc' :: [Rol] -> Funcion -> Int
cuantos_doc' xs c = length (filter (==Docente c) xs)

-- En esta fucnción, se separan con filter todos aquellos elementos iguales a Docente con la funcion c y luego pasa a contar con length la cantidad de elementos que se han filtrado.

--e)
--se añade el argumento de tipo genero
data Genero =  Hombre | Mujer 
               deriving (Show,Eq)
-- y se define Rol'
data Rol' = Decanx' Genero 
 	       | Docente' Funcion
 		   | Estudiante' Carrera Ingreso
 			deriving (Show,Eq)
--f)
-- No podemos representar a un alumno incripto en dos carreras. Para lograr eso deberiamos hacer tuplas con la carerra y el ingreso. De esta forma tener una lista de tuplas con carrera e ingreso cada una, y poder representar cuando un estudiante esta en dos o más carreras.
 
data Rol'' = Decanx'' Genero
 	       | Docente'' Funcion
 		   | Estudiante'' [(Carrera, Ingreso)]
 		    deriving (Show,Eq)

estudia :: Rol'' -> Carrera -> Bool
estudia (Estudiante''  ([])) _ = False
estudia (Estudiante'' ((x,_):xs)) c = x == c && estudia (Estudiante'' xs) c
estudia _ _ = False

---Ejercicio 3]

--a)
data Persona = Per String Int Int Rol
			   deriving (Show,Eq)
--Nombre, Documento, Año de Nacimiento, Rol
--b)
--No ya que se etsaria llamando Persona a si misma
--c)
---1)
edad :: Persona -> Int -> Int
edad (Per _ _ an _ ) a = (a - an)
---2)
existe :: String -> [Persona] -> Bool
existe _ [] = False
existe n ((Per nm _ _ _):xs) = n == nm || existe n xs
---3)
est_astronomia :: [Persona] -> [Persona]
est_astronomia [] = []
est_astronomia ((Per nm dn an (Estudiante n i)):xs) | n == Astronomia = ((Per nm dn an (Estudiante n i)):(est_astronomia xs))
													| otherwise = est_astronomia xs
est_astronomia ((Per _ _ _ _ ):xs) = est_astronomia xs
---4)
padron_docente :: [Persona] -> [(String, Int)]
padron_docente [] = []
padron_docente ((Per nm dn _ (Docente _)):xs) = (nm,dn) : padron_docente xs
padron_docente ((Per _ _ _ _ ):xs) = padron_docente xs

---Ejercicio 4]
--a)
data Cola = Vacia | Encolada Persona Cola
				   deriving (Show,Eq)
---1)
atender :: Cola -> Cola
atender Vacia = Vacia
atender (Encolada(Per _ _ _ _ ) c) = c
---2)
encolar :: Persona -> Cola -> Cola
encolar p Vacia = (Encolada p Vacia)
encolar p (Encolada x Vacia) = (Encolada x (Encolada p Vacia))
encolar p (Encolada x y) = (Encolada x (encolar p y))
---3)
busca :: Cola -> Funcion -> Persona
busca Vacia _ = error "troliado"
busca (Encolada x xs) c | ndeah x == Docente c = x
                        | otherwise = busca xs c
ndeah :: Persona -> Rol
ndeah (Per _ _ _ r) = r
--b)
--Cola tiene similitudes con una lista de Persona.
--Los implementaria de la siguiente manera:
--a
atender' :: [Persona] -> [Persona]
atender' [] = []
atender'  (_:xs) = xs
--b
encolar' :: Persona -> [Persona] -> [Persona]
encolar' p [] = [p]
encolar' p (x:xs) = (x:xs) ++ [p]
--c
busca' :: [Persona] -> Funcion -> Persona
busca' [] _ = error "troliado"
busca' ((Per nm dn an (Docente Practico)):_) Practico = (Per nm dn an (Docente Practico))
busca' ((Per nm dn an (Docente Teorico)):_) Teorico = (Per nm dn an (Docente Teorico))
busca' ((Per _ _ _ (_)):xs) c= busca' xs c
 
---Ejercicio 5]*
data ListaAsoc a b = LisVacia | Nodo a b (ListaAsoc a b)

type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String

--a)
--Utilizando Padron, ListaAsoc Int String.
--b)
---1)
la_long :: ListaAsoc a b -> Int
la_long LisVacia = 0
la_long (Nodo _ _ x) = 1 + la_long x
---2)
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat LisVacia LisVacia = LisVacia
la_concat (Nodo a b c) LisVacia = (Nodo a b c)
la_concat LisVacia (Nodo a b c) = (Nodo a b c)
la_concat (Nodo a b c) x = (Nodo a b (la_concat c x))
---3)
la_pares :: ListaAsoc a b -> [(a, b)]
la_pares LisVacia = []
la_pares (Nodo a b c) = ((a,b) : la_pares c)
---4)
la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca LisVacia _ = Nothing
la_busca (Nodo a b c) d = if (a==d) then (Just b) else (la_busca c d)
---5)
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar _ LisVacia = LisVacia
la_borrar d (Nodo a _ c) = if (a==d) then c else (la_borrar d c)

---Ejercicio 6]

data Arbol a = Hoja | Rama (Arbol a) a (Arbol a)

type AGeo = Arbol String
argentina , brasil , italia , america , europa , arbol_del_mundo :: AGeo
argentina = Rama Hoja "Argentina" Hoja
brasil = Rama Hoja "Brasil" Hoja
italia = Rama Hoja "Italia" Hoja
america = Rama argentina "America" brasil
europa = Rama Hoja "Europa" italia
arbol_del_mundo = Rama europa "Tierra" america
--a)
a_long:: Arbol a -> Int
a_long Hoja = 0
a_long (Rama Hoja _ z) = 1 + a_long z
a_long (Rama x _ Hoja) = 1 + a_long x
a_long (Rama x _ z) = 1 + a_long x + a_long z
--b)
a_hojas:: Arbol a -> Int
a_hojas Hoja = 1
a_hojas (Rama Hoja _ z) = 1 + a_hojas z
a_hojas (Rama x _ Hoja) = 1 + a_hojas x
a_hojas (Rama x _ z) = a_hojas x + a_hojas z
--c)
a_inc :: Arbol Int -> Arbol Int 
a_inc Hoja = Hoja
a_inc (Rama Hoja y Hoja) = (Rama Hoja (y+1) Hoja)
a_inc (Rama x y Hoja) = (Rama (a_inc x) (y+1) Hoja)
a_inc (Rama Hoja y z) = (Rama Hoja (y+1) (a_inc z))
a_inc (Rama x y z) = (Rama (a_inc x) (y+1) (a_inc z))
--d)
a_nombre :: Arbol Persona -> Arbol String
a_nombre Hoja = Hoja
a_nombre (Rama Hoja (Per a _ _ _) Hoja) = (Rama Hoja a Hoja)
a_nombre (Rama Hoja (Per a _ _ _) z) = (Rama Hoja a (a_nombre z))
a_nombre (Rama x (Per a _ _ _) Hoja) = (Rama (a_nombre x) a Hoja)
a_nombre (Rama x (Per a _ _ _) z) = (Rama (a_nombre x) a (a_nombre z))
--e)
a_map :: (a -> b) -> Arbol a -> Arbol b
a_map _ Hoja = Hoja 
a_map f (Rama Hoja y Hoja) = (Rama Hoja (f y) Hoja)
a_map f (Rama Hoja y z) = (Rama Hoja (f y) (a_map f z))
a_map f (Rama x y Hoja) = (Rama (a_map f x) (f y) Hoja)
a_map f (Rama x y z) = (Rama (a_map f x) (f y) (a_map f z))

a_inc':: Arbol Int -> Arbol Int
a_inc' x = (a_map (+1) x)

a_nombre' :: Arbol Persona -> Arbol String
a_nombre' x = (a_map (\(Per y _ _ _) -> y) x)
--d)
a_sum :: Arbol Int -> Int
a_sum Hoja = 0
a_sum (Rama Hoja y Hoja) = y
a_sum (Rama Hoja y z) = y + (a_sum z)
a_sum (Rama x y Hoja) = y + (a_sum x)
a_sum (Rama x y z) = y + (a_sum x) + (a_sum z)