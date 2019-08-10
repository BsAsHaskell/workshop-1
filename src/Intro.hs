-- Hola! Bienvenides al Workshop introductorio de Haskell
-- Primero que nada, veamos como escribir comentarios:
-- Estos son comentarios de linea, ocupan ... una linea

{- Esto es un comentario de bloque
 - y puede ocupar varios renglones -}

-- Todo archivo de un proyecto Haskell tiene que definir de que modulo es,
-- y debe corresponder con el nombre del archivo y su ubicación en el proyecto (como en Java)
module Intro where

-- Con la sintaxis anterior el modulo exporta todas las funciones y declaraciones que define.
-- Si se quiere, se puede limitar que cosas exporta asi:
-- `module Intro where (unaFuncion, UnTipo)`
--
-- Aparte, si el nombre del modulo es `Main` entonces su función `main`
-- será el punto de entrada del ejectuable final.

-- A diferencia de JavaScript, pero como Python, Haskell es sensible a la indentación.
-- Eso significa que las declaraciones deben arrancar en la columna 0,
-- y cualquier linea puede partirse siempre y cuando se indente un poco mas que la anterior.

-- Siendo Haskell un lenguaje con tipado estático, uno de los principales elementos que vamos
-- a definir son, justamente, tipos.

---------------------------------------------------------------------------
--                        Tipos de Datos                                 --
---------------------------------------------------------------------------

-- En otro lenguajes `=` suele significar asignación.
-- En Haskell, `=` significa "está definido así".

--   +--------------------------- Nombre del *tipo*
--   |         +----------------- Nombre del *constructor*
--   |         |       +--------- Toma una `String`
--   |         |       |      +-- Y toma un `Int`
--   V         V       V      V
data Persona = Persona String Int

-- Usamos `data` para definir un nuevo tipo de datos.
-- Todos los tipos deben empezar con mayúscula, y el nombre de este tipo es `Persona`.
-- Este tipo toma dos argumentos: Uno de tipo `String` y otro de tipo `Int`.
-- Capaz son el nombre y el poder de una persona.
-- El constructor se usa igual que una función, y es la única manera de construir
-- variables de tipo `Persona`.
-- Por ejemplo:

maria = Persona "Maria" 32

-- Podemos pensar en `Persona` como que es un String **y** un Int.
-- Pero también queremos poder definir tipos que son una cosa **u** otra.
-- Esto podemos hacerlo así:

--   +------+---------+-------------------- Igual que antes
--   |      |         |     +-------------- "O"
--   |      |         |     | +------------ Otro constructor
--   |      |         |     | |        +--- Argumentos del segundo constructor
--   V      V         V     V V        V
data Figura = Circulo Float | Cuadrado Float Float

-- Este ejemplo define un tipo `Figura` con dos constructores,
-- cada uno con distinta cantidad de argumentos.
-- Una figura puede ser un `Circulo` que toma un radio,
-- **o** un `Cuadrado`, que toma alto y ancho.

-- A diferencia de otros lenguajes, Haskell no provee igualdad, comparación
-- o siquiera una manera de mostrar tipos arbitrarios.
-- Para esto usa un sistema llamado "typeclasses".
-- Una typeclass es una familia de funciones, y un tipo puede o no
-- implementar (o sea, ser miembro de) una typeclass.
-- Por ejemplo la typeclass `Show` tiene la función `show` que ... muestra un valor
-- de un tipo que implemente `Show`.
-- Por suerte, las typeclass básicas podemos "derivarlas" automáticamente,
-- es decir, que el compilador las va a escribir por nosotros.
-- Las typeclass básicas incluyen `Show` para mostrar, `Ord` para ordenar
-- (o sea comparar por mayor o menor) y `Eq` para la igualdad.

data Animal = Perro String Int | Gato String
  deriving (Eq, Ord, Show)

-- ***********************************************
-- ****************** EJERCICIO 1 ****************
-- ***********************************************
--
-- Abrí en la consola de Haskell este archibo con `stack ghci src/Intro.hs`.
-- Ahora definí un animal y una persona en la consola.
-- > persona1 = Persona "nombre" 5
-- Fijate como podes imprimir al animal y no a la persona.
-- ¿Cuál es mas grande, un perro llamado "bobby" de 7 años o un gato llamado "pepita"?
--
-- ***********************************************
-- **************** FIN EJERCICIO 1 **************
-- ***********************************************

-- Hay otra forma mas de defnir tipos y la llamamos "record syntax".
-- Lo bueno que tiene es que nos deja nombrar los parámetros de un constructor
-- y al mismo tiempo define funciones para obtener los mismos.
-- La única trampa es que estas funciones generadas no pueden repetirse
-- en el miso modulo, por eso es buena práctica prefijarlas con el
-- nombre del tipo.

data Jugador = Jugador
 { jugadorNombre :: String
 , jugadorNivel :: Int
 } deriving (Show)

-- Esta declaración define un tipo `Jugador` muy parecido a `Persona`,
-- pero que ahora tiene dos "getters", de manera que podemos hacer:

jugador1 = Jugador "carla" 7
nombreDeJugador1 = jugadorNombre jugador1
nivelDeJugador1 = jugadorNivel jugador1

-- Finalmente tenemos tipos "genéricos" o paramétricos.
-- Estos son tipos donde algún tipo interno es un argumento,
-- muy como en Java o TypeScript.
--
--   +-------------------------- Nombre del tipo
--   |        +----------------- Parámetro de tipo `a`, puede tener cualquier nombre
--   |        |   +------------- Un constructor para `Algun` valor
--   |        |   |     +------- Lo que sea el tipo `a`
--   |        |   |     | +----- "o"
--   |        |   |     | | +--- Un constructor sin argumentos
--   V        V   V     V V V
data Opcional a = Algun a | Nada

-- Este tipo por ejemplo sería como el `Optional<T>` en Java,
-- puede "tener: un valor de tipo `a` o estar vacío.


---------------------------------------------------------------------------
--                           Valores y funciones                         --
---------------------------------------------------------------------------

-- Aunque ya lo vimos, empecemos definiendo una constante.
-- Así definimos un valor del tipo `Persona` que definimos antes:

vegeta = Persona "Vegeta" 9000

-- Si ponemos dos cosas juntas (o sea, separadas por un espacio),
-- Haskell va a intentar "aplicar" los valores de izquierda a derecha.
-- En este caso, como `Persona` es el constructor de tipo y es una función,
-- le va a aplicar la String "Vegeta" y el Int 9001.

-- También podemos anotar un valor con el tipo:

goku :: Persona
goku = Persona "Goku" 9001

-- El símbolo `::` se lee como "tiene el tipo", por lo que podemos leer
-- la declaración anterior como "goku tiene el tipo Persona".
-- Haskell tiene inferencia de tipos, o sea que (en general) puede deducir
-- a que te referís. Pero es una buena práctica anotar todas las definiciones
-- del nivel superior, o sea a nivel módulo. Esto es por dos razones:

-- Primero, hace mucho mas facil de leer y mantener el código si sabes de que tipo
-- es un valor o una función.
-- Y Segundo, le hace el trabajo de deducir el tipo del resto del programa mas facil
-- al compilador.

-- Escribamos una función ahora. Las funciones son valores de primer tipo en Haskell
-- y en principio no se diferencian en nada de los valores contantes que vimos antes.
-- Vamos a escribir una función que sume el poder de una persona:

powerUp :: Persona -> Persona
powerUp (Persona nombre poder) = Persona nombre (poder + 1)

-- +-------+--------------------- powerUp tiene el tipo:
-- |       |  +------------------ Toma una Persona como argumento
-- |       |  |       +---------- Flecha de función
-- |       |  |       |  +------- Devuelve una Persona
-- V       V  V       V  V
-- powerUp :: Persona -> Persona
--          +-------------------- Pattern match en el constructor
--          |       +----+------- Nombre de los parámetros
--          V       V    V
-- powerUp (Persona nombre poder) =
--     +-------------------------- Devolvemos una Persona nueva
--     |       +------------------ Con el mismo nombre
--     |       |     +------------ Y el poder mas uno
--     V       V     V
--     Persona name (poder + 1)

-- Haskell es un lenguaje inmutable, asi que no podemos "modificar" la
-- persona, solo podemos devolver una nueva.

-- ***********************************************
-- ****************** EJERCICIO 2 ****************
-- ***********************************************
-- ¿Cómo obtendrías el nombre de una Persona?
-- ¿Y su edad?
-- Tip: en ghci (la consola de Haskell) podes ver el tipo de algo con:
-- > :t algo

getNombre :: Persona -> String
getNombre = error "Escribime!"

getPoder :: Persona -> Int
getPoder = error "Escribime!"

-- ***********************************************
-- **************** FIN EJERCICIO 2 **************
-- ***********************************************

-- Las funciones que toman multiples argumentos usan la misma flecha,
-- siendo la última cosa lo que devuelve la función.
-- Esta función absorbe el poder de una persona. El guión bajo en la segunda
-- persona significa "no voy a usar este valor" y no les asignamos una variable.

absorber :: Persona -> Persona -> Persona
absorber (Persona nombre poderAnterior) (Persona _ otroPoder) =
    Persona nombre (poderAnterior + otroPoder)

-- ¡Atención! ¿Qué pasa con la persona que le absorvimos el poder?
-- ¿No tendríamos que volverlo a 0? Como Haskell es inmutable no podemos
-- modificarla, pero si podemos devolver las dos "nuevas" personas.
-- Para esto usamos una Tupla, que es basicamente un tipo que tiene
-- dos variables del mismo o distinto tipo.
-- Esta función tambien introduce el `let`, que es la manera que tenemos
-- de asignar valores temporales en una función.

absorber2:: Persona -> Persona -> (Persona, Persona)
absorber2 persona victima =
    let poderAbsorbido = getPoder persona + getPoder victima
        nuevaPersona = Persona (getNombre persona) poderAbsorbido
        victimaDrenada = Persona (getNombre victima) 0
    in (nuevaPersona, victimaDrenada)

-- Hay dos cosas importantes acá: `let` y la *prioridad de aplicación*.
--
-- `let ... in` nos permite definir varias cosas que solo existen en la
-- expresion que sigue inmediatamente al `in`.
--
-- Las funciones aplican con mas prioridad que cualquier cosa, por lo que
--
--     getPoder persona + getPoder victima
--
-- es lo mismo que
--
--     (getPoder persona) + (getPoder victima)
--

-- Otra manera de declarar cosas adentro de una función es con una
-- clausula `where`. Mientras que con un `let` las expresiones viven solo
-- después del `in`, con un `where` viven en todo el cuerpo de la función.
--
-- La misma función, con Jugador en vez de Persona y con `where` quedaría:

absorber3 :: Jugador -> Jugador -> (Jugador, Jugador)
absorber3 persona victima =
    (nuevaPersona, victimaDrenada)
  where
    poderAbsorbido = jugadorNivel persona + jugadorNivel victima
    nuevaPersona = Jugador (jugadorNombre persona) poderAbsorbido
    victimaDrenada = Jugador (jugadorNombre victima) 0

-- Notemos que esta vez usamos los "getters" que nos regalo el record syntax.
--
-- Otra funcionalidad poderosa de Haskell es el pattern matching. El mismo lo usamos
-- para desconstruir Personas anteriormente. Pero también podemos hacer pattern matching
-- con valores:

calcularArea :: Figura -> Float
calcularArea (Circulo radio) = pi * radio ** 2
calcularArea (Cuadrado base altura) = base * altura

-- Hay otra manera de escribir lo mismo usando una clausula `case`:

calcularArea2 :: Figura -> Float
calcularArea2 figura = case figura of
  Circulo radio -> pi * radio ** 2
  Cuadrado base altura -> base * altura

-- Además, podes hacer pattern matching con valores, por ejemplo:

seLlamaBobby :: Animal -> Bool
seLlamaBobby (Perro "bobby" _) = True
seLlamaBobby (Perro "BOBBY" _) = True
seLlamaBobby _ = False

-- Pasemos a algo mas interesante. Las listas o arrays en Haskell se definen así,
-- siendo `:` el construtor de lista que toma un elemento y una lista
-- y nos devuelve otra lista con el elemento al principio.

unaListaDeEnteros :: [Int]
unaListaDeEnteros = [1, 2, 3, 4, 5] -- explicito

otraListaDeEnteros :: [Int]
otraListaDeEnteros = [1..5] -- generadores

unaLista :: [Char]
unaLista = 'h' : 'o' : 'l' :  'a' : [] -- con el constructor

-- Vieron que `:` va infijo, o sea se usa "entre" dos expresiones.
-- Esto se llama función infija y son funciones definidas normalmente.
-- Por suerte Haskell ya nos da varias, como `+`, `<>`, etc.

-- La librería standard de Haskell trae varias funciones para las mismas,
-- podemos ver la documentación aqui:
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html
-- Ahora repasemos algunas de sus operaciones mas comunes.
--
-- Podemos sumar listas

ambasListas :: [Int]
ambasListas = unaListaDeEnteros <> otraListaDeEnteros

-- darlas vuelta

alReves :: [Int]
alReves = reverse ambasListas

-- acceder a su primer elemento o a la "cola" (todos menos el primero)

elPrimeroAlFinal :: [Int]
elPrimeroAlFinal = tail unaListaDeEnteros <> [head unaListaDeEnteros]

-- "modificar" cada elemento

unaListaMasUno :: [Int]
unaListaMasUno = map (+1) unaListaDeEnteros

-- Acá estamos haciendo "aplicación parcial". Supongamos una función:

sumar :: Int -> Int -> Int
sumar a b = a + b

-- si hacemos `(sumar 5)` nos devuelve una función de tipo
-- `Int -> Int`

sumarCinco :: Int -> Int
sumarCinco = sumar 5

doce :: Int
doce = sumarCinco 7

-- ***********************************************
-- ****************** EJERCICIO 3 ****************
-- ***********************************************
-- Dada una lista de jugadores, queremos subir el nivel de todos en un
-- número dado `x`. Definir esa función.

levelear :: Int -> [Jugador] -> [Jugador]
levelear = error "Escribime!"

-- ***********************************************
-- *************** FIN EJERCICIO 3 ***************
-- ***********************************************

----------------------------------------------------------------------------
--                      Do Syntax                                         --
----------------------------------------------------------------------------

-- Okay, hasta acá vimos como definir tipos, valores y funciones.
-- Lo que nos falta es ver como hacer ... cosas.
-- Si todo es inmutable, ¿Cómo imprimimos en pantalla, o abrimos un archivo?
-- Para esto Haskell tiene un tipo especial llamado `IO a`, donde `a` es el tipo
-- del resultado de evaluar y ejecutar esas acciones.
--
-- Una ultima cosa, para "encadenar" acciones de IO tenemos que abrir un bloque `do`.
-- Para esto escribimos `do` y las subsecuentes lineas indentadas al mismo nivel.
-- Todas estas lineas deben ser del tipo IO.

--           +----- Esto significa que estamos haciendo IO
--           |  +-- () significa que no devolvemos nada, el tipo vacio
--           V  V
helloWorld :: IO ()
helloWorld = do
    putStrLn "Hello World!"
    putStrLn "so much functional wow"

-- El bloque `do` secuencia ambas acciones por lo que si evaluamos esta función,
-- deberiamos ver ambas lineas impresas en pantalla.
--
-- Ahora obtengamos alguna entrada del usuario:

prompt :: IO String
prompt = do
    putStrLn "Enter a text plz:"
    line <- getLine
    pure line

-- Acá tenemos un simbolo nuevo: `<-`
-- Esta flechita nos deja obtener el resultado de una acción IO,
-- de tal manera que si lo que esta a la derecha de la flecchita es `IO a`,
-- lo que está a la izquierda es del tipo `a`.
--
-- El tipo de `getLine` es:
--
--     getLine :: IO String
--
-- y cuando se ejecuta, toma una linea de entrada del usuario y devuelve esa String.
-- Con `<-` estamos "ligando" el String de la acción a la variable `line`.

-- La ultima linea de un bloque `do` debe tener el tipo que especificamos `IO a`,
-- o sea que si hubiesemos escrito esto:
--
--     prompt :: IO String
--     prompt = do
--       putStrLn "Enter a text"
--       line <- getLine
--       line
--
-- Tendriamos un error de tipo. Esto es porque `line` es tipo String,
-- pero queremos `IO String`. Para esto tenemos la función `pure`
-- que toma un valor de tipo `a` y lo "eleva" a un `IO a`.
--
-- Además, podemos usar `let` en bloques `do` muy parecido a antes,
-- para asignar variables normales.

customPrompt :: String -> IO String
customPrompt string = do
    let promptStr = string ++ "> "
    putStrLn promptStr
    getLine

-- ***********************************************
-- ****************** EJERCICIO 4 ****************
-- ***********************************************

-- En este ejercicio vamos a juntar varias cosas de las que estuvimos viendo.
-- Queremos hacer una función que le pregunte al usuario por el nombre de una mascota,
-- y que según el nombre ingresado devuelva un perro o un gato.
-- Si el nombre es 'garfield', 'salem' o 'bola de nieve II',
-- devuelve un Gato, si no, devuelve un Perro cuya edad es la cantidad de caracteres del nombre
-- (un String es lo mismo que [Char])

crearMascota :: IO Animal
crearMascota = error "implementame"

-- ***********************************************
-- *************** FIN EJERCICIO 4 ***************
-- ***********************************************

-- Suficiente introducción.
-- ¡Ahora veamos el ejercicio!
