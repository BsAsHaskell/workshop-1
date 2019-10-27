-- Hola! Bienvenides al Workshop introductorio de Haskell
-- Primero que nada, veamos como escribir comentarios:
-- Estos son comentarios de línea, ocupan ... una línea

{- Esto es un comentario de bloque
 - y puede ocupar varios renglones -}

-- Todo archivo de un proyecto Haskell tiene que definir de que módulo es,
-- y debe corresponder con el nombre del archivo y su ubicación en el proyecto (como en Java)
module Intro where

-- Con la sintaxis anterior el módulo exporta todas las funciones y declaraciones que define.
-- Si se quiere, se puede limitar que cosas exporta asi:
-- `module Intro where (unaFuncion, UnTipo)`
--
-- Aparte, si el nombre del módulo es `Main` entonces su función `main`
-- será el punto de entrada del ejectuable final.

-- A diferencia de JavaScript, pero como Python, Haskell es sensible a la indentación.
-- Eso significa que las declaraciones deben arrancar en la columna 0,
-- y cualquier línea puede partirse siempre y cuando se indente un poco más que la anterior.

import Data.List (sort)

-- Acá importamos la función `sort` del módulo Data.List.
-- Hay muchas maneras de importar cosas, pero por ahora vamos a ignorar todas.


---------------------------------------------------------------------------
--                        Consola interactiva                            --
---------------------------------------------------------------------------

-- Antes que nada, vamos a abrir una consola de GHCi, el interprete dinámico que Haskell
-- nos ofrece para poder probar cosas. Esto lo hacemos con:
--
-- > stack ghci src/Intro.hs
--
-- Acá podemos probar cosas sencillas como `4 + 6` o "hola que tal".
--
-- Aparte, podemos ver el tipo de una expresión con `:t` así
-- (`[Char]` es lo mismo que `String`):
--
-- > :t "holis"
-- > "holis" :: [Char]


-- Siendo Haskell un lenguaje con tipado estático, uno de los principales elementos que vamos
-- a definir son, justamente, tipos.

---------------------------------------------------------------------------
--                        Tipos de Datos                                 --
---------------------------------------------------------------------------

-- En otro lenguajes `=` suele significar asignación.
-- En Haskell, `=` significa "está definido así".

--   +------------------------------- Nombre del *tipo*
--   |         +--------------------- Nombre del *constructor*
--   |         |       +------------- Toma una `String`
--   |         |       |      +------ Y toma un `Int`
--   |         |       |      |
--   |         |       |      |   +-- Además, el tipo es "mostrable" (lo vemos depués)
--   |         |       |      |   |
--   V         V       V      V   V
data Persona = Persona String Int deriving (Show)

-- Usamos `data` para definir un nuevo tipo de datos.
-- Todos los tipos deben empezar con mayúscula, y el nombre de este tipo es `Persona`.
-- Este tipo toma dos argumentos: Uno de tipo `String` y otro de tipo `Int`.
-- En este caso vamos a usarlos para representar el nombre y el poder de una persona.
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
data Figura = Circulo Float | Cuadrado Float Float deriving (Show)

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

gatitou1 = Gato "miau1"
gatitou2 = Gato "miau2"

gatitou1_es_menor_que_gatitou2 = gatitou1 < gatitou2

gatitou1_es_igual_que_gatitou2 = gatitou1 == gatitou2

-- Si lo evaluamos en GHCi, vamos a ver que `gatito1_es_menor_que_gatitou2` es `True`
-- y `gatitou1_es_igual_que_gatitou2` es `False


-- ***********************************************
-- ****************** EJERCICIO 1 ****************
-- ***********************************************
--
-- Abrí en la consola de Haskell este archibo con `stack ghci src/Intro.hs`.
-- Ahora definí un animal y una persona en la consola.
-- > persona1 = Persona "nombre" 5
-- > perrito = Perro "bobby" 7
-- ¿Cuál es mas grande, un perro llamado "bobby" de 7 años o un gato llamado "pepita"?
-- ¿Y dos perros con se llaman igual pero uno es mas grande?
--
-- ***********************************************
-- **************** FIN EJERCICIO 1 **************
-- ***********************************************

-- Hay otra forma mas de definir tipos y la llamamos "record syntax".
-- Lo bueno que tiene es que nos deja nombrar los parámetros de un constructor
-- y al mismo tiempo define funciones para obtener los mismos.
-- La única trampa es que estas funciones generadas no pueden repetirse
-- en el mismo módulo, por eso es buena práctica prefijarlas con el
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
data Opcional a = Algun a | Nada deriving (Show)

-- Este tipo por ejemplo sería como el `Optional<T>` en Java,
-- puede "tener: un valor de tipo `a` o estar vacío.

unNumero = Algun 5

ningunString = Nada

-- ¿Cuál es el tipo de `unNumero`? ¿Y el de `ningunString`?


---------------------------------------------------------------------------
--                           Valores y funciones                         --
---------------------------------------------------------------------------

-- Aunque ya lo vimos, empecemos definiendo una constante.
-- Así definimos un valor del tipo `Persona` que definimos antes:

vegeta = Persona "Vegeta" 9000

-- Si ponemos dos cosas juntas (o sea, separadas por un espacio),
-- Haskell va a intentar "aplicar" los valores de izquierda a derecha.
-- En este caso, como `Persona` es el constructor de tipo y es una función,
-- le va a aplicar la String "Vegeta" y el Int 9000.

-- También podemos anotar un valor con el tipo:

goku :: Persona
goku = Persona "Goku" 9001

-- El símbolo `::` se lee como "tiene el tipo", por lo que podemos leer
-- la declaración anterior como "goku tiene el tipo Persona".
-- Haskell tiene inferencia de tipos, o sea que (en general) puede deducir
-- a que te referís. Pero es una buena práctica anotar todas las definiciones
-- del nivel superior, o sea a nivel módulo. Esto es por dos razones:

-- Primero, hace mucho más fácil de leer y mantener el código si sabes de que tipo
-- es un valor o una función.
-- Y Segundo, le hace el trabajo de deducir el tipo del resto del programa más fácil
-- al compilador.

-- Escribamos una función ahora. Las funciones son ciudadanos de primera clase en Haskell,
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
--          |       +------+----- Nombre de los parámetros
--          V       V      V
-- powerUp (Persona nombre poder) =
--     +-------------------------- Devolvemos una Persona nueva
--     |       +------------------ Con el mismo nombre
--     |       |     +------------ Y el poder más uno
--     V       V     V
--     Persona name (poder + 1)

-- Haskell es un lenguaje inmutable, así que no podemos "modificar" la
-- persona, solo podemos devolver una nueva.

-- ***********************************************
-- ****************** EJERCICIO 2 ****************
-- ***********************************************
-- ¿Cómo obtendrías el nombre de una Persona?
-- ¿Y su edad?
-- ¿Cual es el poder de `goku` después de que le aplicamos `powerUp`?
-- Tip: en ghci (la consola de Haskell) podes ver el tipo de algo con:
-- > :t algo

getNombre :: Persona -> String
getNombre = error "Escribime!"

getPoder :: Persona -> Int
getPoder = error "Escribime!"

-- ***********************************************
-- **************** FIN EJERCICIO 2 **************
-- ***********************************************

-- El tipo de las funciones que toman multiples argumentos usan la misma flecha,
-- siendo la última cosa lo que devuelve la función.
-- Esta función absorbe el poder de una persona y se lo asigna a uno mismo.
-- El guión bajo en el segundo patrón significa "no voy a usar este valor"
-- y así no lo asignamos a una variable (total no lo necesitamos).

absorber :: Persona -> Persona -> Persona
absorber (Persona nombre poderAnterior) (Persona _ otroPoder) =
    Persona nombre (poderAnterior + otroPoder)

-- ¡Atención! ¿Qué pasa con la persona que le absorvimos el poder?
-- ¿No tendríamos que hacerlo 0? Como Haskell es inmutable no podemos
-- modificarla, pero si podemos devolver las dos "nuevas" personas.
-- Para esto usamos una Tupla, que es básicamente un tipo que tiene
-- dos variables del mismo o distinto tipo.
-- Esta función tambien introduce el `let`, que es la manera que tenemos
-- de crear definiciones temporales en una función.

absorber2 :: Persona -> Persona -> (Persona, Persona)
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
-- Las funciones aplican con más prioridad que cualquier cosa, por lo que
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
absorber3 persona victima = (nuevaPersona, victimaDrenada)
  where
    poderAbsorbido = jugadorNivel persona + jugadorNivel victima
    nuevaPersona = Jugador (jugadorNombre persona) poderAbsorbido
    victimaDrenada = Jugador (jugadorNombre victima) 0

-- Notemos que esta vez usamos los "getters" que nos regaló el record syntax.
--
-- Cuando el tipo tiene varios constructores, también podemos hacer pattern matching según
-- ese constructor, ya que puede ser que tengan distinta forma (cantidad de argumentos).

calcularArea :: Figura -> Float
calcularArea (Circulo radio) = pi * radio ** 2
calcularArea (Cuadrado base altura) = base * altura

-- Hay otra manera de escribir lo mismo usando una clausula `case`:

calcularArea2 :: Figura -> Float
calcularArea2 figura = case figura of
  Circulo radio -> pi * radio ** 2
  Cuadrado base altura -> base * altura

-- Además, podés hacer pattern matching con valores, por ejemplo:

seLlamaBobby :: Animal -> Bool
seLlamaBobby (Perro "bobby" _) = True
seLlamaBobby (Perro "BOBBY" _) = True
seLlamaBobby _ = False


-- ***********************************************
-- ****************** EJERCICIO 3 ****************
-- ***********************************************
-- Ahora vamos a definir que los Jugadores son comparables y ordenables por su nivel.
-- Para esto podríamos hacer que el tipo sencillamente derive `Ord` y `Eq` como `Animal`,
-- pero esto compararía Jugadores por su nombre también, ya que por default las instancias
-- autogeneradas usan todos los elementos de un tipo para generarse.
--
-- El ejercicio consiste de hacer que Jugador implemente `Ord` y `Eq` pero solo comparando su poder.

instance Eq Jugador where
  j1 == j2 = error "Escribime!"

instance Ord Jugador where
  j1 <= j2 = error "Escribime!"

jugadoresOrdenados = sort [Jugador "Paulina" 9, Jugador "Marilinia" 2, Jugador "Pepe" 5, Jugador "Paulina" 3]

-- Si implementamos correctamente estas funciones, la siguiente expresión deberia ser True:

estanOrdenados = jugadoresOrdenados == [Jugador "Marilinia" 2, Jugador "Paulina" 3, Jugador "Pepe" 5,  Jugador "Paulina" 9]

-- ***********************************************
-- *************** FIN EJERCICIO 3 ***************
-- ***********************************************

----------------------------------------------------------------------------
--                      Do Syntax                                         --
----------------------------------------------------------------------------

-- Okay, hasta acá vimos como definir tipos, valores y funciones.
-- Lo que nos falta es ver cómo hacer ... cosas.
-- Si todo es inmutable, ¿Cómo imprimimos en pantalla, o abrimos un archivo?
-- Para esto Haskell tiene un tipo especial llamado `IO a`, donde `a` es el tipo
-- del resultado de evaluar y ejecutar esas acciones.
--
-- Una última cosa, para "encadenar" acciones de IO tenemos que abrir un bloque `do`.
-- Para esto escribimos `do` y las subsecuentes líneas indentadas al mismo nivel.
-- Todas estas lineas deben ser del tipo IO.

--            +---- Esto significa que estamos haciendo IO
--            |  +- () significa que no devolvemos nada, el tipo vacío
--            V  V
helloWorld :: IO ()
helloWorld = do
    putStrLn "Hello World!"
    putStrLn "so much functional wow"

-- El bloque `do` secuencia ambas acciones, por lo que si evaluamos esta función,
-- deberiamos ver ambas líneas impresas en pantalla.
--
-- Ahora obtengamos alguna entrada de una persona:

prompt :: IO String
prompt = do
    putStrLn "Enter text plz:"
    line <- getLine
    pure line

-- Acá tenemos un simbolo nuevo: `<-`
-- Esta flechita nos deja obtener el resultado de una acción IO,
-- de tal manera que si lo que está a la derecha de la flechita es `IO a`,
-- lo que está a la izquierda es del tipo `a`.
--
-- El tipo de `getLine` es:
--
--     getLine :: IO String
--
-- y cuando se ejecuta, lee línea de entrada y la devuelve como String.
-- Con `<-` estamos "ligando" el String de la acción a la variable `line`.

-- La última línea de un bloque `do` debe tener el tipo que especificamos `IO a`,
-- o sea que si hubiesemos escrito esto:
--
--     prompt :: IO String
--     prompt = do
--       putStrLn "Enter a text"
--       line <- getLine
--       line
--
-- Tendríamos un error de tipo. Esto es porque `line` es tipo String,
-- pero queremos `IO String`. Para esto tenemos la función `pure`
-- que toma un valor de tipo `a` y lo "eleva" a un `IO a`.
--
-- Además, podemos usar `let` en bloques `do` muy parecido a antes,
-- para asignar variables normales.

customPrompt :: String -> IO String
customPrompt string = do
    let promptPersonalizado = string <> ": "
    putStrLn promptPersonalizado
    putStr "> "
    getLine

-- También, dentro de un bloque de IO podemos usar funciones puras (que no
-- tienen IO) u otras funciones de IO que nosotros mismos hayamos definido.
-- Por ejemplo:

saludo :: String -> String
saludo nombre = "Holis, " <> nombre

saludar :: IO ()
saludar = do
    nombre <- customPrompt "Como te llamas?"
    -- Acá usamos customPrompt, una función impura que definimos más arriba
    putStrLn (saludo nombre)
    -- Y acá podemos reusar saludo en vez de tener que escribir toda la lógica
    -- de nuevo

-- El beneficio de hacer funciones como saludo en vez de poner todo
-- en el bloque de IO, es que estas funciones puras son más fáciles de
-- testear por su cuenta y se pueden volver a usar en las definiciones
-- de otras funciones puras:

saludoConEmocion :: String -> String
saludoConEmocion nombre = saludo nombre <> "!!!!"

-- Finalmente, los valores que ingresa la persona los estamos leyendo como
-- strings con la función getLine, pero nada nos impide que transformemos
-- esos valores en valores de otros tipos:

-- Dada está función que convierte un String en un Float
-- (no nos importa como está implementada)

stringToFloat :: String -> Float
stringToFloat numeroComoString = read numeroComoString

-- Podemos escribir nuestra propia función análoga a getLine, pero
-- que devuelve Floats:

getFloat :: IO Float
getFloat = do
    numeroComoString <- getLine
    pure (stringToFloat numeroComoString)

-- Y ahora que tenemos getFloat podemos hacer operaciones numéricas sobre
-- lo que nos pasa quien usa la función, como:

multiplicameDosNumeros :: IO ()
multiplicameDosNumeros = do
    putStrLn "Ingresa el primer numero:"
    primerNumero <- getFloat
    putStrLn "Ingresa el segundo numero:"
    segundoNumero <- getFloat
    let resultado = primerNumero * segundoNumero
    putStrLn ("El resultado es: " <> show resultado)

-- ***********************************************
-- ****************** EJERCICIO 4 ****************
-- ***********************************************

-- En este ejercicio vamos a juntar varias cosas de las que estuvimos viendo.
-- El objetivo final va a ser escribir una funcion IO que sirva para calcular
-- el area de las diferentes figuras que modelamos. De manera que podamos
-- correr la función y tener una interacción como la siguiente:

-- ¿Qué figura querés?:
-- > cuadrado
-- ¿Cuál es la base?:
-- > 5
-- ¿Cuál es la altura?:
-- > 10
-- El área es: 500

-- Para empezar, vamos a hacer un par de funciones de IO que nos devuelvan
-- un círculo y un cuadrado respectivamente usando input de la persona:

-- De la primera les dejamos el tipo ya escrito:
getCirculo :: IO Figura
getCirculo = error "Escribime!"

-- Lo que debería pasar cuando corra esa función es lo siguiente:

-- El interprete debería pedirme el radio del círculo por pantalla y esperar
-- que yo se lo pase

-- Cual es el radio?:
-- > 20

-- Una vez obtenido el valor, debería devolver un Círculo.
-- Si Figura no tiene definida una instancia de Show la función no
-- va a imprimir nada, pero si le escribimos deriving Show al final
-- de la definición de Figura debería también imprimir:

-- Circulo 20.0

-- -------------------------------------------------------------------------
-- Ahora hagamos lo mismo para obtener cuadrados. 
-- En este caso nos debería pedir la base y la altura.
-- Acá te toca a vos escribir el tipo aparte de la implementación.

getCuadrado = error "Escribime!"

-- Lo siguiente que podemos hacer es una función que le pregunte a quien
-- corrió la función qué tipo de figura quiere ingresar y que luego,
-- dependiendo de lo que haya escrito, reutilice lo que ya escribimos.

getFigura = error "Escribime!"

-- La idea sería que al correr getFigura pase lo siguiente:

-- Que figura queres?
-- > 

-- Si ingresamos cuadrado, nos debería preguntar la base y la altura
-- como en getCuadrado; y si ingresamos circulo, nos debería pedir el radio.
-- Esta función de IO debería terminar devolviendo la figura correspondiente.

-- Con todo esto ya tendríamos todo el código para obtener figuras ingresadas
-- por una persona!, lo que nos falta es calcular su área e imprimirla
-- por pantalla.

-- Para esto podemos reusar la función calcularArea que habíamos definido
-- bastante más arriba y la función getFigura que acabamos de implementar.

calcularAreaDeFigura = error "Escribime!"

-- Esta última función es la que debería hacer todo lo que escribimos al
-- inicio del ejercicio:
-- Pedirnos que tipo de figura queremos ingresar
-- Hacernos las preguntas necesarias para construir la figura
-- calcular su área e imprimirla por pantalla.

-- ***********************************************
-- *************** FIN EJERCICIO 4 ***************
-- ***********************************************

-- Suficiente introducción.
-- ¡Ahora veamos el ejercicio!
