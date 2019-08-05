# Librerías
Este archivo pretende dos cosas:

1. Explicar el uso de algunas bibliotecas de Haskell que les van a ser útiles a lo largo del ejercicio
2. Darles ejemplos que puedan copiar y pegar en el ejercicio 🙂


# Import
Todos los ejemplos que veamos en este archivo van a ser úso de bibliotecas externas, por lo que lo primero que tendremos que hacer es importarlas.
Existen muchos _sabores_ de como hacer esto, pero vamos a quedarnos con "unqualified imports".

Haskell nos obliga a poner los imports como una de las primeras cosas en un archivo, de lo contrario nos dá un error gramatical:
`parse error on input ‘import’`


Por ejemplo, importemos la función de `sort` _(ordenar)_ y `reverse` _(dar vuelta)_ de listas


```haskell
-- +------------------------------------------------- Syntaxis para importar
-- |                 +------------------------------- Nombre de la biblioteca
-- |                 |                            +-- Nombre de las funciones a importar
-- |                 |                            |
-- V                 V                            V
import           Data.List                      ( sort
                                                , reverse
                                                )
```
_[La documentación oficial de `Data.List`](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-List.html)_

Ahora que está importada, podemos hacer uso:
```haskell
reverse (sort [3, 1, 2]) -- [3,2,1]
```

# Ahora si
## `text`
### Documentación
https://www.stackage.org/haddock/lts-12.4/text-1.2.3.0/Data-Text.html

### Descripción
En muchos lenguajes, existe el concepto de "string", como una cadena de caracteres. Haskell no es tan diferente al resto, y tiene el tipo de dato `String`.
En Haskell, `String` está definido como: `[Char]`.

Sin embargo, por cuestiones de la vida, `String` es altamente ineficiente, tanto en velocidad de cómputo, como en espacio. Esto, combinado con qué Haskell es un lenguaje fuertemente tipado, nos fuerza a tener diferentes tipos para representar _""lo mismo""_ (una cadena de caracteres).

Muchas de las bibliotecas que hacen tareas pesadas con texto, van a optar por usar `Text` antes que usar su contraparte ineficiente `String`.

### Usos
#### Crear `Text`s
Como tanto `String` como `Text` representa la misma idea, tenemos dos funciones muy simples para transformar de uno al otro:

```haskell
pack :: String -> Text
unpack :: Text -> String
```
Es tan común que no queramos usar `String` en nuestro código, que hay un ["Pragma"](https://wiki.haskell.org/Language_Pragmas) que hace que `String` sea polimorficos con otras cosas, en particular la que nos interesa: `Text`: `OverloadedStrings`.
Así y todo posiblemente queramos importar el tipo de dato `Text`:
```haskell
import           Data.Text               (Text)
```

**Regla de oro**: Pongamos `{-# LANGUAGE OverloadedStrings #-}` como primera línea de nuestro archivo, y usemos `Text` indiscriminadamente! 🎉

## `Http`
### Descripción
Hacer pedidos por `HTTP` tiene varias intricaciones. Saber el schema (`http`/`https`), poder interpretar la URL o qué hacer cuando no. El verbo con el que queramos acceder (`GET`/`POST`/etc), si queremos pasarle un cuerpo al pedido, y tantas otras cosas. Todo esto es altamente interesante, pero les queríamos facilitar una _biblioteca_ nuestra que simplifica inmensamente todo esto _(y es sumamente limitada en lo que puede hacer)_.

### Documentación
El código está en el archivo `src/Http.hs`.

### Usos
La biblioteca tiene solo una función: `get`, que dada una URL, hace un `GET` pedido a esta.

#### GET
```haskell
get :: FromJSON response => Text -> IO response
```
_(`FromJSON` es de la próxima biblioteca: `aeson`)_

Por ejemplo, si tuviésemos un `data Persona = Persona { nombre :: Text, direccion :: Text }` y hubiese una página a la que si le hiciecemos `GET` a `http://www.personas.com/Pepe` nos devolviese un `JSON` que pueda _matchear_ con nuestro tipo `Persona`, entonces podríamos escribir:
```haskell
data Persona = ...

getPersona :: Text => IO Persona
getPersona nombre = get ("http://www.personas.com/" <> nombre)
```

## `aeson`
### Documentación
https://www.stackage.org/haddock/lts-12.4/aeson-1.3.1.1/Data-Aeson.html

### Descripción
Su propósito es darnos la facilidad de convertir `JSON` a valores de nuestros tipos (o fallar gracilmente).
Posiblemente la más rica de las bibliotecas que vayamos a usar, pero para este workshop vamos a apenas tocar la superficie de lo que `aeson` puede hacer.

### Usos
#### Transformar de `JSON` a Haskell
Misma idea que vimos con `Text`, vamos a tener una forma de transformar de `JSON` a un valor y visceversa, pero esta vez es un poquito más complejo.
La idea de `aeson` es que algo es transformable de `JSON` si nuestro tipo de instancia de la typeclass `FromJSON` _(pss, este es el código que vimos arriba en `Http`)_.

Imaginemosnos que tenemos el tipo de dato:
```haskell
data Actriz = Actriz
  { nombre :: String
  , cumpleaños :: String
  }
```
y tenemos un endpoint que nos dá este JSON:
```JSON
{
  "birthday": "1968-07-07",
  "known_for_department": "Acting",
  "deathday": null,
  "id": 536,
  "name": "Jorja Fox",
  "also_known_as": [
    "Jorja-An Fox"
  ],
  "gender": 1,
  "biography": "Jorja-An Fox (born July 7, 1968 ...",
  "popularity": 1.097,
  "place_of_birth": "New York City - New York - USA",
  "profile_path": "/jq3W7EF5juYapLoA779QHmAbAGL.jpg",
  "adult": false,
  "imdb_id": "nm0289080",
  "homepage": null
}
```
_[Request real](http://api.themoviedb.org/3/person/536?api_key=2ba61b38c35668c26d754910aac7a729)_

Lo que tenemos que hacer es codificar cómo podríamos transformar el `JSON` a nuestro tipo, para eso escribimos una instancia de `FromJSON`:
```haskell
instance FromJSON Actriz where
    parseJSON = ...
```

Que tiene solo una función que nos interesa: `parseJSON`.
El tipo de esta es: 
```haskell
parseJSON :: Value -> Parser Actriz
```
_(el tipo `Actriz` está parametrizado por la instancia de `FromJSON` que vimos más arriba)_

Por un lado tenemos [`Value`](https://www.stackage.org/haddock/lts-12.4/aeson-1.3.1.1/Data-Aeson.html#t:Value). Esta es la forma de `aeson` de decir "algo que se parezca a un JSON". Puede ser un string, un objeto (si empieza con `{`, le diremos `Object`), una lista (si empieza con `[`, le diremos `Array`), etc.
Por el otro necesitamos un `Parser` de `Actriz`. Para construir este parser `aeson` nos brinda una miríada de funciones. En particular la que más nos interesa es:
```haskell
(.:) :: FromJSON a => Object -> Text -> Parser a 
```
_(notar que `.:` es una función infija. A diferencias de `pack` o `sort`, que van adelante de los argumentos, `.:` va **entre** los argumentos)_

`Object` es el mismo del que habíamos hablado como representación de JSON que empieza con `{`, por lo que toma un objeto JSON, un texto que representa el nombre de la clave del JSON, y nos da un `Parser` del contenido de la clave de ese JSON.
Por ejemplo, si tuviésemos un objeto
```JSON
{
    "foo": 3
}
```

podríamos construir un parser como:
```haskell
objeto .: "foo"
-- ó
(.:) objeto "foo"
```

Toda esta biblioteca se encarga de manejar los errores cuando las claves no están, o no son del tipo que querríamos en nuestro tipo; todo con muy poco código.

Muchas veces queremos obtener más de una clave de un JSON, y combinarlo con otras claves (como era nuestro ejemplo original). Por suerte `Parser` es una `monada` y está programada para poder ser combinada, por lo que podemos escribir:
```haskell
jsonAActriz objeto = do
  nombre_     <- objeto .: "name"
  cumpleaños_ <- objeto .: "birthday"
  ...
```
Ahora que tenemos los dos valores, necesitamos construir nuestro valor con el tipo:
```haskell
jsonAActriz objeto = do
  nombre_     <- objeto .: "name"
  cumpleaños_ <- objeto .: "birthday"
  pure (Actriz nombre_ cumpleaños_)
```
y tenemos nuestra función: `jsonAActriz :: Object -> Parser Actriz`.

Como `parseJSON` toma un `Value` y nosotros solo podríamos parsear un `Object`, necesitamos completar el patrón con un error, por ejemplo: `parseJSON (Array _) = fail "no pude interpretar el objeto"`

Una ver que ponemos todo junto:
```haskell
data Actriz = Actriz
  { nombre :: String
  , cumpleaños :: String
  }

jsonAActriz :: Object -> Parser Actriz
jsonAActriz objeto = do
  nombre_     <- objeto .: "name"
  cumpleaños_ <- objeto .: "birthday"
  pure (Actriz nombre_ cumpleaños_)

instance FromJSON Actriz where
  parseJSON (Object actrizJSON) = jsonAActriz actrizJSON
  parseJSON _                   = fail "no pude interpretar el objeto"
```

#### Otra forma (`Aplicative`)
`aeson` en su infinita gloria, también nos deja escribir lo mismo pero un poquito más corto, con unos _chirimbolos locos_:

```haskell
jsonAActriz objeto =
    Actriz
       <$> objeto .: "name"
       <*> objeto .: "birthday"
```

Este código construye una Actriz con 2 parametros, el primero (nombre) es el valor de la clave `"name"` del JSON y el segundo (cumpleaños) es el valor de la clave `"birthday"` del JSON.

#### OOOOOOtra forma (`deriving`)
Esta vez vamos a hacer 🙌 al compilador de Haskell quien puede inferir mucho de esto, si hacemos que nuestro `Data` tenga los mismos nombres que los atributos del JSON y _derivar_.

Para esto necesitamos agregar el Pragma: `DeriveGeneric` _(agregar `{-# LANGUAGE DeriveGeneric #-}` como primera línea de nuestro `.hs`)_ y cambiar nuestro `data` así:

```haskell
data Actriz = Actriz
  { name :: String
  , birthday :: String
  } deriving (Generic)
```
Qué lindo que es Haskell.
