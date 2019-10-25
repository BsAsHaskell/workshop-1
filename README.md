# 1er Taller de Haskell

Taller de Haskell - Agosto 2019

## Introducción

Este taller intenta ser una introducción de cero-a-algo al lenguaje de programación
funcional Haskell, es decir que el único pre-requisito para hacerlo es saber programar.

El taller consiste de dos partes:

1. Vamos a tener una [introducción](#intro) super veloz al lenguaje, su sintaxis y libreria estandard
siguiendo el archivo `Intro.hs` en la carpeta `src/`.
2. Formando grupos de 2 o 3 (aunque no es obligatorio, recomendamos hacerlo de al menos dos) vamos a implementar
un [cliente](#cliente) super simple de linea de comandos para la API de [TheMovieDB](https://themoviedb.org).

### API key

Para usar la API necesitamos una API key

```
api_key = "2ba61b38c35668c26d754910aac7a729"
```

## Preparación

### Requisitos

#### una compu

Vas a necesitar una computadora para hacer el ejercicio.

En principio, como el taller está pensado para hacer de a pares capaz no es necesario que traigas tu compu personal
si ya sabes con quien vas a trabajar, pero estaría bueno igual.

Sea el caso que no tengas acceso a una computadora, contactate con los organizadores y vemos que podemos hacer.

#### git

Asumimos que tenes idea de como usar git, pero por si las moscas:

https://git-scm.com/book/en/v2/Getting-Started-Installing-Git

#### stack

`stack` es el package manager y orquestador de compilación de Haskell. Vamos a necesitar tenerlo instalado.

Las instrucciones para eso se encuentran aquí:

http://docs.haskellstack.org/en/stable/install_and_upgrade/

#### una mente abierta

Haskell es raro. No comparte casi nada con ningún lenguaje conocido, pero no por eso es mas difícil!

### Instalación

Como el proyecto usa varias librerias, y las mismas pueden ser pesadas, recomendamos *fuertemente*
instalarlas en sus casas antes de venir al taller. Para esto deben correr:

```bash
git clone https://github.com/BsAsHaskell/workshop-1
cd workshop-1
stack setup
stack build
```

**Esto puede tardar un buen rato.** Si todo sale bien, vas a ver algo del estilo:

```
haskell-workshop-0.1.0.0: copy/register
Installing library in /Users/clrnd/Src/haskell-workshop/.stack-work/install/x86_64-osx/lts-12.10/8.4.3/lib/x86_64-osx-ghc-8.4.3/haskell-workshop-0.1.0.0-4RGcRuf1daA1fbO4MfLutr
Installing executable pelis in /Users/clrnd/Src/haskell-workshop/.stack-work/install/x86_64-osx/lts-12.10/8.4.3/bin
Registering library for haskell-workshop-0.1.0.0..
```

Y deberian poder correr el ejecutable:

```bash
stack run
> pelis: Not implemented...yet!
```

#### (Opcional) Intero con VSCode

**Intero** es una herramienta que te permite habilitar el uso interactivo de Haskell para detectar errores y conocer información del programa que estás escribiendo (como los tipos de cada expresión). Para instalarlo, recomendamos pararte en la carpeta del taller y correr:

```bash
stack build intero
stack test
```

En el caso de que tengas inconvenientes en Win10 y no encuentres el ejecutable una vez instalado, podés probar

```bash
stack build intero --copy-compiler-tool
```

Finalmente, en Visual Studio Code instalás el plugin **Haskero** y listo!

## Intro

Vamos a aprender lo básico de Haskell siguiendo el archivo [src/Intro.hs](src/Intro.hs).

## Cliente

Vamos a implementar una aplicación de terminal que consulte la API de TheMovieDB.

### 1. Buscar una persona desde un ID harcodeado

Queremos que si hacemos:

```bash
stack run
```

Nos traiga la persona con ID 54693 e imprima en la consola su
nombre y fecha de nacimiento.

Podemos usar el código que hay en [Librerias.md](Librerias.md)
para el data type `Actriz`.

### 2. Buscar una persona desde un ID por argumento

Queremos que si hacemos:

```bash
stack run 1
```

Nos traiga la persona con ID 1 e imprima lo mismo que antes.

La función de Haskell para obtener los argumentos de consola
es `getArgs` y se consigue importando `System.Environment`.

### 3. Buscar una pelicula desde un ID por argumento

Queremos que si hacemos:

```bash
stack run pelicula 1
```

Nos traiga la pelicula con ID 1 e imprima su titulo y descripción.

¡Ojo! Hay que modificar el comando para que si hacemos

```bash
stack run persona 1
```

siga andando como antes.

### 4. Mostrar estos tipos con un Show copado

Hasta ahora estamos usando el `Show` por defecto que nos genera Haskell.
Ahora queremos implementar uno nosotros.

Por ejemplo:

```haskell
instance Show Actriz where
  show actriz = ...
```

### 5. Buscar películas

Queremos que si hacemos:

```bash
stack run buscar "back to the future"
```

Nos imprima los títulos y la descripción de cada resultado en la pantalla.

## Después del taller

Si te gustó y queres seguir aprendiendo, te dejamos unos links:

- [http://haskellbook.com/](http://haskellbook.com/) - El libro mas recomendado por la comunidad actualmente. Actualizado y con un enfoque práctico.
- [http://learnyouahaskell.com/](http://learnyouahaskell.com/) - Una introducción un poco menos dura pero también menos industrial.
- [https://github.com/bitemyapp/learnhaskell](https://github.com/bitemyapp/learnhaskell) - Una colección de muchos recursos interesantes.
- [Haskell Is Easy](http://www.haskelliseasy.com) - Una guia corta para arrancar con Haskell.
