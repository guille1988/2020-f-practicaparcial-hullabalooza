module Lib where
import Text.Show.Functions

laVerdad = True

--PUNTO 1,2 y 3--

{-
De cada festival se sabe el lugar donde se realiza, la cantidad y estado de ánimo inicial del público y 
las bandas que tocan en él, ordenadas cronológicamente.
Por ejemplo, el festival Hullabalooza, en el que tocan Miranda, Los Redondos, Metallica y Soda, 
tiene un público de 20000 personas con ánimo inicial “indiferente”.
-}

data Festival = Festival{
lugar :: String,
cantidad:: Float,
estadoDeAnimo :: String,
bandas :: [String]
} deriving (Show)

hullabalooza = Festival "Springfield" 20000 "indiferente" ["Miranda","Los Redondos","Metallica","Soda"]

{-
Las bandas, cada vez que tocan, movilizan al público del festival de acuerdo al género al que pertenezcan. Por ejemplo:
rock nacional: hace que el público aumente en 100 personas
pop: generalmente no afectan al público, sólo en caso que el estado de ánimo sea "indiferente", duplican la cantidad y 
dejan el público "eufórico". 
Otro género que suele estar presente en los festivales es el metal, que tiene variantes que los especialistas denominan subgéneros
Heavy metal: hace que el público aumente 1% cada vez que toca, y a su estado de ánimo se le agregue “pesado” al final.
Trash metal: Hace que el público aumente 1% al tocar y se le agrega "basura" al final del estado de ánimo. 
Existen otros subgéneros del metal que también alteran al público de igual manera, pero agregando otros calificativos al estado de ánimo. 
-}

type Genero = Festival -> Festival

type SubGenero = Festival -> Festival

type Rock = Genero

type Pop = Genero

type Metal = Genero

rockNacional :: Rock
rockNacional festival = modificarPublico festival (+100)

pop :: Pop
pop festival 
 | ((=="Indiferente").estadoDeAnimo) festival = modificarEstadoDeAnimo (modificarPublico festival (*2)) "Indiferente"
 | otherwise = festival

modificarPublico :: Festival -> (Float -> Float ) -> Festival
modificarPublico festival funcion = festival {cantidad = (funcion.cantidad) festival}

modificarEstadoDeAnimo :: Festival -> String -> Festival
modificarEstadoDeAnimo festival  palabra = festival {estadoDeAnimo = palabra}

heavyMetal :: Metal
heavyMetal festival = modificarEstadoDeAnimo (modificarPublico festival (*1.01)) ((estadoDeAnimo festival) ++ "pesado")

trashMetal :: Metal
trashMetal festival =  modificarEstadoDeAnimo (modificarPublico festival (*1.01)) ((estadoDeAnimo festival) ++ "basura")

fusion :: Genero
fusion = pop.heavyMetal

{-
Las bandas
Las bandas tienen un conjunto de descripciones realizadas por los críticos y los decibeles a los que suelen tocar. 
Además, cada vez que tocan, las bandas movilizan al público del festival de acuerdo al género al que pertenezcan. 
Algunas bandas son:
Los redondos, que está descripta como “legendaria” y “pogosa”, toca a 45 decibeles y se considera de rock nacional. 
Soda está descripta como "irrepetible", toca a 40 decibeles y también es de rock nacional.
Miranda es una banda de pop que toca a 60 decibeles y los críticos la describieron como "insípida", "incolora" e "inodora".
Metallica está descripta como “legendaria” y “vendida” y toca a 60 decibeles. Es una de las mayores exponentes del heavy metal.
-}

data Banda = Banda{
genero :: Genero,
descripcion :: [String],
decibeles :: Float
}deriving (Show)

losRedondos = Banda rockNacional ["Legendaria","Pogosa"] 45
soda = Banda rockNacional ["Irrepetible"] 40
miranda = Banda pop ["Insipida","Incolora","Inodora"] 60
metallica = Banda heavyMetal ["Legendaria","Vendida"] 60
manson = Banda trashMetal ["Poronga"] 180
theStrokes = Banda fusion ["suicidio asistido","emocional","linda"] 45

tocar :: Banda -> Festival -> Festival
tocar = genero 

--Poner en consola : tocar losRedondos hullabalooza

--PUNTO 4--

--Definir la función suceder, que hace que suceda un festival.
-- El resultado debe ser el mismo festival pero con el público en su situación final, luego de haber tocado todas las bandas. 

suceder :: Festival -> [Banda] -> Festival
suceder festival listaDeBandas = foldr ($) festival (map genero listaDeBandas)

--PUNTO 5--

{-
Se conocen ciertos criterios de clasificación de bandas, de los cuales depende su popularidad. Por ejemplo:
Vendida: Debe tener tres o más descripciones o bien una descripción que sea “vendida”. 
Acústica: Es la que toca a más de 55 decibeles. 
Legendaria. Debe estar descripta como “legendaria” y tocar a más de 40 decibeles.
Definir las funciones que permitan clasificar a las bandas. 
Una banda puede clasificarse de más de una manera a la vez o ninguna.
-}

clasificacion :: Banda -> [String]
clasificacion banda = map (\x -> x banda) [esVendida,esAcustica,esLegendaria]

esVendida :: Banda -> String
esVendida banda 
 |(length (descripcion banda) >= 3) && (buscarEnLista (descripcion banda) (=="Vendida")) = "Vendida"
 | otherwise = ""

esAcustica :: Banda -> String
esAcustica banda 
 | condicionDecibeles (decibeles banda)  55 = "Acustica"
 | otherwise = ""

esLegendaria :: Banda -> String
esLegendaria banda 
 | (buscarEnLista (descripcion banda) (=="Legendaria")) && condicionDecibeles (decibeles banda) 40 = "Legendaria"
 | otherwise = ""

buscarEnLista :: [String] -> (String -> Bool) -> Bool
buscarEnLista lista condicion = any condicion lista

condicionDecibeles :: Float -> Float -> Bool
condicionDecibeles decibeles numero = decibeles > numero

--PUNTO 6--

popularidad :: Banda -> Int
popularidad  = (*100).length.clasificacion

--PUNTO 7--

{-
Definir la función buenFest, que dado un festival y un conjunto de clasificaciones posibles dice si es un buen fest. 
Esto sucede cuando cronológicamente cada banda es más popular que la anterior, 
y además la popularidad total (la popularidad acumulada de sus bandas) supera los 1000 puntos.
-}

buenFest :: Festival -> [Banda] -> Bool
buenFest festival listaDeBandas
 | (sum (map popularidad listaDeBandas) > 1000) && (lista_ordenada (cadaBandaesMejorAnterior listaDeBandas))  = True
 | otherwise = False
 

cadaBandaesMejorAnterior :: [Banda] -> [Int]
cadaBandaesMejorAnterior listaDeBandas = map popularidad listaDeBandas


compararPopularidadAcumulada :: [Int] -> Bool
compararPopularidadAcumulada listaDePopularidades = lista_ordenada listaDePopularidades

lista_ordenada :: Ord a => [a] -> Bool
lista_ordenada [] = True
lista_ordenada [_] = True
lista_ordenada (x:y:xs) = (x<=y) && lista_ordenada (y:xs)

--PUNTO 8--

--Los trate de utilizar en todo el parcial basicamente, reutilizando cada funcion previamente hecha.

--PUNTO 9--

--Me da mucha paja contestarlo.

--THE END--






