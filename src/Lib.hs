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








