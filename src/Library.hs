module Library where
import PdePreludat

-- Defino mis alias
type Nombre = String
type Nombres = [Nombre]
type Edad = Number
type Habilidad = String
type Habilidades = [Habilidad]
type Deseo = Chico -> Chico
type Deseos = [Deseo]
type Condicion = Chico -> Bool
type Pretendientes = [Chico]
type Chicos = [Chico]

-- Defino mis tipos
data Chico = Chico {
    nombre :: Nombre,
    edad :: Edad,
    habilidades :: Habilidades,
    deseos :: Deseos
} deriving Show

data Chica = Chica {
    nombreChica :: Nombre,
    condicion :: Condicion
} deriving Show

-- Inicializo algunos chicos
timmy = Chico "Timmy" 10 ["mirar television", "jugar en la pc"] [serMayor]

-- Inicializo algunas chicas
trixie = Chica "Trixie Tang" noEsTimmy
vicky = Chica "Vicky" (tieneHabilidad "ser un supermodelo noruego")

-- Defino algunos deseos
aprenderHabilidades :: Habilidades -> Deseo
aprenderHabilidades habilidadesNuevas chico = chico {habilidades = (habilidades chico)++habilidadesNuevas}

todosLosNeedForSpeed :: Number -> [String]
todosLosNeedForSpeed numero = ["jugar al need for speed "++(show numero)]++todosLosNeedForSpeed (numero+1)

serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed chico = chico {habilidades = (habilidades chico)++(todosLosNeedForSpeed 1)}

serMayor :: Deseo
serMayor chico = chico {edad = 18}

-- Defino algunas padrinos magicos
madurar :: Deseo
madurar chico = chico {edad = (+1) (edad chico)}

desMadurar :: Deseo
desMadurar chico = chico {edad = (/2) (edad chico)}

wanda :: Chico -> Chico
wanda chico = ((head.deseos) chico) (madurar chico)

cosmo :: Chico -> Chico
cosmo = desMadurar

muffinMagico :: Chico -> Chico
muffinMagico chico = foldr ($) chico $ (deseos chico)

-- Defino algunas condiciones
tieneHabilidad :: Habilidad -> Condicion
tieneHabilidad habilidadATener = (elem habilidadATener).habilidades

esSuperMaduro :: Condicion
esSuperMaduro chico = ((>18).edad) chico && tieneHabilidad "manejar" chico

noEsTimmy :: Condicion
noEsTimmy = (/="TImmy").nombre

-- Defino las siguientes funciones
quienConquistaA :: Chica -> Pretendientes -> Chico
quienConquistaA _ [pretendiente] = pretendiente
quienConquistaA chica (pretendiente:pretendientes) 
    | (condicion chica) pretendiente = pretendiente
    | otherwise = quienConquistaA chica pretendientes

-- quienConquistaA (Chica "Chica que busca Cocinero" (tieneHabilidad "cocinar")) pretendientes

-- Defino que dada una lista de chicos, obtengo aquellos con deseos prohibidos
habilidadesProhibidas = ["enamorar", "matar", "dominar el mundo"]

esUnaHabilidadProhibida :: Habilidad -> Bool
esUnaHabilidadProhibida = flip elem habilidadesProhibidas

tieneDeseoProhibido :: Chico -> Bool
tieneDeseoProhibido chico = any (esUnaHabilidadProhibida) (take 5 ((habilidades.muffinMagico) chico))

tienenDeseosProhibidos :: Chicos -> Chicos
tienenDeseosProhibidos chicos = filter (tieneDeseoProhibido) chicos

infractoresDeDaRules :: Chicos -> Nombres
infractoresDeDaRules chicos = map (nombre) (tienenDeseosProhibidos chicos)

-- Justificaciones
-- Se uso composicion en tieneHabilidad para formar una nueva funcion que tome como entrada la salida de otra funcion
{-- Se uso orden superior en, lo que me permitio que una funcion tome otra como argumento, 
que sin conocerla, pude delegarle la responsabilidad --}
-- Se uso aplicación parcial en noEsTimmy lo que me permitió componer
{-- Si quiero aprender una nueva habilidades y tengo una lista infinita, la funcion diverge. 
Pero si pregunto por una habilidad, converge. Ya que se evalua a medida que se necesita. Lazy Evaluation --}