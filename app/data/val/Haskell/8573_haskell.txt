--unha declaracion require unha definicion
--o everflow da warning pero non erro na definicion
--na operacion é silencioso

x :: Int
x = 555555555555555555555555555555555555555555555555
--x ^ 20000 -- da overflow e retorna 0 se o casteamos ou indicamos o tipo con :: Int se non infire que é un integer
y :: Integer -- é mais largo que o Int
y = 200000000000000000000000000000000000000000000000 :: Integer
--char
letra :: Char 
letra = 'x'

--float

numeroF :: Float
numeroF = 2.3 :: Float

numeroD :: Double
numeroD = 2.3 --coma na resto de linguaxes por defecto haskell utiliza o double esta desaconsellado usar o float

--Bool

bool :: Bool
bool = False

-- e interesante que cando definimos unha variable en haskell temos que lembrar que non estamos asignando un valor a unha variable
-- xa que todo é unha funcion e definimos unha funcion que non ten parametros e devolve o valor Y


-- ollo porque as veces os tipos son alias de outros String é un alias de [Char]
-- ollo tamen porque as listas aqui teñen que ter o mesmo tipo o senon ter o mesmo tipo pai
lista :: [Int]
lista = [1,2,34]

cadea :: String
cadea = "hola mundo"

cadeaC :: [Char]
cadeaC = ['h','o','l','a']

--temos tamen as tuplas que poden estar formadas por elementos de tipos distintos

tupla :: (String,Int)
tupla = ("hola",1)

--as funcions que se definen dunha forma peculiar xa que non fai distincion entre retorno e argumentos
--o ultimo sempre é o retorno
-- tal que nome :: tipo1 -> tipoArg2 ..... -> tipoArgn -> tipoRetorno
func :: Int -> String

func num = show num

func2 :: Int -> Int -> String

func2 num num2 = (show num) ++ " primeiro argumento o segundo é" ++ (show num2)

--ollo porque haskell é de tipado moi forte nunca vai facer conversions por nos

div2 :: Int -> Double
--por este motivo non podemos devolver un int se a sinatura da funcion pon un double
div2 num = (fromIntegral num) / 2 --se non casteamos primeiro infire que é unha operacion non fracional e peta
-- o facer esto considera o segundo un fracional si ou si xa que os numeros son polimorficos
-- por defecto un numero sen coma flotante interpretao como integer pero se o primeiro é de coma flotante
-- enton o ser polimorficos tratao como un coma flotante

--quiz 9.1
div3 :: Int -> Int
-- so devolve numeros enteiros
div3 num = num `div` 2

--temos para pasar de string a X tipo read
--temos para pasar de X tipo a String show

str = show 5

-- nos non indicamos o tipo
-- infireo el
dbl = read "2.5" :: Double

--quiz 9.2

printDouble :: Int -> String

--non convertimos a tipo especifico concreto convirte a funcion o tipo necesario

printDouble numero = show (fromIntegral numero / 1)

--o curioso e que en haskell as funcions so reciven un argumento
--é equivalente

funcE :: Int -> Int -> Int

--funcE num1 num2 = num1 + num2
--podemos entendelo coma unha funcion que devolve unha funcion que devolve un int
--aqui entra moito en xogo as funcions parcialmente aplicadas
funcE = (\num1 ->
                    (\num2 -> num1 + num2))

--quiz 9.3
-- se non especificamos o tipo e poñemos unha letra minuscula convirtese nunha funcion xenerica
--ie
--func :: a -> a
--se usamos diferentes letras non ten porque ser o mesmo
--func :: a -> b -> c
--o primeiro obriga a que sexa o mesmo tipo o segundo non ten porque
makeAddress :: (String -> (String -> (String -> String)))
makeAddress = (\x -> (\y ->(\z -> x ++ y ++ z)))
