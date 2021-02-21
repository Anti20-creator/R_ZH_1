# klasszikus valószínűségszámítás kérdései:

# számít a sorrend? NEM    ->   kombináció

# számít a sorrend? IGEN   ->   permutáció, variáció
# ha számít a sorrend:
# minden elemet sorba kell rendezni? IGEN -> permutáció
# minden elemet sorba kell rendezni? NEM  -> variáció

# a kombináció alkérdése: egy elem csak egyszer választható? IGEN/NEM -> ism nélküli / ismétléses
# a permutáció és variáció alkérdése: minden elem különböző? IGEN/NEM -> ism nélküli / ismétléses


###################################################################################
# 1. feladat
# Mennyi a valószínűsége, hogy a hatoslottón (itt 45 számból húznak hatot) a kihúzott számok
# között ugyanannyi páros szám van, mint páratlan?

# klasszikus valószínűségszámítási példa: permutáció/variáció/kombináció
# számít a sorrend? NEM -> kombináció
# egy elem csak egyszer választható? IGEN -> ismétlés nélküli kombináció


#45 szám közül fognak összesen hatot húzni
huzhatoszamok = 45
huzasok = 6

# x db huzható szám közül mindig annak alsó egészrésze lesz páros
# és mindig annak felső egészrésze lesz páratlan
dbparosak = floor(huzhatoszamok / 2)
dbparatlanok = ceiling(huzhatoszamok / 2)

# a choose az "n alatt a k" R nyelvbeli megfelelője
# ugyanannyi párosat kell húznunk mint páratlant (3 = huzasok/2)
parosak = choose(dbparosak, huzasok/2)
paratlanok = choose(dbparatlanok, huzasok/2)

# az összes kombinációt megadja az alábbi kifejezés
osszeskombinacio = choose(huzhatoszamok, huzasok)

eredmeny = parosak * paratlanok / osszeskombinacio


##############################################################################

# 2. feladat
# Egy dobozban 3 fajta dobókockánk van, összesen 8 db. Az
# első fajtából 2 db, melyen 2-2 db 1-es, 2-es és 3-as szerepel, a
# második fajtából 2 db, melyen 3 db 2-es és 3 db 1-es szerepel és a
# harmadik fajtából 4 db, melyen 4 db 3-as és 2 db 1-es szerepel.
# A dobozból véletlenszerűen kiválasztunk egy kockát és dobunk vele. Mennyi az esélye, hogy
# az első fajtájú kockából választottunk, feltéve, hogy 1-est dobtunk?

# Bayes-tétel alkalmazása

# B események x/8, ahol x azt jelöli, hogy az adott kockából, hány darab
# áll rendelkezésünkre

# A  = 1-est dobtunk  ~  esemény
# B1 = 2/8            ~ az első fajta kockát választottuk
# B2 = 2/8            ~ a második fajta kockát választottuk
# B3 = 4/8            ~ a harmadik fajta kockát választottuk

# megadjuk, hogy az egyes kockákkal mekkora eséllyel dobhatunk egyest, mivel
# ez a célunk - 'A' esemény

# P(A | B1) = 2/6
# P(A | B2) = 3/6
# P(A | B3) = 2/6

# Bayes-tétel:

PrA <- c(2/8, 2/8, 4/8)
PrB <- c(2/6, 3/6, 2/6)

bayes <- function(prA, prB, kedvezo) {
  szamlalo <- prA[kedvezo] * prB[kedvezo]
  nevezo = 0
  loop = c(1:(length(prA)))
  for (i in loop) {
    nevezo = nevezo + prA[i] * prB[i]
  }
  print(szamlalo/nevezo)
  return (szamlalo/nevezo)
}

# számunkra az a "kedvező eset", ha az első fajtájú kockával dobtunk egyest:
# PrA[1] = mekkora az esélye, hogy az első fajtájú kockával dobunk majd
# PrB[1] = mekkora az esélye, hogy egyest dobunk az első fajtájú kockával

# a "kedvező" nevű változó itt a kocka fajtájára utal, hiszen az, hogy egyest
# dobunk az kötve van az 'A' esemény által
eredmeny1 = bayes(prA = PrA, prB = PrB, kedvezo = 1)
print(eredmény1)

# Ellenőrzés: ha a maradék 2 kockára is kiszámoljuk az eredményt és ezek összege
# pontosan 1-et fog adni, akkor jók vagyunk
eredmeny2 = bayes(prA = PrA, prB = PrB, kedvezo = 2)
eredmeny3 = bayes(prA = PrA, prB = PrB, kedvezo = 3)
print(eredmeny1+eredmeny2+eredmeny3)

##############################################################################
# 3. feladat
# Minden nap metróval jövünk az egyetemre. A metró az egyes napokon a többitől függetlenül
# 0,1 valószínűséggel robban le. Mi a valószínűsége, hogy a héten az ötből legalább négy
# napon gond nélkül be tudunk metróval jönni?

# a hét hány napján közlekedünk a metróval
n = 5

# legalább hány napon kell gond nélkül beérnünk
k = 4

# esély, hogy gond nélkül bejutunk az egyetemre
p = 1 - 0.1

# esély, hogy bejutunk x napon az ötből az egyetemre gond nélkül:
# (5 alatt az x) * (0.9)^x * (0.1)^(5-x)
# jelen esetben a sum(dbinom()) által most az x = 4 és x = 5 eseteket összegezzük 
sum(dbinom(k:n, n, p))

###################################################################################
# 4. feladat
# Tegyük fel, hogy az X változó a (0,2) intervallumon vesz föl értékeket. 
# Sűrűségfüggvénye itt c*(2-x)^3
# Adjuk meg c értékét és a P(1<X<4) valószínűséget!
# A sűrűségfüggvény határozott integráltjának 1-nek kell lennie a megadott 
# intervallumon, mivel a sűrűségfüggvények deriváltja 1 a teljes "értelmes"
# halmazon.

# Definiáljuk a sűrűségfüggvényt c nélkül, hiszen az csak konstans szorzó
suruseg <- function(x) { return ((2-x)^3) }
# Az integráltat pedig határozott formában keressük 0 és 2 között.
result = integrate(suruseg, 0, 2)['value']


# Így integráltuk a konstans 'c' szorzó nélkül a függvényt.
# Tehát a valós határozott integrálunk result * c, ami jelen esetben 4*c
# Innen pedig az alábbi egyenletet kell megoldanunk:
# 1 = 4*c
# c = 1/4
# Tehát c = 1/4.

# Most pedig keressük P(1 < X < 4):
# X értéke csak a (0, 2) intervallumból kerülhet ki!
suruseg2 <- function(x) { return (1/4 * (2-x)^3) }
result2 = integrate(suruseg2, 1, 2)['value']

# A határozott integrált intervalluma (1,2) lett, mivel X > 2 esetén a valószínűség 0
# hiszen az értékek a (0, 2) intervallumból kerülnek ki.
# Innen a megoldás 0.0625, ami elég kis esélynek tűnik, amellett, hogy lefedtük a teljes
# intervallum felét, de a geogebra segíthet vizualizálni az eredményt:
# https://imgur.com/a/4HGMxSv


#################################################################################
# 5. feladat
# Péter feldob egy kockát. Ha páratlan számot dob, veszít 10 Ft-ot, ha 6-ost dob, nyer 40 Ft-ot,
# ha 2-est vagy 4-est dob, újból dobhat. A második dobásnál 10 Ft-ot nyer, ha párost dob, és 20
# Ft-ot veszít, ha páratlant dob. Mennyi a játék várható értéke?

# első dobásra:
# páratlan dobás = -10Ft
# 6-ost dob      = +40Ft
# 2-es / 4-es    = újradobhat

# második dobásra:
# páros dobás    = +10Ft
# páratlan dobás = -20Ft

# első kör:
# P(páratlan) ~ P1 = 3/6      -     x1 = -10Ft
# P(6)        ~ P2 = 1/6      -     x2 = +40Ft
# P(2 vagy 4) ~ P3 = 2/6      -     x3 = ?     (-10Ft/+20Ft)

# második kör:
# P(páros)    ~ P4 = 3/6      -     x4 = +10Ft
# P(páratlan) ~ P5 = 3/6      -     x5 = -20Ft

# Az első körben 4/6 az esélye annak, hogy biztosan nyerünk vagy veszítünk.
# A maradék 2/6 valószínűség viszont tovább bomlik, még hozza 3/6 - 3/6 arányban.
# Így a 2/6-ból 1/6 a valószínűsége, hogy +10Ft-ot nyerünk és 1/6 a valószínűsége,
# hogy veszítünk 20 Ft-ot.

# ezek függvényében az új táblázatunk így fest:
# P(páratlan)          ~ P1 = 3/6      -     x1 = -10Ft
# P(6)                 ~ P2 = 1/6      -     x2 = +40Ft
# P(2/4 majd páros)    ~ P3 = 1/6      -     x3 = +10Ft
# P(2/4 majd páratlan) ~ P4 = 1/6      -     x4 = -20Ft

# M(nyert pénz) = sum(Pk * xk) (k = 1..4)

P <- c(3/6, 1/6, 1/6, 1/6)
x <- c(-10, 40, 10, -20)

varhato <- function(P, x) {
  sum = 0
  loop = c(1:(length(x)))
  for (i in loop) {
    sum = sum + x[i] * P[i]
  }
  return (round(sum, 5))
}

eredmeny = varhato(P, x)
print(eredmeny)

#################################################################################
# 6. feladat
# Legyen X egyenletes eloszlású az (1, 4) intervallumon. Számítsuk ki
# (X - 2)^2 várható értékét és az X és X^2 kovarianciáját! 
# X eleme az (1, 4) intervallumnak -> (X - 2) eleme a (-1, 2) intervallumnak.

# Keressük: E(X-2)^2
# Ami egyenlő D^2(X-2) + E^2(X-2), ahol E a várható érték és D a szórás

# Általános alakban:
# E(X) = (a+b) / 2
# D(X) = (b-a) / sqrt(12), ahol a és b az intervallum kezdő- és végpontjai

varhatoertek <- function(a, b) {
  return ((a+b)/2) 
}

szoras <- function(a, b){
  return ((b-a) / sqrt(12))
}

a = -1
b = 2
eredmeny = (szoras(a, b) * szoras(a, b)) + (varhatoertek(a, b) * varhatoertek(a, b))

# Így E(X - 2)^2 = 1

# cov(X, X^2) = ? ...
