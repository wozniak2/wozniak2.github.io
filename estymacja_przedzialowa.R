# Estymacja przedzialowa

# Poprzednio ustalalismy wartosc nieznanego parametru (sredniej) w punkcie
# Szacowalismy srednia dla populacji na podstawie wylosowanej proby
# majac swiadomosc obciazenia estymatora

# inne podejscie to proba oszacowania tzw. przedzialu ufnosci
# przedzial ufnosci to przedzial wartosci, ktore z danym prawdopodobienstwem 
# przyjmie nieznany parametr. 
# przedzialy mozna ustalac dla roznego prawdopodobienstwa.

# kolejne podejscia to testowanie hipotez i regresja liniowa

# Przedzial ufnosci - def. i filmik?

# Przedzial ufnosci wzor
# gorna granica:
# x.bar + z*sd/sqrt(n)

# dolna granica:
# x.bar - z*sd/sqrt(n)

# wartosc z to wartosc funkcji kwantylowej (qnorm) dla danego poziomu ufności i danego rozkladu; 
# wartosc graniczna
# Jaka jest graniczna wartość zawierająca 95% przedzialu ufnosci dla standardowego rozkladu normalnego? 
# magiczna liczba 1.96
qnorm(0.975, 0, 1)

# importujemy dane cars.csv
# http://pluto.huji.ac.il/~msby/StatThink/Datasets/cars.csv
# skonstruujmy 95% przedzial ufnosci dla sredniej ceny samochodu

cars <- read.csv("cars.csv")
summary(cars)
length(cars$make)

# jakich zmiennych potrzebujemy do wzoru?
# srednia cena samochodu
x.bar <- mean(cars$price,na.rm=TRUE)

# odchylenie standardowe
s <- sd(cars$price,na.rm=TRUE)

# liczba obserwacji

sum(is.na(cars$price))
n <- 201

# wartosc z zawierajaca 95% obszar prawdopodobienstwa (przedzial ufnosci)
z <- qnorm(0.975)

# zgodnie ze wzorem na przedzial dolny i gorny

# przedzial dolny
x.bar - z*s/sqrt(n)

# przedzial gorny
x.bar + z*s/sqrt(n)

# Na podstawie obliczen z proby mamy 95% pewnosc, 
# ze srednia cena samochodow waha sie od 12100 - 14305

#------------------------------------------------------------------------------------------
# rozwazmy teraz przedzial ufnosci dla prawdopodobienstwa zajscia jakiegos zdarzenia
# wzor jest nieco inny

# Przedzial gorny:
# p.hat+z*sqrt(p.hat*(1-p.hat)/n)

# Przedzial dolny:
# p.hat-z*sqrt(p.hat*(1-p.hat)/n)

# p.hat - szacowane prawdopodobienstwo zajscia danego zdarzenia
# pozosostałe parametry bez zmian

# stworzmy przedzial ufnosci dla zdarzenia jakim jest prawdopodobienstwo 
# napotkania samochodu z silnikiem diesla

# wielkość proby; dane kompletne
n <- 205

# prawdopodobienstwo napotkania auta z dieselem
table(cars$fuel.type)
p.hat <- 20/n        # prawdopodobienstwo w punkcie

# przedzialy ufnosci
# dol:
p.hat - z*sqrt(p.hat*(1-p.hat)/n)

# gora:
p.hat + z*sqrt(p.hat*(1-p.hat)/n)

# z 95% pewnosci mozemy powiedziec, ze to prawdopododbienstwo bedzie od 0.057 - 0.138

# estymacja przedzialowa z wykorzystaniem symulacji rozkladu zmiennej empirycznej

# Jestesmy zainteresowani oszacowaniem przedzialow ufnosci dla sredniej
# ceny samochodu.
# Po przyjrzeniu sie danym z proby (cars) zauwazylismy, ze cena samochodow 
# przypomina rozklad wykladniczy zmiennej losowej.

# W R parametrem generatora teoretycznego rozkladu wykladniczego zmiennej (rexp) losowej jest, 
# oprocz liczby zdarzen "n" parametr "lambda", gdzie 
# "lambda" = 1/srednia wartosc zmiennej losowej: rexp(n, lambda)

# wylosujmy probe N=201 z teoretycznego rozkladu wykladniczego
# i sprawdzmy jakie jest prawdopodobienstwo, ze przedzialy ufnosci 
# z wykladniczego rozkladu teoretycznego
# beda zawieraly nasza srednia cene aut ze zbioru "cars". 

# srednia cena auta
sr <- mean(cars$price, na.rm=T)

# parametr rozkladu wykladnicznego
        
lambda <- 1/sr

# magazyn srednich i odchylen
X.bar <- c()
S <- c()

# liczba danych do wygenerowania
n <- 201

# powtorzmy losowanie 10 tys razy, aby otrzymać porownywalne wyniki
for(i in 1:10000)
   {
    
      X <- rexp(n,lambda)
      
        X.bar[i] <- mean(X)
          S[i] <- sd(X)
}

# obliczmy przedzialy ufnosci dla danych wylosowanych z rozkladu wykladniczego
# dolna granica
LCL <- X.bar - z*S/sqrt(n)
# gorna granica
UCL <- X.bar + z*S/sqrt(n)

# i prawdopodobienstwo, ze srednia cena empiryczna znajdzie sie w oszacowanych
# przedzialach
mean((sr >= LCL) & (sr <= UCL))

# nasz poziom ufnosci wyniosl okolo 94,5%, wiec zmiescilismy sie w zadanym przedziale
# ufnosci

# Zadanie 2: symulacja rozkladu prawdopodobienstwa napotkania samochodu z silnikiem Diesela.
# Zauwazyles, ze mozna to prawdopodobienstwo przyblizyc za pomoca rozkladu dwumianowego,
# gdzie sukces oznacza silnik Diesela, a porazka jego brak.
# Generator zmiennych losowych teoretycznego rozkladu dwumianowego to rbinom(N,1,p),
# gdzie N to liczba zdarzen, a p to prawdopodobienstwo zajscia danego zdarzenia

# jak obliczyc prawdopodobienstwo napotkania auta z silnikiem diesla
# Liczba wszystkich zdarzen
N = 205

# prawdopodobienstwo
p = sum(cars$fuel.type=="diesel")/N

# wylosujmy 1000 razy probe z teoretycznego rozkladu dwumianowego
# wzor dla prawdopodobienstwa zajscia zdarzenia
P.hat <- c()
for(i in 1:10^5)
   {
    
      X <- rbinom(N,1,p)
      
        P.hat[i] <- mean(X)
  }

# dolny przedial
LCL <- P.hat - z*sqrt(P.hat*(1-P.hat)/n)

# gorny przedzial
UCL <- P.hat + z*sqrt(P.hat*(1-P.hat)/n)

# sprawdzmy czy prawdopodobienstwo napotkania diesla obliczone ze zbioru
# cars miesci sie w oszacowanych z rozkladu dwumianowego przedzialach ufnosci
mean((p >= LCL) & (p <= UCL))


# ZADANIE DOMOWE

# Zaloz, ze firma ubezpieczeniowa przeprowadzila badanie 400 kierowcow.
# 320 z przebadanych kierowcow twierdzi, ze zawsze zapina pasy przed
# jazda. Utworz 80% przedzial ufnosci dla populacji kierowcow, ktora twierdzi,
# ze zawsze zapina pasy przed jazda i napisz odpowiedz (co oznaczaja otrzymane
# przez Ciebie wyniki?)

# przedzial ufnosci dla wartosci zmiennej czy prawdopodobienstwa zdarzenia? --> wzor








