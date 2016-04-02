## Zliczane sa tu wszystkie jak leci nazwiska.
## Co jesli artykul sie powtarzal?
## Nie szkodzi. Tu interesuje nas czy gdy sie weszlo o danej godzinie do internetu, 
## to czy mozna bylo o kandydacie poczytac. 
ilosciowo_kandydaci <- function(){
      require(dplyr)
      dane <- read.csv("dane/podsumowanie.txt")
      dane %>%
            select(kandydat) %>%
            table %>%
            sort(decreasing = TRUE)
}

ilosciowo_kandydaci()