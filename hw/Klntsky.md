# Программы на языке [Владимира Кальницкого](https://github.com/klntsky/fl_2020_ifmo_spr)

## Факториал
``` Pascal
(read n)
(result := 1)
(i := 1)
(while (i < n) {
  (i := i+1)
  (result := result*i)
})
(i := 1)
(write result)
```

## Фибоначчи
``` Pascal
(read n)
(previous:=0)
(next:=1)
(if(n<2){
  (write n)
})
(else{
  (while(i<n){
    (tmp:=next)
    (next:=next+previous)
    (previous:=tmp)
    })
  (write previous)
})
```
