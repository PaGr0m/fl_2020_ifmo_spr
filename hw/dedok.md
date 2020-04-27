# Программы на языке [Евгения Мартына](https://github.com/dedok1997/fl_2020_ifmo_spr)


## Факториал
``` C++
def factorial(n) {
  result = 1;
  i = 1;
  while (i < n) {
    i = i+1;
    result = result*i;
  };
  return result;
}

def main(){
  a = factorial(5);
  return a;
} 
```


## Фибоначчи
``` C++
def fib(n) {
  previous=0;
  next=1;
  if(n<2){
    return n;
  }
  else{
    while(i<n){
      tmp=next;
      next=next+previous;
      previous=tmp;
    };
    return previous;
  };
}

def main(){
  a = fib(5);
  return a;
} 
```

Синтаксис похож на С++ и немного Python. Замечаний нет.
