# Программы на языке [Евгения Мартына](https://github.com/dedok1997/fl_2020_ifmo_spr)

## Факториал
``` C++
read(n);
result = 1;
i = 1;
while (i < n) {
  i = i+1;
  result = result*i;
};
write(result);
```

Тест:
``` Haskell
  evaluate' (unlines [
    "read(n);",
    "result = 1;",
    "i = 1;",
    "while (i < n) {",
      "i = i+1;",
      "result = result*i;",
    "};",
    "write(result);"]) [5] @?= Just [120]
```


## Факториал
``` C++
read(n);
previous=0;
next=1;
if(n<2){
  write(n);
}
else{
  while(i<n){
    tmp=next;
    next=next+previous;
    previous=tmp;
  };
  write(previous);
};
```

Тест:
``` Haskell
  evaluate' (unlines [
    "read(n);",
    "previous=0;",
    "next=1;",
    "if(n<2){",
      "write(n);",
    "}",
    "else{",
      "while(i<n){",
        "tmp=next;",
        "next=next+previous;",
        "previous=tmp;",
      "};",
      "write(previous);",
    "};"]) [5] @?= Just [8]
```

К сожалению в языке не предусмотрено присвоение одному иденту значение другого, поэтому программа по нахождению чисел Фибоначчи не работает.