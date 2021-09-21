main = function(){
  h=0.001  #Шаг
  
  An = 1   #Коэффициенты в дифференциальном уравнении  
  Bn = 0
  Cn = pi^2

  an=(1/h^2)*(An-Bn*h/2) #Коэффициенты в рекурентной форме
  bn=(-1/h^2)*(2*An-Cn*h^2)
  cn=(1/h^2)*(An+Bn*h/2)
  
  b = 10   #Длина отрезка
  E = 0.01 #Епсилон-орестность вблизи точек разрыва функции 1/cos(pi*x)
  
  
  uprev = 3     #Текущее значение в узле
  ucur = uprev  #Предыдущее значение в узле(вначале равны, посколько первая производная = 0)
  


  flagBr = 0 #Флаг отслеживания точек попадающих в эпсилон-окрестность
  br = 0.5   #Значение x точки разрыва x=1/2+k, k=0,1,...
             #С каждой пройденной точкой разрыва увеличивается на 1
  
  un = function(x){             #Функция, возвращающая значение в следующем узле
                                #На основании значений в двух предыдущих
    
    if(x > br-E && x < br+E){   #Если точка попадает в окрестность вблизи точки разрыва,
      flagBr <<- 1              #то игнорируем изменения
      return (ucur)
    }
    else{
      if(flagBr == 1){
        flagBr <<- 0
        br <<- br + 1
      }
      prevY=((1 / cn) * (f(x)-an*uprev-bn*ucur)) #Возвращаем значение в следующем узле сетке
      return (prevY)
    }
  }
  un2 = function(x){             #Функция, возвращающая значение в следующем узле
    #На основании значений в двух предыдущих
    
    if(x > br2-E && x < br2+E){   #Если точка попадает в окрестность вблизи точки разрыва,
      flagBr <<- 1              #то игнорируем изменения
      return (ucur)
    }
    else{
      if(flagBr == 1){
        flagBr <<- 0
        br2 <<- br2 - 1
      }
      prevY=((1 / an) * (f(x)-an*uprev-bn*ucur)) #Возвращаем значение в следующем узле сетке
      return (prevY)
    }
  }
  
  f = function(x){ #Функция, задающая неоднородность
    return (pi^2/cos(pi*x))
  }
  
  Yanalit = function(x){ #Аналитическое решение задачи Коши дифференциального уровнения
    return ((log(abs(cos(pi*x)), exp(1))+3)*cos(pi*x)+(pi*x+0)*sin(pi*x))
  }
  
  x = seq(from=2*h, to=b, by=h)
  y = seq(from=2*h, to=b, by=h)
  
  len = 2*h
  iter = 1
  while(len <= b){ #Итеративное решение(итеративно получаем значения в узлах сетки)
    len = len + h
    y[iter] = un(len)
    uprev = ucur
    ucur = y[iter]
    iter = iter + 1
  }
  
  x2 = seq(from=-2*h, to=-b, by=-h)
  y2 = seq(from=-2*h, to=-b, by=-h)
  uprev = 3
  ucur = uprev
  flagBr = 0
  br2 = -0.5
  len = -2*h
  iter = 1
  while(len > -b){ #Итеративное решение(итеративно получаем значения в узлах сетки в другую сторону)
    len = len - h
    y2[iter] = un2(len)
    uprev = ucur
    ucur = y2[iter]
    iter = iter + 1
  }
  
  iter = 1
  sum = 0 #Находим ошибку
  for(i in x){
    sum = sum + (y[iter]-Yanalit(i))^2
    iter= iter+1
  }
  sum = sqrt(sum/(b/h-2-1)) #Стандартное отклонение
  
  
  plot(x,y, type="l", xlim=c(-b,b)) #Отрисовываем численное решение
  lines(x2,y2)
  text(0,-25,paste("Error = ", sum))
  grid()
  
  x3=seq(-b,b,h)
  Sys.sleep(3)
  

  lines(x3, Yanalit(x3), col="red") #Отрисовываем аналитическое реешение

}
main()
