from tkinter import *
import turtle
import random


s = turtle.getscreen()
t = turtle.Turtle()
s.bgcolor("black")

n=100

for i in range(n+1):
    x = random.randint(1,5)
    
    if x==1:
        t.color("red")
        t.fd(10)
        t.lt(90)
    elif x==2:
        t.color("yellow")
        t.fd(10)
        t.rt(90)
    elif x==3:
        t.color("cyan")
        t.bk(10)
        t.rt(90)
    else:
        t.color("green")
        t.bk(10)
        t.lt(90)
    
turtle.mainloop() 

s.getcanvas().postscript(file='random2.ps')