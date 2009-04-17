stack = []

def LOAD0(): stack.append(stack[0])
def LOAD1(): stack.append(stack[1])
def LOAD2(): stack.append(stack[2])
def LOAD3(): stack.append(stack[3])
def LOAD4(): stack.append(stack[4])
def LOAD5(): stack.append(stack[5])
def LOAD6(): stack.append(stack[6])
def LOAD7(): stack.append(stack[7])
def LOADF(x): stack.append(stack[x])
def LIT(x): stack.append(x)
def ADD():  stack.append(stack.pop()+stack.pop())
def SUB():  stack.append(-stack.pop()+stack.pop())
def MUL():  stack.append(stack.pop()*stack.pop())
def DIV():
    a = stack.pop()
    b = stack.pop()
    stack.push(b/a)
def MOD():
    a = stack.pop()
    b = stack.pop()
    stack.push(b%a)
    
