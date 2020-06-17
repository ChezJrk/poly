import adt
import random
import numpy.testing as nptest                                          


# ------------------------------------------------------------------------
# Prelude Routines; You can ignore mostly

# from a github gist by victorlei
def extclass(cls):
  return lambda f: (setattr(cls,f.__name__,f) or f)

# Contexts
class Environment:
  """Replacement for Dict with ability to keep a stack"""
  def __init__(self, init_dict=None):
    self._bottom_dict   = init_dict
    self._stack         = [dict()]

  def push(self):
    self._stack.append(dict())

  def pop(self):
    self._stack.pop()

  def __getitem__(self,key):
    for e in reversed(self._stack):
      if key in e:  return e[key]
    if self._bottom_dict and key in self._bottom_dict:
      return self._bottom_dict[key]
    raise KeyError(key)

  def __contains__(self,key):
    for e in reversed(self._stack):
      if key in e: return True
    return bool(self._bottom_dict and key in self._bottom_dict)

  def __setitem__(self,key,val):
    self._stack[-1][key] = val


# ------------------------------------------------------------------------
#   POLY Grammar

BNF_in_ASDL_string = """
module POLY {
  expr  = Const( float  val  )
        | Var  ( string name )
        | Add  ( expr lhs, expr rhs )
        | Mul  ( expr lhs, expr rhs )
        | Let  ( string name, expr rhs, expr body )

  func  = ( string name, string* args, expr body )
}
"""

class expr:
  def __init__(self):
    assert False

class Const(expr):
  def __init__(self, val):
    assert type(val) is float
    self.val = val

class Var(expr):
  def __init__(self, name):
    assert type(name) is str
    self.name = name

class Add(expr):
  def __init__(self,lhs,rhs):
    assert isinstance(lhs,expr)
    assert isinstance(rhs,expr)
    self.lhs = lhs
    self.rhs = rhs

class Mul(expr):
  def __init__(self,lhs,rhs):
    assert isinstance(lhs,expr)
    assert isinstance(rhs,expr)
    self.lhs = lhs
    self.rhs = rhs

class Let(expr):
  def __init__(self,name,rhs,body):
    assert type(name) is str
    assert isinstance(rhs,expr)
    assert isinstance(body,expr)
    self.name = name
    self.rhs  = rhs
    self.body = body

class func:
  def __init__(self,name,args,body):
    assert type(name) is str
    assert type(args) is list
    for a in args: assert type(a) is str
    assert isinstance(body, expr)
    self.name = name
    self.args = args
    self.body = body

# alternatively...
POLY = adt.ADT(BNF_in_ASDL_string)

# note this compresses 46 LoC into 7 LoC



# ------------------------------------------------------------------------
#   POLY Interpreter

class Interpreter:
  def __init__(self, f, *args):
    assert type(f) is func
    kwargs    = { nm : val for nm,val in zip(f.args,args) }
    self._env = Environment(kwargs)
    self.f = f

  def result(self):
    return self.eval(self.f.body)

  def eval(self, e):
    etyp = type(e)

    if etyp is Const:
      return e.val

    elif etyp is Var:
      return self._env[e.name]

    elif etyp is Add:
      return self.eval(e.lhs) + self.eval(e.rhs)

    elif etyp is Mul:
      return self.eval(e.lhs) * self.eval(e.rhs)

    elif etyp is Let:
      self._env.push()
      rval    = self.eval(e.rhs)
      self._env[e.name] = rval
      rval    = self.eval(e.body)
      self._env.pop()
      return rval

    else: assert False, "unexpected case"

@extclass(func)
def eval(self, *args):
  return Interpreter(self, *args).result()


# ------------------------------------------------------------------------
#   POLY Compiler

class Compiler:
  def __init__(self, f):
    assert type(f) is func
    self.f = f

  def c_str(self):
    body = self.let_comp(self.f.body)
    cstr = (f"double {self.f.name}(\n"+
            ",\n".join([ f"  double {a}" for a in self.f.args ])+"\n"+
            f") {{\n{body}\n}}")
    return cstr

  def let_comp(self, e):
    cstrs = []

    while type(e) is Let:
      rhs   = self.e_comp(e.rhs)
      cstrs.append(f"  double {e.name} = {rhs};")
      e     = e.body
    
    assert type(e) is not Let
    cstrs.append( f"  return { self.e_comp(e) };" )

    return "\n".join(cstrs)

  def e_comp(self, e):
    etyp = type(e)

    if etyp is Const:
      return str(e.val)

    elif etyp is Var:
      return e.name

    elif etyp is Add:
      return f"({self.e_comp(e.lhs)} + {self.e_comp(e.rhs)})"

    elif etyp is Mul:
      return f"({self.e_comp(e.lhs)} * {self.e_comp(e.rhs)})"

    elif etyp is Let:
      assert False, "expected all Lets to occur at the top level"

    else: assert False, "unexpected case"

@extclass(func)
def compile(self):
  return Compiler(self).c_str()



# ------------------------------------------------------------------------
#   POLY Pretty-Printer

class Printer:
  def __init__(self, f):
    assert type(f) is func or isinstance(f,expr)
    self.f = f

  def result(self):
    if type(self.f) is func:
      tab  = '' if type(self.f.body) is Let else '  '
      body = self.pp(self.f.body,tab=tab)
      cstr = (f"def {self.f.name}("+
              ",".join(self.f.args)+")"+
              f")\n{body}")
      return cstr
    else:
      return self.pp(self.f)

  def pp(self, e, tab='', prec=0):
    etyp = type(e)

    if etyp is Const:
      return str(e.val)

    elif etyp is Var:
      return e.name

    elif etyp is Add:
      cstr = f"{self.pp(e.lhs,tab,1)} + {self.pp(e.rhs,tab,1)}"
      if prec > 1: cstr = f"({cstr})"
      return cstr

    elif etyp is Mul:
      cstr = f"{self.pp(e.lhs,tab,2)} * {self.pp(e.rhs,tab,2)}"
      if prec > 2: cstr = f"({cstr})"
      return cstr

    elif etyp is Let:
      lines   = []
      while type(e) is Let:
        rhs   = self.pp(e.rhs,tab+'  ',0)
        lines.append(f"{tab}{e.name} = {rhs}")
        e     = e.body

      body  = self.pp(e,tab,0)
      lines.append(f"{tab}in {body}\n")
      cstr  = "\n".join(lines)

      if prec > 0: cstr = f"({cstr})"
      return cstr

    else: assert False, "unexpected case"

@extclass(func)
def __str__(self):
  return Printer(self).result()

@extclass(expr)
def __str__(self):
  return Printer(self).result()


# ------------------------------------------------------------------------
#   POLY Total Derivative

class Derivative:
  def __init__(self, f):
    assert type(f) is func
    kwargs    = { nm : 'd'+nm for nm in f.args }
    args      = f.args + [ 'd'+nm for nm in f.args ]
    self._env = Environment(kwargs)
    body      = self.D(f.body)
    self.final = func(f.name, args, body)

  def result(self):
    return self.final

  def D(self, e):
    etyp = type(e)

    if etyp is Const:
      return Const(0.0)

    elif etyp is Var:
      if e.name in self._env:
        return Var(self._env[e.name])

    elif etyp is Add:
      return Add( self.D(e.lhs),
                  self.D(e.rhs) )

    elif etyp is Mul:
      return Add( Mul( e.lhs, self.D(e.rhs) ),
                  Mul( self.D(e.lhs), e.rhs ) )

    elif etyp is Let:
      self._env.push()
      drhs    = self.D(e.rhs)
      self._env[e.name] = 'd'+e.name
      dbody   = self.D(e.body)
      self._env.pop()
      return Let( e.name, e.rhs, Let('d'+e.name, drhs, dbody) )

    else: assert False, "unexpected case"

@extclass(func)
def deriv(self):
  return Derivative(self).result()


# ------------------------------------------------------------------------
#   POLY Quick-Check

def QC(f0,f1, n_samp = 10):
  assert type(f0) is func
  assert type(f1) is func

  if len(f0.args) != len(f1.args):
    raise TypeError("these functions are not equal because they have "+
                    "different numbers of arguments")
  n_args = len(f0.args)

  def gen_input():
    return [ random.random() for _ in range(n_args) ]

  for _ in range(n_samp):
    args  = gen_input()
    nptest.assert_approx_equal( f0.eval(*args), f1.eval(*args) )






