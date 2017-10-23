import Assn2.{BoolV, Num, NumV, StringV, Value, Str}
import Assn2.Value.multiply
import Assn2.Value.add
import Assn2._


import scala.collection.immutable.ListMap


val t1 = multiply(NumV(2), NumV(3))
val t2 = add(NumV(1), NumV(2))
val t3 = Value.eq(NumV(2), NumV(2))
val t4 = Value.eq(NumV(2), NumV(2))
val t5 = Value.eq(BoolV(true), BoolV(true))
val t6 = Value.eq(StringV("string"), StringV("string"))
val t7 = Value.length(StringV("string"))
val t8 = Value.index(StringV("string"), NumV(0))
val t9 = Value.concat(StringV("string"), StringV("string"))
val t10 = eval(Map(), Num(2))
val t11 = eval(Map(), Plus(Num(2), Num(3)))
val t12 = eval(Map(), Times(Num(2), Num(3)))
val t13 = eval(Map(), Minus(Num(3), Num(2)))
val t14 = eval(Map(), Bool(false))
val t15 = eval(Map(), Eq(Str("abc"), Str("abc")))
val t16 = eval(Map(), IfThenElse(Bool(false), Num(2), Num(3)))
val t17 = eval(Map(), Str("test"))
val t18 = eval(Map() + ("x" -> NumV(5)), Plus(Let("x", Num(3), Plus(Var("x"), Num(7))), Var("x")))
val t19 = eval(Map(), First(Pair(Num(42), Num(27))))
val t20 = eval(Map(), Num(777))
val fxPlus1 = Apply(Var("f"),Plus(Var("x"),Num(1)))
val ifThEl = IfThenElse(Eq(Var("x"),Num(3)),Var("x"),fxPlus1)
eval(Map(), Apply( Rec("f","x",IntTy,IntTy, ifThEl ), Num(1) ) )

// NOTE: Fellow Student Antonios and I collaborated

object Assn2 {
  type Variable = String
  type Env[A] = Map[Variable, A]

  // Arithmetic expressions

  abstract class Expr

  case class Num(n: Integer) extends Expr

  case class Plus(e1: Expr, e2: Expr) extends Expr

  case class Minus(e1: Expr, e2: Expr) extends Expr

  case class Times(e1: Expr, e2: Expr) extends Expr

  // Booleans
  case class Bool(n: Boolean) extends Expr

  case class Eq(e1: Expr, e2: Expr) extends Expr

  case class IfThenElse(e: Expr, e1: Expr, e2: Expr) extends Expr

  // Strings
  case class Str(s: String) extends Expr

  case class Length(e: Expr) extends Expr

  case class Index(e1: Expr, e2: Expr) extends Expr

  case class Concat(e1: Expr, e2: Expr) extends Expr

  // Variables and let-binding
  case class Var(x: Variable) extends Expr

  case class Let(x: Variable, e1: Expr, e2: Expr) extends Expr

  case class LetFun(f: Variable, arg: Variable, ty: Type, e1: Expr, e2: Expr)
    extends Expr

  case class LetRec(f: Variable, arg: Variable, xty: Type, ty: Type, e1: Expr, e2: Expr)
    extends Expr

  case class LetPair(x: Variable, y: Variable, e1: Expr, e2: Expr) extends Expr

  // Pairing
  case class Pair(e1: Expr, e2: Expr) extends Expr

  case class First(e: Expr) extends Expr

  case class Second(e: Expr) extends Expr

  // Functions
  case class Lambda(x: Variable, ty: Type, e: Expr) extends Expr

  case class Apply(e1: Expr, e2: Expr) extends Expr

  case class Rec(f: Variable, x: Variable, tyx: Type, ty: Type, e: Expr) extends Expr

  // Values
  abstract class Value

  case class NumV(n: Integer) extends Value

  case class BoolV(n: Boolean) extends Value

  case class StringV(s: String) extends Value

  case class PairV(v1: Value, v2: Value) extends Value

  case class ClosureV(env: Env[Value], x: Variable, e: Expr) extends Value

  case class RecV(env: Env[Value], f: Variable, x: Variable, e: Expr) extends Value

  // Types
  abstract class Type

  case object IntTy extends Type

  case object BoolTy extends Type

  case object StringTy extends Type

  case class PairTy(ty1: Type, ty2: Type) extends Type

  case class FunTy(ty1: Type, ty2: Type) extends Type


  // ======================================================================
  // Part 1: Interpretation
  // ======================================================================

  // ======================================================================
  // Exercise 1: Primitive operations
  // ======================================================================


  object Value {
    // utility methods for operating on values
    def add(v1: Value, v2: Value): Value = (v1, v2) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
      case _ => sys.error("arguments to addition are non-numeric")
    }

    def subtract(v1: Value, v2: Value): Value = (v1, v2) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 - v2)
      case _ => sys.error("arguments to subtraction are non-numeric")
    }

    def multiply(v1: Value, v2: Value): Value = (v1, v2) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 * v2)
      case _ => sys.error("arguments to multiplication are non-numeric")
    }

    def eq(v1: Value, v2: Value): Value = (v1, v2) match {
      case (NumV(v1), NumV(v2)) =>
        if (v1 == v2) {
          BoolV(true)
        }
        else{
          BoolV(false)
        }
      case (BoolV(v1), BoolV(v2)) =>
        if (v1 == v2) {
          BoolV(true)
        }
        else{
          BoolV(false)
        }
      case (StringV(v1), StringV(v2)) =>
        if (v1 == v2) {
          BoolV(true)
        }
        else{
          BoolV(false)
        }
      case _ => sys.error ("Arguments are not equal.")
    }

    def length(v: Value): Value = v match {
      case StringV(v) => NumV(v.length())
      case _ => sys.error("Argument not a string.")
    }

    def index(v1: Value, v2: Value): Value = (v1, v2) match {
      case (StringV(v1), NumV(v2)) if v2 <= v1.length() => StringV(v1.charAt(v2).toString())
      case _ => sys.error("Not a string, or index out of length")
    }

    def concat(v1: Value, v2: Value): Value = (v1, v2) match {
      case (StringV(v1), StringV(v2)) => StringV(v1+v2)
      case _ => sys.error("Arguments are not strings")
    }


  }


  // ======================================================================
  // Exercise 2: Evaluation
  // ======================================================================

  def eval(env: Env[Value], e: Expr): Value = e match {
    // Arithmetic
    case Num(n) => NumV(n)
    case Plus(e1, e2) =>
      Value.add(eval(env, e1), eval(env, e2))
    case Minus(e1, e2) =>
      Value.subtract(eval(env, e1), eval(env, e2))
    case Times(e1, e2) =>
      Value.multiply(eval(env, e1), eval(env, e2))
    case Bool(b) => BoolV(b)
    case Eq(e1, e2) =>
      Value.eq(eval(env, e1), eval(env, e2))
    case IfThenElse(e, e1, e2) => if (eval(env, e) == BoolV(true)) {
      eval(env, e1)
    } else {
      eval(env, e2)
    }
    case Str(s) => StringV(s)
    case Length(s) => Value.length(eval(env, s))
    case Index(e1, e2) => Value.index(eval(env, e1), eval(env, e2))
    case Concat(e1, e2) => Value.concat(eval(env, e1), eval(env, e2))
    case Var(x) => env(x)
    case Let(x, e1, e2) =>
      val e1v = eval(env, e1)
      eval(env + (x -> e1v), e2)
    case Pair(e1, e2) => PairV(eval(env, e1), eval(env, e2))
    case First(Pair(e1, e2)) => eval(env, e1) //NOT SURE
    case Second(Pair(e1, e2)) => eval(env, e2) //NOT SURE
    case Lambda(x, ty, e) => ClosureV(env, x, e)
    case Rec(f, x, tyx, ty, e) => RecV(env, f, x, e)
    case Apply(Lambda(x, ty, e), e2) => eval(env, Let(x, e2, e))
    case Apply(e1, e2) => val e1v = eval(env, e1)
      e1v match {
      case (RecV(env0, f, x, e)) =>
        val recv = e1v
        val e2v = eval(env, e2)
        val env2 = env + (x -> e2v) + (f -> recv)
        eval(env2, e)
    }
    case _ => sys.error("Can't evaluate a non-handled case.")
  }




  // ======================================================================
  // Part 2: Typechecking
  // ======================================================================

  // ======================================================================
  // Exercise 3: Typechecker
  // ======================================================================

  // typing: calculate the return type of e, or throw an error
  def tyOf(ctx: Env[Type], e: Expr): Type = e match {
    // Arithmetic
    case Num(n) => IntTy
    case Plus(e1, e2) => (tyOf(ctx, e1), tyOf(ctx, e2)) match {
      case (IntTy, IntTy) => IntTy
      case _ => sys.error("non-integer arguments to +")
    }
    case Minus(e1, e2) => (tyOf(ctx, e1), tyOf(ctx, e2)) match {
      case (IntTy, IntTy) => IntTy
      case _ => sys.error("non-integer arguments to -")
    }
    case Times(e1, e2) => (tyOf(ctx, e1), tyOf(ctx, e2)) match {
      case (IntTy, IntTy) => IntTy
      case _ => sys.error("non-integer arguments to x")
    }
    case Bool(b) => BoolTy
    case Eq(e1, e2) => (tyOf(ctx, e1), tyOf(ctx, e2)) match{
      case (IntTy, IntTy) => BoolTy
      case (BoolTy, BoolTy) => BoolTy
      case (StringTy, StringTy) => BoolTy
      case _ => sys.error("expressions not of same type for Eq.")
    }
    case IfThenElse(e, e1, e2) => (tyOf(ctx, e), tyOf(ctx, e1), tyOf(ctx, e2)) match{
      case (BoolTy, IntTy, IntTy) => IntTy
      case (BoolTy, BoolTy, BoolTy) => BoolTy
      case (BoolTy, StringTy, StringTy) => StringTy
      case _ => sys.error("expressions are not of appropriate types for IfThenElse")
    }
    case Str(s)=> StringTy
    case Length(s) => tyOf(ctx, s) match{
      case (StringTy) => IntTy
      case _ => sys.error("expression is not a string")
    }
    case Index(e1, e2) => (tyOf(ctx, e1), tyOf(ctx, e2)) match{
      case (StringTy, IntTy) => StringTy
      case _ => sys.error("expressions not of correct type for Index.")
    }
    case Concat(e1, e2) => (tyOf(ctx, e1), tyOf(ctx, e2)) match{
      case (StringTy, StringTy) => StringTy
      case _ => sys.error("expressions not of correct type for Concat.")
    }
    // Variables and let-binding
    case Var(x) => ctx(x)
    case Let(x, e1, e2) => tyOf(ctx + (x -> (tyOf(ctx, e1))), e2)
    case Pair(e1, e2) => PairTy(tyOf(ctx, e1), tyOf(ctx, e2))
    case First(Pair(e1,e2)) => tyOf(ctx, e1)
    case Second(Pair(e1,e2)) => tyOf(ctx, e2)
    case Apply(e1, e2) => (tyOf(ctx, e1), tyOf(ctx, e2)) match{
      case (FunTy(ty1, ty2), t2) if (ty2 == t2) => t2
      case _ => sys.error("Apply has incorrect input types.")
   }
    case Lambda(x, ty, e) => FunTy(ty, tyOf(ctx + (x -> ty), e))
    case Rec(f, x, tyx, ty, e) => FunTy(ty, tyOf(ctx + (f -> FunTy(tyx, ty)) + (x -> tyx), e))
    case LetPair(x, y, e1, e2) => tyOf(ctx, e1) match{
        case PairTy(ty1, ty2) => tyOf(ctx + (x-> ty1) + (y-> ty2), e2)
        case _ => sys.error("LetPair has incorrect input types.")
    }
    case LetFun(f, arg, ty, e1, e2) => {
      val ctx1 = ctx + (arg -> ty)
      val ty2 = tyOf(ctx1, e1)
      val ctx2 = ctx + (f -> FunTy(ty, ty2))
      tyOf(ctx2, e2)
    }
    case LetRec(f, arg, xty, ty, e1, e2) =>
      val ctx1 = ctx + (f -> FunTy(xty, ty)) + (arg -> xty)
      val e1t = tyOf(ctx1, e1)
      val ctx2 = ctx + (f -> FunTy(xty, ty))
      tyOf(ctx2, e2)
    case _ => sys.error("Types were incorrect on input.")
  }


  // ======================================================================
  // Part 3: Syntactic transformation
  // ======================================================================

  // ======================================================================
  // Exercise 4: Capture-avoiding substitution
  // ======================================================================

  // This object provides a method to generate a "fresh" variable name
  object Gensym {
    private var id = 0

    def gensym(s: Variable): Variable = {
      val fresh_s = s + "_" + id
      id = id + 1
      fresh_s
    }
  }

  def swapVar(x: Variable, y: Variable, z: Variable): Variable =
    if (x == y) {
      z
    } else if (x == z) {
      y
    } else {
      x
    }

  def swap(e: Expr, y: Variable, z: Variable): Expr = {
    def go(e: Expr): Expr = e match {
      case Num(n) => Num(n)
      case Plus(t1, t2) => Plus(go(t1), go(t2))
      case Minus(t1, t2) => Minus(go(t1), go(t2))
      case Times(t1, t2) => Times(go(t1), go(t2))

      case Bool(b) => Bool(b)
      case Eq(t1, t2) => Eq(go(t1), go(t2))
      case IfThenElse(t, t1, t2) => IfThenElse(go(t), go(t1), go(t2))

      case Str(s) => Str(s)
      case Length(t) => Length(go(t))
      case Index(t1, t2) => Index(go(t1), go(t2))
      case Concat(t1, t2) => Concat(go(t1), go(t2))

      case Var(x) => Var(swapVar(x, y, z))
      case Let(x, t1, t2) => Let(swapVar(x, y, z), go(t1), go(t2))
      case LetFun(f, x, ty, t1, t2) => LetFun(swapVar(f, y, z), swapVar(x, y, z), ty, go(t1), go(t2))
      case LetRec(f, x, xty, ty, t1, t2) => LetRec(swapVar(f, y, z), swapVar(x, y, z), xty, ty, go(t1), go(t2))
      case LetPair(x1, x2, t1, t2) => LetPair(swapVar(x1, y, z), swapVar(x2, y, z), go(t1), go(t2))

      case Pair(t1, t2) => Pair(go(t1), go(t2))
      case First(t) => First(go(t))
      case Second(t) => Second(go(t))


      case Lambda(x, ty, t) => Lambda(swapVar(x, y, z), ty, go(t))
      case Apply(t1, t2) => Apply(go(t1), go(t2))
      case Rec(f, x, xty, ty, t) => Rec(swapVar(f, y, z), swapVar(x, y, z), xty, ty, go(t))

    }

    go(e)
  }

  def subst(e1: Expr, e2: Expr, x: Variable): Expr =
    e1 match {
      case Num(e) => Num(e)
      case Bool(e) => Bool(e)
      case Str(e) => Str(e)
      case Plus(t1, t2) => Plus(subst(t1, e2, x), subst(t2, e2, x))
      case Minus(t1, t2) => Minus(subst(t1, e2, x), subst(t2, e2, x))
      case Times(t1, t2) => Times(subst(t1, e2, x), subst(t2, e2, x))
      case IfThenElse(t0, t1, t2) => IfThenElse(subst(t0, e2, x), subst(t1, e2, x), subst(t2, e2, x))
      case Length(t0) => Length(subst(t0, e2, x))
      case Index(t1, t2) => Index(subst(t1, e2, x), subst(t2, e2, x))
      case Concat(t1, t2) => Concat(subst(t1, e2, x), subst(t2, e2, x))
      case Var(y) =>
        if (x == y) {
          e2
        } else {
          Var(y)
        }
      case Lambda(x, ty, t0) =>
        val z = Gensym.gensym(x);
        val fresh_t0 = swap(t0, x, z);
        Lambda(z, ty, subst(fresh_t0, e2, x))
      case Rec(f, y, tyx, ty, t0) => {
        val g = Gensym.gensym(f);
        val z = Gensym.gensym(y);
        val fresh0_t0 = swap(t0, f, g);
        val fresh1_t1 = swap(fresh0_t0, x, z);
        Rec(g, z, tyx, ty, subst(fresh1_t1, e2, x))
      }
      case Let(y, t1, t2) => {
        val z = Gensym.gensym(y);
        val fresh_t2 = swap(t2, y, z);
        Let(z, subst(t1, e2, x), subst(fresh_t2, e2, x))
      }
      case LetPair(y1, y2, t1, t2) => {
        val z1 = Gensym.gensym(y1);
        val z2 = Gensym.gensym(y2);
        val fresh_t1_1 = swap(t1, y1, z1);
        val fresh_t1_2 = swap(fresh_t1_1, y2, z2);
        LetPair(z1, z2, subst(t1, e2, x), subst(fresh_t1_2, e2, x))
      }
      case LetFun(f, y, ty, t1, t2) => {
        val g = Gensym.gensym(f);
        val z = Gensym.gensym(y);
        val fresh_t1 = swap(t1, y, z);
        val fresh_t2 = swap(t2, f, g);
        LetFun(g, z, ty, subst(fresh_t1, e2, x), subst(fresh_t2, e2, x))
      }
      case LetRec(f, y, yty, ty, t1, t2) => {
        val g = Gensym.gensym(f);
        val z = Gensym.gensym(y);
        val fresh_t1_1 = swap(t1, y, z);
        val fresh_t1_2 = swap(fresh_t1_1, f, g)
        val fresh_t2 = swap(t2, f, g);
        LetRec(g, z, yty, ty, subst(fresh_t1_2, e2, x), subst(fresh_t2, e2, x))
      }
      case Pair(t1, t2) => Pair(subst(t1, e2, x), subst(t1, e2, x))
      case First(t1) => First(subst(t1, e2, x))
      case Second(t1) => Second(subst(t1, e2, x))
      case Apply(t1, t2) => Apply(subst(t1, e2, x), subst(t2, e2, x))
      case Eq(t1, t2) => Eq(subst(t1, e2, x), subst(t2, e2, x))
      case _ => sys.error("Inputs to subst are incorrect.")
    }


  // ======================================================================
  // Exercise 5: Desugaring let fun, let rec and let pair
  // ======================================================================

  def desugar(e: Expr): Expr = e match {

    case Num(n) => Num(n)
    case Plus(e1, e2) => Plus(desugar(e1), desugar(e2))
    case Minus(e1, e2) => Minus(desugar(e1), desugar(e2))
    case Times(e1, e2) => Times(desugar(e1), desugar(e2))
    case Bool(b) => Bool(b)
    case Eq(e1, e2) => Eq(desugar(e1), desugar(e2))
    case IfThenElse(e, e1, e2) =>  IfThenElse(desugar(e), desugar(e1), desugar(e2))
    case Str(s) => Str(s)
    case Length(e) => Length(desugar(e))
    case Index(e1, e2) => Index(desugar(e1), desugar(e2))
    case Concat(e1, e2) => Concat(desugar(e1), desugar(e2))
    case Var(x) => Var(x)
    case Let(x, e1, e2) => Let(x, desugar(e1), desugar(e2))
    case Pair(e1, e2) => Pair(desugar(e1), desugar(e2))
    case First(e) => First(desugar(e))
    case Second(e) => Second(desugar(e))
    case Lambda(x, ty, e) => Lambda(x, ty, desugar(e))
    case Apply(e1, e2) => Apply(desugar(e1), desugar(e2))
    case Rec(f,x,tyx,ty,e) => Rec(f,x,tyx,ty,desugar(e))
    //case LetPair(x, y, e1, e2) => {
       //val p = Gensym.gensym(x);
       //Let(p, e1, subst(First(p), e2, x))

      /*var p = Gensym.gensym(x);
      val p1 = First(p);
      val p1 =
      val e2_x = swap(e, x, First(p));
      val e2_xy = subst(e2_x, Second(p), pair)
      Let(p, e1_fresh, e2_xy)*/
   // }
    case LetFun(f, x, ty, e1, e2) =>
      Let(f, Lambda(x, ty, desugar(e1)), desugar(e2))
    case LetRec(f, x, xty, ty, e1, e2) =>
      Let(f, Rec(f,x,xty,ty,desugar(e1)), desugar(e2))
    case _ => sys.error("Not a desugar case.")

  }
}