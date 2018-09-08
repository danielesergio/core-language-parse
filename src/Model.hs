module Model(
    Expr(..),
    Program,
    CoreProgram,
    ScDefn,
    CoreScDefn,
    Def,
    Alter,
    IsRec(..),
    Name
) where


data Expr a
   =  Evar Name
    | ENum Int
    | EConstr Int Int
    | EAp (Expr a) (Expr a)
    | ELet IsRec [Def a] (Expr a)
    | ECase (Expr a) [Alter a]
    | ELam [a] (Expr a)
    deriving Show

type Name = String
type Program a     = [ScDefn a]
type CoreProgram a = ScDefn Name
type ScDefn a       = (Name, [a], Expr a)
type CoreScDefn     = ScDefn Name
type Def a          = (a, Expr a)
type Alter a        = (Int, [a], Expr a)

data IsRec = NonRecursive | Recursive
  deriving Show
